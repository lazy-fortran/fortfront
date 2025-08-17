module type_system_hm
    ! Hindley-Milner type system for lazy fortran compiler frontend
    use iso_c_binding, only: c_loc, c_associated
    implicit none
    private

    ! Public types and interfaces
    public :: type_var_t, mono_type_t, poly_type_t, type_env_t, substitution_t
    public :: allocation_info_t
    public :: create_type_var, create_mono_type, create_poly_type, create_fun_type
    public :: apply_substitution, compose_substitutions
    public :: occurs_check, free_type_vars

    ! Type kinds for Hindley-Milner system
    integer, parameter, public :: TVAR = 1      ! Type variable
    integer, parameter, public :: TINT = 2      ! Integer type
    integer, parameter, public :: TREAL = 3     ! Real type
    integer, parameter, public :: TCHAR = 4     ! Character type
    integer, parameter, public :: TLOGICAL = 5  ! Logical type
    integer, parameter, public :: TFUN = 6      ! Function type
    integer, parameter, public :: TARRAY = 7    ! Array type

    ! Type variable
    type :: type_var_t
        integer :: id
        character(len=:), allocatable :: name  ! e.g., 'a, 'b
    contains
        procedure :: assign => type_var_assign
        generic :: assignment(=) => assign
    end type type_var_t

    ! Memory allocation attributes
    type :: allocation_info_t
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_target = .false.
        logical :: is_allocated = .false.  ! Known at compile time
        logical :: needs_allocation_check = .false.
        logical :: needs_allocatable_string = .false.  ! String needs allocatable len
    end type allocation_info_t

    ! Monomorphic type - simplified to avoid circular dependency
    type :: mono_type_t
        integer :: kind  ! TVAR, TINT, TREAL, etc.
        type(type_var_t) :: var  ! for TVAR
        type(mono_type_t), allocatable :: args(:)  ! for TFUN (arg, result),
        ! TARRAY (element type)
        integer :: size  ! for TCHAR(len=size), TARRAY(size)
        type(allocation_info_t) :: alloc_info  ! Memory allocation attributes
    contains
        procedure :: equals => mono_type_equals
        procedure :: to_string => mono_type_to_string
        procedure :: deep_copy => mono_type_deep_copy
        procedure :: assign => mono_type_assign
        generic :: assignment(=) => assign
    end type mono_type_t

    ! Polymorphic type (type scheme)
    type :: poly_type_t
        type(type_var_t), allocatable :: forall (:)  ! quantified variables
        type(mono_type_t) :: mono  ! the monomorphic type
    contains
        procedure :: to_string => poly_type_to_string
        procedure :: deep_copy => poly_type_deep_copy
        procedure :: assign => poly_type_assign
        generic :: assignment(=) => assign
    end type poly_type_t

    ! Type substitution (maps type variables to types)
    type :: substitution_t
        type(type_var_t), allocatable :: vars(:)
        type(mono_type_t), allocatable :: types(:)
        integer :: count = 0
    contains
        procedure :: add => subst_add
        procedure :: lookup => subst_lookup
        procedure :: apply => subst_apply_to_mono
        procedure :: apply_to_poly => subst_apply_to_poly
        procedure :: deep_copy => subst_deep_copy
        procedure :: assign => substitution_assign
        generic :: assignment(=) => assign
    end type substitution_t

    ! Type environment (maps identifiers to type schemes)
    type :: type_env_t
        character(len=:), allocatable :: names(:)
        type(poly_type_t), allocatable :: schemes(:)
        integer :: count = 0
        integer :: capacity = 0
    contains
        procedure :: lookup => env_lookup
        procedure :: extend => env_extend
        procedure :: extend_many => env_extend_many
        procedure :: remove => env_remove
        procedure :: apply_subst => env_apply_subst
        procedure :: deep_copy => env_deep_copy
        procedure :: assign => type_env_assign
        generic :: assignment(=) => assign
    end type type_env_t

contains


    ! Constructor for type variable
    function create_type_var(id, name) result(tv)
        integer, intent(in) :: id
        character(len=*), intent(in), optional :: name
        type(type_var_t) :: tv

        tv%id = id
        if (present(name)) then
            tv%name = name
        else
            ! Generate name from id: 'a, 'b, ..., 'z, 'a1, 'b1, ...
            if (id <= 26) then
                tv%name = "'"//achar(iachar('a') + id - 1)
            else
                block
                    character(len=10) :: num_str
                    write (num_str, '(i0)') (id - 1)/26
                    tv%name = "'"//achar(iachar('a') + mod(id - 1, 26))//trim(num_str)
                end block
            end if
        end if
    end function create_type_var

    ! Constructor for monomorphic type
    function create_mono_type(kind, var, args, char_size) result(result_type)
        integer, intent(in) :: kind
        type(type_var_t), intent(in), optional :: var
        type(mono_type_t), intent(in), optional :: args(:)
        integer, intent(in), optional :: char_size
        type(mono_type_t) :: result_type
        integer :: i

        result_type%kind = kind

        if (present(var)) then
            result_type%var = var
        else
            ! Initialize var field even for non-TVAR types to avoid undefined behavior
            result_type%var%id = -1
            allocate (character(len=0) :: result_type%var%name)
        end if

        if (present(args)) then
            allocate (result_type%args(size(args)))
            do i = 1, size(args)
                result_type%args(i) = args(i)
            end do
        end if

        if (present(char_size)) result_type%size = char_size

        ! Set defaults
        if (kind == TCHAR .and. .not. present(char_size)) result_type%size = 1
        if (kind == TARRAY) result_type%size = 0  ! arrays don't use size field same way

    end function create_mono_type

    ! Constructor for polymorphic type
    function create_poly_type(forall_vars, mono) result(pt)
        type(type_var_t), intent(in) :: forall_vars(:)
        type(mono_type_t), intent(in) :: mono
        type(poly_type_t) :: pt

        if (size(forall_vars) > 0) then
            allocate (pt%forall(size(forall_vars)))
            pt%forall = forall_vars
        end if
        pt%mono = mono  ! Uses default assignment
    end function create_poly_type

    ! Helper function to create function type with two arguments
    function create_fun_type(arg_type, result_type) result(fun_type)
        type(mono_type_t), intent(in) :: arg_type, result_type
        type(mono_type_t) :: fun_type

        fun_type%kind = TFUN
        fun_type%size = 0
        ! Initialize var field to avoid undefined behavior
        fun_type%var%id = -1
        allocate (character(len=0) :: fun_type%var%name)
        allocate (fun_type%args(2))
        fun_type%args(1) = arg_type  ! Uses default assignment
        fun_type%args(2) = result_type  ! Uses default assignment
    end function create_fun_type

    ! Check if two monomorphic types are equal
    logical function mono_type_equals(this, other) result(equal)
        class(mono_type_t), intent(in) :: this, other
        integer :: i

        equal = .false.

        if (this%kind /= other%kind) return

        select case (this%kind)
        case (TVAR)
            equal = (this%var%id == other%var%id)
        case (TINT, TREAL)
            equal = .true.
        case (TCHAR)
            equal = (this%size == other%size)
        case (TFUN, TARRAY)
            if (.not. allocated(this%args) .or. .not. allocated(other%args)) then
                equal = .false.
                return
            end if
            if (size(this%args) /= size(other%args)) return
            equal = .true.
            ! Simple non-recursive equality check
            do i = 1, size(this%args)
                if (this%args(i)%kind /= other%args(i)%kind) then
                    equal = .false.
                    return
                end if
                ! Check basic fields for equality
                if (this%args(i)%kind == TVAR) then
                    if (this%args(i)%var%id /= other%args(i)%var%id) then
                        equal = .false.
                        return
                    end if
                else if (this%args(i)%kind == TCHAR) then
                    if (this%args(i)%size /= other%args(i)%size) then
                        equal = .false.
                        return
                    end if
                end if
            end do
            if (this%kind == TARRAY) then
                equal = equal .and. (this%size == other%size)
            end if
        end select
    end function mono_type_equals

    ! Convert monomorphic type to string
    function mono_type_to_string(this) result(str)
        class(mono_type_t), intent(in) :: this
        character(len=:), allocatable :: str

        select case (this%kind)
        case (TVAR)
            str = this%var%name
        case (TINT)
            str = "integer"
        case (TREAL)
            str = "real(8)"
        case (TCHAR)
            if (this%size > 0) then
                block
                    character(len=20) :: size_str
                    write (size_str, '(i0)') this%size
                    str = "character(len="//trim(size_str)//")"
                end block
            else
                str = "character(*)"
            end if
        case (TFUN)
            if (allocated(this%args) .and. size(this%args) >= 2) then
                ! Simple non-recursive string representation
                block
                    character(len=:), allocatable :: arg1_str, arg2_str

                    ! Get string for first argument
                    select case (this%args(1)%kind)
                    case (TVAR)
                        arg1_str = this%args(1)%var%name
                    case (TINT)
                        arg1_str = "integer"
                    case (TREAL)
                        arg1_str = "real"
                    case (TCHAR)
                        arg1_str = "character"
                    case default
                        arg1_str = "unknown"
                    end select

                    ! Get string for second argument
                    select case (this%args(2)%kind)
                    case (TVAR)
                        arg2_str = this%args(2)%var%name
                    case (TINT)
                        arg2_str = "integer"
                    case (TREAL)
                        arg2_str = "real"
                    case (TCHAR)
                        arg2_str = "character"
                    case default
                        arg2_str = "unknown"
                    end select

                    str = arg1_str//" -> "//arg2_str
                end block
            else
                str = "function"
            end if
        case (TARRAY)
            if (allocated(this%args) .and. size(this%args) >= 1) then
                ! Simple non-recursive string representation
                block
                    character(len=:), allocatable :: elem_str

                    ! Get string for array element type
                    select case (this%args(1)%kind)
                    case (TVAR)
                        elem_str = this%args(1)%var%name
                    case (TINT)
                        elem_str = "integer"
                    case (TREAL)
                        elem_str = "real"
                    case (TCHAR)
                        elem_str = "character"
                    case default
                        elem_str = "unknown"
                    end select

                    if (this%size > 0) then
                        block
                            character(len=20) :: size_str
                            write (size_str, '(i0)') this%size
                            str = elem_str//"("//trim(size_str)//")"
                        end block
                    else
                        str = elem_str//"(:)"
                    end if
                end block
            else
                str = "array"
            end if
        case default
            str = "<unknown type>"
        end select
    end function mono_type_to_string

    ! Deep copy a monomorphic type (using default assignment)
    function mono_type_deep_copy(this) result(copy)
        class(mono_type_t), intent(in) :: this
        type(mono_type_t) :: copy

        ! Use Fortran's default assignment behavior
        copy = this
    end function mono_type_deep_copy


    ! Convert polymorphic type to string
    function poly_type_to_string(this) result(str)
        class(poly_type_t), intent(in) :: this
        character(len=:), allocatable :: str
        integer :: i

        if (allocated(this%forall) .and. size(this%forall) > 0) then
            str = "forall"
            do i = 1, size(this%forall)
                str = str//" "//this%forall(i)%name
            end do
            str = str//". "//this%mono%to_string()
        else
            str = this%mono%to_string()
        end if
    end function poly_type_to_string

    ! Deep copy a polymorphic type to avoid shared ownership issues
    function poly_type_deep_copy(this) result(copy)
        class(poly_type_t), intent(in) :: this
        type(poly_type_t) :: copy

        ! Use Fortran's default assignment behavior
        copy = this
    end function poly_type_deep_copy

    ! Add a substitution
    subroutine subst_add(this, var, typ)
        class(substitution_t), intent(inout) :: this
        type(type_var_t), intent(in) :: var
        type(mono_type_t), intent(in) :: typ
        type(type_var_t), allocatable :: temp_vars(:)
        type(mono_type_t), allocatable :: temp_types(:)
        integer :: i

        ! Grow arrays if needed
        if (this%count == 0) then
            allocate (this%vars(10))
            allocate (this%types(10))
        else if (this%count >= size(this%vars)) then
            allocate (temp_vars(size(this%vars)*2))
            allocate (temp_types(size(this%types)*2))
            temp_vars(1:this%count) = this%vars(1:this%count)
            ! Deep copy mono types to avoid shared references
            do i = 1, this%count
                temp_types(i) = this%types(i)  ! Uses default assignment
            end do
            ! Use move_alloc for O(1) array transfer - deep copy already done above
            call move_alloc(temp_vars, this%vars)
            call move_alloc(temp_types, this%types)
        end if

        this%count = this%count + 1
        this%vars(this%count) = var
        this%types(this%count) = typ
    end subroutine subst_add

    ! Lookup a type variable in substitution
    subroutine subst_lookup(this, var, typ)
        class(substitution_t), intent(in) :: this
        type(type_var_t), intent(in) :: var
        type(mono_type_t), allocatable, intent(out) :: typ
        integer :: i

        ! intent(out) automatically deallocates typ on entry

        ! Safety check
        if (var%id < 0) then
            ! Invalid var, return nothing
            return
        end if

        do i = 1, this%count
            if (this%vars(i)%id == var%id) then
                allocate (typ)
                typ = this%types(i)
                return
            end if
        end do
    end subroutine subst_lookup

    ! Apply substitution to monomorphic type
    subroutine subst_apply_to_mono(this, typ, result_typ)
        class(substitution_t), intent(in) :: this
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t), intent(inout) :: result_typ
        type(mono_type_t), allocatable :: lookup_result
        integer :: i

        ! Initialize result to avoid undefined behavior
        result_typ = typ

        select case (typ%kind)
        case (TVAR)
            ! Apply substitution to type variable
            call this%lookup(typ%var, lookup_result)
            if (allocated(lookup_result)) then
                result_typ = lookup_result
            end if

        case (TFUN, TARRAY)
            ! Simple non-recursive substitution - just handle direct args
            if (allocated(result_typ%args)) then
                do i = 1, size(result_typ%args)
                    ! Simple case: if arg is a type variable, look it up
                    if (result_typ%args(i)%kind == TVAR) then
                        block
                            type(mono_type_t), allocatable :: lookup_result
                            ! Defensive check: ensure var is properly initialized
   if (allocated(result_typ%args(i)%var%name) .and. result_typ%args(i)%var%id >= 0) then
                                call this%lookup(result_typ%args(i)%var, lookup_result)
                                if (allocated(lookup_result)) then
                                    result_typ%args(i) = lookup_result
                                end if
                            end if
                        end block
                    end if
                end do
            end if

        case default
            ! Already deep copied, nothing more to do
        end select
    end subroutine subst_apply_to_mono

    ! Apply substitution to polymorphic type
    subroutine subst_apply_to_poly(this, scheme, result_scheme)
        class(substitution_t), intent(in) :: this
        type(poly_type_t), intent(in) :: scheme
        type(poly_type_t), intent(inout) :: result_scheme
        type(substitution_t) :: filtered_subst
        integer :: i, j
        logical :: should_include

        ! Filter out substitutions for quantified variables
        filtered_subst%count = 0
        do i = 1, this%count
            should_include = .true.
            if (allocated(scheme%forall)) then
                do j = 1, size(scheme%forall)
                    if (this%vars(i)%id == scheme%forall(j)%id) then
                        should_include = .false.
                        exit
                    end if
                end do
            end if
            if (should_include) then
                call filtered_subst%add(this%vars(i), this%types(i))
            end if
        end do

        ! Apply filtered substitution
        if (allocated(scheme%forall)) then
            allocate (result_scheme%forall(size(scheme%forall)))
            result_scheme%forall = scheme%forall
        end if
        call filtered_subst%apply(scheme%mono, result_scheme%mono)
    end subroutine subst_apply_to_poly

    ! Apply substitution (general interface)
    function apply_substitution(subst, typ) result(result_typ)
        type(substitution_t), intent(in) :: subst
        class(*), intent(in) :: typ
        type(mono_type_t) :: result_typ

        select type (typ)
        type is (mono_type_t)
            call subst%apply(typ, result_typ)
        class default
            error stop "apply_substitution: unsupported type"
        end select
    end function apply_substitution

    ! Compose two substitutions
    function compose_substitutions(s1, s2) result(comp)
        type(substitution_t), intent(in) :: s1, s2
        type(substitution_t) :: comp
        integer :: i
        type(mono_type_t) :: applied_type
        logical :: found
        integer :: j

        ! First apply s1 to all types in s2
        do i = 1, s2%count
            call s1%apply(s2%types(i), applied_type)
            call comp%add(s2%vars(i), applied_type)
        end do

        ! Then add all mappings from s1 that are not in s2
        do i = 1, s1%count
            found = .false.
            do j = 1, s2%count
                if (s1%vars(i)%id == s2%vars(j)%id) then
                    found = .true.
                    exit
                end if
            end do
            if (.not. found) then
                call comp%add(s1%vars(i), s1%types(i))
            end if
        end do
    end function compose_substitutions

    ! Deep copy a substitution
    function subst_deep_copy(this) result(copy)
        class(substitution_t), intent(in) :: this
        type(substitution_t) :: copy
        integer :: i

        copy%count = this%count
        if (this%count > 0 .and. allocated(this%vars)) then
            allocate (copy%vars(size(this%vars)))
            allocate (copy%types(size(this%types)))
            do i = 1, this%count
                copy%vars(i) = this%vars(i)
                copy%types(i) = this%types(i)  ! Uses mono_type assignment (deep copy)
            end do
        end if
    end function subst_deep_copy


    ! Occurs check - check if variable occurs in type
    logical function occurs_check(var, typ) result(occurs)
        type(type_var_t), intent(in) :: var
        type(mono_type_t), intent(in) :: typ
        integer :: i

        occurs = .false.

        select case (typ%kind)
        case (TVAR)
            occurs = (var%id == typ%var%id)
        case (TFUN, TARRAY)
            ! Simple non-recursive check - just check direct args
            if (allocated(typ%args)) then
                do i = 1, size(typ%args)
                   if (typ%args(i)%kind == TVAR .and. var%id == typ%args(i)%var%id) then
                        occurs = .true.
                        return
                    end if
                end do
            end if
        end select
    end function occurs_check

    ! Get free type variables in a type
    subroutine free_type_vars(typ, vars)
        type(mono_type_t), intent(in) :: typ
        type(type_var_t), allocatable, intent(out) :: vars(:)
        type(type_var_t), allocatable :: temp_vars(:)
        integer :: count, i, j
        logical :: found

        ! intent(out) automatically deallocates vars on entry

        allocate (temp_vars(100))  ! Temporary storage
        count = 0

        call collect_vars(typ)

        ! Return exact size array
        if (count > 0) then
            allocate (vars(count))
            vars = temp_vars(1:count)
        else
            allocate (vars(0))
        end if

    contains
        recursive subroutine collect_vars(t)
            type(mono_type_t), intent(in) :: t
            integer :: k

            select case (t%kind)
            case (TVAR)
                ! Check if already collected
                found = .false.
                do j = 1, count
                    if (temp_vars(j)%id == t%var%id) then
                        found = .true.
                        exit
                    end if
                end do
                if (.not. found) then
                    count = count + 1
                    temp_vars(count) = t%var
                end if
            case (TFUN, TARRAY)
                ! Simple non-recursive collection - just collect direct args
                if (allocated(t%args)) then
                    do k = 1, size(t%args)
                        if (t%args(k)%kind == TVAR) then
                            ! Check if already collected
                            found = .false.
                            do j = 1, count
                                if (temp_vars(j)%id == t%args(k)%var%id) then
                                    found = .true.
                                    exit
                                end if
                            end do
                            if (.not. found) then
                                count = count + 1
                                temp_vars(count) = t%args(k)%var
                            end if
                        end if
                    end do
                end if
            end select
        end subroutine collect_vars
    end subroutine free_type_vars

    ! Type environment lookup
    subroutine env_lookup(this, name, scheme)
        class(type_env_t), intent(in) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), allocatable, intent(out) :: scheme
        integer :: i

        ! intent(out) automatically deallocates scheme on entry

        ! Safety check: ensure arrays are allocated
        if (.not. allocated(this%names) .or. .not. allocated(this%schemes)) then
            return
        end if

        ! Early return for empty environment
        if (this%count == 0) then
            return
        end if

        do i = 1, this%count
            if (this%names(i) == name) then
                scheme = this%schemes(i)  ! Uses default assignment
                return
            end if
        end do

        ! Scheme not found - scheme remains unallocated
    end subroutine env_lookup

    ! Extend type environment with single binding
    subroutine env_extend(this, name, scheme)
        class(type_env_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), intent(in) :: scheme
        character(len=:), allocatable :: temp_names(:)
        type(poly_type_t), allocatable :: temp_schemes(:)
        integer :: new_capacity, i

        ! Initialize or grow arrays if needed
        if (this%capacity == 0) then
            this%capacity = 10
            allocate (character(len=256) :: this%names(this%capacity))
            allocate (this%schemes(this%capacity))
        else if (this%count >= this%capacity) then
            new_capacity = this%capacity*2
            allocate (character(len=256) :: temp_names(new_capacity))
            allocate (temp_schemes(new_capacity))
            do i = 1, this%count
                temp_names(i) = this%names(i)
                temp_schemes(i) = this%schemes(i)  ! Uses default assignment
            end do
            ! Replace move_alloc with explicit deallocation and reallocation
            deallocate (this%names)
            deallocate (this%schemes)
            allocate (character(len=256) :: this%names(new_capacity))
            allocate (this%schemes(new_capacity))
            do i = 1, this%count
                this%names(i) = temp_names(i)
                this%schemes(i) = temp_schemes(i)
            end do
            this%capacity = new_capacity
        end if

        ! Add new binding (use deep copy to avoid shared ownership)
        this%count = this%count + 1
        this%names(this%count) = name
        this%schemes(this%count) = scheme  ! Uses default assignment
    end subroutine env_extend

    ! Extend type environment with multiple bindings
    subroutine env_extend_many(this, names, schemes)
        class(type_env_t), intent(inout) :: this
        character(len=*), intent(in) :: names(:)
        type(poly_type_t), intent(in) :: schemes(:)
        integer :: i

        do i = 1, size(names)
            call this%extend(names(i), schemes(i))
        end do
    end subroutine env_extend_many

    ! Remove binding from environment
    subroutine env_remove(this, name, new_env)
        class(type_env_t), intent(in) :: this
        character(len=*), intent(in) :: name
        type(type_env_t), intent(inout) :: new_env
        integer :: i

        new_env%capacity = this%capacity
        if (new_env%capacity > 0) then
            allocate (character(len=256) :: new_env%names(new_env%capacity))
            allocate (new_env%schemes(new_env%capacity))
        end if

        new_env%count = 0
        do i = 1, this%count
            if (this%names(i) /= name) then
                new_env%count = new_env%count + 1
                new_env%names(new_env%count) = this%names(i)
                new_env%schemes(new_env%count) = this%schemes(i)
                ! Uses default assignment
            end if
        end do
    end subroutine env_remove

    ! Apply substitution to environment
    subroutine env_apply_subst(this, subst, new_env)
        class(type_env_t), intent(in) :: this
        type(substitution_t), intent(in) :: subst
        type(type_env_t), intent(inout) :: new_env
        integer :: i

        new_env%capacity = this%capacity
        new_env%count = this%count

        if (new_env%capacity > 0) then
            allocate (character(len=256) :: new_env%names(new_env%capacity))
            allocate (new_env%schemes(new_env%capacity))

            do i = 1, this%count
                new_env%names(i) = this%names(i)
                call subst%apply_to_poly(this%schemes(i), new_env%schemes(i))
            end do
        end if
    end subroutine env_apply_subst

    ! Deep copy a type environment
    function env_deep_copy(this) result(copy)
        class(type_env_t), intent(in) :: this
        type(type_env_t) :: copy
        integer :: i

        copy%count = this%count
        copy%capacity = this%capacity

        if (copy%capacity > 0) then
            allocate (character(len=256) :: copy%names(copy%capacity))
            allocate (copy%schemes(copy%capacity))

            do i = 1, this%count
                copy%names(i) = this%names(i)
                copy%schemes(i) = this%schemes(i)
                ! Uses poly_type assignment (deep copy)
            end do
        end if
    end function env_deep_copy



    ! Assignment operators for memory safety
    
    ! Type variable assignment - safe because only allocates string
    subroutine type_var_assign(lhs, rhs)
        class(type_var_t), intent(inout) :: lhs
        type(type_var_t), intent(in) :: rhs
        
        ! Self-assignment check not possible with polymorphic arguments
        ! Fortran will handle this safely with proper deep copying
        
        lhs%id = rhs%id
        if (allocated(rhs%name)) then
            lhs%name = rhs%name  ! Fortran handles string allocation automatically
        else
            if (allocated(lhs%name)) deallocate(lhs%name)
        end if
    end subroutine type_var_assign
    
    ! Mono type assignment - cycle-safe with depth limiting
    subroutine mono_type_assign(lhs, rhs)
        class(mono_type_t), intent(inout) :: lhs
        type(mono_type_t), intent(in) :: rhs
        integer, parameter :: MAX_DEPTH = 50  ! Prevent stack overflow
        
        ! Self-assignment check not possible with polymorphic arguments
        ! Fortran will handle this safely with proper deep copying
        
        call mono_type_copy_recursive(lhs, rhs, 0)
    end subroutine mono_type_assign
    
    ! Recursive helper for mono type copying with depth limiting
    recursive subroutine mono_type_copy_recursive(lhs, rhs, depth)
        type(mono_type_t), intent(inout) :: lhs
        type(mono_type_t), intent(in) :: rhs
        integer, intent(in) :: depth
        integer :: i
        integer, parameter :: MAX_DEPTH = 50
        
        ! Prevent infinite recursion
        if (depth > MAX_DEPTH) then
            ! For deep structures, do shallow copy to break recursion
            lhs%kind = rhs%kind
            lhs%var = rhs%var  ! Uses type_var assignment
            lhs%size = rhs%size
            lhs%alloc_info = rhs%alloc_info
            ! Skip copying args to prevent infinite recursion
            if (allocated(lhs%args)) deallocate(lhs%args)
            return
        end if
        
        ! Copy basic fields
        lhs%kind = rhs%kind
        lhs%var = rhs%var  ! Uses type_var assignment
        lhs%size = rhs%size
        lhs%alloc_info = rhs%alloc_info
        
        ! Deep copy args array if present
        if (allocated(rhs%args)) then
            if (allocated(lhs%args)) deallocate(lhs%args)
            allocate(lhs%args(size(rhs%args)))
            do i = 1, size(rhs%args)
                call mono_type_copy_recursive(lhs%args(i), rhs%args(i), depth + 1)
            end do
        else
            if (allocated(lhs%args)) deallocate(lhs%args)
        end if
    end subroutine mono_type_copy_recursive
    
    ! Polymorphic type assignment
    subroutine poly_type_assign(lhs, rhs)
        class(poly_type_t), intent(inout) :: lhs
        type(poly_type_t), intent(in) :: rhs
        
        ! Self-assignment check not possible with polymorphic arguments
        ! Fortran will handle this safely with proper deep copying
        
        ! Copy forall variables
        if (allocated(rhs%forall)) then
            if (allocated(lhs%forall)) deallocate(lhs%forall)
            allocate(lhs%forall(size(rhs%forall)))
            lhs%forall = rhs%forall  ! Uses type_var assignment
        else
            if (allocated(lhs%forall)) deallocate(lhs%forall)
        end if
        
        ! Copy mono type
        lhs%mono = rhs%mono  ! Uses mono_type assignment
    end subroutine poly_type_assign
    
    ! Substitution assignment
    subroutine substitution_assign(lhs, rhs)
        class(substitution_t), intent(inout) :: lhs
        type(substitution_t), intent(in) :: rhs
        integer :: i
        
        ! Self-assignment check not possible with polymorphic arguments
        ! Fortran will handle this safely with proper deep copying
        
        lhs%count = rhs%count
        
        ! Copy arrays
        if (allocated(rhs%vars)) then
            if (allocated(lhs%vars)) deallocate(lhs%vars)
            allocate(lhs%vars(size(rhs%vars)))
            lhs%vars = rhs%vars  ! Uses type_var assignment
        else
            if (allocated(lhs%vars)) deallocate(lhs%vars)
        end if
        
        if (allocated(rhs%types)) then
            if (allocated(lhs%types)) deallocate(lhs%types)
            allocate(lhs%types(size(rhs%types)))
            do i = 1, size(rhs%types)
                lhs%types(i) = rhs%types(i)  ! Uses mono_type assignment
            end do
        else
            if (allocated(lhs%types)) deallocate(lhs%types)
        end if
    end subroutine substitution_assign
    
    ! Type environment assignment
    subroutine type_env_assign(lhs, rhs)
        class(type_env_t), intent(inout) :: lhs
        type(type_env_t), intent(in) :: rhs
        integer :: i
        
        ! Self-assignment check not possible with polymorphic arguments
        ! Fortran will handle this safely with proper deep copying
        
        lhs%count = rhs%count
        lhs%capacity = rhs%capacity
        
        ! Copy names array
        if (allocated(rhs%names)) then
            if (allocated(lhs%names)) deallocate(lhs%names)
            allocate(character(len=256) :: lhs%names(size(rhs%names)))
            lhs%names = rhs%names
        else
            if (allocated(lhs%names)) deallocate(lhs%names)
        end if
        
        ! Copy schemes array
        if (allocated(rhs%schemes)) then
            if (allocated(lhs%schemes)) deallocate(lhs%schemes)
            allocate(lhs%schemes(size(rhs%schemes)))
            do i = 1, size(rhs%schemes)
                lhs%schemes(i) = rhs%schemes(i)  ! Uses poly_type assignment
            end do
        else
            if (allocated(lhs%schemes)) deallocate(lhs%schemes)
        end if
    end subroutine type_env_assign

end module type_system_hm
