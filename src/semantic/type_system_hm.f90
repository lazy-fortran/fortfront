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

    ! Forward declaration for shared type data
    type :: shared_type_data_t
        integer :: ref_count = 1
        integer :: kind  ! TVAR, TINT, TREAL, etc.
        type(type_var_t) :: var  ! for TVAR
        integer :: size  ! for TCHAR(len=size), TARRAY(size)
        type(allocation_info_t) :: alloc_info  ! Memory allocation attributes
        ! Function type arguments with reference tracking
        type(mono_type_t), allocatable :: args(:)  ! For TFUN and TARRAY
        logical :: has_cycles = .false.  ! Track if this type contains cycles
    end type shared_type_data_t
    
    ! Monomorphic type with reference-counted sharing
    type :: mono_type_t
        type(shared_type_data_t), pointer :: data => null()  ! Shared type data
    contains
        procedure :: equals => mono_type_equals
        procedure :: to_string => mono_type_to_string
        procedure :: deep_copy => mono_type_deep_copy
        procedure :: assign => mono_type_assign
        procedure :: clone => mono_type_clone  ! True deep copy without sharing
        procedure :: finalize => mono_type_finalize
        generic :: assignment(=) => assign
        final :: mono_type_final
    end type mono_type_t

    ! Polymorphic type (type scheme)
    type :: poly_type_t
        type(type_var_t), allocatable :: forall (:)  ! quantified variables
        type(mono_type_t) :: mono  ! the monomorphic type
    contains
        procedure :: to_string => poly_type_to_string
        procedure :: deep_copy => poly_type_deep_copy
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
    end type type_env_t

contains

    ! Initialize mono_type_t to safe state (no allocatable components)

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

    ! Constructor for monomorphic type with reference counting
    function create_mono_type(kind, var, args, char_size) result(result_type)
        integer, intent(in) :: kind
        type(type_var_t), intent(in), optional :: var
        type(mono_type_t), intent(in), optional :: args(:)
        integer, intent(in), optional :: char_size
        type(mono_type_t) :: result_type
        integer :: i

        ! Allocate new shared data
        allocate(result_type%data)
        result_type%data%ref_count = 1
        result_type%data%kind = kind
        result_type%data%has_cycles = .false.

        if (present(var)) then
            result_type%data%var = var
        else
            ! Initialize var field even for non-TVAR types to avoid undefined behavior
            result_type%data%var%id = -1
            result_type%data%var%name = ""
        end if

        ! Initialize size to valid default
        result_type%data%size = 0

        ! Initialize allocation info to defaults
        result_type%data%alloc_info%is_allocatable = .false.
        result_type%data%alloc_info%is_pointer = .false.
        result_type%data%alloc_info%is_target = .false.
        result_type%data%alloc_info%is_allocated = .false.
        result_type%data%alloc_info%needs_allocation_check = .false.
        result_type%data%alloc_info%needs_allocatable_string = .false.

        ! Handle nested args for function and array types
        if (present(args) .and. size(args) > 0) then
            allocate(result_type%data%args(size(args)))
            do i = 1, size(args)
                result_type%data%args(i) = args(i)  ! Reference-counted assignment
            end do
        end if

        if (present(char_size)) result_type%data%size = char_size

        ! Set defaults
        if (kind == TCHAR .and. .not. present(char_size)) result_type%data%size = 1
        if (kind == TARRAY) result_type%data%size = 0  ! arrays don't use size field same way

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

        ! Allocate new shared data
        allocate(fun_type%data)
        fun_type%data%ref_count = 1
        fun_type%data%kind = TFUN
        fun_type%data%size = 0
        fun_type%data%has_cycles = .false.
        
        ! Initialize var field to avoid undefined behavior
        fun_type%data%var%id = -1
        fun_type%data%var%name = ""
        
        ! Initialize allocation info to defaults
        fun_type%data%alloc_info%is_allocatable = .false.
        fun_type%data%alloc_info%is_pointer = .false.
        fun_type%data%alloc_info%is_target = .false.
        fun_type%data%alloc_info%is_allocated = .false.
        fun_type%data%alloc_info%needs_allocation_check = .false.
        fun_type%data%alloc_info%needs_allocatable_string = .false.
        
        ! Store function arguments properly for type checking
        allocate(fun_type%data%args(2))
        fun_type%data%args(1) = arg_type    ! argument type
        fun_type%data%args(2) = result_type ! return type
    end function create_fun_type

    ! Check if two monomorphic types are equal
    recursive logical function mono_type_equals(this, other) result(equal)
        class(mono_type_t), intent(in) :: this, other
        integer :: i

        equal = .false.

        ! Handle null pointers
        if (.not. associated(this%data) .or. .not. associated(other%data)) return
        
        ! Fast equality check - same data pointer
        if (associated(this%data, other%data)) then
            equal = .true.
            return
        end if

        if (this%data%kind /= other%data%kind) return

        select case (this%data%kind)
        case (TVAR)
            equal = (this%data%var%id == other%data%var%id)
        case (TINT, TREAL, TLOGICAL)
            equal = .true.
        case (TCHAR)
            equal = (this%data%size == other%data%size)
        case (TFUN)
            ! Function types must have matching argument structure
            equal = .true.
            if (allocated(this%data%args) .and. allocated(other%data%args)) then
                if (size(this%data%args) /= size(other%data%args)) then
                    equal = .false.
                else
                    do i = 1, size(this%data%args)
                        if (.not. this%data%args(i)%equals(other%data%args(i))) then
                            equal = .false.
                            exit
                        end if
                    end do
                end if
            else if (allocated(this%data%args) .neqv. allocated(other%data%args)) then
                equal = .false.
            end if
        case (TARRAY)
            equal = .true.
            if (this%data%size /= other%data%size) equal = .false.
            ! Check element type if present
            if (allocated(this%data%args) .and. allocated(other%data%args)) then
                if (size(this%data%args) > 0 .and. size(other%data%args) > 0) then
                    equal = equal .and. this%data%args(1)%equals(other%data%args(1))
                end if
            end if
        end select
    end function mono_type_equals

    ! Convert monomorphic type to string
    recursive function mono_type_to_string(this) result(str)
        class(mono_type_t), intent(in) :: this
        character(len=:), allocatable :: str

        if (.not. associated(this%data)) then
            str = "<null type>"
            return
        end if

        select case (this%data%kind)
        case (TVAR)
            str = this%data%var%name
        case (TINT)
            str = "integer"
        case (TREAL)
            str = "real(8)"
        case (TCHAR)
            if (this%data%size > 0) then
                block
                    character(len=20) :: size_str
                    write (size_str, '(i0)') this%data%size
                    str = "character(len="//trim(size_str)//")"
                end block
            else
                str = "character(*)"
            end if
        case (TFUN)
            if (allocated(this%data%args) .and. size(this%data%args) >= 2) then
                str = this%data%args(1)%to_string() // " -> " // this%data%args(2)%to_string()
            else
                str = "function"
            end if
        case (TARRAY)
            if (this%data%size > 0) then
                block
                    character(len=20) :: size_str
                    write (size_str, '(i0)') this%data%size
                    str = "array("//trim(size_str)//")"
                end block
            else
                str = "array(:)"
            end if
        case default
            str = "<unknown type>"
        end select
    end function mono_type_to_string

    ! Deep copy a monomorphic type (true deep copy without sharing)
    function mono_type_deep_copy(this) result(copy)
        class(mono_type_t), intent(in) :: this
        type(mono_type_t) :: copy

        ! Use true deep copy to create independent type
        copy = this%clone()
    end function mono_type_deep_copy
    
    ! Reference-counted assignment operator
    subroutine mono_type_assign(lhs, rhs)
        class(mono_type_t), intent(inout) :: lhs
        class(mono_type_t), intent(in) :: rhs

        ! If assigning to self, do nothing
        if (associated(lhs%data, rhs%data)) return
        
        ! Decrement reference count of old data
        call lhs%finalize()
        
        ! Share the new data and increment reference count
        if (associated(rhs%data)) then
            lhs%data => rhs%data
            lhs%data%ref_count = lhs%data%ref_count + 1
        else
            lhs%data => null()
        end if
    end subroutine mono_type_assign
    
    ! True deep copy without sharing (for breaking cycles)
    recursive function mono_type_clone(this) result(copy)
        class(mono_type_t), intent(in) :: this
        type(mono_type_t) :: copy
        integer :: i

        if (.not. associated(this%data)) then
            copy%data => null()
            return
        end if
        
        ! Allocate new independent data
        allocate(copy%data)
        copy%data%ref_count = 1
        copy%data%kind = this%data%kind
        copy%data%var = this%data%var  ! Deep copy var fields
        copy%data%size = this%data%size
        copy%data%alloc_info = this%data%alloc_info
        copy%data%has_cycles = .false.  ! Reset cycle detection
        
        ! Deep copy args array if present
        if (allocated(this%data%args)) then
            allocate(copy%data%args(size(this%data%args)))
            do i = 1, size(this%data%args)
                copy%data%args(i) = this%data%args(i)%clone()  ! Recursive clone
            end do
        end if
    end function mono_type_clone
    
    ! Finalize: decrement reference count and deallocate if needed
    subroutine mono_type_finalize(this)
        class(mono_type_t), intent(inout) :: this
        
        if (associated(this%data)) then
            this%data%ref_count = this%data%ref_count - 1
            if (this%data%ref_count <= 0) then
                ! Deallocate args before deallocating data to avoid cycles
                if (allocated(this%data%args)) then
                    deallocate(this%data%args)
                end if
                deallocate(this%data)
            end if
            this%data => null()
        end if
    end subroutine mono_type_finalize
    
    ! Final cleanup procedure
    subroutine mono_type_final(this)
        type(mono_type_t), intent(inout) :: this
        
        call this%finalize()
    end subroutine mono_type_final


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
    recursive subroutine subst_apply_to_mono(this, typ, result_typ)
        class(substitution_t), intent(in) :: this
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t), intent(inout) :: result_typ
        type(mono_type_t), allocatable :: lookup_result
        integer :: i

        ! Initialize result to avoid undefined behavior
        result_typ = typ
        
        if (.not. associated(typ%data)) return

        select case (typ%data%kind)
        case (TVAR)
            ! Apply substitution to type variable
            call this%lookup(typ%data%var, lookup_result)
            if (allocated(lookup_result)) then
                result_typ = lookup_result
            end if

        case (TFUN, TARRAY)
            ! Apply substitution to nested arguments - need to clone first
            result_typ = typ%clone()  ! Start with independent copy
            if (allocated(typ%data%args) .and. associated(result_typ%data)) then
                do i = 1, size(typ%data%args)
                    call this%apply(typ%data%args(i), result_typ%data%args(i))
                end do
            end if

        case default
            ! Already copied, nothing more to do
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
    recursive logical function occurs_check(var, typ) result(occurs)
        type(type_var_t), intent(in) :: var
        type(mono_type_t), intent(in) :: typ
        integer :: i

        occurs = .false.
        
        if (.not. associated(typ%data)) return

        select case (typ%data%kind)
        case (TVAR)
            occurs = (var%id == typ%data%var%id)
        case (TFUN, TARRAY)
            ! Check in nested arguments
            if (allocated(typ%data%args)) then
                do i = 1, size(typ%data%args)
                    if (occurs_check(var, typ%data%args(i))) then
                        occurs = .true.
                        exit
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
            
            if (.not. associated(t%data)) return

            select case (t%data%kind)
            case (TVAR)
                ! Check if already collected
                found = .false.
                do j = 1, count
                    if (temp_vars(j)%id == t%data%var%id) then
                        found = .true.
                        exit
                    end if
                end do
                if (.not. found) then
                    count = count + 1
                    temp_vars(count) = t%data%var
                end if
            case (TFUN, TARRAY)
                ! Collect variables from nested arguments
                if (allocated(t%data%args)) then
                    do k = 1, size(t%data%args)
                        call collect_vars(t%data%args(k))
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



end module type_system_hm
