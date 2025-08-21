module type_system_hm
    ! Minimal Hindley-Milner type system with GCC 15.2.1 compatibility
    ! This is a simplified version that avoids "allocate source=" patterns
    implicit none
    private

    ! Public types and interfaces
    public :: type_var_t, mono_type_t, poly_type_t, type_env_t, substitution_t
    public :: allocation_info_t
    public :: create_type_var, create_mono_type, create_poly_type, create_fun_type
    public :: apply_substitution, compose_substitutions
    public :: occurs_check, free_type_vars

    ! Type kinds
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
        character(len=:), allocatable :: name
    contains
        procedure :: assign => type_var_assign
        generic :: assignment(=) => assign
    end type type_var_t

    ! Memory allocation attributes
    type :: allocation_info_t
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_target = .false.
        logical :: is_allocated = .false.
        logical :: needs_allocation_check = .false.
        logical :: needs_allocatable_string = .false.
    end type allocation_info_t

    ! Monomorphic type
    type :: mono_type_t
        integer :: kind
        type(type_var_t) :: var
        type(mono_type_t), allocatable :: args(:)
        integer :: size
        type(allocation_info_t) :: alloc_info
    contains
        procedure :: to_string => mono_type_to_string
        procedure :: assign => mono_type_assign
        generic :: assignment(=) => assign
    end type mono_type_t

    ! Polymorphic type scheme
    type :: poly_type_t
        type(type_var_t), allocatable :: forall_vars(:)
        type(mono_type_t) :: mono
    contains
        procedure :: to_string => poly_type_to_string
        procedure :: assign => poly_type_assign
        procedure :: deep_copy => poly_type_deep_copy
        procedure :: init => poly_type_init
        generic :: assignment(=) => assign
    end type poly_type_t

    ! Type substitution 
    type :: substitution_entry_t
        integer :: var_id
        type(mono_type_t) :: replacement
    contains
        procedure :: assign => subst_entry_assign
        generic :: assignment(=) => assign
    end type substitution_entry_t

    type :: substitution_t
        type(substitution_entry_t), allocatable :: entries(:)
        type(type_var_t), allocatable :: vars(:)           ! For compatibility
        type(mono_type_t), allocatable :: types(:)         ! For compatibility
        integer :: count = 0
    contains
        procedure :: add => subst_add
        procedure :: lookup => subst_lookup
        procedure :: apply => subst_apply_to_mono
        procedure :: apply_to_mono => subst_apply_to_mono
        procedure :: apply_to_poly => subst_apply_to_poly
        procedure :: deep_copy => subst_deep_copy
        procedure :: assign => subst_assign
        generic :: assignment(=) => assign
    end type substitution_t

    ! Type environment
    type, public :: type_env_t
        character(len=256), allocatable :: names(:)
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
        procedure :: assign => env_assign
        generic :: assignment(=) => assign
    end type type_env_t

contains

    subroutine type_var_assign(lhs, rhs)
        class(type_var_t), intent(inout) :: lhs
        type(type_var_t), intent(in) :: rhs
        lhs%id = rhs%id
        if (allocated(rhs%name)) then
            lhs%name = rhs%name
        else
            if (allocated(lhs%name)) deallocate(lhs%name)
        end if
    end subroutine type_var_assign

    function create_type_var(id, name) result(tv)
        integer, intent(in) :: id
        character(*), intent(in), optional :: name
        type(type_var_t) :: tv
        tv%id = id
        if (present(name)) then
            tv%name = name
        end if
    end function create_type_var

    function create_mono_type(kind, var, args, char_size) result(result_type)
        integer, intent(in) :: kind
        type(type_var_t), intent(in), optional :: var
        type(mono_type_t), intent(in), optional :: args(:)
        integer, intent(in), optional :: char_size
        type(mono_type_t) :: result_type
        
        result_type%kind = kind
        if (present(var)) then
            result_type%var = var
        end if
        if (present(args)) then
            result_type%args = args  ! Simple assignment, not allocate source=
        end if
        if (present(char_size)) then
            result_type%size = char_size
        else
            result_type%size = -1
        end if
    end function create_mono_type

    function create_poly_type(forall_vars, mono) result(pt)
        type(type_var_t), intent(in) :: forall_vars(:)
        type(mono_type_t), intent(in) :: mono
        type(poly_type_t) :: pt
        if (size(forall_vars) > 0) then
            pt%forall_vars = forall_vars  ! Simple assignment
        end if
        pt%mono = mono
    end function create_poly_type

    function create_fun_type(arg_type, result_type) result(fun_type)
        type(mono_type_t), intent(in) :: arg_type, result_type
        type(mono_type_t) :: fun_type
        fun_type%kind = TFUN
        allocate(fun_type%args(2))
        fun_type%args(1) = arg_type
        fun_type%args(2) = result_type
    end function create_fun_type

    function mono_type_to_string(this) result(str)
        class(mono_type_t), intent(in) :: this
        character(len=:), allocatable :: str
        str = "type"  ! Simplified implementation
    end function mono_type_to_string

    subroutine mono_type_assign(lhs, rhs)
        class(mono_type_t), intent(inout) :: lhs
        type(mono_type_t), intent(in) :: rhs
        lhs%kind = rhs%kind
        lhs%var = rhs%var
        lhs%size = rhs%size
        lhs%alloc_info = rhs%alloc_info
        if (allocated(rhs%args)) then
            lhs%args = rhs%args  ! Simple assignment, not allocate source=
        else
            if (allocated(lhs%args)) deallocate(lhs%args)
        end if
    end subroutine mono_type_assign

    function poly_type_to_string(this) result(str)
        class(poly_type_t), intent(in) :: this
        character(len=:), allocatable :: str
        str = this%mono%to_string()
    end function poly_type_to_string

    function poly_type_deep_copy(this) result(copy)
        class(poly_type_t), intent(in) :: this
        type(poly_type_t) :: copy
        if (allocated(this%forall_vars)) then
            copy%forall_vars = this%forall_vars
        end if
        copy%mono = this%mono
    end function poly_type_deep_copy

    subroutine poly_type_assign(lhs, rhs)
        class(poly_type_t), intent(inout) :: lhs
        type(poly_type_t), intent(in) :: rhs
        if (allocated(rhs%forall_vars)) then
            lhs%forall_vars = rhs%forall_vars
        else
            if (allocated(lhs%forall_vars)) deallocate(lhs%forall_vars)
        end if
        lhs%mono = rhs%mono
    end subroutine poly_type_assign

    subroutine poly_type_init(this)
        class(poly_type_t), intent(inout) :: this
        if (allocated(this%forall_vars)) deallocate(this%forall_vars)
        this%mono%kind = TVAR
        this%mono%var = create_type_var(0)
        if (allocated(this%mono%args)) deallocate(this%mono%args)
        this%mono%size = -1
    end subroutine poly_type_init

    ! Substitution methods
    subroutine subst_entry_assign(lhs, rhs)
        class(substitution_entry_t), intent(inout) :: lhs
        type(substitution_entry_t), intent(in) :: rhs
        lhs%var_id = rhs%var_id
        lhs%replacement = rhs%replacement
    end subroutine subst_entry_assign

    subroutine subst_add(this, var, typ)
        class(substitution_t), intent(inout) :: this
        type(type_var_t), intent(in) :: var
        type(mono_type_t), intent(in) :: typ
        ! Simplified implementation - just increment count
        this%count = this%count + 1
    end subroutine subst_add

    subroutine subst_lookup(this, var, typ)
        class(substitution_t), intent(in) :: this
        type(type_var_t), intent(in) :: var
        type(mono_type_t), intent(out) :: typ
        ! Return original variable as type (no substitution found)
        typ%kind = TVAR
        typ%var = var
        if (allocated(typ%args)) deallocate(typ%args)
        typ%size = -1
    end subroutine subst_lookup

    subroutine subst_apply_to_mono(this, typ, result_typ)
        class(substitution_t), intent(in) :: this
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t), intent(out) :: result_typ
        result_typ = typ  ! No substitution for now
    end subroutine subst_apply_to_mono

    subroutine subst_apply_to_poly(this, scheme, result_scheme)
        class(substitution_t), intent(in) :: this
        type(poly_type_t), intent(in) :: scheme
        type(poly_type_t), intent(out) :: result_scheme
        result_scheme = scheme  ! No substitution for now
    end subroutine subst_apply_to_poly

    function apply_substitution(subst, typ) result(result_typ)
        type(substitution_t), intent(in) :: subst
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t) :: result_typ
        call subst%apply_to_mono(typ, result_typ)
    end function apply_substitution

    function compose_substitutions(s1, s2) result(comp)
        type(substitution_t), intent(in) :: s1, s2
        type(substitution_t) :: comp
        comp = s1  ! Simplified
    end function compose_substitutions

    function subst_deep_copy(this) result(copy)
        class(substitution_t), intent(in) :: this
        type(substitution_t) :: copy
        copy%count = this%count
        if (allocated(this%entries) .and. this%count > 0) then
            copy%entries = this%entries(1:this%count)
        end if
    end function subst_deep_copy

    subroutine subst_assign(lhs, rhs)
        class(substitution_t), intent(inout) :: lhs
        type(substitution_t), intent(in) :: rhs
        lhs%count = rhs%count
        if (allocated(rhs%entries) .and. rhs%count > 0) then
            lhs%entries = rhs%entries(1:rhs%count)
        else
            if (allocated(lhs%entries)) deallocate(lhs%entries)
        end if
    end subroutine subst_assign

    ! Environment methods
    subroutine env_lookup(this, name, scheme)
        class(type_env_t), intent(in) :: this
        character(*), intent(in) :: name
        type(poly_type_t), intent(out) :: scheme
        integer :: i
        logical :: found
        found = .false.
        if (allocated(this%names) .and. allocated(this%schemes)) then
            do i = 1, this%count
                if (this%names(i) == name) then
                    scheme = this%schemes(i)
                    found = .true.
                    exit
                end if
            end do
        end if
        if (.not. found) then
            call scheme%init()
        end if
    end subroutine env_lookup

    subroutine env_extend(this, name, scheme)
        class(type_env_t), intent(inout) :: this
        character(*), intent(in) :: name
        type(poly_type_t), intent(in) :: scheme
        ! Simplified - just increment count
        this%count = this%count + 1
    end subroutine env_extend

    subroutine env_extend_many(this, names, schemes)
        class(type_env_t), intent(inout) :: this
        character(*), intent(in) :: names(:)
        type(poly_type_t), intent(in) :: schemes(:)
        integer :: i
        do i = 1, size(names)
            call this%extend(names(i), schemes(i))
        end do
    end subroutine env_extend_many

    subroutine env_remove(this, name, new_env)
        class(type_env_t), intent(in) :: this
        character(*), intent(in) :: name
        type(type_env_t), intent(out) :: new_env
        new_env = this  ! Simplified
    end subroutine env_remove

    subroutine env_apply_subst(this, subst, new_env)
        class(type_env_t), intent(in) :: this
        type(substitution_t), intent(in) :: subst
        type(type_env_t), intent(out) :: new_env
        new_env = this  ! Simplified
    end subroutine env_apply_subst

    function env_deep_copy(this) result(copy)
        class(type_env_t), intent(in) :: this
        type(type_env_t) :: copy
        copy%count = this%count
        copy%capacity = this%capacity
        if (allocated(this%names) .and. this%count > 0) then
            copy%names = this%names
            copy%schemes = this%schemes
        end if
    end function env_deep_copy

    subroutine env_assign(lhs, rhs)
        class(type_env_t), intent(inout) :: lhs
        type(type_env_t), intent(in) :: rhs
        lhs%count = rhs%count
        lhs%capacity = rhs%capacity
        if (allocated(rhs%names) .and. rhs%count > 0) then
            lhs%names = rhs%names
            lhs%schemes = rhs%schemes
        else
            if (allocated(lhs%names)) deallocate(lhs%names)
            if (allocated(lhs%schemes)) deallocate(lhs%schemes)
        end if
    end subroutine env_assign

    ! Utility functions
    function occurs_check(var, typ) result(occurs)
        type(type_var_t), intent(in) :: var
        type(mono_type_t), intent(in) :: typ
        logical :: occurs
        occurs = .false.  ! Simplified
    end function occurs_check

    subroutine free_type_vars(typ, vars)
        type(mono_type_t), intent(in) :: typ
        type(type_var_t), allocatable, intent(inout) :: vars(:)
        ! Simplified - just allocate empty array
        if (allocated(vars)) deallocate(vars)
        allocate(vars(0))
    end subroutine free_type_vars

end module type_system_hm