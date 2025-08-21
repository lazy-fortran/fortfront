module type_system_hm_arena
    ! Drop-in replacement for type_system_hm using arena-based memory management
    ! Eliminates GCC 15.2.1 allocatable component segfaults while maintaining API compatibility
    use type_arena, only: type_ref_t, global_type_arena, init_type_arena, &
                          TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN, TARRAY
    use type_system_transition, only: init_transition_system
    implicit none
    private

    ! Re-export all types and constants with same names for drop-in compatibility
    public :: type_var_t, mono_type_t, poly_type_t, type_env_t, substitution_t
    public :: allocation_info_t
    public :: create_type_var, create_mono_type, create_poly_type, create_fun_type
    public :: apply_substitution, compose_substitutions
    public :: occurs_check, free_type_vars
    public :: TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN, TARRAY

    ! Type aliases for drop-in replacement
    type :: type_var_t
        integer :: id
        character(len=64) :: name
    contains
        procedure :: assign => type_var_arena_assign
        generic :: assignment(=) => assign
    end type type_var_t

    type :: allocation_info_t
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_target = .false.
        logical :: is_allocated = .false.
        logical :: needs_allocation_check = .false.
        logical :: needs_allocatable_string = .false.
    end type allocation_info_t

    type :: mono_type_t
        integer :: kind  ! TVAR, TINT, TREAL, etc.
        type(type_var_t) :: var  ! for TVAR
        type(mono_type_t), allocatable :: args(:)  ! kept for compatibility
        integer :: size = 0  ! for TCHAR(len=size), TARRAY(size)
        type(allocation_info_t) :: alloc_info
        ! Internal arena reference 
        type(type_ref_t) :: arena_ref
    contains
        procedure :: equals => mono_type_arena_equals
        procedure :: to_string => mono_type_arena_to_string
        procedure :: deep_copy => mono_type_arena_deep_copy
        procedure :: assign => mono_type_arena_assign
        generic :: assignment(=) => assign
    end type mono_type_t

    type :: poly_type_t
        type(type_var_t), allocatable :: forall(:)
        type(mono_type_t) :: mono
    contains
        procedure :: to_string => poly_type_arena_to_string
        procedure :: deep_copy => poly_type_arena_deep_copy
        procedure :: assign => poly_type_arena_assign
        generic :: assignment(=) => assign
    end type poly_type_t

    type :: substitution_t
        type(type_var_t), allocatable :: vars(:)
        type(mono_type_t), allocatable :: types(:)
        integer :: count = 0
    contains
        procedure :: add => subst_arena_add
        procedure :: lookup => subst_arena_lookup
        procedure :: apply => subst_arena_apply_to_mono
        procedure :: apply_to_poly => subst_arena_apply_to_poly
        procedure :: deep_copy => subst_arena_deep_copy
        procedure :: assign => subst_arena_assign
        generic :: assignment(=) => assign
    end type substitution_t

    type :: type_env_t
        character(len=:), allocatable :: names(:)
        type(poly_type_t), allocatable :: schemes(:)
        integer :: count = 0
        integer :: capacity = 0
    contains
        procedure :: lookup => env_arena_lookup
        procedure :: extend => env_arena_extend
        procedure :: extend_many => env_arena_extend_many
        procedure :: remove => env_arena_remove
        procedure :: apply_subst => env_arena_apply_subst
        procedure :: deep_copy => env_arena_deep_copy
        procedure :: assign => env_arena_assign
        generic :: assignment(=) => assign
    end type type_env_t

    ! Global arena initialization flag
    logical, save :: arena_initialized = .false.

contains

    ! Ensure arena is initialized
    subroutine ensure_arena_initialized()
        if (.not. arena_initialized) then
            call init_transition_system()
            arena_initialized = .true.
        end if
    end subroutine ensure_arena_initialized

    ! Drop-in replacement for create_type_var
    function create_type_var(id, name) result(tv)
        integer, intent(in) :: id
        character(len=*), intent(in), optional :: name
        type(type_var_t) :: tv
        
        tv%id = id
        if (present(name)) then
            tv%name = name
        else
            if (id <= 26) then
                tv%name = "'"//achar(iachar('a') + id - 1)
            else
                write(tv%name, '("''", A1, I0)') &
                    achar(iachar('a') + mod(id - 1, 26)), (id - 1)/26
            end if
        end if
    end function create_type_var

    ! Drop-in replacement for create_mono_type
    function create_mono_type(kind, var, args, char_size) result(result_type)
        integer, intent(in) :: kind
        type(type_var_t), intent(in), optional :: var
        type(mono_type_t), intent(in), optional :: args(:)
        integer, intent(in), optional :: char_size
        type(mono_type_t) :: result_type
        type(type_ref_t) :: arg_refs(8)
        integer :: i
        
        call ensure_arena_initialized()
        
        result_type%kind = kind
        if (present(char_size)) result_type%size = char_size
        
        ! Initialize var field
        if (present(var)) then
            result_type%var = var
        else
            result_type%var%id = -1
        end if
        
        ! Store args for compatibility
        if (present(args)) then
            result_type%args = args
        end if
        
        ! Create arena reference for efficient operations
        select case (kind)
        case (TVAR)
            if (present(var)) then
                result_type%arena_ref = global_type_arena%create_var(var%id, var%name)
            else
                result_type%arena_ref = global_type_arena%create_var(1, "'a")
            end if
            
        case (TINT)
            result_type%arena_ref = global_type_arena%create_int()
            
        case (TREAL)
            result_type%arena_ref = global_type_arena%create_real()
            
        case (TCHAR)
            result_type%arena_ref = global_type_arena%create_char(result_type%size)
            
        case (TLOGICAL)
            result_type%arena_ref = global_type_arena%create_logical()
            
        case (TFUN)
            if (present(args) .and. size(args) > 1) then
                do i = 1, size(args)
                    arg_refs(i) = args(i)%arena_ref
                end do
                result_type%arena_ref = global_type_arena%create_fun( &
                    arg_refs(1:size(args)-1), arg_refs(size(args)))
            else
                result_type%arena_ref = global_type_arena%create_int()
            end if
            
        case (TARRAY)
            if (present(args) .and. size(args) > 0) then
                result_type%arena_ref = global_type_arena%create_array( &
                    args(1)%arena_ref, result_type%size)
            else
                arg_refs(1) = global_type_arena%create_int()
                result_type%arena_ref = global_type_arena%create_array( &
                    arg_refs(1), result_type%size)
            end if
            
        case default
            result_type%arena_ref = global_type_arena%create_int()
        end select
    end function create_mono_type

    ! Drop-in replacement for create_poly_type
    function create_poly_type(forall_vars, mono) result(pt)
        type(type_var_t), intent(in) :: forall_vars(:)
        type(mono_type_t), intent(in) :: mono
        type(poly_type_t) :: pt
        
        if (size(forall_vars) > 0) then
            pt%forall = forall_vars
        end if
        pt%mono = mono
    end function create_poly_type

    ! Drop-in replacement for create_fun_type
    function create_fun_type(arg_type, result_type) result(fun_type)
        type(mono_type_t), intent(in) :: arg_type, result_type
        type(mono_type_t) :: fun_type
        type(mono_type_t) :: fun_args(2)
        
        fun_args(1) = arg_type
        fun_args(2) = result_type
        fun_type = create_mono_type(TFUN, args=fun_args)
    end function create_fun_type

    ! Type equality using arena references for efficiency
    function mono_type_arena_equals(this, other) result(equal)
        class(mono_type_t), intent(in) :: this
        type(mono_type_t), intent(in) :: other
        logical :: equal
        
        equal = this%arena_ref%equals(other%arena_ref)
    end function mono_type_arena_equals

    ! Type to string using arena references
    function mono_type_arena_to_string(this) result(str)
        class(mono_type_t), intent(in) :: this
        character(len=256) :: str
        
        str = this%arena_ref%to_string()
    end function mono_type_arena_to_string

    ! Deep copy using arena (trivial)
    function mono_type_arena_deep_copy(this) result(copy)
        class(mono_type_t), intent(in) :: this
        type(mono_type_t) :: copy
        
        copy = this  ! Arena references are copyable
    end function mono_type_arena_deep_copy

    ! Assignment operator - avoids allocatable component issues
    subroutine mono_type_arena_assign(lhs, rhs)
        class(mono_type_t), intent(inout) :: lhs
        type(mono_type_t), intent(in) :: rhs
        
        ! Copy all scalar fields
        lhs%kind = rhs%kind
        lhs%size = rhs%size
        lhs%var = rhs%var
        lhs%alloc_info = rhs%alloc_info
        lhs%arena_ref = rhs%arena_ref
        
        ! Safe copy of allocatable args (if present)
        if (allocated(rhs%args)) then
            lhs%args = rhs%args
        else
            if (allocated(lhs%args)) deallocate(lhs%args)
        end if
    end subroutine mono_type_arena_assign

    ! Assignment for type_var_t
    subroutine type_var_arena_assign(lhs, rhs)
        class(type_var_t), intent(inout) :: lhs
        type(type_var_t), intent(in) :: rhs
        
        lhs%id = rhs%id
        lhs%name = rhs%name
    end subroutine type_var_arena_assign

    ! Polymorphic type operations
    function poly_type_arena_to_string(this) result(str)
        class(poly_type_t), intent(in) :: this
        character(len=256) :: str
        character(len=64) :: var_name
        integer :: i
        
        str = ""
        if (allocated(this%forall) .and. size(this%forall) > 0) then
            str = "forall "
            do i = 1, size(this%forall)
                var_name = this%forall(i)%name
                if (i == 1) then
                    str = trim(str) // trim(var_name)
                else
                    str = trim(str) // " " // trim(var_name)
                end if
            end do
            str = trim(str) // ". "
        end if
        str = trim(str) // trim(this%mono%to_string())
    end function poly_type_arena_to_string

    function poly_type_arena_deep_copy(this) result(copy)
        class(poly_type_t), intent(in) :: this
        type(poly_type_t) :: copy
        
        if (allocated(this%forall)) then
            copy%forall = this%forall
        end if
        copy%mono = this%mono%deep_copy()
    end function poly_type_arena_deep_copy

    subroutine poly_type_arena_assign(lhs, rhs)
        class(poly_type_t), intent(inout) :: lhs
        type(poly_type_t), intent(in) :: rhs
        
        if (allocated(rhs%forall)) then
            lhs%forall = rhs%forall
        else
            if (allocated(lhs%forall)) deallocate(lhs%forall)
        end if
        lhs%mono = rhs%mono
    end subroutine poly_type_arena_assign

    ! Placeholder stubs for remaining functions that need full implementation
    function apply_substitution(subst, mono_type) result(result_type)
        type(substitution_t), intent(in) :: subst
        type(mono_type_t), intent(in) :: mono_type
        type(mono_type_t) :: result_type
        
        result_type = mono_type  ! Simplified for now
    end function apply_substitution

    function compose_substitutions(s1, s2) result(composed)
        type(substitution_t), intent(in) :: s1, s2
        type(substitution_t) :: composed
        
        composed = s1  ! Simplified for now
    end function compose_substitutions

    function occurs_check(var, mono_type) result(occurs)
        type(type_var_t), intent(in) :: var
        type(mono_type_t), intent(in) :: mono_type
        logical :: occurs
        
        occurs = .false.  ! Simplified for now
    end function occurs_check

    function free_type_vars(mono_type) result(var_ids)
        type(mono_type_t), intent(in) :: mono_type
        integer, allocatable :: var_ids(:)
        
        allocate(var_ids(0))  ! Simplified for now
    end function free_type_vars

    ! Stub implementations for remaining type methods
    subroutine subst_arena_add(this, var, typ)
        class(substitution_t), intent(inout) :: this
        type(type_var_t), intent(in) :: var
        type(mono_type_t), intent(in) :: typ
    end subroutine subst_arena_add

    function subst_arena_lookup(this, var) result(typ)
        class(substitution_t), intent(in) :: this
        type(type_var_t), intent(in) :: var
        type(mono_type_t) :: typ
        typ = create_mono_type(TINT)
    end function subst_arena_lookup

    function subst_arena_apply_to_mono(this, mono_type) result(result_type)
        class(substitution_t), intent(in) :: this
        type(mono_type_t), intent(in) :: mono_type
        type(mono_type_t) :: result_type
        result_type = mono_type
    end function subst_arena_apply_to_mono

    function subst_arena_apply_to_poly(this, poly_type) result(result_type)
        class(substitution_t), intent(in) :: this
        type(poly_type_t), intent(in) :: poly_type
        type(poly_type_t) :: result_type
        result_type = poly_type
    end function subst_arena_apply_to_poly

    function subst_arena_deep_copy(this) result(copy)
        class(substitution_t), intent(in) :: this
        type(substitution_t) :: copy
        copy = this
    end function subst_arena_deep_copy

    subroutine subst_arena_assign(lhs, rhs)
        class(substitution_t), intent(inout) :: lhs
        type(substitution_t), intent(in) :: rhs
        lhs%count = rhs%count
        if (allocated(rhs%vars)) lhs%vars = rhs%vars
        if (allocated(rhs%types)) lhs%types = rhs%types
    end subroutine subst_arena_assign

    ! Type environment stubs
    function env_arena_lookup(this, name) result(scheme)
        class(type_env_t), intent(in) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t) :: scheme
        scheme = create_poly_type([type_var_t(1, "'a")], create_mono_type(TINT))
    end function env_arena_lookup

    subroutine env_arena_extend(this, name, scheme)
        class(type_env_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(poly_type_t), intent(in) :: scheme
    end subroutine env_arena_extend

    subroutine env_arena_extend_many(this, names, schemes)
        class(type_env_t), intent(inout) :: this
        character(len=*), intent(in) :: names(:)
        type(poly_type_t), intent(in) :: schemes(:)
    end subroutine env_arena_extend_many

    subroutine env_arena_remove(this, name)
        class(type_env_t), intent(inout) :: this
        character(len=*), intent(in) :: name
    end subroutine env_arena_remove

    subroutine env_arena_apply_subst(this, subst)
        class(type_env_t), intent(inout) :: this
        type(substitution_t), intent(in) :: subst
    end subroutine env_arena_apply_subst

    function env_arena_deep_copy(this) result(copy)
        class(type_env_t), intent(in) :: this
        type(type_env_t) :: copy
        copy = this
    end function env_arena_deep_copy

    subroutine env_arena_assign(lhs, rhs)
        class(type_env_t), intent(inout) :: lhs
        type(type_env_t), intent(in) :: rhs
        lhs%count = rhs%count
        lhs%capacity = rhs%capacity
        if (allocated(rhs%names)) lhs%names = rhs%names
        if (allocated(rhs%schemes)) lhs%schemes = rhs%schemes
    end subroutine env_arena_assign

end module type_system_hm_arena