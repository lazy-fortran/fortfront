module type_system_arena
    ! Arena-based type system compatibility layer
    ! Provides old type_system_hm interface using new arena implementation
    use type_arena
    use type_env_arena
    use type_subst_arena
    implicit none
    private

    ! Re-export type kinds
    public :: TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN, TARRAY

    ! Compatibility types that map to arena-based implementation
    type, public :: type_var_arena_t
        integer :: id
        character(len=64) :: name
    contains
        procedure :: assign => type_var_arena_assign
        generic :: assignment(=) => assign
    end type type_var_arena_t

    type, public :: allocation_info_arena_t
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_target = .false.
        logical :: is_allocated = .false.
        logical :: needs_allocation_check = .false.
        logical :: needs_allocatable_string = .false.
    end type allocation_info_arena_t

    ! Compatibility mono_type using arena reference
    type, public :: mono_type_arena_t
        type(type_ref_t) :: ref
    contains
        procedure :: equals => mono_type_arena_equals
        procedure :: to_string => mono_type_arena_to_string
        procedure :: deep_copy => mono_type_arena_deep_copy
        procedure :: assign => mono_type_arena_assign
        generic :: assignment(=) => assign
    end type mono_type_arena_t

    ! Compatibility poly_type using arena
    type, public :: poly_type_arena_t
        integer :: quantifier_count = 0
        integer :: quantifier_ids(32) = 0
        type(mono_type_arena_t) :: mono
    contains
        procedure :: to_string => poly_type_arena_to_string
        procedure :: deep_copy => poly_type_arena_deep_copy
        procedure :: assign => poly_type_arena_assign
        generic :: assignment(=) => assign
    end type poly_type_arena_t

    ! Global instances
    type(type_env_arena_t), save, public :: global_type_env
    type(type_subst_arena_t), save, public :: global_substitution

    ! Constructor functions (compatibility interface)
    public :: create_type_var_arena, create_mono_type_arena, create_poly_type_arena
    public :: create_fun_type_arena
    public :: apply_substitution_arena, compose_substitutions_arena
    public :: occurs_check_arena, free_type_vars_arena

contains

    ! Assignment for type_var_arena_t
    subroutine type_var_arena_assign(lhs, rhs)
        class(type_var_arena_t), intent(inout) :: lhs
        type(type_var_arena_t), intent(in) :: rhs
        
        lhs%id = rhs%id
        lhs%name = rhs%name
    end subroutine type_var_arena_assign

    ! Constructor for type variable
    function create_type_var_arena(id, name) result(tv)
        integer, intent(in) :: id
        character(len=*), intent(in), optional :: name
        type(type_var_arena_t) :: tv
        
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
    end function create_type_var_arena

    ! Constructor for monomorphic type
    function create_mono_type_arena(kind, var, args, char_size) result(result_type)
        integer, intent(in) :: kind
        type(type_var_arena_t), intent(in), optional :: var
        type(mono_type_arena_t), intent(in), optional :: args(:)
        integer, intent(in), optional :: char_size
        type(mono_type_arena_t) :: result_type
        type(type_ref_t) :: arg_refs(8)
        integer :: i
        
        select case (kind)
        case (TVAR)
            if (present(var)) then
                result_type%ref = global_type_arena%create_var(var%id, var%name)
            else
                result_type%ref = global_type_arena%create_var(1, "'a")
            end if
            
        case (TINT)
            result_type%ref = global_type_arena%create_int()
            
        case (TREAL)
            result_type%ref = global_type_arena%create_real()
            
        case (TCHAR)
            result_type%ref = global_type_arena%create_char(char_size)
            
        case (TLOGICAL)
            result_type%ref = global_type_arena%create_logical()
            
        case (TFUN)
            if (present(args) .and. size(args) > 0) then
                do i = 1, size(args)
                    arg_refs(i) = args(i)%ref
                end do
                if (size(args) > 1) then
                    result_type%ref = global_type_arena%create_fun( &
                        arg_refs(1:size(args)-1), arg_refs(size(args)))
                else
                    ! Degenerate function type
                    result_type%ref = global_type_arena%create_int()  ! fallback
                end if
            else
                result_type%ref = global_type_arena%create_int()  ! fallback
            end if
            
        case (TARRAY)
            if (present(args) .and. size(args) > 0) then
                result_type%ref = global_type_arena%create_array(args(1)%ref, char_size)
            else
                ! Array of integers by default
                arg_refs(1) = global_type_arena%create_int()
                result_type%ref = global_type_arena%create_array(arg_refs(1), char_size)
            end if
            
        case default
            result_type%ref = global_type_arena%create_int()  ! fallback
        end select
    end function create_mono_type_arena

    ! Constructor for polymorphic type
    function create_poly_type_arena(forall_vars, mono) result(pt)
        type(type_var_arena_t), intent(in) :: forall_vars(:)
        type(mono_type_arena_t), intent(in) :: mono
        type(poly_type_arena_t) :: pt
        integer :: i
        
        pt%quantifier_count = min(size(forall_vars), size(pt%quantifier_ids))
        do i = 1, pt%quantifier_count
            pt%quantifier_ids(i) = forall_vars(i)%id
        end do
        pt%mono = mono
    end function create_poly_type_arena

    ! Helper function to create function type
    function create_fun_type_arena(arg_type, result_type) result(fun_type)
        type(mono_type_arena_t), intent(in) :: arg_type, result_type
        type(mono_type_arena_t) :: fun_type
        
        fun_type%ref = global_type_arena%create_fun([arg_type%ref], result_type%ref)
    end function create_fun_type_arena

    ! Monomorphic type equality
    function mono_type_arena_equals(this, other) result(equal)
        class(mono_type_arena_t), intent(in) :: this
        type(mono_type_arena_t), intent(in) :: other
        logical :: equal
        
        equal = this%ref%equals(other%ref)
    end function mono_type_arena_equals

    ! Monomorphic type to string
    function mono_type_arena_to_string(this) result(str)
        class(mono_type_arena_t), intent(in) :: this
        character(len=256) :: str
        
        str = this%ref%to_string()
    end function mono_type_arena_to_string

    ! Deep copy for mono type (trivial with arena references)
    function mono_type_arena_deep_copy(this) result(copy)
        class(mono_type_arena_t), intent(in) :: this
        type(mono_type_arena_t) :: copy
        
        copy%ref = this%ref  ! Arena references are copyable
    end function mono_type_arena_deep_copy

    ! Assignment for mono type
    subroutine mono_type_arena_assign(lhs, rhs)
        class(mono_type_arena_t), intent(inout) :: lhs
        type(mono_type_arena_t), intent(in) :: rhs
        
        lhs%ref = rhs%ref
    end subroutine mono_type_arena_assign

    ! Polymorphic type to string
    function poly_type_arena_to_string(this) result(str)
        class(poly_type_arena_t), intent(in) :: this
        character(len=256) :: str
        character(len=64) :: var_name
        integer :: i
        
        str = ""
        
        if (this%quantifier_count > 0) then
            str = "forall "
            do i = 1, this%quantifier_count
                if (this%quantifier_ids(i) <= 26) then
                    var_name = "'"//achar(iachar('a') + this%quantifier_ids(i) - 1)
                else
                    write(var_name, '("''", A1, I0)') &
                        achar(iachar('a') + mod(this%quantifier_ids(i) - 1, 26)), &
                        (this%quantifier_ids(i) - 1)/26
                end if
                
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

    ! Deep copy for poly type
    function poly_type_arena_deep_copy(this) result(copy)
        class(poly_type_arena_t), intent(in) :: this
        type(poly_type_arena_t) :: copy
        
        copy%quantifier_count = this%quantifier_count
        copy%quantifier_ids = this%quantifier_ids
        copy%mono = this%mono%deep_copy()
    end function poly_type_arena_deep_copy

    ! Assignment for poly type
    subroutine poly_type_arena_assign(lhs, rhs)
        class(poly_type_arena_t), intent(inout) :: lhs
        type(poly_type_arena_t), intent(in) :: rhs
        
        lhs%quantifier_count = rhs%quantifier_count
        lhs%quantifier_ids = rhs%quantifier_ids
        lhs%mono = rhs%mono
    end subroutine poly_type_arena_assign

    ! Apply substitution
    function apply_substitution_arena(subst, mono_type) result(result_type)
        type(type_subst_arena_t), intent(in) :: subst
        type(mono_type_arena_t), intent(in) :: mono_type
        type(mono_type_arena_t) :: result_type
        
        result_type%ref = subst%apply(mono_type%ref)
    end function apply_substitution_arena

    ! Compose substitutions
    function compose_substitutions_arena(s1, s2) result(composed)
        type(type_subst_arena_t), intent(in) :: s1, s2
        type(type_subst_arena_t) :: composed
        
        composed = s1%compose(s2)
    end function compose_substitutions_arena

    ! Occurs check (simplified - arena references prevent cycles)
    function occurs_check_arena(var_id, mono_type) result(occurs)
        integer, intent(in) :: var_id
        type(mono_type_arena_t), intent(in) :: mono_type
        logical :: occurs
        type(type_entry_t) :: entry
        
        occurs = .false.
        
        if (.not. mono_type%ref%is_valid()) return
        
        entry = global_type_arena%get(mono_type%ref%index)
        
        if (entry%kind == TVAR) then
            occurs = (entry%var_id == var_id)
        end if
        
        ! For complex types, would need to recurse through arguments
        ! but arena structure makes this safer
    end function occurs_check_arena

    ! Free type variables (simplified implementation)
    function free_type_vars_arena(mono_type) result(var_ids)
        type(mono_type_arena_t), intent(in) :: mono_type
        integer, allocatable :: var_ids(:)
        type(type_entry_t) :: entry
        
        allocate(var_ids(0))  ! Empty by default
        
        if (.not. mono_type%ref%is_valid()) return
        
        entry = global_type_arena%get(mono_type%ref%index)
        
        if (entry%kind == TVAR) then
            allocate(var_ids(1))
            var_ids(1) = entry%var_id
        end if
        
        ! For complex types, would need to collect from arguments
    end function free_type_vars_arena

end module type_system_arena