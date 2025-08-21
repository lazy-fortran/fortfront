module type_subst_arena
    ! Arena-based type substitution
    ! Eliminates allocatable component issues in substitution_t
    use type_arena, only: type_ref_t, type_entry_t, global_type_arena, &
                          TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN, TARRAY
    implicit none
    private

    ! Maximum substitutions
    integer, parameter :: MAX_SUBST_ENTRIES = 500

    ! Substitution entry
    type :: subst_entry_t
        integer :: var_id = -1  ! type variable ID
        type(type_ref_t) :: type_ref  ! substituting type
    end type subst_entry_t

    ! Type substitution with arena-based storage
    type, public :: type_subst_arena_t
        type(subst_entry_t) :: entries(MAX_SUBST_ENTRIES)
        integer :: count = 0
        integer :: capacity = MAX_SUBST_ENTRIES
    contains
        procedure :: add => subst_arena_add
        procedure :: lookup => subst_arena_lookup
        procedure :: apply => subst_arena_apply
        procedure :: compose => subst_arena_compose
        procedure :: clear => subst_arena_clear
        procedure :: get_count => subst_arena_get_count
        procedure :: is_empty => subst_arena_is_empty
    end type type_subst_arena_t

    ! Public interface
    public :: subst_entry_t, create_type_subst_arena

contains

    ! Create new type substitution arena
    function create_type_subst_arena() result(subst)
        type(type_subst_arena_t) :: subst
        
        subst%count = 0
        subst%capacity = MAX_SUBST_ENTRIES
        subst%entries = subst_entry_t()
    end function create_type_subst_arena

    ! Add substitution var_id -> type_ref
    subroutine subst_arena_add(this, var_id, type_ref)
        class(type_subst_arena_t), intent(inout) :: this
        integer, intent(in) :: var_id
        type(type_ref_t), intent(in) :: type_ref
        integer :: i
        
        ! Check if substitution already exists - update it
        do i = 1, this%count
            if (this%entries(i)%var_id == var_id) then
                this%entries(i)%type_ref = type_ref
                return
            end if
        end do
        
        ! Add new substitution
        if (this%count >= this%capacity) then
            error stop "Type substitution capacity exceeded"
        end if
        
        this%count = this%count + 1
        this%entries(this%count)%var_id = var_id
        this%entries(this%count)%type_ref = type_ref
    end subroutine subst_arena_add

    ! Lookup substitution for type variable
    function subst_arena_lookup(this, var_id) result(type_ref)
        class(type_subst_arena_t), intent(in) :: this
        integer, intent(in) :: var_id
        type(type_ref_t) :: type_ref
        integer :: i
        
        ! Initialize to invalid reference
        type_ref%index = 0
        
        do i = 1, this%count
            if (this%entries(i)%var_id == var_id) then
                type_ref = this%entries(i)%type_ref
                exit
            end if
        end do
    end function subst_arena_lookup

    ! Apply substitution to a type
    recursive function subst_arena_apply(this, type_ref) result(result_ref)
        class(type_subst_arena_t), intent(in) :: this
        type(type_ref_t), intent(in) :: type_ref
        type(type_ref_t) :: result_ref
        type(type_entry_t) :: entry
        type(type_ref_t) :: subst_ref
        type(type_ref_t) :: new_args(8)
        integer :: i
        
        if (.not. type_ref%is_valid()) then
            result_ref = type_ref
            return
        end if
        
        entry = global_type_arena%get(type_ref%index)
        
        select case (entry%kind)
        case (TVAR)
            ! Look up substitution for this type variable
            subst_ref = this%lookup(entry%var_id)
            if (subst_ref%is_valid()) then
                result_ref = subst_ref
            else
                result_ref = type_ref  ! no substitution found
            end if
            
        case (TINT, TREAL, TLOGICAL)
            ! Base types - no substitution needed
            result_ref = type_ref
            
        case (TCHAR)
            ! Character type - no substitution needed for size
            result_ref = type_ref
            
        case (TFUN)
            ! Function type - apply to all arguments
            do i = 1, entry%arg_count
                new_args(i)%index = entry%arg_indices(i)
                new_args(i) = this%apply(new_args(i))
            end do
            
            ! Create new function type with substituted arguments
            if (entry%arg_count > 1) then
                result_ref = global_type_arena%create_fun( &
                    new_args(1:entry%arg_count-1), &
                    new_args(entry%arg_count))
            else
                result_ref = type_ref  ! degenerate case
            end if
            
        case (TARRAY)
            ! Array type - apply to element type
            if (entry%arg_count > 0) then
                new_args(1)%index = entry%arg_indices(1)
                new_args(1) = this%apply(new_args(1))
                result_ref = global_type_arena%create_array(new_args(1), entry%size)
            else
                result_ref = type_ref
            end if
            
        case default
            ! Unknown type - no substitution
            result_ref = type_ref
        end select
    end function subst_arena_apply

    ! Compose two substitutions (this . other)
    function subst_arena_compose(this, other) result(composed)
        class(type_subst_arena_t), intent(in) :: this
        type(type_subst_arena_t), intent(in) :: other
        type(type_subst_arena_t) :: composed
        integer :: i
        type(type_ref_t) :: applied_type
        
        composed = create_type_subst_arena()
        
        ! Apply this substitution to all types in other
        do i = 1, other%count
            applied_type = this%apply(other%entries(i)%type_ref)
            call composed%add(other%entries(i)%var_id, applied_type)
        end do
        
        ! Add all mappings from this that are not in other
        do i = 1, this%count
            if (.not. subst_contains_var(other, this%entries(i)%var_id)) then
                call composed%add(this%entries(i)%var_id, this%entries(i)%type_ref)
            end if
        end do
    end function subst_arena_compose

    ! Clear all substitutions
    subroutine subst_arena_clear(this)
        class(type_subst_arena_t), intent(inout) :: this
        
        this%count = 0
        this%entries = subst_entry_t()
    end subroutine subst_arena_clear

    ! Get count of substitutions
    function subst_arena_get_count(this) result(count)
        class(type_subst_arena_t), intent(in) :: this
        integer :: count
        
        count = this%count
    end function subst_arena_get_count

    ! Check if substitution is empty
    function subst_arena_is_empty(this) result(empty)
        class(type_subst_arena_t), intent(in) :: this
        logical :: empty
        
        empty = (this%count == 0)
    end function subst_arena_is_empty

    ! Helper: check if substitution contains variable
    function subst_contains_var(subst, var_id) result(contains)
        type(type_subst_arena_t), intent(in) :: subst
        integer, intent(in) :: var_id
        logical :: contains
        integer :: i
        
        contains = .false.
        do i = 1, subst%count
            if (subst%entries(i)%var_id == var_id) then
                contains = .true.
                exit
            end if
        end do
    end function subst_contains_var

end module type_subst_arena