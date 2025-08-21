module type_system_transition
    ! Transition compatibility module for migrating to arena-based types
    ! Provides old interface using new arena implementation underneath
    use type_system_hm, only: mono_type_t, poly_type_t, type_env_t, substitution_t, &
                              type_var_t, create_mono_type, create_type_var, create_poly_type, &
                              TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN, TARRAY
    use type_arena, only: type_ref_t, type_entry_t, type_arena_t, global_type_arena, &
                          init_type_arena
    use type_env_arena, only: type_env_arena_t, create_type_env_arena
    use type_subst_arena, only: type_subst_arena_t, create_type_subst_arena
    use type_system_arena, only: poly_type_arena_t, global_type_env, global_substitution
    implicit none
    private

    ! Type conversion interface
    public :: mono_to_arena, arena_to_mono
    public :: poly_to_arena, arena_to_poly
    public :: env_to_arena, arena_to_env
    public :: subst_to_arena, arena_to_subst
    public :: init_transition_system

contains

    ! Initialize the transition system
    subroutine init_transition_system()
        call init_type_arena(global_type_arena)
        global_type_env = create_type_env_arena()
        global_substitution = create_type_subst_arena()
    end subroutine init_transition_system

    ! Convert old mono_type_t to arena reference
    recursive function mono_to_arena(old_mono) result(arena_ref)
        type(mono_type_t), intent(in) :: old_mono
        type(type_ref_t) :: arena_ref
        type(mono_type_t), allocatable :: arg_types(:)
        type(type_ref_t) :: arg_refs(8)
        integer :: i
        
        select case (old_mono%kind)
        case (TVAR)
            arena_ref = global_type_arena%create_var(old_mono%var%id, old_mono%var%name)
            
        case (TINT)
            arena_ref = global_type_arena%create_int()
            
        case (TREAL)
            arena_ref = global_type_arena%create_real()
            
        case (TCHAR)
            arena_ref = global_type_arena%create_char(old_mono%size)
            
        case (TLOGICAL)
            arena_ref = global_type_arena%create_logical()
            
        case (TFUN)
            if (allocated(old_mono%args) .and. size(old_mono%args) > 1) then
                ! Convert argument types to arena references
                do i = 1, size(old_mono%args)
                    arg_refs(i) = mono_to_arena(old_mono%args(i))
                end do
                arena_ref = global_type_arena%create_fun( &
                    arg_refs(1:size(old_mono%args)-1), &
                    arg_refs(size(old_mono%args)))
            else
                ! Fallback for malformed function type
                arena_ref = global_type_arena%create_int()
            end if
            
        case (TARRAY)
            if (allocated(old_mono%args) .and. size(old_mono%args) > 0) then
                arg_refs(1) = mono_to_arena(old_mono%args(1))
                arena_ref = global_type_arena%create_array(arg_refs(1), old_mono%size)
            else
                ! Array of integers by default
                arg_refs(1) = global_type_arena%create_int()
                arena_ref = global_type_arena%create_array(arg_refs(1), old_mono%size)
            end if
            
        case default
            ! Fallback
            arena_ref = global_type_arena%create_int()
        end select
    end function mono_to_arena

    ! Convert arena reference back to old mono_type_t
    recursive function arena_to_mono(arena_ref) result(old_mono)
        type(type_ref_t), intent(in) :: arena_ref
        type(mono_type_t) :: old_mono
        type(type_entry_t) :: entry
        type(mono_type_t), allocatable :: arg_types(:)
        type(type_ref_t) :: arg_ref
        integer :: i
        
        if (.not. arena_ref%is_valid()) then
            old_mono = create_mono_type(TINT)
            return
        end if
        
        entry = global_type_arena%get(arena_ref%index)
        
        select case (entry%kind)
        case (TVAR)
            old_mono = create_mono_type(TVAR, &
                create_type_var(entry%var_id, entry%var_name))
            
        case (TINT)
            old_mono = create_mono_type(TINT)
            
        case (TREAL)
            old_mono = create_mono_type(TREAL)
            
        case (TCHAR)
            old_mono = create_mono_type(TCHAR, char_size=entry%size)
            
        case (TLOGICAL)
            old_mono = create_mono_type(TLOGICAL)
            
        case (TFUN)
            if (entry%arg_count > 1) then
                allocate(arg_types(entry%arg_count))
                do i = 1, entry%arg_count
                    arg_ref%index = entry%arg_indices(i)
                    arg_types(i) = arena_to_mono(arg_ref)
                end do
                old_mono = create_mono_type(TFUN, args=arg_types)
            else
                old_mono = create_mono_type(TINT)  ! fallback
            end if
            
        case (TARRAY)
            if (entry%arg_count > 0) then
                allocate(arg_types(1))
                arg_ref%index = entry%arg_indices(1)
                arg_types(1) = arena_to_mono(arg_ref)
                old_mono = create_mono_type(TARRAY, args=arg_types, char_size=entry%size)
            else
                allocate(arg_types(1))
                arg_types(1) = create_mono_type(TINT)
                old_mono = create_mono_type(TARRAY, args=arg_types, char_size=entry%size)
            end if
            
        case default
            old_mono = create_mono_type(TINT)
        end select
        
        ! Copy allocation info if available
        old_mono%alloc_info%is_allocatable = entry%is_allocatable
        old_mono%alloc_info%is_pointer = entry%is_pointer
        old_mono%alloc_info%is_target = entry%is_target
        old_mono%alloc_info%is_allocated = entry%is_allocated
        old_mono%alloc_info%needs_allocation_check = entry%needs_allocation_check
        old_mono%alloc_info%needs_allocatable_string = entry%needs_allocatable_string
    end function arena_to_mono

    ! Convert old poly_type_t to arena-based poly type
    function poly_to_arena(old_poly) result(arena_poly)
        type(poly_type_t), intent(in) :: old_poly
        type(poly_type_arena_t) :: arena_poly
        integer :: i
        
        ! Convert quantifiers
        if (allocated(old_poly%forall)) then
            arena_poly%quantifier_count = min(size(old_poly%forall), &
                                              size(arena_poly%quantifier_ids))
            do i = 1, arena_poly%quantifier_count
                arena_poly%quantifier_ids(i) = old_poly%forall(i)%id
            end do
        else
            arena_poly%quantifier_count = 0
        end if
        
        ! Convert mono type
        arena_poly%mono%ref = mono_to_arena(old_poly%mono)
    end function poly_to_arena

    ! Convert arena-based poly type back to old poly_type_t
    function arena_to_poly(arena_poly) result(old_poly)
        type(poly_type_arena_t), intent(in) :: arena_poly
        type(poly_type_t) :: old_poly
        type(type_var_t), allocatable :: forall_vars(:)
        integer :: i
        
        ! Convert quantifiers
        if (arena_poly%quantifier_count > 0) then
            allocate(forall_vars(arena_poly%quantifier_count))
            do i = 1, arena_poly%quantifier_count
                forall_vars(i) = create_type_var(arena_poly%quantifier_ids(i))
            end do
        else
            allocate(forall_vars(0))
        end if
        
        ! Convert mono type and create poly type
        old_poly = create_poly_type(forall_vars, arena_to_mono(arena_poly%mono%ref))
    end function arena_to_poly

    ! Convert old type_env_t to arena-based environment
    subroutine env_to_arena(old_env, arena_env)
        type(type_env_t), intent(in) :: old_env
        type(type_env_arena_t), intent(inout) :: arena_env
        type(poly_type_arena_t) :: arena_poly
        integer :: i
        
        call arena_env%clear()
        
        if (old_env%count > 0) then
            do i = 1, old_env%count
                arena_poly = poly_to_arena(old_env%schemes(i))
                if (arena_poly%quantifier_count > 0) then
                    call arena_env%extend(old_env%names(i), arena_poly%mono%ref, &
                                         arena_poly%quantifier_ids(1:arena_poly%quantifier_count))
                else
                    call arena_env%extend(old_env%names(i), arena_poly%mono%ref)
                end if
            end do
        end if
    end subroutine env_to_arena

    ! Convert arena-based environment back to old type_env_t
    subroutine arena_to_env(arena_env, old_env)
        type(type_env_arena_t), intent(in) :: arena_env
        type(type_env_t), intent(inout) :: old_env
        character(len=256) :: names(arena_env%capacity)
        type(poly_type_t) :: scheme
        type(poly_type_arena_t) :: arena_poly
        integer :: actual_count, i
        
        ! Clear old environment
        old_env%count = 0
        if (allocated(old_env%names)) deallocate(old_env%names)
        if (allocated(old_env%schemes)) deallocate(old_env%schemes)
        
        call arena_env%get_names(names, actual_count)
        
        if (actual_count > 0) then
            old_env%capacity = actual_count
            allocate(character(len=256) :: old_env%names(actual_count))
            allocate(old_env%schemes(actual_count))
            
            do i = 1, actual_count
                old_env%names(i) = names(i)
                
                ! Get type scheme from arena
                arena_poly%mono%ref = arena_env%lookup(names(i))
                if (arena_env%entries(i)%is_quantified) then
                    arena_poly%quantifier_count = arena_env%entries(i)%quantifier_count
                    arena_poly%quantifier_ids(1:arena_poly%quantifier_count) = &
                        arena_env%entries(i)%quantifier_ids(1:arena_poly%quantifier_count)
                else
                    arena_poly%quantifier_count = 0
                end if
                
                old_env%schemes(i) = arena_to_poly(arena_poly)
            end do
            
            old_env%count = actual_count
        end if
    end subroutine arena_to_env

    ! Convert old substitution_t to arena-based substitution
    subroutine subst_to_arena(old_subst, arena_subst)
        type(substitution_t), intent(in) :: old_subst
        type(type_subst_arena_t), intent(inout) :: arena_subst
        type(type_ref_t) :: type_ref
        integer :: i
        
        call arena_subst%clear()
        
        if (old_subst%count > 0) then
            do i = 1, old_subst%count
                type_ref = mono_to_arena(old_subst%types(i))
                call arena_subst%add(old_subst%vars(i)%id, type_ref)
            end do
        end if
    end subroutine subst_to_arena

    ! Convert arena-based substitution back to old substitution_t
    subroutine arena_to_subst(arena_subst, old_subst)
        type(type_subst_arena_t), intent(in) :: arena_subst
        type(substitution_t), intent(inout) :: old_subst
        integer :: i
        
        ! Clear old substitution
        old_subst%count = 0
        if (allocated(old_subst%vars)) deallocate(old_subst%vars)
        if (allocated(old_subst%types)) deallocate(old_subst%types)
        
        if (arena_subst%count > 0) then
            allocate(old_subst%vars(arena_subst%count))
            allocate(old_subst%types(arena_subst%count))
            
            do i = 1, arena_subst%count
                old_subst%vars(i) = create_type_var(arena_subst%entries(i)%var_id)
                old_subst%types(i) = arena_to_mono(arena_subst%entries(i)%type_ref)
            end do
            
            old_subst%count = arena_subst%count
        end if
    end subroutine arena_to_subst

end module type_system_transition