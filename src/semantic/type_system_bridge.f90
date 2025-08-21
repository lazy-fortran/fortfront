module type_system_bridge
    ! Bridge module for migrating from allocatable-based to arena-based types
    ! Provides conversion functions and compatibility layer
    ! Allows gradual migration while maintaining existing API compatibility
    
    use type_system_hm
    use type_system_arena
    implicit none
    private
    
    ! Public conversion interface
    public :: convert_mono_to_arena, convert_arena_to_mono
    public :: convert_poly_to_arena, convert_arena_to_poly
    public :: create_type_system_with_arena
    public :: type_system_context_t
    
    ! Enhanced type system context with both approaches
    type :: type_system_context_t
        type(type_arena_t) :: arena                    ! Arena for new types
        type(type_env_t) :: env                        ! Traditional environment
        logical :: use_arena = .true.                  ! Prefer arena allocation
        integer :: conversion_count = 0                ! Statistics
    contains
        procedure :: create_mono_safe => context_create_mono_safe
        procedure :: create_poly_safe => context_create_poly_safe
        procedure :: cleanup => context_cleanup
        procedure :: get_stats => context_get_stats
    end type type_system_context_t
    
contains
    
    ! Convert traditional mono_type_t to arena-based storage
    recursive function convert_mono_to_arena(arena, mono) result(handle)
        type(type_arena_t), intent(inout) :: arena
        type(mono_type_t), intent(in) :: mono
        type(mono_handle_t) :: handle
        type(arena_mono_type_t) :: arena_mono
        type(mono_handle_t), allocatable :: arena_args(:)
        integer :: i
        
        ! Convert scalar fields
        arena_mono%kind = mono%kind
        arena_mono%size = mono%size
        arena_mono%var_id = mono%var%id
        if (allocated(mono%var%name)) then
            arena_mono%var_name = mono%var%name
        end if
        
        ! Convert allocation info
        arena_mono%is_allocatable = mono%alloc_info%is_allocatable
        arena_mono%is_pointer = mono%alloc_info%is_pointer
        arena_mono%is_target = mono%alloc_info%is_target
        
        ! Convert args array recursively (limited depth for safety)
        if (allocated(mono%args)) then
            allocate(arena_args(size(mono%args)))
            do i = 1, size(mono%args)
                arena_args(i) = convert_mono_to_arena(arena, mono%args(i))
            end do
            arena_mono%args = store_type_args(arena, arena_args)
        else
            arena_mono%args = null_args_handle()
        end if
        
        ! Store in arena
        handle = store_mono_type(arena, arena_mono)
    end function convert_mono_to_arena
    
    ! Convert arena-based handle back to traditional mono_type_t
    recursive function convert_arena_to_mono(arena, handle) result(mono)
        type(type_arena_t), intent(in) :: arena
        type(mono_handle_t), intent(in) :: handle
        type(mono_type_t) :: mono
        type(arena_mono_type_t) :: arena_mono
        type(mono_handle_t), allocatable :: arena_args(:)
        integer :: i
        
        if (.not. is_valid_mono_handle(handle)) then
            mono = create_mono_type(TINT)  ! Safe default
            return
        end if
        
        ! Get from arena
        arena_mono = get_mono_type(arena, handle)
        
        ! Convert scalar fields
        mono%kind = arena_mono%kind
        mono%size = arena_mono%size
        mono%var = create_type_var(arena_mono%var_id, trim(arena_mono%var_name))
        
        ! Convert allocation info
        mono%alloc_info%is_allocatable = arena_mono%is_allocatable
        mono%alloc_info%is_pointer = arena_mono%is_pointer
        mono%alloc_info%is_target = arena_mono%is_target
        
        ! Convert args array
        if (is_valid_args_handle(arena_mono%args)) then
            call get_type_args(arena, arena_mono%args, arena_args)
            if (allocated(arena_args)) then
                allocate(mono%args(size(arena_args)))
                do i = 1, size(arena_args)
                    mono%args(i) = convert_arena_to_mono(arena, arena_args(i))
                end do
            end if
        end if
    end function convert_arena_to_mono
    
    ! Convert traditional poly_type_t to arena-based storage
    function convert_poly_to_arena(arena, poly) result(handle)
        type(type_arena_t), intent(inout) :: arena
        type(poly_type_t), intent(in) :: poly
        type(poly_handle_t) :: handle
        type(arena_poly_type_t) :: arena_poly
        type(mono_handle_t), allocatable :: forall_handles(:)
        integer :: i
        
        ! Convert monomorphic part
        arena_poly%mono = convert_mono_to_arena(arena, poly%mono)
        
        ! Convert quantified variables
        if (allocated(poly%forall)) then
            allocate(forall_handles(size(poly%forall)))
            do i = 1, size(poly%forall)
                ! Convert type variables to mono handles
                block
                    type(arena_mono_type_t) :: var_mono
                    var_mono%kind = TVAR
                    var_mono%var_id = poly%forall(i)%id
                    if (allocated(poly%forall(i)%name)) then
                        var_mono%var_name = poly%forall(i)%name
                    end if
                    forall_handles(i) = store_mono_type(arena, var_mono)
                end block
            end do
            arena_poly%forall_vars = store_type_args(arena, forall_handles)
        else
            arena_poly%forall_vars = null_args_handle()
        end if
        
        handle = store_poly_type(arena, arena_poly)
    end function convert_poly_to_arena
    
    ! Convert arena-based poly handle back to traditional poly_type_t
    function convert_arena_to_poly(arena, handle) result(poly)
        type(type_arena_t), intent(in) :: arena
        type(poly_handle_t), intent(in) :: handle
        type(poly_type_t) :: poly
        type(arena_poly_type_t) :: arena_poly
        type(mono_handle_t), allocatable :: forall_handles(:)
        type(arena_mono_type_t) :: var_mono
        integer :: i
        
        if (.not. is_valid_poly_handle(handle)) then
            ! Safe default
            poly%mono = create_mono_type(TINT)
            return
        end if
        
        ! Get from arena
        arena_poly = get_poly_type(arena, handle)
        
        ! Convert monomorphic part
        poly%mono = convert_arena_to_mono(arena, arena_poly%mono)
        
        ! Convert quantified variables
        if (is_valid_args_handle(arena_poly%forall_vars)) then
            call get_type_args(arena, arena_poly%forall_vars, forall_handles)
            if (allocated(forall_handles)) then
                allocate(poly%forall(size(forall_handles)))
                do i = 1, size(forall_handles)
                    var_mono = get_mono_type(arena, forall_handles(i))
                    poly%forall(i) = create_type_var(var_mono%var_id, &
                                                     trim(var_mono%var_name))
                end do
            end if
        end if
    end function convert_arena_to_poly
    
    ! Create enhanced type system context with arena
    function create_type_system_with_arena(chunk_size) result(context)
        integer, intent(in), optional :: chunk_size
        type(type_system_context_t) :: context
        
        if (present(chunk_size)) then
            context%arena = create_type_arena(chunk_size)
        else
            context%arena = create_type_arena()
        end if
        
        context%use_arena = .true.
        context%conversion_count = 0
    end function create_type_system_with_arena
    
    ! Safe monomorphic type creation using arena
    function context_create_mono_safe(this, kind, var_id, var_name, char_size) &
            result(handle)
        class(type_system_context_t), intent(inout) :: this
        integer, intent(in) :: kind
        integer, intent(in), optional :: var_id
        character(len=*), intent(in), optional :: var_name
        integer, intent(in), optional :: char_size
        type(mono_handle_t) :: handle
        type(arena_mono_type_t) :: mono
        
        ! Initialize arena mono type
        mono%kind = kind
        if (present(var_id)) mono%var_id = var_id
        if (present(var_name)) mono%var_name = var_name
        if (present(char_size)) mono%size = char_size
        mono%args = null_args_handle()
        
        handle = store_mono_type(this%arena, mono)
        this%conversion_count = this%conversion_count + 1
    end function context_create_mono_safe
    
    ! Safe polymorphic type creation using arena
    function context_create_poly_safe(this, mono_handle, forall_vars) result(handle)
        class(type_system_context_t), intent(inout) :: this
        type(mono_handle_t), intent(in) :: mono_handle
        type(mono_handle_t), intent(in), optional :: forall_vars(:)
        type(poly_handle_t) :: handle
        type(arena_poly_type_t) :: poly
        
        poly%mono = mono_handle
        if (present(forall_vars)) then
            poly%forall_vars = store_type_args(this%arena, forall_vars)
        else
            poly%forall_vars = null_args_handle()
        end if
        
        handle = store_poly_type(this%arena, poly)
        this%conversion_count = this%conversion_count + 1
    end function context_create_poly_safe
    
    ! Clean up context
    subroutine context_cleanup(this)
        class(type_system_context_t), intent(inout) :: this
        
        call destroy_type_arena(this%arena)
        this%conversion_count = 0
    end subroutine context_cleanup
    
    ! Get context statistics
    function context_get_stats(this) result(stats_string)
        class(type_system_context_t), intent(in) :: this
        character(len=:), allocatable :: stats_string
        type(type_arena_stats_t) :: arena_stats
        
        arena_stats = this%arena%get_stats()
        
        stats_string = "Type System Context Stats: " // &
            "mono=" // trim(adjustl(to_string(arena_stats%mono_types))) // &
            " poly=" // trim(adjustl(to_string(arena_stats%poly_types))) // &
            " args=" // trim(adjustl(to_string(arena_stats%arg_arrays))) // &
            " mem=" // trim(adjustl(to_string(arena_stats%total_memory))) // "B" // &
            " util=" // trim(adjustl(to_string(int(arena_stats%utilization * 100)))) // "%"
    end function context_get_stats
    
    ! Helper function to convert integers to strings
    pure function to_string(i) result(str)
        integer, intent(in) :: i
        character(len=20) :: str
        write(str, '(I0)') i
        str = trim(adjustl(str))
    end function to_string
    
end module type_system_bridge