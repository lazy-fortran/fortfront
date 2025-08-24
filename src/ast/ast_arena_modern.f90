module ast_arena_modern
    ! Modern AST arena with generation-based handles and efficient operations  
    ! Replaces deprecated ast_arena.f90 with safer architecture
    ! Part of unified arena architecture following KISS principles
    ! 
    ! This module provides a clean interface by re-exporting components from:
    ! - ast_arena_core: Core arena implementation
    ! - ast_arena_compat: Compatibility layer for old API
    ! - ast_arena_base_interface: Base arena interface implementations
    
    use ast_base, only: ast_node
    use ast_arena_core, only: ast_arena_core_t, ast_handle_t, ast_node_arena_t, &
                              ast_arena_stats_t, ast_free_result_t, &
                              create_ast_arena_core, destroy_ast_arena_core, &
                              is_valid_ast_handle, null_ast_handle
    use ast_arena_compat, only: ast_arena_compat_t, ast_entry_t, &
                                create_ast_arena_compat
    implicit none
    
    ! Re-export core types and functions
    public :: ast_arena_t, ast_handle_t, ast_node_arena_t, ast_entry_t
    public :: create_ast_arena, destroy_ast_arena
    public :: store_ast_node, get_ast_node, is_valid_ast_handle, null_ast_handle
    public :: ast_arena_stats_t, ast_free_result_t
    public :: free_ast_node, is_node_active, get_free_statistics
    
    ! Use compatibility arena directly as the main arena type
    type, extends(ast_arena_compat_t) :: ast_arena_t
    contains
        ! Compatibility method for old API
        procedure :: clear => clear_ast_arena
        ! Override push to sync size field
        procedure :: push => ast_arena_push_with_size_sync
    end type ast_arena_t

contains

    ! Create modern AST arena with compatibility layer
    function create_ast_arena(initial_capacity) result(arena)
        integer, intent(in), optional :: initial_capacity
        type(ast_arena_t) :: arena
        integer :: capacity
        
        ! Set default capacity first
        capacity = 1024
        if (present(initial_capacity)) capacity = initial_capacity
        
        ! Create compatibility layer with explicit capacity
        arena%ast_arena_compat_t = create_ast_arena_compat(capacity)
        
        ! CRITICAL FIX: Manually set capacity field to ensure it's available at the ast_arena_t level
        ! This is needed because the parent assignment may not properly propagate all base fields
        arena%capacity = capacity
        arena%size = 0  ! Initialize size from base_arena_t
        arena%generation = 1  ! Initialize generation from base_arena_t
    end function create_ast_arena
    
    ! Push with size synchronization for backward compatibility
    subroutine ast_arena_push_with_size_sync(this, node, node_type, parent_index)
        class(ast_arena_t), intent(inout) :: this
        class(ast_node), intent(in) :: node
        character(len=*), intent(in), optional :: node_type
        integer, intent(in), optional :: parent_index
        
        ! Call parent push method
        call this%ast_arena_compat_t%push(node, node_type, parent_index)
        
        ! Sync size field with compat_size
        this%size = this%compat_size
        
        ! CRITICAL FIX: Sync capacity field to prevent validation errors
        if (allocated(this%entries)) then
            this%capacity = size(this%entries)
        else
            this%capacity = 0
        end if
    end subroutine ast_arena_push_with_size_sync
    
    ! Destroy AST arena
    subroutine destroy_ast_arena(arena)
        type(ast_arena_t), intent(inout) :: arena
        
        call destroy_ast_arena_core(arena%ast_arena_compat_t%ast_arena_core_t)
    end subroutine destroy_ast_arena
    
    ! Free an AST node
    function free_ast_node(arena, handle) result(free_result)
        type(ast_arena_t), intent(inout) :: arena
        type(ast_handle_t), intent(in) :: handle
        type(ast_free_result_t) :: free_result
        
        ! Use the core implementation through compatibility layer
        free_result = arena%ast_arena_compat_t%free_node(handle)
        
        ! Sync compat_size with actual node count for statistics
        if (free_result%success) then
            arena%ast_arena_compat_t%compat_size = &
                arena%ast_arena_compat_t%ast_arena_core_t%get_node_count()
        end if
    end function free_ast_node
    
    ! Check if node is active
    function is_node_active(arena, handle) result(is_active)
        type(ast_arena_t), intent(inout) :: arena
        type(ast_handle_t), intent(in) :: handle
        logical :: is_active
        
        ! Use the core implementation through compatibility layer
        is_active = arena%ast_arena_compat_t%is_active(handle)
    end function is_node_active
    
    ! Get free statistics
    function get_free_statistics(arena) result(stats)
        type(ast_arena_t), intent(inout) :: arena
        type(ast_arena_stats_t) :: stats
        
        ! Use the core implementation's free stats directly
        stats = arena%ast_arena_compat_t%ast_arena_core_t%get_free_stats()
    end function get_free_statistics
    
    ! Store an AST node
    function store_ast_node(arena, node) result(ast_handle)
        use ast_arena_core, only: core_store => store_ast_node
        type(ast_arena_t), intent(inout) :: arena  
        type(ast_node_arena_t), intent(in) :: node
        type(ast_handle_t) :: ast_handle
        
        ! Delegate to core function through inheritance casting
        ast_handle = core_store(arena%ast_arena_compat_t%ast_arena_core_t, node)
        
        ! Sync compat_size with actual node count for statistics
        if (is_valid_ast_handle(ast_handle)) then
            arena%ast_arena_compat_t%compat_size = &
                arena%ast_arena_compat_t%ast_arena_core_t%get_node_count()
        end if
    end function store_ast_node
    
    ! Get an AST node
    function get_ast_node(arena, handle) result(arena_node)
        use ast_arena_core, only: core_get => get_ast_node
        type(ast_arena_t), intent(inout) :: arena
        type(ast_handle_t), intent(in) :: handle
        type(ast_node_arena_t) :: arena_node
        
        ! Delegate to core function through inheritance casting
        arena_node = core_get(arena%ast_arena_compat_t%ast_arena_core_t, handle)
    end function get_ast_node
    
    ! Clear/reset arena (compatibility wrapper)
    subroutine clear_ast_arena(this)
        class(ast_arena_t), intent(inout) :: this
        
        ! Delegate to reset method in core
        call this%ast_arena_compat_t%reset()
        
        ! Sync compat_size with actual node count (should be 0 after reset)
        this%ast_arena_compat_t%compat_size = &
            this%ast_arena_compat_t%ast_arena_core_t%get_node_count()
    end subroutine clear_ast_arena
    
end module ast_arena_modern