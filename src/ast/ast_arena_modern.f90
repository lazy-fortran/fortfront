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
                              store_ast_node, get_ast_node, is_valid_ast_handle, &
                              null_ast_handle, free_ast_node, is_node_active, &
                              get_free_statistics
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
    end type ast_arena_t

contains

    ! Create modern AST arena with compatibility layer
    function create_ast_arena(initial_capacity) result(arena)
        integer, intent(in), optional :: initial_capacity
        type(ast_arena_t) :: arena
        
        arena%ast_arena_compat_t = create_ast_arena_compat(initial_capacity)
    end function create_ast_arena
    
    ! Destroy AST arena
    subroutine destroy_ast_arena(arena)
        type(ast_arena_t), intent(inout) :: arena
        
        call destroy_ast_arena_core(arena%ast_arena_compat_t%ast_arena_core_t)
    end subroutine destroy_ast_arena
    
end module ast_arena_modern