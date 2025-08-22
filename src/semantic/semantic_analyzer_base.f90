module semantic_analyzer_base
    ! Base analyzer interface for the semantic pipeline
    use ast_core, only: ast_arena_t
    implicit none
    private
    
    public :: semantic_analyzer_t
    
    ! Abstract base analyzer for semantic pipeline
    type, abstract :: semantic_analyzer_t
    contains
        procedure(analyze_interface), deferred :: analyze
        procedure(get_results_interface), deferred :: get_results
        procedure(get_name_interface), deferred :: get_name
        procedure(assign_interface), deferred :: assign
        procedure(get_dependencies_interface), deferred :: get_dependencies
    end type semantic_analyzer_t
    
    ! Abstract interfaces for analyzer methods
    abstract interface
        subroutine analyze_interface(this, shared_context, arena, node_index)
            import :: semantic_analyzer_t, ast_arena_t
            class(semantic_analyzer_t), intent(inout) :: this
            class(*), intent(in) :: shared_context
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: node_index
        end subroutine analyze_interface
        
        function get_results_interface(this) result(results)
            import :: semantic_analyzer_t
            class(semantic_analyzer_t), intent(in) :: this
            class(*), allocatable :: results
        end function get_results_interface
        
        function get_name_interface(this) result(name)
            import :: semantic_analyzer_t
            class(semantic_analyzer_t), intent(in) :: this
            character(:), allocatable :: name
        end function get_name_interface
        
        subroutine assign_interface(lhs, rhs)
            import :: semantic_analyzer_t
            class(semantic_analyzer_t), intent(out) :: lhs
            class(semantic_analyzer_t), intent(in) :: rhs
        end subroutine assign_interface
        
        function get_dependencies_interface(this) result(deps)
            import :: semantic_analyzer_t
            class(semantic_analyzer_t), intent(in) :: this
            character(:), allocatable :: deps(:)
        end function get_dependencies_interface
    end interface
    
end module semantic_analyzer_base