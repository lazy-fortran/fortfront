module semantic_analyzer_base
    use ast_core, only: ast_arena_t
    implicit none
    private

    public :: semantic_analyzer_t

    ! Abstract base interface for semantic analyzers
    type, abstract :: semantic_analyzer_t
    contains
        procedure(analyze_interface), deferred :: analyze
        procedure(get_results_interface), deferred :: get_results
        procedure(assign_interface), deferred :: assign
        procedure(get_dependencies_interface), deferred :: get_dependencies
        procedure :: get_name => get_analyzer_name
        generic :: assignment(=) => assign
    end type

    ! Abstract interfaces for deferred procedures
    abstract interface
        subroutine analyze_interface(this, shared_context, arena, node_index)
            import :: semantic_analyzer_t, ast_arena_t
            class(semantic_analyzer_t), intent(inout) :: this
            class(*), intent(in) :: shared_context  ! Will be semantic_context_t
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in) :: node_index
        end subroutine

        function get_results_interface(this) result(results)
            import :: semantic_analyzer_t
            class(semantic_analyzer_t), intent(in) :: this
            class(*), allocatable :: results  ! Analyzer-specific results
        end function

        subroutine assign_interface(lhs, rhs)
            import :: semantic_analyzer_t
            class(semantic_analyzer_t), intent(inout) :: lhs
            class(semantic_analyzer_t), intent(in) :: rhs
        end subroutine

        function get_dependencies_interface(this) result(deps)
            import :: semantic_analyzer_t
            class(semantic_analyzer_t), intent(in) :: this
            character(len=32), allocatable :: deps(:)
        end function
    end interface

contains

    function get_analyzer_name(this) result(name)
        class(semantic_analyzer_t), intent(in) :: this
        character(:), allocatable :: name
        
        ! Default implementation - subclasses should override
        name = "unknown_analyzer"
    end function

end module semantic_analyzer_base