module test_analyzer
    use semantic_analyzer_base, only: semantic_analyzer_t
    use ast_core, only: ast_arena_t
    implicit none
    private

    public :: simple_test_analyzer_t

    ! Simple test analyzer for basic pipeline testing
    type, extends(semantic_analyzer_t) :: simple_test_analyzer_t
        logical :: analysis_executed = .false.
        integer :: nodes_visited = 0
    contains
        procedure :: analyze => analyze_test
        procedure :: get_results => get_test_results
        procedure :: get_name => get_test_analyzer_name
        procedure :: assign => assign_test_analyzer
        procedure :: get_dependencies => get_test_dependencies
        procedure :: was_executed
        procedure :: get_nodes_visited
    end type

contains

    subroutine analyze_test(this, shared_context, arena, node_index)
        class(simple_test_analyzer_t), intent(inout) :: this
        class(*), intent(in) :: shared_context
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        
        ! Mark as executed
        this%analysis_executed = .true.
        
        ! Simple analysis: count how many nodes we would visit
        ! For now, just set to 1 (the root node)
        this%nodes_visited = 1
        
        ! In a real analyzer, we would traverse the AST and
        ! populate the shared context with analysis results
    end subroutine

    function get_test_results(this) result(results)
        class(simple_test_analyzer_t), intent(in) :: this
        class(*), allocatable :: results
        
        ! For testing, return the execution status
        allocate(logical :: results)
        select type(results)
        type is (logical)
            results = this%analysis_executed
        end select
    end function

    function get_test_analyzer_name(this) result(name)
        class(simple_test_analyzer_t), intent(in) :: this
        character(:), allocatable :: name
        
        name = "simple_test_analyzer"
    end function

    function was_executed(this) result(executed)
        class(simple_test_analyzer_t), intent(in) :: this
        logical :: executed
        
        executed = this%analysis_executed
    end function

    function get_nodes_visited(this) result(count)
        class(simple_test_analyzer_t), intent(in) :: this
        integer :: count
        
        count = this%nodes_visited
    end function

    subroutine assign_test_analyzer(lhs, rhs)
        class(simple_test_analyzer_t), intent(inout) :: lhs
        class(semantic_analyzer_t), intent(in) :: rhs
        
        select type(rhs)
        type is (simple_test_analyzer_t)
            lhs%analysis_executed = rhs%analysis_executed
            lhs%nodes_visited = rhs%nodes_visited
        class default
            error stop "Type mismatch in simple_test_analyzer assignment"
        end select
    end subroutine

    function get_test_dependencies(this) result(deps)
        class(simple_test_analyzer_t), intent(in) :: this
        character(len=32), allocatable :: deps(:)
        
        ! Test analyzer has no dependencies
        allocate(deps(0))
        
        associate(dummy => this)
        end associate
    end function

end module test_analyzer