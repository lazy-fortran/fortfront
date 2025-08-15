module control_flow_analyzer
    use semantic_analyzer_base, only: semantic_analyzer_t
    use ast_core, only: ast_arena_t
    use control_flow_graph_module, only: control_flow_graph_t, basic_block_t, &
        cfg_edge_t, create_control_flow_graph, &
        cfg_find_unreachable_code => find_unreachable_code, &
        get_unreachable_statements, is_block_reachable, &
        EDGE_UNCONDITIONAL, EDGE_TRUE_BRANCH, EDGE_FALSE_BRANCH, &
        EDGE_LOOP_BACK, EDGE_BREAK, EDGE_CONTINUE, EDGE_RETURN
    use cfg_builder_module, only: build_control_flow_graph
    implicit none
    private

    public :: control_flow_analyzer_t

    ! Control flow analyzer plugin
    type, extends(semantic_analyzer_t) :: control_flow_analyzer_t
        type(control_flow_graph_t) :: cfg
        logical :: analysis_complete = .false.
    contains
        procedure :: analyze => analyze_control_flow
        procedure :: get_results => get_control_flow_results
        procedure :: get_name => get_control_flow_analyzer_name
        procedure :: assign => assign_control_flow_analyzer
        
        ! Analysis methods for fluff rules
        procedure :: find_unreachable_code
        procedure :: find_hot_paths
        procedure :: detect_expensive_operations
        procedure :: analyze_loop_complexity
        procedure :: get_dead_code_locations
        procedure :: check_early_returns
    end type

contains

    subroutine analyze_control_flow(this, shared_context, arena, node_index)
        class(control_flow_analyzer_t), intent(inout) :: this
        class(*), intent(in) :: shared_context
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! Build CFG from AST using the function interface
        this%cfg = build_control_flow_graph(arena, node_index)
        
        this%analysis_complete = .true.
    end subroutine

    function get_control_flow_results(this) result(results)
        class(control_flow_analyzer_t), intent(in) :: this
        class(*), allocatable :: results
        
        ! Return the control flow graph
        allocate(control_flow_graph_t :: results)
        select type(results)
        type is (control_flow_graph_t)
            results = this%cfg
        end select
    end function

    function get_control_flow_analyzer_name(this) result(name)
        class(control_flow_analyzer_t), intent(in) :: this
        character(:), allocatable :: name
        
        name = "control_flow_analyzer"
    end function

    subroutine assign_control_flow_analyzer(lhs, rhs)
        class(control_flow_analyzer_t), intent(inout) :: lhs
        class(semantic_analyzer_t), intent(in) :: rhs
        
        select type(rhs)
        type is (control_flow_analyzer_t)
            ! Deep copy the CFG
            lhs%cfg = rhs%cfg
            lhs%analysis_complete = rhs%analysis_complete
        class default
            error stop "Type mismatch in control_flow_analyzer assignment"
        end select
    end subroutine

    ! Analysis methods for fluff rules
    function find_unreachable_code(this) result(unreachable_nodes)
        class(control_flow_analyzer_t), intent(in) :: this
        integer, allocatable :: unreachable_nodes(:)
        
        if (.not. this%analysis_complete) then
            allocate(unreachable_nodes(0))
            return
        end if
        
        ! Use existing CFG functionality to find unreachable code
        unreachable_nodes = get_unreachable_statements(this%cfg)
    end function

    function find_hot_paths(this) result(hot_path_blocks)
        class(control_flow_analyzer_t), intent(in) :: this
        integer, allocatable :: hot_path_blocks(:)
        
        if (.not. this%analysis_complete) then
            allocate(hot_path_blocks(0))
            return
        end if
        
        ! Find blocks in loops (potential hot paths for P001, P002 rules)
        hot_path_blocks = identify_loop_blocks(this%cfg)
    end function

    function detect_expensive_operations(this) result(expensive_ops)
        class(control_flow_analyzer_t), intent(in) :: this
        integer, allocatable :: expensive_ops(:)
        
        if (.not. this%analysis_complete) then
            allocate(expensive_ops(0))
            return
        end if
        
        ! Find blocks with potentially expensive operations (P003, P004 rules)
        expensive_ops = identify_expensive_blocks(this%cfg)
    end function

    function analyze_loop_complexity(this) result(complex_loops)
        class(control_flow_analyzer_t), intent(in) :: this
        integer, allocatable :: complex_loops(:)
        
        if (.not. this%analysis_complete) then
            allocate(complex_loops(0))
            return
        end if
        
        ! Find loops with high complexity (P001, P007 rules)
        complex_loops = identify_complex_loops(this%cfg)
    end function

    function get_dead_code_locations(this) result(dead_code)
        class(control_flow_analyzer_t), intent(in) :: this
        integer, allocatable :: dead_code(:)
        
        if (.not. this%analysis_complete) then
            allocate(dead_code(0))
            return
        end if
        
        ! Find unreachable code for F006 rule
        dead_code = this%find_unreachable_code()
    end function

    function check_early_returns(this) result(early_return_blocks)
        class(control_flow_analyzer_t), intent(in) :: this
        integer, allocatable :: early_return_blocks(:)
        
        if (.not. this%analysis_complete) then
            allocate(early_return_blocks(0))
            return
        end if
        
        ! Find blocks with early return patterns
        early_return_blocks = identify_early_return_blocks(this%cfg)
    end function

    ! Helper functions for analysis
    function identify_loop_blocks(cfg) result(loop_blocks)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: loop_blocks(:)
        
        integer :: i, j, count
        integer, allocatable :: temp_blocks(:)
        
        ! Find blocks with loop-back edges (indicating loops)
        count = 0
        allocate(temp_blocks(cfg%block_count))
        
        do i = 1, cfg%edge_count
            if (cfg%edges(i)%edge_type == EDGE_LOOP_BACK) then
                count = count + 1
                temp_blocks(count) = cfg%edges(i)%from_block_id
            end if
        end do
        
        ! Copy to result array
        allocate(loop_blocks(count))
        loop_blocks(1:count) = temp_blocks(1:count)
    end function

    function identify_expensive_blocks(cfg) result(expensive_blocks)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: expensive_blocks(:)
        
        ! For now, return empty - would need AST analysis of statements
        ! This would examine blocks for operations like:
        ! - Dynamic allocations
        ! - I/O operations  
        ! - Function calls in loops
        allocate(expensive_blocks(0))
    end function

    function identify_complex_loops(cfg) result(complex_loops)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: complex_loops(:)
        
        integer :: i, complexity_count
        integer, allocatable :: temp_loops(:)
        
        ! Find loops with high cyclomatic complexity
        complexity_count = 0
        allocate(temp_loops(cfg%block_count))
        
        do i = 1, cfg%block_count
            if (calculate_block_complexity(cfg, i) > 10) then
                complexity_count = complexity_count + 1
                temp_loops(complexity_count) = i
            end if
        end do
        
        allocate(complex_loops(complexity_count))
        complex_loops(1:complexity_count) = temp_loops(1:complexity_count)
    end function

    function identify_early_return_blocks(cfg) result(early_returns)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: early_returns(:)
        
        integer :: i, return_count
        integer, allocatable :: temp_returns(:)
        
        ! Find blocks with return edges that aren't the final block
        return_count = 0
        allocate(temp_returns(cfg%block_count))
        
        do i = 1, cfg%edge_count
            if (cfg%edges(i)%edge_type == EDGE_RETURN .and. &
                cfg%edges(i)%from_block_id < cfg%block_count) then
                return_count = return_count + 1
                temp_returns(return_count) = cfg%edges(i)%from_block_id
            end if
        end do
        
        allocate(early_returns(return_count))
        early_returns(1:return_count) = temp_returns(1:return_count)
    end function

    function calculate_block_complexity(cfg, block_id) result(complexity)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, intent(in) :: block_id
        integer :: complexity
        
        integer :: i, branch_count
        
        ! Simple complexity metric: count outgoing branches
        branch_count = 0
        do i = 1, cfg%edge_count
            if (cfg%edges(i)%from_block_id == block_id) then
                branch_count = branch_count + 1
            end if
        end do
        
        complexity = branch_count
    end function

end module control_flow_analyzer