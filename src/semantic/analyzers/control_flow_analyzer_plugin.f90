module control_flow_analyzer_plugin
    use base_analyzer
    use control_flow_graph_module
    use semantic_analyzer
    use ast_core
    implicit none
    private

    public :: control_flow_analyzer_t

    ! Performance metrics for fluff P-series rules
    type :: performance_metrics_t
        integer, allocatable :: hot_paths(:)          ! P001: Hot loop paths
        integer, allocatable :: expensive_ops(:)      ! P002: Expensive operations
        real, allocatable :: loop_complexity(:)       ! P003: Loop complexity scores
        integer, allocatable :: antipatterns(:)       ! P007: Detected antipatterns
        logical :: has_nested_loops = .false.
        logical :: has_recursive_calls = .false.
        integer :: max_loop_depth = 0
        real :: overall_complexity = 0.0
    end type performance_metrics_t

    ! Control flow analyzer plugin
    type, extends(base_analyzer_t) :: control_flow_analyzer_t
        type(control_flow_graph_t) :: cfg
        type(performance_metrics_t) :: perf_metrics
        character(len=:), allocatable :: current_procedure
        logical :: analysis_complete = .false.
    contains
        procedure :: analyze => analyze_control_flow
        procedure :: initialize => initialize_analyzer
        procedure :: get_control_flow_graph
        procedure :: get_hot_paths
        procedure :: get_expensive_ops
        procedure :: analyze_loop_complexity
        procedure :: detect_antipatterns
        procedure :: cleanup => cleanup_analyzer
        ! Private implementation procedures
        procedure, private :: build_cfg_from_ast
        procedure, private :: analyze_performance_characteristics
        procedure, private :: detect_hot_paths
        procedure, private :: detect_expensive_operations
        procedure, private :: calculate_loop_complexity
        procedure, private :: detect_performance_antipatterns
        procedure, private :: traverse_ast_for_cfg
        procedure, private :: add_cfg_blocks_for_node
        procedure, private :: connect_cfg_blocks
        procedure, private :: handle_if_statement
        procedure, private :: handle_do_loop
        procedure, private :: handle_do_while
        procedure, private :: add_statement_to_block
        procedure, private :: is_block_in_loop
        procedure, private :: is_expensive_operation
        procedure, private :: get_loop_depth_for_block
    end type control_flow_analyzer_t

contains

    ! Initialize the control flow analyzer
    subroutine initialize_analyzer(this, name)
        class(control_flow_analyzer_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        
        ! Set analyzer properties
        this%id%name = name
        this%id%id = 194  ! Issue number for identification
        this%enabled = .true.
        this%priority = 100  ! High priority for control flow analysis
        this%analysis_complete = .false.
        
        ! Initialize empty CFG
        this%cfg = create_control_flow_graph()
        
        ! Initialize performance metrics
        allocate(this%perf_metrics%hot_paths(0))
        allocate(this%perf_metrics%expensive_ops(0))
        allocate(this%perf_metrics%loop_complexity(0))
        allocate(this%perf_metrics%antipatterns(0))
        this%perf_metrics%has_nested_loops = .false.
        this%perf_metrics%has_recursive_calls = .false.
        this%perf_metrics%max_loop_depth = 0
        this%perf_metrics%overall_complexity = 0.0
    end subroutine initialize_analyzer

    ! Main analysis function
    function analyze_control_flow(this, ctx, arena, root_index) result(results)
        class(control_flow_analyzer_t), intent(inout) :: this
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        type(analysis_results_t) :: results
        
        ! Initialize results
        results%converged = .false.
        results%iteration_count = 1
        results%changes_made = 0
        results%confidence_score = 1.0
        results%status_message = "Control flow analysis started"
        
        ! Validate input
        if (root_index <= 0 .or. root_index > arena%size) then
            results%status_message = "Invalid root index for analysis"
            return
        end if
        
        if (.not. allocated(arena%entries(root_index)%node)) then
            results%status_message = "No AST node at root index"
            return
        end if
        
        ! Build control flow graph from AST
        call this%build_cfg_from_ast(arena, root_index)
        results%changes_made = results%changes_made + 1
        
        ! Perform reachability analysis
        call find_reachable_blocks(this%cfg)
        
        ! Analyze performance characteristics
        call this%analyze_performance_characteristics(arena)
        results%changes_made = results%changes_made + 1
        
        ! Mark analysis as complete
        this%analysis_complete = .true.
        results%converged = .true.
        results%status_message = "Control flow analysis completed successfully"
        
    end function analyze_control_flow

    ! Build CFG from AST
    subroutine build_cfg_from_ast(this, arena, root_index)
        class(control_flow_analyzer_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        integer :: block_id
        
        ! Reset CFG
        this%cfg = create_control_flow_graph("main")
        
        ! Traverse AST and build CFG
        call this%traverse_ast_for_cfg(arena, root_index, 0)
        
        ! Ensure we have at least an entry block
        if (this%cfg%block_count == 0) then
            block_id = add_basic_block(this%cfg, "entry", [root_index], &
                                     is_entry=.true., is_exit=.true.)
        end if
    end subroutine build_cfg_from_ast

    ! Traverse AST and build CFG structure
    recursive subroutine traverse_ast_for_cfg(this, arena, node_index, &
                                              parent_block_id)
        class(control_flow_analyzer_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index, parent_block_id
        integer :: block_id
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        
        type is (program_node)
            ! Create entry block for program
            block_id = add_basic_block(this%cfg, "program_entry", [node_index], &
                                     is_entry=.true.)
            call this%add_cfg_blocks_for_node(arena, node, block_id)
            
        type is (function_def_node)
            ! Create function entry block
            this%current_procedure = node%name
            block_id = add_basic_block(this%cfg, "func_" // trim(node%name), &
                                     [node_index], is_entry=.true.)
            call this%add_cfg_blocks_for_node(arena, node, block_id)
            
        type is (subroutine_def_node)
            ! Create subroutine entry block
            this%current_procedure = node%name
            block_id = add_basic_block(this%cfg, "sub_" // trim(node%name), &
                                     [node_index], is_entry=.true.)
            call this%add_cfg_blocks_for_node(arena, node, block_id)
            
        type is (if_node)
            ! Create blocks for if statement
            call this%handle_if_statement(arena, node, node_index, parent_block_id)
            
        type is (do_loop_node)
            ! Create blocks for do loop
            call this%handle_do_loop(arena, node, node_index, parent_block_id)
            
        type is (do_while_node)
            ! Create blocks for do while loop
            call this%handle_do_while(arena, node, node_index, parent_block_id)
            
        class default
            ! Regular statement - add to current block or create new one
            if (parent_block_id > 0) then
                ! Add to existing block
                call this%add_statement_to_block(parent_block_id, node_index)
            else
                ! Create new block
                block_id = add_basic_block(this%cfg, "stmt_block", [node_index])
            end if
        end select
    end subroutine traverse_ast_for_cfg

    ! Add CFG blocks for a node
    subroutine add_cfg_blocks_for_node(this, arena, node, block_id)
        class(control_flow_analyzer_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        class(ast_node), intent(in) :: node
        integer, intent(in) :: block_id
        
        ! Default implementation - can be specialized for different node types
        ! For now, just mark the block as processed
        if (block_id > 0 .and. block_id <= this%cfg%block_count) then
            this%cfg%blocks(block_id)%is_reachable = .true.
        end if
    end subroutine add_cfg_blocks_for_node

    ! Connect CFG blocks with edges
    subroutine connect_cfg_blocks(this, from_block, to_block, edge_type, condition)
        class(control_flow_analyzer_t), intent(inout) :: this
        integer, intent(in) :: from_block, to_block, edge_type
        character(len=*), intent(in), optional :: condition
        
        if (present(condition)) then
            call add_cfg_edge(this%cfg, from_block, to_block, edge_type, condition)
        else
            call add_cfg_edge(this%cfg, from_block, to_block, edge_type)
        end if
    end subroutine connect_cfg_blocks

    ! Handle if statement CFG construction
    subroutine handle_if_statement(this, arena, if_stmt, node_index, parent_block_id)
        class(control_flow_analyzer_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        type(if_node), intent(in) :: if_stmt
        integer, intent(in) :: node_index, parent_block_id
        integer :: condition_block, then_block, else_block, merge_block
        
        ! Create condition block
        condition_block = add_basic_block(this%cfg, "if_condition", [node_index])
        
        ! Create then block
        then_block = add_basic_block(this%cfg, "if_then")
        
        ! Create else block if needed
        if (allocated(if_stmt%else_body_indices)) then
            else_block = add_basic_block(this%cfg, "if_else")
        else
            else_block = 0
        end if
        
        ! Create merge block
        merge_block = add_basic_block(this%cfg, "if_merge")
        
        ! Connect blocks with appropriate edges
        if (parent_block_id > 0) then
            call this%connect_cfg_blocks(parent_block_id, condition_block, &
                                        EDGE_UNCONDITIONAL)
        end if
        
        ! True branch
        call this%connect_cfg_blocks(condition_block, then_block, &
                                    EDGE_TRUE_BRANCH, "condition")
        call this%connect_cfg_blocks(then_block, merge_block, EDGE_UNCONDITIONAL)
        
        ! False branch
        if (else_block > 0) then
            call this%connect_cfg_blocks(condition_block, else_block, &
                                        EDGE_FALSE_BRANCH, "!condition")
            call this%connect_cfg_blocks(else_block, merge_block, EDGE_UNCONDITIONAL)
        else
            call this%connect_cfg_blocks(condition_block, merge_block, &
                                        EDGE_FALSE_BRANCH, "!condition")
        end if
    end subroutine handle_if_statement

    ! Handle do loop CFG construction  
    subroutine handle_do_loop(this, arena, do_stmt, node_index, parent_block_id)
        class(control_flow_analyzer_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        type(do_loop_node), intent(in) :: do_stmt
        integer, intent(in) :: node_index, parent_block_id
        integer :: init_block, condition_block, body_block, exit_block
        
        ! Create loop blocks
        init_block = add_basic_block(this%cfg, "loop_init", [node_index])
        condition_block = add_basic_block(this%cfg, "loop_condition")
        body_block = add_basic_block(this%cfg, "loop_body")
        exit_block = add_basic_block(this%cfg, "loop_exit")
        
        ! Connect parent to init
        if (parent_block_id > 0) then
            call this%connect_cfg_blocks(parent_block_id, init_block, &
                                        EDGE_UNCONDITIONAL)
        end if
        
        ! Connect init to condition
        call this%connect_cfg_blocks(init_block, condition_block, EDGE_UNCONDITIONAL)
        
        ! Connect condition to body (true branch)
        call this%connect_cfg_blocks(condition_block, body_block, &
                                    EDGE_TRUE_BRANCH, "loop_continue")
        
        ! Connect body back to condition (loop back)
        call this%connect_cfg_blocks(body_block, condition_block, EDGE_LOOP_BACK)
        
        ! Connect condition to exit (false branch)
        call this%connect_cfg_blocks(condition_block, exit_block, &
                                    EDGE_FALSE_BRANCH, "loop_exit")
        
        ! Track loop nesting for complexity analysis
        this%perf_metrics%has_nested_loops = .true.
        this%perf_metrics%max_loop_depth = this%perf_metrics%max_loop_depth + 1
    end subroutine handle_do_loop

    ! Handle do while loop CFG construction
    subroutine handle_do_while(this, arena, do_while_stmt, node_index, parent_block_id)
        class(control_flow_analyzer_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        type(do_while_node), intent(in) :: do_while_stmt
        integer, intent(in) :: node_index, parent_block_id
        integer :: body_block, condition_block, exit_block
        
        ! Create blocks
        body_block = add_basic_block(this%cfg, "while_body", [node_index])
        condition_block = add_basic_block(this%cfg, "while_condition")
        exit_block = add_basic_block(this%cfg, "while_exit")
        
        ! Connect parent to body (do-while executes body first)
        if (parent_block_id > 0) then
            call this%connect_cfg_blocks(parent_block_id, body_block, &
                                        EDGE_UNCONDITIONAL)
        end if
        
        ! Connect body to condition
        call this%connect_cfg_blocks(body_block, condition_block, EDGE_UNCONDITIONAL)
        
        ! Connect condition back to body (true branch, loop back)
        call this%connect_cfg_blocks(condition_block, body_block, &
                                    EDGE_LOOP_BACK, "condition")
        
        ! Connect condition to exit (false branch)
        call this%connect_cfg_blocks(condition_block, exit_block, &
                                    EDGE_FALSE_BRANCH, "!condition")
    end subroutine handle_do_while

    ! Add statement to existing block
    subroutine add_statement_to_block(this, block_id, stmt_index)
        class(control_flow_analyzer_t), intent(inout) :: this
        integer, intent(in) :: block_id, stmt_index
        integer, allocatable :: temp_stmts(:)
        integer :: current_size
        
        if (block_id <= 0 .or. block_id > this%cfg%block_count) return
        
        ! Extend statement indices array
        if (allocated(this%cfg%blocks(block_id)%statement_indices)) then
            current_size = size(this%cfg%blocks(block_id)%statement_indices)
            allocate(temp_stmts(current_size + 1))
            temp_stmts(1:current_size) = this%cfg%blocks(block_id)%statement_indices
            temp_stmts(current_size + 1) = stmt_index
            
            deallocate(this%cfg%blocks(block_id)%statement_indices)
            allocate(this%cfg%blocks(block_id)%statement_indices(current_size + 1))
            this%cfg%blocks(block_id)%statement_indices = temp_stmts
        else
            allocate(this%cfg%blocks(block_id)%statement_indices(1))
            this%cfg%blocks(block_id)%statement_indices(1) = stmt_index
        end if
    end subroutine add_statement_to_block

    ! Analyze performance characteristics
    subroutine analyze_performance_characteristics(this, arena)
        class(control_flow_analyzer_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        
        ! Detect hot paths (P001)
        call this%detect_hot_paths()
        
        ! Detect expensive operations (P002)
        call this%detect_expensive_operations(arena)
        
        ! Calculate loop complexity (P003)
        call this%calculate_loop_complexity()
        
        ! Detect antipatterns (P007)
        call this%detect_performance_antipatterns(arena)
        
        ! Calculate overall complexity score
        this%perf_metrics%overall_complexity = &
            real(this%perf_metrics%max_loop_depth) + &
            real(size(this%perf_metrics%expensive_ops)) * 0.5
    end subroutine analyze_performance_characteristics

    ! Detect hot paths in CFG
    subroutine detect_hot_paths(this)
        class(control_flow_analyzer_t), intent(inout) :: this
        integer :: i, hot_count
        integer, allocatable :: temp_hot_paths(:)
        
        ! Count blocks that are in loops (have loop back edges)
        hot_count = 0
        do i = 1, this%cfg%block_count
            if (this%is_block_in_loop(i)) then
                hot_count = hot_count + 1
            end if
        end do
        
        ! Collect hot path blocks
        if (hot_count > 0) then
            allocate(temp_hot_paths(hot_count))
            hot_count = 0
            do i = 1, this%cfg%block_count
                if (this%is_block_in_loop(i)) then
                    hot_count = hot_count + 1
                    temp_hot_paths(hot_count) = i
                end if
            end do
            
            ! Update metrics
            if (allocated(this%perf_metrics%hot_paths)) then
                deallocate(this%perf_metrics%hot_paths)
            end if
            allocate(this%perf_metrics%hot_paths(hot_count))
            this%perf_metrics%hot_paths = temp_hot_paths
        end if
    end subroutine detect_hot_paths

    ! Check if block is in a loop
    function is_block_in_loop(this, block_id) result(in_loop)
        class(control_flow_analyzer_t), intent(in) :: this
        integer, intent(in) :: block_id
        logical :: in_loop
        integer :: i
        
        in_loop = .false.
        
        ! Check if any edges point back to this block or earlier blocks
        do i = 1, this%cfg%edge_count
            if (this%cfg%edges(i)%edge_type == EDGE_LOOP_BACK .and. &
                (this%cfg%edges(i)%from_block_id == block_id .or. &
                 this%cfg%edges(i)%to_block_id == block_id)) then
                in_loop = .true.
                return
            end if
        end do
    end function is_block_in_loop

    ! Detect expensive operations
    subroutine detect_expensive_operations(this, arena)
        class(control_flow_analyzer_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer :: i, j, expensive_count
        integer, allocatable :: temp_expensive(:)
        
        ! Count expensive operations
        expensive_count = 0
        do i = 1, this%cfg%block_count
            if (allocated(this%cfg%blocks(i)%statement_indices)) then
                do j = 1, size(this%cfg%blocks(i)%statement_indices)
                    if (this%is_expensive_operation(arena, &
                         this%cfg%blocks(i)%statement_indices(j))) then
                        expensive_count = expensive_count + 1
                    end if
                end do
            end if
        end do
        
        ! Collect expensive operations
        if (expensive_count > 0) then
            allocate(temp_expensive(expensive_count))
            expensive_count = 0
            do i = 1, this%cfg%block_count
                if (allocated(this%cfg%blocks(i)%statement_indices)) then
                    do j = 1, size(this%cfg%blocks(i)%statement_indices)
                        if (this%is_expensive_operation(arena, &
                             this%cfg%blocks(i)%statement_indices(j))) then
                            expensive_count = expensive_count + 1
                            temp_expensive(expensive_count) = &
                                this%cfg%blocks(i)%statement_indices(j)
                        end if
                    end do
                end if
            end do
            
            ! Update metrics
            if (allocated(this%perf_metrics%expensive_ops)) then
                deallocate(this%perf_metrics%expensive_ops)
            end if
            allocate(this%perf_metrics%expensive_ops(expensive_count))
            this%perf_metrics%expensive_ops = temp_expensive
        end if
    end subroutine detect_expensive_operations

    ! Check if operation is expensive
    function is_expensive_operation(this, arena, node_index) result(is_expensive)
        class(control_flow_analyzer_t), intent(in) :: this
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical :: is_expensive
        
        is_expensive = .false.
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select type (node => arena%entries(node_index)%node)
        type is (binary_op_node)
            ! Exponentiation is expensive
            if (trim(node%operator) == "**") then
                is_expensive = .true.
            end if
        type is (call_or_subscript_node)
            ! Math functions are expensive
            select case (trim(node%name))
            case ("sqrt", "exp", "sin", "cos", "tan", "log", "asin", "acos", "atan")
                is_expensive = .true.
            end select
        end select
    end function is_expensive_operation

    ! Calculate loop complexity scores
    subroutine calculate_loop_complexity(this)
        class(control_flow_analyzer_t), intent(inout) :: this
        integer :: i, complexity_count, loop_depth
        real, allocatable :: temp_complexity(:)
        
        ! Count loops and calculate complexity
        complexity_count = 0
        do i = 1, this%cfg%block_count
            loop_depth = this%get_loop_depth_for_block(i)
            if (loop_depth > 0) then
                complexity_count = complexity_count + 1
            end if
        end do
        
        ! Calculate complexity scores
        if (complexity_count > 0) then
            allocate(temp_complexity(complexity_count))
            complexity_count = 0
            do i = 1, this%cfg%block_count
                loop_depth = this%get_loop_depth_for_block(i)
                if (loop_depth > 0) then
                    complexity_count = complexity_count + 1
                    ! Exponential complexity for nested loops
                    temp_complexity(complexity_count) = real(loop_depth) ** 1.5
                end if
            end do
            
            ! Update metrics
            if (allocated(this%perf_metrics%loop_complexity)) then
                deallocate(this%perf_metrics%loop_complexity)
            end if
            allocate(this%perf_metrics%loop_complexity(complexity_count))
            this%perf_metrics%loop_complexity = temp_complexity
        end if
    end subroutine calculate_loop_complexity

    ! Get loop depth for a block
    function get_loop_depth_for_block(this, block_id) result(depth)
        class(control_flow_analyzer_t), intent(in) :: this
        integer, intent(in) :: block_id
        integer :: depth, i
        
        depth = 0
        
        ! Count number of loop back edges that could affect this block
        do i = 1, this%cfg%edge_count
            if (this%cfg%edges(i)%edge_type == EDGE_LOOP_BACK) then
                ! Simplified: assume each loop back edge increases depth
                if (this%cfg%edges(i)%to_block_id <= block_id .and. &
                    this%cfg%edges(i)%from_block_id >= block_id) then
                    depth = depth + 1
                end if
            end if
        end do
    end function get_loop_depth_for_block

    ! Detect performance antipatterns
    subroutine detect_performance_antipatterns(this, arena)
        class(control_flow_analyzer_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        
        ! For now, just initialize empty antipatterns
        ! This would be expanded with specific antipattern detection logic
        if (allocated(this%perf_metrics%antipatterns)) then
            deallocate(this%perf_metrics%antipatterns)
        end if
        allocate(this%perf_metrics%antipatterns(0))
    end subroutine detect_performance_antipatterns

    ! Public API functions for fluff P-series rules

    ! Get control flow graph
    function get_control_flow_graph(this) result(cfg)
        class(control_flow_analyzer_t), intent(in) :: this
        type(control_flow_graph_t) :: cfg
        cfg = this%cfg
    end function get_control_flow_graph

    ! Get hot paths (P001)
    function get_hot_paths(this) result(hot_paths)
        class(control_flow_analyzer_t), intent(in) :: this
        integer, allocatable :: hot_paths(:)
        
        if (allocated(this%perf_metrics%hot_paths)) then
            allocate(hot_paths(size(this%perf_metrics%hot_paths)))
            hot_paths = this%perf_metrics%hot_paths
        else
            allocate(hot_paths(0))
        end if
    end function get_hot_paths

    ! Get expensive operations (P002)
    function get_expensive_ops(this) result(expensive_ops)
        class(control_flow_analyzer_t), intent(in) :: this
        integer, allocatable :: expensive_ops(:)
        
        if (allocated(this%perf_metrics%expensive_ops)) then
            allocate(expensive_ops(size(this%perf_metrics%expensive_ops)))
            expensive_ops = this%perf_metrics%expensive_ops
        else
            allocate(expensive_ops(0))
        end if
    end function get_expensive_ops

    ! Analyze loop complexity (P003)
    function analyze_loop_complexity(this) result(complexity_scores)
        class(control_flow_analyzer_t), intent(in) :: this
        real, allocatable :: complexity_scores(:)
        
        if (allocated(this%perf_metrics%loop_complexity)) then
            allocate(complexity_scores(size(this%perf_metrics%loop_complexity)))
            complexity_scores = this%perf_metrics%loop_complexity
        else
            allocate(complexity_scores(0))
        end if
    end function analyze_loop_complexity

    ! Detect antipatterns (P007)
    function detect_antipatterns(this) result(antipatterns)
        class(control_flow_analyzer_t), intent(in) :: this
        integer, allocatable :: antipatterns(:)
        
        if (allocated(this%perf_metrics%antipatterns)) then
            allocate(antipatterns(size(this%perf_metrics%antipatterns)))
            antipatterns = this%perf_metrics%antipatterns
        else
            allocate(antipatterns(0))
        end if
    end function detect_antipatterns

    ! Cleanup analyzer resources
    subroutine cleanup_analyzer(this)
        class(control_flow_analyzer_t), intent(inout) :: this
        
        ! Deallocate performance metrics
        if (allocated(this%perf_metrics%hot_paths)) then
            deallocate(this%perf_metrics%hot_paths)
        end if
        if (allocated(this%perf_metrics%expensive_ops)) then
            deallocate(this%perf_metrics%expensive_ops)
        end if
        if (allocated(this%perf_metrics%loop_complexity)) then
            deallocate(this%perf_metrics%loop_complexity)
        end if
        if (allocated(this%perf_metrics%antipatterns)) then
            deallocate(this%perf_metrics%antipatterns)
        end if
        
        ! Reset state
        this%analysis_complete = .false.
        if (allocated(this%current_procedure)) then
            deallocate(this%current_procedure)
        end if
    end subroutine cleanup_analyzer

end module control_flow_analyzer_plugin