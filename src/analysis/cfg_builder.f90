module cfg_builder_module
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_arena
    use control_flow_graph_module
    use constant_folding_module, only: evaluate_constant_condition
    implicit none
    private

    ! Constants for constant folding results
    integer, parameter :: CONSTANT_UNKNOWN = 0
    integer, parameter :: CONSTANT_TRUE = 1
    integer, parameter :: CONSTANT_FALSE = 2

    ! Public interface
    public :: cfg_builder_t, create_cfg_builder
    public :: build_control_flow_graph

    ! CFG builder state
    type :: cfg_builder_t
        type(control_flow_graph_t) :: cfg
        integer :: current_block_id = 0
        integer, allocatable :: statement_buffer(:)
        integer :: buffer_size = 0
        integer :: buffer_capacity = 16
    end type cfg_builder_t

contains

    ! Create a new CFG builder
    function create_cfg_builder() result(builder)
        type(cfg_builder_t) :: builder
        
        builder%cfg = create_control_flow_graph()
        builder%current_block_id = 0
        allocate(builder%statement_buffer(builder%buffer_capacity))
        builder%buffer_size = 0
    end function create_cfg_builder

    ! Main entry point: build CFG from AST
    function build_control_flow_graph(arena, root_index) result(cfg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(control_flow_graph_t) :: cfg
        
        type(cfg_builder_t) :: builder
        character(len=:), allocatable :: proc_name
        
        ! Create builder
        builder = create_cfg_builder()
        
        ! Get procedure name if available
        if (allocated(arena%entries(root_index)%node)) then
            select case (arena%entries(root_index)%node_type)
            case ("program")
                select type (node => arena%entries(root_index)%node)
                type is (program_node)
                    proc_name = node%name
                class default
                    proc_name = "anonymous"
                end select
            case ("function_def")
                select type (node => arena%entries(root_index)%node)
                type is (function_def_node)
                    proc_name = node%name
                class default
                    proc_name = "anonymous"
                end select
            case ("subroutine_def")
                select type (node => arena%entries(root_index)%node)
                type is (subroutine_def_node)
                    proc_name = node%name
                class default
                    proc_name = "anonymous"
                end select
            case default
                proc_name = "anonymous"
            end select
            
            builder%cfg%procedure_name = proc_name
        end if
        
        ! Create entry block
        builder%current_block_id = add_basic_block(builder%cfg, "entry", &
                                                  is_entry=.true.)
        
        ! Process the AST
        call process_node(builder, arena, root_index)
        
        ! Flush any remaining statements
        call flush_statement_buffer(builder)
        
        ! Find reachable blocks
        call find_reachable_blocks(builder%cfg)
        
        ! Return the built CFG
        cfg = builder%cfg
    end function build_control_flow_graph

    ! Process an AST node
    recursive subroutine process_node(builder, arena, node_index)
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        character(len=:), allocatable :: node_type
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        node_type = arena%entries(node_index)%node_type
        
        
        select case (node_type)
        case ("program", "function_def", "subroutine_def")
            call process_procedure(builder, arena, node_index)
            
        case ("if_statement", "if_node")
            call process_if_statement(builder, arena, node_index)
            
        case ("do_loop", "do_loop_node")
            call process_do_loop(builder, arena, node_index)
            
        case ("do_while", "do_while_node")
            call process_do_while(builder, arena, node_index)
            
        case ("select_case", "select_case_node")
            call process_select_case(builder, arena, node_index)
            
        case ("return_statement", "return_node")
            ! Flush any buffered statements first, then process return
            call flush_statement_buffer(builder)
            call process_return(builder, arena, node_index)
            
        case ("stop_statement", "stop_node")
            ! Flush any buffered statements first, then process stop
            call flush_statement_buffer(builder)
            call process_stop(builder, arena, node_index)
            
        case ("exit_statement", "exit_node")
            call process_exit(builder, arena, node_index)
            
        case ("cycle_statement", "cycle_node")
            call process_cycle(builder, arena, node_index)
            
        case ("where_statement", "where_node")
            call process_where(builder, arena, node_index)
            
        case ("forall_statement", "forall_node")
            call process_forall(builder, arena, node_index)
            
        case ("allocate_statement", "allocate_node")
            call process_allocate(builder, arena, node_index)
            
        case ("deallocate_statement", "deallocate_node")
            call process_deallocate(builder, arena, node_index)
            
        case ("goto_statement", "goto_node")
            call process_goto(builder, arena, node_index)
            
        case ("error_stop_statement", "error_stop_node")
            call process_error_stop(builder, arena, node_index)
            
        case ("open_statement", "open_node")
            call process_io_with_exception(builder, arena, node_index)
            
        case ("read_statement", "read_node")
            call process_io_with_exception(builder, arena, node_index)
            
        case ("write_statement", "write_node")
            call process_io_with_exception(builder, arena, node_index)
            
        case default
            ! For other nodes, add to current block
            call add_statement_to_buffer(builder, node_index)
        end select
    end subroutine process_node

    ! Process a procedure (program, function, subroutine)
    subroutine process_procedure(builder, arena, node_index)
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        integer, allocatable :: body_indices(:)
        integer :: i, exit_block_id
        
        ! Get body statements
        select case (arena%entries(node_index)%node_type)
        case ("program")
            select type (node => arena%entries(node_index)%node)
            type is (program_node)
                if (allocated(node%body_indices)) then
                    allocate(body_indices(size(node%body_indices)))
                    body_indices = node%body_indices
                end if
            class default
                ! Should not happen
            end select
        case ("function_def")
            select type (node => arena%entries(node_index)%node)
            type is (function_def_node)
                if (allocated(node%body_indices)) then
                    allocate(body_indices(size(node%body_indices)))
                    body_indices = node%body_indices
                end if
            class default
                ! Should not happen
            end select
        case ("subroutine_def")
            select type (node => arena%entries(node_index)%node)
            type is (subroutine_def_node)
                if (allocated(node%body_indices)) then
                    allocate(body_indices(size(node%body_indices)))
                    body_indices = node%body_indices
                end if
            class default
                ! Should not happen
            end select
        end select
        
        ! Process body statements
        if (allocated(body_indices)) then
            do i = 1, size(body_indices)
                call process_node(builder, arena, body_indices(i))
            end do
        end if
        
        ! Create exit block if not already created
        call flush_statement_buffer(builder)
        exit_block_id = add_basic_block(builder%cfg, "exit", is_exit=.true.)
        
        ! Connect last block to exit if needed
        if (builder%current_block_id > 0 .and. &
            builder%current_block_id /= exit_block_id) then
            call add_cfg_edge(builder%cfg, builder%current_block_id, &
                             exit_block_id, EDGE_UNCONDITIONAL)
        end if
    end subroutine process_procedure

    ! Process if statement
    subroutine process_if_statement(builder, arena, node_index)
        use ast_nodes_control, only: elseif_wrapper
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        integer :: condition_index, then_block_id, merge_block_id
        integer :: current_block, else_block_id, elseif_block_id
        integer, allocatable :: then_indices(:), else_indices(:)
        type(elseif_wrapper), allocatable :: elseif_blocks(:)
        integer :: i, j
        
        ! Initialize variables
        condition_index = 0
        else_block_id = 0
        elseif_block_id = 0
        
        ! Flush current buffer
        call flush_statement_buffer(builder)
        current_block = builder%current_block_id
        
        ! Get if node details
        select type (node => arena%entries(node_index)%node)
        type is (if_node)
            condition_index = node%condition_index
            if (allocated(node%then_body_indices)) then
                allocate(then_indices(size(node%then_body_indices)))
                then_indices = node%then_body_indices
            end if
            if (allocated(node%elseif_blocks)) then
                allocate(elseif_blocks(size(node%elseif_blocks)))
                elseif_blocks = node%elseif_blocks
            end if
            if (allocated(node%else_body_indices)) then
                allocate(else_indices(size(node%else_body_indices)))
                else_indices = node%else_body_indices
            end if
        class default
            ! Should not happen if node_type was checked correctly
            return
        end select
        
        ! Check for constant folding opportunity
        block
            integer :: constant_result
            constant_result = evaluate_constant_condition(arena, condition_index)
            
            if (constant_result /= CONSTANT_UNKNOWN) then
                ! We have a constant condition - build CFG but don't connect unreachable paths
                then_block_id = add_basic_block(builder%cfg, "then")
                merge_block_id = add_basic_block(builder%cfg, "merge")
                
                ! Add condition to current block
                call add_statement_to_buffer(builder, condition_index)
                call flush_statement_buffer(builder)
                
                if (constant_result == CONSTANT_TRUE) then
                    ! Only connect the true branch
                    call add_cfg_edge(builder%cfg, current_block, then_block_id, &
                                     EDGE_TRUE_BRANCH, "if-condition")
                    
                    ! Process then branch
                    builder%current_block_id = then_block_id
                    if (allocated(then_indices) .and. size(then_indices) > 0) then
                        do i = 1, size(then_indices)
                            call process_node(builder, arena, then_indices(i))
                        end do
                    end if
                    call flush_statement_buffer(builder)
                    if (builder%current_block_id > 0) then
                        call add_cfg_edge(builder%cfg, builder%current_block_id, &
                                         merge_block_id, EDGE_UNCONDITIONAL)
                    end if
                    
                    ! Create else block but don't connect it (unreachable)
                    if (allocated(else_indices) .and. size(else_indices) > 0) then
                        else_block_id = add_basic_block(builder%cfg, "else")
                        ! Don't connect it to entry - it's unreachable
                        builder%current_block_id = else_block_id
                        do i = 1, size(else_indices)
                            call process_node(builder, arena, else_indices(i))
                        end do
                        call flush_statement_buffer(builder)
                    end if
                    
                else  ! CONSTANT_FALSE
                    ! Only connect the false branch
                    if (allocated(else_indices) .and. size(else_indices) > 0) then
                        else_block_id = add_basic_block(builder%cfg, "else")
                        call add_cfg_edge(builder%cfg, current_block, else_block_id, &
                                         EDGE_FALSE_BRANCH, "if-condition")
                        
                        ! Process else branch
                        builder%current_block_id = else_block_id
                        do i = 1, size(else_indices)
                            call process_node(builder, arena, else_indices(i))
                        end do
                        call flush_statement_buffer(builder)
                        if (builder%current_block_id > 0) then
                            call add_cfg_edge(builder%cfg, builder%current_block_id, &
                                             merge_block_id, EDGE_UNCONDITIONAL)
                        end if
                    else
                        ! No else branch, direct connection to merge
                        call add_cfg_edge(builder%cfg, current_block, merge_block_id, &
                                         EDGE_FALSE_BRANCH, "if-condition")
                    end if
                    
                    ! Create then block but don't connect it (unreachable)
                    builder%current_block_id = then_block_id
                    if (allocated(then_indices) .and. size(then_indices) > 0) then
                        do i = 1, size(then_indices)
                            call process_node(builder, arena, then_indices(i))
                        end do
                    end if
                    call flush_statement_buffer(builder)
                end if
                
                ! Continue from merge block
                builder%current_block_id = merge_block_id
                return
            end if
            
            ! If we reach here, it's a dynamic condition - proceed with normal CFG building
        end block
        
        ! Create blocks
        then_block_id = add_basic_block(builder%cfg, "then")
        merge_block_id = add_basic_block(builder%cfg, "merge")
        
        ! Add condition to current block
        call add_statement_to_buffer(builder, condition_index)
        call flush_statement_buffer(builder)
        
        ! Process then branch
        call add_cfg_edge(builder%cfg, current_block, then_block_id, &
                         EDGE_TRUE_BRANCH, "if-condition")
        
        builder%current_block_id = then_block_id
        if (allocated(then_indices) .and. size(then_indices) > 0) then
            do i = 1, size(then_indices)
                call process_node(builder, arena, then_indices(i))
            end do
        end if
        call flush_statement_buffer(builder)
        ! Only connect to merge if this branch doesn't end with a terminating statement
        if (builder%current_block_id > 0 .and. builder%current_block_id == then_block_id) then
            call add_cfg_edge(builder%cfg, builder%current_block_id, &
                             merge_block_id, EDGE_UNCONDITIONAL)
        end if
        
        ! Process elseif blocks if present
        ! Each elseif creates two blocks: one for the condition check and one for the body
        ! The chain connects: if-false -> elseif1-cond -> elseif1-body (true) or elseif2-cond (false)
        if (allocated(elseif_blocks) .and. size(elseif_blocks) > 0) then
            ! The previous condition block needs to connect to the first elseif
            elseif_block_id = add_basic_block(builder%cfg, "elseif")
            call add_cfg_edge(builder%cfg, current_block, elseif_block_id, &
                             EDGE_FALSE_BRANCH, "if-condition")
            
            do j = 1, size(elseif_blocks)
                builder%current_block_id = elseif_block_id
                
                ! Add elseif condition to block
                call add_statement_to_buffer(builder, elseif_blocks(j)%condition_index)
                call flush_statement_buffer(builder)
                
                ! Create block for elseif body
                then_block_id = add_basic_block(builder%cfg, "elseif-then")
                call add_cfg_edge(builder%cfg, elseif_block_id, then_block_id, &
                                 EDGE_TRUE_BRANCH, "elseif-condition")
                
                ! Process elseif body
                builder%current_block_id = then_block_id
                if (allocated(elseif_blocks(j)%body_indices)) then
                    do i = 1, size(elseif_blocks(j)%body_indices)
                        call process_node(builder, arena, elseif_blocks(j)%body_indices(i))
                    end do
                end if
                call flush_statement_buffer(builder)
                if (builder%current_block_id > 0) then
                    call add_cfg_edge(builder%cfg, builder%current_block_id, &
                                     merge_block_id, EDGE_UNCONDITIONAL)
                end if
                
                ! Prepare for next elseif or else
                if (j < size(elseif_blocks)) then
                    ! Create next elseif block
                    current_block = elseif_block_id
                    elseif_block_id = add_basic_block(builder%cfg, "elseif")
                    call add_cfg_edge(builder%cfg, current_block, elseif_block_id, &
                                     EDGE_FALSE_BRANCH, "elseif-condition")
                else
                    ! Last elseif - check if there's an else block
                    if (allocated(else_indices) .and. size(else_indices) > 0) then
                        else_block_id = add_basic_block(builder%cfg, "else")
                        call add_cfg_edge(builder%cfg, elseif_block_id, else_block_id, &
                                         EDGE_FALSE_BRANCH, "elseif-condition")
                    else
                        ! No else - connect to merge
                        call add_cfg_edge(builder%cfg, elseif_block_id, merge_block_id, &
                                         EDGE_FALSE_BRANCH, "elseif-condition")
                    end if
                end if
            end do
        else
            ! No elseif blocks - check for else
            if (allocated(else_indices) .and. size(else_indices) > 0) then
                else_block_id = add_basic_block(builder%cfg, "else")
                call add_cfg_edge(builder%cfg, current_block, else_block_id, &
                                 EDGE_FALSE_BRANCH, "if-condition")
            else
                call add_cfg_edge(builder%cfg, current_block, merge_block_id, &
                                 EDGE_FALSE_BRANCH, "if-condition")
                else_block_id = 0
            end if
        end if
        
        ! Process else branch if present
        if (allocated(else_indices) .and. size(else_indices) > 0 .and. else_block_id > 0) then
            builder%current_block_id = else_block_id
            do i = 1, size(else_indices)
                call process_node(builder, arena, else_indices(i))
            end do
            call flush_statement_buffer(builder)
            ! Only connect to merge if this branch doesn't end with a terminating statement
            if (builder%current_block_id > 0 .and. builder%current_block_id == else_block_id) then
                call add_cfg_edge(builder%cfg, builder%current_block_id, &
                                 merge_block_id, EDGE_UNCONDITIONAL)
            end if
        end if
        
        ! Continue from merge block
        builder%current_block_id = merge_block_id
    end subroutine process_if_statement

    ! Process do loop
    subroutine process_do_loop(builder, arena, node_index)
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        integer :: loop_header_id, loop_body_id, loop_exit_id
        integer :: current_block
        integer, allocatable :: body_indices(:)
        integer :: i
        
        ! Flush current buffer
        call flush_statement_buffer(builder)
        current_block = builder%current_block_id
        
        ! Get loop details
        select type (node => arena%entries(node_index)%node)
        type is (do_loop_node)
            if (allocated(node%body_indices)) then
                allocate(body_indices(size(node%body_indices)))
                body_indices = node%body_indices
            end if
        class default
            ! Should not happen if node_type was checked correctly
        end select
        
        ! Create blocks
        loop_header_id = add_basic_block(builder%cfg, "loop_header")
        loop_body_id = add_basic_block(builder%cfg, "loop_body")
        loop_exit_id = add_basic_block(builder%cfg, "loop_exit")
        
        ! Connect to loop header
        call add_cfg_edge(builder%cfg, current_block, loop_header_id, &
                         EDGE_UNCONDITIONAL)
        
        ! Loop header branches
        call add_cfg_edge(builder%cfg, loop_header_id, loop_body_id, &
                         EDGE_TRUE_BRANCH, "loop-condition")
        call add_cfg_edge(builder%cfg, loop_header_id, loop_exit_id, &
                         EDGE_FALSE_BRANCH, "loop-condition")
        
        ! Process loop body
        builder%current_block_id = loop_body_id
        if (allocated(body_indices)) then
            do i = 1, size(body_indices)
                call process_node(builder, arena, body_indices(i))
            end do
        end if
        call flush_statement_buffer(builder)
        
        ! Loop back edge
        if (builder%current_block_id > 0) then
            call add_cfg_edge(builder%cfg, builder%current_block_id, &
                             loop_header_id, EDGE_LOOP_BACK)
        end if
        
        ! Continue from loop exit
        builder%current_block_id = loop_exit_id
    end subroutine process_do_loop

    ! Process do while loop
    subroutine process_do_while(builder, arena, node_index)
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! Similar to do loop but with condition at the beginning
        call process_do_loop(builder, arena, node_index)
    end subroutine process_do_while

    ! Process select case
    subroutine process_select_case(builder, arena, node_index)
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        integer :: merge_block_id, current_block
        integer, allocatable :: case_blocks(:)
        integer :: i, case_block_id
        
        ! Flush current buffer
        call flush_statement_buffer(builder)
        current_block = builder%current_block_id
        
        ! Create merge block
        merge_block_id = add_basic_block(builder%cfg, "case_merge")
        
        ! For simplicity, treat as sequential blocks
        ! In a real implementation, would create branches for each case
        
        ! Continue from merge block
        builder%current_block_id = merge_block_id
    end subroutine process_select_case

    ! Process return statement
    subroutine process_return(builder, arena, node_index)
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        integer :: exit_block_id
        
        ! Add return to current block (buffer should already be flushed by caller)
        call add_statement_to_buffer(builder, node_index)
        call flush_statement_buffer(builder)
        
        ! Find or create exit block
        exit_block_id = 0
        block
            integer :: i
            do i = 1, builder%cfg%block_count
                if (builder%cfg%blocks(i)%is_exit) then
                    exit_block_id = i
                    exit
                end if
            end do
        end block
        
        if (exit_block_id == 0) then
            exit_block_id = add_basic_block(builder%cfg, "exit", is_exit=.true.)
        end if
        
        ! Add edge to exit
        call add_cfg_edge(builder%cfg, builder%current_block_id, &
                         exit_block_id, EDGE_RETURN)
        
        ! Mark this execution path as terminated
        builder%current_block_id = 0
    end subroutine process_return

    ! Process stop statement
    subroutine process_stop(builder, arena, node_index)
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! Similar to return
        call process_return(builder, arena, node_index)
    end subroutine process_stop

    ! Process exit statement
    subroutine process_exit(builder, arena, node_index)
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! Add exit to current block
        call add_statement_to_buffer(builder, node_index)
        call flush_statement_buffer(builder)
        
        ! In a real implementation, would need to find enclosing loop's exit block
        ! For now, mark as a break edge
        builder%current_block_id = 0
    end subroutine process_exit

    ! Process cycle statement
    subroutine process_cycle(builder, arena, node_index)
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! Add cycle to current block
        call add_statement_to_buffer(builder, node_index)
        call flush_statement_buffer(builder)
        
        ! In a real implementation, would need to find enclosing loop's header
        ! For now, mark as a continue edge
        builder%current_block_id = 0
    end subroutine process_cycle
    
    ! Process goto statement
    subroutine process_goto(builder, arena, node_index)
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        character(len=:), allocatable :: label
        integer :: target_block_id, current_block
        
        ! Ensure we have a valid current block
        if (builder%current_block_id == 0) then
            builder%current_block_id = add_basic_block(builder%cfg, "goto_block")
        end if
        
        current_block = builder%current_block_id
        
        ! Add goto to current block
        call add_statement_to_buffer(builder, node_index)
        call flush_statement_buffer(builder)
        
        ! Get the target label with validation
        label = "unknown"
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (goto_node)
                    if (allocated(node%label) .and. len_trim(node%label) > 0) then
                        label = node%label
                    end if
                end select
            end if
        end if
        
        ! Create or find target block (simplified - real implementation would maintain label map)
        target_block_id = add_basic_block(builder%cfg, "label_" // label)
        
        ! Add unconditional jump edge
        if (current_block > 0) then
            call add_cfg_edge(builder%cfg, current_block, target_block_id, &
                             EDGE_UNCONDITIONAL, "goto-" // label)
        end if
        
        ! Mark subsequent code as potentially unreachable
        builder%current_block_id = 0
    end subroutine process_goto
    
    ! Process error stop statement
    subroutine process_error_stop(builder, arena, node_index)
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        integer :: exit_block_id, current_block
        
        ! Ensure we have a valid current block
        if (builder%current_block_id == 0) then
            builder%current_block_id = add_basic_block(builder%cfg, "error_stop_block")
        end if
        
        current_block = builder%current_block_id
        
        ! Add error stop to current block
        call add_statement_to_buffer(builder, node_index)
        call flush_statement_buffer(builder)
        
        ! Find or create exit block
        exit_block_id = 0
        block
            integer :: i
            do i = 1, builder%cfg%block_count
                if (builder%cfg%blocks(i)%is_exit) then
                    exit_block_id = i
                    exit
                end if
            end do
        end block
        
        if (exit_block_id == 0) then
            exit_block_id = add_basic_block(builder%cfg, "exit", is_exit=.true.)
        end if
        
        ! Add edge to exit (error stop terminates program)
        if (current_block > 0) then
            call add_cfg_edge(builder%cfg, current_block, &
                             exit_block_id, EDGE_CONDITIONAL_RETURN)
        end if
        
        ! Mark subsequent code as potentially unreachable
        builder%current_block_id = 0
    end subroutine process_error_stop

    ! Process where statement
    subroutine process_where(builder, arena, node_index)
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! WHERE is not a control flow construct, add to current block
        call add_statement_to_buffer(builder, node_index)
    end subroutine process_where

    ! Process forall statement
    subroutine process_forall(builder, arena, node_index)
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! FORALL is not a control flow construct, add to current block
        call add_statement_to_buffer(builder, node_index)
    end subroutine process_forall

    ! Add statement to buffer
    subroutine add_statement_to_buffer(builder, stmt_index)
        type(cfg_builder_t), intent(inout) :: builder
        integer, intent(in) :: stmt_index
        
        integer, allocatable :: temp_buffer(:)
        
        if (stmt_index <= 0) return
        
        ! If no current block (after return/stop), create or reuse unreachable block
        if (builder%current_block_id == 0) then
            ! Look for existing unreachable block first to avoid creating multiple ones
            block
                integer :: i
                do i = 1, builder%cfg%block_count
                    if (.not. builder%cfg%blocks(i)%is_reachable .and. &
                        .not. builder%cfg%blocks(i)%is_entry .and. &
                        .not. builder%cfg%blocks(i)%is_exit) then
                        builder%current_block_id = i
                        return
                    end if
                end do
            end block
            
            ! No existing unreachable block found, create new one
            builder%current_block_id = add_basic_block(builder%cfg, "unreachable")
            ! Note: is_reachable is false by default, so this creates an unreachable block
        end if
        
        ! Expand buffer if needed
        if (builder%buffer_size >= builder%buffer_capacity) then
            builder%buffer_capacity = builder%buffer_capacity * 2
            allocate(temp_buffer(builder%buffer_capacity))
            temp_buffer(1:builder%buffer_size) = builder%statement_buffer(1:builder%buffer_size)
            
            ! Use move_alloc for O(1) performance instead of O(n) copying
            call move_alloc(temp_buffer, builder%statement_buffer)
        end if
        
        builder%buffer_size = builder%buffer_size + 1
        builder%statement_buffer(builder%buffer_size) = stmt_index
    end subroutine add_statement_to_buffer

    ! Flush statement buffer to current block
    subroutine flush_statement_buffer(builder)
        type(cfg_builder_t), intent(inout) :: builder
        
        integer :: i
        
        if (builder%buffer_size == 0) return
        if (builder%current_block_id == 0) return
        
        ! Add buffered statements to current block
        do i = 1, builder%buffer_size
            builder%cfg%blocks(builder%current_block_id)%statement_indices = &
                [builder%cfg%blocks(builder%current_block_id)%statement_indices, &
                 builder%statement_buffer(i)]
        end do
        
        ! Update entry/exit statements
        if (size(builder%cfg%blocks(builder%current_block_id)%statement_indices) > 0) then
            builder%cfg%blocks(builder%current_block_id)%entry_statement = &
                builder%cfg%blocks(builder%current_block_id)%statement_indices(1)
            builder%cfg%blocks(builder%current_block_id)%exit_statement = &
                builder%cfg%blocks(builder%current_block_id)%statement_indices( &
                    size(builder%cfg%blocks(builder%current_block_id)%statement_indices))
        end if
        
        ! Clear buffer
        builder%buffer_size = 0
    end subroutine flush_statement_buffer

    ! Process allocate statement with exception handling
    subroutine process_allocate(builder, arena, node_index)
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        integer :: success_block_id, failure_block_id, merge_block_id
        integer :: current_block
        logical :: has_stat_param, has_errmsg_param
        
        ! Check if this allocate has stat= or errmsg= parameters
        call check_allocate_exception_params(arena, node_index, has_stat_param, has_errmsg_param)
        
        if (has_stat_param .or. has_errmsg_param) then
            ! Create exception handling blocks
            call flush_statement_buffer(builder)
            current_block = builder%current_block_id
            
            ! Add allocate statement to current block
            call add_statement_to_buffer(builder, node_index)
            call flush_statement_buffer(builder)
            
            ! Create blocks for different execution paths
            success_block_id = add_basic_block(builder%cfg, "allocate_success")
            failure_block_id = add_basic_block(builder%cfg, "allocate_failure")
            merge_block_id = add_basic_block(builder%cfg, "allocate_merge")
            
            ! Add conditional edges based on allocation result
            call add_cfg_edge(builder%cfg, current_block, success_block_id, &
                             EDGE_SUCCESS_PATH, "allocation-success")
            call add_cfg_edge(builder%cfg, current_block, failure_block_id, &
                             EDGE_FAILURE_PATH, "allocation-failure")
            
            ! Connect both paths to merge block
            call add_cfg_edge(builder%cfg, success_block_id, merge_block_id, &
                             EDGE_UNCONDITIONAL)
            call add_cfg_edge(builder%cfg, failure_block_id, merge_block_id, &
                             EDGE_UNCONDITIONAL)
            
            ! Continue from merge block
            builder%current_block_id = merge_block_id
        else
            ! No exception handling, treat as regular statement
            call add_statement_to_buffer(builder, node_index)
        end if
    end subroutine process_allocate

    ! Process deallocate statement with exception handling
    subroutine process_deallocate(builder, arena, node_index)
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        integer :: success_block_id, failure_block_id, merge_block_id
        integer :: current_block
        logical :: has_stat_param, has_errmsg_param
        
        ! Check if this deallocate has stat= or errmsg= parameters
        call check_deallocate_exception_params(arena, node_index, has_stat_param, has_errmsg_param)
        
        if (has_stat_param .or. has_errmsg_param) then
            ! Create exception handling blocks
            call flush_statement_buffer(builder)
            current_block = builder%current_block_id
            
            ! Add deallocate statement to current block
            call add_statement_to_buffer(builder, node_index)
            call flush_statement_buffer(builder)
            
            ! Create blocks for different execution paths
            success_block_id = add_basic_block(builder%cfg, "deallocate_success")
            failure_block_id = add_basic_block(builder%cfg, "deallocate_failure")
            merge_block_id = add_basic_block(builder%cfg, "deallocate_merge")
            
            ! Add conditional edges based on deallocation result
            call add_cfg_edge(builder%cfg, current_block, success_block_id, &
                             EDGE_SUCCESS_PATH, "deallocation-success")
            call add_cfg_edge(builder%cfg, current_block, failure_block_id, &
                             EDGE_FAILURE_PATH, "deallocation-failure")
            
            ! Connect both paths to merge block
            call add_cfg_edge(builder%cfg, success_block_id, merge_block_id, &
                             EDGE_UNCONDITIONAL)
            call add_cfg_edge(builder%cfg, failure_block_id, merge_block_id, &
                             EDGE_UNCONDITIONAL)
            
            ! Continue from merge block
            builder%current_block_id = merge_block_id
        else
            ! No exception handling, treat as regular statement
            call add_statement_to_buffer(builder, node_index)
        end if
    end subroutine process_deallocate

    ! Process I/O statements with exception handling
    subroutine process_io_with_exception(builder, arena, node_index)
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        integer :: success_block_id, error_block_id, end_block_id, merge_block_id
        integer :: current_block
        logical :: has_iostat, has_err, has_end
        
        ! Check if this I/O statement has exception handling parameters
        call check_io_exception_params(arena, node_index, has_iostat, has_err, has_end)
        
        if (has_iostat .or. has_err .or. has_end) then
            ! Create exception handling blocks
            call flush_statement_buffer(builder)
            current_block = builder%current_block_id
            
            ! Add I/O statement to current block
            call add_statement_to_buffer(builder, node_index)
            call flush_statement_buffer(builder)
            
            ! Create blocks for different execution paths
            success_block_id = add_basic_block(builder%cfg, "io_success")
            merge_block_id = add_basic_block(builder%cfg, "io_merge")
            
            ! Add success path
            call add_cfg_edge(builder%cfg, current_block, success_block_id, &
                             EDGE_SUCCESS_PATH, "io-success")
            call add_cfg_edge(builder%cfg, success_block_id, merge_block_id, &
                             EDGE_UNCONDITIONAL)
            
            ! Add error handling paths if present
            if (has_err) then
                error_block_id = add_basic_block(builder%cfg, "io_error")
                call add_cfg_edge(builder%cfg, current_block, error_block_id, &
                                 EDGE_ERROR_HANDLING, "io-error")
                call add_cfg_edge(builder%cfg, error_block_id, merge_block_id, &
                                 EDGE_UNCONDITIONAL)
            end if
            
            if (has_end) then
                end_block_id = add_basic_block(builder%cfg, "io_end")
                call add_cfg_edge(builder%cfg, current_block, end_block_id, &
                                 EDGE_ERROR_HANDLING, "io-end-of-file")
                call add_cfg_edge(builder%cfg, end_block_id, merge_block_id, &
                                 EDGE_UNCONDITIONAL)
            end if
            
            if (.not. has_err .and. .not. has_end) then
                ! Only iostat, create general error path
                error_block_id = add_basic_block(builder%cfg, "io_error")
                call add_cfg_edge(builder%cfg, current_block, error_block_id, &
                                 EDGE_FAILURE_PATH, "io-error")
                call add_cfg_edge(builder%cfg, error_block_id, merge_block_id, &
                                 EDGE_UNCONDITIONAL)
            end if
            
            ! Continue from merge block
            builder%current_block_id = merge_block_id
        else
            ! No exception handling, treat as regular statement
            call add_statement_to_buffer(builder, node_index)
        end if
    end subroutine process_io_with_exception

    ! Helper function to check allocate statement exception parameters
    subroutine check_allocate_exception_params(arena, node_index, has_stat, has_errmsg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical, intent(out) :: has_stat, has_errmsg
        
        has_stat = .false.
        has_errmsg = .false.
        
        ! Validate arena and indices
        if (.not. allocated(arena%entries)) return
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Check node type before accessing fields
        if (arena%entries(node_index)%node_type == "allocate_statement" .or. &
            arena%entries(node_index)%node_type == "allocate_node") then
            select type (node => arena%entries(node_index)%node)
            type is (allocate_statement_node)
                has_stat = (node%stat_var_index > 0)
                has_errmsg = (node%errmsg_var_index > 0)
            end select
        end if
    end subroutine check_allocate_exception_params
    
    ! Helper function to check deallocate statement exception parameters
    subroutine check_deallocate_exception_params(arena, node_index, has_stat, has_errmsg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical, intent(out) :: has_stat, has_errmsg
        
        has_stat = .false.
        has_errmsg = .false.
        
        ! Validate arena and indices
        if (.not. allocated(arena%entries)) return
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Check node type before accessing fields
        if (arena%entries(node_index)%node_type == "deallocate_statement" .or. &
            arena%entries(node_index)%node_type == "deallocate_node") then
            select type (node => arena%entries(node_index)%node)
            type is (deallocate_statement_node)
                has_stat = (node%stat_var_index > 0)
                has_errmsg = (node%errmsg_var_index > 0)
            end select
        end if
    end subroutine check_deallocate_exception_params
    
    ! Helper function to check I/O statement exception parameters
    subroutine check_io_exception_params(arena, node_index, has_iostat, has_err, has_end)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical, intent(out) :: has_iostat, has_err, has_end
        
        has_iostat = .false.
        has_err = .false.
        has_end = .false.
        
        ! Validate arena and indices
        if (.not. allocated(arena%entries)) return
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        select case (arena%entries(node_index)%node_type)
        case ("read_statement", "read_node")
            select type (node => arena%entries(node_index)%node)
            type is (read_statement_node)
                has_iostat = (node%iostat_var_index > 0)
                has_err = (node%err_label_index > 0)
                has_end = (node%end_label_index > 0)
            class default
                ! Node type mismatch - silently return defaults
            end select
        case ("write_statement", "write_node")
            select type (node => arena%entries(node_index)%node)
            type is (write_statement_node)
                has_iostat = (node%iostat_var_index > 0)
                has_err = (node%err_label_index > 0)
                has_end = (node%end_label_index > 0)
            class default
                ! Node type mismatch - silently return defaults
            end select
        case default
            ! Unsupported node type - return defaults
        end select
    end subroutine check_io_exception_params

end module cfg_builder_module