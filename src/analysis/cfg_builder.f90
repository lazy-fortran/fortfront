module cfg_builder_module
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_arena
    use control_flow_graph_module
    implicit none
    private

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
            
        case ("if_statement")
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
        type(cfg_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        integer :: condition_index, then_block_id, else_block_id, merge_block_id
        integer :: current_block
        integer, allocatable :: then_indices(:), else_indices(:)
        integer :: i
        
        ! Initialize condition_index
        condition_index = 0
        
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
            if (allocated(node%else_body_indices)) then
                allocate(else_indices(size(node%else_body_indices)))
                else_indices = node%else_body_indices
            end if
        class default
            ! Should not happen if node_type was checked correctly
            return
        end select
        
        ! Create blocks
        then_block_id = add_basic_block(builder%cfg, "then")
        if (allocated(else_indices) .and. size(else_indices) > 0) then
            else_block_id = add_basic_block(builder%cfg, "else")
        else
            else_block_id = 0
        end if
        merge_block_id = add_basic_block(builder%cfg, "merge")
        
        ! Add condition to current block
        call add_statement_to_buffer(builder, condition_index)
        call flush_statement_buffer(builder)
        
        ! Add edges for branches
        call add_cfg_edge(builder%cfg, current_block, then_block_id, &
                         EDGE_TRUE_BRANCH, "if-condition")
        if (else_block_id > 0) then
            call add_cfg_edge(builder%cfg, current_block, else_block_id, &
                             EDGE_FALSE_BRANCH, "if-condition")
        else
            call add_cfg_edge(builder%cfg, current_block, merge_block_id, &
                             EDGE_FALSE_BRANCH, "if-condition")
        end if
        
        ! Process then branch
        builder%current_block_id = then_block_id
        if (allocated(then_indices)) then
            do i = 1, size(then_indices)
                call process_node(builder, arena, then_indices(i))
            end do
        end if
        call flush_statement_buffer(builder)
        if (builder%current_block_id > 0) then
            call add_cfg_edge(builder%cfg, builder%current_block_id, &
                             merge_block_id, EDGE_UNCONDITIONAL)
        end if
        
        ! Process else branch if present
        if (else_block_id > 0) then
            builder%current_block_id = else_block_id
            if (allocated(else_indices)) then
                do i = 1, size(else_indices)
                    call process_node(builder, arena, else_indices(i))
                end do
            end if
            call flush_statement_buffer(builder)
            if (builder%current_block_id > 0) then
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
        
        ! No current block after return
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
            
            ! Replace move_alloc with explicit deallocation and reallocation
            if (allocated(builder%statement_buffer)) deallocate(builder%statement_buffer)
            allocate(builder%statement_buffer(builder%buffer_capacity))
            builder%statement_buffer(1:builder%buffer_size) = temp_buffer(1:builder%buffer_size)
            deallocate(temp_buffer)
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

end module cfg_builder_module