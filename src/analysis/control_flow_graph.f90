module control_flow_graph_module
    use iso_fortran_env, only: error_unit
    use ast_core
    implicit none
    private

    ! Public types and procedures
    public :: control_flow_graph_t, basic_block_t, cfg_edge_t
    public :: create_control_flow_graph, add_basic_block, add_cfg_edge
    public :: find_reachable_blocks, find_unreachable_code
    public :: find_conditionally_unreachable_code
    public :: get_entry_block, get_exit_blocks, get_all_blocks
    public :: get_block_predecessors, get_block_successors
    public :: is_block_reachable, get_unreachable_statements
    public :: print_cfg, cfg_to_dot

    ! Edge types
    integer, parameter, public :: EDGE_UNCONDITIONAL = 1
    integer, parameter, public :: EDGE_TRUE_BRANCH = 2
    integer, parameter, public :: EDGE_FALSE_BRANCH = 3
    integer, parameter, public :: EDGE_LOOP_BACK = 4
    integer, parameter, public :: EDGE_BREAK = 5
    integer, parameter, public :: EDGE_CONTINUE = 6
    integer, parameter, public :: EDGE_RETURN = 7
    integer, parameter, public :: EDGE_EXCEPTION = 8
    integer, parameter, public :: EDGE_CONDITIONAL_RETURN = 9
    integer, parameter, public :: EDGE_ERROR_HANDLING = 10
    integer, parameter, public :: EDGE_SUCCESS_PATH = 11
    integer, parameter, public :: EDGE_FAILURE_PATH = 12

    ! Basic block type
    type :: basic_block_t
        integer :: block_id = 0
        character(len=:), allocatable :: label
        integer, allocatable :: statement_indices(:)  ! AST node indices
        integer :: entry_statement = 0  ! First statement
        integer :: exit_statement = 0   ! Last statement
        logical :: is_entry = .false.
        logical :: is_exit = .false.
        logical :: is_reachable = .false.
    end type basic_block_t

    ! Control flow edge
    type :: cfg_edge_t
        integer :: from_block_id
        integer :: to_block_id
        integer :: edge_type
        character(len=:), allocatable :: condition  ! For conditional edges
    end type cfg_edge_t

    ! Control flow graph
    type :: control_flow_graph_t
        type(basic_block_t), allocatable :: blocks(:)
        type(cfg_edge_t), allocatable :: edges(:)
        integer :: block_count = 0
        integer :: edge_count = 0
        integer :: block_capacity = 0
        integer :: edge_capacity = 0
        integer :: entry_block_id = 0
        character(len=:), allocatable :: procedure_name
    end type control_flow_graph_t

contains

    ! Create a new control flow graph
    function create_control_flow_graph(procedure_name) result(cfg)
        character(len=*), intent(in), optional :: procedure_name
        type(control_flow_graph_t) :: cfg
        
        ! Initialize with capacity
        cfg%block_capacity = 16
        cfg%edge_capacity = 32
        allocate(cfg%blocks(cfg%block_capacity))
        allocate(cfg%edges(cfg%edge_capacity))
        cfg%block_count = 0
        cfg%edge_count = 0
        cfg%entry_block_id = 0
        
        if (present(procedure_name)) then
            cfg%procedure_name = procedure_name
        else
            cfg%procedure_name = ""
        end if
    end function create_control_flow_graph

    ! Add a basic block to the CFG
    function add_basic_block(cfg, label, statement_indices, is_entry, is_exit) result(block_id)
        type(control_flow_graph_t), intent(inout) :: cfg
        character(len=*), intent(in), optional :: label
        integer, intent(in), optional :: statement_indices(:)
        logical, intent(in), optional :: is_entry, is_exit
        integer :: block_id
        
        type(basic_block_t) :: new_block
        type(basic_block_t), allocatable :: temp_blocks(:)
        
        ! Create new block
        cfg%block_count = cfg%block_count + 1
        block_id = cfg%block_count
        
        new_block%block_id = block_id
        if (present(label)) then
            new_block%label = label
        else
            write(new_block%label, '(A,I0)') "BB", block_id
        end if
        
        if (present(statement_indices)) then
            new_block%statement_indices = statement_indices
            if (size(statement_indices) > 0) then
                new_block%entry_statement = statement_indices(1)
                new_block%exit_statement = statement_indices(size(statement_indices))
            end if
        else
            allocate(new_block%statement_indices(0))
        end if
        
        if (present(is_entry)) new_block%is_entry = is_entry
        if (present(is_exit)) new_block%is_exit = is_exit
        
        ! Set entry block
        if (new_block%is_entry) then
            cfg%entry_block_id = block_id
            new_block%is_reachable = .true.
        end if
        
        ! Expand blocks array if needed
        if (cfg%block_count > cfg%block_capacity) then
            cfg%block_capacity = max(cfg%block_capacity + cfg%block_capacity/2, &
                                     cfg%block_capacity + 16)
            allocate(temp_blocks(cfg%block_capacity))
            temp_blocks(1:cfg%block_count-1) = cfg%blocks(1:cfg%block_count-1)
            
            ! Replace move_alloc with explicit deallocation and reallocation
            if (allocated(cfg%blocks)) deallocate(cfg%blocks)
            allocate(cfg%blocks(cfg%block_capacity))
            cfg%blocks = temp_blocks
            deallocate(temp_blocks)
        end if
        
        cfg%blocks(block_id) = new_block
    end function add_basic_block

    ! Add an edge between blocks
    subroutine add_cfg_edge(cfg, from_block_id, to_block_id, edge_type, condition)
        type(control_flow_graph_t), intent(inout) :: cfg
        integer, intent(in) :: from_block_id, to_block_id
        integer, intent(in) :: edge_type
        character(len=*), intent(in), optional :: condition
        
        type(cfg_edge_t) :: new_edge
        type(cfg_edge_t), allocatable :: temp_edges(:)
        
        ! Create new edge
        new_edge%from_block_id = from_block_id
        new_edge%to_block_id = to_block_id
        new_edge%edge_type = edge_type
        if (present(condition)) then
            new_edge%condition = condition
        else
            new_edge%condition = ""
        end if
        
        ! Expand edges array if needed
        if (cfg%edge_count >= cfg%edge_capacity) then
            cfg%edge_capacity = max(cfg%edge_capacity + cfg%edge_capacity/2, &
                                   cfg%edge_capacity + 16)
            allocate(temp_edges(cfg%edge_capacity))
            if (cfg%edge_count > 0) then
                temp_edges(1:cfg%edge_count) = cfg%edges(1:cfg%edge_count)
            end if
            
            ! Replace move_alloc with explicit deallocation and reallocation
            if (allocated(cfg%edges)) deallocate(cfg%edges)
            allocate(cfg%edges(cfg%edge_capacity))
            cfg%edges = temp_edges
            deallocate(temp_edges)
        end if
        
        cfg%edge_count = cfg%edge_count + 1
        cfg%edges(cfg%edge_count) = new_edge
    end subroutine add_cfg_edge

    ! Find all reachable blocks from entry
    subroutine find_reachable_blocks(cfg)
        type(control_flow_graph_t), intent(inout) :: cfg
        logical, allocatable :: visited(:)
        integer, allocatable :: worklist(:)
        integer :: worklist_size, current_block, i
        
        if (cfg%block_count == 0) return
        if (cfg%entry_block_id == 0) return
        
        allocate(visited(cfg%block_count))
        allocate(worklist(cfg%block_count))
        visited = .false.
        worklist_size = 0
        
        ! Reset reachability
        do i = 1, cfg%block_count
            cfg%blocks(i)%is_reachable = .false.
        end do
        
        ! Start from entry block
        worklist_size = 1
        worklist(1) = cfg%entry_block_id
        visited(cfg%entry_block_id) = .true.
        
        ! Breadth-first search
        do while (worklist_size > 0)
            current_block = worklist(1)
            
            ! Shift worklist
            do i = 1, worklist_size - 1
                worklist(i) = worklist(i + 1)
            end do
            worklist_size = worklist_size - 1
            
            ! Mark as reachable
            cfg%blocks(current_block)%is_reachable = .true.
            
            ! Add successors to worklist
            do i = 1, cfg%edge_count
                if (cfg%edges(i)%from_block_id == current_block) then
                    if (.not. visited(cfg%edges(i)%to_block_id)) then
                        visited(cfg%edges(i)%to_block_id) = .true.
                        worklist_size = worklist_size + 1
                        worklist(worklist_size) = cfg%edges(i)%to_block_id
                    end if
                end if
            end do
        end do
    end subroutine find_reachable_blocks

    ! Find unreachable code (blocks and statements)
    function find_unreachable_code(cfg) result(unreachable_blocks)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: unreachable_blocks(:)
        integer :: i, count
        
        ! Count unreachable blocks
        count = 0
        do i = 1, cfg%block_count
            if (.not. cfg%blocks(i)%is_reachable .and. &
                .not. cfg%blocks(i)%is_entry) then
                count = count + 1
            end if
        end do
        
        ! Collect unreachable blocks
        allocate(unreachable_blocks(count))
        count = 0
        do i = 1, cfg%block_count
            if (.not. cfg%blocks(i)%is_reachable .and. &
                .not. cfg%blocks(i)%is_entry) then
                count = count + 1
                unreachable_blocks(count) = i
            end if
        end do
    end function find_unreachable_code

    ! Get entry block ID
    function get_entry_block(cfg) result(entry_id)
        type(control_flow_graph_t), intent(in) :: cfg
        integer :: entry_id
        
        entry_id = cfg%entry_block_id
    end function get_entry_block

    ! Get all exit blocks
    function get_exit_blocks(cfg) result(exit_ids)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: exit_ids(:)
        integer :: i, count
        
        ! Count exit blocks
        count = 0
        do i = 1, cfg%block_count
            if (cfg%blocks(i)%is_exit) then
                count = count + 1
            end if
        end do
        
        ! Collect exit blocks
        allocate(exit_ids(count))
        count = 0
        do i = 1, cfg%block_count
            if (cfg%blocks(i)%is_exit) then
                count = count + 1
                exit_ids(count) = i
            end if
        end do
    end function get_exit_blocks

    ! Get all blocks
    function get_all_blocks(cfg) result(block_ids)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: block_ids(:)
        integer :: i
        
        allocate(block_ids(cfg%block_count))
        do i = 1, cfg%block_count
            block_ids(i) = i
        end do
    end function get_all_blocks

    ! Get predecessors of a block
    function get_block_predecessors(cfg, block_id) result(pred_ids)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, intent(in) :: block_id
        integer, allocatable :: pred_ids(:)
        integer :: i, count
        integer, allocatable :: temp_preds(:)
        
        allocate(temp_preds(cfg%block_count))
        count = 0
        
        do i = 1, cfg%edge_count
            if (cfg%edges(i)%to_block_id == block_id) then
                count = count + 1
                temp_preds(count) = cfg%edges(i)%from_block_id
            end if
        end do
        
        allocate(pred_ids(count))
        pred_ids = temp_preds(1:count)
    end function get_block_predecessors

    ! Get successors of a block
    function get_block_successors(cfg, block_id) result(succ_ids)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, intent(in) :: block_id
        integer, allocatable :: succ_ids(:)
        integer :: i, count
        integer, allocatable :: temp_succs(:)
        
        allocate(temp_succs(cfg%block_count))
        count = 0
        
        do i = 1, cfg%edge_count
            if (cfg%edges(i)%from_block_id == block_id) then
                count = count + 1
                temp_succs(count) = cfg%edges(i)%to_block_id
            end if
        end do
        
        allocate(succ_ids(count))
        succ_ids = temp_succs(1:count)
    end function get_block_successors

    ! Check if a block is reachable
    function is_block_reachable(cfg, block_id) result(reachable)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, intent(in) :: block_id
        logical :: reachable
        
        if (block_id > 0 .and. block_id <= cfg%block_count) then
            reachable = cfg%blocks(block_id)%is_reachable
        else
            reachable = .false.
        end if
    end function is_block_reachable

    ! Get all unreachable statements
    function get_unreachable_statements(cfg) result(stmt_indices)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: stmt_indices(:)
        integer :: i, j, count
        integer, allocatable :: temp_stmts(:)
        
        ! Count unreachable statements
        count = 0
        do i = 1, cfg%block_count
            if (.not. cfg%blocks(i)%is_reachable) then
                count = count + size(cfg%blocks(i)%statement_indices)
            end if
        end do
        
        ! Collect unreachable statements
        allocate(temp_stmts(count))
        count = 0
        do i = 1, cfg%block_count
            if (.not. cfg%blocks(i)%is_reachable) then
                do j = 1, size(cfg%blocks(i)%statement_indices)
                    count = count + 1
                    temp_stmts(count) = cfg%blocks(i)%statement_indices(j)
                end do
            end if
        end do
        
        allocate(stmt_indices(count))
        stmt_indices = temp_stmts(1:count)
    end function get_unreachable_statements

    ! Print CFG to console
    subroutine print_cfg(cfg)
        type(control_flow_graph_t), intent(in) :: cfg
        integer :: i
        
        print *, "Control Flow Graph for: ", trim(cfg%procedure_name)
        print *, "Blocks: ", cfg%block_count
        print *, "Edges: ", cfg%edge_count
        print *, ""
        
        ! Print blocks
        do i = 1, cfg%block_count
            print *, "Block ", i, " (", trim(cfg%blocks(i)%label), ")"
            if (cfg%blocks(i)%is_entry) print *, "  [ENTRY]"
            if (cfg%blocks(i)%is_exit) print *, "  [EXIT]"
            if (cfg%blocks(i)%is_reachable) then
                print *, "  Reachable: YES"
            else
                print *, "  Reachable: NO"
            end if
            if (size(cfg%blocks(i)%statement_indices) > 0) then
                print *, "  Statements: ", cfg%blocks(i)%statement_indices
            end if
        end do
        
        print *, ""
        print *, "Edges:"
        do i = 1, cfg%edge_count
            select case (cfg%edges(i)%edge_type)
            case (EDGE_UNCONDITIONAL)
                print *, "  ", cfg%edges(i)%from_block_id, " -> ", &
                         cfg%edges(i)%to_block_id, " [unconditional]"
            case (EDGE_TRUE_BRANCH)
                print *, "  ", cfg%edges(i)%from_block_id, " -> ", &
                         cfg%edges(i)%to_block_id, " [true: ", &
                         trim(cfg%edges(i)%condition), "]"
            case (EDGE_FALSE_BRANCH)
                print *, "  ", cfg%edges(i)%from_block_id, " -> ", &
                         cfg%edges(i)%to_block_id, " [false: ", &
                         trim(cfg%edges(i)%condition), "]"
            case (EDGE_LOOP_BACK)
                print *, "  ", cfg%edges(i)%from_block_id, " -> ", &
                         cfg%edges(i)%to_block_id, " [loop]"
            case (EDGE_RETURN)
                print *, "  ", cfg%edges(i)%from_block_id, " -> ", &
                         cfg%edges(i)%to_block_id, " [return]"
            case (EDGE_EXCEPTION)
                print *, "  ", cfg%edges(i)%from_block_id, " -> ", &
                         cfg%edges(i)%to_block_id, " [exception]"
            case (EDGE_CONDITIONAL_RETURN)
                print *, "  ", cfg%edges(i)%from_block_id, " -> ", &
                         cfg%edges(i)%to_block_id, " [conditional-return]"
            case (EDGE_ERROR_HANDLING)
                print *, "  ", cfg%edges(i)%from_block_id, " -> ", &
                         cfg%edges(i)%to_block_id, " [error-handling]"
            case (EDGE_SUCCESS_PATH)
                print *, "  ", cfg%edges(i)%from_block_id, " -> ", &
                         cfg%edges(i)%to_block_id, " [success]"
            case (EDGE_FAILURE_PATH)
                print *, "  ", cfg%edges(i)%from_block_id, " -> ", &
                         cfg%edges(i)%to_block_id, " [failure]"
            case default
                print *, "  ", cfg%edges(i)%from_block_id, " -> ", &
                         cfg%edges(i)%to_block_id
            end select
        end do
    end subroutine print_cfg

    ! Generate DOT format for GraphViz
    function cfg_to_dot(cfg) result(dot_string)
        type(control_flow_graph_t), intent(in) :: cfg
        character(len=:), allocatable :: dot_string
        character(len=1024) :: buffer
        integer :: i
        
        dot_string = "digraph CFG {" // new_line('a')
        dot_string = dot_string // '  rankdir=TB;' // new_line('a')
        dot_string = dot_string // '  node [shape=box];' // new_line('a')
        
        ! Add nodes
        do i = 1, cfg%block_count
            write(buffer, '(A,I0,A,A,A)') '  B', i, ' [label="', &
                  trim(cfg%blocks(i)%label), '"'
            dot_string = dot_string // trim(buffer)
            
            if (cfg%blocks(i)%is_entry) then
                dot_string = dot_string // ',style=filled,fillcolor=lightgreen'
            else if (cfg%blocks(i)%is_exit) then
                dot_string = dot_string // ',style=filled,fillcolor=lightcoral'
            else if (.not. cfg%blocks(i)%is_reachable) then
                dot_string = dot_string // ',style=filled,fillcolor=lightgray'
            end if
            
            dot_string = dot_string // '];' // new_line('a')
        end do
        
        ! Add edges
        do i = 1, cfg%edge_count
            write(buffer, '(A,I0,A,I0)') '  B', cfg%edges(i)%from_block_id, &
                  ' -> B', cfg%edges(i)%to_block_id
            dot_string = dot_string // trim(buffer)
            
            select case (cfg%edges(i)%edge_type)
            case (EDGE_TRUE_BRANCH)
                dot_string = dot_string // ' [label="T",color=green]'
            case (EDGE_FALSE_BRANCH)
                dot_string = dot_string // ' [label="F",color=red]'
            case (EDGE_LOOP_BACK)
                dot_string = dot_string // ' [style=dashed]'
            case (EDGE_RETURN, EDGE_CONDITIONAL_RETURN)
                dot_string = dot_string // ' [label="return",color=orange]'
            case (EDGE_EXCEPTION, EDGE_ERROR_HANDLING, EDGE_FAILURE_PATH)
                dot_string = dot_string // ' [label="error",color=red,style=dashed]'
            case (EDGE_SUCCESS_PATH)
                dot_string = dot_string // ' [label="success",color=green]'
            end select
            
            dot_string = dot_string // ';' // new_line('a')
        end do
        
        dot_string = dot_string // '}' // new_line('a')
    end function cfg_to_dot

    ! Find conditionally unreachable code in early return patterns
    function find_conditionally_unreachable_code(cfg) result(unreachable_blocks)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: unreachable_blocks(:)
        integer :: i, count
        logical, allocatable :: is_conditionally_unreachable(:)
        
        ! Initialize tracking array
        allocate(is_conditionally_unreachable(cfg%block_count))
        is_conditionally_unreachable = .false.
        
        ! Find blocks that are reachable but become unreachable due to early returns
        do i = 1, cfg%block_count
            if (cfg%blocks(i)%is_reachable) then
                ! Check if this block has predecessors with early return patterns
                call check_early_return_patterns(cfg, i, is_conditionally_unreachable)
            end if
        end do
        
        ! Count conditionally unreachable blocks
        count = 0
        do i = 1, cfg%block_count
            if (is_conditionally_unreachable(i)) then
                count = count + 1
            end if
        end do
        
        ! Collect conditionally unreachable blocks
        allocate(unreachable_blocks(count))
        count = 0
        do i = 1, cfg%block_count
            if (is_conditionally_unreachable(i)) then
                count = count + 1
                unreachable_blocks(count) = i
            end if
        end do
    end function find_conditionally_unreachable_code
    
    ! Check if a block has early return patterns in its predecessors
    subroutine check_early_return_patterns(cfg, block_id, is_conditionally_unreachable)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, intent(in) :: block_id
        logical, intent(inout) :: is_conditionally_unreachable(:)
        integer :: i, pred_id
        logical :: has_conditional_return_predecessor
        logical :: has_fallthrough_predecessor
        
        has_conditional_return_predecessor = .false.
        has_fallthrough_predecessor = .false.
        
        ! Check all edges leading to this block
        do i = 1, cfg%edge_count
            if (cfg%edges(i)%to_block_id == block_id) then
                pred_id = cfg%edges(i)%from_block_id
                
                ! Check if predecessor has early return pattern
                if (has_early_return_pattern(cfg, pred_id, cfg%edges(i)%edge_type)) then
                    has_conditional_return_predecessor = .true.
                else
                    has_fallthrough_predecessor = .true.
                end if
            end if
        end do
        
        ! If block only has conditional return predecessors and no fallthrough,
        ! it's conditionally unreachable
        if (has_conditional_return_predecessor .and. .not. has_fallthrough_predecessor) then
            is_conditionally_unreachable(block_id) = .true.
        end if
    end subroutine check_early_return_patterns
    
    ! Check if a block contains an early return pattern
    function has_early_return_pattern(cfg, block_id, edge_type) result(has_pattern)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, intent(in) :: block_id, edge_type
        logical :: has_pattern
        integer :: i
        
        has_pattern = .false.
        
        ! Check if this is a conditional return edge
        if (edge_type == EDGE_CONDITIONAL_RETURN .or. edge_type == EDGE_RETURN) then
            has_pattern = .true.
            return
        end if
        
        ! Check if block leads to return statements
        do i = 1, cfg%edge_count
            if (cfg%edges(i)%from_block_id == block_id .and. &
                (cfg%edges(i)%edge_type == EDGE_RETURN .or. &
                 cfg%edges(i)%edge_type == EDGE_CONDITIONAL_RETURN)) then
                has_pattern = .true.
                return
            end if
        end do
    end function has_early_return_pattern

end module control_flow_graph_module