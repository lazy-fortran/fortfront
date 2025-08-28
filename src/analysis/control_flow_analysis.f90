module control_flow_analysis
    ! Control flow graph building and analysis functionality  
    ! Provides APIs for building and querying control flow graphs
    
    use ast_core, only: ast_arena_t
    use control_flow_graph_module, only: control_flow_graph_t, basic_block_t, cfg_edge_t, &
                                        create_control_flow_graph, add_basic_block, add_cfg_edge, &
                                        find_reachable_blocks, find_unreachable_code, &
                                        get_entry_block, get_exit_blocks, get_all_blocks, &
                                        get_block_predecessors, get_block_successors, &
                                        is_block_reachable, get_unreachable_statements, &
                                        print_cfg, cfg_to_dot
    use cfg_builder_module, only: build_control_flow_graph
    
    implicit none
    private
    
    ! Public control flow graph functions
    public :: build_cfg_from_arena, get_unreachable_code_from_cfg, &
              get_cfg_entry_block, get_cfg_exit_blocks, &
              get_cfg_all_blocks, get_cfg_block_predecessors, &
              get_cfg_block_successors, is_cfg_block_reachable, &
              get_cfg_unreachable_statements, print_control_flow_graph, &
              export_cfg_to_dot
    
contains

    ! Build control flow graph from AST arena
    function build_cfg_from_arena(arena, root_index) result(cfg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(control_flow_graph_t) :: cfg
        
        ! Use the existing CFG builder
        cfg = build_control_flow_graph(arena, root_index)
    end function build_cfg_from_arena

    ! Get unreachable code blocks from CFG
    function get_unreachable_code_from_cfg(cfg) result(block_ids)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: block_ids(:)
        
        block_ids = find_unreachable_code(cfg)
    end function get_unreachable_code_from_cfg

    ! Get entry block ID from CFG
    function get_cfg_entry_block(cfg) result(block_id)
        type(control_flow_graph_t), intent(in) :: cfg
        integer :: block_id
        
        block_id = get_entry_block(cfg)
    end function get_cfg_entry_block

    ! Get exit block IDs from CFG
    function get_cfg_exit_blocks(cfg) result(block_ids)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: block_ids(:)
        
        block_ids = get_exit_blocks(cfg)
    end function get_cfg_exit_blocks

    ! Get all block IDs from CFG
    function get_cfg_all_blocks(cfg) result(block_ids)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: block_ids(:)
        
        block_ids = get_all_blocks(cfg)
    end function get_cfg_all_blocks

    ! Get predecessor block IDs for a given block
    function get_cfg_block_predecessors(cfg, block_id) result(pred_ids)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, intent(in) :: block_id
        integer, allocatable :: pred_ids(:)
        
        pred_ids = get_block_predecessors(cfg, block_id)
    end function get_cfg_block_predecessors

    ! Get successor block IDs for a given block
    function get_cfg_block_successors(cfg, block_id) result(succ_ids)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, intent(in) :: block_id
        integer, allocatable :: succ_ids(:)
        
        succ_ids = get_block_successors(cfg, block_id)
    end function get_cfg_block_successors

    ! Check if a block is reachable in the CFG
    function is_cfg_block_reachable(cfg, block_id) result(reachable)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, intent(in) :: block_id
        logical :: reachable
        
        reachable = is_block_reachable(cfg, block_id)
    end function is_cfg_block_reachable

    ! Get unreachable statement indices from CFG
    function get_cfg_unreachable_statements(cfg) result(stmt_indices)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: stmt_indices(:)
        
        stmt_indices = get_unreachable_statements(cfg)
    end function get_cfg_unreachable_statements

    ! Print control flow graph for debugging
    subroutine print_control_flow_graph(cfg)
        type(control_flow_graph_t), intent(in) :: cfg
        
        call print_cfg(cfg)
    end subroutine print_control_flow_graph

    ! Export CFG to DOT format for visualization
    function export_cfg_to_dot(cfg) result(dot_string)
        type(control_flow_graph_t), intent(in) :: cfg
        character(len=:), allocatable :: dot_string
        
        dot_string = cfg_to_dot(cfg)
    end function export_cfg_to_dot

end module control_flow_analysis