module fortfront_advanced
    ! fortfront Advanced Functions - Main interface module
    ! This module re-exports functionality from specialized modules:
    ! - call_graph_analysis: Call graph building and analysis
    ! - control_flow_analysis: Control flow graph functions
    
    ! Re-export all functionality from specialized modules
    use call_graph_analysis
    use control_flow_analysis
    
    implicit none
    private
    
    ! Re-export all public interfaces
    public :: build_call_graph_from_arena, get_unused_procedures, &
              get_procedure_callers, get_procedure_callees, &
              is_procedure_used, get_all_procedures_in_graph, &
              get_call_edges, get_recursive_cycles, &
              build_cfg_from_arena, get_unreachable_code_from_cfg, &
              get_cfg_entry_block, get_cfg_exit_blocks, &
              get_cfg_all_blocks, get_cfg_block_predecessors, &
              get_cfg_block_successors, is_cfg_block_reachable, &
              get_cfg_unreachable_statements, print_control_flow_graph, &
              export_cfg_to_dot

contains
    
    ! This module serves as a re-export interface for the refactored
    ! fortfront advanced functionality, which is now split into
    ! specialized modules for better maintainability and compliance
    ! with the 1000-line limit.

end module fortfront_advanced
