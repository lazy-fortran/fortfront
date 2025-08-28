module fortfront_advanced
    ! fortfront Advanced Functions - Main interface module
    ! This module re-exports functionality from specialized modules:
    ! - ast_node_accessors: Type-safe AST node accessors
    ! - symbol_management: Symbol table and scope analysis  
    ! - type_utilities: Type system utilities and intrinsics
    ! - expression_temporaries: Expression temporary tracking
    ! - call_graph_analysis: Call graph building and analysis
    ! - control_flow_analysis: Control flow graph functions
    ! - procedure_analysis: Procedure and parameter analysis
    
    ! Re-export all functionality from specialized modules
    ! NOTE: Multiple modules temporarily disabled due to API mismatch
    ! use ast_node_accessors
    ! use symbol_management  
    ! use type_utilities
    ! use expression_temporaries
    use call_graph_analysis
    use control_flow_analysis
    ! use procedure_analysis
    
    implicit none
    private
    
    ! Re-export all public interfaces
    ! NOTE: ast_node_accessors functions temporarily disabled
    ! public :: get_assignment_indices, get_binary_op_info, get_identifier_name, &
    !           get_literal_value, get_call_info, get_array_literal_info, &
    !           get_program_info, get_declaration_info, get_parameter_declaration_info, &
    !           get_declaration_details, get_parameter_declaration_details, &
    ! NOTE: symbol_management functions temporarily disabled
    ! public :: get_symbol_info, get_symbols_in_scope, get_symbol_references, &
    !           get_scope_info, get_all_scopes, &
    ! NOTE: expression_temporaries functions temporarily disabled
    ! public :: get_expression_temporaries, get_temporary_info, &
    !           get_active_temporary_count, get_total_temporary_count, &
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
    ! NOTE: procedure_analysis functions temporarily disabled
    ! count_procedure_parameters, get_parameter_intent, get_parameter_type, &
    ! is_parameter_optional, get_procedure_signature, &
    ! get_procedure_references, is_procedure_used_in_generic, &
    ! get_parameter_usage_context
    ! NOTE: type_utilities functions temporarily disabled 
    ! lookup_symbol, get_scope_symbols, mono_type_to_type_info, &
    ! get_type_info_for_base_type, get_integer_literal_value, &
    ! get_real_literal_value, get_string_literal_value, &
    ! is_intrinsic_function, get_intrinsic_signature, get_type_string

contains
    
    ! This module serves as a re-export interface for the refactored
    ! fortfront advanced functionality, which is now split into
    ! specialized modules for better maintainability and compliance
    ! with the 1000-line limit.

end module fortfront_advanced