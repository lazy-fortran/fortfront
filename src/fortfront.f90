module fortfront
    ! fortfront Public API - Facade module exposing all functionality for fluff
    ! This module provides a unified interface to all fortfront phases:
    ! - Lexical Analysis
    ! - AST Construction and Arena Management
    ! - Semantic Analysis with Type Inference
    ! - Code Generation
    !
    ! IMPORTANT: AST Node Access Policy
    ! =================================
    ! AST nodes MUST NOT be copied due to complex allocatable components
    ! that can cause memory corruption and segmentation faults.
    ! 
    ! USE ONLY the visitor pattern for safe node access:
    !   - visit_node_at() for visiting nodes by index
    !   - AST traversal functions with custom visitors
    !
    ! DO NOT attempt to:
    !   - Copy nodes with allocate(source=...)
    !   - Create functions that return node copies
    !   - Perform shallow copies of nodes
    !
    ! For read-only access to node properties, use:
    !   - get_node_type_id_from_arena()
    !   - get_node_source_location_from_arena()
    !   - get_node_type_kind()
    !   - get_node_type_details()
    
    ! Re-export core pipeline functionality
    use frontend, only: lex_source, parse_tokens, analyze_semantics, &
                       emit_fortran, &
                       transform_lazy_fortran_string, &
                       transform_lazy_fortran_string_with_format, &
                       compilation_options_t, format_options_t
    
    ! Re-export AST arena and core types
    use ast_core, only: ast_arena_t, ast_node, program_node, assignment_node, &
                        binary_op_node, function_def_node, identifier_node, &
                        literal_node, array_literal_node, call_or_subscript_node, &
                        subroutine_def_node, subroutine_call_node, declaration_node, &
                        parameter_declaration_node, if_node, do_loop_node, &
                        do_while_node, select_case_node, case_block_node, &
                        module_node, use_statement_node, include_statement_node, &
                        print_statement_node, write_statement_node, &
                        read_statement_node, format_descriptor_node, &
                        allocate_statement_node, deallocate_statement_node, &
                        stop_node, return_node, cycle_node, exit_node, &
                        goto_node, error_stop_node, &
                        where_node, interface_block_node, derived_type_node, &
                        pointer_assignment_node, forall_node, case_range_node, &
                        case_default_node, complex_literal_node, &
                        comment_node, contains_node, implicit_statement_node, &
                        LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, &
                        LITERAL_LOGICAL, LITERAL_ARRAY, LITERAL_COMPLEX, &
                        create_ast_arena, ast_arena_stats_t, &
                        create_function_def, create_subroutine_def, &
                        is_procedure_node, get_procedure_name, get_procedure_params, &
                        get_procedure_body, procedure_has_return_type, get_procedure_return_type
    
    ! Re-export AST node data utilities  
    use ast_nodes_data, only: intent_type_to_string, INTENT_NONE, INTENT_IN, &
                             INTENT_OUT, INTENT_INOUT
    
    ! Re-export semantic analyzer functionality
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    use expression_temporary_tracker_module, only: temp_info_t
    
    ! Re-export lexer token type
    use lexer_core, only: token_t, tokenize_core
    
    ! Re-export type system
    use type_system_hm, only: mono_type_t, poly_type_t, TINT, TREAL, TCHAR, TLOGICAL, &
                             TFUN, TARRAY, TVAR
    
    ! Re-export scope management
    use scope_manager, only: scope_stack_t, SCOPE_GLOBAL, SCOPE_MODULE, &
                           SCOPE_FUNCTION, SCOPE_SUBROUTINE, SCOPE_BLOCK, &
                           SCOPE_INTERFACE
    
    ! Re-export AST introspection APIs for issue #12
    use ast_introspection, only: visit_node_at, get_node_type_id, has_semantic_info, &
                                get_node_source_location, &
                                get_node_type_kind, get_node_type_details, &
                                get_node_type_id_from_arena, get_node_source_location_from_arena
    
    ! Re-export AST traversal and visitor functionality
    use ast_traversal, only: traverse_ast_visitor => traverse_ast, &
                            traverse_preorder, traverse_postorder, &
                            is_program_node, is_assignment_node, is_binary_op_node, &
                            is_function_def_node, is_subroutine_def_node, &
                            is_identifier_node, is_literal_node, is_declaration_node, &
                            is_if_node, is_do_loop_node, is_do_while_node, &
                            is_call_or_subscript_node, is_subroutine_call_node, &
                            is_print_statement_node, is_use_statement_node, &
                            is_select_case_node, is_derived_type_node, &
                            is_module_node, is_interface_block_node
    
    ! Re-export visitor pattern
    use ast_visitor, only: ast_visitor_t, debug_visitor_t
    
    ! Re-export call graph analysis functionality
    use call_graph_module, only: call_graph_t, create_call_graph, &
                               procedure_info_t, call_edge_t, &
                               get_all_procedures, find_unused_procedures, &
                               get_callers, get_callees, find_recursive_cycles, &
                               cg_is_procedure_used => is_procedure_used
    use call_graph_builder_module, only: build_call_graph
    
    ! Re-export control flow graph functionality
    use control_flow_graph_module, only: control_flow_graph_t, basic_block_t, cfg_edge_t, &
                                       create_control_flow_graph, add_basic_block, add_cfg_edge, &
                                       find_reachable_blocks, find_unreachable_code, &
                                       get_entry_block, get_exit_blocks, get_all_blocks, &
                                       get_block_predecessors, get_block_successors, &
                                       is_block_reachable, get_unreachable_statements, &
                                       print_cfg, cfg_to_dot, &
                                       EDGE_UNCONDITIONAL, EDGE_TRUE_BRANCH, EDGE_FALSE_BRANCH, &
                                       EDGE_LOOP_BACK, EDGE_BREAK, EDGE_CONTINUE, &
                                       EDGE_RETURN, EDGE_EXCEPTION
    use cfg_builder_module, only: build_control_flow_graph
    
    ! Variable usage tracking for issue #16
    use variable_usage_tracker_module, only: variable_usage_info_t, expression_visitor_t, &
        create_variable_usage_info, get_variables_in_expression, &
        get_identifiers_in_subtree, visit_expression_nodes, &
        is_variable_used_in_expression, count_variable_usage
    
    ! Performance optimization for issue #15
    use ast_performance_module, only: ast_cache_entry_t, memory_stats_t, &
        cache_ast, load_cached_ast, clear_ast_cache, &
        is_cache_valid, get_cache_stats, &
        release_ast_memory, compact_arena, get_memory_stats, &
        update_ast_range, supports_incremental_update, &
        lock_arena, unlock_arena, is_arena_locked, &
        deep_copy_arena, deep_copy_semantic_context, compute_arena_hash
    
    ! Re-export intrinsic function registry (using renamed imports to avoid conflicts)
    use intrinsic_registry, only: registry_is_intrinsic => is_intrinsic_function, &
                                 registry_get_signature => get_intrinsic_signature, &
                                 get_intrinsic_info, &
                                 initialize_intrinsic_registry, &
                                 intrinsic_signature_t
    
    ! Semantic query API for advanced semantic analysis (issues #189, #196)
    use semantic_query_api, only: semantic_query_t, create_semantic_query, &
                                  variable_info_t, function_info_t, &
                                  semantic_query_type_info_t => type_info_t, &
                                  symbol_info_t, &
                                  SYMBOL_VARIABLE, SYMBOL_FUNCTION, SYMBOL_SUBROUTINE, &
                                  SYMBOL_UNKNOWN, &
                                  is_identifier_defined_direct, get_unused_variables_direct, &
                                  get_symbols_in_scope_direct
    
    ! NEW: Extensible Semantic Pipeline (issue #202)
    use semantic_pipeline, only: semantic_pipeline_t, analyzer_ptr, create_pipeline
    use semantic_analyzer_base, only: semantic_analyzer_t
    use builtin_analyzers, only: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t, &
                                 call_graph_analyzer_t, control_flow_analyzer_t, &
                                 usage_tracker_analyzer_t, source_reconstruction_analyzer_t, &
                                 interface_analyzer_t
    use semantic_pipeline_integration, only: analyze_semantics_with_pipeline, &
                                             create_default_semantic_pipeline
    
    implicit none
    public
    
    ! ===== SYMBOL TABLE AND SCOPE API TYPES =====
    
    ! Symbol reference information for cross-reference analysis
    type :: symbol_reference_t
        integer :: node_index = 0        ! Where symbol is referenced
        integer :: scope_level = 0       ! Scope level of reference
        logical :: is_definition = .false. ! True if this is the declaration
        logical :: is_assignment = .false. ! True if symbol is being assigned
    end type symbol_reference_t
    
    ! Scope information type
    type :: scope_info_t
        integer :: level = 0
        integer :: scope_type = 0  ! SCOPE_GLOBAL, SCOPE_MODULE, etc.
        character(len=:), allocatable :: name
        integer :: symbol_count = 0
    end type scope_info_t
    
    ! Expression temporary information
    type :: expression_temp_info_t
        integer :: temp_id = -1
        character(len=:), allocatable :: type_name
        integer :: size_bytes = 0
        integer :: created_at_node = 0
        integer :: released_at_node = -1
        logical :: is_active = .false.
        logical :: is_reusable = .true.
    end type expression_temp_info_t
    
    ! Public symbol table interface functions
    public :: symbol_info_t, symbol_reference_t, scope_info_t
    public :: get_symbol_info, get_symbols_in_scope, get_symbol_references, &
              get_scope_info, get_all_scopes
    
    ! Public node accessor functions
    public :: get_assignment_indices, get_binary_op_info, get_identifier_name, &
              get_literal_value, get_call_info, get_array_literal_info, &
              get_program_info, get_declaration_info, get_parameter_declaration_info, &
              get_declaration_details, get_parameter_declaration_details
    
    ! Public AST introspection APIs for issue #12
    public :: visit_node_at, get_node_type_id, get_node_source_location, &
              has_semantic_info, &
              get_node_type_kind, get_node_type_details, &
              get_node_type_id_from_arena, get_node_source_location_from_arena
    
    ! Public node type identification functions for issue #34
    public :: get_node_type
    
    ! Public AST navigation APIs for issue #19
    public :: get_next_sibling, get_previous_sibling, get_block_statements, &
              is_last_in_block, is_block_node
    
    ! Public expression temporary tracking APIs for issue #26
    public :: expression_temp_info_t, get_expression_temporaries, &
              get_temporary_info, get_active_temporary_count, &
              get_total_temporary_count
    
    ! Public call graph APIs for issue #18
    public :: call_graph_t, build_call_graph_from_arena, &
              get_unused_procedures, get_procedure_callers, &
              get_procedure_callees, is_procedure_used, &
              get_all_procedures_in_graph, get_call_edges, &
              get_recursive_cycles
    
    ! Public control flow graph APIs for issue #17
    public :: control_flow_graph_t, basic_block_t, cfg_edge_t, &
              build_cfg_from_arena, get_unreachable_code_from_cfg, &
              get_cfg_entry_block, get_cfg_exit_blocks, &
              get_cfg_all_blocks, get_cfg_block_predecessors, &
              get_cfg_block_successors, is_cfg_block_reachable, &
              get_cfg_unreachable_statements, print_control_flow_graph, &
              export_cfg_to_dot

    ! Public procedure and parameter analysis APIs for issue #84
    public :: count_procedure_parameters, get_parameter_intent, &
              get_parameter_type, is_parameter_optional, &
              get_procedure_signature, get_procedure_references, &
              is_procedure_used_in_generic, get_parameter_usage_context

    ! Public variable usage tracking APIs for issue #16
    public :: variable_usage_info_t, expression_visitor_t, &
              create_variable_usage_info, get_variables_in_expression, &
              get_identifiers_in_subtree, visit_expression_nodes, &
              is_variable_used_in_expression, count_variable_usage
    
    ! Public performance optimization APIs for issue #15
    public :: ast_cache_entry_t, memory_stats_t, &
              cache_ast, load_cached_ast, clear_ast_cache, &
              is_cache_valid, get_cache_stats, &
              release_ast_memory, compact_arena, get_memory_stats, &
              update_ast_range, supports_incremental_update, &
              lock_arena, unlock_arena, is_arena_locked, &
              deep_copy_arena, deep_copy_semantic_context, compute_arena_hash
    
    ! Public semantic query APIs for issues #189, #196
    public :: semantic_query_t, create_semantic_query, &
              variable_info_t, function_info_t, semantic_query_type_info_t, &
              symbol_info_t, &
              SYMBOL_VARIABLE, SYMBOL_FUNCTION, SYMBOL_SUBROUTINE, SYMBOL_UNKNOWN, &
              is_identifier_defined_direct, get_unused_variables_direct, &
              get_symbols_in_scope_direct

    ! Public extensible semantic pipeline APIs (issue #202)
    public :: semantic_pipeline_t, analyzer_ptr, create_pipeline, &
              semantic_analyzer_t, analyze_semantics_with_pipeline, &
              create_default_semantic_pipeline

    ! Public analysis plugin APIs for fluff integration
    public :: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t, &
              call_graph_analyzer_t, control_flow_analyzer_t, &
              usage_tracker_analyzer_t, source_reconstruction_analyzer_t, &
              interface_analyzer_t
    ! Node type constants for type queries
    integer, parameter :: NODE_PROGRAM = 1
    integer, parameter :: NODE_FUNCTION_DEF = 2
    integer, parameter :: NODE_ASSIGNMENT = 3
    integer, parameter :: NODE_BINARY_OP = 4
    integer, parameter :: NODE_IDENTIFIER = 5
    integer, parameter :: NODE_LITERAL = 6
    integer, parameter :: NODE_ARRAY_LITERAL = 7
    integer, parameter :: NODE_CALL_OR_SUBSCRIPT = 8
    integer, parameter :: NODE_SUBROUTINE_DEF = 9
    integer, parameter :: NODE_SUBROUTINE_CALL = 10
    integer, parameter :: NODE_DECLARATION = 11
    integer, parameter :: NODE_PARAMETER_DECLARATION = 12
    integer, parameter :: NODE_IF = 13
    integer, parameter :: NODE_DO_LOOP = 14
    integer, parameter :: NODE_DO_WHILE = 15
    integer, parameter :: NODE_SELECT_CASE = 16
    integer, parameter :: NODE_CASE_BLOCK = 17
    integer, parameter :: NODE_MODULE = 18
    integer, parameter :: NODE_USE_STATEMENT = 19
    integer, parameter :: NODE_PRINT_STATEMENT = 20
    integer, parameter :: NODE_WRITE_STATEMENT = 21
    integer, parameter :: NODE_READ_STATEMENT = 22
    integer, parameter :: NODE_ALLOCATE_STATEMENT = 23
    integer, parameter :: NODE_DEALLOCATE_STATEMENT = 24
    integer, parameter :: NODE_STOP = 25
    integer, parameter :: NODE_RETURN = 26
    integer, parameter :: NODE_GOTO = 27
    integer, parameter :: NODE_ERROR_STOP = 28
    integer, parameter :: NODE_CYCLE = 29
    integer, parameter :: NODE_EXIT = 30
    integer, parameter :: NODE_WHERE = 31
    integer, parameter :: NODE_INTERFACE_BLOCK = 32
    integer, parameter :: NODE_DERIVED_TYPE = 33
    integer, parameter :: NODE_POINTER_ASSIGNMENT = 34
    integer, parameter :: NODE_FORALL = 35
    integer, parameter :: NODE_CASE_RANGE = 36
    integer, parameter :: NODE_CASE_DEFAULT = 37
    integer, parameter :: NODE_COMPLEX_LITERAL = 38
    integer, parameter :: NODE_INCLUDE_STATEMENT = 39
    integer, parameter :: NODE_CONTAINS = 40
    integer, parameter :: NODE_FORMAT_DESCRIPTOR = 41
    integer, parameter :: NODE_COMMENT = 42
    integer, parameter :: NODE_IMPLICIT_STATEMENT = 43
    integer, parameter :: NODE_UNKNOWN = 99
    
    ! Additional facade-specific types and procedures
    
    ! Source location tracking
    type :: source_location_t
        integer :: line = 1
        integer :: column = 1
        integer :: byte_offset = 0  ! For efficient text manipulation
    end type source_location_t
    
    type :: source_range_t
        type(source_location_t) :: start
        type(source_location_t) :: end
    end type source_range_t
    
    ! Diagnostic information
    integer, parameter :: DIAGNOSTIC_ERROR = 1
    integer, parameter :: DIAGNOSTIC_WARNING = 2
    integer, parameter :: DIAGNOSTIC_INFO = 3
    integer, parameter :: DIAGNOSTIC_HINT = 4
    
    ! Type information
    type :: type_info_t
        integer :: base_type = TVAR       ! TINT, TREAL, etc.
        integer :: bit_width = 32         ! 32, 64, etc.
        logical :: is_signed = .true.     ! For integers
        integer :: array_rank = 0         ! 0 for scalars
        integer, allocatable :: array_dims(:)  ! Shape if known
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        character(len=:), allocatable :: derived_type_name
    end type type_info_t
    
    type :: diagnostic_t
        integer :: severity = DIAGNOSTIC_INFO
        character(len=:), allocatable :: message
        type(source_range_t) :: location
        character(len=:), allocatable :: code      ! Error code (e.g., "F001")
        character(len=:), allocatable :: category  ! Error category
    end type diagnostic_t
    
    ! Enhanced symbol information type now imported from semantic_query_api (issues #189, #190)
    
    ! Function signature type
    type :: function_signature_t
        type(type_info_t), allocatable :: param_types(:)
        type(type_info_t) :: return_type
        logical :: is_elemental = .false.
        logical :: is_pure = .false.
    end type function_signature_t
    
contains
    
    ! Check if a node exists at the given index
    function node_exists(arena, node_index) result(exists)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical :: exists
        
        exists = node_index > 0 .and. node_index <= arena%size
        if (exists) then
            exists = allocated(arena%entries(node_index)%node)
        end if
    end function node_exists
    
    ! Get node type at index (returns empty string if invalid)
    function get_node_type_at(arena, node_index) result(node_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: node_type
        
        if (node_exists(arena, node_index)) then
            node_type = arena%entries(node_index)%node_type
        else
            node_type = ""
        end if
    end function get_node_type_at
    
    ! Get node line/column info
    subroutine get_node_location(arena, node_index, line, column)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: line, column
        
        line = 0
        column = 0
        if (node_exists(arena, node_index)) then
            line = arena%entries(node_index)%node%line
            column = arena%entries(node_index)%node%column
        end if
    end subroutine get_node_location
    
    
    ! Get parent node for a given node index
    function get_parent(arena, node_index) result(parent_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: parent_index
        
        parent_index = 0
        if (node_index > 0 .and. node_index <= arena%size) then
            parent_index = arena%entries(node_index)%parent_index
        end if
    end function get_parent
    
    ! Get children indices for a node
    function get_children(arena, node_index) result(child_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable :: child_indices(:)
        
        allocate(child_indices(0))
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%child_indices)) then
                child_indices = arena%entries(node_index)%child_indices(&
                    1:arena%entries(node_index)%child_count)
            end if
        end if
    end function get_children

    ! Get next sibling node in the same parent
    function get_next_sibling(arena, node_index) result(next_sibling)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: next_sibling
        
        next_sibling = arena%get_next_sibling(node_index)
    end function get_next_sibling

    ! Get previous sibling node in the same parent
    function get_previous_sibling(arena, node_index) result(prev_sibling)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: prev_sibling
        
        prev_sibling = arena%get_previous_sibling(node_index)
    end function get_previous_sibling

    ! Get all statements in a block (for block nodes like if, do, etc.)
    function get_block_statements(arena, block_index) result(stmt_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: block_index
        integer, allocatable :: stmt_indices(:)
        
        stmt_indices = arena%get_block_statements(block_index)
    end function get_block_statements

    ! Check if a statement is the last executable statement in its block
    function is_last_in_block(arena, node_index) result(is_last)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical :: is_last
        
        is_last = arena%is_last_in_block(node_index)
    end function is_last_in_block

    ! Check if a node represents a block (contains statements)
    function is_block_node(arena, node_index) result(is_block)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical :: is_block
        
        is_block = arena%is_block_node(node_index)
    end function is_block_node
    
    ! Traverse AST with callback procedure
    subroutine traverse_ast(arena, root_index, callback, pre_order)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        interface
            subroutine callback(arena, node_index)
                import :: ast_arena_t
                type(ast_arena_t), intent(in) :: arena
                integer, intent(in) :: node_index
            end subroutine callback
        end interface
        logical, intent(in), optional :: pre_order
        
        logical :: do_pre_order
        
        do_pre_order = .true.
        if (present(pre_order)) do_pre_order = pre_order
        
        call traverse_node(arena, root_index, callback, do_pre_order)
    end subroutine traverse_ast
    
    ! Internal recursive traversal
    recursive subroutine traverse_node(arena, node_index, callback, pre_order)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        interface
            subroutine callback(arena, node_index)
                import :: ast_arena_t
                type(ast_arena_t), intent(in) :: arena
                integer, intent(in) :: node_index
            end subroutine callback
        end interface
        logical, intent(in) :: pre_order
        
        integer, allocatable :: child_indices(:)
        integer :: i
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Pre-order visit
        if (pre_order) then
            call callback(arena, node_index)
        end if
        
        ! Visit children
        child_indices = get_children(arena, node_index)
        do i = 1, size(child_indices)
            call traverse_node(arena, child_indices(i), callback, pre_order)
        end do
        
        ! Post-order visit
        if (.not. pre_order) then
            call callback(arena, node_index)
        end if
    end subroutine traverse_node
    
    ! Get source range for a node
    function get_node_range(arena, node_index) result(range)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(source_range_t) :: range
        
        integer :: line, column
        
        ! Initialize with default values
        range%start%line = 1
        range%start%column = 1
        range%start%byte_offset = 0
        range%end = range%start
        
        if (node_exists(arena, node_index)) then
            call get_node_location(arena, node_index, line, column)
            range%start%line = line
            range%start%column = column
            ! End position would need to be calculated based on node content
            ! For now, use same as start
            range%end = range%start
        end if
    end function get_node_range
    
    ! Arena statistics wrapper
    function get_arena_stats(arena) result(stats)
        type(ast_arena_t), intent(in) :: arena
        type(ast_arena_stats_t) :: stats
        
        ! Direct access instead of type-bound procedure to avoid compiler crash
        stats%total_nodes = arena%size
        stats%max_depth = arena%max_depth
        stats%capacity = arena%capacity
        stats%memory_usage = arena%capacity * 64  ! Rough estimate
    end function get_arena_stats
    
    ! Analyze program with explicit context (for advanced usage)
    subroutine analyze_program(ctx, arena, prog_index)
        use semantic_analyzer, only: analyze_program_impl => analyze_program
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        
        call analyze_program_impl(ctx, arena, prog_index)
    end subroutine analyze_program
    
    ! Get type information for a node
    subroutine get_type_for_node(arena, node_index, node_type, found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(mono_type_t), allocatable, intent(out) :: node_type
        logical, intent(out) :: found
        
        integer :: i
        
        found = .false.
        if (node_exists(arena, node_index)) then
            if (allocated(arena%entries(node_index)%node)) then
                if (allocated(arena%entries(node_index)%node%inferred_type)) then
                    allocate(node_type)
                    ! Manual deep copy to avoid issues with assignment operator
                    associate (src_type => arena%entries(node_index)%node%inferred_type)
                        node_type%kind = src_type%kind
                        node_type%size = src_type%size
                        node_type%var%id = src_type%var%id
                        if (allocated(src_type%var%name)) then
                            node_type%var%name = src_type%var%name
                        else
                            allocate(character(len=0) :: node_type%var%name)
                        end if
                        if (allocated(src_type%args)) then
                            allocate(node_type%args(size(src_type%args)))
                            do i = 1, size(src_type%args)
                                ! For now, shallow copy args to avoid recursion issues
                                node_type%args(i) = src_type%args(i)
                            end do
                        end if
                    end associate
                    found = .true.
                end if
            end if
        end if
    end subroutine get_type_for_node
    
    ! Collect diagnostics (placeholder for future implementation)
    function get_diagnostics(ctx) result(diagnostics)
        type(semantic_context_t), intent(in) :: ctx
        type(diagnostic_t), allocatable :: diagnostics(:)
        
        ! Placeholder - return empty array
        allocate(diagnostics(0))
    end function get_diagnostics
    
    ! AST to JSON serialization
    subroutine ast_to_json(arena, root_index, json_string)
        use ast_json, only: ast_to_json_string
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        character(len=:), allocatable, intent(out) :: json_string
        
        json_string = ast_to_json_string(arena, root_index)
    end subroutine ast_to_json
    
    ! Semantic info to JSON (placeholder)
    subroutine semantic_info_to_json(ctx, json_string)
        type(semantic_context_t), intent(in) :: ctx
        character(len=:), allocatable, intent(out) :: json_string
        
        ! Placeholder implementation
        json_string = '{"semantic_info": "not implemented"}'
    end subroutine semantic_info_to_json
    
    ! Find nodes by type name
    function find_nodes_by_type(arena, type_name) result(indices)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: type_name
        integer, allocatable :: indices(:)
        
        indices = arena%find_by_type(type_name)
    end function find_nodes_by_type
    
    ! Get maximum depth of AST from a given node
    function get_max_depth(arena, node_index) result(max_depth)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: max_depth
        
        max_depth = compute_depth(arena, node_index, 1)  ! Start at depth 1 for root
    end function get_max_depth
    
    ! Recursively compute depth
    recursive function compute_depth(arena, node_index, current_depth) result(max_depth)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(in) :: current_depth
        integer :: max_depth
        integer, allocatable :: children(:)
        integer :: i, child_depth
        
        max_depth = current_depth
        
        if (node_index <= 0 .or. node_index > arena%size) return
        
        children = get_children(arena, node_index)
        if (allocated(children)) then
            do i = 1, size(children)
                child_depth = compute_depth(arena, children(i), current_depth + 1)
                if (child_depth > max_depth) max_depth = child_depth
            end do
        end if
    end function compute_depth
    

    ! Get node type as integer constant
    function get_node_type(arena, node_index) result(node_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: node_type
        class(ast_node), allocatable :: node
        
        node_type = NODE_UNKNOWN
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Map node type string to constant (handle both with and without _node suffix)
        select case (trim(arena%entries(node_index)%node_type))
        case ("program_node", "program")
            node_type = NODE_PROGRAM
        case ("function_def_node", "function_def")
            node_type = NODE_FUNCTION_DEF
        case ("assignment_node", "assignment")
            node_type = NODE_ASSIGNMENT
        case ("binary_op_node", "binary_op")
            node_type = NODE_BINARY_OP
        case ("identifier_node", "identifier")
            node_type = NODE_IDENTIFIER
        case ("literal_node", "literal")
            node_type = NODE_LITERAL
        case ("array_literal_node", "array_literal")
            node_type = NODE_ARRAY_LITERAL
        case ("call_or_subscript_node", "call_or_subscript")
            node_type = NODE_CALL_OR_SUBSCRIPT
        case ("subroutine_def_node", "subroutine_def")
            node_type = NODE_SUBROUTINE_DEF
        case ("subroutine_call_node", "subroutine_call")
            node_type = NODE_SUBROUTINE_CALL
        case ("declaration_node", "declaration")
            node_type = NODE_DECLARATION
        case ("parameter_declaration_node", "parameter_declaration")
            node_type = NODE_PARAMETER_DECLARATION
        case ("if_node", "if")
            node_type = NODE_IF
        case ("do_loop_node", "do_loop")
            node_type = NODE_DO_LOOP
        case ("do_while_node", "do_while")
            node_type = NODE_DO_WHILE
        case ("select_case_node", "select_case")
            node_type = NODE_SELECT_CASE
        case ("case_block_node", "case_block")
            node_type = NODE_CASE_BLOCK
        case ("module_node", "module")
            node_type = NODE_MODULE
        case ("use_statement_node", "use_statement")
            node_type = NODE_USE_STATEMENT
        case ("print_statement_node", "print_statement")
            node_type = NODE_PRINT_STATEMENT
        case ("write_statement_node", "write_statement")
            node_type = NODE_WRITE_STATEMENT
        case ("read_statement_node", "read_statement")
            node_type = NODE_READ_STATEMENT
        case ("allocate_statement_node", "allocate_statement")
            node_type = NODE_ALLOCATE_STATEMENT
        case ("deallocate_statement_node", "deallocate_statement")
            node_type = NODE_DEALLOCATE_STATEMENT
        case ("stop_node", "stop")
            node_type = NODE_STOP
        case ("return_node", "return")
            node_type = NODE_RETURN
        case ("goto_node", "goto")
            node_type = NODE_GOTO
        case ("error_stop_node", "error_stop")
            node_type = NODE_ERROR_STOP
        case ("cycle_node", "cycle")
            node_type = NODE_CYCLE
        case ("exit_node", "exit")
            node_type = NODE_EXIT
        case ("where_node", "where")
            node_type = NODE_WHERE
        case ("interface_block_node", "interface_block")
            node_type = NODE_INTERFACE_BLOCK
        case ("derived_type_node", "derived_type")
            node_type = NODE_DERIVED_TYPE
        case ("pointer_assignment_node", "pointer_assignment")
            node_type = NODE_POINTER_ASSIGNMENT
        case ("forall_node", "forall")
            node_type = NODE_FORALL
        case ("case_range_node", "case_range")
            node_type = NODE_CASE_RANGE
        case ("case_default_node", "case_default")
            node_type = NODE_CASE_DEFAULT
        case ("complex_literal_node", "complex_literal")
            node_type = NODE_COMPLEX_LITERAL
        case ("include_statement_node", "include_statement")
            node_type = NODE_INCLUDE_STATEMENT
        case ("contains_node", "contains")
            node_type = NODE_CONTAINS
        case ("format_descriptor_node", "format_descriptor")
            node_type = NODE_FORMAT_DESCRIPTOR
        case ("comment_node", "comment")
            node_type = NODE_COMMENT
        case ("implicit_statement_node", "implicit_statement")
            node_type = NODE_IMPLICIT_STATEMENT
        end select
    end function get_node_type
    
    ! Get typed node access functions
    function get_node_as_program(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(program_node), pointer :: node
        
        nullify(node)
        if (index > 0 .and. index <= arena%size) then
            if (allocated(arena%entries(index)%node)) then
                select type (p => arena%entries(index)%node)
                type is (program_node)
                    node => p
                end select
            end if
        end if
    end function get_node_as_program
    
    function get_node_as_assignment(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(assignment_node), pointer :: node
        
        nullify(node)
        if (index > 0 .and. index <= arena%size) then
            if (allocated(arena%entries(index)%node)) then
                select type (p => arena%entries(index)%node)
                type is (assignment_node)
                    node => p
                end select
            end if
        end if
    end function get_node_as_assignment
    
    function get_node_as_function_def(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(function_def_node), pointer :: node
        
        nullify(node)
        if (index > 0 .and. index <= arena%size) then
            if (allocated(arena%entries(index)%node)) then
                select type (p => arena%entries(index)%node)
                type is (function_def_node)
                    node => p
                end select
            end if
        end if
    end function get_node_as_function_def
    
    ! ===== TYPE-SAFE ACCESSOR FUNCTIONS =====
    ! These functions provide safe access to node fields without exposing &
    ! internal structure
    
    ! Assignment node accessors
    function get_assignment_indices(arena, node_index, target_index, &
                                     value_index, operator) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: target_index, value_index
        character(len=:), allocatable, intent(out) :: operator
        logical :: found
        
        found = .false.
        target_index = 0
        value_index = 0
        ! operator is intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (assignment_node)
                    target_index = node%target_index
                    value_index = node%value_index
                    if (allocated(node%operator)) then
                        operator = node%operator
                    else
                        operator = "="
                    end if
                    found = .true.
                end select
            end if
        end if
    end function get_assignment_indices
    
    ! Binary operation accessors
    function get_binary_op_info(arena, node_index, left_index, right_index, &
                                 operator) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: left_index, right_index
        character(len=:), allocatable, intent(out) :: operator
        logical :: found
        
        found = .false.
        left_index = 0
        right_index = 0
        ! operator is intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (binary_op_node)
                    left_index = node%left_index
                    right_index = node%right_index
                    if (allocated(node%operator)) then
                        operator = node%operator
                    else
                        operator = "unknown"
                    end if
                    found = .true.
                end select
            end if
        end if
    end function get_binary_op_info
    
    ! Identifier accessors
    function get_identifier_name(arena, node_index, name) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        logical :: found
        
        found = .false.
        ! name is intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (identifier_node)
                    if (allocated(node%name)) then
                        name = node%name
                        found = .true.
                    end if
                end select
            end if
        end if
    end function get_identifier_name
    
    ! Literal accessors
    function get_literal_value(arena, node_index, value, literal_type) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: value, literal_type
        logical :: found
        
        found = .false.
        ! value and literal_type are intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (literal_node)
                    if (allocated(node%value)) then
                        value = node%value
                    else
                        value = ""
                    end if
                    if (allocated(node%literal_type)) then
                        literal_type = node%literal_type
                    else
                        literal_type = "unknown"
                    end if
                    found = .true.
                end select
            end if
        end if
    end function get_literal_value
    
    ! Function/array call accessors
    function get_call_info(arena, node_index, name, arg_indices) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: arg_indices(:)
        logical :: found
        
        found = .false.
        ! name and arg_indices are intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (call_or_subscript_node)
                    if (allocated(node%name)) then
                        name = node%name
                    else
                        name = ""
                    end if
                    if (allocated(node%arg_indices)) then
                        allocate(arg_indices(size(node%arg_indices)))
                        arg_indices = node%arg_indices
                    else
                        allocate(arg_indices(0))
                    end if
                    found = .true.
                end select
            end if
        end if
    end function get_call_info
    
    ! Array literal accessors
    function get_array_literal_info(arena, node_index, element_indices, &
                                     element_type) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: element_indices(:)
        character(len=:), allocatable, intent(out) :: element_type
        logical :: found
        
        found = .false.
        ! element_indices and element_type are intent(out) - automatically &
        ! deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (array_literal_node)
                    if (allocated(node%element_indices)) then
                        allocate(element_indices(size(node%element_indices)))
                        element_indices = node%element_indices
                    else
                        allocate(element_indices(0))
                    end if
                    if (allocated(node%element_type)) then
                        element_type = node%element_type
                    else
                        element_type = "unknown"
                    end if
                    found = .true.
                end select
            end if
        end if
    end function get_array_literal_info
    
    ! Program node accessors
    function get_program_info(arena, node_index, name, body_indices) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: body_indices(:)
        logical :: found
        
        found = .false.
        ! name and body_indices are intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (program_node)
                    if (allocated(node%name)) then
                        name = node%name
                    else
                        name = ""
                    end if
                    if (allocated(node%body_indices)) then
                        allocate(body_indices(size(node%body_indices)))
                        body_indices = node%body_indices
                    else
                        allocate(body_indices(0))
                    end if
                    found = .true.
                end select
            end if
        end if
    end function get_program_info
    
    ! Comprehensive declaration node accessor - provides access to ALL &
    ! declaration details
    function get_declaration_info(arena, node_index, var_names, type_spec, &
                                   attributes) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: var_names(:)
        character(len=:), allocatable, intent(out) :: type_spec
        character(len=:), allocatable, intent(out) :: attributes(:)
        logical :: found
        
        found = .false.
        ! All output parameters are intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (declaration_node)
                    ! Get variable names
                    if (node%is_multi_declaration .and. allocated(node%var_names)) then
                        allocate(character(len=len(node%var_names)) :: &
                            var_names(size(node%var_names)))
                        var_names = node%var_names
                    else if (allocated(node%var_name)) then
                        allocate(character(len=len(node%var_name)) :: var_names(1))
                        var_names(1) = node%var_name
                    else
                        allocate(character(len=1) :: var_names(0))
                    end if
                    
                    ! Get comprehensive type specification with kind 
                    if (allocated(node%type_name)) then
                        if (node%has_kind) then
                            type_spec = node%type_name // "(" // trim(adjustl(int_to_str(node%kind_value))) // ")"
                        else
                            type_spec = node%type_name
                        end if
                    else
                        type_spec = "unknown"
                    end if
                    
                    ! Build comprehensive attributes list
                    call build_declaration_attributes(node, attributes)
                    
                    found = .true.
                end select
            end if
        end if
    end function get_declaration_info

    ! Get detailed declaration information with all fields accessible
    function get_declaration_details(arena, node_index, var_names, type_name, kind_value, has_kind, &
                                   intent_spec, has_intent, is_array, dimension_indices, &
                                   is_allocatable, is_pointer, initializer_index, has_initializer) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: var_names(:)
        character(len=:), allocatable, intent(out) :: type_name
        integer, intent(out) :: kind_value
        logical, intent(out) :: has_kind
        character(len=:), allocatable, intent(out) :: intent_spec
        logical, intent(out) :: has_intent
        logical, intent(out) :: is_array
        integer, allocatable, intent(out) :: dimension_indices(:)
        logical, intent(out) :: is_allocatable
        logical, intent(out) :: is_pointer
        integer, intent(out) :: initializer_index
        logical, intent(out) :: has_initializer
        logical :: found
        
        found = .false.
        ! Initialize output parameters
        kind_value = 0
        has_kind = .false.
        has_intent = .false.
        is_array = .false.
        is_allocatable = .false.
        is_pointer = .false.
        initializer_index = 0
        has_initializer = .false.
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (declaration_node)
                    ! Variable names
                    if (node%is_multi_declaration .and. allocated(node%var_names)) then
                        allocate(character(len=len(node%var_names)) :: &
                            var_names(size(node%var_names)))
                        var_names = node%var_names
                    else if (allocated(node%var_name)) then
                        allocate(character(len=len(node%var_name)) :: var_names(1))
                        var_names(1) = node%var_name
                    else
                        allocate(character(len=1) :: var_names(0))
                    end if
                    
                    ! Type information
                    if (allocated(node%type_name)) then
                        type_name = node%type_name
                    else
                        type_name = "unknown"
                    end if
                    
                    ! Kind information
                    kind_value = node%kind_value
                    has_kind = node%has_kind
                    
                    ! Intent information
                    if (allocated(node%intent)) then
                        intent_spec = node%intent
                    else
                        intent_spec = ""
                    end if
                    has_intent = node%has_intent
                    
                    ! Array information
                    is_array = node%is_array
                    if (allocated(node%dimension_indices)) then
                        allocate(dimension_indices(size(node%dimension_indices)))
                        dimension_indices = node%dimension_indices
                    else
                        allocate(dimension_indices(0))
                    end if
                    
                    ! Memory attributes
                    is_allocatable = node%is_allocatable
                    is_pointer = node%is_pointer
                    
                    ! Initializer information
                    initializer_index = node%initializer_index 
                    has_initializer = node%has_initializer
                    
                    found = .true.
                end select
            end if
        end if
    end function get_declaration_details
    
    ! Comprehensive parameter declaration node accessor
    function get_parameter_declaration_info(arena, node_index, var_names, values, type_spec) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: var_names(:)
        character(len=:), allocatable, intent(out) :: values(:)
        character(len=:), allocatable, intent(out) :: type_spec
        logical :: found
        
        found = .false.
        ! All output parameters are intent(out) - automatically deallocated on entry
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (parameter_declaration_node)
                    ! Get parameter names
                    if (allocated(node%name)) then
                        allocate(character(len=len(node%name)) :: var_names(1))
                        var_names(1) = node%name
                    else
                        allocate(character(len=1) :: var_names(0))
                    end if
                    
                    ! Parameter values - return empty for now (future: could access initializers)
                    allocate(character(len=1) :: values(0))
                    
                    ! Get comprehensive type specification with kind
                    if (allocated(node%type_name)) then
                        if (node%has_kind) then
                            type_spec = node%type_name // "(" // trim(adjustl(int_to_str(node%kind_value))) // ")"
                        else
                            type_spec = node%type_name
                        end if
                    else
                        type_spec = "unknown"
                    end if
                    
                    found = .true.
                end select
            end if
        end if
    end function get_parameter_declaration_info

    ! Get detailed parameter declaration information with all fields accessible  
    function get_parameter_declaration_details(arena, node_index, name, type_name, kind_value, has_kind, &
                                             intent_spec, has_intent, is_array, dimension_indices) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        character(len=:), allocatable, intent(out) :: type_name
        integer, intent(out) :: kind_value
        logical, intent(out) :: has_kind
        character(len=:), allocatable, intent(out) :: intent_spec
        logical, intent(out) :: has_intent
        logical, intent(out) :: is_array
        integer, allocatable, intent(out) :: dimension_indices(:)
        logical :: found
        
        found = .false.
        ! Initialize output parameters
        kind_value = 0
        has_kind = .false.
        has_intent = .false.
        is_array = .false.
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (node => arena%entries(node_index)%node)
                type is (parameter_declaration_node)
                    ! Parameter name
                    if (allocated(node%name)) then
                        name = node%name
                    else
                        name = ""
                    end if
                    
                    ! Type information
                    if (allocated(node%type_name)) then
                        type_name = node%type_name
                    else
                        type_name = "unknown"
                    end if
                    
                    ! Kind information
                    kind_value = node%kind_value
                    has_kind = node%has_kind
                    
                    ! Intent information
                    intent_spec = intent_type_to_string(node%intent_type)
                    has_intent = (node%intent_type /= INTENT_NONE)
                    
                    ! Array information
                    is_array = node%is_array
                    if (allocated(node%dimension_indices)) then
                        allocate(dimension_indices(size(node%dimension_indices)))
                        dimension_indices = node%dimension_indices
                    else
                        allocate(dimension_indices(0))
                    end if
                    
                    found = .true.
                end select
            end if
        end if
    end function get_parameter_declaration_details
    
    ! ===== SYMBOL TABLE AND SCOPE API IMPLEMENTATION =====
    
    ! Symbol lookup - enhanced version
    function lookup_symbol(ctx, name, scope_node_index) result(symbol)
        type(semantic_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: name
        integer, intent(in) :: scope_node_index
        type(symbol_info_t) :: symbol
        type(poly_type_t), allocatable :: scheme
        
        ! Initialize symbol with defaults (using semantic_query_api symbol_info_t fields)
        symbol%name = ""
        symbol%definition_line = 0
        symbol%definition_column = 0
        symbol%is_used = .false.
        symbol%is_parameter = .false.
        
        ! Try to find symbol in scopes
        call ctx%scopes%lookup(name, scheme)
        if (allocated(scheme)) then
            symbol%name = name
            symbol%type_info = scheme%mono
        end if
    end function lookup_symbol
    
    ! Get enhanced symbol information
    function get_symbol_info(ctx, name, scope_level) result(symbol)
        type(semantic_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: scope_level  ! If not provided, searches all scopes
        type(symbol_info_t) :: symbol
        type(poly_type_t), allocatable :: scheme
        integer :: search_scope
        
        ! Initialize with defaults (using semantic_query_api symbol_info_t fields)
        symbol%name = ""
        symbol%definition_line = 0
        symbol%definition_column = 0
        symbol%is_used = .false.
        symbol%is_parameter = .false.
        
        ! Search for symbol in scopes
        if (present(scope_level)) then
            search_scope = scope_level
        else
            search_scope = -1  ! Search all scopes
        end if
        
        call ctx%scopes%lookup(name, scheme)
        if (allocated(scheme)) then
            symbol%name = name
            symbol%type_info = scheme%mono
            
            ! Check parameter tracker for additional attributes
            if (ctx%param_tracker%count > 0) then
                call check_parameter_attributes(ctx, name, symbol)
            end if
        end if
    end function get_symbol_info
    
    ! Get all symbols in a specific scope level
    function get_symbols_in_scope(ctx, scope_level) result(symbols)
        type(semantic_context_t), intent(in) :: ctx
        integer, intent(in) :: scope_level
        type(symbol_info_t), allocatable :: symbols(:)
        integer :: i, count, symbol_idx
        
        ! Count symbols in the specified scope
        count = 0
        if (scope_level > 0 .and. scope_level <= ctx%scopes%depth) then
            count = ctx%scopes%scopes(scope_level)%env%count
        end if
        
        ! Allocate result array
        allocate(symbols(count))
        
        ! Populate symbols
        if (count > 0) then
            do i = 1, count
                symbols(i)%name = ctx%scopes%scopes(scope_level)%env%names(i)
                symbols(i)%type_info = ctx%scopes%scopes(scope_level)%env%schemes(i)%mono
                
                ! Check parameter attributes
                call check_parameter_attributes(ctx, symbols(i)%name, symbols(i))
            end do
        end if
    end function get_symbols_in_scope
    
    ! Get references to a symbol (for cross-reference analysis) - optimized single pass
    function get_symbol_references(arena, ctx, symbol_name) result(references)
        type(ast_arena_t), intent(in) :: arena
        type(semantic_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: symbol_name
        type(symbol_reference_t), allocatable :: references(:)
        integer :: i, ref_count, initial_capacity
        character(len=:), allocatable :: node_name
        
        ! Pre-allocate with reasonable initial capacity to minimize reallocations
        initial_capacity = min(64, max(1, arena%size / 10))  
        allocate(references(initial_capacity))
        ref_count = 0
        
        ! Single pass: find and collect references
        do i = 1, arena%size
            if (get_identifier_name(arena, i, node_name)) then
                if (node_name == symbol_name) then
                    ref_count = ref_count + 1
                    
                    ! Grow array if needed using geometric growth
                    if (ref_count > size(references)) then
                        references = [references, references]  ! Double the size
                    end if
                    
                    ! Populate reference information
                    references(ref_count)%node_index = i
                    references(ref_count)%scope_level = ctx%scopes%depth
                    references(ref_count)%is_definition = .false.  ! Would need AST analysis to determine
                    references(ref_count)%is_assignment = .false.  ! Would need parent node analysis
                end if
            end if
        end do
        
        ! Trim array to actual size to free unused memory
        if (ref_count < size(references)) then
            references = references(1:ref_count)
        end if
    end function get_symbol_references
    
    ! Get scope information
    function get_scope_info(ctx, scope_level) result(scope_info)
        type(semantic_context_t), intent(in) :: ctx
        integer, intent(in) :: scope_level
        type(scope_info_t) :: scope_info
        
        scope_info%level = 0
        scope_info%scope_type = 0
        scope_info%name = ""
        scope_info%symbol_count = 0
        
        if (scope_level > 0 .and. scope_level <= ctx%scopes%depth) then
            scope_info%level = scope_level
            scope_info%scope_type = ctx%scopes%scopes(scope_level)%scope_type
            scope_info%name = ctx%scopes%scopes(scope_level)%name
            scope_info%symbol_count = ctx%scopes%scopes(scope_level)%env%count
        end if
    end function get_scope_info
    
    ! Get all scope information
    function get_all_scopes(ctx) result(scopes)
        type(semantic_context_t), intent(in) :: ctx
        type(scope_info_t), allocatable :: scopes(:)
        integer :: i
        
        allocate(scopes(ctx%scopes%depth))
        
        do i = 1, ctx%scopes%depth
            scopes(i) = get_scope_info(ctx, i)
        end do
    end function get_all_scopes
    
    ! Helper to check parameter attributes
    subroutine check_parameter_attributes(ctx, name, symbol)
        type(semantic_context_t), intent(in) :: ctx
        character(len=*), intent(in) :: name
        type(symbol_info_t), intent(inout) :: symbol
        integer :: i
        
        ! Check parameter tracker for attributes
        do i = 1, ctx%param_tracker%count
            if (ctx%param_tracker%params(i)%name == name) then
                symbol%is_parameter = .true.
                exit
            end if
        end do
    end subroutine check_parameter_attributes
    
    ! Get all symbols in a scope
    function get_scope_symbols(ctx, scope_node_index) result(symbols)
        type(semantic_context_t), intent(in) :: ctx
        integer, intent(in) :: scope_node_index
        type(symbol_info_t), allocatable :: symbols(:)
        
        ! For now, return empty array as we don't have a way to enumerate all symbols
        ! This would require extending the scope_manager module
        allocate(symbols(0))
        
        ! TODO: Implement proper symbol enumeration when scope_manager supports it
    end function get_scope_symbols
    
    ! Convert mono_type to type_info
    recursive function mono_type_to_type_info(mono) result(info)
        type(mono_type_t), intent(in) :: mono
        type(type_info_t) :: info
        
        info%base_type = mono%kind
        
        select case (mono%kind)
        case (TINT)
            info%bit_width = 32
            info%is_signed = .true.
            info%array_rank = 0
        case (TREAL)
            info%bit_width = 32
            info%is_signed = .true.
            info%array_rank = 0
        case (TCHAR)
            info%bit_width = 8
            info%is_signed = .false.
            info%array_rank = 0
        case (TLOGICAL)
            info%bit_width = 32
            info%is_signed = .false.
            info%array_rank = 0
        case (TARRAY)
            if (allocated(mono%args)) then
                if (size(mono%args) > 0) then
                    info = mono_type_to_type_info(mono%args(1))  ! Element type
                    info%array_rank = 1  ! Simple 1D array for now
                end if
            end if
        end select
    end function mono_type_to_type_info
    
    ! Get type info for base type
    function get_type_info_for_base_type(base_type) result(info)
        integer, intent(in) :: base_type
        type(type_info_t) :: info
        
        info%base_type = base_type
        
        select case (base_type)
        case (TINT)
            info%bit_width = 32
            info%is_signed = .true.
            info%array_rank = 0
        case (TREAL)
            info%bit_width = 32
            info%is_signed = .true.
            info%array_rank = 0
        case (TCHAR)
            info%bit_width = 8
            info%is_signed = .false.
            info%array_rank = 0
        case (TLOGICAL)
            info%bit_width = 32
            info%is_signed = .false.
            info%array_rank = 0
        case default
            info%bit_width = 32
            info%is_signed = .false.
            info%array_rank = 0
        end select
    end function get_type_info_for_base_type
    
    ! Get integer literal value
    function get_integer_literal_value(arena, node_index) result(value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer(kind=8) :: value
        class(ast_node), allocatable :: node
        integer :: i
        
        value = 0
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (p => arena%entries(node_index)%node)
                type is (literal_node)
                    if (p%literal_kind == LITERAL_INTEGER) then
                        read(p%value, *, iostat=i) value
                    end if
                end select
            end if
        end if
    end function get_integer_literal_value
    
    ! Get real literal value
    function get_real_literal_value(arena, node_index) result(value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        real(kind=8) :: value
        class(ast_node), allocatable :: node
        integer :: i
        
        value = 0.0
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (p => arena%entries(node_index)%node)
                type is (literal_node)
                    if (p%literal_kind == LITERAL_REAL) then
                        read(p%value, *, iostat=i) value
                    end if
                end select
            end if
        end if
    end function get_real_literal_value
    
    ! Get string literal value
    function get_string_literal_value(arena, node_index) result(value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: value
        class(ast_node), allocatable :: node
        
        value = ""
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type (p => arena%entries(node_index)%node)
                type is (literal_node)
                    if (p%literal_kind == LITERAL_STRING) then
                        ! Remove quotes if present
                        if (len(p%value) >= 2) then
                            if ((p%value(1:1) == '"' .and. &
                                 p%value(len(p%value):len(p%value)) == '"') .or. &
                                (p%value(1:1) == "'" .and. &
                                 p%value(len(p%value):len(p%value)) == "'")) then
                                value = p%value(2:len(p%value)-1)
                            else
                                value = p%value
                            end if
                        else
                            value = p%value
                        end if
                    end if
                end select
            end if
        end if
    end function get_string_literal_value
    
    ! Check if function is intrinsic
    function is_intrinsic_function(name) result(is_intrinsic)
        character(len=*), intent(in) :: name
        logical :: is_intrinsic
        
        ! Common Fortran intrinsic functions
        select case (trim(adjustl(name)))
        case ("sin", "cos", "tan", "asin", "acos", "atan", "atan2", &
              "exp", "log", "log10", "sqrt", "abs", &
              "max", "min", "mod", "sign", &
              "int", "real", "dble", "cmplx", &
              "len", "len_trim", "trim", "adjustl", "adjustr", &
              "index", "scan", "verify", &
              "allocated", "associated", "present", &
              "size", "shape", "lbound", "ubound", &
              "merge", "pack", "unpack", "reshape", &
              "matmul", "dot_product", "transpose", &
              "sum", "product", "maxval", "minval", &
              "count", "any", "all")
            is_intrinsic = .true.
        case default
            is_intrinsic = .false.
        end select
    end function is_intrinsic_function
    
    ! Get intrinsic function signature
    function get_intrinsic_signature(name) result(signature)
        character(len=*), intent(in) :: name
        type(function_signature_t) :: signature
        
        ! Initialize with defaults
        signature%is_elemental = .false.
        signature%is_pure = .true.
        
        ! Simple examples for common intrinsics
        select case (trim(adjustl(name)))
        case ("sin", "cos", "tan", "exp", "log", "sqrt", "abs")
            allocate(signature%param_types(1))
            signature%param_types(1) = get_type_info_for_base_type(TREAL)
            signature%return_type = get_type_info_for_base_type(TREAL)
            signature%is_elemental = .true.
        case ("int")
            allocate(signature%param_types(1))
            signature%param_types(1) = get_type_info_for_base_type(TREAL)
            signature%return_type = get_type_info_for_base_type(TINT)
            signature%is_elemental = .true.
        case ("real", "dble")
            allocate(signature%param_types(1))
            signature%param_types(1) = get_type_info_for_base_type(TINT)
            signature%return_type = get_type_info_for_base_type(TREAL)
            signature%is_elemental = .true.
        case ("len", "len_trim")
            allocate(signature%param_types(1))
            signature%param_types(1) = get_type_info_for_base_type(TCHAR)
            signature%return_type = get_type_info_for_base_type(TINT)
        case default
            ! Unknown intrinsic - return empty signature
        end select
    end function get_intrinsic_signature
    
    ! Get string representation of type
    function get_type_string(type_info) result(type_str)
        type(type_info_t), intent(in) :: type_info
        character(len=:), allocatable :: type_str
        character(len=10) :: bit_str
        
        select case (type_info%base_type)
        case (TINT)
            write(bit_str, '(I0)') type_info%bit_width / 8
            type_str = "integer(" // trim(bit_str) // ")"
        case (TREAL)
            write(bit_str, '(I0)') type_info%bit_width / 8
            type_str = "real(" // trim(bit_str) // ")"
        case (TCHAR)
            type_str = "character"
        case (TLOGICAL)
            type_str = "logical"
        case (TARRAY)
            type_str = "array"
            if (type_info%array_rank > 0) then
                write(bit_str, '(I0)') type_info%array_rank
                type_str = type_str // "(" // trim(bit_str) // "D)"
            end if
        case (TFUN)
            type_str = "function"
        case (TVAR)
            type_str = "type_variable"
        case default
            type_str = "unknown"
        end select
        
        if (type_info%array_rank > 0 .and. type_info%base_type /= TARRAY) then
            type_str = type_str // ", dimension(:)"
        end if
    end function get_type_string
    
    ! ===== HELPER FUNCTIONS =====

    ! Convert integer to string
    function int_to_str(int_val) result(str)
        integer, intent(in) :: int_val
        character(len=32) :: str
        
        write(str, '(I0)') int_val
        str = trim(adjustl(str))
    end function int_to_str

    ! Build comprehensive declaration attributes list
    subroutine build_declaration_attributes(node, attributes)
        type(declaration_node), intent(in) :: node
        character(len=:), allocatable, intent(out) :: attributes(:)
        character(len=:), allocatable :: temp_attrs(:)
        integer :: attr_count, i
        
        ! Count attributes
        attr_count = 0
        if (node%has_intent .and. allocated(node%intent)) attr_count = attr_count + 1
        if (node%is_array) attr_count = attr_count + 1
        if (node%is_allocatable) attr_count = attr_count + 1
        if (node%is_pointer) attr_count = attr_count + 1
        if (node%has_initializer) attr_count = attr_count + 1
        
        ! Allocate result array
        if (attr_count > 0) then
            allocate(character(len=64) :: attributes(attr_count))
        else
            allocate(character(len=1) :: attributes(0))
            return
        end if
        
        ! Build attributes list
        i = 0
        if (node%has_intent .and. allocated(node%intent)) then
            i = i + 1
            attributes(i) = "intent(" // node%intent // ")"
        end if
        if (node%is_array) then
            i = i + 1
            if (allocated(node%dimension_indices)) then
                attributes(i) = "dimension(" // trim(int_to_str(size(node%dimension_indices))) // ")"
            else
                attributes(i) = "dimension(:)"
            end if
        end if
        if (node%is_allocatable) then
            i = i + 1
            attributes(i) = "allocatable"
        end if
        if (node%is_pointer) then
            i = i + 1
            attributes(i) = "pointer"
        end if
        if (node%has_initializer) then
            i = i + 1
            attributes(i) = "initialized"
        end if
    end subroutine build_declaration_attributes

    ! ========================================================================
    ! Expression Temporary Tracking Functions (Issue #26)
    ! ========================================================================
    
    ! Get list of temporary IDs for an expression node
    function get_expression_temporaries(ctx, expr_node_index) result(temp_ids)
        type(semantic_context_t), intent(in) :: ctx
        integer, intent(in) :: expr_node_index
        integer, allocatable :: temp_ids(:)
        
        if (ctx%temp_tracker%active_count >= 0) then
            temp_ids = ctx%temp_tracker%get_temps_for_expr(expr_node_index)
        else
            allocate(temp_ids(0))
        end if
    end function get_expression_temporaries
    
    ! Get information about a specific temporary
    function get_temporary_info(ctx, temp_id) result(temp_info)
        type(semantic_context_t), intent(in) :: ctx
        integer, intent(in) :: temp_id
        type(expression_temp_info_t) :: temp_info
        type(temp_info_t) :: internal_info
        
        ! Get internal temporary info
        internal_info = ctx%temp_tracker%get_temp_info(temp_id)
        
        ! Convert to public type
        temp_info%temp_id = internal_info%temp_id
        if (allocated(internal_info%type_info)) then
            temp_info%type_name = internal_info%type_info
        end if
        temp_info%size_bytes = internal_info%size_in_bytes
        temp_info%created_at_node = internal_info%created_at_node
        temp_info%released_at_node = internal_info%released_at_node
        temp_info%is_active = internal_info%is_active
        temp_info%is_reusable = internal_info%is_reusable
    end function get_temporary_info
    
    ! Get count of currently active temporaries
    function get_active_temporary_count(ctx) result(count)
        type(semantic_context_t), intent(in) :: ctx
        integer :: count
        
        count = ctx%temp_tracker%get_active_count()
    end function get_active_temporary_count
    
    ! Get total count of temporaries allocated
    function get_total_temporary_count(ctx) result(count)
        type(semantic_context_t), intent(in) :: ctx
        integer :: count
        
        count = ctx%temp_tracker%get_total_count()
    end function get_total_temporary_count

    ! ========================================================================
    ! Call Graph Analysis Functions (Issue #18)
    ! ========================================================================
    
    ! Build call graph from AST
    function build_call_graph_from_arena(arena, root_index) result(graph)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(call_graph_t) :: graph
        
        graph = build_call_graph(arena, root_index)
    end function build_call_graph_from_arena
    
    ! Get list of unused procedures
    function get_unused_procedures(graph) result(proc_names)
        type(call_graph_t), intent(in) :: graph
        character(len=:), allocatable :: proc_names(:)
        
        proc_names = find_unused_procedures(graph)
    end function get_unused_procedures
    
    ! Get procedures that call a given procedure
    function get_procedure_callers(graph, procedure_name) result(caller_names)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: procedure_name
        character(len=:), allocatable :: caller_names(:)
        
        caller_names = get_callers(graph, procedure_name)
    end function get_procedure_callers
    
    ! Get procedures called by a given procedure
    function get_procedure_callees(graph, procedure_name) result(callee_names)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: procedure_name
        character(len=:), allocatable :: callee_names(:)
        
        callee_names = get_callees(graph, procedure_name)
    end function get_procedure_callees
    
    ! Check if a procedure is used (called by any other procedure)
    function is_procedure_used(graph, procedure_name) result(is_used)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: procedure_name
        logical :: is_used
        
        is_used = cg_is_procedure_used(graph, procedure_name)
    end function is_procedure_used
    
    ! Get all procedures in the call graph
    function get_all_procedures_in_graph(graph) result(proc_names)
        type(call_graph_t), intent(in) :: graph
        character(len=:), allocatable :: proc_names(:)
        
        proc_names = get_all_procedures(graph)
    end function get_all_procedures_in_graph
    
    ! Get all call edges in the graph
    function get_call_edges(graph) result(edges)
        type(call_graph_t), intent(in) :: graph
        type(call_edge_t), allocatable :: edges(:)
        
        if (allocated(graph%calls)) then
            allocate(edges(graph%call_count))
            edges = graph%calls(1:graph%call_count)
        else
            allocate(edges(0))
        end if
    end function get_call_edges
    
    ! Get recursive cycles in the call graph
    function get_recursive_cycles(graph) result(cycles)
        type(call_graph_t), intent(in) :: graph
        character(len=:), allocatable :: cycles(:)
        
        cycles = find_recursive_cycles(graph)
    end function get_recursive_cycles

    ! ========================================================================
    ! Control Flow Graph Functions (Issue #17)
    ! ========================================================================
    
    ! Build control flow graph from AST
    function build_cfg_from_arena(arena, root_index) result(cfg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(control_flow_graph_t) :: cfg
        
        cfg = build_control_flow_graph(arena, root_index)
    end function build_cfg_from_arena
    
    ! Get unreachable code blocks
    function get_unreachable_code_from_cfg(cfg) result(block_ids)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: block_ids(:)
        
        block_ids = find_unreachable_code(cfg)
    end function get_unreachable_code_from_cfg
    
    ! Get CFG entry block
    function get_cfg_entry_block(cfg) result(block_id)
        type(control_flow_graph_t), intent(in) :: cfg
        integer :: block_id
        
        block_id = get_entry_block(cfg)
    end function get_cfg_entry_block
    
    ! Get CFG exit blocks
    function get_cfg_exit_blocks(cfg) result(block_ids)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: block_ids(:)
        
        block_ids = get_exit_blocks(cfg)
    end function get_cfg_exit_blocks
    
    ! Get all blocks in CFG
    function get_cfg_all_blocks(cfg) result(block_ids)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: block_ids(:)
        
        block_ids = get_all_blocks(cfg)
    end function get_cfg_all_blocks
    
    ! Get predecessors of a block
    function get_cfg_block_predecessors(cfg, block_id) result(pred_ids)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, intent(in) :: block_id
        integer, allocatable :: pred_ids(:)
        
        pred_ids = get_block_predecessors(cfg, block_id)
    end function get_cfg_block_predecessors
    
    ! Get successors of a block
    function get_cfg_block_successors(cfg, block_id) result(succ_ids)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, intent(in) :: block_id
        integer, allocatable :: succ_ids(:)
        
        succ_ids = get_block_successors(cfg, block_id)
    end function get_cfg_block_successors
    
    ! Check if a block is reachable
    function is_cfg_block_reachable(cfg, block_id) result(reachable)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, intent(in) :: block_id
        logical :: reachable
        
        reachable = is_block_reachable(cfg, block_id)
    end function is_cfg_block_reachable
    
    ! Get unreachable statements
    function get_cfg_unreachable_statements(cfg) result(stmt_indices)
        type(control_flow_graph_t), intent(in) :: cfg
        integer, allocatable :: stmt_indices(:)
        
        stmt_indices = get_unreachable_statements(cfg)
    end function get_cfg_unreachable_statements
    
    ! Print control flow graph
    subroutine print_control_flow_graph(cfg)
        type(control_flow_graph_t), intent(in) :: cfg
        
        call print_cfg(cfg)
    end subroutine print_control_flow_graph
    
    ! Export CFG to DOT format
    function export_cfg_to_dot(cfg) result(dot_string)
        type(control_flow_graph_t), intent(in) :: cfg
        character(len=:), allocatable :: dot_string
        
        dot_string = cfg_to_dot(cfg)
    end function export_cfg_to_dot

    ! ========================================================================
    ! Procedure and Parameter Analysis Functions (Issue #84)
    ! ========================================================================
    
    ! Count the number of parameters in a procedure
    function count_procedure_parameters(arena, node_index) result(param_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: param_count
        
        integer, allocatable :: param_indices(:)
        
        param_count = 0
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        param_indices = get_procedure_params(arena%entries(node_index)%node)
        param_count = size(param_indices)
    end function count_procedure_parameters
    
    ! Get the intent of a specific parameter by index (1-based)
    function get_parameter_intent(arena, node_index, param_index) result(intent_str)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, param_index
        character(len=:), allocatable :: intent_str
        
        integer, allocatable :: param_indices(:)
        integer :: param_node_index
        
        intent_str = ""
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        param_indices = get_procedure_params(arena%entries(node_index)%node)
        if (param_index < 1 .or. param_index > size(param_indices)) return
        
        param_node_index = param_indices(param_index)
        if (param_node_index <= 0 .or. param_node_index > arena%size) return
        if (.not. allocated(arena%entries(param_node_index)%node)) return
        
        select type (param_node => arena%entries(param_node_index)%node)
        type is (parameter_declaration_node)
            intent_str = intent_type_to_string(param_node%intent_type)
        class default
            intent_str = ""
        end select
    end function get_parameter_intent
    
    ! Get the type of a specific parameter by index (1-based)
    function get_parameter_type(arena, node_index, param_index) result(type_str)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, param_index
        character(len=:), allocatable :: type_str
        
        integer, allocatable :: param_indices(:)
        integer :: param_node_index
        
        type_str = ""
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        param_indices = get_procedure_params(arena%entries(node_index)%node)
        if (param_index < 1 .or. param_index > size(param_indices)) return
        
        param_node_index = param_indices(param_index)
        if (param_node_index <= 0 .or. param_node_index > arena%size) return
        if (.not. allocated(arena%entries(param_node_index)%node)) return
        
        select type (param_node => arena%entries(param_node_index)%node)
        type is (parameter_declaration_node)
            if (allocated(param_node%type_name)) then
                type_str = param_node%type_name
            else
                type_str = "unknown"
            end if
        class default
            type_str = "unknown"
        end select
    end function get_parameter_type
    
    ! Check if a specific parameter is optional by index (1-based)
    function is_parameter_optional(arena, node_index, param_index) result(is_optional)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, param_index
        logical :: is_optional
        
        integer, allocatable :: param_indices(:)
        integer :: param_node_index
        
        is_optional = .false.
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        param_indices = get_procedure_params(arena%entries(node_index)%node)
        if (param_index < 1 .or. param_index > size(param_indices)) return
        
        param_node_index = param_indices(param_index)
        if (param_node_index <= 0 .or. param_node_index > arena%size) return
        if (.not. allocated(arena%entries(param_node_index)%node)) return
        
        select type (param_node => arena%entries(param_node_index)%node)
        type is (parameter_declaration_node)
            is_optional = param_node%is_optional
        end select
    end function is_parameter_optional
    
    ! Get a human-readable signature string for a procedure
    function get_procedure_signature(arena, node_index) result(signature)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: signature
        
        character(len=:), allocatable :: proc_name, return_type
        integer, allocatable :: param_indices(:)
        integer :: i
        character(len=256) :: temp_sig
        character(len=:), allocatable :: param_type, param_intent
        
        signature = ""
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Get procedure name
        proc_name = get_procedure_name(arena%entries(node_index)%node)
        if (len(proc_name) == 0) return
        
        ! Check if it's a function with return type
        if (procedure_has_return_type(arena%entries(node_index)%node)) then
            return_type = get_procedure_return_type(arena%entries(node_index)%node)
            write(temp_sig, '(A,A,A)') "function ", trim(proc_name), "("
        else
            write(temp_sig, '(A,A,A)') "subroutine ", trim(proc_name), "("
        end if
        
        ! Add parameters
        param_indices = get_procedure_params(arena%entries(node_index)%node)
        do i = 1, size(param_indices)
            if (i > 1) then
                temp_sig = trim(temp_sig) // ", "
            end if
            param_type = get_parameter_type(arena, node_index, i)
            param_intent = get_parameter_intent(arena, node_index, i)
            
            if (len(param_intent) > 0) then
                temp_sig = trim(temp_sig) // trim(param_intent) // " " // trim(param_type)
            else
                temp_sig = trim(temp_sig) // trim(param_type)
            end if
            
            if (is_parameter_optional(arena, node_index, i)) then
                temp_sig = trim(temp_sig) // ", optional"
            end if
        end do
        
        temp_sig = trim(temp_sig) // ")"
        
        ! Add return type for functions
        if (procedure_has_return_type(arena%entries(node_index)%node)) then
            temp_sig = trim(temp_sig) // " result(" // trim(return_type) // ")"
        end if
        
        signature = trim(temp_sig)
    end function get_procedure_signature
    
    ! Get all references to a procedure by name throughout the AST
    function get_procedure_references(arena, proc_name) result(reference_indices)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: proc_name
        integer, allocatable :: reference_indices(:)
        
        integer, parameter :: MAX_REFS = 1000
        integer :: temp_refs(MAX_REFS)
        integer :: ref_count, i
        
        ref_count = 0
        
        ! Search through all nodes for procedure calls and references
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            
            select type (node => arena%entries(i)%node)
            type is (subroutine_call_node)
                if (allocated(node%name) .and. node%name == proc_name) then
                    ref_count = ref_count + 1
                    if (ref_count <= MAX_REFS) temp_refs(ref_count) = i
                end if
            type is (call_or_subscript_node)
                if (allocated(node%name) .and. node%name == proc_name) then
                    ref_count = ref_count + 1
                    if (ref_count <= MAX_REFS) temp_refs(ref_count) = i
                end if
            end select
        end do
        
        ! Copy results to properly sized array
        allocate(reference_indices(ref_count))
        if (ref_count > 0) then
            reference_indices(1:ref_count) = temp_refs(1:ref_count)
        end if
    end function get_procedure_references
    
    ! Check if a procedure is used in a generic interface
    function is_procedure_used_in_generic(arena, proc_name) result(is_used)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: proc_name
        logical :: is_used
        
        integer :: i
        
        is_used = .false.
        
        ! Search through interface blocks for generic procedure usage
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            
            select type (node => arena%entries(i)%node)
            type is (interface_block_node)
                if (allocated(node%procedure_indices)) then
                    block
                        integer :: j, proc_idx
                        do j = 1, size(node%procedure_indices)
                            proc_idx = node%procedure_indices(j)
                            if (proc_idx > 0 .and. proc_idx <= arena%size) then
                                if (allocated(arena%entries(proc_idx)%node)) then
                                    select type (proc_node => arena%entries(proc_idx)%node)
                                    type is (function_def_node)
                                        if (allocated(proc_node%name) .and. proc_node%name == proc_name) then
                                            is_used = .true.
                                            return
                                        end if
                                    type is (subroutine_def_node)
                                        if (allocated(proc_node%name) .and. proc_node%name == proc_name) then
                                            is_used = .true.
                                            return
                                        end if
                                    end select
                                end if
                            end if
                        end do
                    end block
                end if
            end select
        end do
    end function is_procedure_used_in_generic
    
    ! Get usage context information for a parameter
    function get_parameter_usage_context(arena, param_name) result(context_info)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: param_name
        character(len=:), allocatable :: context_info
        
        integer :: i, usage_count, definition_count
        character(len=256) :: temp_info
        
        usage_count = 0
        definition_count = 0
        
        ! Count parameter usages and definitions
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            
            select type (node => arena%entries(i)%node)
            type is (identifier_node)
                if (allocated(node%name) .and. node%name == param_name) then
                    usage_count = usage_count + 1
                end if
            type is (assignment_node)
                ! Check if parameter is being assigned to (definition)
                if (node%target_index > 0 .and. node%target_index <= arena%size) then
                    if (allocated(arena%entries(node%target_index)%node)) then
                        select type (target => arena%entries(node%target_index)%node)
                        type is (identifier_node)
                            if (allocated(target%name) .and. target%name == param_name) then
                                definition_count = definition_count + 1
                            end if
                        end select
                    end if
                end if
            end select
        end do
        
        write(temp_info, '(A,I0,A,I0,A)') "Parameter '", usage_count, "' usages, '", definition_count, "' definitions"
        context_info = trim(temp_info)
    end function get_parameter_usage_context

end module fortfront