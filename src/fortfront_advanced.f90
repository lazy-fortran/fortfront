module fortfront_advanced
    ! fortfront Advanced Functions - Complex AST accessor functions and analysis
    ! This module contains advanced functionality for detailed AST operations:
    ! - Type-safe accessor functions for all node types
    ! - Symbol table and scope analysis API
    ! - Expression temporary tracking
    ! - Call graph analysis  
    ! - Control flow graph functions
    ! - Procedure and parameter analysis
    ! - Complex semantic queries
    
    use ast_core, only: ast_arena_t, ast_node, program_node, assignment_node, &
                       binary_op_node, function_def_node, identifier_node, &
                       literal_node, array_literal_node, call_or_subscript_node, &
                       subroutine_def_node, subroutine_call_node, declaration_node, &
                       parameter_declaration_node, interface_block_node, &
                       LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_ARRAY, LITERAL_COMPLEX, &
                       get_procedure_name, get_procedure_params, get_procedure_body, &
                       procedure_has_return_type, get_procedure_return_type
    use ast_nodes_data, only: intent_type_to_string, INTENT_NONE, INTENT_IN, &
                             INTENT_OUT, INTENT_INOUT
    use semantic_analyzer, only: semantic_context_t
    use type_system_unified, only: mono_type_t, poly_type_t, TINT, TREAL, TCHAR, TLOGICAL, &
                              TFUN, TARRAY, TVAR, type_args_allocated, type_args_size, type_args_element
    use expression_temporary_tracker_module, only: temp_info_t
    use call_graph_module, only: call_graph_t, call_edge_t, &
                                get_all_procedures, find_unused_procedures, &
                                get_callers, get_callees, find_recursive_cycles, &
                                cg_is_procedure_used => is_procedure_used
    use call_graph_builder_module, only: build_call_graph
    use control_flow_graph_module, only: control_flow_graph_t, basic_block_t, cfg_edge_t, &
                                        create_control_flow_graph, add_basic_block, add_cfg_edge, &
                                        find_reachable_blocks, find_unreachable_code, &
                                        get_entry_block, get_exit_blocks, get_all_blocks, &
                                        get_block_predecessors, get_block_successors, &
                                        is_block_reachable, get_unreachable_statements, &
                                        print_cfg, cfg_to_dot
    use cfg_builder_module, only: build_control_flow_graph
    use fortfront_types, only: symbol_info_t, symbol_reference_t, scope_info_t, &
                              expression_temp_info_t, type_info_t, function_signature_t
    
    implicit none
    private
    
    ! Public advanced functions  
    public :: get_assignment_indices, get_binary_op_info, get_identifier_name, &
              get_literal_value, get_call_info, get_array_literal_info, &
              get_program_info, get_declaration_info, get_parameter_declaration_info, &
              get_declaration_details, get_parameter_declaration_details, &
              get_symbol_info, get_symbols_in_scope, get_symbol_references, &
              get_scope_info, get_all_scopes, &
              get_expression_temporaries, get_temporary_info, &
              get_active_temporary_count, get_total_temporary_count, &
              build_call_graph_from_arena, get_unused_procedures, &
              get_procedure_callers, get_procedure_callees, &
              is_procedure_used, get_all_procedures_in_graph, &
              get_call_edges, get_recursive_cycles, &
              build_cfg_from_arena, get_unreachable_code_from_cfg, &
              get_cfg_entry_block, get_cfg_exit_blocks, &
              get_cfg_all_blocks, get_cfg_block_predecessors, &
              get_cfg_block_successors, is_cfg_block_reachable, &
              get_cfg_unreachable_statements, print_control_flow_graph, &
              export_cfg_to_dot, count_procedure_parameters, &
              get_parameter_intent, get_parameter_type, &
              is_parameter_optional, get_procedure_signature, &
              get_procedure_references, is_procedure_used_in_generic, &
              get_parameter_usage_context, &
              lookup_symbol, get_scope_symbols, mono_type_to_type_info, &
              get_type_info_for_base_type, get_integer_literal_value, &
              get_real_literal_value, get_string_literal_value, &
              is_intrinsic_function, get_intrinsic_signature, get_type_string

contains

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

    ! ===== SYMBOL TABLE AND SCOPE API IMPLEMENTATION =====
    
    ! Symbol lookup - enhanced version
    function lookup_symbol(ctx, name, scope_node_index) result(symbol)
        type(semantic_context_t), intent(inout) :: ctx
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
            symbol%type_info = scheme%get_mono()
        end if
    end function lookup_symbol
    
    ! Get enhanced symbol information
    function get_symbol_info(ctx, name, scope_level) result(symbol)
        type(semantic_context_t), intent(inout) :: ctx
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
            symbol%type_info = scheme%get_mono()
            
            ! Check parameter tracker for additional attributes
            if (ctx%param_tracker%count > 0) then
                call check_parameter_attributes(ctx, name, symbol)
            end if
        end if
    end function get_symbol_info
    
    ! Get all symbols in a specific scope level
    function get_symbols_in_scope(ctx, scope_level) result(symbols)
        type(semantic_context_t), intent(inout) :: ctx
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
                symbols(i)%type_info = ctx%scopes%scopes(scope_level)%env%schemes(i)%get_mono()
                
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
            if (type_args_allocated(mono)) then
                if (type_args_size(mono) > 0) then
                    info = mono_type_to_type_info(type_args_element(mono, 1))  ! Element type
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

end module fortfront_advanced