module ast_introspection
    ! AST Node Introspection APIs for Static Analysis
    ! Provides comprehensive API for examining AST nodes and their properties
    
    use ast_core
    use ast_nodes_core
    use ast_nodes_procedure
    use ast_nodes_control
    use ast_nodes_data
    use ast_nodes_io
    use ast_nodes_misc
    use type_system_hm, only: mono_type_t
    implicit none
    private

    ! Public API functions from issue #12
    public :: get_node
    public :: get_node_type_id
    public :: get_node_source_location
    public :: has_semantic_info
    public :: get_node_type_info_from_arena  ! Legacy (returns unallocated)
    
    ! New safe read-only type access functions
    public :: get_node_type_kind
    public :: get_node_type_details
    
    ! Additional safe introspection functions that work with indices
    public :: get_node_type_id_from_arena
    public :: get_node_source_location_from_arena

contains

    ! Get node by index from arena (issue #12 requirement)
    ! 
    ! IMPORTANT: Due to Fortran limitations with polymorphic deep copying and
    ! the inability to return pointers to allocatable components, this function
    ! creates a shallow copy of the node WITHOUT the inferred_type field to
    ! avoid segmentation faults. 
    !
    ! For full type information access, use get_node_type_kind() and 
    ! get_node_type_details() which provide safe read-only access.
    function get_node(arena, index) result(node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        class(ast_node), allocatable :: node
        
        ! Bounds checking
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (size(arena%entries) < index) return
        if (.not. allocated(arena%entries(index)%node)) return
        
        ! Create a safe copy using select type to avoid deep copy issues
        select type (src_node => arena%entries(index)%node)
        type is (identifier_node)
            allocate(identifier_node :: node)
            select type (node)
            type is (identifier_node)
                node%name = src_node%name
                node%line = src_node%line
                node%column = src_node%column
                ! Don't copy inferred_type to avoid segfault
            end select
        type is (literal_node)
            allocate(literal_node :: node)
            select type (node)
            type is (literal_node)
                node%value = src_node%value
                node%literal_kind = src_node%literal_kind
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (assignment_node)
            allocate(assignment_node :: node)
            select type (node)
            type is (assignment_node)
                node%target_index = src_node%target_index
                node%value_index = src_node%value_index
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (program_node)
            allocate(program_node :: node)
            select type (node)
            type is (program_node)
                node%name = src_node%name
                if (allocated(src_node%body_indices)) then
                    allocate(node%body_indices(size(src_node%body_indices)))
                    node%body_indices = src_node%body_indices
                end if
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (binary_op_node)
            allocate(binary_op_node :: node)
            select type (node)
            type is (binary_op_node)
                node%left_index = src_node%left_index
                node%right_index = src_node%right_index
                node%operator = src_node%operator
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (declaration_node)
            allocate(declaration_node :: node)
            select type (node)
            type is (declaration_node)
                node%type_name = src_node%type_name
                node%var_name = src_node%var_name
                if (allocated(src_node%var_names)) then
                    allocate(node%var_names, source=src_node%var_names)
                end if
                node%is_multi_declaration = src_node%is_multi_declaration
                node%kind_value = src_node%kind_value
                node%has_kind = src_node%has_kind
                node%intent = src_node%intent
                node%has_intent = src_node%has_intent
                node%initializer_index = src_node%initializer_index
                node%has_initializer = src_node%has_initializer
                node%is_array = src_node%is_array
                if (allocated(src_node%dimension_indices)) then
                    allocate(node%dimension_indices(size(src_node%dimension_indices)))
                    node%dimension_indices = src_node%dimension_indices
                end if
                node%is_allocatable = src_node%is_allocatable
                node%is_pointer = src_node%is_pointer
                node%is_target = src_node%is_target
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (if_node)
            allocate(if_node :: node)
            select type (node)
            type is (if_node)
                node%condition_index = src_node%condition_index
                if (allocated(src_node%then_body_indices)) then
                    allocate(node%then_body_indices(size(src_node%then_body_indices)))
                    node%then_body_indices = src_node%then_body_indices
                end if
                if (allocated(src_node%else_body_indices)) then
                    allocate(node%else_body_indices(size(src_node%else_body_indices)))
                    node%else_body_indices = src_node%else_body_indices
                end if
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (function_def_node)
            allocate(function_def_node :: node)
            select type (node)
            type is (function_def_node)
                node%name = src_node%name
                node%return_type = src_node%return_type
                if (allocated(src_node%param_indices)) then
                    allocate(node%param_indices(size(src_node%param_indices)))
                    node%param_indices = src_node%param_indices
                end if
                if (allocated(src_node%body_indices)) then
                    allocate(node%body_indices(size(src_node%body_indices)))
                    node%body_indices = src_node%body_indices
                end if
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (array_literal_node)
            allocate(array_literal_node :: node)
            select type (node)
            type is (array_literal_node)
                if (allocated(src_node%element_indices)) then
                    allocate(node%element_indices(size(src_node%element_indices)))
                    node%element_indices = src_node%element_indices
                end if
                node%element_type = src_node%element_type
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (call_or_subscript_node)
            allocate(call_or_subscript_node :: node)
            select type (node)
            type is (call_or_subscript_node)
                node%name = src_node%name
                if (allocated(src_node%arg_indices)) then
                    allocate(node%arg_indices(size(src_node%arg_indices)))
                    node%arg_indices = src_node%arg_indices
                end if
                node%is_intrinsic = src_node%is_intrinsic
                node%intrinsic_signature = src_node%intrinsic_signature
                node%is_array_access = src_node%is_array_access
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (component_access_node)
            allocate(component_access_node :: node)
            select type (node)
            type is (component_access_node)
                node%base_expr_index = src_node%base_expr_index
                node%component_name = src_node%component_name
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (range_subscript_node)
            allocate(range_subscript_node :: node)
            select type (node)
            type is (range_subscript_node)
                node%base_expr_index = src_node%base_expr_index
                node%start_index = src_node%start_index
                node%end_index = src_node%end_index
                node%is_character_substring = src_node%is_character_substring
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (pointer_assignment_node)
            allocate(pointer_assignment_node :: node)
            select type (node)
            type is (pointer_assignment_node)
                node%pointer_index = src_node%pointer_index
                node%target_index = src_node%target_index
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (subroutine_def_node)
            allocate(subroutine_def_node :: node)
            select type (node)
            type is (subroutine_def_node)
                node%name = src_node%name
                if (allocated(src_node%param_indices)) then
                    allocate(node%param_indices(size(src_node%param_indices)))
                    node%param_indices = src_node%param_indices
                end if
                if (allocated(src_node%body_indices)) then
                    allocate(node%body_indices(size(src_node%body_indices)))
                    node%body_indices = src_node%body_indices
                end if
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (subroutine_call_node)
            allocate(subroutine_call_node :: node)
            select type (node)
            type is (subroutine_call_node)
                node%name = src_node%name
                if (allocated(src_node%arg_indices)) then
                    allocate(node%arg_indices(size(src_node%arg_indices)))
                    node%arg_indices = src_node%arg_indices
                end if
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (parameter_declaration_node)
            allocate(parameter_declaration_node :: node)
            select type (node)
            type is (parameter_declaration_node)
                node%name = src_node%name
                node%type_name = src_node%type_name
                node%kind_value = src_node%kind_value
                node%has_kind = src_node%has_kind
                node%intent = src_node%intent
                node%has_intent = src_node%has_intent
                node%is_array = src_node%is_array
                if (allocated(src_node%dimension_indices)) then
                    allocate(node%dimension_indices(size(src_node%dimension_indices)))
                    node%dimension_indices = src_node%dimension_indices
                end if
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (do_loop_node)
            allocate(do_loop_node :: node)
            select type (node)
            type is (do_loop_node)
                node%var_name = src_node%var_name
                node%label = src_node%label
                node%start_expr_index = src_node%start_expr_index
                node%end_expr_index = src_node%end_expr_index
                node%step_expr_index = src_node%step_expr_index
                if (allocated(src_node%body_indices)) then
                    allocate(node%body_indices(size(src_node%body_indices)))
                    node%body_indices = src_node%body_indices
                end if
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (do_while_node)
            allocate(do_while_node :: node)
            select type (node)
            type is (do_while_node)
                node%condition_index = src_node%condition_index
                if (allocated(src_node%body_indices)) then
                    allocate(node%body_indices(size(src_node%body_indices)))
                    node%body_indices = src_node%body_indices
                end if
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (select_case_node)
            allocate(select_case_node :: node)
            select type (node)
            type is (select_case_node)
                node%selector_index = src_node%selector_index
                if (allocated(src_node%case_indices)) then
                    allocate(node%case_indices(size(src_node%case_indices)))
                    node%case_indices = src_node%case_indices
                end if
                node%default_index = src_node%default_index
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (case_block_node)
            allocate(case_block_node :: node)
            select type (node)
            type is (case_block_node)
                if (allocated(src_node%value_indices)) then
                    allocate(node%value_indices(size(src_node%value_indices)))
                    node%value_indices = src_node%value_indices
                end if
                if (allocated(src_node%body_indices)) then
                    allocate(node%body_indices(size(src_node%body_indices)))
                    node%body_indices = src_node%body_indices
                end if
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (module_node)
            allocate(module_node :: node)
            select type (node)
            type is (module_node)
                node%name = src_node%name
                if (allocated(src_node%declaration_indices)) then
                    allocate(node%declaration_indices(size(src_node%declaration_indices)))
                    node%declaration_indices = src_node%declaration_indices
                end if
                if (allocated(src_node%procedure_indices)) then
                    allocate(node%procedure_indices(size(src_node%procedure_indices)))
                    node%procedure_indices = src_node%procedure_indices
                end if
                node%has_contains = src_node%has_contains
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (use_statement_node)
            allocate(use_statement_node :: node)
            select type (node)
            type is (use_statement_node)
                node%module_name = src_node%module_name
                if (allocated(src_node%only_list)) then
                    allocate(node%only_list, source=src_node%only_list)
                end if
                if (allocated(src_node%rename_list)) then
                    allocate(node%rename_list, source=src_node%rename_list)
                end if
                node%has_only = src_node%has_only
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (print_statement_node)
            allocate(print_statement_node :: node)
            select type (node)
            type is (print_statement_node)
                if (allocated(src_node%expression_indices)) then
                    allocate(node%expression_indices(size(src_node%expression_indices)))
                    node%expression_indices = src_node%expression_indices
                end if
                node%format_spec = src_node%format_spec
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (write_statement_node)
            allocate(write_statement_node :: node)
            select type (node)
            type is (write_statement_node)
                node%unit_spec = src_node%unit_spec
                node%format_spec = src_node%format_spec
                if (allocated(src_node%arg_indices)) then
                    allocate(node%arg_indices(size(src_node%arg_indices)))
                    node%arg_indices = src_node%arg_indices
                end if
                node%iostat_var_index = src_node%iostat_var_index
                node%err_label_index = src_node%err_label_index
                node%end_label_index = src_node%end_label_index
                node%format_expr_index = src_node%format_expr_index
                node%is_formatted = src_node%is_formatted
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (read_statement_node)
            allocate(read_statement_node :: node)
            select type (node)
            type is (read_statement_node)
                node%unit_spec = src_node%unit_spec
                node%format_spec = src_node%format_spec
                if (allocated(src_node%var_indices)) then
                    allocate(node%var_indices(size(src_node%var_indices)))
                    node%var_indices = src_node%var_indices
                end if
                node%iostat_var_index = src_node%iostat_var_index
                node%err_label_index = src_node%err_label_index
                node%end_label_index = src_node%end_label_index
                node%format_expr_index = src_node%format_expr_index
                node%is_formatted = src_node%is_formatted
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (allocate_statement_node)
            allocate(allocate_statement_node :: node)
            select type (node)
            type is (allocate_statement_node)
                if (allocated(src_node%var_indices)) then
                    allocate(node%var_indices(size(src_node%var_indices)))
                    node%var_indices = src_node%var_indices
                end if
                if (allocated(src_node%shape_indices)) then
                    allocate(node%shape_indices(size(src_node%shape_indices)))
                    node%shape_indices = src_node%shape_indices
                end if
                node%stat_var_index = src_node%stat_var_index
                node%errmsg_var_index = src_node%errmsg_var_index
                node%source_expr_index = src_node%source_expr_index
                node%mold_expr_index = src_node%mold_expr_index
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (deallocate_statement_node)
            allocate(deallocate_statement_node :: node)
            select type (node)
            type is (deallocate_statement_node)
                if (allocated(src_node%var_indices)) then
                    allocate(node%var_indices(size(src_node%var_indices)))
                    node%var_indices = src_node%var_indices
                end if
                node%stat_var_index = src_node%stat_var_index
                node%errmsg_var_index = src_node%errmsg_var_index
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (stop_node)
            allocate(stop_node :: node)
            select type (node)
            type is (stop_node)
                node%stop_code_index = src_node%stop_code_index
                node%stop_message = src_node%stop_message
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (return_node)
            allocate(return_node :: node)
            select type (node)
            type is (return_node)
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (cycle_node)
            allocate(cycle_node :: node)
            select type (node)
            type is (cycle_node)
                node%label = src_node%label
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (exit_node)
            allocate(exit_node :: node)
            select type (node)
            type is (exit_node)
                node%label = src_node%label
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (where_node)
            allocate(where_node :: node)
            select type (node)
            type is (where_node)
                node%mask_expr_index = src_node%mask_expr_index
                if (allocated(src_node%where_body_indices)) then
                    allocate(node%where_body_indices(size(src_node%where_body_indices)))
                    node%where_body_indices = src_node%where_body_indices
                end if
                if (allocated(src_node%elsewhere_clauses)) then
                    allocate(node%elsewhere_clauses(size(src_node%elsewhere_clauses)))
                    node%elsewhere_clauses = src_node%elsewhere_clauses
                end if
                node%mask_is_simple = src_node%mask_is_simple
                node%can_vectorize = src_node%can_vectorize
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (interface_block_node)
            allocate(interface_block_node :: node)
            select type (node)
            type is (interface_block_node)
                node%name = src_node%name
                node%kind = src_node%kind
                node%operator = src_node%operator
                if (allocated(src_node%procedure_indices)) then
                    allocate(node%procedure_indices(size(src_node%procedure_indices)))
                    node%procedure_indices = src_node%procedure_indices
                end if
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (derived_type_node)
            allocate(derived_type_node :: node)
            select type (node)
            type is (derived_type_node)
                node%name = src_node%name
                if (allocated(src_node%component_indices)) then
                    allocate(node%component_indices(size(src_node%component_indices)))
                    node%component_indices = src_node%component_indices
                end if
                node%has_parameters = src_node%has_parameters
                if (allocated(src_node%param_indices)) then
                    allocate(node%param_indices(size(src_node%param_indices)))
                    node%param_indices = src_node%param_indices
                end if
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (forall_node)
            allocate(forall_node :: node)
            select type (node)
            type is (forall_node)
                node%num_indices = src_node%num_indices
                if (allocated(src_node%index_names)) then
                    allocate(node%index_names, source=src_node%index_names)
                end if
                if (allocated(src_node%lower_bound_indices)) then
                    allocate(node%lower_bound_indices(size(src_node%lower_bound_indices)))
                    node%lower_bound_indices = src_node%lower_bound_indices
                end if
                if (allocated(src_node%upper_bound_indices)) then
                    allocate(node%upper_bound_indices(size(src_node%upper_bound_indices)))
                    node%upper_bound_indices = src_node%upper_bound_indices
                end if
                if (allocated(src_node%stride_indices)) then
                    allocate(node%stride_indices(size(src_node%stride_indices)))
                    node%stride_indices = src_node%stride_indices
                end if
                node%has_mask = src_node%has_mask
                node%mask_expr_index = src_node%mask_expr_index
                if (allocated(src_node%body_indices)) then
                    allocate(node%body_indices(size(src_node%body_indices)))
                    node%body_indices = src_node%body_indices
                end if
                node%has_dependencies = src_node%has_dependencies
                node%is_parallel_safe = src_node%is_parallel_safe
                if (allocated(src_node%dependency_pairs)) then
                    allocate(node%dependency_pairs(size(src_node%dependency_pairs,1), &
                                                   size(src_node%dependency_pairs,2)))
                    node%dependency_pairs = src_node%dependency_pairs
                end if
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (case_range_node)
            allocate(case_range_node :: node)
            select type (node)
            type is (case_range_node)
                node%start_value = src_node%start_value
                node%end_value = src_node%end_value
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (case_default_node)
            allocate(case_default_node :: node)
            select type (node)
            type is (case_default_node)
                if (allocated(src_node%body_indices)) then
                    allocate(node%body_indices(size(src_node%body_indices)))
                    node%body_indices = src_node%body_indices
                end if
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (complex_literal_node)
            allocate(complex_literal_node :: node)
            select type (node)
            type is (complex_literal_node)
                node%real_index = src_node%real_index
                node%imag_index = src_node%imag_index
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (include_statement_node)
            allocate(include_statement_node :: node)
            select type (node)
            type is (include_statement_node)
                node%filename = src_node%filename
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (contains_node)
            allocate(contains_node :: node)
            select type (node)
            type is (contains_node)
                node%line = src_node%line
                node%column = src_node%column
            end select
        type is (format_descriptor_node)
            allocate(format_descriptor_node :: node)
            select type (node)
            type is (format_descriptor_node)
                node%descriptor_type = src_node%descriptor_type
                node%width = src_node%width
                node%decimal_places = src_node%decimal_places
                node%exponent_width = src_node%exponent_width
                node%repeat_count = src_node%repeat_count
                node%is_literal = src_node%is_literal
                node%literal_text = src_node%literal_text
                node%line = src_node%line
                node%column = src_node%column
            end select
        class default
            ! For unsupported types, return unallocated
            return
        end select
    end function get_node

    ! Get node type identifier (issue #12 requirement)
    function get_node_type_id(node) result(type_id)
        class(ast_node), intent(in) :: node
        integer :: type_id

        ! Use select type to determine node type and return standard constants
        select type (node)
        type is (program_node)
            type_id = 1
        type is (function_def_node)
            type_id = 2
        type is (assignment_node)
            type_id = 3
        type is (binary_op_node)
            type_id = 4
        type is (identifier_node)
            type_id = 5
        type is (literal_node)
            type_id = 6
        type is (array_literal_node)
            type_id = 7
        type is (call_or_subscript_node)
            type_id = 8
        type is (subroutine_def_node)
            type_id = 9
        type is (subroutine_call_node)
            type_id = 10
        type is (declaration_node)
            type_id = 11
        type is (parameter_declaration_node)
            type_id = 12
        type is (if_node)
            type_id = 13
        type is (do_loop_node)
            type_id = 14
        type is (do_while_node)
            type_id = 15
        type is (select_case_node)
            type_id = 16
        type is (case_block_node)
            type_id = 17
        type is (module_node)
            type_id = 18
        type is (use_statement_node)
            type_id = 19
        type is (print_statement_node)
            type_id = 20
        type is (write_statement_node)
            type_id = 21
        type is (read_statement_node)
            type_id = 22
        type is (allocate_statement_node)
            type_id = 23
        type is (deallocate_statement_node)
            type_id = 24
        type is (stop_node)
            type_id = 25
        type is (return_node)
            type_id = 26
        type is (cycle_node)
            type_id = 27
        type is (exit_node)
            type_id = 28
        type is (where_node)
            type_id = 29
        type is (interface_block_node)
            type_id = 30
        type is (derived_type_node)
            type_id = 31
        type is (pointer_assignment_node)
            type_id = 32
        type is (forall_node)
            type_id = 33
        type is (case_range_node)
            type_id = 34
        type is (case_default_node)
            type_id = 35
        type is (complex_literal_node)
            type_id = 36
        type is (include_statement_node)
            type_id = 37
        type is (contains_node)
            type_id = 38
        type is (format_descriptor_node)
            type_id = 39
        class default
            type_id = 99
        end select
    end function get_node_type_id

    ! Get source location for any node (issue #12 requirement)
    ! Note: Returns a simple type that matches the existing source_location_t in fortfront.f90
    subroutine get_node_source_location(node, line, column)
        class(ast_node), intent(in) :: node
        integer, intent(out) :: line, column

        ! Extract line and column from node's line and column fields
        ! All AST nodes have line and column fields
        line = node%line
        column = node%column
    end subroutine get_node_source_location

    ! Check if a node has semantic information attached (issue #12 requirement)
    function has_semantic_info(node) result(has_info)
        class(ast_node), intent(in) :: node
        logical :: has_info

        ! Check if the node has inferred type information
        has_info = allocated(node%inferred_type)
    end function has_semantic_info

    ! Get basic type kind from node (safe read-only access)
    function get_node_type_kind(arena, index) result(type_kind)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        integer :: type_kind

        type_kind = 0  ! Unknown/no type
        
        ! Bounds checking
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (size(arena%entries) < index) return
        if (.not. allocated(arena%entries(index)%node)) return
        if (.not. allocated(arena%entries(index)%node%inferred_type)) return
        
        ! Safe read-only access to basic type kind
        type_kind = arena%entries(index)%node%inferred_type%kind
    end function get_node_type_kind

    ! Get type details without dangerous deep copy (safe read-only access)
    subroutine get_node_type_details(arena, index, kind, type_size, is_allocatable, &
                                   is_pointer, found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        integer, intent(out) :: kind, type_size
        logical, intent(out) :: is_allocatable, is_pointer, found

        found = .false.
        kind = 0
        type_size = 0
        is_allocatable = .false.
        is_pointer = .false.
        
        ! Bounds checking
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (size(arena%entries) < index) return
        if (.not. allocated(arena%entries(index)%node)) return
        if (.not. allocated(arena%entries(index)%node%inferred_type)) return
        
        ! Safe read-only access to type details
        associate (node_type => arena%entries(index)%node%inferred_type)
            kind = node_type%kind
            type_size = node_type%size
            is_allocatable = node_type%alloc_info%is_allocatable
            is_pointer = node_type%alloc_info%is_pointer
            found = .true.
        end associate
    end subroutine get_node_type_details

    ! Legacy function - now returns unallocated for safety
    function get_node_type_info_from_arena(arena, index) result(type_info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(mono_type_t), allocatable :: type_info
        
        ! Always return unallocated to prevent segfaults
        ! Use get_node_type_kind() or get_node_type_details() instead
    end function get_node_type_info_from_arena

    ! Get node type ID directly from arena (safe, no copying)
    function get_node_type_id_from_arena(arena, index) result(type_id)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        integer :: type_id
        
        type_id = 99  ! Unknown by default
        
        ! Bounds checking
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (size(arena%entries) < index) return
        if (.not. allocated(arena%entries(index)%node)) return
        
        ! Use existing function with the node reference
        type_id = get_node_type_id(arena%entries(index)%node)
    end function get_node_type_id_from_arena

    ! Get source location directly from arena (safe, no copying)
    subroutine get_node_source_location_from_arena(arena, index, line, column)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        integer, intent(out) :: line, column
        
        line = 0
        column = 0
        
        ! Bounds checking
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (size(arena%entries) < index) return
        if (.not. allocated(arena%entries(index)%node)) return
        
        ! Direct access to node fields
        line = arena%entries(index)%node%line
        column = arena%entries(index)%node%column
    end subroutine get_node_source_location_from_arena

end module ast_introspection