module ast_core
    ! DEPRECATED: This module implements an anti-pattern of re-exporting everything
    ! ============================================================================
    ! 
    ! WARNING: This module creates hidden dependencies and tight coupling.
    ! Please migrate to explicit imports from specific AST modules:
    !
    ! Instead of: use ast_core
    ! Use specific imports like:
    !   use ast_nodes_core, only: assignment_node, identifier_node
    !   use ast_arena, only: ast_arena_t
    !   use ast_factory, only: create_assignment
    !
    ! This module will be deprecated in a future release.
    ! See Migration Guide in DOCS/AST_MIGRATION.md
    !
    ! AST Node Copying is now SAFE with cycle-protected memory management
    ! =====================================================================
    ! AST nodes can be safely copied using proper assignment operators that
    ! implement depth-limited copying for mono_type_t structures.
    
    use type_system_unified, only: mono_type_t
    use intrinsic_registry, only: get_intrinsic_info
    use uid_generator, only: generate_uid
    
    ! Re-export base types and interfaces
    use ast_base, only: ast_node, visit_interface, to_json_interface, string_t, &
                        ast_node_wrapper, &
                        LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, &
                        LITERAL_LOGICAL, LITERAL_ARRAY, LITERAL_COMPLEX
    
    ! Re-export arena functionality (modern generational arena)
    use ast_arena_modern, only: ast_arena_t, ast_entry_t, ast_arena_stats_t, create_ast_arena, destroy_ast_arena
    
    ! Re-export all node types
    use ast_nodes_core, only: program_node, assignment_node, &
                              pointer_assignment_node, identifier_node, literal_node, &
                              binary_op_node, call_or_subscript_node, &
                              array_literal_node, &
                              create_pointer_assignment, create_array_literal
    use ast_nodes_control, only: if_node, do_loop_node, do_while_node, forall_node, &
                                 elseif_wrapper, case_wrapper, &
                                 select_case_node, case_block_node, case_range_node, &
                                 case_default_node, &
                                 where_node, cycle_node, exit_node, &
                                 stop_node, return_node, goto_node, error_stop_node, &
                                 create_do_loop, create_do_while, create_if, &
                                 create_select_case
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
                                   subroutine_call_node, &
                                   create_function_def, create_subroutine_def, &
                                   is_procedure_node, get_procedure_name, get_procedure_params, &
                                   get_procedure_body, procedure_has_return_type, &
                                   get_procedure_return_type
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
                               module_node, derived_type_node, &
                               mixed_construct_container_node, &
                               create_declaration, create_derived_type, &
                               create_mixed_construct_container
    use ast_nodes_io, only: print_statement_node, write_statement_node, &
                             read_statement_node, format_descriptor_node, &
                             create_print_statement
    use ast_nodes_misc, only: complex_literal_node, allocate_statement_node, &
                              deallocate_statement_node, &
                              use_statement_node, include_statement_node, &
                              contains_node, interface_block_node, &
                              comment_node, blank_line_node, implicit_statement_node, &
                              end_statement_node, &
                              implicit_type_spec_t, implicit_letter_spec_t
    use ast_nodes_bounds, only: array_bounds_node, array_slice_node, &
                                range_expression_node, array_operation_node, &
                                get_array_bounds_node, get_array_slice_node, &
                                get_range_expression_node, get_array_operation_node, &
                                array_bounds_t, array_spec_t, &
                                NODE_ARRAY_BOUNDS, NODE_ARRAY_SLICE, &
                                NODE_RANGE_EXPRESSION, &
                                NODE_ARRAY_OPERATION
    
    implicit none
    
    ! Re-export everything as public
    public :: ast_node, visit_interface, to_json_interface, string_t, &
              ast_node_wrapper
    public :: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL, &
              LITERAL_ARRAY, LITERAL_COMPLEX
    public :: ast_arena_t, ast_entry_t, ast_arena_stats_t, create_ast_arena, destroy_ast_arena
    public :: program_node, assignment_node, pointer_assignment_node, &
              identifier_node, literal_node
    public :: binary_op_node, call_or_subscript_node, array_literal_node
    public :: if_node, do_loop_node, do_while_node, forall_node, &
              elseif_wrapper, case_wrapper
    public :: select_case_node, case_block_node, case_range_node, case_default_node
    public :: where_node, cycle_node, exit_node, stop_node, return_node, &
              goto_node, error_stop_node
    public :: function_def_node, subroutine_def_node, subroutine_call_node
    public :: declaration_node, parameter_declaration_node, module_node, &
              derived_type_node, mixed_construct_container_node
    public :: print_statement_node, write_statement_node, &
              read_statement_node, format_descriptor_node
    public :: complex_literal_node, allocate_statement_node, &
              deallocate_statement_node
    public :: use_statement_node, include_statement_node, contains_node, &
              interface_block_node, comment_node, blank_line_node, end_statement_node
    public :: array_bounds_node, array_slice_node, range_expression_node, &
              array_operation_node
    public :: get_array_bounds_node, get_array_slice_node, get_range_expression_node, &
              get_array_operation_node
    public :: array_bounds_t, array_spec_t
    public :: NODE_ARRAY_BOUNDS, NODE_ARRAY_SLICE, NODE_RANGE_EXPRESSION, &
              NODE_ARRAY_OPERATION
    ! Re-export factory functions
    public :: create_pointer_assignment, create_array_literal, &
              create_function_def, create_subroutine_def, &
              create_print_statement, create_declaration, create_do_loop, &
              create_do_while, create_if, &
              create_select_case, create_derived_type, &
              create_mixed_construct_container
    ! Procedure helper functions for consistent interface
    public :: is_procedure_node, get_procedure_name, get_procedure_params, &
              get_procedure_body, procedure_has_return_type, get_procedure_return_type
    ! Factory functions in this module
    public :: create_identifier, create_literal, create_binary_op, &
              create_call_or_subscript, &
              create_assignment, create_program, create_subroutine_call, &
              create_use_statement, create_implicit_statement, &
              create_include_statement, create_interface_block, create_module, &
              create_stop, &
              create_return, create_goto, create_error_stop, &
              create_cycle, create_exit, create_where, &
              create_comment, create_blank_line, create_array_bounds, create_array_slice, &
              create_range_expression, create_array_operation
    
contains

    ! Factory functions for backward compatibility
    
    function create_identifier(name, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: line, column
        type(identifier_node) :: node

        node%name = name
        node%uid = generate_uid()
        if (present(line)) then
            node%line = line
        end if
        if (present(column)) then
            node%column = column
        end if
    end function create_identifier

    function create_literal(value, kind, line, column) result(node)
        character(len=*), intent(in) :: value
        integer, intent(in) :: kind
        integer, intent(in), optional :: line, column
        type(literal_node) :: node

        node%value = value
        node%literal_kind = kind
        node%uid = generate_uid()
        if (present(line)) then
            node%line = line
        end if
        if (present(column)) then
            node%column = column
        end if
    end function create_literal

    function create_binary_op(left_index, right_index, operator, line, column) &
            result(node)
        integer, intent(in) :: left_index, right_index
        character(len=*), intent(in) :: operator
        integer, intent(in), optional :: line, column
        type(binary_op_node) :: node

        node%left_index = left_index
        node%right_index = right_index
        node%operator = operator
        node%uid = generate_uid()
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_binary_op

    function create_call_or_subscript(name, args, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: args(:)
        integer, intent(in), optional :: line, column
        type(call_or_subscript_node) :: node

        node%name = name
        node%uid = generate_uid()
        if (present(args)) then
            if (size(args) > 0) then
                node%arg_indices = args
            end if
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
        
        ! Check if this is an intrinsic function
        call get_intrinsic_info(name, node%is_intrinsic, node%intrinsic_signature)
    end function create_call_or_subscript

    function create_assignment(target_index, value_index, line, column, &
            inferred_type, inferred_type_name) result(node)
        integer, intent(in) :: target_index, value_index
        integer, intent(in), optional :: line, column
        type(mono_type_t), intent(in), optional :: inferred_type
        character(len=*), intent(in), optional :: inferred_type_name
        type(assignment_node) :: node

        node%target_index = target_index
        node%value_index = value_index
        node%operator = "="
        node%uid = generate_uid()
        if (present(inferred_type)) then
            node%inferred_type = inferred_type
        end if
        if (present(inferred_type_name)) then
            node%inferred_type_name = inferred_type_name
            node%type_was_inferred = .true.
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_assignment

    function create_program(name, body_indices, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(program_node) :: node

        node%name = name
        node%uid = generate_uid()
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                node%body_indices = body_indices
            end if
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_program

    function create_subroutine_call(name, args, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: args(:)
        integer, intent(in), optional :: line, column
        type(subroutine_call_node) :: node

        node%name = name
        node%uid = generate_uid()
        if (present(args)) then
            if (size(args) > 0) then
                node%arg_indices = args
            end if
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_subroutine_call

    function create_use_statement(module_name, only_list, rename_list, &
            has_only, line, column, url_spec) result(node)
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in), optional :: only_list(:), rename_list(:)
        character(len=*), intent(in), optional :: url_spec
        logical, intent(in), optional :: has_only
        integer, intent(in), optional :: line, column
        type(use_statement_node) :: node
        integer :: i

        node%module_name = module_name
        node%uid = generate_uid()
        if (present(url_spec)) node%url_spec = url_spec
        if (present(has_only)) node%has_only = has_only
        
        if (present(only_list)) then
            if (size(only_list) > 0) then
                allocate(node%only_list(size(only_list)))
                do i = 1, size(only_list)
                    node%only_list(i)%s = only_list(i)
                end do
            end if
        end if
        
        if (present(rename_list)) then
            if (size(rename_list) > 0) then
                allocate(node%rename_list(size(rename_list)))
                do i = 1, size(rename_list)
                    node%rename_list(i)%s = rename_list(i)
                end do
            end if
        end if
        
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_use_statement

    function create_implicit_statement(is_none, type_name, kind_value, has_kind, &
                                       length_value, has_length, letter_ranges, &
                                       line, column) result(node)
        logical, intent(in) :: is_none
        character(len=*), intent(in), optional :: type_name
        integer, intent(in), optional :: kind_value
        logical, intent(in), optional :: has_kind
        integer, intent(in), optional :: length_value
        logical, intent(in), optional :: has_length
        character(len=*), intent(in), optional :: letter_ranges(:)  ! "a", "i-n", "o-z"
        integer, intent(in), optional :: line, column
        type(implicit_statement_node) :: node
        integer :: i, dash_pos

        node%is_none = is_none
        node%uid = generate_uid()
        
        if (.not. is_none) then
            if (present(type_name)) node%type_spec%type_name = type_name
            if (present(has_kind)) node%type_spec%has_kind = has_kind
            if (present(kind_value)) node%type_spec%kind_value = kind_value
            if (present(has_length)) node%type_spec%has_length = has_length
            if (present(length_value)) node%type_spec%length_value = length_value
            
            if (present(letter_ranges)) then
                allocate(node%letter_specs(size(letter_ranges)))
                do i = 1, size(letter_ranges)
                    dash_pos = index(letter_ranges(i), '-')
                    if (dash_pos > 0) then
                        ! Range like "a-h"
                        node%letter_specs(i)%start_letter = letter_ranges(i)(1:1)
                        node%letter_specs(i)%end_letter = letter_ranges(i)(dash_pos+1:dash_pos+1)
                    else
                        ! Single letter like "a"
                        node%letter_specs(i)%start_letter = letter_ranges(i)(1:1)
                        node%letter_specs(i)%end_letter = letter_ranges(i)(1:1)
                    end if
                end do
            end if
        end if
        
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_implicit_statement

    function create_include_statement(filename, line, column) result(node)
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: line, column
        type(include_statement_node) :: node

        node%filename = filename
        node%uid = generate_uid()
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_include_statement

    function create_interface_block(name, kind, operator, procedure_indices, &
            line, column) result(node)
        character(len=*), intent(in), optional :: name, kind, operator
        integer, intent(in), optional :: procedure_indices(:)
        integer, intent(in), optional :: line, column
        type(interface_block_node) :: node

        node%uid = generate_uid()
        if (present(name)) node%name = name
        if (present(kind)) node%kind = kind
        if (present(operator)) node%operator = operator
        
        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                node%procedure_indices = procedure_indices
            end if
        end if
        
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_interface_block

    function create_module(name, declaration_indices, procedure_indices, &
            has_contains, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: declaration_indices(:), procedure_indices(:)
        logical, intent(in), optional :: has_contains
        integer, intent(in), optional :: line, column
        type(module_node) :: node

        node%uid = generate_uid()
        node%name = name
        if (present(has_contains)) node%has_contains = has_contains
        
        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) then
                node%declaration_indices = declaration_indices
            end if
        end if
        
        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                node%procedure_indices = procedure_indices
            end if
        end if
        
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_module

    function create_stop(stop_code_index, stop_message, line, column) result(node)
        integer, intent(in), optional :: stop_code_index
        character(len=*), intent(in), optional :: stop_message
        integer, intent(in), optional :: line, column
        type(stop_node) :: node

        node%uid = generate_uid()
        if (present(stop_code_index)) node%stop_code_index = stop_code_index
        if (present(stop_message)) node%stop_message = stop_message
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_stop

    function create_return(line, column) result(node)
        integer, intent(in), optional :: line, column
        type(return_node) :: node

        node%uid = generate_uid()
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_return

    function create_goto(label, line, column) result(node)
        character(len=*), intent(in), optional :: label
        integer, intent(in), optional :: line, column
        type(goto_node) :: node

        node%uid = generate_uid()
        if (present(label)) node%label = label
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_goto

    function create_error_stop(error_code_index, error_message, line, column) result(node)
        integer, intent(in), optional :: error_code_index
        character(len=*), intent(in), optional :: error_message
        integer, intent(in), optional :: line, column
        type(error_stop_node) :: node

        node%uid = generate_uid()
        if (present(error_code_index)) node%error_code_index = error_code_index
        if (present(error_message)) node%error_message = error_message
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_error_stop

    function create_cycle(loop_label, line, column) result(node)
        character(len=*), intent(in), optional :: loop_label
        integer, intent(in), optional :: line, column
        type(cycle_node) :: node

        node%uid = generate_uid()
        if (present(loop_label)) node%label = loop_label
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_cycle

    function create_exit(loop_label, line, column) result(node)
        character(len=*), intent(in), optional :: loop_label
        integer, intent(in), optional :: line, column
        type(exit_node) :: node

        node%uid = generate_uid()
        if (present(loop_label)) node%label = loop_label
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_exit

    function create_where(mask_expr_index, where_body_indices, &
            elsewhere_body_indices, line, column) result(node)
        integer, intent(in) :: mask_expr_index
        integer, intent(in), optional :: where_body_indices(:), &
                                          elsewhere_body_indices(:)
        integer, intent(in), optional :: line, column
        type(where_node) :: node

        node%uid = generate_uid()
        ! Initialize all fields to safe defaults
        node%mask_expr_index = mask_expr_index
        node%mask_is_simple = .false.
        node%can_vectorize = .false.
        
        ! Validate mask index
        if (mask_expr_index <= 0) then
            error stop "Invalid mask expression index in create_where"
        end if
        
        if (present(where_body_indices)) then
            if (size(where_body_indices) > 0) then
                allocate(node%where_body_indices(size(where_body_indices)))
                node%where_body_indices = where_body_indices
            end if
        end if
        
        ! For backward compatibility, treat simple elsewhere as final elsewhere
        if (present(elsewhere_body_indices)) then
            if (size(elsewhere_body_indices) > 0) then
                ! Create a single elsewhere clause without mask
                allocate(node%elsewhere_clauses(1))
                node%elsewhere_clauses(1)%mask_index = 0
                allocate(node%elsewhere_clauses(1)%body_indices(&
                    size(elsewhere_body_indices)))
                node%elsewhere_clauses(1)%body_indices = elsewhere_body_indices
            end if
        end if
        
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_where

    function create_comment(text, line, column) result(node)
        character(len=*), intent(in) :: text
        integer, intent(in), optional :: line, column
        type(comment_node) :: node

        node%uid = generate_uid()
        node%text = text
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_comment

    function create_blank_line(count, line, column) result(node)
        integer, intent(in), optional :: count
        integer, intent(in), optional :: line, column
        type(blank_line_node) :: node

        node%uid = generate_uid()
        if (present(count)) node%count = count
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_blank_line

    function create_end_statement(line, column) result(node)
        integer, intent(in), optional :: line, column
        type(end_statement_node) :: node

        node%uid = generate_uid()
        ! Input validation
        if (present(line)) then
            if (line < 1) then
                node%line = 1  ! Default to line 1 for invalid input
            else
                node%line = line
            end if
        end if
        
        if (present(column)) then
            if (column < 1) then
                node%column = 1  ! Default to column 1 for invalid input
            else
                node%column = column
            end if
        end if
    end function create_end_statement

    ! Backward compatibility wrapper for create_declaration
    function create_declaration_wrapper(type_name, var_name, kind_value, &
            initializer, dimensions, &
                               is_allocatable, is_pointer, line, column) result(node)
        character(len=*), intent(in) :: type_name
        character(len=*), intent(in) :: var_name
        integer, intent(in), optional :: kind_value
        class(ast_node), allocatable, intent(in), optional :: initializer
        type(ast_node_wrapper), intent(in), optional :: dimensions(:)
        logical, intent(in), optional :: is_allocatable
        logical, intent(in), optional :: is_pointer
        integer, intent(in), optional :: line, column
        type(declaration_node) :: node
        integer :: initializer_index
        integer, allocatable :: dim_indices(:)
        integer :: i

        node%uid = generate_uid()
        
        ! Convert initializer to index (assuming it would be in arena)
        initializer_index = 0
        
        ! Convert dimensions wrappers to indices
        if (present(dimensions)) then
            if (size(dimensions) > 0) then
                allocate(dim_indices(size(dimensions)))
                do i = 1, size(dimensions)
                    dim_indices(i) = dimensions(i)%stack_index
                end do
            end if
        end if

        ! Call the real create_declaration
        node = create_declaration(type_name, var_name, kind_value, &
                                  initializer_index, dim_indices, &
                                 is_allocatable, is_pointer, .false., line, column)
    end function create_declaration_wrapper

    function create_array_bounds(lower_index, upper_index, stride_index) result(node)
        integer, intent(in) :: lower_index, upper_index
        integer, intent(in), optional :: stride_index
        type(array_bounds_node) :: node

        node%uid = generate_uid()
        node%lower_bound_index = lower_index
        node%upper_bound_index = upper_index
        if (present(stride_index)) then
            node%stride_index = stride_index
        else
            node%stride_index = -1
        end if
    end function create_array_bounds

    function create_array_slice(array_index, bounds_indices, num_dims) result(node)
        integer, intent(in) :: array_index
        integer, intent(in) :: bounds_indices(:)
        integer, intent(in) :: num_dims
        type(array_slice_node) :: node
        integer :: i

        node%uid = generate_uid()
        node%array_index = array_index
        ! Validate non-negative dimensions
        if (num_dims < 0) then
            node%num_dimensions = 0
        else
            node%num_dimensions = min(num_dims, 10)  ! Max 10 dimensions
        end if
        
        do i = 1, node%num_dimensions
            if (i <= size(bounds_indices)) then
                node%bounds_indices(i) = bounds_indices(i)
            else
                node%bounds_indices(i) = -1
            end if
        end do
    end function create_array_slice

    function create_range_expression(start_index, end_index, stride_index) result(node)
        integer, intent(in) :: start_index, end_index
        integer, intent(in), optional :: stride_index
        type(range_expression_node) :: node

        node%uid = generate_uid()
        node%start_index = start_index
        node%end_index = end_index
        if (present(stride_index)) then
            node%stride_index = stride_index
        else
            node%stride_index = -1
        end if
    end function create_range_expression

    function create_array_operation(operation, left_index, right_index, &
                                    array_spec, result_spec) result(node)
        character(len=*), intent(in) :: operation
        integer, intent(in) :: left_index, right_index
        type(array_spec_t), intent(in), optional :: array_spec, result_spec
        type(array_operation_node) :: node

        node%uid = generate_uid()
        node%operation = operation
        node%left_operand_index = left_index
        node%right_operand_index = right_index
        
        if (present(array_spec)) then
            node%array_spec = array_spec
        end if
        
        if (present(result_spec)) then
            node%result_spec = result_spec
        end if
        
        ! These will be set by semantic analysis
        node%bounds_checked = .false.
        node%shape_conformant = .false.
    end function create_array_operation

end module ast_core