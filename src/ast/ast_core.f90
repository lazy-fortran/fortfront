module ast_core
    ! Compatibility module that re-exports all AST modules
    ! This allows existing code to continue working during the transition
    
    use type_system_hm, only: mono_type_t
    use intrinsic_registry, only: get_intrinsic_info
    
    ! Re-export base types and interfaces
    use ast_base, only: ast_node, visit_interface, to_json_interface, string_t, &
                        ast_node_wrapper, &
                        LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, &
                        LITERAL_LOGICAL, LITERAL_ARRAY, LITERAL_COMPLEX
    
    ! Re-export arena functionality
    use ast_arena, only: ast_arena_t, ast_entry_t, ast_arena_stats_t, create_ast_arena
    
    ! Re-export all node types
    use ast_nodes_core, only: program_node, assignment_node, &
                              pointer_assignment_node, identifier_node, literal_node, &
                              binary_op_node, call_or_subscript_node, array_literal_node, &
                              create_pointer_assignment, create_array_literal
    use ast_nodes_control, only: if_node, do_loop_node, do_while_node, forall_node, &
                                 elseif_wrapper, case_wrapper, &
                                 select_case_node, case_block_node, case_range_node, &
                                 case_default_node, &
                                 where_node, cycle_node, exit_node, stop_node, return_node, &
                                 create_do_loop, create_do_while, create_if, &
                                 create_select_case
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
                                   subroutine_call_node, &
                                   create_function_def, create_subroutine_def
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
                               module_node, derived_type_node, &
                               create_declaration, create_derived_type
    use ast_nodes_io, only: print_statement_node, write_statement_node, &
                             read_statement_node, format_descriptor_node, &
                             create_print_statement
    use ast_nodes_misc, only: complex_literal_node, allocate_statement_node, &
                              deallocate_statement_node, &
                              use_statement_node, include_statement_node, &
                              contains_node, interface_block_node, &
                              comment_node
    
    implicit none
    
    ! Re-export everything as public
    public :: ast_node, visit_interface, to_json_interface, string_t, &
              ast_node_wrapper
    public :: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL, &
              LITERAL_ARRAY, LITERAL_COMPLEX
    public :: ast_arena_t, ast_entry_t, ast_arena_stats_t, create_ast_arena
    public :: program_node, assignment_node, pointer_assignment_node, &
              identifier_node, literal_node
    public :: binary_op_node, call_or_subscript_node, array_literal_node
    public :: if_node, do_loop_node, do_while_node, forall_node, &
              elseif_wrapper, case_wrapper
    public :: select_case_node, case_block_node, case_range_node, case_default_node
    public :: where_node, cycle_node, exit_node, stop_node, return_node
    public :: function_def_node, subroutine_def_node, subroutine_call_node
    public :: declaration_node, parameter_declaration_node, module_node, &
              derived_type_node
    public :: print_statement_node, write_statement_node, &
              read_statement_node, format_descriptor_node
    public :: complex_literal_node, allocate_statement_node, &
              deallocate_statement_node
    public :: use_statement_node, include_statement_node, contains_node, &
              interface_block_node, comment_node
    ! Re-export factory functions
    public :: create_pointer_assignment, create_array_literal, &
              create_function_def, create_subroutine_def, &
              create_print_statement, create_declaration, create_do_loop, &
              create_do_while, create_if, &
              create_select_case, create_derived_type
    ! Factory functions in this module
    public :: create_identifier, create_literal, create_binary_op, &
              create_call_or_subscript, &
              create_assignment, create_program, create_subroutine_call, &
              create_use_statement, &
              create_include_statement, create_interface_block, create_module, &
              create_stop, &
              create_return, create_cycle, create_exit, create_where, &
              create_comment
    
contains

    ! Factory functions for backward compatibility
    
    function create_identifier(name, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: line, column
        type(identifier_node) :: node

        node%name = name
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
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_binary_op

    function create_call_or_subscript(name, args, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: args(:)
        integer, intent(in), optional :: line, column
        type(call_or_subscript_node) :: node

        node%name = name
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
        if (present(inferred_type)) then
            allocate(node%inferred_type)
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
        if (present(args)) then
            if (size(args) > 0) then
                node%arg_indices = args
            end if
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_subroutine_call

    function create_use_statement(module_name, only_list, rename_list, has_only, &
            line, column) result(node)
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in), optional :: only_list(:), rename_list(:)
        logical, intent(in), optional :: has_only
        integer, intent(in), optional :: line, column
        type(use_statement_node) :: node
        integer :: i

        node%module_name = module_name
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

    function create_include_statement(filename, line, column) result(node)
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: line, column
        type(include_statement_node) :: node

        node%filename = filename
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_include_statement

    function create_interface_block(name, kind, operator, procedure_indices, &
            line, column) result(node)
        character(len=*), intent(in), optional :: name, kind, operator
        integer, intent(in), optional :: procedure_indices(:)
        integer, intent(in), optional :: line, column
        type(interface_block_node) :: node

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

        if (present(stop_code_index)) node%stop_code_index = stop_code_index
        if (present(stop_message)) node%stop_message = stop_message
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_stop

    function create_return(line, column) result(node)
        integer, intent(in), optional :: line, column
        type(return_node) :: node

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_return

    function create_cycle(loop_label, line, column) result(node)
        character(len=*), intent(in), optional :: loop_label
        integer, intent(in), optional :: line, column
        type(cycle_node) :: node

        if (present(loop_label)) node%label = loop_label
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_cycle

    function create_exit(loop_label, line, column) result(node)
        character(len=*), intent(in), optional :: loop_label
        integer, intent(in), optional :: line, column
        type(exit_node) :: node

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
                allocate(node%elsewhere_clauses(1)%body_indices(size(elsewhere_body_indices)))
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

        node%text = text
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_comment

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
                                 is_allocatable, is_pointer, line, column)
    end function create_declaration_wrapper

end module ast_core