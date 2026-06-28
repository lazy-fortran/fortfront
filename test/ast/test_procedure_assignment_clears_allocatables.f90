program test_procedure_assignment_clears_allocatables
    use ast_nodes_core, only: array_literal_node, assignment_node, binary_op_node, &
        call_or_subscript_node, identifier_node, &
        literal_node, program_node
    use ast_nodes_data, only: declaration_node, mixed_construct_container_node, &
        parameter_declaration_node, &
        create_mixed_construct_container
    use ast_nodes_io, only: print_statement_node
    use ast_nodes_procedure, only: function_def_node, subroutine_call_node, &
        subroutine_def_node, create_function_def, &
        create_subroutine_def
    implicit none

    call test_core_assignment()
    call test_declaration_assignment()
    call test_parameter_declaration_assignment()
    call test_print_assignment()
    call test_function_assignment()
    call test_subroutine_assignment()
    call test_call_assignment()
    call test_mixed_container_assignment()

    print *, 'PASS: AST assignment clears absent allocatable fields'

contains

    subroutine test_core_assignment()
        type(program_node) :: program_lhs, program_rhs
        type(assignment_node) :: assign_lhs, assign_rhs
        type(identifier_node) :: ident_lhs, ident_rhs
        type(literal_node) :: literal_lhs, literal_rhs
        type(binary_op_node) :: binary_lhs, binary_rhs
        type(call_or_subscript_node) :: call_lhs, call_rhs
        type(array_literal_node) :: array_lhs, array_rhs

        program_lhs%name = 'old'
        program_lhs%body_indices = [1]
        program_rhs%line = 1
        program_lhs = program_rhs
        if (allocated(program_lhs%name)) error stop 'stale program name'
        if (allocated(program_lhs%body_indices)) error stop 'stale program body'

        assign_lhs%operator = '='
        assign_lhs%inferred_type_name = 'integer'
        assign_rhs%target_index = 1
        assign_lhs = assign_rhs
        if (allocated(assign_lhs%operator)) error stop 'stale assignment operator'
        if (allocated(assign_lhs%inferred_type_name)) then
            error stop 'stale assignment inferred type'
        end if

        ident_lhs%name = 'old'
        ident_rhs%line = 2
        ident_lhs = ident_rhs
        if (allocated(ident_lhs%name)) error stop 'stale identifier name'

        literal_lhs%value = '1'
        literal_lhs%literal_type = 'integer'
        literal_rhs%literal_kind = 1
        literal_lhs = literal_rhs
        if (allocated(literal_lhs%value)) error stop 'stale literal value'
        if (allocated(literal_lhs%literal_type)) error stop 'stale literal type'

        binary_lhs%operator = '+'
        binary_rhs%left_index = 1
        binary_lhs = binary_rhs
        if (allocated(binary_lhs%operator)) error stop 'stale binary operator'

        call_lhs%name = 'old'
        call_lhs%arg_indices = [1]
        call_lhs%intrinsic_signature = 'old(integer)'
        call_rhs%is_intrinsic = .true.
        call_lhs = call_rhs
        if (allocated(call_lhs%name)) error stop 'stale call name'
        if (allocated(call_lhs%arg_indices)) error stop 'stale call args'
        if (allocated(call_lhs%intrinsic_signature)) then
            error stop 'stale call intrinsic signature'
        end if

        array_lhs%element_indices = [1]
        array_lhs%element_type = 'integer'
        array_lhs%type_spec = 'integer'
        array_lhs%syntax_style = 'bracket'
        array_rhs%line = 3
        array_lhs = array_rhs
        if (allocated(array_lhs%element_indices)) error stop 'stale array elements'
        if (allocated(array_lhs%element_type)) error stop 'stale array element type'
        if (allocated(array_lhs%type_spec)) error stop 'stale array type spec'
        if (allocated(array_lhs%syntax_style)) error stop 'stale array syntax'
    end subroutine test_core_assignment

    subroutine test_declaration_assignment()
        type(declaration_node) :: lhs
        type(declaration_node) :: rhs

        lhs%type_name = 'integer'
        lhs%var_name = 'old'
        lhs%var_names = [character(len=4) :: 'old1', 'old2']
        lhs%character_length_expr = '*'
        lhs%intent = 'in'
        lhs%accessibility = 'public'
        lhs%dimension_indices = [1]
        rhs%line = 4

        lhs = rhs

        if (allocated(lhs%type_name)) error stop 'stale declaration type'
        if (allocated(lhs%var_name)) error stop 'stale declaration variable'
        if (allocated(lhs%var_names)) error stop 'stale declaration variables'
        if (allocated(lhs%character_length_expr)) then
            error stop 'stale declaration character length'
        end if
        if (allocated(lhs%intent)) error stop 'stale declaration intent'
        if (allocated(lhs%accessibility)) error stop 'stale declaration access'
        if (allocated(lhs%dimension_indices)) error stop 'stale declaration dims'
    end subroutine test_declaration_assignment

    subroutine test_parameter_declaration_assignment()
        type(parameter_declaration_node) :: lhs
        type(parameter_declaration_node) :: rhs

        lhs%name = 'arg'
        lhs%type_name = 'integer'
        lhs%character_length_expr = '*'
        lhs%dimension_indices = [1]
        rhs%line = 5

        lhs = rhs

        if (allocated(lhs%name)) error stop 'stale parameter name'
        if (allocated(lhs%type_name)) error stop 'stale parameter type'
        if (allocated(lhs%character_length_expr)) then
            error stop 'stale parameter character length'
        end if
        if (allocated(lhs%dimension_indices)) error stop 'stale parameter dims'
    end subroutine test_parameter_declaration_assignment

    subroutine test_print_assignment()
        type(print_statement_node) :: lhs
        type(print_statement_node) :: rhs

        lhs%expression_indices = [1, 2]
        lhs%format_spec = '*'
        rhs%line = 6

        lhs = rhs

        if (allocated(lhs%expression_indices)) error stop 'stale print expressions'
        if (allocated(lhs%format_spec)) error stop 'stale print format'
    end subroutine test_print_assignment

    subroutine test_function_assignment()
        type(function_def_node) :: lhs
        type(function_def_node) :: rhs

        lhs = create_function_def('old', param_indices=[1], return_type='integer', &
            body_indices=[2], result_variable='res', &
            prefix_keywords=[character(len=16) :: 'pure'])
        lhs%param_intents = [character(len=16) :: 'in']
        lhs%bind_c_clause = 'bind(c)'

        rhs = create_function_def('new', return_type='')
        lhs = rhs

        if (allocated(lhs%param_indices)) error stop 'stale function params'
        if (allocated(lhs%body_indices)) error stop 'stale function body'
        if (allocated(lhs%result_variable)) error stop 'stale result variable'
        if (allocated(lhs%prefix_keywords)) error stop 'stale function prefixes'
        if (allocated(lhs%param_intents)) error stop 'stale function intents'
        if (allocated(lhs%bind_c_clause)) error stop 'stale function bind clause'
    end subroutine test_function_assignment

    subroutine test_subroutine_assignment()
        type(subroutine_def_node) :: lhs
        type(subroutine_def_node) :: rhs

        lhs = create_subroutine_def('old', param_indices=[1], body_indices=[2], &
            prefix_keywords=[character(len=16) :: &
            'recursive'], &
            is_recursive=.true.)
        lhs%param_intents = [character(len=16) :: 'out']
        lhs%bind_c_clause = 'bind(c)'

        rhs = create_subroutine_def('new')
        lhs = rhs

        if (allocated(lhs%param_indices)) error stop 'stale subroutine params'
        if (allocated(lhs%body_indices)) error stop 'stale subroutine body'
        if (allocated(lhs%prefix_keywords)) error stop 'stale subroutine prefixes'
        if (allocated(lhs%param_intents)) error stop 'stale subroutine intents'
        if (allocated(lhs%bind_c_clause)) error stop 'stale subroutine bind clause'
        if (lhs%is_recursive) error stop 'stale recursive flag'
    end subroutine test_subroutine_assignment

    subroutine test_call_assignment()
        type(subroutine_call_node) :: lhs
        type(subroutine_call_node) :: rhs

        lhs%name = 'old'
        lhs%arg_indices = [1, 2]
        rhs%name = 'new'

        lhs = rhs

        if (allocated(lhs%arg_indices)) error stop 'stale call args'
    end subroutine test_call_assignment

    subroutine test_mixed_container_assignment()
        type(mixed_construct_container_node) :: lhs
        type(mixed_construct_container_node) :: rhs

        lhs = create_mixed_construct_container('old', implicit_indices=[1], &
            explicit_indices=[2])

        rhs%line = 3
        lhs = rhs

        if (allocated(lhs%module_name)) error stop 'stale container module name'
        if (allocated(lhs%implicit_declaration_indices)) then
            error stop 'stale container implicit indices'
        end if
        if (allocated(lhs%explicit_program_indices)) then
            error stop 'stale container explicit indices'
        end if
    end subroutine test_mixed_container_assignment

end program test_procedure_assignment_clears_allocatables
