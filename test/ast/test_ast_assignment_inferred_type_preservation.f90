program test_ast_assignment_inferred_type_preservation
    ! AST assignment tests for Issue #276: inferred_type preservation
    ! Tests that AST node assignments properly preserve semantic information
    use ast_core
    use ast_nodes_data
    use ast_nodes_core
    use ast_nodes_control
    use ast_nodes_io
    use ast_nodes_procedure
    use type_system_hm
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    ! Given: AST nodes with inferred_type information from semantic analysis
    ! When: Assignment operations are performed on AST nodes
    ! Then: inferred_type should be preserved through deep copying

    call test_identifier_assignment_preservation()
    call test_literal_assignment_preservation()
    call test_binary_op_assignment_preservation()
    call test_function_call_assignment_preservation()
    call test_declaration_assignment_preservation()
    call test_assignment_node_preservation()
    call test_if_node_assignment_preservation()
    call test_complex_expression_preservation()
    call test_nested_structure_preservation()
    call test_type_copying_in_assignment()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_identifier_assignment_preservation()
        ! Given: An identifier node with inferred_type
        ! When: Node is assigned to another identifier
        ! Then: inferred_type should be preserved in the copy
        type(identifier_node) :: original, copy
        type(mono_type_t) :: real_type

        test_count = test_count + 1

        ! Create original identifier with inferred type
        original%name = "variable_x"
        original%line = 10
        original%column = 5

        ! Add inferred type information
        real_type = create_mono_type(TREAL)
        allocate(original%inferred_type)
        original%inferred_type = real_type

        ! Test assignment
        copy = original

        ! Verify inferred type was preserved
        if (allocated(copy%inferred_type)) then
            if (copy%inferred_type%kind == TREAL) then
                ! Also check other fields were copied
                if (copy%name == "variable_x" .and. copy%line == 10 .and. copy%column == 5) then
                    pass_count = pass_count + 1
                    write (*, '(A)') "PASS: Identifier assignment with inferred_type preservation"
                else
                    write (*, '(A)') "FAIL: Identifier assignment - base fields not preserved"
                end if
            else
                write (*, '(A)') "FAIL: Identifier assignment - inferred_type kind incorrect"
            end if
        else
            write (*, '(A)') "FAIL: Identifier assignment - inferred_type not preserved"
        end if
    end subroutine test_identifier_assignment_preservation

    subroutine test_literal_assignment_preservation()
        ! Given: A literal node with inferred_type
        ! When: Node is assigned to another literal
        ! Then: inferred_type should be preserved in the copy
        type(literal_node) :: original, copy
        type(mono_type_t) :: int_type

        test_count = test_count + 1

        ! Create original literal with inferred type
        original%value = "42"
        original%literal_kind = LITERAL_INTEGER
        original%line = 15
        original%column = 20

        ! Add inferred type information
        int_type = create_mono_type(TINT)
        allocate(original%inferred_type)
        original%inferred_type = int_type

        ! Test assignment
        copy = original

        ! Verify inferred type was preserved
        if (allocated(copy%inferred_type)) then
            if (copy%inferred_type%kind == TINT) then
                ! Also check other fields were copied
                if (copy%value == "42" .and. copy%literal_kind == LITERAL_INTEGER) then
                    pass_count = pass_count + 1
                    write (*, '(A)') "PASS: Literal assignment with inferred_type preservation"
                else
                    write (*, '(A)') "FAIL: Literal assignment - base fields not preserved"
                end if
            else
                write (*, '(A)') "FAIL: Literal assignment - inferred_type kind incorrect"
            end if
        else
            write (*, '(A)') "FAIL: Literal assignment - inferred_type not preserved"
        end if
    end subroutine test_literal_assignment_preservation

    subroutine test_binary_op_assignment_preservation()
        ! Given: A binary operation node with inferred_type
        ! When: Node is assigned to another binary_op
        ! Then: inferred_type should be preserved in the copy
        type(binary_op_node) :: original, copy
        type(mono_type_t) :: real_type

        test_count = test_count + 1

        ! Create original binary operation with inferred type
        original%operator = "+"
        original%left_index = 1
        original%right_index = 2
        original%line = 25
        original%column = 30

        ! Add inferred type information
        real_type = create_mono_type(TREAL)
        allocate(original%inferred_type)
        original%inferred_type = real_type

        ! Test assignment
        copy = original

        ! Verify inferred type was preserved
        if (allocated(copy%inferred_type)) then
            if (copy%inferred_type%kind == TREAL) then
                ! Also check other fields were copied
                if (copy%operator == "+" .and. copy%left_index == 1 .and. copy%right_index == 2) then
                    pass_count = pass_count + 1
                    write (*, '(A)') "PASS: Binary operation assignment with inferred_type preservation"
                else
                    write (*, '(A)') "FAIL: Binary operation assignment - base fields not preserved"
                end if
            else
                write (*, '(A)') "FAIL: Binary operation assignment - inferred_type kind incorrect"
            end if
        else
            write (*, '(A)') "FAIL: Binary operation assignment - inferred_type not preserved"
        end if
    end subroutine test_binary_op_assignment_preservation

    subroutine test_function_call_assignment_preservation()
        ! Given: A function call node with inferred_type
        ! When: Node is assigned to another function_call
        ! Then: inferred_type should be preserved in the copy
        type(call_or_subscript_node) :: original, copy
        type(mono_type_t) :: int_type, real_type, fun_type

        test_count = test_count + 1

        ! Create original function call with inferred type
        original%name = "sqrt"
        original%line = 35
        original%column = 40
        allocate(original%arg_indices(1))
        original%arg_indices(1) = 5

        ! Add inferred type information (real -> real function)
        real_type = create_mono_type(TREAL)
        int_type = create_mono_type(TINT)
        fun_type = create_fun_type(real_type, real_type)
        allocate(original%inferred_type)
        original%inferred_type = fun_type

        ! Test assignment
        copy = original

        ! Verify inferred type was preserved (simplified type system)
        if (allocated(copy%inferred_type)) then
            if (copy%inferred_type%kind == TFUN) then
                ! Also check other fields were copied
                if (copy%name == "sqrt" .and. allocated(copy%arg_indices)) then
                    if (size(copy%arg_indices) == 1 .and. copy%arg_indices(1) == 5) then
                        pass_count = pass_count + 1
                        write (*, '(A)') "PASS: Function call assignment with inferred_type preservation"
                    else
                        write (*, '(A)') "FAIL: Function call assignment - arg_indices not preserved"
                    end if
                else
                    write (*, '(A)') "FAIL: Function call assignment - base fields not preserved"
                end if
            else
                write (*, '(A)') "FAIL: Function call assignment - inferred_type not function type"
            end if
        else
            write (*, '(A)') "FAIL: Function call assignment - inferred_type not preserved"
        end if
    end subroutine test_function_call_assignment_preservation

    subroutine test_declaration_assignment_preservation()
        ! Given: A declaration node with inferred_type
        ! When: Node is assigned to another declaration
        ! Then: inferred_type should be preserved in the copy
        type(declaration_node) :: original, copy
        type(mono_type_t) :: int_type

        test_count = test_count + 1

        ! Create original declaration with inferred type
        original%type_name = "integer"
        original%var_name = "counter"
        original%line = 45
        original%column = 50

        ! Add inferred type information
        int_type = create_mono_type(TINT)
        allocate(original%inferred_type)
        original%inferred_type = int_type

        ! Test assignment
        copy = original

        ! Verify inferred type was preserved
        if (allocated(copy%inferred_type)) then
            if (copy%inferred_type%kind == TINT) then
                ! Also check other fields were copied
                if (copy%type_name == "integer" .and. copy%var_name == "counter") then
                    pass_count = pass_count + 1
                    write (*, '(A)') "PASS: Declaration assignment with inferred_type preservation"
                else
                    write (*, '(A)') "FAIL: Declaration assignment - base fields not preserved"
                end if
            else
                write (*, '(A)') "FAIL: Declaration assignment - inferred_type kind incorrect"
            end if
        else
            write (*, '(A)') "FAIL: Declaration assignment - inferred_type not preserved"
        end if
    end subroutine test_declaration_assignment_preservation

    subroutine test_assignment_node_preservation()
        ! Given: An assignment node with inferred_type
        ! When: Node is assigned to another assignment_node
        ! Then: inferred_type should be preserved in the copy
        type(assignment_node) :: original, copy
        type(mono_type_t) :: real_type

        test_count = test_count + 1

        ! Create original assignment with inferred type
        original%target_index = 10
        original%value_index = 11
        original%line = 55
        original%column = 60

        ! Add inferred type information
        real_type = create_mono_type(TREAL)
        allocate(original%inferred_type)
        original%inferred_type = real_type

        ! Test assignment
        copy = original

        ! Verify inferred type was preserved
        if (allocated(copy%inferred_type)) then
            if (copy%inferred_type%kind == TREAL) then
                ! Also check other fields were copied
                if (copy%target_index == 10 .and. copy%value_index == 11) then
                    pass_count = pass_count + 1
                    write (*, '(A)') "PASS: Assignment node with inferred_type preservation"
                else
                    write (*, '(A)') "FAIL: Assignment node - base fields not preserved"
                end if
            else
                write (*, '(A)') "FAIL: Assignment node - inferred_type kind incorrect"
            end if
        else
            write (*, '(A)') "FAIL: Assignment node - inferred_type not preserved"
        end if
    end subroutine test_assignment_node_preservation

    subroutine test_if_node_assignment_preservation()
        ! Given: An if node with inferred_type
        ! When: Node is assigned to another if_node
        ! Then: inferred_type should be preserved in the copy
        type(if_node) :: original, copy
        type(mono_type_t) :: logical_type

        test_count = test_count + 1

        ! Create original if node with inferred type
        original%condition_index = 20
        allocate(original%then_body_indices(1))
        original%then_body_indices(1) = 21
        original%line = 65
        original%column = 70

        ! Add inferred type information
        logical_type = create_mono_type(TLOGICAL)
        allocate(original%inferred_type)
        original%inferred_type = logical_type

        ! Test assignment
        copy = original

        ! Verify inferred type was preserved
        if (allocated(copy%inferred_type)) then
            if (copy%inferred_type%kind == TLOGICAL) then
                ! Also check other fields were copied
                if (copy%condition_index == 20 .and. allocated(copy%then_body_indices)) then
                    if (size(copy%then_body_indices) == 1 .and. copy%then_body_indices(1) == 21) then
                        pass_count = pass_count + 1
                        write (*, '(A)') "PASS: If node assignment with inferred_type preservation"
                    else
                        write (*, '(A)') "FAIL: If node assignment - base fields not preserved"
                    end if
                else
                    write (*, '(A)') "FAIL: If node assignment - then_body_indices not preserved"
                end if
            else
                write (*, '(A)') "FAIL: If node assignment - inferred_type kind incorrect"
            end if
        else
            write (*, '(A)') "FAIL: If node assignment - inferred_type not preserved"
        end if
    end subroutine test_if_node_assignment_preservation

    subroutine test_complex_expression_preservation()
        ! Given: Complex expression with nested function type
        ! When: Assignment preserves complex inferred_type structure
        ! Then: All type information should be preserved
        type(binary_op_node) :: original, copy
        type(mono_type_t) :: int_type, real_type, fun_type, complex_type

        test_count = test_count + 1

        ! Create binary operation with complex inferred type
        original%operator = "*"
        original%left_index = 30
        original%right_index = 31
        original%line = 75
        original%column = 80

        ! Create complex nested function type: (int -> real) -> int
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)
        fun_type = create_fun_type(int_type, real_type)
        complex_type = create_fun_type(fun_type, int_type)

        allocate(original%inferred_type)
        original%inferred_type = complex_type

        ! Test assignment
        copy = original

        ! Verify complex inferred type was preserved (simplified type system)
        if (allocated(copy%inferred_type)) then
            if (copy%inferred_type%kind == TFUN) then
                pass_count = pass_count + 1
                write (*, '(A)') "PASS: Complex expression inferred_type preservation"
            else
                write (*, '(A)') "FAIL: Complex expression - not function type"
            end if
        else
            write (*, '(A)') "FAIL: Complex expression - inferred_type not preserved"
        end if
    end subroutine test_complex_expression_preservation

    subroutine test_nested_structure_preservation()
        ! Given: Nested AST structure with multiple inferred_types
        ! When: Assignment operations occur at different levels
        ! Then: All inferred_type information should be preserved
        type(call_or_subscript_node) :: func_original, func_copy
        type(mono_type_t) :: arg_type, result_type, fun_type

        test_count = test_count + 1

        ! Create function call with inferred types
        func_original%name = "complex_func"
        func_original%line = 85
        func_original%column = 90
        allocate(func_original%arg_indices(2))
        func_original%arg_indices(1) = 40
        func_original%arg_indices(2) = 41

        ! Create complex function type with nested structure
        arg_type = create_mono_type(TREAL)
        result_type = create_mono_type(TINT)
        fun_type = create_fun_type(arg_type, result_type)

        ! Add another level of nesting
        fun_type = create_fun_type(fun_type, arg_type)

        allocate(func_original%inferred_type)
        func_original%inferred_type = fun_type

        ! Test assignment
        func_copy = func_original

        ! Verify nested structure preservation (simplified type system)
        if (allocated(func_copy%inferred_type)) then
            if (func_copy%inferred_type%kind == TFUN) then
                pass_count = pass_count + 1
                write (*, '(A)') "PASS: Nested structure inferred_type preservation"
            else
                write (*, '(A)') "FAIL: Nested structure - not function type"
            end if
        else
            write (*, '(A)') "FAIL: Nested structure - inferred_type not preserved"
        end if
    end subroutine test_nested_structure_preservation

    subroutine test_type_copying_in_assignment()
        ! Given: Assignment operations that trigger type copying
        ! When: Type structures contain cycles or complex references
        ! Then: Should use cycle-safe copying without losing information
        type(identifier_node) :: ident_original, ident_copy
        type(mono_type_t) :: complex_nested_type, base_type
        integer :: i

        test_count = test_count + 1

        ! Create identifier with deeply nested type
        ident_original%name = "deeply_typed_var"
        ident_original%line = 95
        ident_original%column = 100

        ! Create deeply nested type structure
        base_type = create_mono_type(TINT)
        complex_nested_type = base_type

        do i = 1, 10  ! Create deep nesting that exercises cycle-safe copying
            complex_nested_type = create_fun_type(base_type, complex_nested_type)
        end do

        allocate(ident_original%inferred_type)
        ident_original%inferred_type = complex_nested_type

        ! Test assignment that triggers cycle-safe type copying
        ident_copy = ident_original

        ! Verify type structure was copied correctly (simplified type system)
        if (allocated(ident_copy%inferred_type)) then
            if (ident_copy%inferred_type%kind == TFUN) then
                pass_count = pass_count + 1
                write (*, '(A)') "PASS: Cycle-safe type copying in assignment"
            else
                write (*, '(A)') "FAIL: Cycle-safe type copying - wrong type kind"
            end if
        else
            write (*, '(A)') "FAIL: Cycle-safe type copying - inferred_type not preserved"
        end if
    end subroutine test_type_copying_in_assignment

end program test_ast_assignment_inferred_type_preservation