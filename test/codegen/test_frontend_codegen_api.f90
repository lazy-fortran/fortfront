program test_frontend_codegen_api
    use codegen_core, only: generate_code_from_arena
    use ast_core
    use ast_factory
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Code Generation API Unit Tests ==='
    print *

    ! Test individual code generation features via API
    if (.not. test_assignment_codegen()) all_passed = .false.
    if (.not. test_literal_codegen()) all_passed = .false.
    if (.not. test_binary_operation_codegen()) all_passed = .false.
    if (.not. test_print_statement_codegen()) all_passed = .false.
    if (.not. test_program_codegen()) all_passed = .false.
    if (.not. test_identifier_codegen()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All code generation API tests passed!'
        stop 0
    else
        print *, 'Some code generation API tests failed!'
        stop 1
    end if

contains

    logical function test_assignment_codegen()
        type(ast_arena_t) :: arena
        integer :: target_index, value_index, assign_index
        character(len=:), allocatable :: code

        test_assignment_codegen = .true.
        print *, 'Testing assignment code generation...'

        ! Create arena
        arena = create_ast_arena()

        ! Create assignment: x = 42
        target_index = push_identifier(arena, 'x', 1, 1)
        value_index = push_literal(arena, '42', LITERAL_INTEGER, 1, 5)
        assign_index = push_assignment(arena, target_index, value_index, 1, 1)

        ! Generate code
        code = generate_code_from_arena(arena, assign_index)

        if (len_trim(code) > 0) then
            print *, '  PASS: Assignment code generation'
            print *, '    Generated:', trim(code)
        else
            print *, '  FAIL: Assignment code generation produced empty output'
            test_assignment_codegen = .false.
        end if

        ! Clean up
        call arena%clear()

    end function test_assignment_codegen

    logical function test_literal_codegen()
        type(ast_arena_t) :: arena
        integer :: lit_index
        character(len=:), allocatable :: code

        test_literal_codegen = .true.
        print *, 'Testing literal code generation...'

        ! Create arena
        arena = create_ast_arena()

        ! Integer literal
        lit_index = push_literal(arena, '123', LITERAL_INTEGER, 1, 1)
        code = generate_code_from_arena(arena, lit_index)

        if (len_trim(code) > 0) then
            print *, '  PASS: Integer literal code generation'
        else
            print *, '  FAIL: Integer literal code generation'
            test_literal_codegen = .false.
            call arena%clear()
            return
        end if

        ! Clear arena for next test
        call arena%clear()

        ! Real literal
        lit_index = push_literal(arena, '3.14', LITERAL_REAL, 1, 1)
        code = generate_code_from_arena(arena, lit_index)

        if (len_trim(code) > 0) then
            print *, '  PASS: Real literal code generation'
        else
            print *, '  FAIL: Real literal code generation'
            test_literal_codegen = .false.
        end if

        ! Clean up
        call arena%clear()

    end function test_literal_codegen

    logical function test_binary_operation_codegen()
        type(ast_arena_t) :: arena
        integer :: left_index, right_index, binop_index
        character(len=:), allocatable :: code

        test_binary_operation_codegen = .true.
        print *, 'Testing binary operation code generation...'

        ! Create arena
        arena = create_ast_arena()

        ! Create binary operation: a + b
        left_index = push_identifier(arena, 'a', 1, 1)
        right_index = push_identifier(arena, 'b', 1, 5)
        binop_index = push_binary_op(arena, left_index, right_index, '+', 1, 3)

        ! Generate code
        code = generate_code_from_arena(arena, binop_index)

        if (len_trim(code) > 0) then
            print *, '  PASS: Binary operation code generation'
            print *, '    Generated:', trim(code)
        else
            print *, '  FAIL: Binary operation code generation'
            test_binary_operation_codegen = .false.
        end if

        ! Clean up
        call arena%clear()

    end function test_binary_operation_codegen

    logical function test_print_statement_codegen()
        type(ast_arena_t) :: arena
        integer :: arg_index, print_index
        integer :: arg_indices(1)
        character(len=:), allocatable :: code

        test_print_statement_codegen = .true.
        print *, 'Testing print statement code generation...'

        ! Create arena
        arena = create_ast_arena()

        ! Create print statement with one argument
        arg_index = push_identifier(arena, 'result', 1, 7)
        arg_indices(1) = arg_index
        print_index = push_print_statement(arena, '*', arg_indices, line=1, column=1)

        ! Generate code
        code = generate_code_from_arena(arena, print_index)

        if (len_trim(code) > 0) then
            print *, '  PASS: Print statement code generation'
            print *, '    Generated:', trim(code)
        else
            print *, '  FAIL: Print statement code generation'
            test_print_statement_codegen = .false.
        end if

        ! Clean up
        call arena%clear()

    end function test_print_statement_codegen

    logical function test_program_codegen()
        type(ast_arena_t) :: arena
        integer :: target_index, value_index, assign_index, prog_index
        integer :: body_indices(1)
        character(len=:), allocatable :: code

        test_program_codegen = .true.
        print *, 'Testing program code generation...'

        ! Create arena
        arena = create_ast_arena()

        ! Create assignment: answer = 42
        target_index = push_identifier(arena, 'answer', 1, 1)
        value_index = push_literal(arena, '42', LITERAL_INTEGER, 1, 10)
        assign_index = push_assignment(arena, target_index, value_index, 1, 1)

        ! Create program with the assignment
        body_indices(1) = assign_index
        prog_index = push_program(arena, 'test_program', body_indices, line=1, column=1)

        ! Generate code
        code = generate_code_from_arena(arena, prog_index)

        if (len_trim(code) > 0) then
            print *, '  PASS: Program code generation'
            print *, '    Generated length:', len(code), 'characters'
        else
            print *, '  FAIL: Program code generation'
            test_program_codegen = .false.
        end if

        ! Clean up
        call arena%clear()

    end function test_program_codegen

    logical function test_identifier_codegen()
        type(ast_arena_t) :: arena
        integer :: id_index
        character(len=:), allocatable :: code

        test_identifier_codegen = .true.
        print *, 'Testing identifier code generation...'

        ! Create arena
        arena = create_ast_arena()

        ! Create identifier
        id_index = push_identifier(arena, 'variable_name', 1, 1)

        ! Generate code
        code = generate_code_from_arena(arena, id_index)

        if (len_trim(code) > 0) then
            print *, '  PASS: Identifier code generation'
            print *, '    Generated:', trim(code)
        else
            print *, '  FAIL: Identifier code generation'
            test_identifier_codegen = .false.
        end if

        ! Clean up
        call arena%clear()

    end function test_identifier_codegen

end program test_frontend_codegen_api
