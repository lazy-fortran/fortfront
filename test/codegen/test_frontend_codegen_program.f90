program test_codegen_program
    use ast_core
    use ast_factory
    use codegen_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run program code generation tests
    if (.not. test_statement_generation()) all_passed = .false.
    if (.not. test_use_statement()) all_passed = .false.
    if (.not. test_print_statement()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All program code generation tests passed"
        stop 0
    else
        print '(a)', "Some program code generation tests failed"
        stop 1
    end if

contains

    logical function test_statement_generation()
        type(ast_arena_t) :: arena
        integer :: target_idx, x_idx, y_idx, expr_idx, assign_idx
        character(len=:), allocatable :: code

        test_statement_generation = .true.
        print '(a)', "Testing statement code generation..."

        ! Initialize arena
        arena = create_ast_stack()

        ! Create assignment with expression: z = x + y
        target_idx = push_identifier(arena, "z", 1, 1)
        x_idx = push_identifier(arena, "x", 1, 5)
        y_idx = push_identifier(arena, "y", 1, 9)
        expr_idx = push_binary_op(arena, x_idx, y_idx, "+", 1, 7)
        assign_idx = push_assignment(arena, target_idx, expr_idx, 1, 1)

        ! Generate code
        code = generate_code_from_arena(arena, assign_idx)

        ! Check generated code
        if (code /= "z = x + y") then
            print '(a)', "FAIL: Statement generation incorrect"
            print '(a)', "  Expected: 'z = x + y'"
            print '(a)', "  Got: '" // code // "'"
            test_statement_generation = .false.
        else
            print '(a)', "PASS: Statement generation"
        end if

        ! Clean up
        call arena%clear()
    end function test_statement_generation

    logical function test_use_statement()
        type(ast_arena_t) :: arena
        integer :: use_idx
        character(len=:), allocatable :: code

        test_use_statement = .true.
        print '(a)', "Testing use statement generation..."

        ! Initialize arena
        arena = create_ast_stack()

        ! Create use statement
        use_idx = push_use_statement(arena, "iso_fortran_env", line=1, column=1)

        ! Generate code
        code = generate_code_from_arena(arena, use_idx)

        ! Check generated code
        if (code /= "use iso_fortran_env") then
            print '(a)', "FAIL: Use statement generation incorrect"
            print '(a)', "  Expected: 'use iso_fortran_env'"
            print '(a)', "  Got: '" // code // "'"
            test_use_statement = .false.
        else
            print '(a)', "PASS: Use statement generation"
        end if

        ! Clean up
        call arena%clear()
    end function test_use_statement

    logical function test_print_statement()
        type(ast_arena_t) :: arena
        integer :: var_idx, print_idx
        integer :: arg_indices(1)
        character(len=:), allocatable :: code

        test_print_statement = .true.
        print '(a)', "Testing print statement generation..."

        ! Initialize arena
        arena = create_ast_stack()

        ! Create print statement with one argument
        var_idx = push_identifier(arena, "result", 1, 7)
        arg_indices(1) = var_idx
        print_idx = push_print_statement(arena, "*", arg_indices, line=1, column=1)

        ! Generate code
        code = generate_code_from_arena(arena, print_idx)

        ! Check generated code
        if (code /= "print *, result") then
            print '(a)', "FAIL: Print statement generation incorrect"
            print '(a)', "  Expected: 'print *, result'"
            print '(a)', "  Got: '" // code // "'"
            test_print_statement = .false.
        else
            print '(a)', "PASS: Print statement generation"
        end if

        ! Clean up
        call arena%clear()
    end function test_print_statement

end program test_codegen_program
