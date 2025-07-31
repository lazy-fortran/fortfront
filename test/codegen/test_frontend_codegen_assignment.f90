program test_frontend_codegen_assignment
    use ast_core
    use ast_factory
    use codegen_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run assignment code generation tests
    if (.not. test_simple_assignment_codegen()) all_passed = .false.
    if (.not. test_complex_assignment_codegen()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All codegen assignment tests passed"
        stop 0
    else
        print '(a)', "Some codegen assignment tests failed"
        stop 1
    end if

contains

    logical function test_simple_assignment_codegen()
        type(ast_arena_t) :: arena
        integer :: target_index, value_index, assign_index
        character(len=:), allocatable :: code

        test_simple_assignment_codegen = .true.
        print '(a)', "Testing simple assignment code generation..."

        ! Create arena
        arena = create_ast_arena()

        ! Create assignment: x = 1
        target_index = push_identifier(arena, "x", 1, 1)
        value_index = push_literal(arena, "1", LITERAL_INTEGER, 1, 5)
        assign_index = push_assignment(arena, target_index, value_index, 1, 1)

        ! Generate code
        code = generate_code_from_arena(arena, assign_index)

        ! Check generated code
        if (code /= "x = 1") then
            print '(a)', "FAIL: Simple assignment code generation incorrect"
            print '(a)', "  Expected: 'x = 1'"
            print '(a)', "  Got: '" // code // "'"
            test_simple_assignment_codegen = .false.
        else
            print '(a)', "PASS: Simple assignment code generation"
        end if
    end function test_simple_assignment_codegen

    logical function test_complex_assignment_codegen()
        type(ast_arena_t) :: arena
        integer :: target_index, left_index, right_index, binop_index, assign_index
        character(len=:), allocatable :: code

        test_complex_assignment_codegen = .true.
        print '(a)', "Testing complex assignment code generation..."

        ! Create arena
        arena = create_ast_arena()

        ! Create assignment: result = a + b
        target_index = push_identifier(arena, "result", 1, 1)
        left_index = push_identifier(arena, "a", 1, 10)
        right_index = push_identifier(arena, "b", 1, 14)
        binop_index = push_binary_op(arena, left_index, right_index, "+", 1, 12)
        assign_index = push_assignment(arena, target_index, binop_index, 1, 1)

        ! Generate code
        code = generate_code_from_arena(arena, assign_index)

        ! Check generated code
        if (code /= "result = a + b") then
            print '(a)', "FAIL: Complex assignment code generation incorrect"
            print '(a)', "  Expected: 'result = a + b'"
            print '(a)', "  Got: '" // code // "'"
            test_complex_assignment_codegen = .false.
        else
            print '(a)', "PASS: Complex assignment code generation"
        end if
    end function test_complex_assignment_codegen

end program test_frontend_codegen_assignment
