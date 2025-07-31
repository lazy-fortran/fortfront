program test_codegen_basic
    use ast_core
    use ast_factory
    use codegen_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run basic code generation tests
    if (.not. test_literal_codegen()) all_passed = .false.
    if (.not. test_identifier_codegen()) all_passed = .false.
    if (.not. test_assignment_codegen()) all_passed = .false.
    if (.not. test_binary_op_codegen()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All basic code generation tests passed"
        stop 0
    else
        print '(a)', "Some basic code generation tests failed"
        stop 1
    end if

contains

    logical function test_literal_codegen()
        type(ast_arena_t) :: arena
        integer :: lit_index
        character(len=:), allocatable :: code

        test_literal_codegen = .true.
        print '(a)', "Testing literal code generation..."

        ! Create arena
        arena = create_ast_arena()

        ! Create integer literal
        lit_index = push_literal(arena, "42", LITERAL_INTEGER, 1, 1)

        ! Generate code
        code = generate_code_from_arena(arena, lit_index)

        ! Check generated code
        if (code /= "42") then
            print '(a)', "FAIL: Integer literal code generation incorrect"
            print '(a)', "  Expected: '42'"
            print '(a)', "  Got: '" // code // "'"
            test_literal_codegen = .false.
        else
            print '(a)', "PASS: Literal code generation"
        end if
    end function test_literal_codegen

    logical function test_identifier_codegen()
        type(ast_arena_t) :: arena
        integer :: id_index
        character(len=:), allocatable :: code

        test_identifier_codegen = .true.
        print '(a)', "Testing identifier code generation..."

        ! Create arena
        arena = create_ast_arena()

        ! Create identifier
        id_index = push_identifier(arena, "x", 1, 1)

        ! Generate code
        code = generate_code_from_arena(arena, id_index)

        ! Check generated code
        if (code /= "x") then
            print '(a)', "FAIL: Identifier code generation incorrect"
            print '(a)', "  Expected: 'x'"
            print '(a)', "  Got: '" // code // "'"
            test_identifier_codegen = .false.
        else
            print '(a)', "PASS: Identifier code generation"
        end if
    end function test_identifier_codegen

    logical function test_assignment_codegen()
        type(ast_arena_t) :: arena
        integer :: target_index, value_index, assign_index
        character(len=:), allocatable :: code

        test_assignment_codegen = .true.
        print '(a)', "Testing assignment code generation..."

        ! Create arena
        arena = create_ast_arena()

        ! Create assignment: x = 42
        target_index = push_identifier(arena, "x", 1, 1)
        value_index = push_literal(arena, "42", LITERAL_INTEGER, 1, 5)
        assign_index = push_assignment(arena, target_index, value_index, 1, 1)

        ! Generate code
        code = generate_code_from_arena(arena, assign_index)

        ! Check generated code
        if (code /= "x = 42") then
            print '(a)', "FAIL: Assignment code generation incorrect"
            print '(a)', "  Expected: 'x = 42'"
            print '(a)', "  Got: '" // code // "'"
            test_assignment_codegen = .false.
        else
            print '(a)', "PASS: Assignment code generation"
        end if
    end function test_assignment_codegen

    logical function test_binary_op_codegen()
        type(ast_arena_t) :: arena
        integer :: left_index, right_index, binop_index
        character(len=:), allocatable :: code

        test_binary_op_codegen = .true.
        print '(a)', "Testing binary operation code generation..."

        ! Create arena
        arena = create_ast_arena()

        ! Create binary operation: a + b
        left_index = push_identifier(arena, "a", 1, 1)
        right_index = push_identifier(arena, "b", 1, 5)
        binop_index = push_binary_op(arena, left_index, right_index, "+", 1, 3)

        ! Generate code
        code = generate_code_from_arena(arena, binop_index)

        ! Check generated code
        if (code /= "a + b") then
            print '(a)', "FAIL: Binary operation code generation incorrect"
            print '(a)', "  Expected: 'a + b'"
            print '(a)', "  Got: '" // code // "'"
            test_binary_op_codegen = .false.
        else
            print '(a)', "PASS: Binary operation code generation"
        end if
    end function test_binary_op_codegen

end program test_codegen_basic
