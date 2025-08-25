program test_codegen_core_direct
    use codegen_core, only: generate_code_from_arena, generate_code_polymorphic
    use ast_core, only: ast_arena_t, create_ast_arena, create_identifier, &
                        create_literal, create_program, identifier_node, &
                        literal_node, LITERAL_INTEGER, LITERAL_STRING
    implicit none

    integer :: total_tests, passed_tests
    type(ast_arena_t) :: arena
    type(identifier_node) :: id_node
    type(literal_node) :: lit_node
    character(len=:), allocatable :: code
    integer :: node_index

    total_tests = 0
    passed_tests = 0

    print *, "=== Codegen Core Direct Function Tests ==="
    print *, ""

    ! Test 1: Generate code for identifier
    call test_start("Generate identifier code")
    arena = create_ast_arena()
    id_node = create_identifier("test_var", 1, 1)
    call arena%push(id_node, "identifier")
    node_index = arena%size
    code = generate_code_from_arena(arena, node_index)
    if (len_trim(code) > 0 .and. index(code, "test_var") > 0) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: non-empty code containing 'test_var'"
        print *, "  Got: '", code, "'"
    end if

    ! Test 2: Generate code for integer literal
    call test_start("Generate integer literal code")
    arena = create_ast_arena()
    lit_node = create_literal("42", LITERAL_INTEGER, 1, 1)
    call arena%push(lit_node, "literal")
    node_index = arena%size
    code = generate_code_from_arena(arena, node_index)
    if (len_trim(code) > 0 .and. index(code, "42") > 0) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: non-empty code containing '42'"
        print *, "  Got: '", code, "'"
    end if

    ! Test 3: Generate code for string literal
    call test_start("Generate string literal code")
    arena = create_ast_arena()
    lit_node = create_literal("'hello'", LITERAL_STRING, 1, 1)
    call arena%push(lit_node, "literal")
    node_index = arena%size
    code = generate_code_from_arena(arena, node_index)
    if (len_trim(code) > 0 .and. index(code, "hello") > 0) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: non-empty code containing 'hello'"
        print *, "  Got: '", code, "'"
    end if

    ! Test 4: Generate code for empty program
    call test_start("Generate empty program code")
    arena = create_ast_arena()
    call arena%push(create_program("test_prog", [integer::]), "program")
    node_index = arena%size
    code = generate_code_from_arena(arena, node_index)
    if (len_trim(code) > 0 .and. index(code, "program") > 0) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: non-empty code containing 'program'"
        print *, "  Got: '", code, "'"
    end if

    ! Test 5: Invalid node index handling
    call test_start("Invalid node index")
    arena = create_ast_arena()
    code = generate_code_from_arena(arena, 999)  ! Invalid index
    if (len_trim(code) == 0) then  ! Should return empty string
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: empty code for invalid index"
        print *, "  Got: '", code, "'"
    end if

    ! Test 6: Empty arena handling
    call test_start("Empty arena")
    arena = create_ast_arena()
    code = generate_code_from_arena(arena, 1)  ! No nodes in arena
    if (len_trim(code) == 0) then  ! Should return empty string
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: empty code for empty arena"
        print *, "  Got: '", code, "'"
    end if

    ! Test 7: Multiple nodes in arena
    call test_start("Multiple nodes in arena")
    arena = create_ast_arena()
    call arena%push(create_identifier("x"), "identifier")
    call arena%push(create_literal("5", LITERAL_INTEGER), "literal")
    
    ! Generate code for first node
    code = generate_code_from_arena(arena, 1)
    if (len_trim(code) > 0 .and. index(code, "x") > 0) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: code for first node containing 'x'"
        print *, "  Got: '", code, "'"
    end if

    ! Test 8: Generate code for second node
    call test_start("Second node in arena")
    code = generate_code_from_arena(arena, 2)
    if (len_trim(code) > 0 .and. index(code, "5") > 0) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: code for second node containing '5'"
        print *, "  Got: '", code, "'"
    end if

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (passed_tests == total_tests) then
        print *, "All codegen core direct tests passed!"
    else
        print *, "Some codegen core tests failed!"
        error stop 1
    end if

contains

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_pass()
        print *, " ... PASSED"
        passed_tests = passed_tests + 1
    end subroutine test_pass

    subroutine test_fail()
        print *, " ... FAILED"
    end subroutine test_fail

end program test_codegen_core_direct