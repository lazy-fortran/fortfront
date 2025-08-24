program test_ast_core_direct
    ! Modernized test using modern AST arena API instead of deprecated ast_core
    use ast_arena_modern, only: ast_arena_t, create_ast_arena, ast_arena_stats_t
    use ast_types, only: create_identifier, create_literal, identifier_node, literal_node, &
                        LITERAL_INTEGER, LITERAL_STRING
    implicit none

    integer :: total_tests, passed_tests
    type(ast_arena_t) :: arena
    type(ast_arena_stats_t) :: stats
    type(identifier_node) :: id_node
    type(literal_node) :: lit_node

    total_tests = 0
    passed_tests = 0

    print *, "=== AST Core Direct Function Tests (Modern Arena API) ==="
    print *, ""

    ! Test 1: Create AST arena with default capacity
    call test_start("Create AST arena default")
    arena = create_ast_arena()
    stats = arena%get_stats()
    if (arena%size == 0 .and. stats%capacity >= 256) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: size=0, capacity>=256"
        print *, "  Got: size=", arena%size, ", capacity=", stats%capacity
    end if

    ! Test 2: Create AST arena with custom capacity
    call test_start("Create AST arena custom")
    arena = create_ast_arena(512)
    stats = arena%get_stats()
    if (arena%size == 0 .and. stats%capacity >= 512) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: size=0, capacity>=512"
        print *, "  Got: size=", arena%size, ", capacity=", stats%capacity
    end if

    ! Test 3: Create identifier node
    call test_start("Create identifier node")
    id_node = create_identifier("test_var", 10, 5)
    if (id_node%name == "test_var" .and. id_node%line == 10 .and. &
        id_node%column == 5) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: name='test_var', line=10, column=5"
        print *, "  Got: name='", id_node%name, "', line=", id_node%line, &
                 ", column=", id_node%column
    end if

    ! Test 4: Create identifier node with default position
    call test_start("Create identifier default position")
    id_node = create_identifier("var2")
    if (id_node%name == "var2" .and. id_node%line == 1 .and. &
        id_node%column == 1) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: name='var2', line=1, column=1"
        print *, "  Got: name='", id_node%name, "', line=", id_node%line, &
                 ", column=", id_node%column
    end if

    ! Test 5: Create integer literal node
    call test_start("Create integer literal")
    lit_node = create_literal("42", LITERAL_INTEGER, 5, 10)
    if (lit_node%value == "42" .and. lit_node%literal_kind == LITERAL_INTEGER .and. &
        lit_node%line == 5 .and. lit_node%column == 10) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: value='42', literal_kind=", LITERAL_INTEGER, &
                 ", line=5, column=10"
        print *, "  Got: value='", lit_node%value, "', literal_kind=", lit_node%literal_kind, &
                 ", line=", lit_node%line, ", column=", lit_node%column
    end if

    ! Test 6: Create string literal node
    call test_start("Create string literal")
    lit_node = create_literal("hello", LITERAL_STRING)
    if (lit_node%value == "hello" .and. lit_node%literal_kind == LITERAL_STRING .and. &
        lit_node%line == 1 .and. lit_node%column == 1) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: value='hello', literal_kind=", LITERAL_STRING, &
                 ", line=1, column=1"
        print *, "  Got: value='", lit_node%value, "', literal_kind=", lit_node%literal_kind, &
                 ", line=", lit_node%line, ", column=", lit_node%column
    end if

    ! Test 7: Push node to arena
    call test_start("Push node to arena")
    arena = create_ast_arena()
    call arena%push(id_node, "identifier")
    ! Modern API: size is the current count, equivalent to old current_index
    if (arena%size == 1) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: size=1"
        print *, "  Got: size=", arena%size
    end if

    ! Test 8: Push multiple nodes
    call test_start("Push multiple nodes")
    call arena%push(lit_node, "literal")
    if (arena%size == 2) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: size=2"
        print *, "  Got: size=", arena%size
    end if

    ! Test 9: Get arena depth
    call test_start("Get arena depth")
    if (arena%max_depth == 0) then  ! All nodes at root level so far
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: depth=0"
        print *, "  Got: depth=", arena%max_depth
    end if

    ! Test 10: Arena statistics
    call test_start("Arena statistics")
    stats = arena%get_stats()
    if (stats%total_nodes == 2 .and. stats%max_depth == 0 .and. &
        stats%capacity >= 256) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: total_nodes=2, max_depth=0, capacity>=256"
        print *, "  Got: total_nodes=", stats%total_nodes, &
                 ", max_depth=", stats%max_depth, &
                 ", capacity=", stats%capacity
    end if

    ! Test 11: Arena clear functionality
    call test_start("Arena clear")
    call arena%clear()
    ! Modern API: after clear, size is 0 and max_depth is reset
    if (arena%size == 0 .and. arena%max_depth == 0) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: size=0, max_depth=0"
        print *, "  Got: size=", arena%size, ", max_depth=", arena%max_depth
    end if

    ! Test 12: Arena capacity growth
    call test_start("Arena capacity growth")
    arena = create_ast_arena(2)  ! Small initial capacity
    ! Push more nodes than initial capacity
    call arena%push(create_identifier("a"), "identifier")
    call arena%push(create_identifier("b"), "identifier")
    call arena%push(create_identifier("c"), "identifier")
    stats = arena%get_stats()
    if (arena%size == 3 .and. stats%capacity >= 3) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: size=3, capacity>=3"
        print *, "  Got: size=", arena%size, ", capacity=", stats%capacity
    end if

    ! Test 13: Node hierarchy (parent-child)
    call test_start("Parent-child relationship")
    call arena%clear()
    call arena%push(create_identifier("parent"), "identifier")  ! Index 1
    call arena%push(create_identifier("child"), "identifier", 1)  ! Index 2, parent=1
    if (arena%size == 2 .and. arena%max_depth == 1) then  ! Depth should be 1 now
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: size=2, depth=1"
        print *, "  Got: size=", arena%size, ", depth=", arena%max_depth
    end if

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (passed_tests == total_tests) then
        print *, "All modernized AST core tests passed!"
        stop 0
    else
        print *, "Some modernized AST core tests failed!"
        stop 1
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

end program test_ast_core_direct