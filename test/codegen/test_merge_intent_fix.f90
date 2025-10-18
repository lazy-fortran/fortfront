program test_merge_intent_fix
    use codegen_utilities, only: generate_grouped_body
    use codegen_core, only: initialize_codegen
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_data, only: create_declaration
    implicit none

    integer :: total, passed
    type(ast_arena_t) :: arena
    integer, allocatable :: body(:)
    character(len=:), allocatable :: code

    total = 0
    passed = 0

    call initialize_codegen()

    call test_start("Grouped decl with no intent (MERGE length)")
    arena = create_ast_arena()
    ! Two declarations with same type and no intent
    call arena%push(create_declaration("real", "a"), "declaration")
    call arena%push(create_declaration("real", "b"), "declaration")
    body = [1, 2]
    code = generate_grouped_body(arena, body, 0)
    if (index(code, "real :: a, b") > 0 .or. &
        (index(code, "real :: a") > 0 .and. index(code, "real :: b") > 0)) then
        call test_pass()
    else
        call test_fail()
        print *, "Expected grouped 'real :: a, b' or separate lines"
        print *, "Got: ", trim(code)
    end if

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0,A,I0)') "Passed ", passed, "/", total
    if (passed /= total) error stop 1

contains
    subroutine test_start(name)
        character(len=*), intent(in) :: name
        total = total + 1
        write (*, '(A,A)', advance='no') "Testing: ", name
    end subroutine test_start

    subroutine test_pass()
        print *, " ... PASSED"
        passed = passed + 1
    end subroutine test_pass

    subroutine test_fail()
        print *, " ... FAILED"
    end subroutine test_fail

end program test_merge_intent_fix
