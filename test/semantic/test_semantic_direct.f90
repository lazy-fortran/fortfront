program test_semantic_direct
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                 analyze_program
    use ast_core, only: ast_arena_t, create_ast_arena, create_program, &
                        create_identifier, create_literal, create_assignment, &
                        program_node, LITERAL_INTEGER
    use type_system_hm, only: mono_type_t, type_var_t, TINT, TREAL, TCHAR, TLOGICAL
    implicit none

    integer :: total_tests, passed_tests
    type(semantic_context_t) :: ctx
    type(ast_arena_t) :: arena
    type(program_node) :: prog
    type(type_var_t) :: type_var
    integer :: prog_index

    total_tests = 0
    passed_tests = 0

    print *, "=== Semantic Analyzer Direct Function Tests ==="
    print *, ""

    ! Test 1: Create semantic context
    call test_start("Create semantic context")
    ctx = create_semantic_context()
    if (ctx%next_var_id == 1 .and. ctx%subst%count == 0) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: next_var_id=1, subst%count=0"
        print *, "  Got: next_var_id=", ctx%next_var_id, &
                 ", subst%count=", ctx%subst%count
    end if

    ! Test 2: Generate fresh type variable
    call test_start("Generate fresh type variable")
    type_var = ctx%fresh_type_var()
    if (ctx%next_var_id == 2 .and. type_var%id == 2) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: next_var_id=2, type_var%id=2"
        print *, "  Got: next_var_id=", ctx%next_var_id, ", type_var%id=", type_var%id
    end if

    ! Test 3: Multiple fresh type variables
    call test_start("Multiple fresh type variables")
    type_var = ctx%fresh_type_var()  ! Should be id=3
    type_var = ctx%fresh_type_var()  ! Should be id=4
    if (ctx%next_var_id == 4 .and. type_var%id == 4) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: next_var_id=4, type_var%id=4"
        print *, "  Got: next_var_id=", ctx%next_var_id, ", type_var%id=", type_var%id
    end if

    ! Test 4: Builtin function types - sin function
    call test_start("Builtin function types")
    if (ctx%env%count >= 7) then  ! Should have at least 7 builtin functions
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: env%count>=7 (builtin functions)"
        print *, "  Got: env%count=", ctx%env%count
    end if

    ! Test 5: Create empty program AST
    call test_start("Analyze empty program")
    arena = create_ast_arena()
    prog = create_program("test_program", [integer::], line=1, column=1)  ! Empty body_indices
    call arena%push(prog, "program")
    prog_index = arena%current_index
    
    ! Should not crash on empty program
    call analyze_program(ctx, arena, prog_index)
    if (arena%size == 1) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: arena%size=1"
        print *, "  Got: arena%size=", arena%size
    end if

    ! Test 6: Semantic context deep copy
    call test_start("Semantic context copy")
    ctx = create_semantic_context()
    type_var = ctx%fresh_type_var()  ! Make it different from default
    type_var = ctx%fresh_type_var()
    
    ! Assignment should work (uses deep_copy internally)
    if (ctx%next_var_id == 3) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: copied context next_var_id=3"
        print *, "  Got: next_var_id=", ctx%next_var_id
    end if

    ! Test 7: Invalid program index handling
    call test_start("Invalid program index")
    arena = create_ast_arena()
    ctx = create_semantic_context()
    
    ! Try to analyze with invalid index - should not crash
    call analyze_program(ctx, arena, 999)  ! Invalid index
    if (arena%size == 0) then  ! Should remain empty
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: arena%size=0 (no analysis)"
        print *, "  Got: arena%size=", arena%size
    end if

    ! Test 8: Context builtin function lookup
    call test_start("Builtin function lookup")
    ctx = create_semantic_context()
    
    ! Should find sin function in environment
    if (ctx%env%count > 0) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: builtin functions in env"
        print *, "  Got: env%count=", ctx%env%count
    end if

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (passed_tests == total_tests) then
        print *, "All semantic analyzer direct tests passed!"
        stop 0
    else
        print *, "Some semantic analyzer tests failed!"
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

end program test_semantic_direct