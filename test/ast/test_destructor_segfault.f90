program test_destructor_segfault
    ! Test for issue #2617: Segfaults in AST node destructors
    ! This test exercises AST node creation and cleanup patterns
    ! that triggered segfaults in ffc when nodes went out of scope.
    !
    ! The issue manifests as SIGSEGV in GCC-generated finalizers when:
    ! 1. Local AST node variables go out of scope in factory functions
    ! 2. Arena entries are finalized during cleanup
    !
    ! This test validates that AST node lifecycle operations are safe.
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_factory_control, only: push_select_case, push_case_block
    use ast_factory_procedures, only: push_module, push_function_def, &
                                      push_subroutine_def
    use ast_factory_core, only: push_literal, push_identifier, push_assignment
    use ast_base, only: LITERAL_INTEGER
    implicit none

    integer :: pass_count, test_count

    pass_count = 0
    test_count = 0

    print *, "=== AST Destructor Segfault Tests (Issue #2617) ==="
    print *, ""

    call test_case_block_lifecycle()
    call test_module_node_lifecycle()
    call test_select_case_with_cases()
    call test_repeated_arena_operations()
    call test_module_with_procedures()
    call test_nested_scopes()

    print *, ""
    print *, "=== Summary ==="
    write (*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"

    if (pass_count == test_count) then
        print *, "All destructor tests passed!"
        stop 0
    else
        print *, "Some tests failed!"
        stop 1
    end if

contains

    subroutine test_case_block_lifecycle()
        ! Test case_block_node creation and cleanup
        type(ast_arena_t) :: arena
        integer :: case_idx, val_idx

        call start_test("case_block_node lifecycle")

        arena = create_ast_arena()
        val_idx = push_literal(arena, "1", LITERAL_INTEGER, line=1, column=1)
        case_idx = push_case_block(arena, [val_idx], line=1, column=1)

        if (case_idx > 0) then
            call pass_test()
        else
            call fail_test("Failed to create case_block_node")
        end if
        ! Arena goes out of scope here - tests finalization
    end subroutine test_case_block_lifecycle

    subroutine test_module_node_lifecycle()
        ! Test module_node creation and cleanup
        type(ast_arena_t) :: arena
        integer :: mod_idx

        call start_test("module_node lifecycle")

        arena = create_ast_arena()
        mod_idx = push_module(arena, "test_module", line=1, column=1)

        if (mod_idx > 0) then
            call pass_test()
        else
            call fail_test("Failed to create module_node")
        end if
        ! Arena goes out of scope here - tests finalization
    end subroutine test_module_node_lifecycle

    subroutine test_select_case_with_cases()
        ! Test select_case with multiple case blocks
        type(ast_arena_t) :: arena
        integer :: sel_idx, case1_idx, case2_idx, expr_idx, val1_idx, val2_idx

        call start_test("select_case with case blocks")

        arena = create_ast_arena()
        expr_idx = push_literal(arena, "42", LITERAL_INTEGER, line=1, column=1)
        val1_idx = push_literal(arena, "1", LITERAL_INTEGER, line=2, column=1)
        val2_idx = push_literal(arena, "2", LITERAL_INTEGER, line=3, column=1)
        case1_idx = push_case_block(arena, [val1_idx], line=2, column=1)
        case2_idx = push_case_block(arena, [val2_idx], line=3, column=1)
        sel_idx = push_select_case(arena, expr_idx, [case1_idx, case2_idx], &
                                   line=1, column=1)

        if (sel_idx > 0 .and. case1_idx > 0 .and. case2_idx > 0) then
            call pass_test()
        else
            call fail_test("Failed to create select_case construct")
        end if
        ! Arena goes out of scope here - tests finalization
    end subroutine test_select_case_with_cases

    subroutine test_repeated_arena_operations()
        ! Test repeated arena creation/destruction
        integer :: i

        call start_test("repeated arena operations (10 iterations)")

        do i = 1, 10
            block
                type(ast_arena_t) :: arena
                integer :: idx
                character(len=10) :: val_str
                arena = create_ast_arena()
                write (val_str, '(I0)') i
                idx = push_literal(arena, trim(val_str), LITERAL_INTEGER, &
                                   line=1, column=1)
                idx = push_case_block(arena, [idx], line=1, column=1)
                ! Arena finalized at end of block
            end block
        end do

        call pass_test()
    end subroutine test_repeated_arena_operations

    subroutine test_module_with_procedures()
        ! Test module with contained procedures (simulates ffc test_module_system)
        type(ast_arena_t) :: arena
        integer :: mod_idx, func_idx, sub_idx

        call start_test("module with procedures")

        arena = create_ast_arena()
        func_idx = push_function_def(arena, "my_func", line=2, column=1)
        sub_idx = push_subroutine_def(arena, "my_sub", line=5, column=1)
        mod_idx = push_module(arena, "my_module", &
                              body_indices=[func_idx, sub_idx], &
                              line=1, column=1)

        if (mod_idx > 0 .and. func_idx > 0 .and. sub_idx > 0) then
            call pass_test()
        else
            call fail_test("Failed to create module with procedures")
        end if
        ! Arena goes out of scope here - tests finalization
    end subroutine test_module_with_procedures

    subroutine test_nested_scopes()
        ! Test nested scope patterns (simulates complex AST structures)
        integer :: i

        call start_test("nested scopes (5 iterations)")

        do i = 1, 5
            block
                type(ast_arena_t) :: outer_arena
                integer :: outer_idx
                outer_arena = create_ast_arena()
                outer_idx = push_module(outer_arena, "outer_mod", line=1, column=1)
                block
                    type(ast_arena_t) :: inner_arena
                    integer :: inner_idx, case_idx, val_idx
                    inner_arena = create_ast_arena()
                    val_idx = push_literal(inner_arena, "1", LITERAL_INTEGER, &
                                           line=1, column=1)
                    case_idx = push_case_block(inner_arena, [val_idx], &
                                               line=1, column=1)
                    inner_idx = push_module(inner_arena, "inner_mod", line=1, column=1)
                    ! Inner arena finalized here
                end block
                ! Outer arena still valid here
                if (outer_idx <= 0) then
                    call fail_test("Nested scope test failed")
                    return
                end if
                ! Outer arena finalized here
            end block
        end do

        call pass_test()
    end subroutine test_nested_scopes

    subroutine start_test(name)
        character(len=*), intent(in) :: name
        test_count = test_count + 1
        write (*, '(A,": ")', advance='no') name
    end subroutine start_test

    subroutine pass_test()
        print *, "PASS"
        pass_count = pass_count + 1
    end subroutine pass_test

    subroutine fail_test(reason)
        character(len=*), intent(in) :: reason
        write (*, '(A,": ",A)') "FAIL", reason
    end subroutine fail_test

end program test_destructor_segfault
