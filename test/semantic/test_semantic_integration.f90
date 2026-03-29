program test_semantic_integration
    use fortfront, only: semantic_analyzer_t, semantic_context_t, &
                         create_semantic_context, &
                         lex_source, parse_tokens, &
                         create_ast_arena, token_t, ast_arena_t, &
                         analyze_program
    use semantic_analyzer, only: has_semantic_errors
    use semantic_input_mode, only: INPUT_MODE_LAZY
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    print *, "=== Semantic Integration Test ==="

    call test_types_accessible_through_api()
    call test_semantic_analysis_on_parsed_ast()
    call test_semantic_analysis_empty_arena()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", &
        test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_types_accessible_through_api()
        type(semantic_context_t) :: ctx
        type(ast_arena_t) :: arena

        test_count = test_count + 1

        call create_semantic_context(ctx)
        arena = create_ast_arena()

        if (ctx%next_var_id < 1) then
            write (*, '(A)') "FAIL: semantic context next_var_id not initialized"
            return
        end if
        if (arena%size /= 0) then
            write (*, '(A)') "FAIL: fresh arena should have size 0"
            return
        end if

        pass_count = pass_count + 1
        write (*, '(A)') "PASS: semantic pipeline types accessible through fortfront API"
    end subroutine test_types_accessible_through_api

    subroutine test_semantic_analysis_on_parsed_ast()
        type(semantic_context_t), allocatable :: ctx
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source, error_msg
        integer :: prog_index

        test_count = test_count + 1

        source = &
            'x = 5' // new_line('a') // &
            'y = x + 3'

        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            write (*, '(A,A)') "FAIL: lex error: ", error_msg
            return
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (len_trim(error_msg) > 0) then
            write (*, '(A,A)') "FAIL: parse error: ", error_msg
            return
        end if

        if (prog_index < 1) then
            write (*, '(A)') "FAIL: parse returned invalid prog_index"
            return
        end if

        allocate (ctx)
        call create_semantic_context(ctx)
        ctx%input_mode = INPUT_MODE_LAZY
        call analyze_program(ctx, arena, prog_index)

        if (has_semantic_errors(ctx)) then
            write (*, '(A)') "FAIL: semantic analysis reported errors on valid program"
            return
        end if

        pass_count = pass_count + 1
        write (*, '(A)') "PASS: semantic analysis completes without errors on parsed AST"
    end subroutine test_semantic_analysis_on_parsed_ast

    subroutine test_semantic_analysis_empty_arena()
        type(semantic_context_t), allocatable :: ctx
        type(ast_arena_t) :: arena

        test_count = test_count + 1

        arena = create_ast_arena()
        allocate (ctx)
        call create_semantic_context(ctx)

        call analyze_program(ctx, arena, 0)

        if (has_semantic_errors(ctx)) then
            write (*, '(A)') "FAIL: empty arena should not produce semantic errors"
            return
        end if

        pass_count = pass_count + 1
        write (*, '(A)') "PASS: semantic analysis handles empty arena gracefully"
    end subroutine test_semantic_analysis_empty_arena

end program test_semantic_integration
