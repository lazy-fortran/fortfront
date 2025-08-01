program test_semantic_simple
    use frontend
    use ast_core
    use lexer_core, only: token_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, analyze_program
    implicit none

    logical :: all_passed
    all_passed = .true.

    print *, "Testing semantic analyzer..."

    if (.not. test_simple_assignment()) all_passed = .false.

    if (all_passed) then
        print *, "All semantic analyzer tests passed"
        stop 0
    else
        print *, "Some semantic analyzer tests failed"
        stop 1
    end if

contains

    logical function test_simple_assignment()
        character(len=*), parameter :: source = "x = 1"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        type(semantic_context_t) :: sem_ctx
        character(len=:), allocatable :: error_msg

        test_simple_assignment = .true.
        print *, "Testing simple assignment semantic analysis..."

        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "FAIL: Tokenization failed: ", error_msg
                test_simple_assignment = .false.
                return
            end if
        end if

        print *, "PASS: Tokenization completed"

        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "FAIL: Parsing failed: ", error_msg
                test_simple_assignment = .false.
                return
            end if
        end if

        print *, "PASS: Parsing completed"

        ! Semantic analysis
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        print *, "PASS: Semantic analysis completed without segfault"

    end function test_simple_assignment

end program test_semantic_simple
