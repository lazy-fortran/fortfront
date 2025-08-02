program test_semantic_query_api
    use semantic_query_api
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                 analyze_program
    use ast_core
    use frontend, only: lex_source, parse_tokens
    use lexer_core, only: token_t
    implicit none

    logical :: all_passed = .true.

    print *, "Testing semantic query API..."

    if (.not. test_basic_functionality()) all_passed = .false.

    if (all_passed) then
        print *, "All semantic query API tests passed!"
        stop 0
    else
        print *, "Some semantic query API tests failed!"
        stop 1
    end if

contains

    logical function test_basic_functionality()
        character(len=*), parameter :: source = "x = 1"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        type(semantic_context_t) :: ctx
        type(semantic_query_t) :: query
        character(len=:), allocatable :: error_msg

        test_basic_functionality = .true.
        print *, "Testing basic semantic query functionality..."

        ! Tokenize
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "FAIL: Tokenization failed: ", error_msg
            test_basic_functionality = .false.
            return
        end if

        ! Parse
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "FAIL: Parsing failed: ", error_msg
            test_basic_functionality = .false.
            return
        end if

        ! Semantic analysis
        ctx = create_semantic_context()
        call analyze_program(ctx, arena, prog_index)

        ! Create query interface
        query = create_semantic_query(arena, ctx)

        ! Test that query interface was created successfully
        ! For now, just verify the pointers are associated
        if (.not. associated(query%arena)) then
            print *, "FAIL: Query arena pointer not associated"
            test_basic_functionality = .false.
            return
        end if

        if (.not. associated(query%context)) then
            print *, "FAIL: Query context pointer not associated"
            test_basic_functionality = .false.
            return
        end if

        print *, "PASS: Basic semantic query functionality"
    end function test_basic_functionality

end program test_semantic_query_api