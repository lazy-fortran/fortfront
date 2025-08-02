program test_semantic_query_api
    use semantic_query_api
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                 analyze_program
    use ast_core
    use frontend, only: lex_source, parse_tokens
    use lexer_core, only: token_t
    use scope_manager, only: SCOPE_GLOBAL, SCOPE_FUNCTION
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
        type(semantic_context_t) :: ctx
        type(semantic_query_t) :: query
        integer :: root_index
        character(len=:), allocatable :: error_msg
        logical :: defined
        
        test_basic_functionality = .true.
        print *, "Testing basic semantic query functionality..."

        ! Initialize arena and context
        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        ! Lex, parse, and analyze
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            error stop "Lexing failed: " // error_msg
        end if
        
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            error stop "Parsing failed: " // error_msg
        end if
        
        call analyze_program(ctx, arena, root_index)
        
        ! Create query interface - NOTE: We need arena and ctx to remain in scope
        query = create_semantic_query(arena, ctx)

        ! Test that query interface was created successfully
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
        
        ! Test simple symbol definition check for intrinsic
        defined = query%is_symbol_defined("sin")
        if (.not. defined) then
            print *, "INFO: Symbol 'sin' not found (this is expected for simple semantic context)"
        else
            print *, "INFO: Symbol 'sin' found as intrinsic"
        end if

        print *, "  Basic functionality: PASS"
    end function test_basic_functionality

end program test_semantic_query_api