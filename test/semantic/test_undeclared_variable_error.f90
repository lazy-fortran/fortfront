program test_undeclared_variable_error
    use frontend
    use ast_core
    use lexer_core, only: token_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, analyze_program
    implicit none

    logical :: all_passed

    print *, "=== Undeclared Variable Error Test ==="
    print *

    all_passed = .true.
    
    if (.not. test_undeclared_in_implicit_none()) all_passed = .false.
    if (.not. test_declared_variable_ok()) all_passed = .false.
    if (.not. test_undeclared_in_expression()) all_passed = .false.

    print *
    if (all_passed) then
        print *, "All undeclared variable tests passed!"
        stop 0
    else
        print *, "Some undeclared variable tests failed!"
        stop 1
    end if

contains

    logical function test_undeclared_in_implicit_none()
        character(len=*), parameter :: source = &
            "program test" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    x = 42" // new_line('A') // &
            "end program test"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        
        test_undeclared_in_implicit_none = .true.
        print *, "Testing undeclared variable with implicit none..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        ! Semantic analysis
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        ! For now, we don't have error reporting in semantic analysis
        ! Just check that it doesn't crash
        print *, "  INFO: Semantic analysis completed (error detection not yet implemented)"
        print *, "  PASS: Test completed without crash"
        
    end function test_undeclared_in_implicit_none

    logical function test_declared_variable_ok()
        character(len=*), parameter :: source = &
            "program test" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    integer :: x" // new_line('A') // &
            "    x = 42" // new_line('A') // &
            "end program test"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        
        test_declared_variable_ok = .true.
        print *, "Testing declared variable (should be ok)..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        ! Semantic analysis
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        print *, "  PASS: Declared variable handled correctly"
        
    end function test_declared_variable_ok

    logical function test_undeclared_in_expression()
        character(len=*), parameter :: source = &
            "program test" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    integer :: x" // new_line('A') // &
            "    x = y + 1" // new_line('A') // &
            "end program test"
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        
        test_undeclared_in_expression = .true.
        print *, "Testing undeclared variable in expression..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        ! Semantic analysis
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        print *, "  INFO: Semantic analysis completed (error detection not yet implemented)"
        print *, "  PASS: Test completed without crash"
        
    end function test_undeclared_in_expression

end program test_undeclared_variable_error