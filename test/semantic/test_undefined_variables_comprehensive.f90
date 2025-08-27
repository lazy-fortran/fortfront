program test_undefined_variables_comprehensive
    use frontend
    use ast_core
    use lexer_core, only: token_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, analyze_program, &
                                 has_semantic_errors
    implicit none

    logical :: all_passed
    integer :: test_count = 0
    integer :: passed_count = 0

    print *, "=== Comprehensive Undefined Variable Detection Tests ==="
    print *

    all_passed = .true.
    
    ! Test multiple scenarios
    call test_scenario("Undefined in binary expression", &
        "program test" // new_line('A') // &
        "    integer :: x" // new_line('A') // &
        "    x = y + 1" // new_line('A') // &
        "end program test", .true.)
        
    call test_scenario("Undefined in assignment target", &
        "program test" // new_line('A') // &
        "    y = 42" // new_line('A') // &
        "end program test", .true.)
        
    call test_scenario("Properly declared variable", &
        "program test" // new_line('A') // &
        "    integer :: x" // new_line('A') // &
        "    x = 42" // new_line('A') // &
        "end program test", .false.)
        
    call test_scenario("Multiple undefined variables", &
        "program test" // new_line('A') // &
        "    z = x + y" // new_line('A') // &
        "end program test", .true.)
        
    call test_scenario("Mixed declared and undeclared", &
        "program test" // new_line('A') // &
        "    integer :: x" // new_line('A') // &
        "    z = x + y" // new_line('A') // &
        "end program test", .true.)
        
    call test_scenario("Undefined in function call", &
        "program test" // new_line('A') // &
        "    real :: result" // new_line('A') // &
        "    result = sin(undefined_var)" // new_line('A') // &
        "end program test", .true.)

    print *
    print *, "Tests passed:", passed_count, "/", test_count
    if (passed_count == test_count) then
        print *, "All comprehensive tests passed!"
        stop 0
    else
        print *, "Some comprehensive tests failed!"
        stop 1
    end if

contains

    subroutine test_scenario(name, source, expect_errors)
        character(len=*), intent(in) :: name, source
        logical, intent(in) :: expect_errors
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        logical :: has_errors
        
        test_count = test_count + 1
        print *, "Testing:", trim(name)
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (len(error_msg) > 0) then
            print *, "  SKIP: Lex error -", error_msg
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (len(error_msg) > 0) then
            print *, "  SKIP: Parse error -", error_msg
            return
        end if
        
        ! Semantic analysis
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        has_errors = has_semantic_errors(sem_ctx)
        
        if (expect_errors .and. has_errors) then
            print *, "  PASS: Expected errors detected"
            passed_count = passed_count + 1
        else if (.not. expect_errors .and. .not. has_errors) then
            print *, "  PASS: No errors as expected"
            passed_count = passed_count + 1
        else if (expect_errors .and. .not. has_errors) then
            print *, "  FAIL: Expected errors but none detected"
        else
            print *, "  FAIL: Unexpected errors detected"
        end if
        
    end subroutine test_scenario

end program test_undefined_variables_comprehensive