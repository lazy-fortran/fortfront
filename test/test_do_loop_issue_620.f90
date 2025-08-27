program test_do_loop_issue_620
    ! Focused test for GitHub issue #620 - do loops with TODO placeholders
    use iso_fortran_env, only: error_unit
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use ast_core, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none
    
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    character(len=:), allocatable :: error_msg, code, source
    integer :: prog_index
    logical :: has_failure
    
    has_failure = .false.
    
    print *, "=== Testing Issue #620: Do Loop TODO Placeholders ==="
    
    ! Original issue: do loops generating TODO placeholders
    ! Testing different scenarios that might trigger the issue
    
    ! Test 1: Basic do loop
    print *, ""
    print *, "Test 1: Basic do loop (should always work)"
    source = 'do i = 1, 3' // new_line('A') // &
             '  print *, i' // new_line('A') // &
             'end do'
    call process_and_check(source, has_failure, "Basic do loop")
    
    ! Test 2: Do loop with expressions
    print *, ""  
    print *, "Test 2: Do loop with expressions"
    source = 'n = 10' // new_line('A') // &
             'do i = n-5, n+5, 2' // new_line('A') // &
             '  print *, i' // new_line('A') // &
             'end do'
    call process_and_check(source, has_failure, "Do loop with expressions")
    
    ! Test 3: Do loop with array operations
    print *, ""
    print *, "Test 3: Do loop with array operations"  
    source = 'do i = 1, 5' // new_line('A') // &
             '  arr(i) = i * 2' // new_line('A') // &
             'end do'
    call process_and_check(source, has_failure, "Do loop with array ops")
    
    ! Test 4: Nested do loops
    print *, ""
    print *, "Test 4: Nested do loops"
    source = 'do i = 1, 2' // new_line('A') // &
             '  do j = 1, 3' // new_line('A') // &
             '    print *, i, j' // new_line('A') // &
             '  end do' // new_line('A') // &
             'end do'
    call process_and_check(source, has_failure, "Nested do loops")
    
    ! Test 5: Do loop in control flow
    print *, ""
    print *, "Test 5: Do loop inside if statement"
    source = 'if (x > 0) then' // new_line('A') // &
             '  do i = 1, 3' // new_line('A') // &
             '    print *, i' // new_line('A') // &
             '  end do' // new_line('A') // &
             'end if'
    call process_and_check(source, has_failure, "Do loop in if")
    
    ! Test 6: Do while loop (different node type)
    print *, ""
    print *, "Test 6: Do while loop"
    source = 'i = 1' // new_line('A') // &
             'do while (i <= 3)' // new_line('A') // &
             '  print *, i' // new_line('A') // &
             '  i = i + 1' // new_line('A') // &
             'end do'
    call process_and_check(source, has_failure, "Do while loop")
    
    ! Summary
    print *, ""
    print *, "=========================================="
    if (has_failure) then
        print *, "RESULT: Issue #620 CONFIRMED - TODO placeholders or errors found"
        stop 1
    else
        print *, "RESULT: All tests passed - do loops work correctly"
    end if
    
contains

    subroutine process_and_check(source, has_failure, test_desc)
        character(len=*), intent(in) :: source, test_desc
        logical, intent(inout) :: has_failure
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: error_msg, code
        integer :: prog_index
        
        ! Process through the pipeline
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "  LEXING ERROR: ", error_msg
            has_failure = .true.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "  PARSING ERROR: ", error_msg
            has_failure = .true.
            return
        end if
        
        call emit_fortran(arena, prog_index, code)
        
        ! Check for problems
        if (index(code, 'TODO') > 0) then
            print *, "  ✗ FAILED: TODO placeholder found in ", test_desc
            print *, "  Generated code:"
            print *, "  ", code
            has_failure = .true.
        else if (index(code, 'Unparsed') > 0) then
            print *, "  ✗ FAILED: Unparsed statement found in ", test_desc
            print *, "  Generated code:"
            print *, "  ", code
            has_failure = .true.
        else if (index(code, 'do') == 0 .and. index(source, 'do') > 0) then
            print *, "  ✗ FAILED: Missing do statement in ", test_desc
            print *, "  Generated code:"
            print *, "  ", code
            has_failure = .true.
        else
            print *, "  ✓ PASSED: ", test_desc
        end if
        
    end subroutine process_and_check
    
end program test_do_loop_issue_620