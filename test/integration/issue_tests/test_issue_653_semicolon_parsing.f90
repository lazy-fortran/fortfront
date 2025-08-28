program test_issue_653_semicolon_parsing
    ! Test for Issue #653: CRITICAL if/else parsing completely broken with false error messages
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
    logical :: test_passed
    
    print *, "=== Testing Issue #653: Semicolon If/Else Parsing ==="
    
    test_passed = .true.
    
    ! Test 1: Exact case from GitHub issue
    source = 'if (x > 0) then; print *, "pos"; else; print *, "neg"; end if'
    
    print *, "Test 1 - GitHub Issue Exact Case:"
    print *, "INPUT: ", source
    
    call lex_source(source, tokens, error_msg)
    if (error_msg /= "") then
        print *, "LEXING ERROR: ", error_msg
        test_passed = .false.
    else
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "PARSING ERROR: ", error_msg
            test_passed = .false.
        else
            call emit_fortran(arena, prog_index, code)
            ! Check that both then and else clauses are present
            if (index(code, 'print *, "pos"') > 0 .and. index(code, 'print *, "neg"') > 0 .and. &
                index(code, 'else') > 0) then
                print *, "✅ PASS: Both then and else clauses correctly generated"
            else
                print *, "❌ FAIL: Generated code missing then/else clauses:"
                print *, code
                test_passed = .false.
            end if
        end if
    end if
    
    ! Test 2: Assignment statements with semicolons
    source = 'if (flag) then; x = 1; y = 2; else; x = 3; y = 4; end if'
    
    print *, ""
    print *, "Test 2 - Assignment Statements:"
    print *, "INPUT: ", source
    
    call lex_source(source, tokens, error_msg)
    if (error_msg /= "") then
        print *, "LEXING ERROR: ", error_msg
        test_passed = .false.
    else
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "PARSING ERROR: ", error_msg
            test_passed = .false.
        else
            call emit_fortran(arena, prog_index, code)
            if (index(code, 'x = 1') > 0 .and. index(code, 'y = 2') > 0 .and. &
                index(code, 'x = 3') > 0 .and. index(code, 'y = 4') > 0 .and. &
                index(code, 'else') > 0) then
                print *, "✅ PASS: Multiple assignment statements correctly parsed"
            else
                print *, "❌ FAIL: Generated code missing assignments:"
                print *, code  
                test_passed = .false.
            end if
        end if
    end if
    
    ! Test 3: Nested if with semicolons
    source = 'if (a > 0) then; if (b > 0) then; print *, "both"; end if; else; print *, "not a"; end if'
    
    print *, ""
    print *, "Test 3 - Nested If Statements:"
    print *, "INPUT: ", source
    
    call lex_source(source, tokens, error_msg)
    if (error_msg /= "") then
        print *, "LEXING ERROR: ", error_msg
        test_passed = .false.
    else
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "PARSING ERROR: ", error_msg
            test_passed = .false.
        else
            call emit_fortran(arena, prog_index, code)
            if (index(code, 'print *, "both"') > 0 .and. index(code, 'print *, "not a"') > 0) then
                print *, "✅ PASS: Nested if statements correctly parsed"
            else
                print *, "❌ FAIL: Generated code missing nested statements:"
                print *, code
                test_passed = .false.
            end if
        end if
    end if
    
    ! Final result
    print *, ""
    if (test_passed) then
        print *, "🎉 SUCCESS: Issue #653 completely resolved!"
        print *, "Semicolon if/else parsing working correctly"
    else
        print *, "💥 FAILURE: Issue #653 not fully resolved"
        stop 1
    end if
    
end program test_issue_653_semicolon_parsing