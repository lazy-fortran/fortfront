program test_issue_2394_uppercase_keywords
    ! Regression test for issue 2394: uppercase keywords in interfaces caused a loop
    ! Root cause: case-sensitive token%text comparisons in parser
    ! Fixed files: procedure_signatures, module_structures, procedure_bodies,
    ! procedure_definition_bodies

    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    logical :: test_passed

    test_passed = .true.

    ! Test 1: Uppercase FUNCTION in interface
    call run_case("Uppercase FUNCTION in interface", &
        "interface" // new_line('a') // &
        "    FUNCTION foo()" // new_line('a') // &
        "    END FUNCTION" // new_line('a') // &
        "end interface", test_passed)

    ! Test 2: Uppercase SUBROUTINE in interface
    call run_case("Uppercase SUBROUTINE in interface", &
        "interface" // new_line('a') // &
        "    SUBROUTINE bar()" // new_line('a') // &
        "    END SUBROUTINE" // new_line('a') // &
        "end interface", test_passed)

    ! Test 3: Mixed case
    call run_case("Mixed case FUNCTION in interface", &
        "interface" // new_line('a') // &
        "    Function baz(x)" // new_line('a') // &
        "        INTEGER :: x" // new_line('a') // &
        "    End Function" // new_line('a') // &
        "end interface", test_passed)

    if (test_passed) then
        print *, "All tests passed!"
    else
        stop 1
    end if

contains

    subroutine run_case(description, source, overall_passed)
        character(len=*), intent(in) :: description
        character(len=*), intent(in) :: source
        logical, intent(inout) :: overall_passed

        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: lex_error
        character(len=512) :: parse_error
        integer :: root_index

        arena = create_ast_arena()
        call lex_source(source, tokens, lex_error)
        parse_error = ""
        if (len_trim(lex_error) == 0) then
            call parse_tokens(tokens, arena, root_index, parse_error)
        else
            root_index = 0
        end if

        if (len_trim(lex_error) == 0 .and. len_trim(parse_error) == 0 .and. &
            root_index > 0) then
            print *, "PASS: " // trim(description)
        else
            print *, "FAIL: " // trim(description)
            if (len_trim(lex_error) > 0) print *, "Lex error: " // &
                trim(lex_error)
            if (len_trim(parse_error) > 0) print *, "Parse error: " // &
                trim(parse_error)
            overall_passed = .false.
        end if
    end subroutine run_case

end program test_issue_2394_uppercase_keywords
