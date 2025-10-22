program test_issue_1664_result_keywords
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use standardizer, only: standardize_ast
    use codegen_core, only: codegen_core_generate_arena, initialize_codegen
    use ast_arena_modern, only: ast_arena_t
    use lexer_core, only: token_t
    implicit none

    logical :: ok

    ok = .true.
    ok = check_result_variable("in") .and. ok
    ok = check_result_variable("out") .and. ok
    ok = check_result_variable("inout") .and. ok

    if (ok) then
        print *, "PASS: Issue #1664 - result clause retains keyword names"
    else
        error stop "FAIL: Issue #1664 - keyword result names lost"
    end if

contains

    function check_result_variable(result_name) result(passed)
        character(len=*), intent(in) :: result_name
        logical :: passed
        character(len=:), allocatable :: error_msg, code, source
        character(len=:), allocatable :: result_clause, assignment_fragment
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index

        passed = .true.
        result_clause = "result(" // trim(result_name) // ")"
        assignment_fragment = trim(result_name) // " = "

        source = &
            "program p" // new_line('a') // &
            "contains" // new_line('a') // &
            "function f(vals) result(" // trim(result_name) // ")" // new_line('a') // &
            "integer, intent(in) :: vals" // new_line('a') // &
            "integer :: " // trim(result_name) // new_line('a') // &
            trim(result_name) // " = vals" // new_line('a') // &
            "end function f" // new_line('a') // &
            "end program p"

        call initialize_codegen()
        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: lexing error:", trim(error_msg)
            passed = .false.
            return
        end if

        call parse_tokens(tokens, arena, root_index, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "FAIL: parsing error:", trim(error_msg)
            passed = .false.
            return
        end if

        call standardize_ast(arena, root_index)

        code = codegen_core_generate_arena(arena, root_index)

        if (index(code, result_clause) <= 0) then
            print *, "FAIL: missing ", trim(result_clause)
            passed = .false.
        else
            print *, "PASS: found ", trim(result_clause)
        end if

        if (index(code, assignment_fragment) <= 0) then
            print *, "FAIL: assignment missing for ", trim(result_name)
            passed = .false.
        else
            print *, "PASS: assignment retained for ", trim(result_name)
        end if
    end function check_result_variable

end program test_issue_1664_result_keywords
