program test_issue_1771_module_parameter_types
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use standardizer, only: standardize_ast
    use codegen_core, only: codegen_core_generate_arena, initialize_codegen
    use ast_arena_modern, only: ast_arena_t
    use lexer_core, only: token_t
    implicit none

    logical :: ok

    ok = check_module_parameter_types()
    if (ok) then
        print *, "PASS: Issue #1771 - Module parameter types preserved"
    else
        error stop "FAIL: Issue #1771 - Module parameter types not preserved"
    end if

contains

    function check_module_parameter_types() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: output_code
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index

        passed = .true.

        call initialize_codegen()

        source = &
            "module math_utils" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer, parameter :: dp = selected_real_kind(15, 307)" // &
            & new_line('a') // &
            "contains" // new_line('a') // &
            "    function square(x) result(res)" // new_line('a') // &
            "        real(dp), intent(in) :: x" // new_line('a') // &
            "        real(dp) :: res" // new_line('a') // &
            "        res = x * x" // new_line('a') // &
            "    end function square" // new_line('a') // &
            "end module math_utils" // new_line('a') // &
            "" // new_line('a') // &
            "program test_module_only" // new_line('a') // &
            "    use math_utils, only: square" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    real(selected_real_kind(15, 307)) :: x" // new_line('a') // &
            "    x = 2.5" // new_line('a') // &
            "    print *, 'Square:', square(x)" // new_line('a') // &
            "end program test_module_only"

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

        output_code = codegen_core_generate_arena(arena, root_index)

        if (index(output_code, "real(selected_real_kind(15,307)) :: x") <= 0) then
            print *, "FAIL: Type specifier not preserved in program declaration"
            print *, "Expected: real(selected_real_kind(15,307)) :: x"
            print *, "Output:"
            print *, trim(output_code)
            passed = .false.
            return
        end if

    end function check_module_parameter_types

end program test_issue_1771_module_parameter_types
