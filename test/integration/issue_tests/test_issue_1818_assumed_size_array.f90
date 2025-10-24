program test_issue_1818_assumed_size_array
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use standardizer, only: standardize_ast
    use codegen_core, only: codegen_core_generate_arena, initialize_codegen
    use ast_arena_modern, only: ast_arena_t
    use lexer_core, only: token_t
    implicit none

    logical :: ok

    ok = check_assumed_size_preserved()
    if (ok) then
        print *, "PASS: Issue #1818 - assumed-size array x(*) preserved"
    else
        error stop "FAIL: Issue #1818 - assumed-size array x(*) converted to scalar"
    end if

contains

    function check_assumed_size_preserved() result(passed)
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
            "program test_assumed_size_array" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: arr(5)" // new_line('a') // &
            "    arr = [1, 2, 3, 4, 5]" // new_line('a') // &
            "    call print_array(arr, 5)" // new_line('a') // &
            "contains" // new_line('a') // &
            "    subroutine print_array(x, n)" // new_line('a') // &
            "        integer, intent(in) :: n" // new_line('a') // &
            "        integer, intent(in) :: x(*)" // new_line('a') // &
            "        integer :: i" // new_line('a') // &
            "        do i = 1, n" // new_line('a') // &
            "            print *, x(i)" // new_line('a') // &
            "        end do" // new_line('a') // &
            "    end subroutine print_array" // new_line('a') // &
            "end program test_assumed_size_array"

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

        if (index(output_code, "x(*)") <= 0) then
            print *, "FAIL: assumed-size array x(*) not preserved"
            print *, "Output:"
            print *, trim(output_code)
            passed = .false.
            return
        end if

        if (index(output_code, "integer, intent(in) :: x(*)") <= 0) then
            print *, "FAIL: full declaration not correct"
            print *, "Output:"
            print *, trim(output_code)
            passed = .false.
        end if

    end function check_assumed_size_preserved

end program test_issue_1818_assumed_size_array
