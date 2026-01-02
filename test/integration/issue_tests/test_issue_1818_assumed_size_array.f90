program test_issue_1818_assumed_size_array
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
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
        print *, 'PASS: Issue #1818 - assumed-size array x(*) preserved'
    else
        error stop 'FAIL: Issue #1818 - assumed-size array x(*) converted'
    end if

contains

    include '../../common/read_example.inc'


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
        call read_example('examples/f90/issue_1818_assumed_size_array.f90', &
                          source)

        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: lexing error: ' // trim(error_msg)
            passed = .false.
            return
        end if

        call parse_tokens(tokens, arena, root_index, error_msg)
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: parsing error: ' // trim(error_msg)
            passed = .false.
            return
        end if

        call standardize_ast(arena, root_index)

        output_code = codegen_core_generate_arena(arena, root_index)

        if (index(output_code, 'x(*)') <= 0) then
            write (error_unit, '(A)') 'FAIL: assumed-size array x(*) not preserved'
            write (error_unit, '(A)') trim(output_code)
            passed = .false.
            return
        end if

        if (index(output_code, 'integer, intent(in) :: x(*)') <= 0) then
            write (error_unit, '(A)') 'FAIL: declaration for x(*) incorrect'
            write (error_unit, '(A)') trim(output_code)
            passed = .false.
        end if

    end function check_assumed_size_preserved

end program test_issue_1818_assumed_size_array
