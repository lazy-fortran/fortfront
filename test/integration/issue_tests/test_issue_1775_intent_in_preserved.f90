program test_issue_1775_intent_in_preserved
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

    ok = check_intent_in_preserved()
    if (ok) then
        print *, 'PASS: Issue #1775 - intent(in) preserved with keyword args'
    else
        error stop 'FAIL: Issue #1775 - intent(in) incorrectly changed to inout'
    end if

contains

    include '../../common/read_example.inc'


    function check_intent_in_preserved() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: output_code
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index

        passed = .true.

        call initialize_codegen()
        call read_example('examples/lf/issue_1775_intent_in_preserved.lf', &
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

        if (index(output_code, 'intent(inout)') > 0) then
            write (error_unit, '(A)') &
                'FAIL: intent(in) was changed to intent(inout)'
            write (error_unit, '(A)') trim(output_code)
            passed = .false.
            return
        end if

        if (index(output_code, 'call process(b = 2, a = 1, c = 3)') <= 0) then
            write (error_unit, '(A)') 'FAIL: keyword arguments not preserved'
            write (error_unit, '(A)') 'Generated code:'
            write (error_unit, '(A)') trim(output_code)
            passed = .false.
        end if

    end function check_intent_in_preserved

end program test_issue_1775_intent_in_preserved
