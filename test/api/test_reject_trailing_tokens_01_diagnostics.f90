program test_reject_trailing_tokens_01_diagnostics
    ! A statement parser must not accept a valid prefix and silently discard
    ! malformed tokens that follow it (gfortran.dg/error_recovery_2.f90).
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string
    use semantic_input_mode, only: INPUT_MODE_STANDARD
    implicit none

    integer :: failures

    failures = 0
    call expect_rejected( &
        "subroutine bad"//new_line('a')// &
        "  character*20 :: y, x 00"//new_line('a')// &
        "end subroutine bad", "trailing declaration token")
    call expect_rejected( &
        "subroutine bad"//new_line('a')// &
        "  character*20 :: y, x"//new_line('a')// &
        "  data y /'abcdef'/, x /'jbnhjk'/ pp"//new_line('a')// &
        "end subroutine bad", "trailing DATA token")
    call expect_accepted( &
        "subroutine good"//new_line('a')// &
        "  character*20 :: y, x"//new_line('a')// &
        "  data y /'abcdef'/, x /'jbnhjk'/"//new_line('a')// &
        "end subroutine good", "valid declaration and DATA")
    call expect_standard_accepted( &
        "subroutine intent_in_out"//new_line('a')// &
        "  real(8), intent(in out) :: a(:,:), b(:,:)"//new_line('a')// &
        "end subroutine intent_in_out", "INTENT(IN OUT) declaration")
    call expect_standard_rejected( &
        "program pointer_target"//new_line('a')// &
        "  integer, pointer :: p"//new_line('a')// &
        "  integer :: i"//new_line('a')// &
        "  p => (i)"//new_line('a')// &
        "end program pointer_target", "parenthesized pointer target")

    if (failures /= 0) error stop 1
    print '(A)', 'PASS: reject-trailing-tokens-01 diagnostics'

contains

    subroutine expect_rejected(source, label)
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: label
        character(len=5000) :: parse_error

        call parse_source(source, parse_error)
        if (index(parse_error, 'Syntax error') == 0) then
            print '(A)', 'FAIL: accepted '//trim(label)
            if (len_trim(parse_error) > 0) print '(A)', trim(parse_error)
            failures = failures + 1
        else
            print '(A)', 'PASS: rejected '//trim(label)
        end if
    end subroutine expect_rejected

    subroutine expect_accepted(source, label)
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: label
        character(len=5000) :: parse_error

        call parse_source(source, parse_error)
        if (len_trim(parse_error) /= 0) then
            print '(A)', 'FAIL: rejected '//trim(label)
            print '(A)', trim(parse_error)
            failures = failures + 1
        else
            print '(A)', 'PASS: accepted '//trim(label)
        end if
    end subroutine expect_accepted

    subroutine expect_standard_accepted(source, label)
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: label
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result

        options%input_mode = INPUT_MODE_STANDARD
        options%run_semantics = .false.
        options%standardize = .false.
        call compile_frontend_from_string(source, result, options)
        if (.not. result%parse_ok) then
            print '(A)', 'FAIL: rejected '//trim(label)
            if (allocated(result%diagnostic_text)) then
                print '(A)', trim(result%diagnostic_text)
            end if
            failures = failures + 1
        else
            print '(A)', 'PASS: accepted '//trim(label)
        end if
    end subroutine expect_standard_accepted

    subroutine expect_standard_rejected(source, label)
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: label
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result

        options%input_mode = INPUT_MODE_STANDARD
        options%run_semantics = .false.
        options%standardize = .false.
        call compile_frontend_from_string(source, result, options)
        if (result%parse_ok) then
            print '(A)', 'FAIL: accepted '//trim(label)
            failures = failures + 1
        else
            print '(A)', 'PASS: rejected '//trim(label)
        end if
    end subroutine expect_standard_rejected

    subroutine parse_source(source, parse_error)
        character(len=*), intent(in) :: source
        character(len=*), intent(out) :: parse_error
        character(len=:), allocatable :: lex_error
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: root_index

        parse_error = ''
        arena = create_ast_arena()
        call lex_source(source, tokens, lex_error)
        if (allocated(lex_error)) then
            if (len_trim(lex_error) > 0) then
                parse_error = 'lexer: '//trim(lex_error)
                return
            end if
        end if
        call parse_tokens(tokens, arena, root_index, parse_error)
    end subroutine parse_source

end program test_reject_trailing_tokens_01_diagnostics
