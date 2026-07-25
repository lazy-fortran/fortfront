program test_reject_do_concurrent_01_diagnostics
    ! Issue #2901: DO CONCURRENT locality-spec constraints (F2023 C1129/C1130
    ! and the R1131 reduce-operation list) must be rejected with a
    ! rule-specific source diagnostic, while corrected neighbours stay accepted.
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    integer :: failures

    failures = 0

    ! C1130: a variable named in two locality-specs.
    call expect_rejected( &
        "program p"//new_line('a')// &
        "  implicit none"//new_line('a')// &
        "  integer :: i, total"//new_line('a')// &
        "  total = 0"//new_line('a')// &
        "  do concurrent (i = 1:10) shared(total) reduce(+:total)"// &
        new_line('a')// &
        "    total = total + i"//new_line('a')// &
        "  end do"//new_line('a')// &
        "end program p", &
        "has already been specified in a locality-spec", &
        "duplicate variable in locality-spec-list")

    ! C1129: DEFAULT (NONE) given twice.
    call expect_rejected( &
        "program p"//new_line('a')// &
        "  implicit none"//new_line('a')// &
        "  integer :: i, b"//new_line('a')// &
        "  do concurrent (i = 1:4) default(none) shared(b) default(none)"// &
        new_line('a')// &
        "    b = i"//new_line('a')// &
        "  end do"//new_line('a')// &
        "end program p", &
        "DEFAULT (NONE) specified more than once", &
        "repeated DEFAULT (NONE)")

    ! R1131: "-" is not a reduce-operation.
    call expect_rejected( &
        "program p"//new_line('a')// &
        "  implicit none"//new_line('a')// &
        "  integer :: i, x"//new_line('a')// &
        "  x = 0"//new_line('a')// &
        "  do concurrent (i = 2:4) reduce(-:x)"//new_line('a')// &
        "    x = x - i"//new_line('a')// &
        "  end do"//new_line('a')// &
        "end program p", &
        "Expected reduction operator or function name", &
        "invalid reduce-operation")

    ! R1131: the reduce-operation must be followed by ":".
    call expect_rejected( &
        "program p"//new_line('a')// &
        "  implicit none"//new_line('a')// &
        "  integer :: i, x"//new_line('a')// &
        "  x = 0"//new_line('a')// &
        "  do concurrent (i = 2:4) reduce(+ x)"//new_line('a')// &
        "    x = x + i"//new_line('a')// &
        "  end do"//new_line('a')// &
        "end program p", &
        "Expected ':' in DO CONCURRENT", "missing reduce colon")

    ! R1130: REDUCTION is not a locality-spec keyword.
    call expect_rejected( &
        "program p"//new_line('a')// &
        "  implicit none"//new_line('a')// &
        "  integer :: i, x"//new_line('a')// &
        "  x = 0"//new_line('a')// &
        "  do concurrent (i = 2:4) reduction(+: x)"//new_line('a')// &
        "    x = x + i"//new_line('a')// &
        "  end do"//new_line('a')// &
        "end program p", &
        "unknown locality specifier", "misspelled locality-spec")

    ! Corrected neighbours: every locality-spec form, each name used once.
    call expect_accepted( &
        "program p"//new_line('a')// &
        "  implicit none"//new_line('a')// &
        "  integer :: i, arr(10), total, temp"//new_line('a')// &
        "  total = 0"//new_line('a')// &
        "  do concurrent (i = 1:10) default(none) local(temp) shared(arr) "// &
        "reduce(+:total)"//new_line('a')// &
        "    temp = i * 2"//new_line('a')// &
        "    arr(i) = temp"//new_line('a')// &
        "    total = total + arr(i)"//new_line('a')// &
        "  end do"//new_line('a')// &
        "end program p", &
        "full locality-spec-list")

    call expect_accepted( &
        "program p"//new_line('a')// &
        "  implicit none"//new_line('a')// &
        "  integer :: i, m"//new_line('a')// &
        "  m = 0"//new_line('a')// &
        "  do concurrent (i = 1:10) reduce(max:m)"//new_line('a')// &
        "    m = max(m, i)"//new_line('a')// &
        "  end do"//new_line('a')// &
        "end program p", &
        "intrinsic-function reduce-operation")

    call expect_accepted( &
        "program p"//new_line('a')// &
        "  implicit none"//new_line('a')// &
        "  integer :: i, a(10), b(10), t"//new_line('a')// &
        "  a = 0"//new_line('a')// &
        "  do concurrent (i = 1:10) local_init(t) shared(a, b)"// &
        new_line('a')// &
        "    a(i) = b(i)"//new_line('a')// &
        "  end do"//new_line('a')// &
        "end program p", &
        "LOCAL_INIT and multi-name SHARED")

    ! A DO CONCURRENT with no locality-spec at all must stay accepted.
    call expect_accepted( &
        "program p"//new_line('a')// &
        "  implicit none"//new_line('a')// &
        "  integer :: i, a(10)"//new_line('a')// &
        "  do concurrent (i = 1:10)"//new_line('a')// &
        "    a(i) = i"//new_line('a')// &
        "  end do"//new_line('a')// &
        "end program p", &
        "bare DO CONCURRENT")

    if (failures /= 0) then
        write (error_unit, '(A,I0,A)') "FAIL: ", failures, " locality checks"
        error stop 1
    end if
    write (output_unit, '(A)') "PASS: reject-do-concurrent-01 diagnostics"

contains

    subroutine expect_rejected(source, expected_fragment, label)
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: expected_fragment
        character(len=*), intent(in) :: label
        character(len=5000) :: parse_error

        call parse_source(source, parse_error)

        if (len_trim(parse_error) == 0) then
            write (output_unit, '(A)') "FAIL: accepted invalid "//label
            failures = failures + 1
            return
        end if
        if (index(parse_error, expected_fragment) == 0) then
            write (output_unit, '(A)') "FAIL: wrong diagnostic for "//label
            write (output_unit, '(A)') trim(parse_error)
            failures = failures + 1
            return
        end if
        write (output_unit, '(A)') "PASS: rejected invalid "//label
    end subroutine expect_rejected

    subroutine expect_accepted(source, label)
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: label
        character(len=5000) :: parse_error

        call parse_source(source, parse_error)

        if (len_trim(parse_error) /= 0) then
            write (output_unit, '(A)') "FAIL: rejected valid "//label
            write (output_unit, '(A)') trim(parse_error)
            failures = failures + 1
            return
        end if
        write (output_unit, '(A)') "PASS: accepted valid "//label
    end subroutine expect_accepted

    subroutine parse_source(source, parse_error)
        character(len=*), intent(in) :: source
        character(len=*), intent(out) :: parse_error
        character(len=:), allocatable :: lex_error
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: root_index

        parse_error = ""
        arena = create_ast_arena()
        call lex_source(source, tokens, lex_error)
        if (allocated(lex_error)) then
            if (len_trim(lex_error) > 0) then
                parse_error = "lexer: "//trim(lex_error)
                return
            end if
        end if
        call parse_tokens(tokens, arena, root_index, parse_error)
    end subroutine parse_source

end program test_reject_do_concurrent_01_diagnostics
