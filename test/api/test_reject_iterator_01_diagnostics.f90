program test_reject_iterator_01_diagnostics
    ! Issue #2898: malformed iterators and implied-DO delimiters must be
    ! rejected with a rule-specific source diagnostic, while the corrected
    ! neighbouring form stays accepted.
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    integer :: failures

    failures = 0

    ! gfortran.dg/pr19936_2.f90: iterator bound followed by junk in an
    ! array-constructor implied-DO.
    call expect_rejected( &
        "program p"//new_line('a')// &
        "  integer i"//new_line('a')// &
        "  print *,(/(i,i=1a,4)/)"//new_line('a')// &
        "end program p", &
        "Syntax error in iterator", "array-constructor iterator")
    call expect_accepted( &
        "program p"//new_line('a')// &
        "  integer i"//new_line('a')// &
        "  print *,(/(i,i=1,4)/)"//new_line('a')// &
        "end program p", &
        "array-constructor iterator")

    ! The same iterator rule outside an I/O list, where the array-constructor
    ! parser owns the implied-DO control.
    call expect_rejected( &
        "program p"//new_line('a')// &
        "  implicit none"//new_line('a')// &
        "  integer :: i, a(4)"//new_line('a')// &
        "  a = (/ (i, i = 1a, 4) /)"//new_line('a')// &
        "end program p", &
        "Syntax error in iterator", "assigned array-constructor iterator")
    call expect_accepted( &
        "program p"//new_line('a')// &
        "  implicit none"//new_line('a')// &
        "  integer :: i, a(4)"//new_line('a')// &
        "  a = (/ (i, i = 1, 4) /)"//new_line('a')// &
        "end program p", &
        "assigned array-constructor iterator")

    ! gfortran.dg/implied_do_2.f90: I/O implied-DO with no closing parenthesis.
    call expect_rejected( &
        "program p"//new_line('a')// &
        "  implicit none"//new_line('a')// &
        "  integer :: ir"//new_line('a')// &
        "  write(*,*) ( ir, ir = 1,10"//new_line('a')// &
        "end program p", &
        "Expected a right parenthesis", "I/O implied-DO delimiter")
    call expect_accepted( &
        "program p"//new_line('a')// &
        "  implicit none"//new_line('a')// &
        "  integer :: ir"//new_line('a')// &
        "  write(*,*) ( ir, ir = 1,10 )"//new_line('a')// &
        "end program p", &
        "I/O implied-DO delimiter")

    ! The same iterator rule on a DO construct: a bound followed by junk.
    call expect_rejected( &
        "program p"//new_line('a')// &
        "  implicit none"//new_line('a')// &
        "  integer :: i, s"//new_line('a')// &
        "  s = 0"//new_line('a')// &
        "  do i = 1a, 4"//new_line('a')// &
        "    s = s + i"//new_line('a')// &
        "  end do"//new_line('a')// &
        "end program p", &
        "Syntax error in DO iterator", "DO construct iterator")
    call expect_accepted( &
        "program p"//new_line('a')// &
        "  implicit none"//new_line('a')// &
        "  integer :: i, s"//new_line('a')// &
        "  s = 0"//new_line('a')// &
        "  do i = 1, 4"//new_line('a')// &
        "    s = s + i"//new_line('a')// &
        "  end do"//new_line('a')// &
        "end program p", &
        "DO construct iterator")

    ! A stride and a nested I/O implied-DO must keep parsing cleanly.
    call expect_accepted( &
        "program p"//new_line('a')// &
        "  implicit none"//new_line('a')// &
        "  integer :: i, a(9)"//new_line('a')// &
        "  a = 0"//new_line('a')// &
        "  write(*,*) (a(i), i = 1, 9, 2)"//new_line('a')// &
        "end program p", &
        "implied-DO with stride")

    if (failures /= 0) then
        write (error_unit, '(A,I0,A)') "FAIL: ", failures, " iterator checks"
        error stop 1
    end if
    write (output_unit, '(A)') "PASS: reject-iterator-01 diagnostics"

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

end program test_reject_iterator_01_diagnostics
