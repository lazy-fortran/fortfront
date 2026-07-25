program test_reject_submodule_01_diagnostics
    ! Rejection coverage for submodule declaration consistency.
    !
    ! Every negative fixture must be rejected with a diagnostic from this rule
    ! family, and every corrected neighbour must still be accepted. Over-eager
    ! rejection is the failure mode this test guards against, so the positive
    ! cases are as load bearing as the negative ones.
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Nested SUBMODULE declarations (F2018 R1116: a submodule is a program unit)
    call assert_rejected('examples/f90/submodule_twice.f90', &
        'SUBMODULE declaration is not allowed', all_passed)
    call assert_rejected('examples/f90/submodule_unexp.f90', &
        'SUBMODULE declaration is not allowed', all_passed)

    ! Corrected neighbours must keep compiling
    call assert_accepted('examples/f90/submodule_placement_valid.f90', all_passed)
    call assert_accepted('examples/f90/issue_1827_submodule_simple.f90', all_passed)
    call assert_accepted('examples/f90/issue_1827_submodule_with_contents.f90', &
        all_passed)

    if (.not. all_passed) error stop 1
    write (output_unit, '(A)') 'PASS: submodule rejection diagnostics'

contains

    include '../common/read_example.inc'

    subroutine parse_example(path, parse_error)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: parse_error
        character(len=:), allocatable :: source
        character(len=:), allocatable :: lex_error
        character(len=5000) :: error_buffer
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: root_index

        call read_example(path, source)

        arena = create_ast_arena()
        call lex_source(source, tokens, lex_error)
        if (allocated(lex_error)) then
            if (len_trim(lex_error) > 0) then
                write (error_unit, '(A)') 'FAIL: lexing error in '//path//': '// &
                    trim(lex_error)
                error stop 1
            end if
        end if

        error_buffer = ''
        call parse_tokens(tokens, arena, root_index, error_buffer)
        parse_error = trim(error_buffer)
    end subroutine parse_example

    subroutine assert_rejected(path, expected_fragment, passed)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: expected_fragment
        logical, intent(inout) :: passed
        character(len=:), allocatable :: parse_error

        call parse_example(path, parse_error)

        if (len_trim(parse_error) == 0) then
            write (error_unit, '(A)') 'FAIL: '//path//' was accepted'
            passed = .false.
            return
        end if

        if (index(parse_error, expected_fragment) == 0) then
            write (error_unit, '(A)') 'FAIL: '//path// &
                ' missing expected diagnostic "'//expected_fragment//'"'
            write (error_unit, '(A)') trim(parse_error)
            passed = .false.
        end if
    end subroutine assert_rejected

    subroutine assert_accepted(path, passed)
        character(len=*), intent(in) :: path
        logical, intent(inout) :: passed
        character(len=:), allocatable :: parse_error

        call parse_example(path, parse_error)

        if (len_trim(parse_error) /= 0) then
            write (error_unit, '(A)') 'FAIL: '//path//' was rejected'
            write (error_unit, '(A)') trim(parse_error)
            passed = .false.
        end if
    end subroutine assert_accepted

end program test_reject_submodule_01_diagnostics
