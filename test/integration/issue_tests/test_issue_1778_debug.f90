program test_issue_1778_debug
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use lexer_core
    use fortfront
    implicit none
    character(len=:), allocatable :: input_code, output_code, error_msg
    type(token_t), allocatable :: tokens(:)

    print *, "Testing nested array literal with debugging..."

    ! Nested 2D array
    call read_example('examples/lf/issue_1778_debug.lf', input_code)

    print *, "Input:", trim(input_code)

    ! Test lexing first
    call lex_source(input_code, tokens, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "Lexer error:", trim(error_msg)
        stop 1
    end if

    print *, "Number of tokens:", size(tokens)
    print *, "Tokens:"
    block
        integer :: i
        do i = 1, min(size(tokens), 20)
            print *, "  ", i, ":", trim(tokens(i)%text), "kind=", tokens(i)%kind
        end do
    end block

    ! Now test transformation
    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "Transform error:", trim(error_msg)
    end if

    if (allocated(output_code)) then
        print *, "Output allocated: YES"
        print *, "Output length:", len(output_code)
        if (len(output_code) > 0) then
            print *, "Output:"
            print *, trim(output_code)
        else
            print *, "OUTPUT IS EMPTY (but allocated)!"
        end if
    else
        print *, "OUTPUT NOT ALLOCATED!"
    end if

contains

    include '../../common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example

end program test_issue_1778_debug
