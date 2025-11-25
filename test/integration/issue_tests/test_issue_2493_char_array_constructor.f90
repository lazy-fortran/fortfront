program test_issue_2493_char_array_constructor
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                              iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg
    logical :: has_padded_char

    call read_example('examples/f90/issue_2493_char_array_constructor.f90', source)
    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a)') 'FAIL: unexpected error: ' // trim(error_msg)
            stop 1
        end if
    end if

    if (index(transformed, 'character(len=1)') == 0) then
        write (error_unit, '(a)') 'FAIL: character length not preserved as 1'
        write (error_unit, '(a)') transformed
        stop 1
    end if

    if (index(transformed, 'character(len=5)') /= 0) then
        write (error_unit, '(a)') 'FAIL: character length inflated to 5'
        write (error_unit, '(a)') transformed
        stop 1
    end if

    has_padded_char = index(transformed, "'1    '") /= 0 .or. &
                      index(transformed, "'2    '") /= 0 .or. &
                      index(transformed, "'a    '") /= 0 .or. &
                      index(transformed, "'b    '") /= 0 .or. &
                      index(transformed, "'c    '") /= 0
    if (has_padded_char) then
        write (error_unit, '(a)') 'FAIL: string literals were padded with spaces'
        write (error_unit, '(a)') transformed
        stop 1
    end if

    if (index(transformed, "(/'1', '2' /)") == 0 .and. &
        index(transformed, "(/ '1', '2' /)") == 0 .and. &
        index(transformed, "(/'1','2' /)") == 0) then
        write (error_unit, '(a)') 'FAIL: legacy array constructor not preserved'
        write (error_unit, '(a)') transformed
        stop 1
    end if

    if (index(transformed, "['a', 'b', 'c']") == 0 .and. &
        index(transformed, "['a','b','c']") == 0) then
        write (error_unit, '(a)') 'FAIL: modern array constructor not preserved'
        write (error_unit, '(a)') transformed
        stop 1
    end if

    write (error_unit, '(a)') &
        'PASS: character array constructors emit unpadded string literals'

contains

    include '../../common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(a)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example

end program test_issue_2493_char_array_constructor
