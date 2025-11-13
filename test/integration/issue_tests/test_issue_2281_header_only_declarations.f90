program test_issue_2281_header_only_declarations
    use fortfront, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        iostat_eor
    implicit none

    character(len=:), allocatable :: source, result_code, error_msg

    call read_example('examples/f90/issue_2281_header_only_declarations.f90', source)

    call transform_lazy_fortran_string(source, result_code, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: unexpected error message'
        write (error_unit, '(A)') trim(error_msg)
        error stop 'FAIL: transform_lazy_fortran_string returned error'
    end if

    if (index(result_code, 'program main') > 0) then
        write (error_unit, '(A)') 'FAIL: synthetic program main emitted for declarations'
        write (error_unit, '(A)') trim(result_code)
        error stop 'FAIL: program wrapper present'
    end if

    if (index(result_code, 'type :: mytype') == 0 .or. &
        index(result_code, 'end type mytype') == 0) then
        write (error_unit, '(A)') 'FAIL: derived type definition missing'
        write (error_unit, '(A)') trim(result_code)
        error stop 'FAIL: derived type removed'
    end if

    if (index(result_code, 'integer :: x') == 0) then
        write (error_unit, '(A)') 'FAIL: trailing declaration missing'
        write (error_unit, '(A)') trim(result_code)
        error stop 'FAIL: declarations dropped'
    end if

    print *, 'PASS: test_issue_2281_header_only_declarations'

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

end program test_issue_2281_header_only_declarations
