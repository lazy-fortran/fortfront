program test_tooling_passthrough
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use transformation_api, only: transform_lazy_fortran_string

    character(len=:), allocatable :: input_text
    character(len=:), allocatable :: output_text
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: expected

    call read_example('examples/f90/external_tool_example.f90', input_text)
    expected = input_text // new_line('a')

    call transform_lazy_fortran_string(expected, output_text, error_msg)

    if (.not. allocated(output_text)) then
        print *, 'FAIL: passthrough produced no output'
        stop 1
    end if

    if (output_text /= expected) then
        print *, 'FAIL: tooling example should be unchanged by transformation'
        print *, 'Expected:'
        print *, trim(expected)
        print *, 'Actual:'
        print *, trim(output_text)
        stop 1
    end if

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: unexpected error message:'
            print *, trim(error_msg)
            stop 1
        end if
    end if

    print *, 'PASS: tooling passthrough preserved external tooling example'

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

end program test_tooling_passthrough
