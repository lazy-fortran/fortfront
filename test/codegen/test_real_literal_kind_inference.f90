program test_real_literal_kind_inference
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: all_passed

    call read_example('examples/lf/real_literal_kind_inference.lf', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'Unexpected error: ' // trim(error_msg)
        stop 1
    end if

    all_passed = check_output(output, 'double precision :: c', 'c = 1.0d0') .and. &
                 check_output(output, 'double precision :: d', 'd = 3.14159_8')

    if (all_passed) then
        print *, 'PASS: real literal kind inference retains double precision'
    else
        error stop 'FAIL: real literal kind inference regression'
    end if

contains

    include '../common/cli_io_reader.inc'

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

    logical function check_output(buffer, declaration, assignment)
        character(len=*), intent(in) :: buffer
        character(len=*), intent(in) :: declaration
        character(len=*), intent(in) :: assignment

        check_output = index(buffer, declaration) > 0 .and. &
                       index(buffer, assignment) > 0 .and. &
                       index(buffer, '!ERROR:') == 0
        if (.not. check_output) then
            write (error_unit, '(A)') 'Generated output:'
            write (error_unit, '(A)') trim(buffer)
        end if
    end function check_output

end program test_real_literal_kind_inference
