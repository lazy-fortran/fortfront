program test_issue_1738_derived_type_allocatable
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_message

    call read_example('examples/f90/issue_1738_derived_type_allocatable.f90', &
                      input_code)

    ! Transform the code
    call transform_lazy_fortran_string(input_code, output_code, error_message)

    ! Check that transformation succeeded
    if (len_trim(error_message) > 0) then
        print *, 'FAIL: Transformation failed'
        print *, 'Error:', error_message
        error stop 1
    end if

    ! Check that allocatable component is preserved
    if (index(output_code, 'allocatable') == 0) then
        print *, 'FAIL: allocatable attribute missing from output'
        print *, 'Output:', output_code
        error stop 1
    end if

    ! Check that values component exists in type definition
    if (index(output_code, 'values') == 0) then
        print *, 'FAIL: values component missing from output'
        print *, 'Output:', output_code
        error stop 1
    end if

    ! Check that component access is preserved (v%values not v)
    if (index(output_code, 'v%values') == 0) then
        print *, 'FAIL: component access v%values not preserved'
        print *, 'Output:', output_code
        error stop 1
    end if

    print *, 'PASS: Issue #1738 - derived type with allocatable components'

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

end program test_issue_1738_derived_type_allocatable
