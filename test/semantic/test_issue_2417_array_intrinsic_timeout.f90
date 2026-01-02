program test_issue_2417_array_intrinsic_timeout
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, &
                                             iostat_end, iostat_eor
    implicit none
    character(len=:), allocatable :: source, output, error_msg

    ! Test minloc with mask - this should not timeout
    call read_example('examples/f90/issue_2417_minloc_with_mask.f90', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: Transformation failed: ' // error_msg
            stop 1
        end if
    end if

    ! Verify output contains expected elements
    call assert_contains(output, 'program test_minloc_timeout', &
                         'Expected program name')
    call assert_contains(output, 'minloc', &
                         'Expected minloc intrinsic call')

    write (*, '(A)') 'PASS: array intrinsic timeout test (issue 2417)'

contains

    include '../common/cli_io_reader.inc'
    include '../common/read_example.inc'

    subroutine assert_contains(text, pattern, message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: message

        if (index(text, pattern) == 0) then
            write (error_unit, '(A)') 'FAIL: ' // message
            write (error_unit, '(A)') 'Pattern: ' // trim(pattern)
            write (error_unit, '(A)') 'Output:'
            write (error_unit, '(A)') text
            stop 1
        end if
    end subroutine assert_contains



end program test_issue_2417_array_intrinsic_timeout
