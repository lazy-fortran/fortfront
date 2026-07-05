program test_issue_2846_trailing_comments_control_flow
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: errors

    call read_example('examples/f90/trailing_comments_control_flow.f90', source)
    call transform_lazy_fortran_string(source, output, errors)

    if (len_trim(errors) > 0) then
        print *, "Unexpected errors:", trim(errors)
        error stop 1
    end if

    ! Trailing comments on end do and end if are preserved (appear on separate lines)
    ! Note: trailing comments on DO/IF headers and inside bodies require
    ! control flow parser changes to capture them properly.
    if (index(output, '! End loop') == 0) then
        print *, "ERROR: Comment 'End loop' was dropped"
        print *, "Output:"
        print *, output
        error stop 1
    end if

    if (index(output, '! End check') == 0) then
        print *, "ERROR: Comment 'End check' was dropped"
        print *, "Output:"
        print *, output
        error stop 1
    end if

    print *, "PASS: Trailing comments on control flow terminators preserved"

    contains

    include '../common/read_example.inc'
end program test_issue_2846_trailing_comments_control_flow
