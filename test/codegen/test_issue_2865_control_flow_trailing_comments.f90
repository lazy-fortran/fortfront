program test_issue_2865_control_flow_trailing_comments
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

    call assert_present(output, 'do i = 1, 10 ! Loop from 1 to 10', 'DO header')
    call assert_present(output, 'sum = sum + i ! Accumulate', 'DO body')
    call assert_present(output, 'if (sum > 50) then ! Check threshold', 'IF header')
    call assert_present(output, '"Large sum" ! Print message', 'IF body')

    print *, "PASS: Trailing comments on DO/IF headers and bodies preserved"

contains

    subroutine assert_present(text, needle, label)
        character(len=*), intent(in) :: text, needle, label
        if (index(text, needle) == 0) then
            print *, "ERROR: "//label//" not preserved"
            print *, "Expected substring: "//needle
            print *, "Output:"
            print *, text
            error stop 1
        end if
    end subroutine assert_present

    include '../common/read_example.inc'
end program test_issue_2865_control_flow_trailing_comments
