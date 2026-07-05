program test_issue_2846_trailing_comments
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: errors

    call read_example('examples/f90/trailing_comments.f90', source)
    call transform_lazy_fortran_string(source, output, errors)

    if (len_trim(errors) > 0) then
        print *, "Unexpected errors:", trim(errors)
        error stop 1
    end if

    ! Check that trailing comments are preserved on each line
    if (index(output, '! This is the implicit none statement') == 0) then
        print *, "ERROR: Trailing comment on implicit none was dropped"
        print *, "Output:"
        print *, output
        error stop 1
    end if

    if (index(output, '! Variable declaration for x') == 0) then
        print *, "ERROR: Trailing comment on integer declaration was dropped"
        print *, "Output:"
        print *, output
        error stop 1
    end if

    if (index(output, '! Variable declaration for y') == 0) then
        print *, "ERROR: Trailing comment on real declaration was dropped"
        print *, "Output:"
        print *, output
        error stop 1
    end if

    if (index(output, '! Assign one to x') == 0) then
        print *, "ERROR: Trailing comment on assignment was dropped"
        print *, "Output:"
        print *, output
        error stop 1
    end if

    if (index(output, '! Assign two to y') == 0) then
        print *, "ERROR: Trailing comment on real assignment was dropped"
        print *, "Output:"
        print *, output
        error stop 1
    end if

    if (index(output, '! Print both variables') == 0) then
        print *, "ERROR: Trailing comment on print statement was dropped"
        print *, "Output:"
        print *, output
        error stop 1
    end if

    print *, "PASS: All trailing comments preserved correctly"

contains

    include '../common/read_example.inc'
end program test_issue_2846_trailing_comments
