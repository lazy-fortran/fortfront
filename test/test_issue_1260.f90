program test_issue_1260
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    logical :: test_passed

    test_passed = test_subroutine_parsing()

    if (test_passed) then
        print *, 'PASS: Issue #1260 subroutine parsing - no extra unnamed_subroutine'
    else
        print *, 'FAIL: Issue #1260 - extra unnamed_subroutine generated'
        stop 1
    end if

contains

    include 'common/read_example.inc'


    function test_subroutine_parsing() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        passed = .true.

        call read_example('examples/lf/issue_1260_subroutine_no_name.lf', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'ERROR: Failed to parse unnamed end case: ' &
                // trim(error_msg)
            passed = .false.
        else if (.not. allocated(output)) then
            write (error_unit, '(A)') 'ERROR: No output generated for unnamed end'
            passed = .false.
        else if (index(output, 'unnamed_subroutine') /= 0) then
            write (error_unit, '(A)') &
                'ERROR: Found extra unnamed_subroutine in output'
            passed = .false.
        end if

        call read_example('examples/lf/issue_1260_subroutine_named_end.lf', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'ERROR: Failed to parse named end case: ' &
                // trim(error_msg)
            passed = .false.
        else if (.not. allocated(output)) then
            write (error_unit, '(A)') 'ERROR: No output generated for named end'
            passed = .false.
        else if (index(output, 'unnamed_subroutine') /= 0) then
            write (error_unit, '(A)') &
                'ERROR: Found extra unnamed_subroutine with named end'
            passed = .false.
        end if

    end function test_subroutine_parsing

end program test_issue_1260
