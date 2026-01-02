program test_issue_957_parameter_attributes
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    logical :: test_passed

    test_passed = test_parameter_attributes()

    if (test_passed) then
        print *, 'PASS: Issue #957 parameter attributes test'
    else
        print *, 'FAIL: Issue #957 parameter attributes test'
        stop 1
    end if

contains

    include 'common/read_example.inc'


    function test_parameter_attributes() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        passed = .true.

        call read_example('examples/lf/issue_957_parameter_attributes.lf', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'ERROR: Failed to parse: ' // trim(error_msg)
            passed = .false.
        else if (.not. allocated(output)) then
            write (error_unit, '(A)') 'ERROR: No output generated'
            passed = .false.
        else
            if (index(output, 'intent(in)') == 0) then
                write (error_unit, '(A)') &
                    "ERROR: Parameter 'required' should have intent(in)"
                passed = .false.
            end if

            if (index(output, 'intent(out)') == 0) then
                write (error_unit, '(A)') &
                    "ERROR: Parameter 'output' should have intent(out)"
                passed = .false.
            end if

            if (index(output, 'optional') == 0) then
                write (error_unit, '(A)') &
                    "ERROR: Parameter 'opt' should be optional"
                passed = .false.
            end if
        end if

    end function test_parameter_attributes

end program test_issue_957_parameter_attributes
