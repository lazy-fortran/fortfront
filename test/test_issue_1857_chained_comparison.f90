program test_issue_1857_chained_comparison
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    if (test_chained_comparison_output()) then
        print *, 'PASS: Issue #1857 chained comparison detection'
    else
        error stop 'FAIL: Issue #1857 chained comparison not detected'
    end if

contains

    include 'common/cli_io_reader.inc'
    include 'common/read_example.inc'


    logical function test_chained_comparison_output()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_chained_comparison_output = .true.

        call read_example('examples/lf/chained_comparison.lf', &
                          source)

        call transform_lazy_fortran_string(source, output, error_msg)

        if (.not. allocated(output)) then
            write (error_unit, '(A)') 'ERROR: No output generated'
            test_chained_comparison_output = .false.
            return
        end if

        if (index(output, '.and.') == 0) then
            write (error_unit, '(A)') &
                'ERROR: Output missing .and. expansion for chained comparison'
            write (error_unit, '(A)') trim(output)
            test_chained_comparison_output = .false.
            return
        end if

        if (index(output, '1 < x') == 0 .or. index(output, 'x < 10') == 0) then
            write (error_unit, '(A)') &
                'ERROR: Output missing one of the chained comparison bounds'
            write (error_unit, '(A)') trim(output)
            test_chained_comparison_output = .false.
        end if
    end function test_chained_comparison_output

end program test_issue_1857_chained_comparison
