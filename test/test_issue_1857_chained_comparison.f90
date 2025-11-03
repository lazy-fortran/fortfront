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

    logical function test_chained_comparison_output()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_chained_comparison_output = .true.

        call read_example('examples/lf/issue_1857_chained_comparison.lf', &
                          source)

        call transform_lazy_fortran_string(source, output, error_msg)

        if (.not. allocated(output)) then
            write (error_unit, '(A)') 'ERROR: No output generated'
            test_chained_comparison_output = .false.
            return
        end if

        if (index(output, '< 10') /= 0) then
            write (error_unit, '(A)') &
                "ERROR: Output contains '< 10', chained comparison not truncated"
            write (error_unit, '(A)') trim(output)
            test_chained_comparison_output = .false.
        else if (index(output, 'result = 1 < x') == 0) then
            write (error_unit, '(A)') &
                'ERROR: Output missing expected partial expression'
            write (error_unit, '(A)') trim(output)
            test_chained_comparison_output = .false.
        end if
    end function test_chained_comparison_output

end program test_issue_1857_chained_comparison
