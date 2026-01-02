program test_issue_2019_statements_after_if_dropped
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                                                              iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    use lexer_core, only: to_lower
    implicit none

    character(len=:), allocatable :: input_text
    character(len=:), allocatable :: output_text
    character(len=:), allocatable :: lower_output_text
    character(len=:), allocatable :: error_msg

    call read_example('examples/lf/issue_2019_statements_after_if_dropped.lf', &
                      input_text)

    call transform_lazy_fortran_string(input_text, output_text, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: transformation reported error:'
            print *, trim(error_msg)
            error stop 1
        end if
    end if

    if (.not. allocated(output_text)) then
        print *, 'FAIL: no output produced for issue_2019 example'
        error stop 1
    end if

    lower_output_text = to_lower(output_text)

    if (index(lower_output_text, 'max_val') == 0) then
        print *, 'FAIL: variable max_val is missing from output'
        print *, 'Expected to find max_val declaration and usage'
        print *, 'Output:'
        print *, trim(output_text)
        error stop 1
    end if

    if (index(lower_output_text, 'max_val = x') == 0) then
        print *, 'FAIL: assignment max_val = x is missing from output'
        print *, 'Statement after first IF block was dropped'
        print *, 'Output:'
        print *, trim(output_text)
        error stop 1
    end if

    if (index(lower_output_text, 'if (y > max_val)') == 0 .and. &
        index(lower_output_text, 'if(y > max_val)') == 0) then
        print *, 'FAIL: second IF statement is missing from output'
        print *, 'Expected: if (y > max_val) then'
        print *, 'Output:'
        print *, trim(output_text)
        error stop 1
    end if

    if (index(lower_output_text, 'maximum') == 0) then
        print *, 'FAIL: print statement with Maximum is missing from output'
        print *, 'Expected: print *, Maximum:, max_val'
        print *, 'Output:'
        print *, trim(output_text)
        error stop 1
    end if

    print *, 'PASS: issue_2019 all statements after IF block are preserved'


contains

    include '../../common/cli_io_reader.inc'

    include '../../common/read_example.inc'
end program test_issue_2019_statements_after_if_dropped
