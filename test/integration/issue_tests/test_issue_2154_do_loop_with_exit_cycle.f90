program test_issue_2154_do_loop_with_exit_cycle
    ! Test for issue #2154: Statements after DO loop with EXIT/CYCLE silently deleted
    ! This test verifies that print statements after a DO loop containing EXIT/CYCLE
    ! are correctly parsed and included in the output.
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                              iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    use lexer_core, only: to_lower
    implicit none

    character(len=:), allocatable :: input_text
    character(len=:), allocatable :: output_text
    character(len=:), allocatable :: lower_output_text
    character(len=:), allocatable :: error_msg

    ! Read the example file
    call read_example('examples/lf/issue_playtest5_print_after_loop_deleted.lf', &
                      input_text)

    ! Transform the lazy fortran code
    call transform_lazy_fortran_string(input_text, output_text, error_msg)

    ! Check that transformation succeeded
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: transformation reported error:'
            print *, trim(error_msg)
            error stop 1
        end if
    end if

    if (.not. allocated(output_text)) then
        print *, 'FAIL: no output produced for issue_2154 example'
        error stop 1
    end if

    lower_output_text = to_lower(output_text)

    ! Verify the print statement is present in the output
    if (index(lower_output_text, 'print') == 0) then
        print *, 'FAIL: print statement missing from output'
        print *, 'The statement after DO loop with EXIT/CYCLE was silently deleted'
        print *, 'Output:'
        print *, trim(output_text)
        error stop 1
    end if

    ! Verify "Sum:" appears in the output (from the print statement)
    if (index(lower_output_text, 'sum') == 0) then
        print *, 'FAIL: Sum variable missing from print statement'
        print *, 'Output:'
        print *, trim(output_text)
        error stop 1
    end if

    ! Verify the DO loop is present
    if (index(lower_output_text, 'do ') == 0 .and. index(lower_output_text, 'do i') == 0) then
        print *, 'FAIL: DO loop missing from output'
        print *, 'Output:'
        print *, trim(output_text)
        error stop 1
    end if

    ! Verify EXIT is present
    if (index(lower_output_text, 'exit') == 0) then
        print *, 'FAIL: EXIT statement missing from output'
        print *, 'Output:'
        print *, trim(output_text)
        error stop 1
    end if

    ! Verify CYCLE is present
    if (index(lower_output_text, 'cycle') == 0) then
        print *, 'FAIL: CYCLE statement missing from output'
        print *, 'Output:'
        print *, trim(output_text)
        error stop 1
    end if

    print *, 'PASS: issue_2154 statements after DO loop with EXIT/CYCLE are preserved'


contains

    include '../../common/cli_io_reader.inc'

    include '../../common/read_example.inc'
end program test_issue_2154_do_loop_with_exit_cycle
