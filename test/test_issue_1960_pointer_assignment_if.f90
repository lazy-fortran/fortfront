program test_issue_1960_pointer_assignment_if
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use transformation_api, only: transform_with_context, transform_context_t
    use frontend_transformation, only: INPUT_MODE_STANDARD
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: context
    logical :: has_pointer_line
    logical :: has_print_line
    logical :: pointer_before_print
    logical :: pointer_before_end
    integer :: pointer_pos
    integer :: print_pos
    integer :: end_if_pos

    print *, "=== Issue #1960: preserve pointer assignment inside IF blocks ==="

    call read_example('examples/f90/issue_1960_pointer_assignment_if.f90', &
                      input_code)

    context%input_mode = INPUT_MODE_STANDARD
    context%has_filename = .true.
    context%source_name = "test_issue_1960_input"

    call transform_with_context(input_code, output_code, error_msg, context)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: transform_with_context returned error:", trim(error_msg)
        error stop 1
    end if

    pointer_pos = index(output_code, "    p => values(1)")
    print_pos = index(output_code, "        print *, 'p is now null'")
    end_if_pos = index(output_code, "    end if")

    has_pointer_line = pointer_pos > 0
    has_print_line = print_pos > 0
    pointer_before_print = has_pointer_line .and. has_print_line .and. pointer_pos < print_pos
    pointer_before_end = has_pointer_line .and. end_if_pos > 0 .and. pointer_pos < end_if_pos

    if (.not. has_pointer_line) then
        print *, "FAIL: pointer assignment line missing from transformed output"
        print *, trim(output_code)
        error stop 1
    end if

    if (.not. has_print_line) then
        print *, "FAIL: print statement missing from transformed output"
        print *, trim(output_code)
        error stop 1
    end if

    if (.not. pointer_before_print) then
        print *, "FAIL: pointer assignment not positioned before print within IF block"
        print *, trim(output_code)
        error stop 1
    end if

    if (.not. pointer_before_end) then
        print *, "FAIL: pointer assignment moved outside IF block"
        print *, trim(output_code)
        error stop 1
    end if

    print *, "PASS: pointer assignment preserved inside IF block"


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_1960_pointer_assignment_if
