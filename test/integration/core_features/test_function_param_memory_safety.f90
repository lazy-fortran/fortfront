program test_function_param_memory_safety
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string_with_format, &
                         format_options_t
    implicit none

    character(len=:), allocatable :: input
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    type(format_options_t) :: options
    logical :: test_passed

    call read_example('examples/lf/function_param_memory_safety.lf', input)

    test_passed = .true.
    call transform_lazy_fortran_string_with_format(input, output, error_msg, &
                                                   options)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'Error: ' // trim(error_msg)
        test_passed = .false.
    else
        if (index(output, 'Unparsed statement') > 0) then
            write (error_unit, '(A)') &
                'INFO: parser limitation triggered (Unparsed statement)'
        end if

        if (index(output, 'x') == 0 .or. index(output, 'y') == 0 .or. &
            index(output, 'z') == 0 .or. index(output, 'alpha') == 0 .or. &
            index(output, 'beta') == 0 .or. index(output, 'gamma') == 0) then
            write (error_unit, '(A)') 'FAIL: Missing parameters'
            write (error_unit, '(A)') trim(output)
            test_passed = .false.
        end if

        if (index(output, 'result(res)') == 0) then
            write (error_unit, '(A)') &
                'WARNING: Missing result clause (tracked separately)'
        end if

        block
            integer :: i
            logical :: has_garbage
            has_garbage = .false.
            do i = 1, len(output)
                if (iachar(output(i:i)) < 32 .and. iachar(output(i:i)) /= 10) then
                    has_garbage = .true.
                    exit
                end if
            end do
            if (has_garbage) then
                write (error_unit, '(A)') &
                    'FAIL: Contains garbage/non-printable characters'
                test_passed = .false.
            end if
        end block
    end if

    if (test_passed) then
        print *, 'PASS: function parameter list emitted without corruption'
        stop 0
    else
        error stop 'FAIL: parameter list corruption detected'
    end if


contains

    include '../../common/cli_io_reader.inc'

    include '../../common/read_example.inc'
end program test_function_param_memory_safety
