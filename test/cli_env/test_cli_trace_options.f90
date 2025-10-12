program test_cli_trace_options
    use, intrinsic :: iso_fortran_env, only: error_unit
    use cli_env, only: parse_trace_option, parse_trace_flag_value
    implicit none

    call test_is_trace_option()
    call test_parse_trace_flag_value()
    print *, 'PASS: CLI trace option parsing'
    stop 0

contains

    subroutine test_is_trace_option()
        logical :: rec, is_file
        character(len=:), allocatable :: val

        call parse_trace_option('--trace', rec, is_file, val)
        if (.not. rec .or. is_file .or. len_trim(val) /= 0) then
            write (error_unit, '(A)') 'FAIL: --trace recognition'
            stop 1
        end if

        call parse_trace_option('--trace=off', rec, is_file, val)
        if (.not. rec .or. is_file .or. trim(val) /= 'off') then
            write (error_unit, '(A)') 'FAIL: --trace=off recognition'
            stop 1
        end if

        call parse_trace_option('--trace-file=log.txt', rec, is_file, val)
        if (.not. rec .or. .not. is_file .or. trim(val) /= 'log.txt') then
            write (error_unit, '(A)') 'FAIL: --trace-file=log.txt recognition'
            stop 1
        end if

        call parse_trace_option('-x', rec, is_file, val)
        if (rec) then
            write (error_unit, '(A)') 'FAIL: random option should not be recognized'
            stop 1
        end if
    end subroutine test_is_trace_option

    subroutine test_parse_trace_flag_value()
        if (.not. parse_trace_flag_value('')) then
            write (error_unit, '(A)') 'FAIL: empty should be truthy for --trace'
            stop 1
        end if
        if (parse_trace_flag_value('off')) then
            write (error_unit, '(A)') 'FAIL: off should be false'
            stop 1
        end if
        if (.not. parse_trace_flag_value('on')) then
            write (error_unit, '(A)') 'FAIL: on should be true'
            stop 1
        end if
        if (.not. parse_trace_flag_value('TRUE')) then
            write (error_unit, '(A)') 'FAIL: TRUE should be true'
            stop 1
        end if
        if (parse_trace_flag_value('0')) then
            write (error_unit, '(A)') 'FAIL: 0 should be false'
            stop 1
        end if
    end subroutine test_parse_trace_flag_value

end program test_cli_trace_options
