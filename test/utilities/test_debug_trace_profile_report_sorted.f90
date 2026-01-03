program test_debug_trace_profile_report_sorted
    use, intrinsic :: iso_fortran_env, only: error_unit, int64
    use debug_trace, only: trace_enter, trace_leave, trace_profile_reset, &
                           trace_set_profile_enabled, trace_finalize
    implicit none

    character(len=512) :: executable_path
    character(len=32) :: mode
    character(len=256) :: report_path
    character(len=1024) :: command
    integer :: exit_code

    executable_path = ''
    mode = ''
    report_path = ''
    call get_command_argument(0, executable_path)
    call get_command_argument(1, mode)

    if (trim(mode) == 'child') then
        call run_child()
        stop 0
    end if

    if (len_trim(executable_path) == 0) then
        write (error_unit, '(A)') 'FAIL: Could not determine executable path'
        stop 1
    end if

    call make_tmpfile(report_path)
    call run_child_process(executable_path, report_path, exit_code)
    if (exit_code /= 0) then
        write (error_unit, '(A,I0)') 'FAIL: Child process exit code = ', exit_code
        call delete_file(report_path)
        stop 1
    end if

    call assert_profile_report_sorted(report_path)
    call delete_file(report_path)

    print *, 'PASS: debug_trace profile report sorts sections by total time'

contains

    subroutine run_child()
        integer :: rate_default
        integer(int64) :: target_fast, target_slow

        call trace_profile_reset()
        call trace_set_profile_enabled(.true.)

        call system_clock(count_rate=rate_default)
        if (rate_default <= 0) then
            target_fast = 5_int64
            target_slow = 50_int64
        else
            target_fast = max(1_int64, int(rate_default, kind=int64) / 500_int64)
            target_slow = max(target_fast + 1_int64, &
                              int(rate_default, kind=int64) / 50_int64)
        end if

        call trace_enter('profile:fast')
        call burn_counts(target_fast)
        call trace_leave('profile:fast')

        call trace_enter('profile:slow')
        call burn_counts(target_slow)
        call trace_leave('profile:slow')

        call trace_finalize()
    end subroutine run_child

    subroutine burn_counts(target_counts)
        integer(int64), intent(in) :: target_counts
        integer :: start_default
        integer :: now_default
        integer(int64) :: start_count, now_count, elapsed

        start_default = 0
        now_default = 0
        call system_clock(count=start_default)
        start_count = int(start_default, kind=int64)
        elapsed = 0_int64

        do while (elapsed < target_counts)
            call system_clock(count=now_default)
            now_count = int(now_default, kind=int64)
            elapsed = now_count - start_count
            if (elapsed < 0_int64) elapsed = 0_int64
        end do
    end subroutine burn_counts

    subroutine run_child_process(exe_path, out_path, exit_code)
        character(len=*), intent(in) :: exe_path
        character(len=*), intent(in) :: out_path
        integer, intent(out) :: exit_code
        character(len=1024) :: redirect

        redirect = ''
        if (is_windows()) then
            redirect = ' > NUL 2> "' // trim(out_path) // '"'
        else
            redirect = ' > /dev/null 2> "' // trim(out_path) // '"'
        end if

        command = '"' // trim(exe_path) // '" child' // trim(redirect)
        call execute_command_line(trim(command), exitstat=exit_code)
    end subroutine run_child_process

    subroutine assert_profile_report_sorted(path)
        character(len=*), intent(in) :: path
        character(len=2048) :: line
        integer :: unit, ios
        integer :: line_no
        integer :: pos_fast, pos_slow

        pos_fast = 0
        pos_slow = 0
        line_no = 0

        open (newunit=unit, file=trim(path), status='old', action='read', &
              iostat=ios)
        if (ios /= 0) then
            write (error_unit, '(A,1X,A)') 'FAIL: Could not open report:', trim(path)
            stop 1
        end if

        do
            read (unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            line_no = line_no + 1
            if (pos_fast == 0) then
                if (index(line, 'profile:fast') > 0) pos_fast = line_no
            end if
            if (pos_slow == 0) then
                if (index(line, 'profile:slow') > 0) pos_slow = line_no
            end if
        end do
        close (unit)

        if (pos_fast == 0) then
            write (error_unit, '(A)') 'FAIL: Missing profile:fast in report'
            stop 1
        end if
        if (pos_slow == 0) then
            write (error_unit, '(A)') 'FAIL: Missing profile:slow in report'
            stop 1
        end if
        if (pos_slow >= pos_fast) then
            write (error_unit, '(A,I0,A,I0)') &
                'FAIL: Expected slow before fast (slow=', pos_slow, &
                ', fast=', pos_fast, ')'
            stop 1
        end if
    end subroutine assert_profile_report_sorted

    include '../common/cli_system_tests.inc'

end program test_debug_trace_profile_report_sorted
