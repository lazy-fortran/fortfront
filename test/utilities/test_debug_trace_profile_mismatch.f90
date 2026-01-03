program test_debug_trace_profile_mismatch
    use, intrinsic :: iso_fortran_env, only: error_unit
    use debug_trace, only: trace_enter, trace_leave, trace_profile_reset, &
                           trace_set_profile_enabled
    implicit none

    character(len=512) :: executable_path
    character(len=32) :: mode
    character(len=1024) :: command
    integer :: exit_code

    executable_path = ''
    mode = ''
    call get_command_argument(0, executable_path)
    call get_command_argument(1, mode)

    if (trim(mode) == 'child') then
        call trace_profile_reset()
        call trace_set_profile_enabled(.true.)
        call trace_enter('profile:outer')
        call trace_leave('profile:wrong')
        write (error_unit, '(A)') 'FAIL: Expected trace_leave mismatch to stop'
        stop 1
    end if

    if (len_trim(executable_path) == 0) then
        write (error_unit, '(A)') 'FAIL: Could not determine executable path'
        stop 1
    end if

    command = '"' // trim(executable_path) // '" child'
    if (is_windows()) then
        command = trim(command) // ' > NUL 2>&1'
    else
        command = trim(command) // ' > /dev/null 2>&1'
    end if

    call execute_command_line(trim(command), exitstat=exit_code)
    if (exit_code == 0) then
        write (error_unit, '(A)') 'FAIL: Expected mismatch to return nonzero'
        stop 1
    end if

    print *, 'PASS: debug_trace profiling validates enter/leave names'

contains

    include '../common/cli_system_tests.inc'

end program test_debug_trace_profile_mismatch
