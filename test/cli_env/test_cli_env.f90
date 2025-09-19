program test_cli_env
    use, intrinsic :: iso_fortran_env, only: error_unit
    use cli_env, only: compute_cli_trace_settings
    implicit none

    logical :: enabled
    character(len=:), allocatable :: path

    call compute_cli_trace_settings('', '', enabled, path)
    if (enabled .or. trim(path) /= 'cli_trace.txt') then
        write(error_unit, '(A,1X,L1,1X,A)') 'FAIL: default settings', enabled, trim(path)
        stop 1
    else
        write(*, '(A)') 'PASS: default disabled with default path'
    end if

    call compute_cli_trace_settings('1', '', enabled, path)
    if (.not. enabled) then
        write(error_unit, '(A)') 'FAIL: truthy 1 should enable'
        stop 1
    else
        write(*, '(A)') 'PASS: truthy 1 enables trace'
    end if

    call compute_cli_trace_settings('false', '', enabled, path)
    if (enabled) then
        write(error_unit, '(A)') 'FAIL: false should disable'
        stop 1
    else
        write(*, '(A)') 'PASS: false disables trace'
    end if

    call compute_cli_trace_settings('TRUE', '', enabled, path)
    if (.not. enabled) then
        write(error_unit, '(A)') 'FAIL: TRUE should enable (case-insensitive)'
        stop 1
    else
        write(*, '(A)') 'PASS: TRUE enables (case-insensitive)'
    end if

    call compute_cli_trace_settings('on', 'my_log.txt', enabled, path)
    if (.not. enabled .or. trim(path) /= 'my_log.txt') then
        write(error_unit, '(A,1X,L1,1X,A)') 'FAIL: file override with on', enabled, trim(path)
        stop 1
    else
        write(*, '(A)') 'PASS: file override respected when enabled'
    end if

    stop 0
end program test_cli_env

