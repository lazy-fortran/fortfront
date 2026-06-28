program test_frontend_conformance_smoke
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=2048) :: command
    integer :: exit_code
    logical :: is_windows

    is_windows = check_if_windows()
    if (is_windows) then
        print *, 'SKIP: frontend conformance smoke is shell-based on Unix'
        stop 0
    end if

    command = 'sh -c "rm -rf /tmp/fortfront_conf_smoke_gcc ' // &
        '/tmp/fortfront_conf_smoke_lfortran && ' // &
        'FF_GFORTRAN_DG_DIR=/tmp/fortfront_conf_smoke_gcc/gcc/testsuite/gfortran.dg ' // &
        'FF_LFORTRAN_DIR=/tmp/fortfront_conf_smoke_lfortran ' // &
        'scripts/run_frontend_conformance.sh --suite all ' // &
        '--report /tmp/fortfront_conf_smoke.jsonl"'

    call execute_command_line(trim(command), exitstat=exit_code)
    if (exit_code /= 0) then
        write (error_unit, '(A,I0)') &
            'FAIL: frontend conformance smoke exited ', exit_code
        stop 1
    end if

    print *, 'PASS: frontend conformance wrapper skips absent external suites'

contains

    logical function check_if_windows()
        character(len=16) :: value
        integer :: status

        check_if_windows = .false.
        call get_environment_variable('OS', value, status=status)
        if (status == 0 .and. len_trim(value) >= 7) then
            if (value(1:7) == 'Windows') then
                check_if_windows = .true.
                return
            end if
        end if

        call get_environment_variable('WINDIR', value, status=status)
        if (status == 0) check_if_windows = .true.
    end function check_if_windows

end program test_frontend_conformance_smoke
