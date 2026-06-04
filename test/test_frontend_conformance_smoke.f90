program test_frontend_conformance_smoke
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=2048) :: command
    integer :: exit_code

    command = 'sh -c "rm -rf /tmp/fortfront_conf_smoke_gcc ' // &
              '/tmp/fortfront_conf_smoke_lfortran && ' // &
              'scripts/run_frontend_conformance.sh --suite all ' // &
              '--gcc-root /tmp/fortfront_conf_smoke_gcc ' // &
              '--lfortran-root /tmp/fortfront_conf_smoke_lfortran ' // &
              '--report /tmp/fortfront_conf_smoke.jsonl"'

    call execute_command_line(trim(command), exitstat=exit_code)
    if (exit_code /= 0) then
        write (error_unit, '(A,I0)') &
            'FAIL: frontend conformance smoke exited ', exit_code
        stop 1
    end if

    print *, 'PASS: frontend conformance wrapper skips absent external suites'
end program test_frontend_conformance_smoke
