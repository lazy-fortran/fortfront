program test_reject_literal_01_diagnostics
    ! Issue #2894: malformed or disallowed literal forms must be rejected with a
    ! source diagnostic, while the corrected neighboring form stays accepted.
    !
    ! Negative fixtures mirror the gfortran.dg sources kind_tests_4.f90,
    ! pr95690.f90, unexpected_eof_2.f90, unexpected_eof_3.f90 and
    ! unsigned_37.f90. Each is paired with a corrected neighbor.
    use fortfront_compiler, only: compiler_frontend_result_t, &
        compiler_frontend_options_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, OPERATING_MODE_STRICT
    implicit none

    logical :: all_passed

    all_passed = .true.

    call check_kind_tests_4(all_passed)
    call check_pr95690(all_passed)
    call check_unexpected_eof_2(all_passed)
    call check_unexpected_eof_3(all_passed)
    call check_unsigned_37(all_passed)

    if (all_passed) then
        print *, 'All literal form rejection tests passed'
    else
        print *, 'Literal form rejection tests FAILED'
        stop 1
    end if

contains

    function nl() result(c)
        character(len=1) :: c

        c = char(10)
    end function nl

    ! An undefined named kind parameter leaves the literal without a kind.
    subroutine check_kind_tests_4(passed)
        logical, intent(inout) :: passed

        call expect_rejected('kind_tests_4', &
            'rPos=0.0_dp'//nl()// &
            'end'//nl(), passed)
        call expect_accepted('kind_tests_4 corrected', &
            'program p'//nl()// &
            '    integer, parameter :: dp = kind(1.0d0)'//nl()// &
            '    real(dp) :: rpos'//nl()// &
            '    rpos = 0.0_dp'//nl()// &
            'end program p'//nl(), passed)
    end subroutine check_kind_tests_4

    ! A procedure name is not a constant, so it is not a valid output item.
    subroutine check_pr95690(passed)
        logical, intent(inout) :: passed

        call expect_rejected('pr95690', &
            'module m'//nl()// &
            'contains'//nl()// &
            '   subroutine s'//nl()// &
            '      print *, (erfc)'//nl()// &
            '   end'//nl()// &
            '   function erfc()'//nl()// &
            '   end'//nl()// &
            'end'//nl(), passed)
        call expect_accepted('pr95690 corrected', &
            'module m'//nl()// &
            'contains'//nl()// &
            '   subroutine s'//nl()// &
            '      real :: value'//nl()// &
            '      value = 1.0'//nl()// &
            '      print *, value'//nl()// &
            '   end subroutine s'//nl()// &
            'end module m'//nl(), passed)
    end subroutine check_pr95690

    ! A character constant that is never closed runs off the end of the file.
    subroutine check_unexpected_eof_2(passed)
        logical, intent(inout) :: passed

        call expect_rejected('unexpected_eof_2', &
            'program p'//nl()// &
            '   character(8) :: z'//nl()// &
            '   z = ''abc&  ! unterminated'//nl(), passed)
        call expect_accepted('unexpected_eof_2 corrected', &
            'program p'//nl()// &
            '   character(8) :: z'//nl()// &
            '   z = ''abc'''//nl()// &
            'end program p'//nl(), passed)
    end subroutine check_unexpected_eof_2

    ! Same rule in an initializer rather than an assignment.
    subroutine check_unexpected_eof_3(passed)
        logical, intent(inout) :: passed

        call expect_rejected('unexpected_eof_3', &
            'program p'//nl()// &
            '   character(8) :: z = ''abc& ! unterminated'//nl(), passed)
        call expect_accepted('unexpected_eof_3 corrected', &
            'program p'//nl()// &
            '   character(8) :: z = ''abc'''//nl()// &
            'end program p'//nl(), passed)
    end subroutine check_unexpected_eof_3

    ! Unsigned kind names are not entities of ISO_FORTRAN_ENV.
    subroutine check_unsigned_37(passed)
        logical, intent(inout) :: passed

        call expect_rejected('unsigned_37', &
            'program main'//nl()// &
            '  use iso_fortran_env, only : uint32'//nl()// &
            'end program main'//nl(), passed)
        call expect_accepted('unsigned_37 corrected', &
            'program main'//nl()// &
            '  use iso_fortran_env, only : int32'//nl()// &
            '  integer(int32) :: value'//nl()// &
            '  value = 1_int32'//nl()// &
            'end program main'//nl(), passed)
    end subroutine check_unsigned_37

    ! A rejected fixture must fail the frontend and carry a source diagnostic.
    subroutine expect_rejected(name, source, passed)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: source
        logical, intent(inout) :: passed
        type(compiler_frontend_result_t) :: result
        logical :: rejected, has_message

        call run_frontend(source, result)
        rejected = .not. (result%parse_ok .and. result%semantic_ok)
        has_message = .false.
        if (allocated(result%error_msg)) has_message = len_trim(result%error_msg) > 0

        if (rejected .and. has_message) then
            print *, 'PASS: rejected ', name
        else
            print *, 'FAIL: expected rejection with a diagnostic for ', name
            passed = .false.
        end if
    end subroutine expect_rejected

    ! The corrected neighbor of every rule must still compile.
    subroutine expect_accepted(name, source, passed)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: source
        logical, intent(inout) :: passed
        type(compiler_frontend_result_t) :: result

        call run_frontend(source, result)
        if (result%parse_ok .and. result%semantic_ok) then
            print *, 'PASS: accepted ', name
        else
            print *, 'FAIL: expected acceptance for ', name
            if (allocated(result%error_msg)) print *, '  ', trim(result%error_msg)
            passed = .false.
        end if
    end subroutine expect_accepted

    subroutine run_frontend(source, result)
        character(len=*), intent(in) :: source
        type(compiler_frontend_result_t), intent(out) :: result
        type(compiler_frontend_options_t) :: options

        options%input_mode = INPUT_MODE_STANDARD
        options%operating_mode = OPERATING_MODE_STRICT
        call compile_frontend_from_string(source, result, options)
    end subroutine run_frontend

end program test_reject_literal_01_diagnostics
