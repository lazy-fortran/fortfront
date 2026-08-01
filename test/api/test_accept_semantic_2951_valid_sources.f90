program test_accept_semantic_2951_valid_sources
    ! Issue #2951: three valid corpus sources were rejected by the semantic
    ! tightening rules of #2894 and the submodule interface check.
    !
    !   * lfortran intent_01.f90: inside a function, the function name denotes
    !     the result variable, so it is a valid output list item.
    !   * lfortran intrinsics_404.f90: logical8 is an F2023 entity of the
    !     intrinsic module ISO_FORTRAN_ENV.
    !   * lfortran submodule_52.f90: a separate module subprogram need not
    !     repeat the binding label of its interface body.
    !
    ! Each accepted fixture is paired with the rejection it must not weaken.
    use fortfront_compiler, only: compiler_frontend_result_t, &
        compiler_frontend_options_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD, OPERATING_MODE_STRICT
    implicit none

    logical :: all_passed

    all_passed = .true.

    call check_result_variable_output(all_passed)
    call check_logical_kind_import(all_passed)
    call check_module_procedure_binding(all_passed)

    if (all_passed) then
        print *, 'All issue 2951 acceptance tests passed'
    else
        print *, 'Issue 2951 acceptance tests FAILED'
        stop 1
    end if

contains

    function nl() result(c)
        character(len=1) :: c

        c = char(10)
    end function nl

    ! The function name inside its own body is the result variable.
    subroutine check_result_variable_output(passed)
        logical, intent(inout) :: passed

        call expect_accepted('intent_01', &
            'module dflt_intent'//nl()// &
            'contains'//nl()// &
            'subroutine foo(c, d)'//nl()// &
            'real :: c, d, e, g'//nl()// &
            'e = f(c)'//nl()// &
            'g = f(d)'//nl()// &
            'contains'//nl()// &
            'real function f(x)'//nl()// &
            'real, intent(in) :: x'//nl()// &
            'f = 2*x'//nl()// &
            'print *, f'//nl()// &
            'end function f'//nl()// &
            'end subroutine foo'//nl()// &
            'end module'//nl(), passed)
        ! A procedure name used outside its own body stays a rejection.
        call expect_rejected('pr95690', &
            'module m'//nl()// &
            'contains'//nl()// &
            '   subroutine s'//nl()// &
            '      print *, (erfc)'//nl()// &
            '   end'//nl()// &
            '   function erfc()'//nl()// &
            '   end'//nl()// &
            'end'//nl(), passed)
    end subroutine check_result_variable_output

    ! logical8 is a standard ISO_FORTRAN_ENV entity since F2023.
    subroutine check_logical_kind_import(passed)
        logical, intent(inout) :: passed

        call expect_accepted('intrinsics_404', &
            'program intrinsics_401'//nl()// &
            '    use iso_fortran_env, only: logical8'//nl()// &
            '    implicit none'//nl()// &
            '    logical(kind=logical8) :: x'//nl()// &
            '    x = .true.'//nl()// &
            '    if (.not. x) error stop'//nl()// &
            'end program intrinsics_401'//nl(), passed)
        ! Unsigned kind names remain a vendor extension, not an entity.
        call expect_rejected('unsigned_37', &
            'program main'//nl()// &
            '  use iso_fortran_env, only : uint32'//nl()// &
            'end program main'//nl(), passed)
    end subroutine check_logical_kind_import

    ! A MODULE PROCEDURE body carries no binding label of its own.
    subroutine check_module_procedure_binding(passed)
        logical, intent(inout) :: passed

        call expect_accepted('submodule_52', &
            'module submodule_52_m'//nl()// &
            '  implicit none'//nl()// &
            '  interface'//nl()// &
            '    module integer function bar(x) bind(C, name="bar_c")'//nl()// &
            '      integer, intent(in) :: x'//nl()// &
            '    end function'//nl()// &
            '  end interface'//nl()// &
            'end module'//nl()// &
            'submodule(submodule_52_m) submodule_52_m_sub'//nl()// &
            '  implicit none'//nl()// &
            'contains'//nl()// &
            '  module procedure bar'//nl()// &
            '    bar = x + 1'//nl()// &
            '  end procedure'//nl()// &
            'end submodule'//nl(), passed)
        ! A restated but different binding label is still a mismatch.
        call expect_rejected('submodule binding mismatch', &
            'module m'//nl()// &
            '  implicit none'//nl()// &
            '  interface'//nl()// &
            '    module subroutine foo() bind(C, name="foo_c")'//nl()// &
            '    end subroutine'//nl()// &
            '  end interface'//nl()// &
            'end module'//nl()// &
            'submodule(m) m_sub'//nl()// &
            '  implicit none'//nl()// &
            'contains'//nl()// &
            '  module subroutine foo() bind(C, name="other_c")'//nl()// &
            '  end subroutine'//nl()// &
            'end submodule'//nl(), passed)
    end subroutine check_module_procedure_binding

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

    subroutine run_frontend(source, result)
        character(len=*), intent(in) :: source
        type(compiler_frontend_result_t), intent(out) :: result
        type(compiler_frontend_options_t) :: options

        options%input_mode = INPUT_MODE_STANDARD
        options%operating_mode = OPERATING_MODE_STRICT
        call compile_frontend_from_string(source, result, options)
    end subroutine run_frontend

end program test_accept_semantic_2951_valid_sources
