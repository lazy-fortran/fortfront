program test_issue_2871_parameter_stmt_scopes
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    if (.not. test_subroutine_scope()) all_passed = .false.
    if (.not. test_function_scope()) all_passed = .false.
    if (.not. test_module_scope()) all_passed = .false.
    if (.not. test_program_scope_regression()) all_passed = .false.

    if (all_passed) then
        print *, "PASS: Issue #2871 parameter statement scope tests"
    else
        print *, "FAIL: Issue #2871 parameter statement scope tests"
        stop 1
    end if

contains

    logical function has_parameter_binding(output) result(ok)
        character(len=*), intent(in) :: output
        ok = (index(output, ", parameter") > 0) .and. (index(output, "= 1") > 0)
    end function has_parameter_binding

    function test_subroutine_scope() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg

        passed = .true.
        source = "module m" // char(10) // &
            "contains" // char(10) // &
            "  subroutine s" // char(10) // &
            "    integer :: i" // char(10) // &
            "    parameter(i=1)" // char(10) // &
            "    print *, i" // char(10) // &
            "  end subroutine s" // char(10) // &
            "end module m"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "ERROR sub: " // trim(error_msg)
            passed = .false.
            return
        end if

        if (.not. has_parameter_binding(output)) then
            write (error_unit, '(A)') &
                "ERROR: parameter not applied at subroutine scope"
            write (error_unit, '(A)') "Output: " // trim(output)
            passed = .false.
        end if
    end function test_subroutine_scope

    function test_function_scope() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg

        passed = .true.
        source = "module m" // char(10) // &
            "contains" // char(10) // &
            "  integer function f()" // char(10) // &
            "    integer :: i" // char(10) // &
            "    parameter(i=1)" // char(10) // &
            "    f = i" // char(10) // &
            "  end function f" // char(10) // &
            "end module m"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "ERROR func: " // trim(error_msg)
            passed = .false.
            return
        end if

        if (.not. has_parameter_binding(output)) then
            write (error_unit, '(A)') &
                "ERROR: parameter not applied at function scope"
            write (error_unit, '(A)') "Output: " // trim(output)
            passed = .false.
        end if
    end function test_function_scope

    function test_module_scope() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg

        passed = .true.
        source = "module m" // char(10) // &
            "    integer :: i" // char(10) // &
            "    parameter(i=1)" // char(10) // &
            "end module m"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "ERROR mod: " // trim(error_msg)
            passed = .false.
            return
        end if

        if (.not. has_parameter_binding(output)) then
            write (error_unit, '(A)') &
                "ERROR: parameter not applied at module scope"
            write (error_unit, '(A)') "Output: " // trim(output)
            passed = .false.
        end if
    end function test_module_scope

    function test_program_scope_regression() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg

        passed = .true.
        source = "program p" // char(10) // &
            "    integer :: i" // char(10) // &
            "    parameter(i=1)" // char(10) // &
            "    print *, i" // char(10) // &
            "end program p"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "ERROR prog: " // trim(error_msg)
            passed = .false.
            return
        end if

        if (.not. has_parameter_binding(output)) then
            write (error_unit, '(A)') &
                "ERROR: parameter regressed at program scope"
            write (error_unit, '(A)') "Output: " // trim(output)
            passed = .false.
        end if
    end function test_program_scope_regression

end program test_issue_2871_parameter_stmt_scopes
