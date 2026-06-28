program test_issue_2487_parameter_statement
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    if (.not. test_simple_parameter()) all_passed = .false.
    if (.not. test_character_array_parameter()) all_passed = .false.
    if (.not. test_parameter_in_explicit_program()) all_passed = .false.

    if (all_passed) then
        print *, "PASS: Issue #2487 parameter statement tests"
    else
        print *, "FAIL: Issue #2487 parameter statement tests"
        stop 1
    end if

contains

    include 'common/read_example.inc'


    function test_simple_parameter() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg

        passed = .true.
        source = "integer :: x" // char(10) // &
            "PARAMETER(X = 5)" // char(10) // &
            "print *, x"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "ERROR: Failed to parse: " // trim(error_msg)
            passed = .false.
            return
        end if

        if (index(output, ", parameter") == 0) then
            write (error_unit, '(A)') &
                "ERROR: Expected 'parameter' attribute in declaration"
            write (error_unit, '(A)') "Output: " // trim(output)
            passed = .false.
        end if

        if (index(output, "= 5") == 0) then
            write (error_unit, '(A)') &
                "ERROR: Expected initializer '= 5' in declaration"
            write (error_unit, '(A)') "Output: " // trim(output)
            passed = .false.
        end if
    end function test_simple_parameter

    function test_character_array_parameter() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg

        passed = .true.
        call read_example("examples/f90/issue_2487_parameter_statement.f90", source)

        call transform_lazy_fortran_string(source, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "ERROR: Failed to parse: " // trim(error_msg)
            passed = .false.
            return
        end if

        if (index(output, ", parameter :: x = 5") == 0) then
            write (error_unit, '(A)') &
                "ERROR: Expected 'parameter :: x = 5'"
            write (error_unit, '(A)') "Output: " // trim(output)
            passed = .false.
        end if

        if (index(output, ", parameter :: MY_STRING") == 0) then
            write (error_unit, '(A)') &
                "ERROR: Expected 'parameter :: MY_STRING'"
            write (error_unit, '(A)') "Output: " // trim(output)
            passed = .false.
        end if
    end function test_character_array_parameter

    function test_parameter_in_explicit_program() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg

        passed = .true.
        source = "program main" // char(10) // &
            "    integer :: y" // char(10) // &
            "    PARAMETER(Y = 42)" // char(10) // &
            "end program"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') "ERROR: Failed to parse: " // trim(error_msg)
            passed = .false.
            return
        end if

        if (index(output, ", parameter") == 0) then
            write (error_unit, '(A)') &
                "ERROR: Expected 'parameter' attribute in explicit program"
            write (error_unit, '(A)') "Output: " // trim(output)
            passed = .false.
        end if

        if (index(output, "= 42") == 0) then
            write (error_unit, '(A)') &
                "ERROR: Expected initializer '= 42'"
            write (error_unit, '(A)') "Output: " // trim(output)
            passed = .false.
        end if
    end function test_parameter_in_explicit_program

end program test_issue_2487_parameter_statement
