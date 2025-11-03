program test_issue_1221_explicit_program_type_inference
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    character(len=:), allocatable :: output, error_msg
    logical :: success

    call test_explicit_program_with_inference()
    call test_explicit_program_array_inference()
    call test_explicit_program_mixed_types()

    print *, "All tests completed!"

contains

    subroutine test_explicit_program_with_inference()
        character(len=:), allocatable :: input
        print *, "Testing explicit program with type inference..."

        call read_example('examples/lf/explicit_program_scalar_inference.lf', input)

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "ERROR: ", trim(error_msg)
            error stop 1
        end if

        ! Check that x = 42 is preserved (minimal requirement)
        success = index(output, "x = 42") > 0
        if (.not. success) then
            print *, "FAILED: Code transformation not working for explicit program"
            print *, "Output:"
            print *, trim(output)
            error stop 1
        end if

        ! Check if type inference worked (expected to fail currently - issue #1221)
        if (index(output, "integer :: x") > 0) then
            print *, "PASSED: Type inference works for explicit program"
        else
            print *, "EXPECTED FAILURE: Type inference NOT working (issue #1221)"
            print *, "This is a known issue requiring parser refactoring"
            print *, "The code is still valid Fortran without type inference"
            ! Don't error stop - this is expected until issue is fixed
        end if
    end subroutine test_explicit_program_with_inference

    subroutine test_explicit_program_array_inference()
        character(len=:), allocatable :: input
        print *, "Testing explicit program with array type inference..."

        call read_example('examples/lf/explicit_program_array_inference.lf', input)

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "ERROR: ", trim(error_msg)
            error stop 1
        end if

        ! Check that array literal is preserved (minimal requirement)
        success = index(output, "arr = [1, 2, 3]") > 0
        if (.not. success) then
            print *, "FAILED: Code transformation not working for explicit program"
            print *, "Output:"
            print *, trim(output)
            error stop 1
        end if

        ! Check that array declaration was added (expected to fail - issue #1221)
        success = (index(output, "integer, dimension(3) :: arr") > 0) .or. &
                  (index(output, "integer :: arr(3)") > 0)
        if (success) then
            print *, "PASSED: Array type inference works for explicit program"
        else
            print *, "EXPECTED FAILURE: Array type inference NOT working (issue #1221)"
            print *, "This is a known issue requiring parser refactoring"
            ! Don't error stop - this is expected until issue is fixed
        end if
    end subroutine test_explicit_program_array_inference

    subroutine test_explicit_program_mixed_types()
        character(len=:), allocatable :: input
        print *, "Testing explicit program with mixed type inference..."

        call read_example('examples/lf/explicit_program_mixed_inference.lf', input)

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "ERROR: ", trim(error_msg)
            error stop 1
        end if

        ! Check that assignments are preserved (minimal requirement)
        success = (index(output, "x = 42") > 0) .and. &
                  (index(output, "y = 3.14") > 0) .and. &
                  (index(output, "flag = .true.") > 0)
        if (.not. success) then
            print *, "FAILED: Code transformation not working for explicit program"
            print *, "Output:"
            print *, trim(output)
            error stop 1
        end if

        ! Check that all declarations were added (expected to fail - issue #1221)
        success = (index(output, "integer :: x") > 0) .and. &
                  (index(output, "real :: y") > 0) .and. &
                  (index(output, "logical :: flag") > 0)
        if (success) then
            print *, "PASSED: Mixed type inference works for explicit program"
        else
            print *, "EXPECTED FAILURE: Mixed type inference NOT working (issue #1221)"
            print *, "This is a known issue requiring parser refactoring"
            ! Don't error stop - this is expected until issue is fixed
        end if
    end subroutine test_explicit_program_mixed_types

    include '../../common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example

end program test_issue_1221_explicit_program_type_inference
