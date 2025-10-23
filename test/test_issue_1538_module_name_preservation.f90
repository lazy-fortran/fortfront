! Test for issue 1538: Module name changed, top-level statements after module dropped
!
! This test verifies that when a module is followed by top-level statements,
! the module name should be preserved and the top-level statements should
! be included in a program wrapper.

program test_issue_1538_module_name_preservation
    use transformation_api, only: transform_with_context, transform_context_t
    use frontend_transformation, only: INPUT_MODE_LAZY
    implicit none

    character(len=:), allocatable :: input, output, error_msg
    type(transform_context_t) :: context
    logical :: test_passed

    test_passed = .true.

    ! Test case: Module followed by top-level statements
    input = "module math_utils" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "contains" // new_line('a') // &
            "    function square(x)" // new_line('a') // &
            "        real, intent(in) :: x" // new_line('a') // &
            "        real :: square" // new_line('a') // &
            "        square = x * x" // new_line('a') // &
            "    end function square" // new_line('a') // &
            "end module math_utils" // new_line('a') // &
            new_line('a') // &
            "use math_utils" // new_line('a') // &
            "print *, square(5.0)"

    ! Set up context
    context%source_name = "test_file"
    context%module_name = "test_file"  ! This should NOT override actual module name
    context%program_name = "main"
    context%has_filename = .true.
    context%input_mode = INPUT_MODE_LAZY

    call transform_with_context(input, output, error_msg, context)

    print *, "=========================================="
    print *, "Test: Issue 1538 - Module name preservation"
    print *, "=========================================="
    print *, "Input:"
    print *, input
    print *, "=========================================="
    print *, "Output:"
    print *, output
    print *, "=========================================="

    ! Check results
    if (index(output, "module math_utils") > 0) then
        print *, "PASS: Module name preserved correctly"
    else
        print *, "FAIL: Module name not preserved"
        if (index(output, "module test_file") > 0) then
            print *, "  - Module name was incorrectly changed to filename"
        end if
        test_passed = .false.
    end if

    if (index(output, "use math_utils") > 0 .and. &
        (index(output, "print *, square(5.0)") > 0 .or. &
         index(output, "print *, square(5.0d0)") > 0)) then
        print *, "PASS: Top-level statements preserved"
    else
        print *, "FAIL: Top-level statements missing or incomplete"
        test_passed = .false.
    end if

    if (test_passed) then
        print *, "=========================================="
        print *, "PASS: All checks"
        print *, "=========================================="
    else
        print *, "=========================================="
        print *, "FAIL: One or more checks failed"
        print *, "=========================================="
    end if

    if (allocated(error_msg) .and. len(error_msg) > 0) then
        print *, "Error:", error_msg
    end if

    if (.not. test_passed) then
        error stop 1
    end if

end program test_issue_1538_module_name_preservation
