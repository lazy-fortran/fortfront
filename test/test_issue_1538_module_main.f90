program test_issue_1538_module_main
    use frontend, only: transform_with_context, transform_context_t
    use frontend_transformation, only: INPUT_MODE_LAZY, detect_input_mode_from_content
    implicit none

    character(len=:), allocatable :: input, output, error_msg
    type(transform_context_t) :: context
    logical :: test_passed

    test_passed = .true.

    ! Test case from the issue
    input = "! Test module with contains" // new_line('a') // &
            "module math_utils" // new_line('a') // &
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

    ! Set up context with filename-based module name (this is what causes the issue)
    context%source_name = "test_file"
    context%module_name = "test_file"  ! This should NOT override the actual module name
    context%program_name = "main"
    context%has_filename = .true.
    context%input_mode = INPUT_MODE_LAZY

    call transform_with_context(input, output, error_msg, context)

    print *, "Input:"
    print *, input
    print *, "=========================================="
    print *, "Output:"
    print *, output
    print *, "=========================================="

    ! Check if the output contains the expected module name
    if (index(output, "module math_utils") > 0) then
        print *, "SUCCESS: Module name preserved correctly"
    else
        print *, "FAILURE: Module name not preserved"
        if (index(output, "module test_file") > 0) then
            print *, "  - Module name was incorrectly changed to filename"
        end if
        test_passed = .false.
    end if

    ! Check if the top-level statements are present
    if (index(output, "use math_utils") > 0 .and. &
        (index(output, "print *, square(5.0)") > 0 .or. &
         index(output, "print *, square(5.0d0)") > 0)) then
        print *, "SUCCESS: Top-level statements preserved"
    else
        print *, "FAILURE: Top-level statements missing"
        test_passed = .false.
    end if

    if (allocated(error_msg) .and. len(error_msg) > 0) then
        print *, "Error:", error_msg
    end if

    if (.not. test_passed) then
        error stop 1
    end if

end program test_issue_1538_module_main
