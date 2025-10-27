program test_issue_1959_target_attribute
    use transformation_api, only: transform_with_context, transform_context_t
    use frontend_transformation, only: INPUT_MODE_STANDARD
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: context
    logical :: has_target_attr
    logical :: test_passed

    print *, "=== Issue #1959: preserve target attribute for pointer targets ==="

    input_code = &
        "program test_pointer" // new_line('A') // &
        "    implicit none" // new_line('A') // &
        "    integer, target :: x, y" // new_line('A') // &
        "    integer, pointer :: p" // new_line('A') // &
        new_line('A') // &
        "    x = 10" // new_line('A') // &
        "    y = 20" // new_line('A') // &
        "    p => x" // new_line('A') // &
        "end program test_pointer"

    context%input_mode = INPUT_MODE_STANDARD
    context%has_filename = .true.
    context%source_name = "test_issue_1959_input"

    call transform_with_context(input_code, output_code, error_msg, context)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: transform_with_context returned error:", trim(error_msg)
        error stop 1
    end if

    has_target_attr = index(output_code, ", target :: x") > 0 .or. &
                      index(output_code, ", target :: y") > 0

    test_passed = has_target_attr

    if (.not. has_target_attr) then
        print *, "FAIL: target attribute missing from transformed declaration"
        print *, "Transformed output:"
        print *, trim(output_code)
        error stop 1
    end if

    if (test_passed) then
        print *, "PASS: target attribute preserved for pointer associations"
    else
        error stop 1
    end if

end program test_issue_1959_target_attribute
