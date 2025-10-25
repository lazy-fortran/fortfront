program test_monomorphization
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input, output, error_msg
    logical :: test_passed

    print *, "=== Monomorphization Tests ==="

    ! Test 1: Generic function with multiple call signatures
    print *, "Testing generic function monomorphization..."
    input = &
        "function add(a, b)" // new_line('A') // &
        "    add = a + b" // new_line('A') // &
        "end function" // new_line('A') // &
        "" // new_line('A') // &
        "x = add(5, 3)" // new_line('A') // &
        "y = add(2.5d0, 1.5d0)" // new_line('A')

    call transform_lazy_fortran_string(input, output, error_msg)

    ! Verify output contains monomorphized module
    test_passed = .false.
    if (index(output, "module auto_add") > 0) then
        if (index(output, "interface add") > 0) then
            if (index(output, "module procedure") > 0) then
                test_passed = .true.
            end if
        end if
    end if

    if (test_passed) then
        print *, "  PASS: Module structure generated"
    else
        print *, "  FAIL: Module structure missing"
        print *, "  Output:", output
        stop 1
    end if

    ! Test 2: Verify type-specific variants created
    print *, "Testing type-specific variants..."
    test_passed = .false.
    ! Check for integer variant (k2 = TINT)
    if (index(output, "add__k2_k2") > 0) then
        ! Check for double precision variant (k9 = TDOUBLE)
        if (index(output, "add__k9_k9") > 0) then
            test_passed = .true.
        end if
    end if

    if (test_passed) then
        print *, "  PASS: Type-specific variants created"
    else
        print *, "  FAIL: Variants missing"
        stop 1
    end if

    ! Test 3: Single signature function (should NOT be monomorphized)
    print *, "Testing single-signature function..."
    input = &
        "function square(x)" // new_line('A') // &
        "    square = x * x" // new_line('A') // &
        "end function" // new_line('A') // &
        "" // new_line('A') // &
        "y = square(5)" // new_line('A')

    call transform_lazy_fortran_string(input, output, error_msg)

    ! Should NOT have auto_square module
    test_passed = .false.
    if (index(output, "module auto_square") == 0) then
        if (index(output, "function square") > 0) then
            test_passed = .true.
        end if
    end if

    if (test_passed) then
        print *, "  PASS: Single signature not monomorphized"
    else
        print *, "  FAIL: Unnecessary monomorphization"
        stop 1
    end if

    print *, ""
    print *, "All monomorphization tests passed!"

end program test_monomorphization
