program test_do_loop_codegen_issue
    ! Test that do loops generate correct code, not broken declarations
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg
    logical :: test_passed

    print *, "=== Testing Do Loop Code Generation - CRITICAL ISSUE ==="

    ! Test 1: Simple do loop
    print *, ""
    print *, "Test 1: Simple do loop (do i = 1, 10)"

    source = "program test" // new_line('a') // &
             "  implicit none" // new_line('a') // &
             "  integer :: i" // new_line('a') // &
             "  do i = 1, 10" // new_line('a') // &
             "    print *, i" // new_line('a') // &
             "  end do" // new_line('a') // &
             "end program test"

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            stop 1
        end if
    end if

    test_passed = .false.
    if (index(output, 'do i = 1, 10') > 0 .or. index(output, 'do i=1,10') > 0) then
        print *, '  OK: Found correct do loop'
        test_passed = .true.
    end if

    if (index(output, 'integer :: do') > 0) then
        print *, '  ERROR: Found broken declaration'
        test_passed = .false.
    end if

    if (.not. test_passed) then
        print *, '  FAIL: Do loop not generated correctly!'
        print *, '  Actual output:'
        print *, trim(output)
        stop 1
    end if

    ! Test 2: Do loop with variable bounds
    print *, ""
    print *, "Test 2: Do loop with variables (do i = 1, n)"

    source = "program test" // new_line('a') // &
             "  implicit none" // new_line('a') // &
             "  integer :: i, n" // new_line('a') // &
             "  n = 10" // new_line('a') // &
             "  do i = 1, n" // new_line('a') // &
             "    print *, i" // new_line('a') // &
             "  end do" // new_line('a') // &
             "end program test"

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            stop 1
        end if
    end if

    test_passed = .false.
    if (index(output, 'do i = 1, n') > 0 .or. index(output, 'do i=1,n') > 0) then
        print *, '  OK: Found correct do loop'
        test_passed = .true.
    end if

    if (index(output, 'integer :: do') > 0) then
        print *, '  ERROR: Found broken declaration'
        test_passed = .false.
    end if

    if (.not. test_passed) then
        print *, '  FAIL: Do loop with variables not generated correctly!'
        stop 1
    end if

    ! Test 3: Do loop with expressions (CRITICAL TEST)
    print *, ""
    print *, "Test 3: Do loop with expressions (do i = n-5, n+5) - CRITICAL"

    source = "program test" // new_line('a') // &
             "  implicit none" // new_line('a') // &
             "  integer :: i, n" // new_line('a') // &
             "  n = 10" // new_line('a') // &
             "  do i = n-5, n+5" // new_line('a') // &
             "    print *, i" // new_line('a') // &
             "  end do" // new_line('a') // &
             "end program test"

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            stop 1
        end if
    end if

    test_passed = .false.
    if (index(output, 'do i =') > 0 .and. &
        (index(output, 'n-5') > 0 .or. index(output, 'n - 5') > 0 .or. &
         index(output, 'n-5d0') > 0 .or. index(output, 'n - 5d0') > 0)) then
        print *, '  OK: Found do loop with expressions'
        test_passed = .true.
    end if

    if (index(output, 'integer :: do') > 0) then
        print *, '  ERROR: Found broken declaration'
        test_passed = .false.
    end if

    if (.not. test_passed) then
        print *, '  FAIL: Do loop with expressions not generated correctly!'
        print *, '  Actual output:'
        print *, trim(output)
        stop 1
    end if

    print *, ""
    print *, "ALL TESTS PASSED! Do loops generate correct code!"
    stop 0

end program test_do_loop_codegen_issue
