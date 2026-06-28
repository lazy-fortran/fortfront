program test_issue_212_exponentiation_precedence
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #212: Wrong grouping for exponentiation ==='

    if (.not. test_exponentiation_precedence()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #212 test passed!'
        stop 0
    else
        print *, 'Issue #212 test failed!'
        stop 1
    end if

contains

    logical function test_exponentiation_precedence()
        character(len=:), allocatable :: source, output, error_msg
        logical :: has_correct_precedence

        test_exponentiation_precedence = .true.
        print *, 'Testing exponentiation precedence...'

        source = 'r = 10' // new_line('a') // &
            'area = 3.14 * r**2' // new_line('a') // &
            'print*,area'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_exponentiation_precedence = .false.
                return
            end if
        end if

        has_correct_precedence = .false.

        if (contains_without_spaces(output, '3.14d0*r**2') .or. &
            contains_without_spaces(output, '3.14_8*r**2') .or. &
            contains_without_spaces(output, '3.14*r**2')) then
            has_correct_precedence = .true.
            print *, '  OK: Found correct precedence'
        end if

        if (contains_without_spaces(output, '(3.14d0*r)**2') .or. &
            contains_without_spaces(output, '(3.14_8*r)**2') .or. &
            contains_without_spaces(output, '(3.14*r)**2')) then
            print *, '  FAIL: Found incorrect grouping'
            test_exponentiation_precedence = .false.
            return
        end if

        if (.not. has_correct_precedence) then
            print *, '  FAIL: Did not find expected "3.14*r**2" or "3.14d0*r**2" in output'
            test_exponentiation_precedence = .false.
        end if

    end function test_exponentiation_precedence

    logical function contains_without_spaces(text, pattern)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: compressed
        integer :: i

        compressed = ''
        do i = 1, len_trim(text)
            if (text(i:i) /= ' ') compressed = compressed // text(i:i)
        end do
        contains_without_spaces = index(compressed, pattern) > 0
    end function contains_without_spaces

end program test_issue_212_exponentiation_precedence
