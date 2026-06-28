program test_issue_960_operator_precedence
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #960: Operator precedence associativity (exponentiation) ==='

    if (.not. test_exponentiation_right_associative()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #960 tests passed!'
        stop 0
    else
        print *, 'Issue #960 tests failed!'
        stop 1
    end if

contains

    logical function test_exponentiation_right_associative()
        character(len=:), allocatable :: source, output, error_msg
        logical :: saw_chain, saw_wrong_grouping

        test_exponentiation_right_associative = .true.
        print *, 'Testing exponentiation right-associativity (a**b**c == a**(b**c))...'

        source = 'a = 2' // new_line('a') // &
            'b = 3' // new_line('a') // &
            'c = 2' // new_line('a') // &
            'x = a**b**c'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_exponentiation_right_associative = .false.
                return
            end if
        end if

        saw_chain = .false.
        saw_wrong_grouping = .false.

        if (contains_without_spaces(output, 'a**b**c')) then
            saw_chain = .true.
        end if

        if (contains_without_spaces(output, '(a**b)**c')) then
            print *, '  FAIL: Found incorrect left-associative grouping'
            saw_wrong_grouping = .true.
        end if

        if (.not. saw_chain) then
            print *, '  FAIL: Did not find expected chained exponentiation in output'
            test_exponentiation_right_associative = .false.
            return
        end if

        if (saw_wrong_grouping) then
            test_exponentiation_right_associative = .false.
            return
        end if

    end function test_exponentiation_right_associative

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

end program test_issue_960_operator_precedence
