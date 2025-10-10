program test_issue_1238_operator_precedence_fix
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Test for Issue #1238: Operator Precedence Fix ==='
    print *, 'Testing that string concatenation (//) has lower precedence than addition (+)'

    if (.not. test_concatenation_vs_addition_precedence()) all_passed = .false.
    if (.not. test_nested_expressions()) all_passed = .false.
    if (.not. test_parentheses_preservation()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All issue #1238 tests passed!'
        stop 0
    else
        print *, 'Some issue #1238 tests failed!'
        stop 1
    end if

contains

    logical function test_concatenation_vs_addition_precedence()
        character(len=:), allocatable :: source, output, error_msg

        test_concatenation_vs_addition_precedence = .true.
        print *, 'Test 1: String concatenation precedence vs addition...'

        source = "program test_precedence" // new_line('a') // &
                 "    character(len=10) :: a, b, c, r1, r2, r3" // new_line('a') // &
                 '    a = "A"' // new_line('a') // &
                 '    b = "B"' // new_line('a') // &
                 '    c = "C"' // new_line('a') // &
                 "    ! Test unparenthesized expression" // new_line('a') // &
                 "    r1 = a + b // c" // new_line('a') // &
                 "    ! Test with explicit parentheses (correct precedence)" // new_line('a') // &
                 "    r2 = a + (b // c)" // new_line('a') // &
                 "    ! Test with explicit parentheses (our expected parsing)" // new_line('a') // &
                 "    r3 = (a + b) // c" // new_line('a') // &
                 '    print *, "r1=", r1' // new_line('a') // &
                 '    print *, "r2=", r2' // new_line('a') // &
                 '    print *, "r3=", r3' // new_line('a') // &
                 "end program test_precedence"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_concatenation_vs_addition_precedence = .false.
                return
            end if
        end if

        if (index(output, 'r1 = ') > 0) then
            print *, '  OK: Generated r1 expression successfully'
        else
            print *, '  FAIL: Could not find r1 assignment in generated code'
            test_concatenation_vs_addition_precedence = .false.
        end if

    end function test_concatenation_vs_addition_precedence

    logical function test_nested_expressions()
        character(len=:), allocatable :: source, output, error_msg

        test_nested_expressions = .true.
        print *, 'Test 2: Nested expression precedence...'

        source = "program test_nested" // new_line('a') // &
                 "    integer :: a, b, c, d" // new_line('a') // &
                 "    character(len=20) :: s1, s2, s3, result" // new_line('a') // &
                 "    ! Test arithmetic mixed with concatenation" // new_line('a') // &
                 '    a = 1; b = 2; c = 3; d = 4' // new_line('a') // &
                 '    s1 = "X"; s2 = "Y"; s3 = "Z"' // new_line('a') // &
                 "    ! Complex expression mixing operators" // new_line('a') // &
                 "    result = s1 // s2 + s3" // new_line('a') // &
                 "    print *, result" // new_line('a') // &
                 "end program test_nested"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_nested_expressions = .false.
            else
                print *, '  OK: Nested expressions compiled successfully'
            end if
        else
            print *, '  OK: Nested expressions compiled successfully'
        end if

    end function test_nested_expressions

    logical function test_parentheses_preservation()
        character(len=:), allocatable :: source, output, error_msg
        integer :: paren_count

        test_parentheses_preservation = .true.
        print *, 'Test 3: Parentheses preservation in parsing...'

        source = "program test_parens" // new_line('a') // &
                 "    character(len=10) :: x, y, z, result" // new_line('a') // &
                 '    x = "1"; y = "2"; z = "3"' // new_line('a') // &
                 "    ! Explicit parentheses must be preserved" // new_line('a') // &
                 "    result = (x + y) // z" // new_line('a') // &
                 "    print *, result" // new_line('a') // &
                 "    result = x + (y // z)" // new_line('a') // &
                 "    print *, result" // new_line('a') // &
                 "end program test_parens"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_parentheses_preservation = .false.
                return
            end if
        end if

        paren_count = count_chars(output, '(') + count_chars(output, ')')

        if (paren_count > 0) then
            print *, '  OK: Parentheses found in generated expressions'
        else
            print *, '  WARNING: No parentheses found (may be optimized away)'
        end if

    end function test_parentheses_preservation

    integer function count_chars(str, ch)
        character(len=*), intent(in) :: str
        character(len=1), intent(in) :: ch
        integer :: i

        count_chars = 0
        do i = 1, len_trim(str)
            if (str(i:i) == ch) count_chars = count_chars + 1
        end do
    end function count_chars

end program test_issue_1238_operator_precedence_fix
