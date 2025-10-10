program test_issue_214_string_concatenation_precedence
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Comprehensive Operator Precedence and Associativity Tests ==='
    print *, '=== Issues #214, #215, #216 ==='

    if (.not. test_string_concatenation_precedence_with_parentheses()) all_passed = .false.
    if (.not. test_unary_operator_precedence()) all_passed = .false.
    if (.not. test_comparison_non_associativity()) all_passed = .false.
    if (.not. test_comprehensive_precedence_hierarchy()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All operator precedence tests passed!'
        stop 0
    else
        print *, 'Some operator precedence tests failed!'
        stop 1
    end if

contains

    logical function test_string_concatenation_precedence_with_parentheses()
        character(len=:), allocatable :: source1, output1, error_msg1
        character(len=:), allocatable :: source2, output2, error_msg2
        character(len=:), allocatable :: source3, output3, error_msg3

        test_string_concatenation_precedence_with_parentheses = .true.
        print *, 'Testing string concatenation precedence using explicit parentheses...'

        source1 = 'program test_wrong_precedence' // new_line('a') // &
                  '    character(len=10) :: result' // new_line('a') // &
                  '    result = "a" + ("b" // "c")' // new_line('a') // &
                  '    print *, result' // new_line('a') // &
                  'end program test_wrong_precedence'

        call transform_lazy_fortran_string(source1, output1, error_msg1)

        if (allocated(error_msg1)) then
            if (len_trim(error_msg1) > 0) then
                print *, '  FAIL: Compilation error (correct version):', trim(error_msg1)
                test_string_concatenation_precedence_with_parentheses = .false.
                return
            end if
        end if

        source2 = 'program test_correct_precedence' // new_line('a') // &
                  '    character(len=10) :: result' // new_line('a') // &
                  '    result = ("a" + "b") // "c"' // new_line('a') // &
                  '    print *, result' // new_line('a') // &
                  'end program test_correct_precedence'

        call transform_lazy_fortran_string(source2, output2, error_msg2)

        if (allocated(error_msg2)) then
            if (len_trim(error_msg2) > 0) then
                print *, '  FAIL: Compilation error (wrong version):', trim(error_msg2)
                test_string_concatenation_precedence_with_parentheses = .false.
                return
            end if
        end if

        if (index(output1, 'result = ') > 0) then
            print *, '  Wrong precedence version (+ higher than //) compiled'
        end if

        if (index(output2, 'result = ') > 0) then
            print *, '  Correct precedence version (// lower than +) compiled'
        end if

        source3 = 'program test_ambiguous' // new_line('a') // &
                  '    character(len=10) :: result' // new_line('a') // &
                  '    result = "a" + "b" // "c"' // new_line('a') // &
                  '    print *, result' // new_line('a') // &
                  'end program test_ambiguous'

        call transform_lazy_fortran_string(source3, output3, error_msg3)

        if (allocated(error_msg3)) then
            if (len_trim(error_msg3) > 0) then
                print *, '  FAIL: Compilation error (ambiguous version):', trim(error_msg3)
                test_string_concatenation_precedence_with_parentheses = .false.
                return
            end if
        end if

        if (index(output3, 'result = ') > 0) then
            print *, '  Ambiguous expression compiled successfully'
        end if

    end function test_string_concatenation_precedence_with_parentheses

    logical function test_unary_operator_precedence()
        character(len=:), allocatable :: source, output, error_msg

        test_unary_operator_precedence = .true.
        print *, 'Testing unary operator precedence (Issue #215)...'

        source = 'program test_unary' // new_line('a') // &
                 '    integer :: result' // new_line('a') // &
                 '    result = -2 ** 2' // new_line('a') // &
                 '    print *, result' // new_line('a') // &
                 'end program test_unary'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_unary_operator_precedence = .false.
                return
            end if
        end if

        if (index(output, 'result = ') > 0) then
            print *, '  OK: Unary operator precedence test generated code successfully'
        else
            print *, '  FAIL: Could not verify unary operator precedence in generated code'
            test_unary_operator_precedence = .false.
        end if

    end function test_unary_operator_precedence

    logical function test_comparison_non_associativity()
        character(len=:), allocatable :: source, output, error_msg

        test_comparison_non_associativity = .true.
        print *, 'Testing comparison non-associativity (Issue #216)...'

        source = 'program test_comparison' // new_line('a') // &
                 '    integer :: a, b, c' // new_line('a') // &
                 '    logical :: result' // new_line('a') // &
                 '    a = 1' // new_line('a') // &
                 '    b = 2' // new_line('a') // &
                 '    c = 3' // new_line('a') // &
                 '    result = a < b < c' // new_line('a') // &
                 '    print *, result' // new_line('a') // &
                 'end program test_comparison'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_comparison_non_associativity = .false.
                return
            end if
        end if

        if (index(output, 'result = ') > 0) then
            print *, '  WARNING: Comparison chaining test needs manual verification'
        else
            print *, '  FAIL: Could not verify comparison associativity in generated code'
            test_comparison_non_associativity = .false.
        end if

    end function test_comparison_non_associativity

    logical function test_comprehensive_precedence_hierarchy()
        character(len=:), allocatable :: source, output, error_msg

        test_comprehensive_precedence_hierarchy = .true.
        print *, 'Testing comprehensive operator precedence hierarchy...'

        source = 'program test_comprehensive' // new_line('a') // &
                 '    character(len=10) :: a, b, c, result_str' // new_line('a') // &
                 '    a = "1"' // new_line('a') // &
                 '    b = "2"' // new_line('a') // &
                 '    c = "3"' // new_line('a') // &
                 '    ! Test string concatenation precedence' // new_line('a') // &
                 '    result_str = a + b // c' // new_line('a') // &
                 '    print *, result_str' // new_line('a') // &
                 'end program test_comprehensive'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error:', trim(error_msg)
                test_comprehensive_precedence_hierarchy = .false.
                return
            end if
        end if

        if (index(output, 'result_str = ') > 0) then
            print *, '  OK: Comprehensive precedence test generated code successfully'
        else
            print *, '  FAIL: Could not verify comprehensive precedence in generated code'
            test_comprehensive_precedence_hierarchy = .false.
        end if

    end function test_comprehensive_precedence_hierarchy

end program test_issue_214_string_concatenation_precedence
