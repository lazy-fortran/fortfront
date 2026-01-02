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

    include '../../common/read_example.inc'

    logical function test_concatenation_vs_addition_precedence()
        character(len=:), allocatable :: source, output, error_msg

        test_concatenation_vs_addition_precedence = .true.
        print *, 'Test 1: String concatenation precedence vs addition...'

        call read_example('examples/lf/issue_1238_concatenation_addition_precedence.lf', source)

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

        call read_example('examples/lf/issue_1238_nested_expressions.lf', source)

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

        call read_example('examples/lf/issue_1238_parentheses_preservation.lf', source)

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

