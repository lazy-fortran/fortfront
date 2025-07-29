program test_lexer_operators
    use lexer_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Run comprehensive operator tokenization tests
    if (.not. test_arithmetic_operators()) all_passed = .false.
    if (.not. test_comparison_operators()) all_passed = .false.
    if (.not. test_assignment_operators()) all_passed = .false.
    if (.not. test_delimiters()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All operator tokenization tests passed"
        stop 0
    else
        print '(a)', "Some operator tokenization tests failed"
        stop 1
    end if

contains

    logical function test_arithmetic_operators()
        type(token_t), allocatable :: tokens(:)
        character(len=1), dimension(4) :: single_ops = ["+", "-", "*", "/"]
        integer :: i

        test_arithmetic_operators = .true.
        print '(a)', "Testing arithmetic operator tokenization..."

        ! Test single character operators
        do i = 1, size(single_ops)
            call tokenize_core(single_ops(i), tokens)
            if (size(tokens) /= 2) then  ! operator + EOF
                print '(a,a)', "FAIL: Wrong token count for operator: ", single_ops(i)
                test_arithmetic_operators = .false.
                return
            end if

            if (tokens(1)%kind /= TK_OPERATOR) then
                print '(a,a)', "FAIL: Expected OPERATOR token for: ", single_ops(i)
                test_arithmetic_operators = .false.
                return
            end if

            if (tokens(1)%text /= single_ops(i)) then
                print '(a,a)', "FAIL: Wrong text for operator: ", single_ops(i)
                test_arithmetic_operators = .false.
                return
            end if
        end do

        ! Test power operator (**)
        call tokenize_core("**", tokens)
        if (size(tokens) /= 2) then
            print '(a)', "FAIL: Wrong token count for ** operator"
            test_arithmetic_operators = .false.
            return
        end if

        if (tokens(1)%kind /= TK_OPERATOR .or. tokens(1)%text /= "**") then
            print '(a)', "FAIL: ** operator not tokenized correctly"
            test_arithmetic_operators = .false.
            return
        end if

        print '(a)', "PASS: Arithmetic operator tokenization"
    end function test_arithmetic_operators

    logical function test_comparison_operators()
        type(token_t), allocatable :: tokens(:)
        character(len=2), dimension(4) :: comp_ops = ["==", "/=", "<=", ">="]
        character(len=1), dimension(2) :: single_comp = ["<", ">"]
        integer :: i

        test_comparison_operators = .true.
        print '(a)', "Testing comparison operator tokenization..."

        ! Test two-character comparison operators
        do i = 1, size(comp_ops)
            call tokenize_core(comp_ops(i), tokens)
            if (size(tokens) /= 2) then  ! operator + EOF
                print '(a,a)', "FAIL: Wrong token count for operator: ", comp_ops(i)
                test_comparison_operators = .false.
                return
            end if

            if (tokens(1)%kind /= TK_OPERATOR) then
                print '(a,a)', "FAIL: Expected OPERATOR token for: ", comp_ops(i)
                test_comparison_operators = .false.
                return
            end if

            if (tokens(1)%text /= comp_ops(i)) then
                print '(a,a)', "FAIL: Wrong text for operator: ", comp_ops(i)
                test_comparison_operators = .false.
                return
            end if
        end do

        ! Test single character comparison operators
        do i = 1, size(single_comp)
            call tokenize_core(single_comp(i), tokens)
            if (tokens(1)%kind /= TK_OPERATOR .or. tokens(1)%text /= single_comp(i)) then
                print '(a,a)', "FAIL: Single comparison operator: ", single_comp(i)
                test_comparison_operators = .false.
                return
            end if
        end do

        print '(a)', "PASS: Comparison operator tokenization"
    end function test_comparison_operators

    logical function test_assignment_operators()
        type(token_t), allocatable :: tokens(:)

        test_assignment_operators = .true.
        print '(a)', "Testing assignment operator tokenization..."

        ! Test simple assignment
        call tokenize_core("=", tokens)
        if (size(tokens) /= 2) then
            print '(a)', "FAIL: Wrong token count for = operator"
            test_assignment_operators = .false.
            return
        end if

        if (tokens(1)%kind /= TK_OPERATOR .or. tokens(1)%text /= "=") then
            print '(a)', "FAIL: = operator not tokenized correctly"
            test_assignment_operators = .false.
            return
        end if

        ! Test type declaration operator
        call tokenize_core("::", tokens)
        if (tokens(1)%kind /= TK_OPERATOR .or. tokens(1)%text /= "::") then
            print '(a)', "FAIL: :: operator not tokenized correctly"
            test_assignment_operators = .false.
            return
        end if

        print '(a)', "PASS: Assignment operator tokenization"
    end function test_assignment_operators

    logical function test_delimiters()
        type(token_t), allocatable :: tokens(:)
        character(len=1), dimension(8) :: delims = ["(", ")", "[", "]", "{", "}", ",", ";"]
        integer :: i

        test_delimiters = .true.
        print '(a)', "Testing delimiter tokenization..."

        ! Test all delimiters
        do i = 1, size(delims)
            call tokenize_core(delims(i), tokens)
            if (size(tokens) /= 2) then  ! delimiter + EOF
                print '(a,a)', "FAIL: Wrong token count for delimiter: ", delims(i)
                test_delimiters = .false.
                return
            end if

            if (tokens(1)%kind /= TK_OPERATOR) then
                print '(a,a)', "FAIL: Expected OPERATOR token for delimiter: ", delims(i)
                test_delimiters = .false.
                return
            end if

            if (tokens(1)%text /= delims(i)) then
                print '(a,a)', "FAIL: Wrong text for delimiter: ", delims(i)
                test_delimiters = .false.
                return
            end if
        end do

        ! Test dot (special case)
        call tokenize_core(".", tokens)
        if (tokens(1)%kind /= TK_OPERATOR .or. tokens(1)%text /= ".") then
            print '(a)', "FAIL: . delimiter not tokenized correctly"
            test_delimiters = .false.
            return
        end if

        print '(a)', "PASS: Delimiter tokenization"
    end function test_delimiters

end program test_lexer_operators
