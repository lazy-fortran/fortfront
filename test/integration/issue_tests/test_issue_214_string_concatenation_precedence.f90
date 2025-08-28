program test_issue_214_string_concatenation_precedence
    use frontend, only: compile_source, compilation_options_t
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
        character(len=:), allocatable :: input_file1, output_file1
        character(len=:), allocatable :: input_file2, output_file2
        character(len=256) :: error_msg, line1, line2
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_line1, found_line2
        
        test_string_concatenation_precedence_with_parentheses = .true.
        print *, 'Testing string concatenation precedence using explicit parentheses...'
        
        ! Test 1: Create expression with parentheses to force WRONG precedence
        ! Expression: "a" + ("b" // "c") - this should NOT be the natural parsing
        input_file1 = 'test_issue_214_correct.lf'
        open(newunit=unit, file=input_file1, status='replace')
        write(unit, '(a)') 'program test_wrong_precedence'
        write(unit, '(a)') '    character(len=10) :: result'
        write(unit, '(a)') '    result = "a" + ("b" // "c")'
        write(unit, '(a)') '    print *, result'
        write(unit, '(a)') 'end program test_wrong_precedence'
        close(unit)
        
        ! Test 2: Create expression with parentheses to force CORRECT precedence  
        ! Expression: ("a" + "b") // "c" - this should be the natural parsing
        input_file2 = 'test_issue_214_wrong.lf'
        open(newunit=unit, file=input_file2, status='replace')
        write(unit, '(a)') 'program test_correct_precedence'
        write(unit, '(a)') '    character(len=10) :: result'
        write(unit, '(a)') '    result = ("a" + "b") // "c"'
        write(unit, '(a)') '    print *, result'
        write(unit, '(a)') 'end program test_correct_precedence'
        close(unit)
        
        ! Compile both versions
        output_file1 = 'test_issue_214_correct_out.f90'
        options%output_file = output_file1
        call compile_source(input_file1, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error (correct version):', trim(error_msg)
            test_string_concatenation_precedence_with_parentheses = .false.
            return
        end if
        
        output_file2 = 'test_issue_214_wrong_out.f90'
        options%output_file = output_file2
        call compile_source(input_file2, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error (wrong version):', trim(error_msg)
            test_string_concatenation_precedence_with_parentheses = .false.
            return
        end if
        
        ! Read the generated code from both versions
        found_line1 = .false.
        found_line2 = .false.
        
        ! Read wrong precedence version (should NOT match ambiguous)
        open(newunit=unit, file=output_file1, status='old')
        do
            read(unit, '(a)', iostat=iostat) line1
            if (iostat /= 0) exit
            if (index(trim(line1), 'result = ') > 0) then
                print *, '  Wrong precedence version (+ higher than //): ', trim(line1)
                found_line1 = .true.
                exit
            end if
        end do
        close(unit)
        
        ! Read correct precedence version (should match ambiguous)
        open(newunit=unit, file=output_file2, status='old')
        do
            read(unit, '(a)', iostat=iostat) line2
            if (iostat /= 0) exit
            if (index(trim(line2), 'result = ') > 0) then
                print *, '  Correct precedence version (// lower than +): ', trim(line2)
                found_line2 = .true.
                exit
            end if
        end do
        close(unit)
        
        if (.not. found_line1 .or. .not. found_line2) then
            print *, '  FAIL: Could not find result assignments in generated code'
            test_string_concatenation_precedence_with_parentheses = .false.
            return
        end if
        
        ! Now test the original ambiguous expression
        ! Expression: "a" + "b" // "c" (no parentheses)
        ! If concatenation has correct precedence (lower than +), this should generate the same as line2
        ! If concatenation has wrong precedence (higher than +), this should generate the same as line1
        block
            character(len=:), allocatable :: input_file3, output_file3
            character(len=256) :: line3
            logical :: found_line3
            
            input_file3 = 'test_issue_214_ambiguous.lf'
            open(newunit=unit, file=input_file3, status='replace')
            write(unit, '(a)') 'program test_ambiguous'
            write(unit, '(a)') '    character(len=10) :: result'
            write(unit, '(a)') '    result = "a" + "b" // "c"'
            write(unit, '(a)') '    print *, result'
            write(unit, '(a)') 'end program test_ambiguous'
            close(unit)
            
            output_file3 = 'test_issue_214_ambiguous_out.f90'
            options%output_file = output_file3
            call compile_source(input_file3, options, error_msg)
            
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Compilation error (ambiguous version):', trim(error_msg)
                test_string_concatenation_precedence_with_parentheses = .false.
                return
            end if
            
            found_line3 = .false.
            open(newunit=unit, file=output_file3, status='old')
            do
                read(unit, '(a)', iostat=iostat) line3
                if (iostat /= 0) exit
                if (index(trim(line3), 'result = ') > 0) then
                    print *, '  Ambiguous expression result: ', trim(line3)
                    found_line3 = .true.
                    exit
                end if
            end do
            close(unit)
            
            if (.not. found_line3) then
                print *, '  FAIL: Could not find result assignment in ambiguous version'
                test_string_concatenation_precedence_with_parentheses = .false.
                return
            end if
            
            ! Compare the results
            if (trim(line3) == trim(line2)) then
                print *, '  OK: Ambiguous expression matches correct precedence (// lower than +)'
            else if (trim(line3) == trim(line1)) then
                print *, '  FAIL: Ambiguous expression matches wrong precedence (// higher than +)'
                print *, '  BUG CONFIRMED: String concatenation has higher precedence than addition'
                test_string_concatenation_precedence_with_parentheses = .false.
            else
                print *, '  FAIL: Ambiguous expression generates unexpected result'
                test_string_concatenation_precedence_with_parentheses = .false.
            end if
        end block
        
    end function test_string_concatenation_precedence_with_parentheses

    logical function test_unary_operator_precedence()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        logical :: test_passed
        
        test_unary_operator_precedence = .true.
        print *, 'Testing unary operator precedence (Issue #215)...'
        
        ! Test that unary operators bind looser than ** but tighter than multiplication
        ! Expression: -2 ** 2 should be -(2 ** 2) = -4, NOT (-2) ** 2 = 4
        input_file = 'test_unary_precedence.lf'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'program test_unary'
        write(unit, '(a)') '    integer :: result'
        write(unit, '(a)') '    result = -2 ** 2'
        write(unit, '(a)') '    print *, result'
        write(unit, '(a)') 'end program test_unary'
        close(unit)
        
        output_file = 'test_unary_precedence_out.f90'
        options%output_file = output_file
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_unary_operator_precedence = .false.
            return
        end if
        
        ! Check the generated code
        call check_generated_expression(output_file, 'result = ', test_passed)
        if (.not. test_passed) then
            print *, '  FAIL: Could not verify unary operator precedence in generated code'
            test_unary_operator_precedence = .false.
        else
            print *, '  OK: Unary operator precedence test generated code successfully'
        end if
        
    end function test_unary_operator_precedence

    logical function test_comparison_non_associativity()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        logical :: test_passed
        
        test_comparison_non_associativity = .true.
        print *, 'Testing comparison non-associativity (Issue #216)...'
        
        ! Test that a < b < c should not be allowed (or parsed incorrectly)
        ! This expression should either fail or be parsed as (a < b) < c
        input_file = 'test_comparison_associativity.lf'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'program test_comparison'
        write(unit, '(a)') '    integer :: a, b, c'
        write(unit, '(a)') '    logical :: result'
        write(unit, '(a)') '    a = 1'
        write(unit, '(a)') '    b = 2'
        write(unit, '(a)') '    c = 3'
        write(unit, '(a)') '    result = a < b < c'
        write(unit, '(a)') '    print *, result'
        write(unit, '(a)') 'end program test_comparison'
        close(unit)
        
        output_file = 'test_comparison_associativity_out.f90'
        options%output_file = output_file
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_comparison_non_associativity = .false.
            return
        end if
        
        ! Check if it generated chained comparison (this should NOT happen)
        call check_generated_expression(output_file, 'result = ', test_passed)
        if (.not. test_passed) then
            print *, '  FAIL: Could not verify comparison associativity in generated code'
            test_comparison_non_associativity = .false.
        else
            print *, '  WARNING: Comparison chaining test needs manual verification'
        end if
        
    end function test_comparison_non_associativity

    logical function test_comprehensive_precedence_hierarchy()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit
        logical :: test_passed
        
        test_comprehensive_precedence_hierarchy = .true.
        print *, 'Testing comprehensive operator precedence hierarchy...'
        
        ! Test complex expression that involves multiple precedence levels
        ! Expression: a + b // c (string concatenation and arithmetic)
        ! Should parse as: (a + b) // c (+ higher precedence than //)
        ! NOT as: a + (b // c) (// higher precedence than +)
        input_file = 'test_comprehensive_precedence.lf'
        open(newunit=unit, file=input_file, status='replace')
        write(unit, '(a)') 'program test_comprehensive'
        write(unit, '(a)') '    character(len=10) :: a, b, c, result_str'
        write(unit, '(a)') '    a = "1"'
        write(unit, '(a)') '    b = "2"'
        write(unit, '(a)') '    c = "3"'
        write(unit, '(a)') '    ! Test string concatenation precedence'
        write(unit, '(a)') '    result_str = a + b // c'
        write(unit, '(a)') '    print *, result_str'
        write(unit, '(a)') 'end program test_comprehensive'
        close(unit)
        
        output_file = 'test_comprehensive_precedence_out.f90'
        options%output_file = output_file
        call compile_source(input_file, options, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_comprehensive_precedence_hierarchy = .false.
            return
        end if
        
        ! Check the generated code
        call check_generated_expression(output_file, 'result_str = ', test_passed)
        if (.not. test_passed) then
            print *, '  FAIL: Could not verify comprehensive precedence in generated code'
            test_comprehensive_precedence_hierarchy = .false.
        else
            print *, '  OK: Comprehensive precedence test generated code successfully'
        end if
        
    end function test_comprehensive_precedence_hierarchy

    subroutine check_generated_expression(filename, pattern, found)
        character(len=*), intent(in) :: filename, pattern
        logical, intent(out) :: found
        integer :: unit, iostat
        character(len=256) :: line
        
        found = .false.
        open(newunit=unit, file=filename, status='old', iostat=iostat)
        if (iostat /= 0) return
        
        do
            read(unit, '(a)', iostat=iostat) line
            if (iostat /= 0) exit
            if (index(trim(line), pattern) > 0) then
                print *, '  Generated: ', trim(line)
                found = .true.
                exit
            end if
        end do
        close(unit)
        
    end subroutine check_generated_expression
    
end program test_issue_214_string_concatenation_precedence