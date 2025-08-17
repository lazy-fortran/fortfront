program test_nested_control_structures
    ! Test Assessment and Implementation for Issue #255
    ! Given: Need comprehensive nested control structure test coverage
    ! When: Testing parser handling of nested if/do/select constructs  
    ! Then: All nested structures should parse correctly with proper nesting relationships

    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    logical :: all_passed
    integer :: test_count, passed_count
    
    test_count = 0
    passed_count = 0
    all_passed = .true.
    
    print *, '=== Nested Control Structures Test Coverage (Issue #255) ==='
    print *, 'Testing parser handling of nested if/do/select constructs'
    print *
    
    ! Test nested if statements
    call run_test('Nested if statements', test_nested_if_statements())
    
    ! Test nested do loops  
    call run_test('Nested do loops', test_nested_do_loops())
    
    ! Test if inside do loop
    call run_test('If statement inside do loop', test_if_inside_do())
    
    ! Test do inside if statement
    call run_test('Do loop inside if statement', test_do_inside_if())
    
    ! Test select case inside do loop  
    call run_test('Select case inside do loop', test_select_inside_do())
    
    ! Test nested select case structures
    call run_test('Nested select case structures', test_nested_select_case())
    
    ! Test complex nested combinations (Issue #255 example)
    call run_test('Complex nested combinations (Issue #255)', test_issue_255_example())
    
    ! Test deeply nested structures (stress test)
    call run_test('Deeply nested structures', test_deeply_nested_structures())
    
    ! Test edge cases with empty blocks
    call run_test('Nested structures with empty blocks', test_nested_empty_blocks())
    
    ! Test nested structures with multiple statements
    call run_test('Nested structures with multiple statements', test_nested_multiple_statements())
    
    ! Report results
    print *
    print *, 'Test Results:'
    print *, '  Total tests:', test_count
    print *, '  Tests passing:', passed_count
    print *, '  Tests failing:', test_count - passed_count
    print *
    
    if (passed_count == test_count) then
        print *, 'SUCCESS: All nested control structure tests PASS!'
        print *, 'Issue #255 nested parsing appears to be working correctly'
        stop 0
    else
        print *, 'ISSUES FOUND: Some nested control structure tests FAIL!'
        print *, 'Issue #255 nested control structure parsing needs implementation work'
        stop 1
    end if
    
contains

    subroutine run_test(test_name, result)
        character(len=*), intent(in) :: test_name
        logical, intent(in) :: result
        
        test_count = test_count + 1
        if (result) then
            passed_count = passed_count + 1
            print *, '  PASS:', test_name
        else
            print *, '  FAIL:', test_name
            all_passed = .false.
        end if
    end subroutine

    function test_nested_if_statements() result(test_passes)
        ! Given: Nested if statements with multiple levels
        ! When: Parser processes nested if constructs
        ! Then: Should maintain proper nesting structure without flattening
        logical :: test_passes
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'program test' // new_line('a') // &
                'implicit none' // new_line('a') // &
                'integer :: x, y' // new_line('a') // &
                'x = 5; y = 3' // new_line('a') // &
                'if (x > 0) then' // new_line('a') // &
                '    if (y > 0) then' // new_line('a') // &
                '        if (x > y) then' // new_line('a') // &
                '            print *, "x > y > 0"' // new_line('a') // &
                '        end if' // new_line('a') // &
                '    end if' // new_line('a') // &
                'end if' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should parse without error and preserve nested structure
        test_passes = (error_msg == "") .and. &
                     (index(output, 'if (x > 0)') > 0) .and. &
                     (index(output, 'if (y > 0)') > 0) .and. &
                     (index(output, 'if (x > y)') > 0) .and. &
                     (index(output, 'print *, "x > y > 0"') > 0)
    end function

    function test_nested_do_loops() result(test_passes)
        ! Given: Nested do loops with different variables
        ! When: Parser processes nested do constructs
        ! Then: Should maintain proper loop nesting without collapsing
        logical :: test_passes
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'program test' // new_line('a') // &
                'implicit none' // new_line('a') // &
                'integer :: i, j, k' // new_line('a') // &
                'do i = 1, 3' // new_line('a') // &
                '    do j = 1, 2' // new_line('a') // &
                '        do k = 1, 1' // new_line('a') // &
                '            print *, i, j, k' // new_line('a') // &
                '        end do' // new_line('a') // &
                '    end do' // new_line('a') // &
                'end do' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should parse without error and preserve all three nested loops
        test_passes = (error_msg == "") .and. &
                     (index(output, 'do i = 1, 3') > 0) .and. &
                     (index(output, 'do j = 1, 2') > 0) .and. &
                     (index(output, 'do k = 1, 1') > 0)
    end function

    function test_if_inside_do() result(test_passes)
        ! Given: If statement nested inside do loop
        ! When: Parser processes mixed control structures  
        ! Then: Should correctly parse both structures maintaining hierarchy
        logical :: test_passes
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'program test' // new_line('a') // &
                'implicit none' // new_line('a') // &
                'integer :: i' // new_line('a') // &
                'do i = 1, 5' // new_line('a') // &
                '    if (i .eq. 3) then' // new_line('a') // &
                '        print *, "Found three"' // new_line('a') // &
                '    else' // new_line('a') // &
                '        print *, "Number:", i' // new_line('a') // &
                '    end if' // new_line('a') // &
                'end do' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should parse without error and preserve if-else inside do
        test_passes = (error_msg == "") .and. &
                     (index(output, 'do i = 1, 5') > 0) .and. &
                     (index(output, 'if (i .eq. 3)') > 0) .and. &
                     (index(output, 'else') > 0)
    end function

    function test_do_inside_if() result(test_passes)
        ! Given: Do loop nested inside if statement
        ! When: Parser processes mixed control structures
        ! Then: Should correctly parse both structures with proper nesting
        logical :: test_passes
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'program test' // new_line('a') // &
                'implicit none' // new_line('a') // &
                'integer :: i, n' // new_line('a') // &
                'n = 5' // new_line('a') // &
                'if (n > 0) then' // new_line('a') // &
                '    do i = 1, n' // new_line('a') // &
                '        print *, "Iteration:", i' // new_line('a') // &
                '    end do' // new_line('a') // &
                'else' // new_line('a') // &
                '    print *, "n is not positive"' // new_line('a') // &
                'end if' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should parse without error and preserve do loop inside if-else
        test_passes = (error_msg == "") .and. &
                     (index(output, 'if (n > 0)') > 0) .and. &
                     (index(output, 'do i = 1, n') > 0) .and. &
                     (index(output, 'else') > 0)
    end function

    function test_select_inside_do() result(test_passes)
        ! Given: Select case nested inside do loop
        ! When: Parser processes select case within loop
        ! Then: Should correctly parse both structures without interference
        logical :: test_passes
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'program test' // new_line('a') // &
                'implicit none' // new_line('a') // &
                'integer :: i' // new_line('a') // &
                'do i = 1, 3' // new_line('a') // &
                '    select case (i)' // new_line('a') // &
                '    case (1)' // new_line('a') // &
                '        print *, "One"' // new_line('a') // &
                '    case (2)' // new_line('a') // &
                '        print *, "Two"' // new_line('a') // &
                '    case default' // new_line('a') // &
                '        print *, "Other"' // new_line('a') // &
                '    end select' // new_line('a') // &
                'end do' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should parse without error and preserve select case inside do loop
        test_passes = (error_msg == "") .and. &
                     (index(output, 'do i = 1, 3') > 0) .and. &
                     (index(output, 'select case (i)') > 0) .and. &
                     (index(output, 'case (1)') > 0)
    end function

    function test_nested_select_case() result(test_passes)
        ! Given: Select case with nested if statements in cases
        ! When: Parser processes nested select structures
        ! Then: Should maintain proper structure within each case
        logical :: test_passes
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'program test' // new_line('a') // &
                'implicit none' // new_line('a') // &
                'integer :: x, y' // new_line('a') // &
                'x = 1; y = 2' // new_line('a') // &
                'select case (x)' // new_line('a') // &
                '    case (1)' // new_line('a') // &
                '        if (y > 0) then' // new_line('a') // &
                '            print *, "Case 1, y positive"' // new_line('a') // &
                '        end if' // new_line('a') // &
                '    case (2)' // new_line('a') // &
                '        if (y < 0) then' // new_line('a') // &
                '            print *, "Case 2, y negative"' // new_line('a') // &
                '        end if' // new_line('a') // &
                'end select' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should parse without error and preserve if statements within cases
        test_passes = (error_msg == "") .and. &
                     (index(output, 'select case (x)') > 0) .and. &
                     (index(output, 'if (y > 0)') > 0) .and. &
                     (index(output, 'if (y < 0)') > 0)
    end function

    function test_issue_255_example() result(test_passes)
        ! Given: The exact example from Issue #255 
        ! When: Parser processes deeply nested if/do combinations
        ! Then: Should parse correctly without losing nested structure
        logical :: test_passes
        character(len=:), allocatable :: source, output, error_msg
        
        ! Exact code from Issue #255
        source = 'program test_nested' // new_line('a') // &
                'implicit none' // new_line('a') // &
                'integer :: i, j' // new_line('a') // &
                '' // new_line('a') // &
                'do i = 1, 5' // new_line('a') // &
                '    if (i > 2) then' // new_line('a') // &
                '        do j = 1, i' // new_line('a') // &
                '            if (j == 2) then' // new_line('a') // &
                '                print *, "Found j=2 at i=", i' // new_line('a') // &
                '            end if' // new_line('a') // &
                '        end do' // new_line('a') // &
                '    end if' // new_line('a') // &
                'end do' // new_line('a') // &
                'end program test_nested'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! CRITICAL: This is the core test for Issue #255
        ! Should parse without error and preserve all nested structures
        test_passes = (error_msg == "") .and. &
                     (index(output, 'do i = 1, 5') > 0) .and. &
                     (index(output, 'if (i > 2)') > 0) .and. &
                     (index(output, 'do j = 1, i') > 0) .and. &
                     (index(output, 'if (j == 2)') > 0) .and. &
                     (index(output, 'Found j=2 at i=') > 0)
    end function

    function test_deeply_nested_structures() result(test_passes)
        ! Given: Deeply nested structures (stress test for parser)
        ! When: Parser processes multiple levels of nesting
        ! Then: Should handle deep nesting without stack overflow or corruption
        logical :: test_passes
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'program test' // new_line('a') // &
                'implicit none' // new_line('a') // &
                'integer :: a, b, c, d' // new_line('a') // &
                'a = 1; b = 2; c = 3; d = 4' // new_line('a') // &
                'if (a > 0) then' // new_line('a') // &
                '    do b = 1, 2' // new_line('a') // &
                '        if (b > 0) then' // new_line('a') // &
                '            do c = 1, 1' // new_line('a') // &
                '                if (c > 0) then' // new_line('a') // &
                '                    print *, a, b, c' // new_line('a') // &
                '                end if' // new_line('a') // &
                '            end do' // new_line('a') // &
                '        end if' // new_line('a') // &
                '    end do' // new_line('a') // &
                'end if' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should parse without error and preserve all nesting levels
        test_passes = (error_msg == "") .and. &
                     (index(output, 'if (a > 0)') > 0) .and. &
                     (index(output, 'do b = 1, 2') > 0) .and. &
                     (index(output, 'if (b > 0)') > 0) .and. &
                     (index(output, 'do c = 1, 1') > 0) .and. &
                     (index(output, 'if (c > 0)') > 0)
    end function

    function test_nested_empty_blocks() result(test_passes)
        ! Given: Nested structures with some empty blocks
        ! When: Parser processes structures with empty nested blocks
        ! Then: Should handle empty blocks correctly without errors
        logical :: test_passes
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'program test' // new_line('a') // &
                'implicit none' // new_line('a') // &
                'integer :: i, j' // new_line('a') // &
                'do i = 1, 2' // new_line('a') // &
                '    if (i == 1) then' // new_line('a') // &
                '        ! Empty block' // new_line('a') // &
                '    else' // new_line('a') // &
                '        do j = 1, 1' // new_line('a') // &
                '            ! Empty nested block' // new_line('a') // &
                '        end do' // new_line('a') // &
                '    end if' // new_line('a') // &
                'end do' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should parse without error even with empty nested blocks
        test_passes = (error_msg == "") .and. &
                     (index(output, 'do i = 1, 2') > 0) .and. &
                     (index(output, 'if (i == 1)') > 0) .and. &
                     (index(output, 'else') > 0) .and. &
                     (index(output, 'do j = 1, 1') > 0)
    end function

    function test_nested_multiple_statements() result(test_passes)
        ! Given: Nested structures with multiple statements in each block
        ! When: Parser processes complex nested blocks with multiple statements
        ! Then: Should preserve all statements in their correct nested context
        logical :: test_passes
        character(len=:), allocatable :: source, output, error_msg
        
        source = 'program test' // new_line('a') // &
                'implicit none' // new_line('a') // &
                'integer :: i, j, sum' // new_line('a') // &
                'sum = 0' // new_line('a') // &
                'do i = 1, 3' // new_line('a') // &
                '    print *, "Outer loop i =", i' // new_line('a') // &
                '    if (i > 1) then' // new_line('a') // &
                '        print *, "i is greater than 1"' // new_line('a') // &
                '        do j = 1, 2' // new_line('a') // &
                '            print *, "Inner loop j =", j' // new_line('a') // &
                '            sum = sum + i * j' // new_line('a') // &
                '            print *, "Current sum =", sum' // new_line('a') // &
                '        end do' // new_line('a') // &
                '        print *, "Finished inner loop"' // new_line('a') // &
                '    end if' // new_line('a') // &
                '    print *, "End of outer iteration"' // new_line('a') // &
                'end do' // new_line('a') // &
                'print *, "Final sum =", sum' // new_line('a') // &
                'end program'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Should parse without error and preserve all statements in correct nesting
        test_passes = (error_msg == "") .and. &
                     (index(output, 'do i = 1, 3') > 0) .and. &
                     (index(output, 'if (i > 1)') > 0) .and. &
                     (index(output, 'do j = 1, 2') > 0) .and. &
                     (index(output, 'sum = sum + i') > 0) .and. &
                     (index(output, 'Final sum =') > 0)
    end function

end program test_nested_control_structures