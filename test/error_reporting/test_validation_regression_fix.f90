program test_validation_regression_fix
    use frontend, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed
    integer :: test_count, passed_count

    test_count = 0
    passed_count = 0
    
    print *, '=== Validation Regression Fix Tests for Issue #256 ==='
    print *, 'Testing specific validation issues mentioned in Issue #256 description'
    print *

    ! Test 1: Comments-only input should be accepted (mentioned in issue description)
    call run_test('Comments-only input should be accepted', &
                  test_comments_only_input())
    
    ! Test 2: Simple expressions without keywords should be accepted
    call run_test('Simple expressions without keywords should be accepted', &
                  test_simple_expressions())
    
    ! Test 3: Valid lazy Fortran constructs should not be rejected
    call run_test('Valid lazy Fortran constructs should not be rejected', &
                  test_valid_lazy_fortran())
    
    ! Test 4: Complete garbage should produce meaningful error messages (not silent fallback)
    call run_test('Complete garbage should produce meaningful errors', &
                  test_complete_garbage_error())
    
    ! Test 5: Invalid syntax should produce error output, not empty programs  
    call run_test('Invalid syntax should produce error output', &
                  test_invalid_syntax_error_output())
    
    ! Test 6: Mixed valid/invalid should handle gracefully
    call run_test('Mixed valid/invalid should handle gracefully', &
                  test_mixed_content())

    ! Report results
    print *
    print *, 'Validation Regression Fix Results:'
    print *, '  Total tests:', test_count
    print *, '  Tests passed:', passed_count
    print *, '  Tests failed:', test_count - passed_count
    print *
    
    if (passed_count == test_count) then
        print *, 'SUCCESS: All validation regression issues are fixed!'
        print *, 'Issue #256 validation logic is working correctly.'
        stop 0
    else
        print *, 'FAILURE: Some validation regression issues remain.'
        print *, 'Additional work needed on validation logic.'
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
        end if
    end subroutine

    ! Test that comments-only input is accepted (Issue #256 specific)
    function test_comments_only_input() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! Given: A source file with only comments
        source = '! This is just a comment' // new_line('a') // &
                '! Another comment line'
        
        ! When: We process the input
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Then: Should be accepted without error and produce valid output
        passed = len_trim(error_msg) == 0 .and. &
                len_trim(output) > 0 .and. &
                index(output, '! COMPILATION FAILED') == 0
    end function

    ! Test that simple expressions without keywords are accepted
    function test_simple_expressions() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! Given: Simple expression without explicit keywords
        source = 'x = 42'
        
        ! When: We process the input
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Then: Should be accepted and produce valid Fortran
        passed = len_trim(error_msg) == 0 .and. &
                index(output, 'x = 42') > 0 .and. &
                index(output, 'program main') > 0
    end function

    ! Test that valid lazy Fortran constructs are not rejected
    function test_valid_lazy_fortran() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! Given: Valid lazy Fortran with multiple statements
        source = 'x = 5' // new_line('a') // &
                'y = x * 2' // new_line('a') // &
                'print *, y'
        
        ! When: We process the input
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Then: Should be accepted and produce valid Fortran
        passed = len_trim(error_msg) == 0 .and. &
                index(output, 'x = 5') > 0 .and. &
                (index(output, 'y = x * 2') > 0 .or. index(output, 'y = x*2') > 0) .and. &
                index(output, 'print') > 0
    end function

    ! Test that complete garbage produces meaningful error messages
    function test_complete_garbage_error() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! Given: Complete garbage input (truly invalid)
        source = 'this is not valid fortran syntax at all *** 123 %%% @#$%'
        
        ! When: We process the input
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Then: Should produce error message (not silent fallback)
        ! Either error_msg should be populated OR output should contain error comments
        passed = len_trim(error_msg) > 0 .or. &
                index(output, '! COMPILATION FAILED') > 0 .or. &
                index(output, '! Error:') > 0
    end function

    ! Test that invalid syntax produces error output instead of empty programs
    function test_invalid_syntax_error_output() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! Given: Invalid syntax that should be caught
        source = 'invalid_keyword_xyz 123 $$$ nonsense'
        
        ! When: We process the input
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Then: Output should indicate compilation failure, not look like valid code
        passed = index(output, '! COMPILATION FAILED') > 0 .or. &
                index(output, '! Error:') > 0 .or. &
                len_trim(error_msg) > 0
    end function

    ! Test mixed valid/invalid content handling
    function test_mixed_content() result(passed)
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        
        ! Given: Mix of valid comments and potentially problematic content
        source = '! Valid comment' // new_line('a') // &
                'x = 42' // new_line('a') // &
                '! Another comment'
        
        ! When: We process the input
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Then: Should handle gracefully (this is valid lazy Fortran)
        passed = len_trim(error_msg) == 0 .and. &
                index(output, 'x = 42') > 0 .and. &
                index(output, '! COMPILATION FAILED') == 0
    end function

end program test_validation_regression_fix