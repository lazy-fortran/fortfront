program test_issue_206_integer_array_literals
    ! TDD Tests for Issue #206: Type inference defaults to real(8) for integer array literals
    ! RED phase: All tests MUST FAIL initially to drive implementation
    
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "=== Issue #206: Integer Array Literal Type Inference Tests ==="
    print *, "Testing that integer array literals correctly infer integer type"
    print *, ""
    
    ! Run all test cases - these MUST fail initially
    call test_single_integer_array_literal(all_tests_passed)
    call test_multiple_integer_array_literal(all_tests_passed)
    call test_integer_array_extension_literal(all_tests_passed)
    call test_integer_expression_array_literal(all_tests_passed)
    call test_real_array_literals_unchanged(all_tests_passed)
    call test_mixed_type_array_resolution(all_tests_passed)
    
    if (all_tests_passed) then
        print *, ""
        print *, "SUCCESS: All tests passed - Issue #206 implementation complete!"
        print *, "Integer array literal type inference working correctly"
        stop 0
    else
        print *, ""
        print *, "FAILURE: Some tests failed - implementation needs work"
        stop 1
    end if

contains

    subroutine test_single_integer_array_literal(all_passed)
        ! Given: Array literal with single integer [10]  
        ! When: Compiling through standardizer
        ! Then: Should infer integer, allocatable :: v(:) NOT real(8)
        logical, intent(inout) :: all_passed
        
        print *, "Test: Single integer array literal [10]"
        
        block
            character(len=*), parameter :: input_code = &
                "v = [10]" // new_line('a') // &
                "print*, v"
            character(len=:), allocatable :: result, error_msg
            
            call transform_lazy_fortran_string(input_code, result, error_msg)
            
            if (error_msg /= "") then
                print *, "FAIL: Compilation error:", trim(error_msg)
                all_passed = .false.
                return
            end if
            
            ! Should contain "integer :: v(1)" (fixed dimension for single element)
            if (index(result, "integer :: v(1)") == 0) then
                print *, "FAIL: Should declare v as integer :: v(1)"
                print *, "Found:"
                print *, trim(result)
                all_passed = .false.
                return
            end if
            
            ! Should NOT contain "real(8) :: v(1)"
            if (index(result, "real(8) :: v(1)") > 0) then
                print *, "FAIL: Should NOT declare v as real(8) :: v(1)"
                print *, "Found:"
                print *, trim(result)
                all_passed = .false.
                return
            end if
            
            print *, "PASS: Single integer array literal correctly inferred"
        end block
        
    end subroutine test_single_integer_array_literal

    subroutine test_multiple_integer_array_literal(all_passed)
        ! Given: Array literal with multiple integers [1, 2, 3]
        ! When: Compiling through standardizer
        ! Then: Should infer integer, dimension(3) :: v NOT real(8)
        logical, intent(inout) :: all_passed
        
        print *, "Test: Multiple integer array literal [1, 2, 3]"
        
        block
            character(len=*), parameter :: input_code = &
                "v = [1, 2, 3]" // new_line('a') // &
                "print*, v"
            character(len=:), allocatable :: result, error_msg
            
            call transform_lazy_fortran_string(input_code, result, error_msg)
            
            if (error_msg /= "") then
                print *, "FAIL: Compilation error:", trim(error_msg)
                all_passed = .false.
                return
            end if
            
            ! Should contain "integer :: v(3)" 
            if (index(result, "integer :: v(3)") == 0) then
                print *, "FAIL: Should declare v as integer :: v(3)"
                print *, "Found:"
                print *, trim(result)
                all_passed = .false.
                return
            end if
            
            ! Should NOT contain "real(8) :: v(3)"
            if (index(result, "real(8) :: v(3)") > 0) then
                print *, "FAIL: Should NOT declare v as real(8) :: v(3)"
                print *, "Found:"
                print *, trim(result)
                all_passed = .false.
                return
            end if
            
            print *, "PASS: Multiple integer array literal correctly inferred"
        end block
        
    end subroutine test_multiple_integer_array_literal

    subroutine test_integer_array_extension_literal(all_passed)
        ! Given: Array extension with integers v = [v, v**2]
        ! When: v was previously assigned [10] (integer array)
        ! Then: Should maintain integer type for extended array
        logical, intent(inout) :: all_passed
        
        print *, "Test: Integer array extension [v, v**2]"
        
        block
            character(len=*), parameter :: input_code = &
                "v = [10]" // new_line('a') // &
                "v = [v, v**2]" // new_line('a') // &
                "print*, v"
            character(len=:), allocatable :: result, error_msg
            
            call transform_lazy_fortran_string(input_code, result, error_msg)
            
            if (error_msg /= "") then
                print *, "FAIL: Compilation error:", trim(error_msg)
                all_passed = .false.
                return
            end if
            
            ! Should contain "integer, allocatable :: v(:)"
            if (index(result, "integer, allocatable :: v(:)") == 0) then
                print *, "FAIL: Should declare v as integer, allocatable :: v(:)"
                print *, "Found:"
                print *, trim(result)
                all_passed = .false.
                return
            end if
            
            ! Should NOT contain "real(8), allocatable :: v(:)" 
            if (index(result, "real(8), allocatable :: v(:)") > 0) then
                print *, "FAIL: Should NOT declare v as real(8), allocatable :: v(:)"
                print *, "Found:"
                print *, trim(result)
                all_passed = .false.
                return
            end if
            
            print *, "PASS: Integer array extension correctly inferred"
        end block
        
    end subroutine test_integer_array_extension_literal

    subroutine test_integer_expression_array_literal(all_passed)
        ! Given: Array literal with integer expressions [1+2, 3*4]
        ! When: Compiling through standardizer
        ! Then: Should infer integer type for array
        logical, intent(inout) :: all_passed
        
        print *, "Test: Integer expression array literal [1+2, 3*4]"
        
        block
            character(len=*), parameter :: input_code = &
                "v = [1+2, 3*4]" // new_line('a') // &
                "print*, v"
            character(len=:), allocatable :: result, error_msg
            
            call transform_lazy_fortran_string(input_code, result, error_msg)
            
            if (error_msg /= "") then
                print *, "FAIL: Compilation error:", trim(error_msg)
                all_passed = .false.
                return
            end if
            
            ! Should contain "integer :: v(2)"
            if (index(result, "integer :: v(2)") == 0) then
                print *, "FAIL: Should declare v as integer :: v(2)"
                print *, "Found:"
                print *, trim(result)
                all_passed = .false.
                return
            end if
            
            ! Should NOT contain "real(8) :: v(2)"
            if (index(result, "real(8) :: v(2)") > 0) then
                print *, "FAIL: Should NOT declare v as real(8) :: v(2)"
                print *, "Found:"
                print *, trim(result)
                all_passed = .false.
                return
            end if
            
            print *, "PASS: Integer expression array literal correctly inferred"
        end block
        
    end subroutine test_integer_expression_array_literal

    subroutine test_real_array_literals_unchanged(all_passed)
        ! Given: Array literal with real numbers [1.0, 2.0]
        ! When: Compiling through standardizer  
        ! Then: Should still infer real(8) type (preserve existing behavior)
        logical, intent(inout) :: all_passed
        
        print *, "Test: Real array literals [1.0, 2.0] should remain real(8)"
        
        block
            character(len=*), parameter :: input_code = &
                "x = [1.0, 2.0]" // new_line('a') // &
                "print*, x"
            character(len=:), allocatable :: result, error_msg
            
            call transform_lazy_fortran_string(input_code, result, error_msg)
            
            if (error_msg /= "") then
                print *, "FAIL: Compilation error:", trim(error_msg)
                all_passed = .false.
                return
            end if
            
            ! Should contain "real(8)" declaration (fixed size is fine)
            if (index(result, "real(8) :: x(2)") == 0) then
                print *, "FAIL: Should declare x as real(8) :: x(2)"
                print *, "Found:"
                print *, trim(result)
                all_passed = .false.
                return
            end if
            
            print *, "PASS: Real array literals correctly preserved as real(8)"
        end block
        
        print *, "  This should PASS (preserve existing real behavior)"
    end subroutine test_real_array_literals_unchanged

    subroutine test_mixed_type_array_resolution(all_passed)
        ! Given: Array literal with mixed types [1, 2.0]
        ! When: Compiling through standardizer
        ! Then: Should promote to real(8) (follow Fortran promotion rules)
        logical, intent(inout) :: all_passed
        
        print *, "Test: Mixed type array [1, 2.0] should promote to real(8)"
        
        block
            character(len=*), parameter :: input_code = &
                "x = [1, 2.0]" // new_line('a') // &
                "print*, x"
            character(len=:), allocatable :: result, error_msg
            
            call transform_lazy_fortran_string(input_code, result, error_msg)
            
            if (error_msg /= "") then
                print *, "FAIL: Compilation error:", trim(error_msg)
                all_passed = .false.
                return
            end if
            
            ! Should contain "real(8)" declaration (promotion)
            if (index(result, "real(8) :: x(2)") == 0) then
                print *, "FAIL: Should declare x as real(8) :: x(2) (promotion)"
                print *, "Found:"
                print *, trim(result)
                all_passed = .false.
                return
            end if
            
            print *, "PASS: Mixed type array correctly promoted to real(8)"
        end block
        
        print *, "  This should PASS (preserve promotion behavior)"
    end subroutine test_mixed_type_array_resolution

end program test_issue_206_integer_array_literals