program test_issue_320_function_variable_declarations
    !! RED Phase tests for Issue #320 - Variables not declared in function
    !! These tests ensure that code generation properly emits variable declarations
    !! for function parameters and result variables when they are inferred by semantic analysis
    use frontend, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed
    
    all_passed = .true.

    ! Run RED phase tests - these should FAIL until Issue #320 is fixed
    if (.not. test_simple_function_missing_declarations()) all_passed = .false.
    if (.not. test_multiple_parameters_missing_declarations()) all_passed = .false.
    if (.not. test_mixed_explicit_implicit_declarations()) all_passed = .false.
    if (.not. test_function_with_complex_result_type()) all_passed = .false.

    ! Report results
    if (all_passed) then
        print '(a)', "All Issue #320 RED phase tests passed"
    else
        print '(a)', "Some Issue #320 RED phase tests failed (EXPECTED in RED phase)"
        error stop 1
    end if

contains

    logical function test_simple_function_missing_declarations()
        !! Given: A simple function with inferred parameter and result types
        !! When: Code generation processes the function
        !! Then: Variable declarations should be generated for parameters and result
        character(len=:), allocatable :: input, output, error_msg
        
        test_simple_function_missing_declarations = .true.
        print '(a)', "Testing simple function missing declarations (Issue #320)..."
        
        ! Given: Simple function from Issue #320 example
        input = "function twice(x) result(y)" // new_line('A') // &
                "y = 2*x" // new_line('A') // &
                "end function"
        
        ! When: Transform the input
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Check for transformation errors
        if (error_msg /= "") then
            print '(a)', "FAIL: Error transforming function: " // error_msg
            test_simple_function_missing_declarations = .false.
            return
        end if
        
        ! Then: Output should contain proper variable declarations
        ! NOTE: This test will FAIL in RED phase - that's expected
        if (index(output, "intent(in) :: x") > 0 .and. &
            index(output, ":: y") > 0 .and. &
            index(output, "y = 2*x") > 0) then
            print '(a)', "PASS: Function contains proper variable declarations"
        else
            print '(a)', "FAIL: Function missing variable declarations (RED phase - expected)"
            print '(a)', "Generated output:"
            print '(a)', output
            print '(a)', "Expected to contain: intent(in) :: x and :: y declarations"
            test_simple_function_missing_declarations = .false.
        end if
    end function test_simple_function_missing_declarations

    logical function test_multiple_parameters_missing_declarations()
        !! Given: A function with multiple parameters requiring declarations
        !! When: Code generation processes the function
        !! Then: All parameter declarations should be generated
        character(len=:), allocatable :: input, output, error_msg
        
        test_multiple_parameters_missing_declarations = .true.
        print '(a)', "Testing multiple parameters missing declarations..."
        
        ! Given: Function with multiple parameters
        input = "function calculate(a, b, c) result(value)" // new_line('A') // &
                "value = a + b * c" // new_line('A') // &
                "end function"
        
        ! When: Transform the input
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Check for transformation errors
        if (error_msg /= "") then
            print '(a)', "FAIL: Error transforming function: " // error_msg
            test_multiple_parameters_missing_declarations = .false.
            return
        end if
        
        ! Then: Output should contain declarations for all parameters
        ! NOTE: This test will FAIL in RED phase - that's expected
        if (index(output, "intent(in) :: a") > 0 .and. &
            index(output, "intent(in) :: b") > 0 .and. &
            index(output, "intent(in) :: c") > 0 .and. &
            index(output, ":: value") > 0) then
            print '(a)', "PASS: Function contains all parameter declarations"
        else
            print '(a)', "FAIL: Function missing parameter declarations (RED phase - expected)"
            print '(a)', "Generated output:"
            print '(a)', output
            print '(a)', "Expected to contain: intent(in) :: a, b, c and :: value declarations"
            test_multiple_parameters_missing_declarations = .false.
        end if
    end function test_multiple_parameters_missing_declarations

    logical function test_mixed_explicit_implicit_declarations()
        !! Given: A function with some explicit and some implicit declarations
        !! When: Code generation processes the function
        !! Then: Only missing declarations should be generated
        character(len=:), allocatable :: input, output, error_msg
        
        test_mixed_explicit_implicit_declarations = .true.
        print '(a)', "Testing mixed explicit/implicit declarations..."
        
        ! Given: Function with partial explicit declarations
        input = "function process(x, y) result(z)" // new_line('A') // &
                "    real, intent(in) :: x" // new_line('A') // &
                "    z = x + y" // new_line('A') // &
                "end function"
        
        ! When: Transform the input
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Check for transformation errors
        if (error_msg /= "") then
            print '(a)', "FAIL: Error transforming function: " // error_msg
            test_mixed_explicit_implicit_declarations = .false.
            return
        end if
        
        ! Then: Output should preserve explicit declarations and add missing ones
        ! NOTE: This test will FAIL in RED phase - that's expected
        if (index(output, "real, intent(in) :: x") > 0 .and. &
            index(output, "intent(in) :: y") > 0 .and. &
            index(output, ":: z") > 0) then
            print '(a)', "PASS: Function preserves explicit and adds missing declarations"
        else
            print '(a)', "FAIL: Function not handling mixed declarations (RED phase - expected)"
            print '(a)', "Generated output:"
            print '(a)', output
            print '(a)', "Expected: preserve 'real, intent(in) :: x' and add declarations for y, z"
            test_mixed_explicit_implicit_declarations = .false.
        end if
    end function test_mixed_explicit_implicit_declarations

    logical function test_function_with_complex_result_type()
        !! Given: A function with complex operations requiring type inference
        !! When: Code generation processes the function
        !! Then: Proper typed declarations should be generated
        character(len=:), allocatable :: input, output, error_msg
        
        test_function_with_complex_result_type = .true.
        print '(a)', "Testing function with complex result type..."
        
        ! Given: Function with operations that require type inference
        input = "function square_sum(x, y) result(total)" // new_line('A') // &
                "total = x*x + y*y" // new_line('A') // &
                "end function"
        
        ! When: Transform the input
        call transform_lazy_fortran_string(input, output, error_msg)
        
        ! Check for transformation errors
        if (error_msg /= "") then
            print '(a)', "FAIL: Error transforming function: " // error_msg
            test_function_with_complex_result_type = .false.
            return
        end if
        
        ! Then: Output should contain proper type declarations based on inference
        ! NOTE: This test will FAIL in RED phase - that's expected
        if (index(output, "intent(in) :: x") > 0 .and. &
            index(output, "intent(in) :: y") > 0 .and. &
            index(output, ":: total") > 0 .and. &
            index(output, "total = x*x + y*y") > 0) then
            print '(a)', "PASS: Function contains proper type declarations for complex expression"
        else
            print '(a)', "FAIL: Function missing declarations for complex expression (RED phase - expected)"
            print '(a)', "Generated output:"
            print '(a)', output
            print '(a)', "Expected: proper declarations for x, y, total with inferred types"
            test_function_with_complex_result_type = .false.
        end if
    end function test_function_with_complex_result_type

end program test_issue_320_function_variable_declarations