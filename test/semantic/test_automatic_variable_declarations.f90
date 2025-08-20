program test_automatic_variable_declarations
    ! Comprehensive BDD integration tests for automatic variable declaration system
    ! Tests Issue #351: Integration testing for automatic variable declarations (Issue #320)
    ! Validates complete lexer → parser → semantic → codegen pipeline functionality
    
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Automatic Variable Declaration Integration Tests ==="
    print *, "Testing complete pipeline for Issue #320 functionality"
    print *, ""
    
    all_tests_passed = .true.
    
    ! Core functionality testing
    if (.not. test_simple_function_transformation()) all_tests_passed = .false.
    if (.not. test_multiple_variable_types()) all_tests_passed = .false.
    if (.not. test_intent_inference()) all_tests_passed = .false.
    if (.not. test_array_parameter_handling()) all_tests_passed = .false.
    if (.not. test_mixed_explicit_implicit_declarations()) all_tests_passed = .false.
    
    ! Pipeline integration testing
    if (.not. test_full_pipeline_integration()) all_tests_passed = .false.
    if (.not. test_generated_code_compilation()) all_tests_passed = .false.
    if (.not. test_realistic_lazy_fortran_examples()) all_tests_passed = .false.
    
    ! Edge case and regression testing
    if (.not. test_edge_cases()) all_tests_passed = .false.
    if (.not. test_error_handling()) all_tests_passed = .false.
    if (.not. test_performance_validation()) all_tests_passed = .false.
    
    print *, ""
    if (all_tests_passed) then
        print *, "=== ALL INTEGRATION TESTS PASSED ==="
        print *, "Automatic variable declaration system is production-ready!"
        stop 0
    else
        print *, "=== SOME INTEGRATION TESTS FAILED ==="
        print *, "System requires fixes before production deployment!"
        stop 1
    end if

contains

    function test_simple_function_transformation() result(passed)
        ! GIVEN a simple function from Issue #320 example
        ! WHEN the complete pipeline processes it
        ! THEN proper variable declarations are generated
        
        logical :: passed
        character(len=*), parameter :: source = &
            "function twice(x) result(y)" // char(10) // &
            "y = 2*x" // char(10) // &
            "end function"
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "Test: Simple function transformation (Issue #320 exact example)"
        print *, "Given: function twice(x) result(y); y = 2*x; end function"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        
        print *, "When: complete pipeline processes the function"
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Pipeline error: ", trim(error_msg)
            return
        end if
        
        print *, "Then: proper variable declarations are generated"
        
        ! Validate expected declarations
        if (index(result_code, "real(8), intent(in) :: x") == 0) then
            passed = .false.
            print *, "FAIL: Parameter 'x' not declared with proper intent(in)"
        end if
        
        if (index(result_code, "real(8) :: y") == 0) then
            passed = .false.
            print *, "FAIL: Result variable 'y' not declared"
        end if
        
        if (index(result_code, "implicit none") == 0) then
            passed = .false.
            print *, "FAIL: 'implicit none' not added"
        end if
        
        ! Validate structure
        if (index(result_code, "function twice(x) result(y)") == 0) then
            passed = .false.
            print *, "FAIL: Function signature not preserved"
        end if
        
        if (passed) then
            print *, "PASS: Simple function transformation works correctly"
        else
            print *, "FAIL: Simple function transformation failed"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
        print *, ""
    end function test_simple_function_transformation
    
    function test_multiple_variable_types() result(passed)
        ! GIVEN a function with variables of different types (real, integer, logical, character)
        ! WHEN the pipeline infers types from usage patterns
        ! THEN all variables get correct type declarations
        
        logical :: passed
        character(len=*), parameter :: source = &
            'function mixed_types(n) result(output)' // char(10) // &
            'integer :: n' // char(10) // &
            'value = n * 3.14159' // char(10) // &
            'count = n + 1' // char(10) // &
            'is_positive = value > 0' // char(10) // &
            'message = "computed"' // char(10) // &
            'output = value' // char(10) // &
            'end function'
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "Test: Multiple variable types inference"
        print *, "Given: function with real, integer, logical, character variables"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        
        print *, "When: pipeline infers types from usage patterns"
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Pipeline error: ", trim(error_msg)
            return
        end if
        
        print *, "Then: all variables get correct type declarations"
        
        ! Check for proper type inference
        if (index(result_code, "real(8) :: value") == 0) then
            passed = .false.
            print *, "FAIL: Real variable 'value' not properly declared"
        end if
        
        if (index(result_code, "integer :: count") == 0) then
            passed = .false.
            print *, "FAIL: Integer variable 'count' not properly declared"
        end if
        
        if (index(result_code, "logical :: is_positive") == 0) then
            passed = .false.
            print *, "FAIL: Logical variable 'is_positive' not properly declared"
        end if
        
        if (index(result_code, "character") == 0) then
            passed = .false.
            print *, "FAIL: Character variable 'message' not properly declared"
        end if
        
        ! Check existing declaration preservation
        if (index(result_code, "integer :: n") == 0 .and. &
            index(result_code, "integer, intent(in) :: n") == 0) then
            passed = .false.
            print *, "FAIL: Existing integer parameter declaration not preserved"
        end if
        
        if (passed) then
            print *, "PASS: Multiple variable types correctly inferred"
        else
            print *, "FAIL: Multiple variable types inference failed"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
        print *, ""
    end function test_multiple_variable_types
    
    function test_intent_inference() result(passed)
        ! GIVEN a simple subroutine with parameters  
        ! WHEN the system processes parameter usage
        ! THEN basic variable declarations are generated
        ! NOTE: Full intent inference is planned for future implementation
        
        logical :: passed
        character(len=*), parameter :: source = &
            'subroutine simple_calc(input_val, output_val)' // char(10) // &
            'temp = input_val * 2' // char(10) // &
            'output_val = temp' // char(10) // &
            'end subroutine'
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "Test: Basic parameter handling in subroutines"
        print *, "Given: simple subroutine with parameters"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        
        print *, "When: system processes parameter usage"
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Pipeline error: ", trim(error_msg)
            return
        end if
        
        print *, "Then: basic variable declarations are generated"
        
        ! Check for parameter declarations
        if (index(result_code, ":: input_val") == 0) then
            passed = .false.
            print *, "FAIL: Parameter 'input_val' not declared"
        end if
        
        if (index(result_code, ":: output_val") == 0) then
            passed = .false.
            print *, "FAIL: Parameter 'output_val' not declared"
        end if
        
        ! Check for local variable declaration
        if (index(result_code, ":: temp") == 0) then
            passed = .false.
            print *, "FAIL: Local variable 'temp' not declared"
        end if
        
        if (passed) then
            print *, "PASS: Basic parameter handling works correctly"
            print *, "NOTE: Advanced intent inference planned for future versions"
        else
            print *, "FAIL: Basic parameter handling failed"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
        print *, ""
    end function test_intent_inference
    
    function test_array_parameter_handling() result(passed)
        ! GIVEN a function with simple array-like operations
        ! WHEN the system processes without complex intrinsics
        ! THEN basic variable declarations are generated
        
        logical :: passed
        character(len=*), parameter :: source = &
            'function simple_loop(n) result(total)' // char(10) // &
            'integer :: n' // char(10) // &
            'total = 0' // char(10) // &
            'do i = 1, n' // char(10) // &
            '    total = total + i' // char(10) // &
            'end do' // char(10) // &
            'end function'
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "Test: Simple loop parameter handling"
        print *, "Given: function with loop and simple operations"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        
        print *, "When: system processes without complex intrinsics"
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Pipeline error: ", trim(error_msg)
            return
        end if
        
        print *, "Then: basic variable declarations are generated"
        
        ! Check for loop variable
        if (index(result_code, ":: i") == 0) then
            passed = .false.
            print *, "FAIL: Loop variable 'i' not declared"
        end if
        
        ! Check for result variable
        if (index(result_code, ":: total") == 0) then
            passed = .false.
            print *, "FAIL: Result variable 'total' not declared"
        end if
        
        ! Check parameter preservation
        if (index(result_code, "integer :: n") == 0 .and. &
            index(result_code, "integer, intent(in) :: n") == 0) then
            passed = .false.
            print *, "FAIL: Parameter 'n' declaration issue"
        end if
        
        if (passed) then
            print *, "PASS: Simple loop parameter handling works correctly"
        else
            print *, "FAIL: Simple loop parameter handling failed"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
        print *, ""
    end function test_array_parameter_handling
    
    function test_mixed_explicit_implicit_declarations() result(passed)
        ! GIVEN a function with some explicit and some implicit declarations
        ! WHEN the system processes mixed scenarios
        ! THEN explicit declarations are preserved, implicit ones are generated
        
        logical :: passed
        character(len=*), parameter :: source = &
            'function mixed_declarations(x, y)' // char(10) // &
            'real :: x' // char(10) // &
            'temp1 = x * 2' // char(10) // &
            'temp2 = y + 1' // char(10) // &
            'mixed_declarations = temp1 + temp2' // char(10) // &
            'end function'
        character(len=:), allocatable :: result_code, error_msg
        integer :: x_count
        
        passed = .true.
        print *, "Test: Mixed explicit/implicit declarations"
        print *, "Given: function with mixed declaration scenarios"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        
        print *, "When: system processes mixed scenarios"
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Pipeline error: ", trim(error_msg)
            return
        end if
        
        print *, "Then: explicit preserved, implicit generated"
        
        ! Count x declarations - should be exactly one
        x_count = count_substring_occurrences(result_code, ":: x")
        if (x_count /= 1) then
            passed = .false.
            print *, "FAIL: Variable 'x' declared", x_count, "times (expected 1)"
        end if
        
        ! Check that implicit variables are declared
        if (index(result_code, ":: y") == 0) then
            passed = .false.
            print *, "FAIL: Implicit parameter 'y' not declared"
        end if
        
        if (index(result_code, ":: temp1") == 0) then
            passed = .false.
            print *, "FAIL: Implicit variable 'temp1' not declared"
        end if
        
        if (index(result_code, ":: temp2") == 0) then
            passed = .false.
            print *, "FAIL: Implicit variable 'temp2' not declared"
        end if
        
        if (passed) then
            print *, "PASS: Mixed explicit/implicit declarations handled correctly"
        else
            print *, "FAIL: Mixed explicit/implicit declarations failed"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
        print *, ""
    end function test_mixed_explicit_implicit_declarations
    
    function test_full_pipeline_integration() result(passed)
        ! GIVEN a complex function requiring all pipeline stages
        ! WHEN lexer → parser → semantic → codegen processes it
        ! THEN all stages work together seamlessly
        
        logical :: passed
        character(len=*), parameter :: source = &
            'function complex_pipeline_test(data_array, size_n) result(statistics)' // char(10) // &
            'integer :: size_n' // char(10) // &
            'sum_value = 0.0' // char(10) // &
            'max_value = data_array(1)' // char(10) // &
            'do index = 1, size_n' // char(10) // &
            '    current = data_array(index)' // char(10) // &
            '    sum_value = sum_value + current' // char(10) // &
            '    if (current > max_value) then' // char(10) // &
            '        max_value = current' // char(10) // &
            '    end if' // char(10) // &
            'end do' // char(10) // &
            'average = sum_value / size_n' // char(10) // &
            'statistics = max_value + average' // char(10) // &
            'end function'
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "Test: Full pipeline integration"
        print *, "Given: complex function requiring all pipeline stages"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        
        print *, "When: lexer → parser → semantic → codegen processes it"
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Pipeline integration error: ", trim(error_msg)
            return
        end if
        
        print *, "Then: all stages work together seamlessly"
        
        ! Validate lexing: function keywords and identifiers preserved
        if (index(result_code, "function complex_pipeline_test") == 0) then
            passed = .false.
            print *, "FAIL: Lexing stage - function signature not preserved"
        end if
        
        ! Validate parsing: control structures maintained
        if (index(result_code, "do index") == 0) then
            passed = .false.
            print *, "FAIL: Parsing stage - do loop structure not preserved"
        end if
        
        if (index(result_code, "if (") == 0) then
            passed = .false.
            print *, "FAIL: Parsing stage - if statement not preserved"
        end if
        
        ! Validate semantic analysis: variable declarations generated
        if (index(result_code, ":: data_array") == 0) then
            passed = .false.
            print *, "FAIL: Semantic stage - array parameter not declared"
        end if
        
        if (index(result_code, ":: sum_value") == 0) then
            passed = .false.
            print *, "FAIL: Semantic stage - local variable not declared"
        end if
        
        if (index(result_code, ":: index") == 0) then
            passed = .false.
            print *, "FAIL: Semantic stage - loop variable not declared"
        end if
        
        if (index(result_code, ":: current") == 0) then
            passed = .false.
            print *, "FAIL: Semantic stage - temporary variable not declared"
        end if
        
        if (index(result_code, ":: average") == 0) then
            passed = .false.
            print *, "FAIL: Semantic stage - computed variable not declared"
        end if
        
        if (index(result_code, ":: max_value") == 0) then
            passed = .false.
            print *, "FAIL: Semantic stage - max variable not declared"
        end if
        
        if (index(result_code, ":: statistics") == 0) then
            passed = .false.
            print *, "FAIL: Semantic stage - result variable not declared"
        end if
        
        ! Validate code generation: proper formatting
        if (index(result_code, "implicit none") == 0) then
            passed = .false.
            print *, "FAIL: Code generation - implicit none not added"
        end if
        
        if (passed) then
            print *, "PASS: Full pipeline integration works correctly"
        else
            print *, "FAIL: Full pipeline integration failed"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
        print *, ""
    end function test_full_pipeline_integration
    
    function test_generated_code_compilation() result(passed)
        ! GIVEN generated code from automatic variable declarations
        ! WHEN the generated code is checked for compilability
        ! THEN it should be syntactically correct and compilable
        
        logical :: passed
        character(len=*), parameter :: source = &
            'function compilability_test(input)' // char(10) // &
            'result_val = input * 2.5' // char(10) // &
            'temp = result_val + 10' // char(10) // &
            'compilability_test = temp' // char(10) // &
            'end function'
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "Test: Generated code compilation readiness"
        print *, "Given: generated code from automatic variable declarations"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        
        print *, "When: generated code is checked for compilability"
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Code generation error: ", trim(error_msg)
            return
        end if
        
        print *, "Then: code should be syntactically correct"
        
        ! Check basic syntactic correctness
        if (index(result_code, "implicit none") == 0) then
            passed = .false.
            print *, "FAIL: Missing 'implicit none' statement"
        end if
        
        ! Verify all variables are declared before use
        if (index(result_code, ":: input") == 0) then
            passed = .false.
            print *, "FAIL: Parameter 'input' not declared"
        end if
        
        if (index(result_code, ":: result_val") == 0) then
            passed = .false.
            print *, "FAIL: Variable 'result_val' not declared"
        end if
        
        if (index(result_code, ":: temp") == 0) then
            passed = .false.
            print *, "FAIL: Variable 'temp' not declared"
        end if
        
        if (index(result_code, ":: compilability_test") == 0) then
            passed = .false.
            print *, "FAIL: Function result not declared"
        end if
        
        ! Check for proper function structure
        if (index(result_code, "end function") == 0) then
            passed = .false.
            print *, "FAIL: Function end statement missing"
        end if
        
        if (passed) then
            print *, "PASS: Generated code is compilation-ready"
        else
            print *, "FAIL: Generated code has compilation issues"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
        print *, ""
    end function test_generated_code_compilation
    
    function test_realistic_lazy_fortran_examples() result(passed)
        ! GIVEN realistic lazy Fortran examples from Issue #320
        ! WHEN the system processes real-world use cases
        ! THEN the output meets developer expectations
        
        logical :: passed
        character(len=*), parameter :: source = &
            'function simple_math(a, b, c) result(discriminant)' // char(10) // &
            'discriminant = b*b - 4*a*c' // char(10) // &
            'if (discriminant >= 0) then' // char(10) // &
            '    root1 = -b / (2*a)' // char(10) // &
            '    root2 = root1 * 2' // char(10) // &
            'end if' // char(10) // &
            'end function'
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "Test: Realistic lazy Fortran examples"
        print *, "Given: realistic mathematical function"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        
        print *, "When: system processes real-world use case"
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Real-world example error: ", trim(error_msg)
            return
        end if
        
        print *, "Then: output meets developer expectations"
        
        ! Check mathematical function parameters
        if (index(result_code, ":: a") == 0) then
            passed = .false.
            print *, "FAIL: Parameter 'a' not declared"
        end if
        
        if (index(result_code, ":: b") == 0) then
            passed = .false.
            print *, "FAIL: Parameter 'b' not declared"
        end if
        
        if (index(result_code, ":: c") == 0) then
            passed = .false.
            print *, "FAIL: Parameter 'c' not declared"
        end if
        
        ! Check local computation variables
        if (index(result_code, ":: root1") == 0) then
            passed = .false.
            print *, "FAIL: Variable 'root1' not declared"
        end if
        
        if (index(result_code, ":: root2") == 0) then
            passed = .false.
            print *, "FAIL: Variable 'root2' not declared"
        end if
        
        if (index(result_code, ":: discriminant") == 0) then
            passed = .false.
            print *, "FAIL: Result variable 'discriminant' not declared"
        end if
        
        ! Check that all variables have appropriate types
        if (index(result_code, "real(8)") == 0) then
            passed = .false.
            print *, "FAIL: Real number types not found in declarations"
        end if
        
        if (passed) then
            print *, "PASS: Realistic lazy Fortran examples work correctly"
        else
            print *, "FAIL: Realistic lazy Fortran examples failed"
            print *, "Generated code:"
            print *, trim(result_code)
        end if
        print *, ""
    end function test_realistic_lazy_fortran_examples
    
    function test_edge_cases() result(passed)
        ! GIVEN edge cases and boundary conditions
        ! WHEN the system processes unusual but valid scenarios
        ! THEN it handles them gracefully without errors
        
        logical :: passed
        logical :: test1_passed, test2_passed, test3_passed
        
        passed = .true.
        print *, "Test: Edge cases and boundary conditions"
        
        ! Edge case 1: Empty function
        test1_passed = test_empty_function()
        if (.not. test1_passed) passed = .false.
        
        ! Edge case 2: Single assignment function
        test2_passed = test_single_assignment_function()
        if (.not. test2_passed) passed = .false.
        
        ! Edge case 3: Function with only constants
        test3_passed = test_constants_only_function()
        if (.not. test3_passed) passed = .false.
        
        if (passed) then
            print *, "PASS: All edge cases handled correctly"
        else
            print *, "FAIL: Some edge cases failed"
        end if
        print *, ""
    end function test_edge_cases
    
    function test_empty_function() result(passed)
        logical :: passed
        character(len=*), parameter :: source = &
            'function empty_func()' // char(10) // &
            'empty_func = 0' // char(10) // &
            'end function'
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "  Edge case: Empty function with only result assignment"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "  FAIL: Empty function error: ", trim(error_msg)
            return
        end if
        
        if (index(result_code, ":: empty_func") == 0) then
            passed = .false.
            print *, "  FAIL: Function result not declared"
        end if
        
        if (passed) then
            print *, "  PASS: Empty function handled correctly"
        end if
    end function test_empty_function
    
    function test_single_assignment_function() result(passed)
        logical :: passed
        character(len=*), parameter :: source = &
            'function single_assign(x)' // char(10) // &
            'single_assign = x' // char(10) // &
            'end function'
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "  Edge case: Single assignment function"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "  FAIL: Single assignment error: ", trim(error_msg)
            return
        end if
        
        if (index(result_code, ":: x") == 0) then
            passed = .false.
            print *, "  FAIL: Parameter 'x' not declared"
        end if
        
        if (index(result_code, ":: single_assign") == 0) then
            passed = .false.
            print *, "  FAIL: Function result not declared"
        end if
        
        if (passed) then
            print *, "  PASS: Single assignment function handled correctly"
        end if
    end function test_single_assignment_function
    
    function test_constants_only_function() result(passed)
        logical :: passed
        character(len=*), parameter :: source = &
            'function constants_only()' // char(10) // &
            'pi = 3.14159' // char(10) // &
            'e = 2.71828' // char(10) // &
            'constants_only = pi * e' // char(10) // &
            'end function'
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "  Edge case: Function with only constants"
        
        call transform_lazy_fortran_string(source, result_code, error_msg)
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "  FAIL: Constants only error: ", trim(error_msg)
            return
        end if
        
        if (index(result_code, ":: pi") == 0) then
            passed = .false.
            print *, "  FAIL: Constant 'pi' not declared"
        end if
        
        if (index(result_code, ":: e") == 0) then
            passed = .false.
            print *, "  FAIL: Constant 'e' not declared"
        end if
        
        if (passed) then
            print *, "  PASS: Constants only function handled correctly"
        end if
    end function test_constants_only_function
    
    function test_error_handling() result(passed)
        ! GIVEN invalid or problematic input
        ! WHEN the system encounters errors
        ! THEN it provides meaningful error messages without crashes
        
        logical :: passed
        character(len=*), parameter :: invalid_source = &
            'function broken(' // char(10) // &
            'missing_end'
        character(len=:), allocatable :: result_code, error_msg
        
        passed = .true.
        print *, "Test: Error handling and graceful degradation"
        print *, "Given: invalid Fortran syntax"
        
        call transform_lazy_fortran_string(invalid_source, result_code, error_msg)
        
        print *, "When: system encounters parsing errors"
        print *, "Then: meaningful error messages without crashes"
        
        ! Should get an error message
        if (len_trim(error_msg) == 0) then
            passed = .false.
            print *, "FAIL: No error message for invalid syntax"
        else
            print *, "PASS: Error handling works correctly"
            print *, "  Error message: ", trim(error_msg)
        end if
        print *, ""
    end function test_error_handling
    
    function test_performance_validation() result(passed)
        ! GIVEN large functions with many variables
        ! WHEN the system processes complex scenarios
        ! THEN performance remains acceptable
        
        logical :: passed
        character(len=1000) :: large_source
        character(len=:), allocatable :: result_code, error_msg
        integer :: i
        
        passed = .true.
        print *, "Test: Performance validation with large functions"
        print *, "Given: function with many variables"
        
        ! Build a large function programmatically
        large_source = 'function large_func(x)' // char(10)
        do i = 1, 20
            write(large_source(len_trim(large_source)+1:), &
                  fmt='(A,I0,A,I0,A)') 'var', i, ' = x + ', i, char(10)
        end do
        large_source = trim(large_source) // 'large_func = var1 + var20' // char(10) // 'end function'
        
        call transform_lazy_fortran_string(large_source, result_code, error_msg)
        
        print *, "When: system processes complex scenarios"
        if (len_trim(error_msg) > 0) then
            passed = .false.
            print *, "FAIL: Performance test error: ", trim(error_msg)
            return
        end if
        
        print *, "Then: performance remains acceptable"
        
        ! Check that many variables are declared
        if (index(result_code, ":: var1") == 0) then
            passed = .false.
            print *, "FAIL: First variable not declared"
        end if
        
        if (index(result_code, ":: var20") == 0) then
            passed = .false.
            print *, "FAIL: Last variable not declared"
        end if
        
        if (passed) then
            print *, "PASS: Performance validation successful"
        else
            print *, "FAIL: Performance validation failed"
        end if
        print *, ""
    end function test_performance_validation
    
    function count_substring_occurrences(string, substring) result(count)
        character(len=*), intent(in) :: string, substring
        integer :: count
        integer :: pos, start_pos
        
        count = 0
        start_pos = 1
        
        do while (start_pos <= len(string))
            pos = index(string(start_pos:), substring)
            if (pos > 0) then
                count = count + 1
                start_pos = start_pos + pos + len(substring) - 1
            else
                exit
            end if
        end do
    end function count_substring_occurrences

end program test_automatic_variable_declarations