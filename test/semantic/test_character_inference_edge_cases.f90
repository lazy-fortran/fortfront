program test_character_inference_edge_cases
    !! Edge case tests for character type inference patterns
    !! These tests focus on specific failure modes identified in Issue #329
    
    use fortfront, only: transform_lazy_fortran_string
    implicit none
    
    logical :: all_passed = .true.
    
    print *, "=== Character Type Inference Edge Cases (Issue #329) ==="
    print *
    
    ! Edge Case 1: Binary operator context hints
    if (.not. test_binary_operator_context_hints()) all_passed = .false.
    
    ! Edge Case 2: Backward propagation from operations
    if (.not. test_backward_propagation_from_operations()) all_passed = .false.
    
    ! Edge Case 3: Character operation constraint collection
    if (.not. test_character_operation_constraints()) all_passed = .false.
    
    ! Edge Case 4: Type defaulting prevention
    if (.not. test_type_defaulting_prevention()) all_passed = .false.
    
    print *
    if (all_passed) then
        print *, "All character inference edge case tests PASSED"
        stop 0
    else
        print *, "Some character inference edge case tests FAILED"
        stop 1
    end if
    
contains

    logical function test_binary_operator_context_hints()
        !! Given: Variables used only in character binary operations (//)
        !! When: The semantic analyzer processes the operations
        !! Then: Variables should be inferred as character from operation context
        
        character(len=:), allocatable :: source, output, error_msg
        
        test_binary_operator_context_hints = .true.
        print *, "Edge Case 1: Binary operator context hints..."
        
        ! GIVEN: Untyped variable in string concatenation context only
        source = 'greeting = hello // world'
        
        ! WHEN: Process with binary operator context analysis
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR during transformation: ", trim(error_msg)
            test_binary_operator_context_hints = .false.
            return
        end if
        
        ! THEN: Both operands should be inferred as character from // context
        if (index(output, "character") > 0 .and. &
            index(output, "hello") > 0 .and. &
            index(output, "world") > 0) then
            ! Must not default to real
            if (index(output, "real") == 0) then
                print *, "  PASS: Binary operator provided correct character context hints"
            else
                print *, "  FAIL: Variables defaulted to real despite character operation context"
                print *, "        Found output:"
                print *, "        ", trim(output)
                test_binary_operator_context_hints = .false.
            end if
        else
            print *, "  FAIL: Binary operator context hints not applied"
            print *, "        Expected: character types for hello and world"
            print *, "        Found output:"
            print *, "        ", trim(output)
            test_binary_operator_context_hints = .false.
        end if
        
    end function test_binary_operator_context_hints

    logical function test_backward_propagation_from_operations()
        !! Given: Function with parameter used in character operation
        !! When: Backward type propagation analyzes the usage
        !! Then: Parameter type should be inferred from operation requirements
        
        character(len=:), allocatable :: source, output, error_msg
        
        test_backward_propagation_from_operations = .true.
        print *, "Edge Case 2: Backward propagation from operations..."
        
        ! GIVEN: Function parameter used in character operation (exact issue case)
        source = 'function greet(name)' // new_line('a') // &
                 '  greet = "Hello, " // name // "!"' // new_line('a') // &
                 'end function'
        
        ! WHEN: Backward propagation from // operations to parameter
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR during transformation: ", trim(error_msg)
            test_backward_propagation_from_operations = .false.
            return
        end if
        
        ! THEN: Parameter should be inferred as character from usage context
        if (index(output, "character") > 0 .and. &
            (index(output, "name") > 0)) then
            print *, "  PASS: Backward propagation correctly inferred parameter type"
        else
            print *, "  FAIL: Backward propagation from operations not working"
            print *, "        Expected: character type for parameter 'name'"
            print *, "        Found output:"
            print *, "        ", trim(output)
            test_backward_propagation_from_operations = .false.
        end if
        
    end function test_backward_propagation_from_operations

    logical function test_character_operation_constraints()
        !! Given: Multiple character operations creating type constraints
        !! When: Constraint collection and solving is performed
        !! Then: All constrained variables should resolve to character type
        
        character(len=:), allocatable :: source, output, error_msg
        
        test_character_operation_constraints = .true.
        print *, "Edge Case 3: Character operation constraints..."
        
        ! GIVEN: Multiple operations creating character type constraints
        source = 'result = prefix // middle // suffix'
        
        ! WHEN: Constraint collection from multiple concatenations
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR during transformation: ", trim(error_msg)
            test_character_operation_constraints = .false.
            return
        end if
        
        ! THEN: All variables should be constrained to character type
        if (index(output, "character") > 0) then
            if (index(output, "result") > 0 .and. &
                index(output, "prefix") > 0 .and. &
                index(output, "middle") > 0 .and. &
                index(output, "suffix") > 0 .and. &
                index(output, "real") == 0) then
                print *, "  PASS: Character operation constraints correctly applied"
            else
                print *, "  FAIL: Some variables not properly constrained"
                test_character_operation_constraints = .false.
            end if
        else
            print *, "  FAIL: Character operation constraints not collected"
            print *, "        Expected: character constraints for all variables"
            print *, "        Found output:"
            print *, "        ", trim(output)
            test_character_operation_constraints = .false.
        end if
        
    end function test_character_operation_constraints

    logical function test_type_defaulting_prevention()
        !! Given: Variables that should be character but might default to real
        !! When: Type defaulting logic runs after constraint solving
        !! Then: Character constraints should prevent real(8) defaulting
        
        character(len=:), allocatable :: source, output, error_msg
        
        test_type_defaulting_prevention = .true.
        print *, "Edge Case 4: Type defaulting prevention..."
        
        ! GIVEN: Function parameter that historically defaults to real(8)
        source = 'function process_text(input)' // new_line('a') // &
                 '  process_text = input' // new_line('a') // &  
                 'end function' // new_line('a') // &
                 'result = process_text("hello")'
        
        ! WHEN: Processing with character literal providing type hint
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR during transformation: ", trim(error_msg)
            test_type_defaulting_prevention = .false.
            return
        end if
        
        ! THEN: Parameter should not default to real despite no direct concatenation
        if (index(output, "character") > 0 .and. &
            index(output, "input") > 0) then
            ! Specifically check that real(8) default was prevented
            if (index(output, "real") == 0) then
                print *, "  PASS: Type defaulting to real prevented by character context"
            else
                print *, "  FAIL: Parameter still defaulted to real despite character usage"
                test_type_defaulting_prevention = .false.
            end if
        else
            print *, "  FAIL: Type defaulting prevention not working"
            print *, "        Expected: character type for parameter 'input'"
            print *, "        Found output:"
            print *, "        ", trim(output)
            test_type_defaulting_prevention = .false.
        end if
        
    end function test_type_defaulting_prevention

end program test_character_inference_edge_cases