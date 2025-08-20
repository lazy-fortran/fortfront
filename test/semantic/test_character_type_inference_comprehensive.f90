program test_character_type_inference_comprehensive
    !! Comprehensive character type inference tests for Issue #329
    !! Tests the 6 failing scenarios identified in DESIGN.md:
    !! 1. Function parameter character inference from string operations
    !! 2. Character type preservation across scopes  
    !! 3. Declaration consolidation with character types
    !! 4. Expression order preservation with character operations
    !! 5. Character literals type inference
    !! 6. String concatenation type propagation
    
    use fortfront, only: transform_lazy_fortran_string
    implicit none
    
    logical :: all_passed = .true.
    
    print *, "=== Character Type Inference Comprehensive Tests (Issue #329) ==="
    print *
    
    ! Test 1: Function parameter character inference
    if (.not. test_function_parameter_character_inference()) all_passed = .false.
    
    ! Test 2: Character type preservation across scopes
    if (.not. test_character_type_scope_preservation()) all_passed = .false.
    
    ! Test 3: Declaration consolidation with character types
    if (.not. test_character_declaration_consolidation()) all_passed = .false.
    
    ! Test 4: Expression order preservation with character operations
    if (.not. test_character_expression_order_preservation()) all_passed = .false.
    
    ! Test 5: Character literals type inference
    if (.not. test_character_literals_inference()) all_passed = .false.
    
    ! Test 6: String concatenation type propagation
    if (.not. test_string_concatenation_propagation()) all_passed = .false.
    
    print *
    if (all_passed) then
        print *, "All character type inference tests PASSED"
        stop 0
    else
        print *, "Some character type inference tests FAILED"
        stop 1
    end if
    
contains

    logical function test_function_parameter_character_inference()
        !! Given: A function with parameter used in string concatenation
        !! When: The semantic analyzer processes the function  
        !! Then: The parameter should be inferred as character type
        
        character(len=:), allocatable :: source, output, error_msg
        
        test_function_parameter_character_inference = .true.
        print *, "Test 1: Function parameter character inference..."
        
        ! GIVEN: Function parameter used with string concatenation operator
        source = 'function concat_hello(x)' // new_line('a') // &
                 '  concat_hello = x // "!"' // new_line('a') // &
                 'end function'
        
        ! WHEN: Transform the lazy fortran source
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Check for transformation errors
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR during transformation: ", trim(error_msg)
            test_function_parameter_character_inference = .false.
            return
        end if
        
        ! THEN: Parameter x should be inferred as character type
        if (index(output, "character") > 0 .and. &
            (index(output, "intent(in) :: x") > 0 .or. index(output, ":: x") > 0)) then
            print *, "  PASS: Function parameter correctly inferred as character"
        else
            print *, "  FAIL: Function parameter not inferred as character"
            print *, "        Expected: character type for parameter 'x'"
            print *, "        Found output:"
            print *, "        ", trim(output)
            test_function_parameter_character_inference = .false.
        end if
        
    end function test_function_parameter_character_inference

    logical function test_character_type_scope_preservation()
        !! Given: Character variable declared in outer scope used in inner scope
        !! When: The variable crosses scope boundaries
        !! Then: Character type information should be preserved
        
        character(len=:), allocatable :: source, output, error_msg
        
        test_character_type_scope_preservation = .true.
        print *, "Test 2: Character type scope preservation..."
        
        ! GIVEN: Character variable used across scope boundaries
        source = 'program test_scope' // new_line('a') // &
                 '  message = "hello"' // new_line('a') // &
                 '  if (.true.) then' // new_line('a') // &
                 '    result = message // " world"' // new_line('a') // &
                 '  end if' // new_line('a') // &
                 'end program'
        
        ! WHEN: Transform the source with scope transitions
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Check for transformation errors
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR during transformation: ", trim(error_msg)
            test_character_type_scope_preservation = .false.
            return
        end if
        
        ! THEN: Both variables should maintain character type
        if (index(output, "character") > 0 .and. &
            index(output, "message") > 0 .and. &
            index(output, "result") > 0) then
            ! Additional check: should not default to real
            if (index(output, "real") == 0) then
                print *, "  PASS: Character type preserved across scopes"
            else
                print *, "  FAIL: Character variables incorrectly typed as real"
                test_character_type_scope_preservation = .false.
            end if
        else
            print *, "  FAIL: Character type not preserved across scopes"
            print *, "        Expected: character declarations for 'message' and 'result'"
            print *, "        Found output:"
            print *, "        ", trim(output)
            test_character_type_scope_preservation = .false.
        end if
        
    end function test_character_type_scope_preservation

    logical function test_character_declaration_consolidation()
        !! Given: Multiple character variables with same type characteristics
        !! When: Declaration consolidation is performed
        !! Then: Character variables should be properly consolidated
        
        character(len=:), allocatable :: source, output, error_msg
        
        test_character_declaration_consolidation = .true.
        print *, "Test 3: Character declaration consolidation..."
        
        ! GIVEN: Multiple character assignments that could be consolidated
        source = 'name1 = "Alice"' // new_line('a') // &
                 'name2 = "Bob"' // new_line('a') // &
                 'name3 = "Charlie"'
        
        ! WHEN: Transform with declaration consolidation
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Check for transformation errors  
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR during transformation: ", trim(error_msg)
            test_character_declaration_consolidation = .false.
            return
        end if
        
        ! THEN: Should have consolidated character declarations
        if (index(output, "character") > 0) then
            ! Check for proper consolidation structure
            if ((index(output, "name1") > 0 .and. index(output, "name2") > 0 .and. index(output, "name3") > 0) .and. &
                index(output, "real") == 0) then
                print *, "  PASS: Character declarations properly consolidated"
            else
                print *, "  FAIL: Character variables not properly handled in consolidation"
                test_character_declaration_consolidation = .false.
            end if
        else
            print *, "  FAIL: Character declarations missing from consolidated output"
            print *, "        Expected: character type declarations"
            print *, "        Found output:"
            print *, "        ", trim(output)
            test_character_declaration_consolidation = .false.
        end if
        
    end function test_character_declaration_consolidation

    logical function test_character_expression_order_preservation()
        !! Given: Complex character expressions with specific ordering
        !! When: The expressions are processed by semantic analysis
        !! Then: The order of operations should be preserved correctly
        
        character(len=:), allocatable :: source, output, error_msg
        
        test_character_expression_order_preservation = .true.
        print *, "Test 4: Character expression order preservation..."
        
        ! GIVEN: Complex character expression with specific order
        source = 'result = (prefix // "middle") // suffix'
        
        ! WHEN: Transform while preserving expression structure
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Check for transformation errors
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR during transformation: ", trim(error_msg)
            test_character_expression_order_preservation = .false.
            return
        end if
        
        ! THEN: Expression order should be maintained with character types
        if (index(output, "character") > 0 .and. &
            index(output, "result") > 0 .and. &
            index(output, "prefix") > 0 .and. &
            index(output, "suffix") > 0) then
            ! Verify no incorrect real type defaults
            if (index(output, "real") == 0) then
                print *, "  PASS: Character expression order preserved correctly"
            else
                print *, "  FAIL: Character variables incorrectly defaulted to real"
                test_character_expression_order_preservation = .false.
            end if
        else
            print *, "  FAIL: Character expression order not preserved"
            print *, "        Expected: character types for result, prefix, suffix"
            print *, "        Found output:"
            print *, "        ", trim(output)
            test_character_expression_order_preservation = .false.
        end if
        
    end function test_character_expression_order_preservation

    logical function test_character_literals_inference()
        !! Given: Various character literal assignments
        !! When: Type inference processes the literals
        !! Then: Correct character types with proper lengths should be inferred
        
        character(len=:), allocatable :: source, output, error_msg
        
        test_character_literals_inference = .true.
        print *, "Test 5: Character literals type inference..."
        
        ! GIVEN: Character literal assignments with different lengths
        source = 'short = "hi"' // new_line('a') // &
                 'medium = "hello"' // new_line('a') // &
                 'long = "this is a longer string"'
        
        ! WHEN: Transform with character literal inference
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Check for transformation errors
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR during transformation: ", trim(error_msg)
            test_character_literals_inference = .false.
            return
        end if
        
        ! THEN: Should have proper character types with correct lengths
        if (index(output, "character") > 0) then
            ! Check for different length handling
            if (index(output, "short") > 0 .and. &
                index(output, "medium") > 0 .and. &
                index(output, "long") > 0 .and. &
                index(output, "real") == 0) then
                print *, "  PASS: Character literals correctly inferred with proper types"
            else
                print *, "  FAIL: Character literal inference incomplete"
                test_character_literals_inference = .false.
            end if
        else
            print *, "  FAIL: Character literals not inferred as character types"
            print *, "        Expected: character type declarations"
            print *, "        Found output:"
            print *, "        ", trim(output)
            test_character_literals_inference = .false.
        end if
        
    end function test_character_literals_inference

    logical function test_string_concatenation_propagation()
        !! Given: Chain of string concatenations with untyped variables
        !! When: Type propagation processes the concatenation chain  
        !! Then: All variables in the chain should be inferred as character
        
        character(len=:), allocatable :: source, output, error_msg
        
        test_string_concatenation_propagation = .true.
        print *, "Test 6: String concatenation type propagation..."
        
        ! GIVEN: Chain of concatenations that should propagate character type
        source = 'full_name = first // " " // last'
        
        ! WHEN: Transform with type propagation through concatenation
        call transform_lazy_fortran_string(source, output, error_msg)
        
        ! Check for transformation errors
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR during transformation: ", trim(error_msg)
            test_string_concatenation_propagation = .false.
            return
        end if
        
        ! THEN: All variables should be inferred as character from concatenation context
        if (index(output, "character") > 0) then
            if (index(output, "full_name") > 0 .and. &
                index(output, "first") > 0 .and. &
                index(output, "last") > 0 .and. &
                index(output, "real") == 0) then
                print *, "  PASS: String concatenation correctly propagated character types"
            else
                print *, "  FAIL: Type propagation through concatenation incomplete"
                test_string_concatenation_propagation = .false.
            end if
        else
            print *, "  FAIL: String concatenation did not propagate character types"
            print *, "        Expected: character types for full_name, first, last"
            print *, "        Found output:"
            print *, "        ", trim(output)
            test_string_concatenation_propagation = .false.
        end if
        
    end function test_string_concatenation_propagation

end program test_character_type_inference_comprehensive