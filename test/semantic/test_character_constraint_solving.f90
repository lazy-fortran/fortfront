program test_character_constraint_solving
    !! Tests for character type constraint collection and solving mechanisms
    !! Focuses on the constraint-based approach described in DESIGN.md Phase 2
    
    use fortfront, only: transform_lazy_fortran_string  
    implicit none
    
    logical :: all_passed = .true.
    
    print *, "=== Character Type Constraint Solving Tests (Issue #329) ==="
    print *
    
    ! Test 1: Constraint collection from character operations
    if (.not. test_constraint_collection_from_operations()) all_passed = .false.
    
    ! Test 2: Bidirectional unification in character contexts
    if (.not. test_bidirectional_character_unification()) all_passed = .false.
    
    ! Test 3: Constraint propagation through expression trees
    if (.not. test_constraint_propagation_trees()) all_passed = .false.
    
    ! Test 4: Character operation registry and solving
    if (.not. test_character_operation_registry()) all_passed = .false.
    
    ! Test 5: Deferred defaulting after constraint solving
    if (.not. test_deferred_defaulting_mechanism()) all_passed = .false.
    
    print *
    if (all_passed) then
        print *, "All character constraint solving tests PASSED"
        stop 0
    else
        print *, "Some character constraint solving tests FAILED"  
        stop 1
    end if
    
contains

    logical function test_constraint_collection_from_operations()
        !! Given: Expression with character operations that should generate constraints
        !! When: Constraint collection phase analyzes the operations
        !! Then: Appropriate MUST_BE_CHARACTER constraints should be collected
        
        character(len=:), allocatable :: source, output, error_msg
        
        test_constraint_collection_from_operations = .true.
        print *, "Test 1: Constraint collection from character operations..."
        
        ! GIVEN: Expression with multiple character operations
        source = 'final = first // second // third'
        
        ! WHEN: Constraint collection analyzes string concatenations
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR during transformation: ", trim(error_msg)
            test_constraint_collection_from_operations = .false.
            return
        end if
        
        ! THEN: All variables should be constrained to character type
        if (index(output, "character") > 0) then
            ! Check that all variables are present and typed as character
            if (index(output, "final") > 0 .and. &
                index(output, "first") > 0 .and. &
                index(output, "second") > 0 .and. &
                index(output, "third") > 0) then
                
                ! Ensure none defaulted to real
                if (index(output, "real") == 0) then
                    print *, "  PASS: Constraints collected from all character operations"
                else
                    print *, "  FAIL: Some variables defaulted to real despite character constraints"
                    test_constraint_collection_from_operations = .false.
                end if
            else
                print *, "  FAIL: Missing variables in constraint collection"
                test_constraint_collection_from_operations = .false.
            end if
        else
            print *, "  FAIL: Character constraints not collected from operations"
            print *, "        Expected: character type constraints for all variables"
            print *, "        Found output:"
            print *, "        ", trim(output)
            test_constraint_collection_from_operations = .false.
        end if
        
    end function test_constraint_collection_from_operations

    logical function test_bidirectional_character_unification()
        !! Given: Character operations with mixed typed and untyped operands
        !! When: Bidirectional unification processes the constraints
        !! Then: Untyped operands should be unified with character type
        
        character(len=:), allocatable :: source, output, error_msg
        
        test_bidirectional_character_unification = .true.
        print *, "Test 2: Bidirectional character unification..."
        
        ! GIVEN: Mix of character literal and untyped variable
        source = 'message = "Hello" // unknown_var'
        
        ! WHEN: Bidirectional unification resolves types
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR during transformation: ", trim(error_msg)
            test_bidirectional_character_unification = .false.
            return
        end if
        
        ! THEN: Both operands should unify to character type
        if (index(output, "character") > 0) then
            if (index(output, "message") > 0 .and. &
                index(output, "unknown_var") > 0 .and. &
                index(output, "real") == 0) then
                print *, "  PASS: Bidirectional unification resolved character types"
            else
                print *, "  FAIL: Unification did not resolve all variables to character"
                test_bidirectional_character_unification = .false.
            end if
        else
            print *, "  FAIL: Bidirectional character unification failed"
            print *, "        Expected: character types for message and unknown_var"
            print *, "        Found output:"
            print *, "        ", trim(output)
            test_bidirectional_character_unification = .false.
        end if
        
    end function test_bidirectional_character_unification

    logical function test_constraint_propagation_trees()
        !! Given: Nested character expressions forming constraint trees
        !! When: Constraint propagation traverses the expression trees
        !! Then: Character type constraints should propagate through all levels
        
        character(len=:), allocatable :: source, output, error_msg
        
        test_constraint_propagation_trees = .true.
        print *, "Test 3: Constraint propagation through expression trees..."
        
        ! GIVEN: Nested character expressions
        source = 'full = (prefix // middle) // (suffix // ending)'
        
        ! WHEN: Constraint propagation through nested expressions
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR during transformation: ", trim(error_msg)
            test_constraint_propagation_trees = .false.
            return
        end if
        
        ! THEN: All variables at all levels should be character
        if (index(output, "character") > 0) then
            if (index(output, "full") > 0 .and. &
                index(output, "prefix") > 0 .and. &
                index(output, "middle") > 0 .and. &
                index(output, "suffix") > 0 .and. &
                index(output, "ending") > 0 .and. &
                index(output, "real") == 0) then
                print *, "  PASS: Constraints propagated through entire expression tree"
            else
                print *, "  FAIL: Constraint propagation incomplete in expression tree"
                test_constraint_propagation_trees = .false.
            end if
        else
            print *, "  FAIL: Constraint propagation through expression trees failed"
            print *, "        Expected: character types for all nested variables"
            print *, "        Found output:"
            print *, "        ", trim(output)
            test_constraint_propagation_trees = .false.
        end if
        
    end function test_constraint_propagation_trees

    logical function test_character_operation_registry()
        !! Given: Various character operations that should be registered
        !! When: Character operation registry processes the operations
        !! Then: All character operations should contribute to constraint solving
        
        character(len=:), allocatable :: source, output, error_msg
        
        test_character_operation_registry = .true.
        print *, "Test 4: Character operation registry and solving..."
        
        ! GIVEN: Multiple types of character operations
        source = 'function format_data(header, data)' // new_line('a') // &
                 '  temp = header // ": "' // new_line('a') // &
                 '  format_data = temp // data' // new_line('a') // &
                 'end function'
        
        ! WHEN: Character operation registry collects all operations
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR during transformation: ", trim(error_msg)
            test_character_operation_registry = .false.
            return
        end if
        
        ! THEN: All parameters and variables should be registered as character
        if (index(output, "character") > 0) then
            if (index(output, "header") > 0 .and. &
                index(output, "data") > 0 .and. &
                index(output, "temp") > 0) then
                
                ! Should not have any real defaults
                if (index(output, "real") == 0) then
                    print *, "  PASS: Character operation registry solved all constraints"
                else
                    print *, "  FAIL: Registry did not prevent real type defaults"
                    test_character_operation_registry = .false.
                end if
            else
                print *, "  FAIL: Registry missed some character operations"
                test_character_operation_registry = .false.
            end if
        else
            print *, "  FAIL: Character operation registry not working"
            print *, "        Expected: character types for header, data, temp"
            print *, "        Found output:"
            print *, "        ", trim(output)
            test_character_operation_registry = .false.
        end if
        
    end function test_character_operation_registry

    logical function test_deferred_defaulting_mechanism()
        !! Given: Variables that would normally default to real(8)
        !! When: Deferred defaulting waits for constraint solving completion
        !! Then: Character constraints should prevent real(8) defaults
        
        character(len=:), allocatable :: source, output, error_msg
        
        test_deferred_defaulting_mechanism = .true.
        print *, "Test 5: Deferred defaulting after constraint solving..."
        
        ! GIVEN: Function parameter that would normally default early to real(8)
        source = 'function append_suffix(base)' // new_line('a') // &
                 '  append_suffix = base // ".txt"' // new_line('a') // &
                 'end function'
        
        ! WHEN: Deferred defaulting allows constraint solving first
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  ERROR during transformation: ", trim(error_msg)
            test_deferred_defaulting_mechanism = .false.
            return
        end if
        
        ! THEN: Parameter should be character, not real(8) default
        if (index(output, "character") > 0 .and. &
            index(output, "base") > 0) then
            
            ! Explicitly check that real(8) default was deferred and prevented
            if (index(output, "real") == 0) then
                print *, "  PASS: Deferred defaulting allowed character constraint to apply"
            else
                print *, "  FAIL: Early defaulting to real occurred despite character usage"
                print *, "        Found output:"
                print *, "        ", trim(output)
                test_deferred_defaulting_mechanism = .false.
            end if
        else
            print *, "  FAIL: Deferred defaulting mechanism not working"
            print *, "        Expected: character type for parameter 'base'"
            print *, "        Found output:"
            print *, "        ", trim(output)
            test_deferred_defaulting_mechanism = .false.
        end if
        
    end function test_deferred_defaulting_mechanism

end program test_character_constraint_solving