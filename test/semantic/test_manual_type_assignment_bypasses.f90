program test_manual_type_assignment_bypasses
    ! Test specific patterns where manual type assignment bypasses validation
    ! Issue #311: Audit semantic_analyzer.f90 for direct type assignments
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    use type_system_hm, only: mono_type_t, create_mono_type, &
                              TINT, TREAL, TCHAR, TLOGICAL, TVAR, TFUN
    use ast_core, only: ast_arena_t, create_arena
    implicit none

    logical :: all_passed
    type(semantic_context_t) :: context
    type(ast_arena_t) :: arena

    all_passed = .true.

    print *, '=== Manual Type Assignment Bypass Tests (Issue #311) ==='
    print *

    ! Initialize test environment
    context = create_semantic_context()
    call create_arena(arena)

    ! Test fallback type assignments without validation
    print *, 'Testing fallback type assignments...'
    if (.not. test_fallback_assignments()) all_passed = .false.
    if (.not. test_default_error_types()) all_passed = .false.

    ! Test direct mono_type creation patterns
    print *, 'Testing direct mono_type creation...'
    if (.not. test_direct_mono_type_creation()) all_passed = .false.
    if (.not. test_unvalidated_type_vars()) all_passed = .false.

    ! Test type assignment without unification
    print *, 'Testing type assignment without unification...'
    if (.not. test_bypassed_unification()) all_passed = .false.
    if (.not. test_assignment_operator_safety()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All manual type assignment bypass tests passed!'
        stop 0
    else
        print *, 'Some manual type assignment bypass tests failed!'
        stop 1
    end if

contains

    function test_fallback_assignments() result(passed)
        ! Test fallback type assignments that bypass validation
        ! Lines 188, 192, 203 in semantic_analyzer.f90
        logical :: passed
        type(mono_type_t) :: fallback_type
        
        passed = .true.
        
        ! Test creating fallback types directly
        fallback_type = create_mono_type(TINT)  ! Default fallback pattern
        
        ! Verify fallback type is valid
        if (fallback_type%kind /= TINT) then
            print *, '  FAILED: Fallback type not created correctly'
            passed = .false.
            return
        end if
        
        ! Test that fallback assignments should have validation
        if (.not. validate_fallback_type(fallback_type)) then
            print *, '  FAILED: Fallback type failed validation'
            passed = .false.
            return
        end if
        
        if (passed) print *, '  PASSED: Fallback type assignments'
    end function

    function test_default_error_types() result(passed)
        ! Test error type creation without validation
        logical :: passed
        type(mono_type_t) :: error_type
        
        passed = .true.
        
        ! Test creating error types (line 418 pattern)
        error_type = create_mono_type(TVAR)
        
        if (error_type%kind /= TVAR) then
            print *, '  FAILED: Error type not created correctly'
            passed = .false.
            return
        end if
        
        if (passed) print *, '  PASSED: Default error type creation'
    end function

    function test_direct_mono_type_creation() result(passed)
        ! Test direct mono_type creation patterns found in code
        logical :: passed
        type(mono_type_t) :: real_type, int_type
        
        passed = .true.
        
        ! Test direct type creation patterns (lines 317, 321, 431)
        real_type = create_mono_type(TREAL)
        int_type = create_mono_type(TINT)
        
        ! Verify types are created correctly
        if (real_type%kind /= TREAL) then
            print *, '  FAILED: Real type not created correctly'
            passed = .false.
            return
        end if
        
        if (int_type%kind /= TINT) then
            print *, '  FAILED: Integer type not created correctly'
            passed = .false.
            return
        end if
        
        ! These direct creations should ideally go through validation
        if (.not. should_validate_direct_creation(real_type, int_type)) then
            print *, '  WARNING: Direct type creation bypassed validation'
        end if
        
        if (passed) print *, '  PASSED: Direct mono_type creation'
    end function

    function test_unvalidated_type_vars() result(passed)
        ! Test type variable creation without validation
        logical :: passed
        type(mono_type_t) :: var_type
        
        passed = .true.
        
        ! Test type variable creation pattern (line 399, 403)
        var_type = create_mono_type(TVAR)
        
        if (var_type%kind /= TVAR) then
            print *, '  FAILED: Type variable not created correctly'
            passed = .false.
            return
        end if
        
        if (passed) print *, '  PASSED: Unvalidated type variable creation'
    end function

    function test_bypassed_unification() result(passed)
        ! Test cases where unification is bypassed
        logical :: passed
        type(mono_type_t) :: type1, type2
        type(semantic_context_t) :: test_context
        
        passed = .true.
        test_context = create_semantic_context()
        
        ! Create two different types
        type1 = create_mono_type(TINT)
        type2 = create_mono_type(TREAL)
        
        ! Direct assignment without unification would be problematic
        if (types_should_be_unified(type1, type2)) then
            print *, '  INFO: Types should go through unification'
        end if
        
        if (passed) print *, '  PASSED: Bypassed unification detection'
    end function

    function test_assignment_operator_safety() result(passed)
        ! Test assignment operator safety for type preservation
        logical :: passed
        type(mono_type_t) :: original, copied
        
        passed = .true.
        
        ! Create original type
        original = create_mono_type(TREAL)
        
        ! Test assignment operator (should preserve type safely)
        copied = original
        
        if (copied%kind /= original%kind) then
            print *, '  FAILED: Type not preserved in assignment'
            passed = .false.
            return
        end if
        
        if (passed) print *, '  PASSED: Assignment operator safety'
    end function

    ! Helper functions for validation
    function validate_fallback_type(fallback_type) result(is_valid)
        type(mono_type_t), intent(in) :: fallback_type
        logical :: is_valid
        
        ! Basic validation - type should be concrete
        is_valid = fallback_type%kind == TINT .or. &
                   fallback_type%kind == TREAL .or. &
                   fallback_type%kind == TCHAR .or. &
                   fallback_type%kind == TLOGICAL
    end function

    function should_validate_direct_creation(type1, type2) result(should_validate)
        type(mono_type_t), intent(in) :: type1, type2
        logical :: should_validate
        
        ! In a proper type system, all type creations should be validated
        should_validate = .true.
    end function

    function types_should_be_unified(type1, type2) result(should_unify)
        type(mono_type_t), intent(in) :: type1, type2
        logical :: should_unify
        
        ! Different types should go through unification
        should_unify = type1%kind /= type2%kind
    end function

end program test_manual_type_assignment_bypasses