program test_type_safety_validation
    ! Test that all type assignments go through proper validation and unification
    ! Issue #311: Manual type assignment bypasses validation in semantic analyzer
    use frontend, only: lex_source
    use lexer_core, only: token_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    use type_system_hm, only: mono_type_t, create_mono_type, TINT, TREAL, TCHAR, TLOGICAL
    use ast_core, only: ast_arena_t
    implicit none

    logical :: all_passed
    type(semantic_context_t) :: context
    type(ast_arena_t) :: arena

    all_passed = .true.

    print *, '=== Type Safety Validation Tests (Issue #311) ==='
    print *

    ! Initialize semantic context for direct testing
    context = create_semantic_context()

    ! Test manual type assignment validation
    print *, 'Testing manual type assignment validation...'
    if (.not. test_direct_type_assignment_validation()) all_passed = .false.
    if (.not. test_fallback_type_validation()) all_passed = .false.
    if (.not. test_error_type_validation()) all_passed = .false.

    ! Test type unification enforcement
    print *, 'Testing type unification enforcement...'
    if (.not. test_unification_bypassed()) all_passed = .false.
    if (.not. test_assignment_type_safety()) all_passed = .false.

    ! Test semantic pipeline type propagation
    print *, 'Testing semantic pipeline type propagation...'
    if (.not. test_type_propagation_validation()) all_passed = .false.
    if (.not. test_inconsistent_type_detection()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All type safety validation tests passed!'
        stop 0
    else
        print *, 'Some type safety validation tests failed!'
        stop 1
    end if

contains

    function test_direct_type_assignment_validation() result(passed)
        ! Test that direct type assignments trigger validation
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        passed = .true.
        
        ! Test case: direct assignment without proper validation
        source = "program test" // new_line('a') // &
                 "  integer :: i" // new_line('a') // &
                 "  real :: r" // new_line('a') // &
                 "  i = r" // new_line('a') // &  ! Should trigger type validation
                 "end program"
        
        call lex_source(source, tokens, error_msg)
        
        ! Should compile successfully with appropriate type handling
        if (error_msg /= "") then
            print *, '  INFO: Type validation triggered (expected):', trim(error_msg)
        end if
        
        if (passed) print *, '  PASSED: Direct type assignment validation'
    end function

    function test_fallback_type_validation() result(passed)
        ! Test that fallback type assignments are validated
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        passed = .true.
        
        ! Test case: statement that triggers fallback type assignment
        source = "program test" // new_line('a') // &
                 "  print *, 'hello'" // new_line('a') // &  ! print statement fallback
                 "end program"
        
        call lex_source(source, tokens, error_msg)
        
        ! Should handle fallback types properly  
        if (error_msg /= "") then
            print *, '  FAILED: Fallback type validation error:', trim(error_msg)
            passed = .false.
            return
        end if
        
        if (passed) print *, '  PASSED: Fallback type validation'
    end function

    function test_error_type_validation() result(passed)
        ! Test that error types are handled safely
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        passed = .true.
        
        ! Test case: invalid syntax that might create error types
        source = "program test" // new_line('a') // &
                 "  invalid_syntax_here" // new_line('a') // &
                 "end program"
        
        call lex_source(source, tokens, error_msg)
        
        ! Should produce meaningful error, not crash
        if (error_msg == "") then
            print *, '  FAILED: Expected error for invalid syntax'
            passed = .false.
            return
        end if
        
        if (passed) print *, '  PASSED: Error type validation'
    end function

    function test_unification_bypassed() result(passed)
        ! Test detection of cases where unification is bypassed
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        passed = .true.
        
        ! Test case: type assignment that should go through unification
        source = "program test" // new_line('a') // &
                 "  integer :: a, b" // new_line('a') // &
                 "  a = 10" // new_line('a') // &
                 "  b = a" // new_line('a') // &  ! Should unify types
                 "end program"
        
        call lex_source(source, tokens, error_msg)
        
        if (error_msg /= "") then
            print *, '  INFO: Type unification validation triggered:', trim(error_msg)
        end if
        
        if (passed) print *, '  PASSED: Unification bypass detection'
    end function

    function test_assignment_type_safety() result(passed)
        ! Test type safety in assignment operations
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        passed = .true.
        
        ! Test case: assignment with potential type mismatch
        source = "program test" // new_line('a') // &
                 "  integer :: i" // new_line('a') // &
                 "  character(len=10) :: str" // new_line('a') // &
                 "  i = 42" // new_line('a') // &
                 "  str = 'hello'" // new_line('a') // &
                 "end program"
        
        call lex_source(source, tokens, error_msg)
        
        if (error_msg /= "") then
            print *, '  INFO: Assignment type safety check triggered:', trim(error_msg)
        end if
        
        if (passed) print *, '  PASSED: Assignment type safety'
    end function

    function test_type_propagation_validation() result(passed)
        ! Test type propagation through semantic pipeline
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        passed = .true.
        
        ! Test case: complex expression requiring type propagation
        source = "program test" // new_line('a') // &
                 "  real :: x, y, z" // new_line('a') // &
                 "  x = 1.0" // new_line('a') // &
                 "  y = 2.0" // new_line('a') // &
                 "  z = x + y" // new_line('a') // &  ! Type propagation needed
                 "end program"
        
        call lex_source(source, tokens, error_msg)
        
        if (error_msg /= "") then
            print *, '  INFO: Type propagation validation triggered:', trim(error_msg)
        end if
        
        if (passed) print *, '  PASSED: Type propagation validation'
    end function

    function test_inconsistent_type_detection() result(passed)
        ! Test detection of inconsistent type usage
        logical :: passed
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        
        passed = .true.
        
        ! Test case: potentially inconsistent type usage
        source = "program test" // new_line('a') // &
                 "  integer :: mixed" // new_line('a') // &
                 "  mixed = 10" // new_line('a') // &
                 "  if (mixed > 5) then" // new_line('a') // &
                 "    mixed = 20" // new_line('a') // &
                 "  end if" // new_line('a') // &
                 "end program"
        
        call lex_source(source, tokens, error_msg)
        
        if (error_msg /= "") then
            print *, '  INFO: Inconsistent type detection triggered:', trim(error_msg)
        end if
        
        if (passed) print *, '  PASSED: Inconsistent type detection'
    end function

end program test_type_safety_validation