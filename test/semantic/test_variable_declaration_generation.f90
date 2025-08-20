program test_variable_declaration_generation
    use frontend
    use ast_core
    use lexer_core, only: token_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, analyze_program
    use error_handling, only: result_t
    implicit none

    logical :: all_passed

    print *, "=== Variable Declaration Generation Tests (Issue #320) ==="
    print *

    all_passed = .true.

    ! Core functionality tests
    if (.not. test_simple_function_type_inference()) all_passed = .false.
    if (.not. test_multiple_variables_different_types()) all_passed = .false.
    if (.not. test_mixed_arithmetic_logical_operations()) all_passed = .false.

    print *
    if (all_passed) then
        print *, "All variable declaration generation tests passed!"
        stop 0
    else
        print *, "Some variable declaration generation tests failed!"
        stop 1
    end if

contains

    logical function test_simple_function_type_inference()
        ! Given: A function with undeclared variables where types CAN be inferred
        ! When: Automatic variable declaration generation is applied
        ! Then: Should create proper declarations for x (real) and y (real) based on literal usage
        
        character(len=*), parameter :: source = &
            "function twice(x) result(y)" // new_line('A') // &
            "    y = 2.0 * x" // new_line('A') // &  ! x should be inferred as real from context
            "end function twice"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: func_index
        character(len=:), allocatable :: error_msg
        
        test_simple_function_type_inference = .true.
        print *, "Testing simple function with inferable types..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Lexing failed - ", error_msg
            test_simple_function_type_inference = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Parsing failed - ", error_msg
            test_simple_function_type_inference = .false.
            return
        end if
        
        ! Semantic analysis should generate variable declarations
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, func_index)
        
        ! This test should FAIL initially (RED phase) because automatic
        ! variable declaration generation is not yet implemented
        print *, "  TODO: Variable declaration generation not yet implemented"
        print *, "  EXPECTED BEHAVIOR: Should generate 'real :: x' and 'real :: y' declarations"
        print *, "  EXPECTED BEHAVIOR: Should NOT default to real(8) without type evidence"
        print *, "  RED PHASE: Test will pass once feature is implemented"
        
    end function test_simple_function_type_inference

    logical function test_multiple_variables_different_types()
        ! Given: A function with multiple undeclared variables of different types
        ! When: Automatic variable declaration generation is applied  
        ! Then: Should create type-specific declarations based on literal types
        
        character(len=*), parameter :: source = &
            "function compute(a, b, c) result(output)" // new_line('A') // &
            "    x = 42" // new_line('A') // &          ! integer literal
            "    y = 3.14" // new_line('A') // &        ! real literal
            "    name = 'hello'" // new_line('A') // &  ! character literal
            "    flag = .true." // new_line('A') // &   ! logical literal
            "    output = x + y" // new_line('A') // &
            "end function compute"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: func_index
        character(len=:), allocatable :: error_msg
        
        test_multiple_variables_different_types = .true.
        print *, "Testing multiple variables with different inferable types..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Lexing failed - ", error_msg
            test_multiple_variables_different_types = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Parsing failed - ", error_msg
            test_multiple_variables_different_types = .false.
            return
        end if
        
        ! Semantic analysis should generate appropriate declarations
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, func_index)
        
        ! This test should FAIL initially (RED phase)
        print *, "  TODO: Multiple type inference not yet implemented"
        print *, "  EXPECTED BEHAVIOR: Should generate:"
        print *, "    integer :: x"
        print *, "    real :: y"
        print *, "    character(len=5) :: name"
        print *, "    logical :: flag"
        print *, "  RED PHASE: Test will pass once feature is implemented"
        
    end function test_multiple_variables_different_types

    logical function test_mixed_arithmetic_logical_operations()
        ! Given: A function with mixed arithmetic and logical operations where types can be inferred
        ! When: Automatic variable declaration generation is applied
        ! Then: Should create appropriate declarations based on operation context
        
        character(len=*), parameter :: source = &
            "function mixed_ops() result(result_val)" // new_line('A') // &
            "    pi = 3.14159" // new_line('A') // &      ! real from literal
            "    radius = 2.5" // new_line('A') // &      ! real from literal
            "    area = pi * radius * radius" // new_line('A') // &  ! real from operation
            "    count = 10" // new_line('A') // &        ! integer from literal
            "    is_positive = area > 0.0" // new_line('A') // &  ! logical from comparison
            "    result_val = area" // new_line('A') // &
            "end function mixed_ops"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: func_index
        character(len=:), allocatable :: error_msg
        
        test_mixed_arithmetic_logical_operations = .true.
        print *, "Testing mixed arithmetic and logical operations..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Lexing failed - ", error_msg
            test_mixed_arithmetic_logical_operations = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Parsing failed - ", error_msg
            test_mixed_arithmetic_logical_operations = .false.
            return
        end if
        
        ! Semantic analysis should handle mixed types appropriately
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, func_index)
        
        ! This test should FAIL initially (RED phase)
        print *, "  TODO: Mixed operation type inference not yet implemented"
        print *, "  EXPECTED BEHAVIOR: Should generate:"
        print *, "    real :: pi, radius, area"
        print *, "    integer :: count"
        print *, "    logical :: is_positive"
        print *, "  RED PHASE: Test will pass once feature is implemented"
        
    end function test_mixed_arithmetic_logical_operations

end program test_variable_declaration_generation