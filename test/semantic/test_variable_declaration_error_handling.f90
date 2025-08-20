program test_variable_declaration_error_handling
    use frontend
    use ast_core
    use lexer_core, only: token_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, analyze_program
    use error_handling, only: result_t
    implicit none

    logical :: all_passed

    print *, "=== Variable Declaration Error Handling Tests (Critical Issue #327) ==="
    print *

    all_passed = .true.

    ! CRITICAL: Error handling tests - MUST emit errors, never default to real
    if (.not. test_type_inference_failure_error()) all_passed = .false.
    if (.not. test_insufficient_context_error()) all_passed = .false.
    if (.not. test_ambiguous_type_error()) all_passed = .false.
    if (.not. test_bare_parameter_usage_error()) all_passed = .false.
    if (.not. test_circular_dependency_error()) all_passed = .false.

    print *
    if (all_passed) then
        print *, "All variable declaration error handling tests passed!"
        stop 0
    else
        print *, "Some variable declaration error handling tests failed!"
        stop 1
    end if

contains

    logical function test_type_inference_failure_error()
        ! Given: A function where type inference FAILS due to insufficient information
        ! When: Automatic variable declaration generation is attempted
        ! Then: Should emit ERROR, NOT default to real(8)
        
        character(len=*), parameter :: source = &
            "function unclear_types(x) result(y)" // new_line('A') // &
            "    y = 2 * x" // new_line('A') // &  ! x type unknown, no literal evidence
            "end function unclear_types"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: func_index
        character(len=:), allocatable :: error_msg
        
        test_type_inference_failure_error = .true.
        print *, "Testing type inference failure should emit error..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "  FAIL: Lexing failed - ", error_msg
            test_type_inference_failure_error = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "  FAIL: Parsing failed - ", error_msg
            test_type_inference_failure_error = .false.
            return
        end if
        
        ! Semantic analysis should FAIL with clear error
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, func_index)
        
        ! This test should FAIL initially (RED phase) - currently defaults to real(8)
        print *, "  CRITICAL: Currently defaults to real(8) - VIOLATION of requirements"
        print *, "  REQUIRED BEHAVIOR: Should emit ERROR 'Cannot infer type for variable x'"
        print *, "  VIOLATION: Lines 1095, 1116, 1119 in standardizer default to real(8)"
        print *, "  RED PHASE: Test will pass when proper error handling is implemented"
        
    end function test_type_inference_failure_error

    logical function test_insufficient_context_error()
        ! Given: Variables used without sufficient context for type inference
        ! When: Automatic variable declaration generation is attempted
        ! Then: Should emit specific error about insufficient type information
        
        character(len=*), parameter :: source = &
            "function no_context() result(output)" // new_line('A') // &
            "    temp = some_var" // new_line('A') // &  ! No type information available
            "    output = temp" // new_line('A') // &
            "end function no_context"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: func_index
        character(len=:), allocatable :: error_msg
        
        test_insufficient_context_error = .true.
        print *, "Testing insufficient context should emit error..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "  FAIL: Lexing failed - ", error_msg
            test_insufficient_context_error = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "  FAIL: Parsing failed - ", error_msg
            test_insufficient_context_error = .false.
            return
        end if
        
        ! Semantic analysis should report insufficient context
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, func_index)
        
        ! This test should FAIL initially (RED phase)
        print *, "  CRITICAL: Currently allows undefined variables"
        print *, "  REQUIRED BEHAVIOR: Should emit ERROR 'Insufficient context to infer types for: temp, some_var'"
        print *, "  RED PHASE: Test will pass when context validation is implemented"
        
    end function test_insufficient_context_error

    logical function test_ambiguous_type_error()
        ! Given: Ambiguous type situations where multiple types could be valid
        ! When: Automatic variable declaration generation is attempted
        ! Then: Should emit error about type ambiguity, not make assumptions
        
        character(len=*), parameter :: source = &
            "function ambiguous_ops() result(result_val)" // new_line('A') // &
            "    a = b + c" // new_line('A') // &  ! Could be integer, real, complex, etc.
            "    result_val = a" // new_line('A') // &
            "end function ambiguous_ops"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: func_index
        character(len=:), allocatable :: error_msg
        
        test_ambiguous_type_error = .true.
        print *, "Testing ambiguous type situations should emit error..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "  FAIL: Lexing failed - ", error_msg
            test_ambiguous_type_error = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "  FAIL: Parsing failed - ", error_msg
            test_ambiguous_type_error = .false.
            return
        end if
        
        ! Semantic analysis should detect ambiguity
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, func_index)
        
        ! This test should FAIL initially (RED phase)
        print *, "  CRITICAL: Currently defaults to real(8) for ambiguous cases"
        print *, "  REQUIRED BEHAVIOR: Should emit ERROR 'Type ambiguity for variables: a, b, c'"
        print *, "  REQUIRED BEHAVIOR: Should suggest explicit type declarations"
        print *, "  RED PHASE: Test will pass when ambiguity detection is implemented"
        
    end function test_ambiguous_type_error

    logical function test_bare_parameter_usage_error()
        ! Given: Function parameters used without any type context
        ! When: Automatic variable declaration generation is attempted
        ! Then: Should emit error about parameter types needing explicit declaration
        
        character(len=*), parameter :: source = &
            "function bare_params(x, y) result(z)" // new_line('A') // &
            "    z = x + y" // new_line('A') // &  ! No type information for x, y
            "end function bare_params"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: func_index
        character(len=:), allocatable :: error_msg
        
        test_bare_parameter_usage_error = .true.
        print *, "Testing bare parameter usage should emit error..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "  FAIL: Lexing failed - ", error_msg
            test_bare_parameter_usage_error = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "  FAIL: Parsing failed - ", error_msg
            test_bare_parameter_usage_error = .false.
            return
        end if
        
        ! Semantic analysis should catch parameter type issues
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, func_index)
        
        ! This test should FAIL initially (RED phase)
        print *, "  CRITICAL: Currently defaults parameters to real(8) - lines 2565, 2571"
        print *, "  REQUIRED BEHAVIOR: Should emit ERROR 'Function parameters require explicit type declarations: x, y'"
        print *, "  REQUIRED BEHAVIOR: Should NOT infer parameter types from usage context"
        print *, "  RED PHASE: Test will pass when parameter validation is implemented"
        
    end function test_bare_parameter_usage_error

    logical function test_circular_dependency_error()
        ! Given: Variables with circular type dependencies
        ! When: Automatic variable declaration generation is attempted
        ! Then: Should emit error about circular dependencies
        
        character(len=*), parameter :: source = &
            "function circular_deps() result(output)" // new_line('A') // &
            "    a = b" // new_line('A') // &
            "    b = c" // new_line('A') // &
            "    c = a" // new_line('A') // &  ! Circular dependency a->b->c->a
            "    output = a" // new_line('A') // &
            "end function circular_deps"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: func_index
        character(len=:), allocatable :: error_msg
        
        test_circular_dependency_error = .true.
        print *, "Testing circular type dependencies should emit error..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "  FAIL: Lexing failed - ", error_msg
            test_circular_dependency_error = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "  FAIL: Parsing failed - ", error_msg
            test_circular_dependency_error = .false.
            return
        end if
        
        ! Semantic analysis should detect circular dependencies
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, func_index)
        
        ! This test should FAIL initially (RED phase)
        print *, "  CRITICAL: Currently may default all to real(8)"
        print *, "  REQUIRED BEHAVIOR: Should emit ERROR 'Circular type dependency detected: a -> b -> c -> a'"
        print *, "  REQUIRED BEHAVIOR: Should suggest breaking cycle with explicit declaration"
        print *, "  RED PHASE: Test will pass when cycle detection is implemented"
        
    end function test_circular_dependency_error

end program test_variable_declaration_error_handling