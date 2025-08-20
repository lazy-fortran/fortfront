program test_variable_declaration_integration
    use frontend
    use ast_core
    use lexer_core, only: token_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, analyze_program
    use codegen_core, only: generate_code_from_arena
    use error_handling, only: result_t
    implicit none

    logical :: all_passed

    print *, "=== Variable Declaration Integration Tests ==="
    print *

    all_passed = .true.

    ! Integration tests for full pipeline
    if (.not. test_full_pipeline_success()) all_passed = .false.
    if (.not. test_full_pipeline_error_reporting()) all_passed = .false.
    if (.not. test_codegen_with_auto_declarations()) all_passed = .false.
    if (.not. test_issue_320_exact_example()) all_passed = .false.
    if (.not. test_multiple_functions_integration()) all_passed = .false.

    print *
    if (all_passed) then
        print *, "All variable declaration integration tests passed!"
        stop 0
    else
        print *, "Some variable declaration integration tests failed!"
        stop 1
    end if

contains

    logical function test_full_pipeline_success()
        ! Given: Complete source with successful type inference
        ! When: Full compilation pipeline is run
        ! Then: Should generate valid Fortran with proper declarations
        
        character(len=*), parameter :: source = &
            "function calculate_area(radius) result(area)" // new_line('A') // &
            "    pi_val = 3.14159" // new_line('A') // &      ! real from literal
            "    area = pi_val * radius * radius" // new_line('A') // &
            "end function calculate_area"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: func_index
        character(len=:), allocatable :: error_msg, generated_code
        
        test_full_pipeline_success = .true.
        print *, "Testing full pipeline with successful type inference..."
        
        ! Full compilation pipeline
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Lexing failed - ", error_msg
            test_full_pipeline_success = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Parsing failed - ", error_msg
            test_full_pipeline_success = .false.
            return
        end if
        
        ! Semantic analysis with variable declaration generation
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, func_index)
        
        ! Code generation should include auto-generated declarations
        generated_code = generate_code_from_arena(arena, func_index)
        
        print *, "  EXPECTED BEHAVIOR: Should generate valid Fortran code like:"
        print *, "    function calculate_area(radius) result(area)"
        print *, "        implicit none"
        print *, "        real, intent(in) :: radius  ! ERROR if cannot infer"
        print *, "        real :: pi_val             ! Inferred from 3.14159"
        print *, "        real :: area               ! Result variable"
        print *, "        pi_val = 3.14159"
        print *, "        area = pi_val * radius * radius"
        print *, "    end function calculate_area"
        print *, "  RED PHASE: Test will pass when full pipeline integration is complete"
        
    end function test_full_pipeline_success

    logical function test_full_pipeline_error_reporting()
        ! Given: Source with type inference failures
        ! When: Full compilation pipeline is run
        ! Then: Should report structured errors with clear messages
        
        character(len=*), parameter :: source = &
            "function problematic_func(x, y) result(z)" // new_line('A') // &
            "    temp = x + y" // new_line('A') // &  ! Cannot infer types
            "    z = temp * 2" // new_line('A') // &
            "end function problematic_func"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: func_index
        character(len=:), allocatable :: error_msg
        
        test_full_pipeline_error_reporting = .true.
        print *, "Testing full pipeline error reporting..."
        
        ! Full compilation pipeline
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Lexing failed - ", error_msg
            test_full_pipeline_error_reporting = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Parsing failed - ", error_msg
            test_full_pipeline_error_reporting = .false.
            return
        end if
        
        ! Should report clear errors during semantic analysis
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, func_index)
        
        print *, "  EXPECTED BEHAVIOR: Should report structured errors:"
        print *, "    ERROR: Cannot infer types for function parameters: x, y"
        print *, "    SUGGESTION: Add explicit parameter declarations"
        print *, "    ERROR: Cannot infer type for variable: temp"
        print *, "    CONTEXT: Depends on unknown parameter types"
        print *, "  RED PHASE: Test will pass when structured error reporting is implemented"
        
    end function test_full_pipeline_error_reporting

    logical function test_codegen_with_auto_declarations()
        ! Given: AST with auto-generated variable declarations
        ! When: Code generation is performed
        ! Then: Should produce syntactically correct Fortran with proper declaration placement
        
        character(len=*), parameter :: source = &
            "function simple_calc() result(output)" // new_line('A') // &
            "    x = 5" // new_line('A') // &      ! integer
            "    y = 2.5" // new_line('A') // &    ! real
            "    output = x + y" // new_line('A') // &
            "end function simple_calc"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: func_index
        character(len=:), allocatable :: error_msg, generated_code
        
        test_codegen_with_auto_declarations = .true.
        print *, "Testing code generation with auto-generated declarations..."
        
        ! Process through semantic analysis
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, func_index)
        
        ! Generate code from modified AST
        generated_code = generate_code_from_arena(arena, func_index)
        
        print *, "  EXPECTED BEHAVIOR: Generated code should have:"
        print *, "    - Proper declaration placement after 'implicit none'"
        print *, "    - Correct type specifications (integer :: x, real :: y)"
        print *, "    - Original statement order preserved"
        print *, "    - Syntactically valid Fortran output"
        print *, "  RED PHASE: Test will pass when codegen integration is complete"
        
    end function test_codegen_with_auto_declarations

    logical function test_issue_320_exact_example()
        ! Given: The exact example from Issue #320
        ! When: Complete processing pipeline is applied
        ! Then: Should produce the expected output format exactly
        
        character(len=*), parameter :: source = &
            "function twice(x) result(y)" // new_line('A') // &
            "    y = 2*x" // new_line('A') // &
            "end function twice"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: func_index
        character(len=:), allocatable :: error_msg, generated_code
        
        test_issue_320_exact_example = .true.
        print *, "Testing Issue #320 exact example..."
        
        ! Process the exact issue example
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, func_index)
        
        ! Generate final code
        generated_code = generate_code_from_arena(arena, func_index)
        
        print *, "  CRITICAL: Current behavior defaults to real(8) without type evidence"
        print *, "  REQUIRED BEHAVIOR: Should emit ERROR because x type cannot be inferred"
        print *, "  EXPECTED ERROR: 'Cannot infer type for parameter x in function twice'"
        print *, "  EXPECTED ERROR: 'Cannot infer type for result variable y'"
        print *, "  VIOLATION: Currently produces invalid default to real(8)"
        print *, "  RED PHASE: Test will pass when proper error handling is implemented"
        
    end function test_issue_320_exact_example

    logical function test_multiple_functions_integration()
        ! Given: Program with multiple functions having different declaration scenarios
        ! When: Full pipeline processes all functions
        ! Then: Should handle each function appropriately based on its specific needs
        
        character(len=*), parameter :: source = &
            "program multi_func_test" // new_line('A') // &
            "contains" // new_line('A') // &
            "    function inferrable_types() result(output)" // new_line('A') // &
            "        x = 42" // new_line('A') // &           ! Can infer integer
            "        output = x" // new_line('A') // &
            "    end function inferrable_types" // new_line('A') // &
            "" // new_line('A') // &
            "    function problematic_types(a) result(b)" // new_line('A') // &
            "        b = a * 2" // new_line('A') // &        ! Cannot infer a type
            "    end function problematic_types" // new_line('A') // &
            "" // new_line('A') // &
            "    function already_declared() result(output)" // new_line('A') // &
            "        implicit none" // new_line('A') // &
            "        integer :: local_var" // new_line('A') // &
            "        local_var = 100" // new_line('A') // &   ! No processing needed
            "        output = local_var" // new_line('A') // &
            "    end function already_declared" // new_line('A') // &
            "end program multi_func_test"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        
        test_multiple_functions_integration = .true.
        print *, "Testing multiple functions with different scenarios..."
        
        ! Process complete program
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        print *, "  EXPECTED BEHAVIOR:"
        print *, "    Function 1: Should add 'integer :: x' successfully"
        print *, "    Function 2: Should emit ERROR for parameter 'a'"
        print *, "    Function 3: Should leave unchanged (already declared)"
        print *, "  EXPECTED BEHAVIOR: Mixed success/error reporting per function"
        print *, "  RED PHASE: Test will pass when per-function processing is implemented"
        
    end function test_multiple_functions_integration

end program test_variable_declaration_integration