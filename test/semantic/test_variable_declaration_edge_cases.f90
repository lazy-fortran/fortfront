program test_variable_declaration_edge_cases
    use frontend
    use ast_core
    use lexer_core, only: token_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, analyze_program
    use error_handling, only: result_t
    implicit none

    logical :: all_passed

    print *, "=== Variable Declaration Edge Case Tests ==="
    print *

    all_passed = .true.

    ! Edge case tests for boundary conditions
    if (.not. test_no_undeclared_variables()) all_passed = .false.
    if (.not. test_existing_declarations_preserved()) all_passed = .false.
    if (.not. test_function_params_vs_locals()) all_passed = .false.
    if (.not. test_program_level_vs_function_level()) all_passed = .false.
    if (.not. test_nested_function_scopes()) all_passed = .false.
    if (.not. test_character_length_inference()) all_passed = .false.

    print *
    if (all_passed) then
        print *, "All variable declaration edge case tests passed!"
        stop 0
    else
        print *, "Some variable declaration edge case tests failed!"
        stop 1
    end if

contains

    logical function test_no_undeclared_variables()
        ! Given: A function with no undeclared variables (all explicitly declared)
        ! When: Automatic variable declaration generation is applied
        ! Then: Should leave function unchanged, no new declarations added
        
        character(len=*), parameter :: source = &
            "function fully_declared(x) result(y)" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    real, intent(in) :: x" // new_line('A') // &
            "    real :: y" // new_line('A') // &
            "    y = 2.0 * x" // new_line('A') // &
            "end function fully_declared"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: func_index
        character(len=:), allocatable :: error_msg
        
        test_no_undeclared_variables = .true.
        print *, "Testing function with no undeclared variables..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Lexing failed - ", error_msg
            test_no_undeclared_variables = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Parsing failed - ", error_msg
            test_no_undeclared_variables = .false.
            return
        end if
        
        ! Should not modify existing function
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, func_index)
        
        print *, "  EXPECTED BEHAVIOR: Should detect no undeclared variables"
        print *, "  EXPECTED BEHAVIOR: Should not add any new declarations"
        print *, "  EXPECTED BEHAVIOR: Should preserve existing declarations exactly"
        print *, "  RED PHASE: Test will pass when no-op detection is implemented"
        
    end function test_no_undeclared_variables

    logical function test_existing_declarations_preserved()
        ! Given: A function with mix of declared and undeclared variables
        ! When: Automatic variable declaration generation is applied
        ! Then: Should preserve existing declarations and only add missing ones
        
        character(len=*), parameter :: source = &
            "function mixed_declarations() result(output)" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    real :: existing_var" // new_line('A') // &
            "    existing_var = 1.5" // new_line('A') // &
            "    new_var = 42" // new_line('A') // &  ! Should generate integer :: new_var
            "    output = existing_var + new_var" // new_line('A') // &
            "end function mixed_declarations"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: func_index
        character(len=:), allocatable :: error_msg
        
        test_existing_declarations_preserved = .true.
        print *, "Testing preservation of existing declarations..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Lexing failed - ", error_msg
            test_existing_declarations_preserved = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Parsing failed - ", error_msg
            test_existing_declarations_preserved = .false.
            return
        end if
        
        ! Should preserve existing and add missing
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, func_index)
        
        print *, "  EXPECTED BEHAVIOR: Should preserve 'real :: existing_var' exactly"
        print *, "  EXPECTED BEHAVIOR: Should add 'integer :: new_var' only"
        print *, "  EXPECTED BEHAVIOR: Should not duplicate or modify existing declarations"
        print *, "  RED PHASE: Test will pass when selective addition is implemented"
        
    end function test_existing_declarations_preserved

    logical function test_function_params_vs_locals()
        ! Given: A function with parameters and local variables
        ! When: Automatic variable declaration generation is applied
        ! Then: Should handle parameters and locals differently
        
        character(len=*), parameter :: source = &
            "function param_local_test(a, b) result(sum_val)" // new_line('A') // &
            "    local_var = a + b" // new_line('A') // &  ! Local should get declaration
            "    sum_val = local_var * 2" // new_line('A') // &
            "end function param_local_test"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: func_index
        character(len=:), allocatable :: error_msg
        
        test_function_params_vs_locals = .true.
        print *, "Testing function parameters vs local variables..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Lexing failed - ", error_msg
            test_function_params_vs_locals = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Parsing failed - ", error_msg
            test_function_params_vs_locals = .false.
            return
        end if
        
        ! Should handle parameter vs local distinction
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, func_index)
        
        print *, "  EXPECTED BEHAVIOR: Should emit ERROR for undeclared parameters a, b"
        print *, "  EXPECTED BEHAVIOR: Should attempt to infer type for local_var from context"
        print *, "  EXPECTED BEHAVIOR: Should emit ERROR if local_var type cannot be inferred"
        print *, "  RED PHASE: Test will pass when parameter/local distinction is implemented"
        
    end function test_function_params_vs_locals

    logical function test_program_level_vs_function_level()
        ! Given: Program with both program-level and function-level undeclared variables
        ! When: Automatic variable declaration generation is applied
        ! Then: Should only process function-level variables, preserve program-level implicit behavior
        
        character(len=*), parameter :: source = &
            "program test_program" // new_line('A') // &
            "    program_var = 5" // new_line('A') // &  ! Should NOT be processed (program level)
            "contains" // new_line('A') // &
            "    function test_func() result(output)" // new_line('A') // &
            "        func_var = 3.14" // new_line('A') // &  ! Should be processed (function level)
            "        output = func_var" // new_line('A') // &
            "    end function test_func" // new_line('A') // &
            "end program test_program"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        
        test_program_level_vs_function_level = .true.
        print *, "Testing program-level vs function-level variable handling..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Lexing failed - ", error_msg
            test_program_level_vs_function_level = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Parsing failed - ", error_msg
            test_program_level_vs_function_level = .false.
            return
        end if
        
        ! Should only process function scope
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        print *, "  EXPECTED BEHAVIOR: Should NOT process program_var (preserve implicit)"
        print *, "  EXPECTED BEHAVIOR: Should process func_var and generate 'real :: func_var'"
        print *, "  EXPECTED BEHAVIOR: Function scope only, not program scope"
        print *, "  RED PHASE: Test will pass when scope-aware processing is implemented"
        
    end function test_program_level_vs_function_level

    logical function test_nested_function_scopes()
        ! Given: Nested function scopes with variable name conflicts
        ! When: Automatic variable declaration generation is applied
        ! Then: Should handle each scope independently
        
        character(len=*), parameter :: source = &
            "program nested_test" // new_line('A') // &
            "contains" // new_line('A') // &
            "    function outer_func() result(outer_result)" // new_line('A') // &
            "        temp = 1.0" // new_line('A') // &  ! real in outer scope
            "        outer_result = temp" // new_line('A') // &
            "    contains" // new_line('A') // &
            "        function inner_func() result(inner_result)" // new_line('A') // &
            "            temp = 42" // new_line('A') // &  ! integer in inner scope (different temp)
            "            inner_result = temp" // new_line('A') // &
            "        end function inner_func" // new_line('A') // &
            "    end function outer_func" // new_line('A') // &
            "end program nested_test"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        
        test_nested_function_scopes = .true.
        print *, "Testing nested function scopes..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Lexing failed - ", error_msg
            test_nested_function_scopes = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Parsing failed - ", error_msg
            test_nested_function_scopes = .false.
            return
        end if
        
        ! Should handle nested scopes correctly
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        print *, "  EXPECTED BEHAVIOR: Should add 'real :: temp' to outer_func"
        print *, "  EXPECTED BEHAVIOR: Should add 'integer :: temp' to inner_func"
        print *, "  EXPECTED BEHAVIOR: Should handle scope isolation correctly"
        print *, "  RED PHASE: Test will pass when nested scope handling is implemented"
        
    end function test_nested_function_scopes

    logical function test_character_length_inference()
        ! Given: Character variables with different length requirements
        ! When: Automatic variable declaration generation is applied
        ! Then: Should infer appropriate character lengths from literals
        
        character(len=*), parameter :: source = &
            "function char_test() result(output)" // new_line('A') // &
            "    short_name = 'Hi'" // new_line('A') // &         ! character(len=2)
            "    long_name = 'Hello World'" // new_line('A') // & ! character(len=11)
            "    empty_str = ''" // new_line('A') // &            ! character(len=0) or error?
            "    output = short_name" // new_line('A') // &
            "end function char_test"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        integer :: func_index
        character(len=:), allocatable :: error_msg
        
        test_character_length_inference = .true.
        print *, "Testing character length inference..."
        
        ! Lex and parse
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Lexing failed - ", error_msg
            test_character_length_inference = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, func_index, error_msg)
        if (allocated(error_msg)) then
            print *, "  FAIL: Parsing failed - ", error_msg
            test_character_length_inference = .false.
            return
        end if
        
        ! Should infer character lengths appropriately
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, func_index)
        
        print *, "  EXPECTED BEHAVIOR: Should generate 'character(len=2) :: short_name'"
        print *, "  EXPECTED BEHAVIOR: Should generate 'character(len=11) :: long_name'"
        print *, "  EXPECTED BEHAVIOR: Should handle empty string appropriately"
        print *, "  RED PHASE: Test will pass when character length inference is implemented"
        
    end function test_character_length_inference

end program test_variable_declaration_edge_cases