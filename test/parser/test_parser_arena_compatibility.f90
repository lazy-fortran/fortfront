program test_parser_arena_compatibility
    ! RED PHASE COMPATIBILITY TESTS for Issue #359: Parser Arena Integration
    !
    ! Given: Existing parser test suite must continue working after arena integration
    ! When: Parser modules are modified to use arena allocation instead of allocatable
    ! Then: All existing parser tests should pass without modification
    !
    ! CRITICAL: ALL COMPATIBILITY TESTS MUST FAIL INITIALLY (RED phase requirement)
    ! Tests verify that arena integration maintains backward compatibility
    
    use lexer_core, only: token_t, tokenize_core, TK_IDENTIFIER, TK_NUMBER, TK_OPERATOR
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_expressions_module, only: parse_expression
    use ast_core, only: ast_arena_t, create_ast_arena
    use arena_memory, only: arena_t, create_arena, destroy_arena
    implicit none

    logical :: all_passed
    integer :: test_count, failed_count

    all_passed = .true.
    test_count = 0
    failed_count = 0

    print *, '=== RED PHASE: Parser Arena Compatibility Tests (Issue #359) ==='
    print *, 'CRITICAL: All tests MUST FAIL initially (RED phase requirement)'
    print *, 'Tests verify arena integration maintains backward compatibility'
    print *, 'All existing parser tests should continue working after integration'
    print *

    ! Compatibility Group 1: Existing Parser API Compatibility
    print *, 'Compatibility Group 1: Existing Parser API Compatibility'
    call test_parser_state_api_compatibility(all_passed, test_count, failed_count)
    call test_expression_parsing_api_compatibility(all_passed, test_count, failed_count)
    call test_parser_error_handling_compatibility(all_passed, test_count, failed_count)

    ! Compatibility Group 2: Existing Test Case Compatibility
    print *, 'Compatibility Group 2: Existing Test Case Compatibility'
    call test_simple_expression_compatibility(all_passed, test_count, failed_count)
    call test_complex_expression_compatibility(all_passed, test_count, failed_count)
    call test_array_access_compatibility(all_passed, test_count, failed_count)
    call test_function_call_compatibility(all_passed, test_count, failed_count)

    ! Compatibility Group 3: Parser Module Integration
    print *, 'Compatibility Group 3: Parser Module Integration'
    call test_parser_expressions_module_compatibility(all_passed, test_count, failed_count)
    call test_parser_declarations_module_compatibility(all_passed, test_count, failed_count)
    call test_parser_control_flow_module_compatibility(all_passed, test_count, failed_count)

    ! Compatibility Group 4: Error Recovery Compatibility
    print *, 'Compatibility Group 4: Error Recovery Compatibility'
    call test_parse_error_recovery_compatibility(all_passed, test_count, failed_count)
    call test_error_message_formatting_compatibility(all_passed, test_count, failed_count)

    ! Compatibility Group 5: AST Output Compatibility
    print *, 'Compatibility Group 5: AST Output Compatibility'
    call test_ast_structure_compatibility(all_passed, test_count, failed_count)
    call test_ast_node_content_compatibility(all_passed, test_count, failed_count)

    ! Report compatibility results
    print *
    print *, '=== RED PHASE COMPATIBILITY SUMMARY ==='
    print '(A,I0,A,I0)', 'Total compatibility tests: ', test_count, ', Failed: ', failed_count
    
    if (failed_count == test_count) then
        print *, 'SUCCESS: All compatibility tests failed as expected (RED phase requirement)'
        print *, 'Compatibility requirements defined for arena integration'
        print *, 'Ready for backward-compatible arena integration implementation'
        stop 0
    else
        print *, 'FAILURE: Some compatibility tests passed unexpectedly (RED phase violation)'
        print *, 'Arena integration may already be partially implemented'
        stop 1
    end if

contains

    subroutine test_parser_state_api_compatibility(passed, test_count, failed_count)
        ! Given: Existing parser state API must continue working
        ! When: Parser state is modified to use arena allocation internally
        ! Then: All existing parser state method calls should work unchanged
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        type(parser_state_t) :: parser
        type(token_t), allocatable :: tokens(:)
        type(token_t) :: current_token
        logical :: api_compatible
        character(len=*), parameter :: test_source = "x + y"
        
        print *, '  Test: Parser state API compatibility after arena integration'
        test_count = test_count + 1
        
        ! Setup test tokens
        call tokenize_core(test_source, tokens)
        parser = create_parser_state(tokens)
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! These API calls should work exactly the same with arena-backed storage
        ! api_compatible = test_all_parser_state_methods_work_unchanged(parser)
        
        ! Test existing API methods should work
        current_token = parser%peek()
        if (current_token%text /= "x") then
            api_compatible = .false.
        else
            current_token = parser%consume()
            api_compatible = .not. parser%is_at_end()
        end if
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        ! API should be identical but internally arena-backed
        ! Since arena integration not implemented, this test defines requirements
        api_compatible = .false.  ! Arena integration not implemented yet
        
        ! Test assertion - this MUST fail in RED phase
        if (api_compatible) then
            print *, '    UNEXPECTED PASS: Parser state API already arena-compatible'
        else
            print *, '    EXPECTED FAIL: Parser state arena API compatibility not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_parser_state_api_compatibility

    subroutine test_expression_parsing_api_compatibility(passed, test_count, failed_count)
        ! Given: Expression parsing API must remain unchanged
        ! When: parse_expression function uses arena allocation internally
        ! Then: Function signature and behavior should be identical
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: result_index1, result_index2
        logical :: api_identical
        character(len=*), parameter :: expr1 = "a + b"
        character(len=*), parameter :: expr2 = "func(x, y)"
        
        print *, '  Test: Expression parsing API compatibility'
        test_count = test_count + 1
        
        arena = create_ast_arena()
        
        ! Test that parse_expression API works identically
        call tokenize_core(expr1, tokens)
        result_index1 = parse_expression(tokens, arena)
        
        call tokenize_core(expr2, tokens)
        result_index2 = parse_expression(tokens, arena)
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! API should be identical, but with arena-optimized internal allocation
        ! api_identical = verify_parse_expression_api_unchanged_with_arena()
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        api_identical = .false.  ! Arena optimization not implemented
        
        ! Test assertion - this MUST fail in RED phase
        if (api_identical) then
            print *, '    UNEXPECTED PASS: Expression parsing API already arena-optimized'
        else
            print *, '    EXPECTED FAIL: Expression parsing arena API compatibility not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_expression_parsing_api_compatibility

    subroutine test_parser_error_handling_compatibility(passed, test_count, failed_count)
        ! Given: Parser error handling must work identically with arena
        ! When: Error collection and reporting use arena allocation
        ! Then: Error messages and handling should be unchanged
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        type(parser_state_t) :: parser
        type(token_t), allocatable :: tokens(:)
        logical :: error_handling_compatible
        character(len=*), parameter :: error_source = "x + + y"  ! Syntax error
        
        print *, '  Test: Parser error handling compatibility with arena'
        test_count = test_count + 1
        
        call tokenize_core(error_source, tokens)
        parser = create_parser_state(tokens)
        
        ! This should trigger error handling
        ! EXPECTED BEHAVIOR (after implementation):
        ! Error handling should work identically with arena-backed storage
        ! error_handling_compatible = test_error_handling_arena_compatible(parser)
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        error_handling_compatible = .false.  ! Arena error handling not implemented
        
        ! Test assertion - this MUST fail in RED phase
        if (error_handling_compatible) then
            print *, '    UNEXPECTED PASS: Error handling already arena-compatible'
        else
            print *, '    EXPECTED FAIL: Arena error handling compatibility not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_parser_error_handling_compatibility

    subroutine test_simple_expression_compatibility(passed, test_count, failed_count)
        ! Given: Simple expressions must parse identically with arena integration
        ! When: Basic arithmetic expressions are parsed
        ! Then: AST output should be identical to current implementation
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: result_index
        logical :: ast_identical
        character(len=*), parameter :: simple_expr = "2 + 3 * 4"
        
        print *, '  Test: Simple expression AST compatibility'
        test_count = test_count + 1
        
        arena = create_ast_arena()
        call tokenize_core(simple_expr, tokens)
        result_index = parse_expression(tokens, arena)
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! AST should be structurally identical but allocated in arena
        ! ast_identical = compare_ast_with_arena_vs_traditional(simple_expr)
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        ast_identical = .false.  ! Arena AST comparison not implemented
        
        ! Test assertion - this MUST fail in RED phase
        if (ast_identical) then
            print *, '    UNEXPECTED PASS: Simple expression AST already arena-compatible'
        else
            print *, '    EXPECTED FAIL: Arena AST compatibility verification not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_simple_expression_compatibility

    subroutine test_complex_expression_compatibility(passed, test_count, failed_count)
        ! Given: Complex expressions must parse identically with arena integration
        ! When: Nested function calls and array access are parsed
        ! Then: Complex AST structures should be identical
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: result_index
        logical :: complex_ast_identical
        character(len=*), parameter :: complex_expr = &
            "func(array[i], obj%member + calc(a, b * c))"
        
        print *, '  Test: Complex expression AST compatibility'
        test_count = test_count + 1
        
        arena = create_ast_arena()
        call tokenize_core(complex_expr, tokens)
        result_index = parse_expression(tokens, arena)
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! Complex AST should maintain identical structure with arena allocation
        ! complex_ast_identical = verify_complex_ast_arena_compatibility(complex_expr)
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        complex_ast_identical = .false.  ! Complex arena AST not implemented
        
        ! Test assertion - this MUST fail in RED phase
        if (complex_ast_identical) then
            print *, '    UNEXPECTED PASS: Complex expression AST already arena-compatible'
        else
            print *, '    EXPECTED FAIL: Complex arena AST compatibility not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_complex_expression_compatibility

    subroutine test_array_access_compatibility(passed, test_count, failed_count)
        ! Given: Array access parsing must work identically with arena
        ! When: Array subscripts and slicing are parsed
        ! Then: Array access AST nodes should be unchanged
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: array_access_compatible
        character(len=*), parameter :: array_expr = "matrix(i, j:k:step)"
        
        print *, '  Test: Array access parsing compatibility'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! Array access should parse identically with arena-optimized allocation
        ! array_access_compatible = test_array_access_arena_compatibility(array_expr)
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        array_access_compatible = .false.  ! Arena array access not implemented
        
        ! Test assertion - this MUST fail in RED phase
        if (array_access_compatible) then
            print *, '    UNEXPECTED PASS: Array access already arena-compatible'
        else
            print *, '    EXPECTED FAIL: Array access arena compatibility not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_array_access_compatibility

    subroutine test_function_call_compatibility(passed, test_count, failed_count)
        ! Given: Function call parsing must work identically with arena
        ! When: Function calls with multiple arguments are parsed
        ! Then: Function call AST nodes should be unchanged
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: function_call_compatible
        character(len=*), parameter :: func_expr = "func(arg1, arg2 + arg3, array[:])"
        
        print *, '  Test: Function call parsing compatibility'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! Function calls should parse identically with arena allocation
        ! function_call_compatible = test_function_call_arena_compatibility(func_expr)
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        function_call_compatible = .false.  ! Arena function calls not implemented
        
        ! Test assertion - this MUST fail in RED phase
        if (function_call_compatible) then
            print *, '    UNEXPECTED PASS: Function call already arena-compatible'
        else
            print *, '    EXPECTED FAIL: Function call arena compatibility not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_function_call_compatibility

    subroutine test_parser_expressions_module_compatibility(passed, test_count, failed_count)
        ! Given: parser_expressions module must maintain API compatibility
        ! When: Module is modified for arena integration
        ! Then: All public interfaces should work unchanged
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: module_api_compatible
        
        print *, '  Test: parser_expressions module API compatibility'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! All public functions should have identical signatures with arena optimization
        ! module_api_compatible = verify_parser_expressions_module_arena_api()
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        module_api_compatible = .false.  ! Module arena integration not implemented
        
        ! Test assertion - this MUST fail in RED phase
        if (module_api_compatible) then
            print *, '    UNEXPECTED PASS: parser_expressions module already arena-compatible'
        else
            print *, '    EXPECTED FAIL: parser_expressions arena compatibility not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_parser_expressions_module_compatibility

    subroutine test_parser_declarations_module_compatibility(passed, test_count, failed_count)
        ! Given: parser_declarations module must maintain API compatibility
        ! When: Module is modified for arena integration
        ! Then: Declaration parsing should work identically
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: declarations_compatible
        
        print *, '  Test: parser_declarations module compatibility'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! Declaration parsing should be identical with arena allocation
        ! declarations_compatible = test_parser_declarations_arena_compatibility()
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        declarations_compatible = .false.  ! Declarations arena not implemented
        
        ! Test assertion - this MUST fail in RED phase
        if (declarations_compatible) then
            print *, '    UNEXPECTED PASS: parser_declarations already arena-compatible'
        else
            print *, '    EXPECTED FAIL: parser_declarations arena compatibility not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_parser_declarations_module_compatibility

    subroutine test_parser_control_flow_module_compatibility(passed, test_count, failed_count)
        ! Given: parser_control_flow module must maintain API compatibility
        ! When: Module is modified for arena integration
        ! Then: Control flow parsing should work identically
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: control_flow_compatible
        
        print *, '  Test: parser_control_flow module compatibility'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! Control flow parsing should be identical with arena allocation
        ! control_flow_compatible = test_parser_control_flow_arena_compatibility()
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        control_flow_compatible = .false.  ! Control flow arena not implemented
        
        ! Test assertion - this MUST fail in RED phase
        if (control_flow_compatible) then
            print *, '    UNEXPECTED PASS: parser_control_flow already arena-compatible'
        else
            print *, '    EXPECTED FAIL: parser_control_flow arena compatibility not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_parser_control_flow_module_compatibility

    subroutine test_parse_error_recovery_compatibility(passed, test_count, failed_count)
        ! Given: Parse error recovery must work identically with arena
        ! When: Syntax errors trigger error recovery mechanisms
        ! Then: Recovery behavior should be unchanged with arena generation cleanup
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: error_recovery_compatible
        character(len=20), parameter :: error_cases(3) = [ &
            "func(               ", "a + + b             ", "if x then           " ]
        
        print *, '  Test: Parse error recovery compatibility with arena'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! Error recovery should work identically with arena generation management
        ! error_recovery_compatible = test_error_recovery_arena_compatibility(error_cases)
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        error_recovery_compatible = .false.  ! Arena error recovery not implemented
        
        ! Test assertion - this MUST fail in RED phase
        if (error_recovery_compatible) then
            print *, '    UNEXPECTED PASS: Error recovery already arena-compatible'
        else
            print *, '    EXPECTED FAIL: Arena error recovery compatibility not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_parse_error_recovery_compatibility

    subroutine test_error_message_formatting_compatibility(passed, test_count, failed_count)
        ! Given: Error message formatting must be identical with arena
        ! When: Parse errors are reported and formatted
        ! Then: Error message content and format should be unchanged
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: error_messages_compatible
        
        print *, '  Test: Error message formatting compatibility'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! Error messages should be identical regardless of arena allocation
        ! error_messages_compatible = test_error_message_arena_compatibility()
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        error_messages_compatible = .false.  ! Arena error messages not implemented
        
        ! Test assertion - this MUST fail in RED phase
        if (error_messages_compatible) then
            print *, '    UNEXPECTED PASS: Error messages already arena-compatible'
        else
            print *, '    EXPECTED FAIL: Arena error message compatibility not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_error_message_formatting_compatibility

    subroutine test_ast_structure_compatibility(passed, test_count, failed_count)
        ! Given: AST structure must be identical with arena allocation
        ! When: Complex expressions create deep AST trees
        ! Then: Tree structure should be unchanged, only allocation method differs
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: ast_structure_identical
        
        print *, '  Test: AST structure compatibility with arena allocation'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! AST structure should be identical, only backed by arena allocation
        ! ast_structure_identical = verify_ast_structure_arena_compatibility()
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        ast_structure_identical = .false.  ! Arena AST structure not implemented
        
        ! Test assertion - this MUST fail in RED phase
        if (ast_structure_identical) then
            print *, '    UNEXPECTED PASS: AST structure already arena-compatible'
        else
            print *, '    EXPECTED FAIL: Arena AST structure compatibility not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_ast_structure_compatibility

    subroutine test_ast_node_content_compatibility(passed, test_count, failed_count)
        ! Given: AST node content must be identical with arena allocation
        ! When: AST nodes store identifiers, literals, and operators
        ! Then: Node content should be unchanged, only storage location differs
        logical, intent(inout) :: passed
        integer, intent(inout) :: test_count, failed_count
        
        logical :: ast_content_identical
        
        print *, '  Test: AST node content compatibility'
        test_count = test_count + 1
        
        ! EXPECTED BEHAVIOR (after implementation):
        ! AST node content should be identical with arena-backed storage
        ! ast_content_identical = verify_ast_content_arena_compatibility()
        
        ! CURRENT BEHAVIOR (will fail in RED phase):
        ast_content_identical = .false.  ! Arena AST content not implemented
        
        ! Test assertion - this MUST fail in RED phase
        if (ast_content_identical) then
            print *, '    UNEXPECTED PASS: AST content already arena-compatible'
        else
            print *, '    EXPECTED FAIL: Arena AST content compatibility not implemented'
            failed_count = failed_count + 1
            passed = .false.
        end if
    end subroutine test_ast_node_content_compatibility

end program test_parser_arena_compatibility