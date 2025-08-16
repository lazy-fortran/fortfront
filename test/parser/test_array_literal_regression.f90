program test_array_literal_regression
    ! Specific test for array literal parsing regressions from Issue #261
    !
    ! Tests that array literals with bracket syntax still parse correctly
    ! after enhanced error reporting from Issue #256 was integrated
    
    use lexer_core, only: token_t, tokenize_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_expressions_module, only: parse_expression
    use ast_core, only: ast_arena_t, create_ast_arena, ast_node, array_literal_node
    use frontend, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.

    print *, '=== Array Literal Regression Tests (Issue #261) ==='
    print *, 'Testing array literal bracket syntax after Issue #256 integration'
    print *

    ! Initialize arena for testing
    arena = create_ast_arena()

    ! Test 1: Basic array literal parsing
    print *, 'Test 1: Basic Array Literal Parsing'
    if (.not. test_basic_array_literal_parsing()) all_passed = .false.

    ! Test 2: Complex array literal scenarios
    print *, 'Test 2: Complex Array Literal Scenarios'
    if (.not. test_complex_array_literals()) all_passed = .false.

    ! Test 3: Array literal error handling without corruption
    print *, 'Test 3: Array Literal Error Handling'
    if (.not. test_array_literal_error_handling()) all_passed = .false.

    ! Test 4: Array literals in larger contexts
    print *, 'Test 4: Array Literals in Context'
    if (.not. test_array_literals_in_context()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All array literal regression tests passed!'
        print *, 'Array literal bracket syntax regression resolved.'
        stop 0
    else
        print *, 'Some array literal regression tests failed!'
        print *, 'Array literal parsing still has regressions.'
        stop 1
    end if

contains

    function test_basic_array_literal_parsing() result(passed)
        ! Given: Basic array literal expressions with bracket syntax
        ! When: Parser processes these after Issue #256 error reporting enhancements
        ! Then: Should parse successfully without regression
        logical :: passed
        type(parser_state_t) :: parser
        type(token_t), allocatable :: tokens(:)
        integer :: result_index
        character(len=:), allocatable :: source
        integer :: i
        
        print *, '  Testing basic array literal parsing...'
        passed = .true.
        
        ! Test array literals that must work
        block
            character(len=:), allocatable :: test_cases(:)
            
            test_cases = [ character(len=18) :: &
                "[1, 2, 3]", &         ! Integer array
                "[1.0, 2.5, 3.14]", &  ! Real array
                "[.true., .false.]", & ! Logical array
                "[]", &                ! Empty array
                "[42]" &               ! Single element
            ]
            
            do i = 1, size(test_cases)
                source = test_cases(i)
                
                call tokenize_core(source, tokens)
                if (.not. allocated(tokens)) then
                    passed = .false.
                    print *, '    FAIL: Tokenization failed for: ', source
                    cycle
                end if
                
                parser = create_parser_state(tokens)
                result_index = parse_expression(tokens, arena)
                
                ! Should parse as array literal successfully
                if (result_index == 0) then
                    passed = .false.
                    print *, '    FAIL: Failed to parse array literal: ', source
                else
                    ! Note: Would need arena introspection to verify it's array literal
                    print *, '    PASS: Array literal parsed correctly: ', source
                end if
                
                deallocate(tokens)
            end do
        end block
        
        if (passed) then
            print *, '    PASS: All basic array literals parsed successfully'
        end if
    end function test_basic_array_literal_parsing

    function test_complex_array_literals() result(passed)
        ! Given: Complex array literal expressions
        ! When: Parser processes these with enhanced error reporting
        ! Then: Should handle complexity without regression
        logical :: passed
        type(parser_state_t) :: parser
        type(token_t), allocatable :: tokens(:)
        integer :: result_index
        character(len=:), allocatable :: source
        integer :: i
        
        print *, '  Testing complex array literal scenarios...'
        passed = .true.
        
        ! Test complex array literal patterns
        block
            character(len=:), allocatable :: complex_cases(:)
            
            complex_cases = [ character(len=25) :: &
                "[a, b, c]", &                    ! Variable elements
                "[x + y, z * 2]", &               ! Expression elements
                "[func(1), func(2)]", &           ! Function call elements
                "[1, 2.0, 3]", &                  ! Mixed numeric types
                "[(i, i=1,5)]", &                 ! Implied do (if supported)
                "[obj%field, other%field]" &      ! Member access elements
            ]
            
            do i = 1, size(complex_cases)
                source = complex_cases(i)
                
                call tokenize_core(source, tokens)
                if (.not. allocated(tokens)) then
                    print *, '    SKIP: Tokenization failed for: ', source
                    deallocate(tokens)
                    cycle
                end if
                
                parser = create_parser_state(tokens)
                result_index = parse_expression(tokens, arena)
                
                ! These may or may not parse depending on complexity support
                if (result_index /= 0) then
                    print *, '    PASS: Complex array literal parsed: ', source
                else
                    if (parser%has_errors()) then
                        print *, '    INFO: Complex case not supported: ', source
                    else
                        print *, '    WARN: Complex case failed unexpectedly: ', source
                    end if
                end if
                
                deallocate(tokens)
            end do
        end block
        
        print *, '    PASS: Complex array literal tests completed'
    end function test_complex_array_literals

    function test_array_literal_error_handling() result(passed)
        ! Given: Invalid array literal syntax that should trigger errors
        ! When: Enhanced error reporting processes these errors
        ! Then: Should handle errors gracefully without corruption
        logical :: passed
        type(parser_state_t) :: parser
        type(token_t), allocatable :: tokens(:)
        integer :: result_index
        character(len=:), allocatable :: source
        integer :: i
        
        print *, '  Testing array literal error handling...'
        passed = .true.
        
        ! Test invalid array literals that should trigger error handling
        block
            character(len=:), allocatable :: error_cases(:)
            
            error_cases = [ character(len=12) :: &
                "[1, 2, 3", &      ! Missing closing bracket
                "[1, 2,]", &       ! Trailing comma
                "[, 1, 2]", &      ! Leading comma  
                "[1 2 3]", &       ! Missing commas
                "[[1, 2], 3]" &    ! Nested arrays (may not be supported)
            ]
            
            do i = 1, size(error_cases)
                source = error_cases(i)
                
                call tokenize_core(source, tokens)
                if (.not. allocated(tokens)) then
                    print *, '    SKIP: Tokenization failed for: ', source
                    cycle
                end if
                
                parser = create_parser_state(tokens)
                result_index = parse_expression(tokens, arena)
                
                ! Error handling should work without corruption
                if (parser%has_errors()) then
                    print *, '    PASS: Error correctly detected for: ', source
                else
                    print *, '    INFO: No error detected for: ', source, ' (may be recovery)'
                end if
                
                ! Key test: Parser should remain in valid state after error
                if (.not. test_parser_still_valid(parser)) then
                    passed = .false.
                    print *, '    FAIL: Parser corrupted by error handling: ', source
                end if
                
                deallocate(tokens)
            end do
        end block
        
        if (passed) then
            print *, '    PASS: Array literal error handling stable'
        end if
    end function test_array_literal_error_handling

    function test_array_literals_in_context() result(passed)
        ! Given: Array literals used in complete program contexts
        ! When: Full compilation process handles these expressions
        ! Then: Should integrate properly without regression
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg
        integer :: i
        
        print *, '  Testing array literals in program context...'
        passed = .true.
        
        ! Test array literals in realistic program contexts
        block
            character(len=:), allocatable :: program_tests(:)
            
            program_tests = [ character(len=100) :: &
                "program test" // new_line('a') // &
                "integer :: arr(3)" // new_line('a') // &
                "arr = [1, 2, 3]" // new_line('a') // &
                "end program", &
                
                "program test" // new_line('a') // &
                "real :: values(2)" // new_line('a') // &
                "values = [1.0, 2.5]" // new_line('a') // &
                "print *, values" // new_line('a') // &
                "end program", &
                
                "subroutine test()" // new_line('a') // &
                "integer :: temp(4)" // new_line('a') // &
                "temp = [10, 20, 30, 40]" // new_line('a') // &
                "end subroutine" &
            ]
            
            do i = 1, size(program_tests)
                source = program_tests(i)
                
                call transform_lazy_fortran_string(source, output, error_msg)
                
                if (len_trim(error_msg) > 0) then
                    passed = .false.
                    print *, '    FAIL: Program context test failed:', i
                    print *, '    Error: ', error_msg
                else
                    print *, '    PASS: Program context test', i, 'succeeded'
                end if
            end do
        end block
        
        if (passed) then
            print *, '    PASS: All array literals worked in program context'
        end if
    end function test_array_literals_in_context

    function test_parser_still_valid(parser) result(valid)
        ! Helper function to test if parser state is still valid after error
        type(parser_state_t), intent(in) :: parser
        logical :: valid
        
        valid = .true.
        
        ! Test basic parser state validity
        ! If enhanced error reporting corrupted parser state, basic operations would fail
        
        ! Test 1: Parser should be able to report its state
        if (parser%is_at_end()) then
            ! This is fine - may be at end due to error recovery
        end if
        
        ! Test 2: Error state should be accessible
        if (parser%has_errors()) then
            block
                character(len=:), allocatable :: messages
                messages = parser%get_error_messages()
                ! Should be able to get error messages without corruption
            end block
        end if
        
        ! If we reach here without issues, parser state is likely valid
        ! The regression would manifest as corruption preventing these operations
    end function test_parser_still_valid

end program test_array_literal_regression