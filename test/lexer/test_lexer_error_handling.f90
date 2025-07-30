program test_lexer_error_handling
    use lexer_core, only: token_t, tokenize_core, TK_UNKNOWN, TK_STRING, TK_NUMBER, &
                          TK_OPERATOR, TK_EOF, TK_IDENTIFIER
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Lexer Error Handling Tests ==='
    print *

    ! Test unterminated string literals
    print *, 'Testing unterminated string literals...'
    if (.not. test_unterminated_string_single()) all_passed = .false.
    if (.not. test_unterminated_string_double()) all_passed = .false.
    if (.not. test_unterminated_string_multiline()) all_passed = .false.
    
    ! Test invalid number formats
    print *, 'Testing invalid number formats...'
    if (.not. test_invalid_float_format()) all_passed = .false.
    if (.not. test_invalid_exponent_format()) all_passed = .false.
    if (.not. test_number_with_invalid_suffix()) all_passed = .false.
    
    ! Test invalid characters
    print *, 'Testing invalid characters...'
    if (.not. test_invalid_unicode_chars()) all_passed = .false.
    if (.not. test_control_characters()) all_passed = .false.
    if (.not. test_invalid_operators()) all_passed = .false.
    
    ! Test edge cases
    print *, 'Testing edge cases...'
    if (.not. test_empty_source()) all_passed = .false.
    if (.not. test_only_whitespace()) all_passed = .false.
    if (.not. test_very_long_identifier()) all_passed = .false.
    if (.not. test_deeply_nested_operators()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All lexer error handling tests passed!'
        stop 0
    else
        print *, 'Some lexer error handling tests failed!'
        stop 1
    end if

contains

    function test_unterminated_string_single() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        source = "program test" // new_line('a') // &
                 "  print *, 'unterminated string"
        
        call tokenize_core(source, tokens)
        
        ! Should still tokenize but mark string as extending to EOF
        if (size(tokens) < 4) then
            print *, '  FAILED: Should tokenize available tokens'
            passed = .false.
        else if (tokens(4)%kind /= TK_STRING) then
            print *, '  FAILED: Should recognize string start'
            passed = .false.
        end if
        
        if (passed) print *, '  PASSED: Unterminated single quote string'
    end function

    function test_unterminated_string_double() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        source = 'x = "unterminated'
        
        call tokenize_core(source, tokens)
        
        if (size(tokens) < 3) then
            print *, '  FAILED: Should tokenize available tokens'
            passed = .false.
        else if (tokens(3)%kind /= TK_STRING) then
            print *, '  FAILED: Should recognize string start'
            passed = .false.
        end if
        
        if (passed) print *, '  PASSED: Unterminated double quote string'
    end function
    
    function test_unterminated_string_multiline() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        source = "s = '" // new_line('a') // "multi" // new_line('a') // "line"
        
        call tokenize_core(source, tokens)
        
        ! Fortran doesn't allow multiline strings without continuation
        if (size(tokens) < 3) then
            print *, '  FAILED: Should tokenize what it can'
            passed = .false.
        end if
        
        if (passed) print *, '  PASSED: Multiline string handling'
    end function

    function test_invalid_float_format() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        source = "x = 3.14.159"  ! Two decimal points
        
        call tokenize_core(source, tokens)
        
        ! Should tokenize as separate tokens
        if (size(tokens) < 5) then
            print *, '  FAILED: Should split invalid number'
            passed = .false.
        else if (tokens(3)%kind /= TK_NUMBER) then
            print *, '  FAILED: First part should be number'
            passed = .false.
        else if (tokens(4)%kind /= TK_OPERATOR) then
            print *, '  FAILED: Period should be operator'
            passed = .false.
        end if
        
        if (passed) print *, '  PASSED: Invalid float format'
    end function
    
    function test_invalid_exponent_format() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        source = "x = 1.0e++"  ! Invalid exponent
        
        call tokenize_core(source, tokens)
        
        if (size(tokens) < 4) then
            print *, '  FAILED: Should tokenize available parts'
            passed = .false.
        else if (tokens(3)%kind /= TK_NUMBER) then
            print *, '  FAILED: Should get base number'
            passed = .false.
        end if
        
        if (passed) print *, '  PASSED: Invalid exponent format'
    end function
    
    function test_number_with_invalid_suffix() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        source = "x = 42abc"  ! Number followed by letters
        
        call tokenize_core(source, tokens)
        
        ! Should split into number and identifier
        if (size(tokens) < 4) then
            print *, '  FAILED: Should split number and suffix'
            passed = .false.
        else if (tokens(3)%kind /= TK_NUMBER) then
            print *, '  FAILED: First part should be number'
            passed = .false.
        else if (tokens(4)%kind /= TK_IDENTIFIER) then
            print *, '  FAILED: Letters should form identifier'
            passed = .false.
        end if
        
        if (passed) print *, '  PASSED: Number with invalid suffix'
    end function

    function test_invalid_unicode_chars() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        ! Using invalid ASCII characters
        source = "x = " // char(1) // char(2) // char(3)
        
        call tokenize_core(source, tokens)
        
        if (size(tokens) < 3) then
            print *, '  FAILED: Should handle invalid chars'
            passed = .false.
        end if
        
        if (passed) print *, '  PASSED: Invalid unicode characters'
    end function
    
    function test_control_characters() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        source = "x" // char(9) // "=" // char(13) // "42"  ! Tab and CR
        
        call tokenize_core(source, tokens)
        
        if (size(tokens) < 4) then
            print *, '  FAILED: Should handle control chars as whitespace'
            passed = .false.
        else
            if (tokens(1)%kind /= TK_IDENTIFIER) then
                print *, '  FAILED: First token should be identifier'
                passed = .false.
            end if
            if (tokens(2)%kind /= TK_OPERATOR) then
                print *, '  FAILED: Second token should be operator'
                passed = .false.
            end if
            if (tokens(3)%kind /= TK_NUMBER) then
                print *, '  FAILED: Third token should be number'
                passed = .false.
            end if
        end if
        
        if (passed) print *, '  PASSED: Control characters'
    end function
    
    function test_invalid_operators() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        source = "x @ y"  ! @ is not a valid Fortran operator
        
        call tokenize_core(source, tokens)
        
        if (size(tokens) < 3) then
            print *, '  FAILED: Should tokenize at least identifier and unknown operator'
            passed = .false.
        else if (size(tokens) >= 4) then
            if (.not. (tokens(2)%kind == TK_UNKNOWN .or. tokens(2)%kind == TK_OPERATOR)) then
                print *, '  FAILED: Invalid operator should be TK_UNKNOWN or TK_OPERATOR'
                passed = .false.
            end if
        end if
        
        if (passed) print *, '  PASSED: Invalid operators'
    end function

    function test_empty_source() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        source = ""
        
        call tokenize_core(source, tokens)
        
        if (size(tokens) /= 1) then
            print *, '  FAILED: Empty source should produce single EOF token'
            passed = .false.
        else if (tokens(1)%kind /= TK_EOF) then
            print *, '  FAILED: Token should be EOF'
            passed = .false.
        end if
        
        if (passed) print *, '  PASSED: Empty source'
    end function
    
    function test_only_whitespace() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        source = "   " // new_line('a') // "  " // new_line('a') // "    "
        
        call tokenize_core(source, tokens)
        
        if (size(tokens) /= 1) then
            print *, '  FAILED: Whitespace-only should produce single EOF token'
            passed = .false.
        else if (tokens(1)%kind /= TK_EOF) then
            print *, '  FAILED: Token should be EOF'
            passed = .false.
        end if
        
        if (passed) print *, '  PASSED: Only whitespace'
    end function
    
    function test_very_long_identifier() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        character(len=200) :: long_name
        integer :: i
        
        passed = .true.
        ! Create 200-character identifier
        do i = 1, 200
            long_name(i:i) = 'a'
        end do
        
        source = trim(long_name) // " = 42"
        
        call tokenize_core(source, tokens)
        
        if (size(tokens) < 4) then
            print *, '  FAILED: Should handle long identifier'
            passed = .false.
        else if (tokens(1)%kind /= TK_IDENTIFIER) then
            print *, '  FAILED: Long name should be identifier'
            passed = .false.
        else if (len(tokens(1)%text) <= 100) then
            print *, '  FAILED: Identifier text should be preserved'
            passed = .false.
        end if
        
        if (passed) print *, '  PASSED: Very long identifier'
    end function
    
    function test_deeply_nested_operators() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        integer :: open_count, close_count, i
        
        passed = .true.
        source = "x = ((((((((((a))))))))))"
        
        call tokenize_core(source, tokens)
        
        ! Count parentheses
        if (size(tokens) < 24) then
            print *, '  FAILED: All operators should be tokenized'
            passed = .false.
        else
            ! Verify balanced parentheses in tokens
            open_count = 0
            close_count = 0
            do i = 1, size(tokens)
                if (tokens(i)%text == "(") open_count = open_count + 1
                if (tokens(i)%text == ")") close_count = close_count + 1
            end do
            
            if (open_count /= 10) then
                print *, '  FAILED: Should have ten open parentheses'
                passed = .false.
            end if
            
            if (close_count /= 10) then
                print *, '  FAILED: Should have ten close parentheses'
                passed = .false.
            end if
        end if
        
        if (passed) print *, '  PASSED: Deeply nested operators'
    end function

end program test_lexer_error_handling