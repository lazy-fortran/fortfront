program test_fortfront_api_lexical
    ! Test the public API lexical analysis functionality
    use fortfront, only: lex_source, token_t
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== fortfront Public API Lexical Analysis Tests ==='
    print *
    
    ! Test basic functionality
    if (.not. test_basic_lex_source()) all_passed = .false.
    if (.not. test_empty_source()) all_passed = .false.
    if (.not. test_complex_source()) all_passed = .false.
    if (.not. test_lazy_fortran_syntax()) all_passed = .false.
    if (.not. test_error_handling()) all_passed = .false.
    if (.not. test_token_positions()) all_passed = .false.
    
    ! Report results
    print *
    if (all_passed) then
        print *, 'All fortfront API lexical tests passed!'
        stop 0
    else
        print *, 'Some fortfront API lexical tests failed!'
        stop 1
    end if
    
contains
    
    logical function test_basic_lex_source()
        test_basic_lex_source = .true.
        print *, 'Testing basic lex_source functionality...'
        
        block
            type(token_t), allocatable :: tokens(:)
            character(len=:), allocatable :: error_msg
            
            ! Simple assignment
            call lex_source('x = 42', tokens, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Unexpected error: ', error_msg
                test_basic_lex_source = .false.
                return
            end if
            
            if (.not. allocated(tokens)) then
                print *, '  FAIL: No tokens allocated'
                test_basic_lex_source = .false.
                return
            end if
            
            if (size(tokens) < 3) then
                print *, '  FAIL: Expected at least 3 tokens, got', size(tokens)
                test_basic_lex_source = .false.
                return
            end if
            
            ! Check first few tokens
            if (tokens(1)%text /= 'x') then
                print *, '  FAIL: First token should be "x", got "', tokens(1)%text, '"'
                test_basic_lex_source = .false.
                return
            end if
            
            if (tokens(2)%text /= '=') then
                print *, '  FAIL: Second token should be "=", got "', tokens(2)%text, '"'
                test_basic_lex_source = .false.
                return
            end if
            
            if (tokens(3)%text /= '42') then
                print *, '  FAIL: Third token should be "42", got "', tokens(3)%text, '"'
                test_basic_lex_source = .false.
                return
            end if
            
            print *, '  PASS: Basic lex_source'
        end block
    end function test_basic_lex_source
    
    logical function test_empty_source()
        test_empty_source = .true.
        print *, 'Testing empty source handling...'
        
        block
            type(token_t), allocatable :: tokens(:)
            character(len=:), allocatable :: error_msg
            
            ! Empty string
            call lex_source('', tokens, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Unexpected error: ', error_msg
                test_empty_source = .false.
                return
            end if
            
            ! Only whitespace
            call lex_source('   ', tokens, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Unexpected error with whitespace: ', error_msg
                test_empty_source = .false.
                return
            end if
            
            print *, '  PASS: Empty source handling'
        end block
    end function test_empty_source
    
    logical function test_complex_source()
        test_complex_source = .true.
        print *, 'Testing complex source code...'
        
        block
            type(token_t), allocatable :: tokens(:)
            character(len=:), allocatable :: error_msg
            character(len=:), allocatable :: source
            
            ! Multi-line function
            source = 'real function add(a, b)' // new_line('A') // &
                     '    real :: a, b' // new_line('A') // &
                     '    add = a + b' // new_line('A') // &
                     'end function add'
            
            call lex_source(source, tokens, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Unexpected error: ', error_msg
                test_complex_source = .false.
                return
            end if
            
            if (.not. allocated(tokens)) then
                print *, '  FAIL: No tokens allocated'
                test_complex_source = .false.
                return
            end if
            
            ! Should have many tokens for a function
            if (size(tokens) < 15) then
                print *, '  FAIL: Expected more tokens for function, got', size(tokens)
                test_complex_source = .false.
                return
            end if
            
            print *, '  PASS: Complex source code'
        end block
    end function test_complex_source
    
    logical function test_lazy_fortran_syntax()
        test_lazy_fortran_syntax = .true.
        print *, 'Testing lazy Fortran syntax...'
        
        block
            type(token_t), allocatable :: tokens(:)
            character(len=:), allocatable :: error_msg
            
            ! Type inference syntax
            call lex_source('x := 42', tokens, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Unexpected error: ', error_msg
                test_lazy_fortran_syntax = .false.
                return
            end if
            
            if (.not. allocated(tokens) .or. size(tokens) < 3) then
                print *, '  FAIL: Expected tokens for lazy assignment'
                test_lazy_fortran_syntax = .false.
                return
            end if
            
            ! Arrow syntax
            call lex_source('y = f(x) -> x * 2', tokens, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Unexpected error with arrow syntax: ', error_msg
                test_lazy_fortran_syntax = .false.
                return
            end if
            
            print *, '  PASS: Lazy Fortran syntax'
        end block
    end function test_lazy_fortran_syntax
    
    logical function test_error_handling()
        test_error_handling = .true.
        print *, 'Testing error handling...'
        
        block
            type(token_t), allocatable :: tokens(:)
            character(len=:), allocatable :: error_msg
            
            ! Currently, the lexer doesn't produce errors for invalid syntax
            ! It just tokenizes what it can
            call lex_source('@@invalid@@', tokens, error_msg)
            
            ! Should not error at lexer level
            if (error_msg /= "") then
                print *, '  Note: Lexer produced error for invalid syntax: ', error_msg
            end if
            
            print *, '  PASS: Error handling (lexer is permissive)'
        end block
    end function test_error_handling
    
    logical function test_token_positions()
        test_token_positions = .true.
        print *, 'Testing token position tracking...'
        
        block
            type(token_t), allocatable :: tokens(:)
            character(len=:), allocatable :: error_msg
            character(len=:), allocatable :: source
            
            ! Multi-line source with specific positions
            source = 'x = 1' // new_line('A') // &
                     'y = 2'
            
            call lex_source(source, tokens, error_msg)
            
            if (error_msg /= "") then
                print *, '  FAIL: Unexpected error: ', error_msg
                test_token_positions = .false.
                return
            end if
            
            if (.not. allocated(tokens) .or. size(tokens) < 6) then
                print *, '  FAIL: Expected at least 6 tokens'
                test_token_positions = .false.
                return
            end if
            
            ! Check line numbers
            if (tokens(1)%line /= 1) then
                print *, '  FAIL: First token should be on line 1, got', tokens(1)%line
                test_token_positions = .false.
                return
            end if
            
            ! Find first token on line 2 (should be 'y')
            block
                integer :: i
                logical :: found_line_2
                found_line_2 = .false.
                
                do i = 1, size(tokens)
                    if (tokens(i)%line == 2 .and. tokens(i)%text == 'y') then
                        found_line_2 = .true.
                        exit
                    end if
                end do
                
                if (.not. found_line_2) then
                    print *, '  FAIL: Could not find "y" token on line 2'
                    test_token_positions = .false.
                    return
                end if
            end block
            
            print *, '  PASS: Token position tracking'
        end block
    end function test_token_positions
    
end program test_fortfront_api_lexical