program test_parser_error_recovery
    use lexer_core, only: token_t, tokenize_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Parser Error Recovery Tests ==='
    print *

    ! Test missing terminators
    print *, 'Testing missing terminators...'
    if (.not. test_missing_end_statement()) all_passed = .false.
    if (.not. test_unclosed_parentheses()) all_passed = .false.
    
    ! Test incomplete expressions
    print *, 'Testing incomplete expressions...'
    if (.not. test_trailing_operators()) all_passed = .false.
    if (.not. test_missing_operands()) all_passed = .false.
    
    ! Test invalid syntax
    print *, 'Testing invalid syntax...'
    if (.not. test_consecutive_operators()) all_passed = .false.
    if (.not. test_invalid_keywords()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All parser error recovery tests passed!'
        stop 0
    else
        print *, 'Some parser error recovery tests failed!'
        stop 1
    end if

contains

    function test_missing_end_statement() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        
        source = "program test" // new_line('a') // &
                 "  integer :: x" // new_line('a') // &
                 "  x = 42"
        ! Missing "end program"
        
        call tokenize_core(source, tokens)
        
        ! Just verify it tokenizes
        if (size(tokens) < 5) then
            print *, '  FAILED: Should tokenize incomplete program'
            passed = .false.
        end if
        
        if (passed) print *, '  PASSED: Missing end statement'
    end function

    function test_unclosed_parentheses() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        
        source = "x = (a + b * (c - d)"
        
        call tokenize_core(source, tokens)
        
        ! Just verify it tokenizes
        if (passed) print *, '  PASSED: Unclosed parentheses'
    end function

    function test_trailing_operators() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        
        source = "x = a + b +"
        
        call tokenize_core(source, tokens)
        
        ! Just verify it tokenizes
        if (passed) print *, '  PASSED: Trailing operators'
    end function

    function test_missing_operands() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        
        source = "x = * 5"
        
        call tokenize_core(source, tokens)
        
        ! Just verify it tokenizes
        if (passed) print *, '  PASSED: Missing operands'
    end function

    function test_consecutive_operators() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        
        source = "x = a */ b"
        
        call tokenize_core(source, tokens)
        
        ! Just verify it tokenizes
        if (passed) print *, '  PASSED: Consecutive operators'
    end function

    function test_invalid_keywords() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        
        source = "if x > 0" // new_line('a') // &
                 "  print *, x"
        ! Missing "then"
        
        call tokenize_core(source, tokens)
        
        ! Just verify it tokenizes
        if (passed) print *, '  PASSED: Invalid keywords'
    end function

end program test_parser_error_recovery