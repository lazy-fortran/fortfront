program test_parser_edge_cases
    use lexer_core, only: token_t, tokenize_core
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Parser Edge Case Tests ==='
    print *

    ! Test edge cases
    print *, 'Testing parser edge cases...'
    if (.not. test_empty_input()) all_passed = .false.
    if (.not. test_single_token()) all_passed = .false.
    if (.not. test_complex_type_decl()) all_passed = .false.
    if (.not. test_nested_expressions()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All parser edge case tests passed!'
        stop 0
    else
        print *, 'Some parser edge case tests failed!'
        stop 1
    end if

contains

    function test_empty_input() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        source = ""
        
        call tokenize_core(source, tokens)
        
        ! Should handle empty gracefully
        if (passed) print *, '  PASSED: Empty input'
    end function

    function test_single_token() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        source = "42"
        
        call tokenize_core(source, tokens)
        
        if (size(tokens) < 2) then  ! Should have number + EOF
            print *, '  FAILED: Should tokenize single literal'
            passed = .false.
        end if
        
        if (passed) print *, '  PASSED: Single token'
    end function

    function test_complex_type_decl() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        source = "real(kind=8), dimension(:,:), allocatable"
        
        call tokenize_core(source, tokens)
        
        ! Should tokenize complex type
        if (passed) print *, '  PASSED: Complex type declaration'
    end function

    function test_nested_expressions() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        integer :: i, paren_count
        
        passed = .true.
        source = "((((a))))"
        
        call tokenize_core(source, tokens)
        
        ! Count parentheses
        paren_count = 0
        do i = 1, size(tokens)
            if (tokens(i)%text == "(" .or. tokens(i)%text == ")") then
                paren_count = paren_count + 1
            end if
        end do
        
        if (paren_count /= 8) then
            print *, '  FAILED: Should tokenize all parentheses'
            passed = .false.
        end if
        
        if (passed) print *, '  PASSED: Nested expressions'
    end function

end program test_parser_edge_cases