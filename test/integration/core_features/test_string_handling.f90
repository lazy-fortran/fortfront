program test_string_handling
    use lexer_core, only: token_t, tokenize_core, TK_STRING
    use string_types
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== String Handling Tests ==='
    print *

    ! Test string operations
    print *, 'Testing string operations...'
    if (.not. test_string_escapes()) all_passed = .false.
    if (.not. test_string_concatenation()) all_passed = .false.
    if (.not. test_empty_strings()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All string handling tests passed!'
        stop 0
    else
        print *, 'Some string handling tests failed!'
        stop 1
    end if

contains

    function test_string_escapes() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        
        passed = .true.
        
        ! Test escaped quotes
        source = "s = 'Don''t forget' // ""Double """"quotes""""""" 
        
        call tokenize_core(source, tokens)
        
        ! Should properly handle escapes
        if (size(tokens) < 5) then
            print *, '  FAILED: Not enough tokens for escaped strings'
            passed = .false.
        end if
        
        if (passed) print *, '  PASSED: String escapes'
    end function

    function test_string_concatenation() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        integer :: i, concat_count
        
        passed = .true.
        
        source = "s = 'Hello' // ' ' // 'World'"
        
        call tokenize_core(source, tokens)
        
        ! Count concatenation operators
        concat_count = 0
        do i = 1, size(tokens)
            if (tokens(i)%text == "//") concat_count = concat_count + 1
        end do
        
        if (concat_count /= 2) then
            print *, '  FAILED: Expected 2 concat operators, got', concat_count
            passed = .false.
        end if
        
        if (passed) print *, '  PASSED: String concatenation'
    end function

    function test_empty_strings() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        integer :: i, empty_count
        
        passed = .true.
        
        source = "s1 = '' // s2 = """""
        
        call tokenize_core(source, tokens)
        
        ! Count empty string tokens
        empty_count = 0
        do i = 1, size(tokens)
            if (tokens(i)%kind == TK_STRING .and. &
                (tokens(i)%text == "''" .or. tokens(i)%text == '""')) then
                empty_count = empty_count + 1
            end if
        end do
        
        if (empty_count < 2) then
            print *, '  FAILED: Expected 2 empty strings, got', empty_count
            passed = .false.
        end if
        
        if (passed) print *, '  PASSED: Empty strings'
    end function

end program test_string_handling