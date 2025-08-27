program debug_mixed_construct
    use mixed_construct_detector
    use lexer_core
    implicit none
    
    character(len=*), parameter :: test_code = &
        'type :: person_t' // new_line('a') // &
        '    integer :: age' // new_line('a') // &
        'end type' // new_line('a') // &
        'program main' // new_line('a') // &
        '    print *, "hello"' // new_line('a') // &
        'end program'
    
    type(token_t), allocatable :: tokens(:)
    type(mixed_construct_result_t) :: mixed_result
    
    print *, "Testing mixed construct detection for Issue #517"
    print *, "Input:"
    print *, test_code
    print *, ""
    
    ! Tokenize
    call tokenize_core(test_code, tokens)
    print *, "Tokens: ", size(tokens)
    
    ! Detect mixed constructs
    call detect_mixed_constructs(tokens, mixed_result)
    
    print *, "Mixed constructs detected:", mixed_result%has_mixed_constructs
    print *, "Implicit ranges:", mixed_result%num_implicit_ranges
    print *, "Explicit ranges:", mixed_result%num_explicit_ranges
    
    if (mixed_result%has_mixed_constructs) then
        print *, "SUCCESS: Mixed constructs properly detected!"
    else
        print *, "FAIL: Mixed constructs not detected"
    end if
    
end program debug_mixed_construct