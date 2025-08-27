program debug_parse_issue
    use frontend_parsing
    use lexer_core
    use ast_arena_modern
    implicit none
    
    character(len=:), allocatable :: test_code
    type(token_t), allocatable :: tokens(:)
    character(len=1000) :: error_msg_lex, error_msg_parse
    type(ast_arena_t) :: arena
    integer :: prog_index, i, unit_start, unit_end, loop_count
    logical :: has_explicit_program_unit
    
    ! Simple test code
    test_code = &
        'type :: person_t' // new_line('a') // &
        '    character(len=20) :: name' // new_line('a') // &
        'end type person_t' // new_line('a') // &
        '' // new_line('a') // &
        'program main' // new_line('a') // &
        '    print *, "hello"' // new_line('a') // &
        'end program main'
    
    print *, "=== Debug Parse Issue ==="
    print *, "Test code:"
    print *, test_code
    print *, ""
    
    ! Tokenize
    call tokenize_core(test_code, tokens)
    
    print *, "Tokens:"
    do i = 1, size(tokens)
        print *, i, ": ", trim(tokens(i)%text), " (", tokens(i)%kind, ")"
    end do
    print *, ""
    
    ! Manual parsing loop with debug
    has_explicit_program_unit = .true.
    i = 1
    loop_count = 0
    
    do while (i <= size(tokens))
        loop_count = loop_count + 1
        print *, "Loop iteration ", loop_count, ", i = ", i
        
        if (loop_count > 10) then
            print *, "ERROR: Too many loop iterations - infinite loop detected"
            exit
        end if
        
        ! Don't exit on first EOF - there might be more content
        if (i == size(tokens) .and. tokens(i)%kind == TK_EOF) then
            print *, "Exiting: EOF at end of tokens"
            exit
        end if
        
        ! Skip empty lines (just EOF tokens)  
        if (tokens(i)%kind == TK_EOF .and. i < size(tokens)) then
            print *, "Skipping EOF token at position ", i
            i = i + 1
            cycle
        end if
        
        print *, "Processing token at ", i, ": ", trim(tokens(i)%text)
        
        ! Find program unit boundary
        call find_program_unit_boundary(tokens, i, unit_start, unit_end, &
                                       has_explicit_program_unit)
        
        print *, "Boundary: start=", unit_start, ", end=", unit_end
        
        ! Check for valid boundary
        if (unit_end < unit_start) then
            print *, "Invalid boundary, advancing by 1"
            i = i + 1
            if (i > size(tokens)) then
                print *, "Exceeded token array bounds"
                exit
            end if
            cycle
        end if
        
        print *, "Valid boundary found, advancing to ", unit_end + 1
        i = unit_end + 1
    end do
    
    print *, "Finished parsing loop"
    
end program debug_parse_issue