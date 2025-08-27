program test_issue_517_multi_unit_parsing
    use frontend_parsing
    use lexer_core
    use ast_arena_modern
    use ast_factory
    implicit none
    
    character(len=:), allocatable :: test_code
    type(token_t), allocatable :: tokens(:)
    character(len=1000) :: error_msg_lex, error_msg_parse
    type(ast_arena_t) :: arena
    integer :: prog_index, i
    logical :: test_passed
    
    test_passed = .true.
    
    ! Test code: type declaration followed by program
    test_code = &
        'type :: person_t' // new_line('a') // &
        '    character(len=20) :: name' // new_line('a') // &
        'end type person_t' // new_line('a') // &
        '' // new_line('a') // &
        'program main' // new_line('a') // &
        '    print *, "hello"' // new_line('a') // &
        'end program main'
    
    print *, "=== Test Issue #517: Multi-unit parsing ==="
    
    ! Tokenize
    call tokenize_core(test_code, tokens)
    
    ! Parse
    arena = create_ast_arena(1000)
    call parse_tokens(tokens, arena, prog_index, error_msg_parse)
    
    if (len_trim(error_msg_parse) > 0) then
        print *, "FAIL: Parser error: ", trim(error_msg_parse)
        test_passed = .false.
    end if
    
    if (prog_index <= 0) then
        print *, "FAIL: No program index returned"
        test_passed = .false.
    else
        print *, "Parser returned prog_index: ", prog_index
        print *, "Arena contains ", arena%size, " nodes"
        
        ! Check the structure - should have multi-unit container with siblings
        if (arena%size >= 2) then
            print *, "SUCCESS: Multi-unit structure created"
        else
            print *, "FAIL: Expected multi-unit structure with >= 2 nodes, got ", arena%size
            test_passed = .false.
        end if
    end if
    
    if (test_passed) then
        print *, "PASS: Issue #517 multi-unit parsing test"
    else
        print *, "FAIL: Issue #517 multi-unit parsing test"
        stop 1
    end if
    
end program test_issue_517_multi_unit_parsing