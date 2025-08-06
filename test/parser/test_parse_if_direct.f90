program test_parse_if_direct
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_tests_passed
    all_tests_passed = .true.

    call test_direct_if_parsing()

    if (all_tests_passed) then
        print *, "All direct if parsing tests PASSED!"
    else
        error stop "Some direct if parsing tests FAILED!"
    end if

contains

    subroutine test_direct_if_parsing()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_control_flow_module, only: parse_if
        use ast_core, only: ast_arena_t, create_ast_arena
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: if_index
        
        print *, "Testing direct if statement parsing..."
        
        ! Test parsing a simple if statement directly
        source = "if (x < 0) then" // new_line('a') // &
                "  y = -1" // new_line('a') // &
                "end if"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        if_index = parse_if(parser, arena)
        
        if (if_index <= 0) then
            print *, "FAILED: Could not parse if statement"
            all_tests_passed = .false.
            return
        end if
        
        ! Check if we actually got an if_node
        if (if_index > 0 .and. if_index <= arena%size) then
            if (arena%entries(if_index)%node_type == "if_statement" .or. &
                arena%entries(if_index)%node_type == "if_node") then
                print *, "PASSED: If statement parsed as", trim(arena%entries(if_index)%node_type)
            else
                print *, "FAILED: If statement parsed as:", trim(arena%entries(if_index)%node_type)
                all_tests_passed = .false.
            end if
        else
            print *, "FAILED: Invalid if_index returned"
            all_tests_passed = .false.
        end if
        
    end subroutine test_direct_if_parsing

end program test_parse_if_direct