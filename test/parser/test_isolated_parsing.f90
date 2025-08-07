program test_isolated_parsing
    use fortfront
    use ast_core
    use parser_control_flow_module, only: parse_basic_statement_multi
    implicit none

    print *, "=== Testing parse_basic_statement_multi in isolation ==="
    call test_goto_parsing()
    call test_error_stop_parsing()

contains

    subroutine test_goto_parsing()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer, allocatable :: stmt_indices(:)
        character(len=:), allocatable :: error_msg
        
        print *, "Testing goto parsing with manually created tokens..."
        
        ! Create tokens manually: "go" "to" "100" EOF
        allocate(tokens(4))
        tokens(1)%text = "go"
        tokens(1)%kind = 5  ! TK_KEYWORD
        tokens(1)%line = 1
        tokens(1)%column = 1
        
        tokens(2)%text = "to" 
        tokens(2)%kind = 5  ! TK_KEYWORD
        tokens(2)%line = 1
        tokens(2)%column = 4
        
        tokens(3)%text = "100"
        tokens(3)%kind = 2  ! TK_INTEGER_LITERAL
        tokens(3)%line = 1
        tokens(3)%column = 7
        
        tokens(4)%text = ""
        tokens(4)%kind = 0  ! TK_EOF
        tokens(4)%line = 1
        tokens(4)%column = 10
        
        arena = create_ast_arena()
        stmt_indices = parse_basic_statement_multi(tokens, arena)
        
        print *, "Result: Got", size(stmt_indices), "statement indices"
        if (size(stmt_indices) > 0 .and. stmt_indices(1) > 0) then
            if (allocated(arena%entries(stmt_indices(1))%node)) then
                print *, "Statement type:", arena%entries(stmt_indices(1))%node_type
                if (arena%entries(stmt_indices(1))%node_type == "goto_node") then
                    print *, "SUCCESS: Manual token parsing works!"
                else
                    print *, "FAILURE: Expected goto_node, got:", arena%entries(stmt_indices(1))%node_type
                end if
            else
                print *, "FAILURE: No node allocated"
            end if
        else
            print *, "FAILURE: No statement index returned"
        end if
    end subroutine test_goto_parsing

    subroutine test_error_stop_parsing()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer, allocatable :: stmt_indices(:)
        
        print *, "Testing error stop parsing with manually created tokens..."
        
        ! Create tokens manually: "error" "stop" EOF
        allocate(tokens(3))
        tokens(1)%text = "error"
        tokens(1)%kind = 5  ! TK_KEYWORD
        tokens(1)%line = 1
        tokens(1)%column = 1
        
        tokens(2)%text = "stop"
        tokens(2)%kind = 5  ! TK_KEYWORD
        tokens(2)%line = 1
        tokens(2)%column = 7
        
        tokens(3)%text = ""
        tokens(3)%kind = 0  ! TK_EOF
        tokens(3)%line = 1
        tokens(3)%column = 12
        
        arena = create_ast_arena()
        stmt_indices = parse_basic_statement_multi(tokens, arena)
        
        print *, "Result: Got", size(stmt_indices), "statement indices"
        if (size(stmt_indices) > 0 .and. stmt_indices(1) > 0) then
            if (allocated(arena%entries(stmt_indices(1))%node)) then
                print *, "Statement type:", arena%entries(stmt_indices(1))%node_type
                if (arena%entries(stmt_indices(1))%node_type == "error_stop_node") then
                    print *, "SUCCESS: Manual error stop parsing works!"
                else
                    print *, "FAILURE: Expected error_stop_node, got:", arena%entries(stmt_indices(1))%node_type
                end if
            else
                print *, "FAILURE: No node allocated"
            end if
        else
            print *, "FAILURE: No statement index returned"
        end if
    end subroutine test_error_stop_parsing

end program test_isolated_parsing