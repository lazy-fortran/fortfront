program test_program_body_parsing_issue
    ! This test documents the actual issue: parse_tokens() doesn't parse program bodies
    use fortfront
    use lexer_core, only: tokenize_core
    use frontend, only: parse_tokens
    implicit none

    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: root_index, i
    character(len=256) :: error_msg
    logical :: has_body
    
    print *, "=== Demonstrating Program Body Parsing Issue ==="
    print *, ""
    
    ! Simple program with body statements
    source = "program test" // new_line('a') // &
            "  integer :: x" // new_line('a') // &
            "  x = 42" // new_line('a') // &
            "  go to 10" // new_line('a') // &
            "  print *, 'unreachable'" // new_line('a') // &
            "10 continue" // new_line('a') // &
            "  error stop 'done'" // new_line('a') // &
            "end program"
    
    print *, "Source code:"
    print *, source
    print *, ""
    
    ! Parse the program
    call tokenize_core(source, tokens)
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, root_index, error_msg)
    
    print *, "Parsing result:"
    print *, "  Root index:", root_index
    if (len_trim(error_msg) > 0) then
        print *, "  Error:", error_msg
    end if
    print *, ""
    
    ! Examine what was actually parsed
    print *, "AST contents:"
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            print *, "  Node", i, ":", arena%entries(i)%node_type
            
            ! Check if program has body
            if (arena%entries(i)%node_type == "program") then
                block
                    use ast_nodes_core, only: program_node
                    select type (node => arena%entries(i)%node)
                    type is (program_node)
                        if (allocated(node%body_indices)) then
                            if (size(node%body_indices) > 0) then
                                print *, "    Program has", size(node%body_indices), "body statements"
                                has_body = .true.
                            else
                                print *, "    Program body is empty array"
                                has_body = .false.
                            end if
                        else
                            print *, "    Program has no body array allocated"
                            has_body = .false.
                        end if
                    end select
                end block
            end if
        end if
    end do
    
    print *, ""
    print *, "=== ISSUE CONFIRMED ==="
    if (.not. has_body) then
        print *, "❌ Program body is not being parsed"
        print *, "   This is why goto_node and error_stop_node don't appear in full programs"
        print *, "   The frontend parse_tokens() function needs to be fixed to parse program bodies"
    else
        print *, "✓ Program body is being parsed (issue may be fixed)"
    end if
    print *, ""
    print *, "=== WORKAROUND ==="
    print *, "Use parse_statement_dispatcher() directly for individual statements"
    print *, "This bypasses the broken program body parsing"
    
    ! Demonstrate the workaround
    block
        use parser_dispatcher_module, only: parse_statement_dispatcher
        type(token_t), allocatable :: goto_tokens(:)
        type(ast_arena_t) :: stmt_arena
        integer :: stmt_index
        
        print *, ""
        print *, "Demonstrating workaround with 'go to 10':"
        
        allocate(goto_tokens(4))
        goto_tokens(1)%kind = 5  ! TK_KEYWORD
        goto_tokens(1)%text = "go"
        goto_tokens(2)%kind = 5  ! TK_KEYWORD
        goto_tokens(2)%text = "to"
        goto_tokens(3)%kind = 2  ! TK_NUMBER
        goto_tokens(3)%text = "10"
        goto_tokens(4)%kind = 0  ! TK_EOF
        
        stmt_arena = create_ast_arena()
        stmt_index = parse_statement_dispatcher(goto_tokens, stmt_arena)
        
        if (stmt_index > 0) then
            print *, "✓ Statement parsed successfully"
            print *, "  Node type:", stmt_arena%entries(stmt_index)%node_type
        else
            print *, "❌ Statement parsing failed"
        end if
    end block
    
end program test_program_body_parsing_issue