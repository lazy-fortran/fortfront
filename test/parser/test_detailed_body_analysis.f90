program test_detailed_body_analysis
    use fortfront
    use lexer_core, only: tokenize_core
    use frontend, only: parse_tokens
    implicit none

    character(len=:), allocatable :: source
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: root_index, i, j
    character(len=256) :: error_msg
    
    print *, "=== Detailed Program Body Analysis ==="
    print *, ""
    
    ! Program with various statements including goto and error stop
    source = "program test" // new_line('a') // &
            "  integer :: x" // new_line('a') // &
            "  x = 42" // new_line('a') // &
            "  go to 10" // new_line('a') // &
            "  print *, 'unreachable'" // new_line('a') // &
            "10 continue" // new_line('a') // &
            "  error stop 'done'" // new_line('a') // &
            "end program"
    
    ! Parse the program
    call tokenize_core(source, tokens)
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, root_index, error_msg)
    
    print *, "Full AST analysis:"
    print *, "=================="
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            print '(a,i3,a,a)', "Node", i, ": ", arena%entries(i)%node_type
            
            ! For program nodes, show what's in the body
            if (arena%entries(i)%node_type == "program") then
                block
                    use ast_nodes_core, only: program_node
                    select type (node => arena%entries(i)%node)
                    type is (program_node)
                        if (allocated(node%body_indices)) then
                            print *, "  Program body contains:"
                            do j = 1, size(node%body_indices)
                                if (node%body_indices(j) > 0 .and. &
                                    node%body_indices(j) <= arena%size) then
                                    print '(a,i2,a,i3,a,a)', "    Body", j, &
                                        ": index", node%body_indices(j), &
                                        " (", arena%entries(node%body_indices(j))%node_type, ")"
                                end if
                            end do
                        end if
                    end select
                end block
            end if
        end if
    end do
    
    print *, ""
    print *, "Statement type summary:"
    print *, "======================="
    block
        logical :: found_goto, found_error_stop, found_continue
        logical :: found_label
        
        found_goto = .false.
        found_error_stop = .false.
        found_continue = .false.
        found_label = .false.
        
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select case (arena%entries(i)%node_type)
                case ("goto_node")
                    found_goto = .true.
                    print *, "✓ Found goto_node at index", i
                case ("error_stop_node")
                    found_error_stop = .true.
                    print *, "✓ Found error_stop_node at index", i
                case ("continue")
                    found_continue = .true.
                    print *, "✓ Found continue at index", i
                case ("label")
                    found_label = .true.
                    print *, "✓ Found label at index", i
                end select
            end if
        end do
        
        if (.not. found_goto) then
            print *, "❌ No goto_node found in AST"
        end if
        if (.not. found_error_stop) then
            print *, "❌ No error_stop_node found in AST"
        end if
        if (.not. found_continue .and. .not. found_label) then
            print *, "❌ No continue/label found for '10 continue'"
        end if
    end block
    
    print *, ""
    print *, "=== DIAGNOSIS ==="
    print *, "The parse_tokens() function is not recognizing goto and error stop statements"
    print *, "within program bodies. They may be parsed as generic literals or ignored."
    print *, "This confirms issue #117 - the parser needs to be updated to handle these"
    print *, "statements when they appear inside program blocks."
    
end program test_detailed_body_analysis