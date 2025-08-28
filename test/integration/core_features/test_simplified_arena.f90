program test_simplified_arena
    use fortfront
    implicit none
    
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    integer :: prog_index, i
    character(len=:), allocatable :: error_msg
    character(len=*), parameter :: source = "x = 42"
    
    print *, "=== Testing simplified arena access ==="
    
    ! Lex
    call lex_source(source, tokens, error_msg)
    print *, "Tokens:", size(tokens)
    
    ! Parse
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, prog_index, error_msg)
    print *, "Parse complete, prog_index:", prog_index
    print *, "Arena size:", arena%size
    
    ! Dump all nodes
    print *, "All nodes in arena:"
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            select type (node => arena%entries(i)%node)
            type is (program_node)
                print *, "  Index", i, ": program_node"
                if (allocated(node%body_indices)) then
                    print *, "    Body indices:", node%body_indices
                end if
            type is (assignment_node)
                print *, "  Index", i, ": assignment_node"
            type is (identifier_node)
                print *, "  Index", i, ": identifier_node '", node%name, "'"
            type is (literal_node)
                print *, "  Index", i, ": literal_node"
            class default
                print *, "  Index", i, ": other node"
            end select
        else
            print *, "  Index", i, ": <empty>"
        end if
    end do
    
    ! Try find_nodes_by_type
    block
        integer, allocatable :: assign_nodes(:)
        assign_nodes = find_nodes_by_type(arena, "assignment")
        print *, "Found", size(assign_nodes), "assignment nodes via find_nodes_by_type"
    end block
    
end program test_simplified_arena