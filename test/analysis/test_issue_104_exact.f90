program test_issue_104_exact
    ! Test that exactly matches the issue #104 description
    use fortfront, only: lex_source, parse_tokens, create_ast_arena, token_t, ast_arena_t
    use variable_usage_tracker_module, only: get_identifiers_in_subtree
    implicit none
    
    character(len=:), allocatable :: source_code
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    type(ast_arena_t) :: arena
    character(len=:), allocatable :: identifiers(:)
    integer :: prog_index, i, j
    logical :: test_passed
    
    test_passed = .true.
    
    ! Test case from issue description
    source_code = &
        'program test' // new_line('A') // &
        '  integer :: used_var' // new_line('A') // &
        '  used_var = 42' // new_line('A') // &
        '  print *, used_var' // new_line('A') // &
        'end program'
    
    print *, "Testing issue #104: get_identifiers_in_subtree returns empty arrays"
    print *, "Source code:"
    print *, source_code
    print *, ""
    
    ! Lex and parse
    call lex_source(source_code, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "Lexing error:", error_msg
        stop 1
    end if
    
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, prog_index, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "Parsing error:", error_msg
        stop 1
    end if
    
    ! Debug output as in issue description
    print *, "DEBUG: Arena contents:"
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            print '(a,i0,a,a)', "  Node ", i, ": ", arena%entries(i)%node_type
            
            ! Test get_identifiers_in_subtree on each node
            identifiers = get_identifiers_in_subtree(arena, i)
            
            select case (arena%entries(i)%node_type)
            case ("print_statement")
                print '(a,i0,a)', "DEBUG: print_statement_node found ", size(identifiers), " identifiers"
                if (size(identifiers) == 0) then
                    print *, "    FAIL: Expected identifiers in print statement"
                    test_passed = .false.
                else
                    do j = 1, size(identifiers)
                        print '(a,a)', "    Identifier: ", identifiers(j)
                    end do
                    if (identifiers(1) /= "used_var") then
                        print *, "    FAIL: Expected 'used_var'"
                        test_passed = .false.
                    end if
                end if
                
            case ("program")
                print '(a,i0,a)', "DEBUG: program_node found ", size(identifiers), " identifiers"
                if (size(identifiers) == 0) then
                    print *, "    FAIL: Expected identifiers in program"
                    test_passed = .false.
                end if
                
            case ("assignment")
                print '(a,i0,a)', "DEBUG: assignment_node found ", size(identifiers), " identifiers"
                ! Assignment nodes might have identifiers on RHS
                
            case default
                print '(a,i0,a)', "DEBUG: Found ", size(identifiers), " identifiers in unhandled node"
            end select
        end if
    end do
    
    print *, ""
    if (test_passed) then
        print *, "UNEXPECTED: Tests passed - issue #104 may already be fixed"
        print *, "Or the test doesn't reproduce the exact conditions"
    else
        print *, "REPRODUCED: Issue #104 - get_identifiers_in_subtree returns empty arrays"
    end if
    
    ! Additional debug: manually inspect nodes
    print *, ""
    print *, "Additional debugging:"
    
    ! Find print statement and check its structure
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            if (arena%entries(i)%node_type == "print_statement") then
                print *, "Inspecting print_statement at index", i
                block
                    use ast_nodes_io, only: print_statement_node
                    select type (node => arena%entries(i)%node)
                    type is (print_statement_node)
                        if (allocated(node%expression_indices)) then
                            print *, "  Has", size(node%expression_indices), "expressions"
                            do j = 1, size(node%expression_indices)
                                if (node%expression_indices(j) > 0) then
                                    print '(a,i0,a,i0,a,a)', "    Expr ", j, " at index ", &
                                        node%expression_indices(j), " type: ", &
                                        arena%entries(node%expression_indices(j))%node_type
                                end if
                            end do
                        else
                            print *, "  No expression indices allocated!"
                        end if
                    end select
                end block
            end if
        end if
    end do
    
end program test_issue_104_exact