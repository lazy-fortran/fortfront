program test_dead_code_detection
    use fortfront, only: lex_source, parse_tokens, analyze_semantics, &
                         create_ast_arena, create_semantic_context, token_t, &
                         ast_arena_t, semantic_context_t
    use variable_usage_tracker_module, only: get_identifiers_in_subtree
    implicit none
    
    character(len=:), allocatable :: source_code
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    type(ast_arena_t) :: arena
    type(semantic_context_t) :: semantic_ctx
    character(len=:), allocatable :: identifiers(:)
    integer :: i, prog_index
    logical :: success
    
    ! Test case from issue #99: variable usage in conditional statements
    ! Using lazy fortran (no explicit program wrapper)
    source_code = 'implicit none' // new_line('A') // &
                  'integer :: x = 5' // new_line('A') // &
                  'if (x > 0) then' // new_line('A') // &
                  '    print *, "positive"' // new_line('A') // &
                  'end if'
    
    print *, "Testing dead code detection fix..."
    print *, "Source code:"
    print *, source_code
    print *, ""
    
    ! Lex the source
    call lex_source(source_code, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "Lexing error:", error_msg
        stop 1
    end if
    
    ! Parse tokens to AST
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, prog_index, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "Parsing error:", error_msg
        stop 1
    end if
    
    ! Debug: Check AST before semantic analysis
    print *, "AST BEFORE semantic analysis:"
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            print *, "  Index", i, ":", arena%entries(i)%node_type
            if (arena%entries(i)%node_type == "if") then
                print *, "    >>> IF NODE FOUND! <<<"
            end if
        end if
    end do
    print *, ""
    
    ! Analyze semantics
    call analyze_semantics(arena, prog_index)
    
    ! Debug: print all node types in the arena
    print *, "All nodes in arena:"
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            print *, "  Index", i, ":", arena%entries(i)%node_type
            
            ! Special debug for literal nodes
            if (arena%entries(i)%node_type == "literal") then
                block
                    use ast_nodes_core, only: literal_node
                    select type (node => arena%entries(i)%node)
                    type is (literal_node)
                        print *, "    Literal value: '", node%value, "'"
                    end select
                end block
            end if
            
            ! Look for if nodes
            if (arena%entries(i)%node_type == "if") then
                print *, "    Found IF node at index", i
            end if
        end if
    end do
    print *, ""
    print *, "Total nodes:", arena%size
    
    ! Find the program node and examine its body for if statements
    success = .false.
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            if (arena%entries(i)%node_type == "program") then
                print *, "Found program node at index", i
                
                ! Debug: Check program body indices
                block
                    use ast_nodes_core, only: program_node
                    integer :: j
                    select type (node => arena%entries(i)%node)
                    type is (program_node)
                        if (allocated(node%body_indices)) then
                            print *, "Program has", size(node%body_indices), "body statements:"
                            do j = 1, size(node%body_indices)
                                if (node%body_indices(j) > 0) then
                                    print *, "  Body", j, ": index", node%body_indices(j), &
                                            "type", arena%entries(node%body_indices(j))%node_type
                                end if
                            end do
                        else
                            print *, "Program has no body statements!"
                        end if
                    end select
                end block
                
                ! Get identifiers from the entire program node (should include condition)
                identifiers = get_identifiers_in_subtree(arena, i)
                
                print *, "Identifiers found in program:"
                if (allocated(identifiers) .and. size(identifiers) > 0) then
                    block
                        integer :: j
                        do j = 1, size(identifiers)
                            print *, "  - ", identifiers(j)
                            if (identifiers(j) == "x") then
                                success = .true.
                            end if
                        end do
                    end block
                else
                    print *, "  (none)"
                end if
                exit
            end if
        end if
    end do
    
    print *, ""
    if (success) then
        print *, "SUCCESS: Variable 'x' correctly detected in conditional expression!"
        print *, "Issue #99 appears to be fixed."
    else
        print *, "FAILURE: Variable 'x' not detected in conditional expression."
        print *, "Issue #99 is NOT fixed."
        stop 1
    end if
    
end program test_dead_code_detection