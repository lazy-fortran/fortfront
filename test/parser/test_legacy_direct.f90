program test_legacy_direct
    use lexer_core
    use parser_state_module
    use parser_expressions_module
    use ast_core
    use codegen_core
    implicit none
    
    character(len=*), parameter :: source = "(/ 1, 2, 3, 4 /)"
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg, result
    type(parser_state_t) :: parser
    type(ast_arena_t) :: arena
    integer :: expr_index, i
    
    ! Tokenize just the expression
    call tokenize_core(source, tokens)
    
    print *, "=== TOKENS ==="
    do i = 1, size(tokens)
        print '(A,I0,A,A,A,I0)', &
            "Token ", i, ": '", trim(tokens(i)%text), &
            "' kind=", tokens(i)%kind
    end do
    
    ! Parse the expression directly
    arena = create_ast_arena()
    parser = create_parser_state(tokens)
    expr_index = parse_primary(parser, arena)
    
    print *, ""
    print *, "=== Parsed Expression Index: ", expr_index
    
    ! Generate code for the expression
    if (expr_index > 0) then
        ! Check node type
        block
            use ast_core, only: array_literal_node, literal_node
            select type (node => arena%entries(expr_index)%node)
            type is (array_literal_node)
                print *, "=== Node is array_literal_node"
                print *, "=== Syntax style: ", node%syntax_style
                print *, "=== Number of elements: ", size(node%element_indices)
                
                ! Check each element
                block
                    integer :: j
                    do j = 1, size(node%element_indices)
                        print '(A,I0,A,I0)', "===   Element ", j, " index: ", node%element_indices(j)
                        if (node%element_indices(j) > 0 .and. node%element_indices(j) <= arena%size) then
                            select type (elem => arena%entries(node%element_indices(j))%node)
                            type is (literal_node)
                                print '(A,I0,A,A)', "===     Element ", j, " is literal: ", elem%value
                            class default
                                print '(A,I0,A)', "===     Element ", j, " is not a literal"
                                ! Try to generate code for this element to see what it is
                                block
                                    character(len=:), allocatable :: elem_code
                                    elem_code = generate_code_from_arena(arena, node%element_indices(j))
                                    print '(A,I0,A,A)', "===       Generated for element ", j, ": ", elem_code
                                end block
                            end select
                        end if
                    end do
                end block
            class default
                print *, "=== Node is not array_literal_node"
            end select
        end block
        
        result = generate_code_from_arena(arena, expr_index)
        print *, "=== Generated: ", result
    else
        print *, "=== Failed to parse expression"
    end if
    
end program test_legacy_direct