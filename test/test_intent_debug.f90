program test_intent_debug
    use frontend
    use ast_core
    use ast_nodes_data, only: declaration_node
    use ast_arena
    use lexer_core, only: token_t
    implicit none

    character(len=*), parameter :: source = "real, intent(in) :: data"
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index, i
    character(len=:), allocatable :: error_msg

    print *, "Testing intent with full debugging:"
    print *, source
    print *, ""
    
    call lex_source(source, tokens, error_msg)
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "Tokenization error: ", error_msg
            stop 1
        end if
    end if
    print *, "Tokenization successful, got ", size(tokens), " tokens"
    
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, prog_index, error_msg)
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "Parse error: ", error_msg
            stop 1
        end if
    end if
    print *, "Parsing completed, arena size: ", arena%size
    
    print *, "All AST nodes:"
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            select type(node => arena%entries(i)%node)
            type is (declaration_node)
                print *, "  Declaration node: var_name='", node%var_name, "'"
                print *, "    type_name='", node%type_name, "'"
                print *, "    has_intent=", node%has_intent
                if (node%has_intent .and. allocated(node%intent)) then
                    print *, "    intent='", node%intent, "'"
                end if
            class default
                print *, "  Other node type"
            end select
        else
            print *, "  Empty node at position ", i
        end if
    end do
    
end program test_intent_debug