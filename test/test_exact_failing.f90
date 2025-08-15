program test_exact_failing
    use frontend
    use ast_core
    use ast_nodes_data, only: declaration_node
    use ast_arena
    use lexer_core, only: token_t
    implicit none

    character(len=*), parameter :: source = "real(8), dimension(100), intent(in) :: data"
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index, i, decl_count
    character(len=:), allocatable :: error_msg

    print *, "Testing exact failing case:"
    print *, source
    
    call lex_source(source, tokens, error_msg)
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "Lex error: ", error_msg
            stop 1
        end if
    end if
    
    print *, "Tokens:"
    do i = 1, size(tokens)
        print *, "  ", i, ": '", tokens(i)%text, "'"
    end do
    
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, prog_index, error_msg)
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "Parse error: ", error_msg
            stop 1
        end if
    end if
    
    print *, "Parse completed. Arena size: ", arena%size
    
    decl_count = 0
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            select type(node => arena%entries(i)%node)
            type is (declaration_node)
                decl_count = decl_count + 1
                print *, "Declaration found: ", node%var_name
                print *, "  type_name: ", node%type_name
                print *, "  has_kind: ", node%has_kind
                print *, "  is_array: ", node%is_array
                print *, "  has_intent: ", node%has_intent
            class default
                print *, "Other node type at position ", i
            end select
        else
            print *, "Empty node at position ", i
        end if
    end do
    
    print *, "Total declarations: ", decl_count
    
end program test_exact_failing