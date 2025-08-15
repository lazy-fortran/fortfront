program test_dimension_basic
    use frontend
    use ast_core
    use ast_nodes_data, only: declaration_node
    use ast_arena
    use lexer_core, only: token_t
    implicit none

    character(len=*), parameter :: source = "real, dimension(5) :: data"
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index, i
    character(len=:), allocatable :: error_msg
    integer :: decl_count

    print *, "Testing dimension attribute:"
    print *, source
    
    call lex_source(source, tokens, error_msg)
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, prog_index, error_msg)
    
    decl_count = 0
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            select type(node => arena%entries(i)%node)
            type is (declaration_node)
                decl_count = decl_count + 1
                print *, "Declaration found: ", node%var_name, " array=", node%is_array
            end select
        end if
    end do
    
    print *, "Total declarations found: ", decl_count
    
end program test_dimension_basic