program test_simple_intent
    use frontend
    use ast_core
    use ast_nodes_data, only: declaration_node
    use ast_arena
    use lexer_core, only: token_t
    implicit none

    ! Test just intent without other complexities
    character(len=*), parameter :: source = "real, intent(in) :: data"
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index, i
    character(len=:), allocatable :: error_msg
    logical :: found_data

    print *, "Testing simple intent parsing:"
    print *, source
    
    call lex_source(source, tokens, error_msg)
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, prog_index, error_msg)
    
    found_data = .false.
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            select type(node => arena%entries(i)%node)
            type is (declaration_node)
                print *, "Found declaration: '", node%var_name, "'"
                if (node%var_name == "data") then
                    found_data = .true.
                    print *, "  has_intent: ", node%has_intent
                    if (node%has_intent) print *, "  intent: '", node%intent, "'"
                end if
            end select
        end if
    end do
    
    if (found_data) then
        print *, "SUCCESS: Found data declaration"
    else
        print *, "FAIL: No data declaration found"
    end if
    
end program test_simple_intent