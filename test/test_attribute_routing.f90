program test_attribute_routing
    use frontend
    use ast_core
    use ast_nodes_data, only: declaration_node
    use ast_arena
    use lexer_core, only: token_t
    implicit none

    character(len=*), parameter :: sources(4) = [ &
        "real, allocatable :: data  ", &
        "real, pointer :: data      ", &
        "real, target :: data       ", &
        "real, intent :: data       " &
    ]
    
    integer :: test_num
    
    do test_num = 1, 4
        call test_single(sources(test_num))
    end do
    
contains
    
    subroutine test_single(source)
        character(len=*), intent(in) :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index, i, decl_count
        character(len=:), allocatable :: error_msg

        print *, "Testing: ", trim(source)
        
        call lex_source(source, tokens, error_msg)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        
        decl_count = 0
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type(node => arena%entries(i)%node)
                type is (declaration_node)
                    decl_count = decl_count + 1
                end select
            end if
        end do
        
        print *, "  -> Declarations found: ", decl_count
        print *, ""
        
    end subroutine test_single
    
end program test_attribute_routing