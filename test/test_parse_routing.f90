program test_parse_routing
    use frontend
    use ast_core
    use ast_arena
    use lexer_core, only: token_t
    implicit none

    character(len=*), parameter :: sources(2) = [ &
        "real, allocatable :: data  ", &
        "real, intent :: data       " &
    ]
    
    integer :: test_num
    
    do test_num = 1, 2
        call test_single(sources(test_num))
    end do
    
contains
    
    subroutine test_single(source)
        character(len=*), intent(in) :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index, i
        character(len=:), allocatable :: error_msg

        print *, "=== Testing: ", trim(source), " ==="
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  Lex error: ", error_msg
                return
            end if
        end if
        print *, "  Tokens: ", size(tokens)
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  Parse error: ", error_msg
                return
            end if
        end if
        print *, "  Arena size: ", arena%size
        
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                print *, "  Node ", i, ": allocated"
            else
                print *, "  Node ", i, ": empty"
            end if
        end do
        print *, ""
        
    end subroutine test_single
    
end program test_parse_routing