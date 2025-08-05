program test_if_parsing
    use fortfront, only: lex_source, parse_tokens, create_ast_arena, token_t, ast_arena_t
    implicit none
    
    character(len=:), allocatable :: source_code
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    type(ast_arena_t) :: arena
    integer :: i, prog_index
    
    ! Simple if statement
    source_code = 'program test' // new_line('A') // &
                  '    if (x > 0) then' // new_line('A') // &
                  '        print *, "positive"' // new_line('A') // &
                  '    end if' // new_line('A') // &
                  'end program'
    
    print *, "Testing if statement parsing..."
    print *, "Source code:"
    print *, source_code
    print *, ""
    
    ! Lex the source
    call lex_source(source_code, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "Lexing error:", error_msg
        stop 1
    end if
    
    print *, "Tokens:"
    do i = 1, size(tokens)
        print *, i, ": kind=", tokens(i)%kind, " text='", tokens(i)%text, "'"
    end do
    print *, ""
    
    ! Parse tokens to AST
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, prog_index, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "Parsing error:", error_msg
        stop 1
    end if
    
    print *, "AST nodes:"
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            print *, "  Index", i, ":", arena%entries(i)%node_type
        end if
    end do
    
    ! Check for if node
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            if (arena%entries(i)%node_type == "if") then
                print *, ""
                print *, "SUCCESS: If node found at index", i
                stop 0
            end if
        end if
    end do
    
    print *, ""
    print *, "FAILURE: No if node found in AST!"
    stop 1
    
end program test_if_parsing