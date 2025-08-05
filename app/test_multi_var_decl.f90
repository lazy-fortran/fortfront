program test_multi_var_decl
    use fortfront, only: lex_source, parse_tokens, create_ast_arena, token_t, ast_arena_t
    use ast_nodes_data, only: declaration_node
    implicit none
    
    character(len=:), allocatable :: source_code
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    type(ast_arena_t) :: arena
    integer :: i, prog_index
    
    ! Test multi-variable declaration
    source_code = 'program test' // new_line('A') // &
                  '    real :: a = 1.0, b = 2.0, c = 3.0' // new_line('A') // &
                  'end program'
    
    print *, "Testing multi-variable declaration parsing..."
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
    
    print *, "AST nodes:"
    do i = 1, arena%size
        if (allocated(arena%entries(i)%node)) then
            print *, "  Index", i, ":", arena%entries(i)%node_type
            
            ! Check for declaration nodes
            if (arena%entries(i)%node_type == "declaration" .or. &
                arena%entries(i)%node_type == "multi_declaration") then
                print *, "    Found declaration node!"
                
                select type (node => arena%entries(i)%node)
                type is (declaration_node)
                    print *, "    Type:", node%type_name
                    print *, "    Var name:", node%var_name
                    print *, "    Is multi:", node%is_multi_declaration
                    if (node%is_multi_declaration .and. allocated(node%var_names)) then
                        block
                            integer :: j
                            print *, "    Variables:"
                            do j = 1, size(node%var_names)
                                print *, "      -", node%var_names(j)
                            end do
                        end block
                    end if
                end select
            end if
        end if
    end do
    
end program test_multi_var_decl