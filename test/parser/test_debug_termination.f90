program test_debug_termination
    use fortfront
    use ast_core
    use ast_nodes_control
    implicit none

    print *, "=== Debugging Termination Statement Parsing ==="
    
    call test_simple_goto()
    call test_function_goto()
    call test_statement_parsing()

contains

    subroutine test_simple_goto()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, i
        
        print *, "Testing simple GOTO parsing..."
        
        ! Simplest possible case
        source = "program test" // new_line('a') // &
                "go to 10" // new_line('a') // &
                "10 continue" // new_line('a') // &
                "end program test"
        
        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "Lexer error:", error_msg
            return
        end if
        
        print *, "Tokens generated:", size(tokens)
        do i = 1, min(10, size(tokens))
            print *, "Token", i, ":", trim(tokens(i)%text), " kind:", tokens(i)%kind
        end do
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, "Parser error:", error_msg
        end if
        
        print *, "Arena size:", arena%size
        print *, "Root index:", root_index
        
        ! Debug all nodes
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                print *, "Node", i, "type:", arena%entries(i)%node_type
                if (arena%entries(i)%node_type == "goto_node") then
                    print *, "  Found goto_node!"
                    select type (node => arena%entries(i)%node)
                    type is (goto_node)
                        if (allocated(node%label)) then
                            print *, "  Label:", node%label
                        else
                            print *, "  No label allocated"
                        end if
                    end select
                end if
            end if
        end do
    end subroutine test_simple_goto

    subroutine test_function_goto()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, i
        
        print *, "Testing GOTO in function..."
        
        source = "program test" // new_line('a') // &
                "contains" // new_line('a') // &
                "function test_func()" // new_line('a') // &
                "  go to 100" // new_line('a') // &
                "100 continue" // new_line('a') // &
                "end function test_func" // new_line('a') // &
                "end program test"
        
        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "Lexer error:", error_msg
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, "Parser error:", error_msg
        end if
        
        print *, "Arena size:", arena%size, "Root index:", root_index
        
        ! Debug all nodes
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                print *, "Node", i, "type:", arena%entries(i)%node_type
                if (arena%entries(i)%node_type == "goto_node") then
                    print *, "  Found goto_node in function!"
                end if
            end if
        end do
    end subroutine test_function_goto

    subroutine test_statement_parsing()
        use parser_control_flow_module, only: parse_basic_statement_multi
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer, allocatable :: stmt_indices(:)
        integer :: i
        character(len=:), allocatable :: error_msg
        
        print *, "Testing direct statement parsing..."
        
        ! Test parse_basic_statement_multi directly
        source = "go to 100"
        call lex_source(source, tokens, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, "Lexer error:", error_msg
            return
        end if
        
        arena = create_ast_arena()
        stmt_indices = parse_basic_statement_multi(tokens, arena)
        
        print *, "Statement indices:", size(stmt_indices)
        do i = 1, size(stmt_indices)
            if (stmt_indices(i) > 0) then
                print *, "Statement", i, "index:", stmt_indices(i)
                if (allocated(arena%entries(stmt_indices(i))%node)) then
                    print *, "  Type:", arena%entries(stmt_indices(i))%node_type
                end if
            end if
        end do
    end subroutine test_statement_parsing

end program test_debug_termination