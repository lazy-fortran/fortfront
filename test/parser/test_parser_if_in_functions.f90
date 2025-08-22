program test_parser_if_in_functions
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_tests_passed
    all_tests_passed = .true.

    call test_if_statement_in_function_body()

    if (all_tests_passed) then
        print *, "All parser if-in-functions tests PASSED!"
    else
        error stop "Some parser if-in-functions tests FAILED!"
    end if

contains

    subroutine test_if_statement_in_function_body()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_definition_statements_module, only: parse_function_definition
        use ast_core, only: ast_arena_t, create_ast_arena
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: func_index
        logical :: has_proper_if_node
        
        print *, "Testing if statement parsing in function body..."
        
        ! This test should expose the parser limitation where if statements
        ! in function bodies are parsed as literal nodes instead of if_node
        source = "function classify(x) result(category)" // new_line('a') // &
                "  integer :: x, category" // new_line('a') // &
                "  if (x < 0) then" // new_line('a') // &
                "    category = -1" // new_line('a') // &
                "    return" // new_line('a') // &
                "  end if" // new_line('a') // &
                "  category = 1" // new_line('a') // &
                "end function"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        func_index = parse_function_definition(parser, arena)
        
        if (func_index <= 0) then
            print *, "FAILED: Could not parse function"
            all_tests_passed = .false.
            return
        end if
        
        ! Check if the function body contains a proper if_node
        ! instead of a literal node with unparsed text
        has_proper_if_node = check_for_proper_if_node(arena, func_index)
        
        if (has_proper_if_node) then
            print *, "PASSED: If statement properly parsed as if_node in function body"
        else
            print *, "FAILED: If statement parsed as literal instead of if_node"
            all_tests_passed = .false.
        end if
        
    end subroutine test_if_statement_in_function_body

    ! Helper function to check if the function body contains a proper if_node
    logical function check_for_proper_if_node(arena, func_index) result(found_if_node)
        use ast_core, only: ast_arena_t
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: func_index
        integer :: i
        
        found_if_node = .false.
        
        ! Check if the function node has any children that are if_nodes
        if (func_index > 0 .and. func_index <= arena%size) then
            ! Look through the function's body indices to find if_statement types
            if (arena%entries(func_index)%node_type == "function_def") then
                ! Search through all nodes to find if_statement nodes
                ! Note: A more precise approach would access the function's body_indices
                ! directly, but this requires knowledge of the AST structure
                do i = 1, arena%size
                    if (arena%entries(i)%node_type == "if_statement") then
                        found_if_node = .true.
                        return
                    end if
                end do
            end if
        end if
    end function check_for_proper_if_node

end program test_parser_if_in_functions