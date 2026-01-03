module parser_call_module
    ! Shared call-statement parser used across control-flow helpers
    use lexer_core, only: token_t
    use lexer_token_types, only: TK_IDENTIFIER, TK_OPERATOR, TK_KEYWORD
    use parser_inline_instantiation_module, only: consume_inline_instantiation
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_range
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_subroutine_call, push_literal
    use ast_types, only: LITERAL_STRING
    implicit none
    private

    public :: parse_call_statement

contains

    subroutine parse_call_arguments(parser, arena, arg_indices)
        use ast_nodes_core, only: assignment_node
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: arg_indices(:)
        type(token_t) :: token
        integer :: arg_index

        allocate (arg_indices(0))
        token = parser%consume()  ! consume left parenthesis

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            end if

            arg_index = parse_range(parser, arena)
            if (arg_index > 0) then
                ! Mark assignment nodes as keyword arguments
                if (arg_index <= arena%size) then
                    if (allocated(arena%entries(arg_index)%node)) then
                        select type (node_ptr => arena%entries(arg_index)%node)
                        type is (assignment_node)
                            ! Directly modify the node in place
                            node_ptr%is_keyword_argument = .true.
                        end select
                    end if
                end if
                arg_indices = [arg_indices, arg_index]
            end if

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
            else if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            else
                exit
            end if
        end do
    end subroutine parse_call_arguments

    function parse_call_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: subroutine_name
        character(len=:), allocatable :: instantiation_text
        integer, allocatable :: arg_indices(:)
        integer :: line, column

        stmt_index = 0

        token = parser%consume()
        line = token%line
        column = token%column

        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER) then
            stmt_index = push_literal(arena, &
                                      "! Error: expected subroutine name after "// &
                                      "call", &
                                      LITERAL_STRING, line, column)
            return
        end if

        subroutine_name = token%text
        token = parser%consume()

        do while (.not. parser%is_at_end())
            call consume_inline_instantiation(parser, instantiation_text)
            if (.not. allocated(instantiation_text)) exit
            subroutine_name = subroutine_name // instantiation_text
            deallocate (instantiation_text)
        end do

        ! Handle component access: call foo%bar%baz(...) or call arr(1)%method()
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "%") then
                token = parser%consume()
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
                    subroutine_name = subroutine_name // "%" // token%text
                    token = parser%consume()
                    do while (.not. parser%is_at_end())
                        call consume_inline_instantiation(parser, instantiation_text)
                        if (.not. allocated(instantiation_text)) exit
                        subroutine_name = subroutine_name // instantiation_text
                        deallocate (instantiation_text)
                    end do
                else
                    exit
                end if
            else if (token%kind == TK_OPERATOR .and. token%text == "{") then
                call consume_inline_instantiation(parser, instantiation_text)
                if (allocated(instantiation_text)) then
                    subroutine_name = subroutine_name // instantiation_text
                    deallocate (instantiation_text)
                end if
            else if (token%kind == TK_OPERATOR .and. token%text == "(") then
                ! This might be array indices (arr(i)%method) or final arguments
                ! Parse the parenthesized expression
                block
                    integer :: saved_pos
                    integer :: paren_depth
                    character(len=:), allocatable :: index_text

                    saved_pos = parser%current_token
                    token = parser%consume()
                    paren_depth = 1
                    index_text = "("

                    do while (.not. parser%is_at_end() .and. paren_depth > 0)
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR) then
                            if (token%text == "(") then
                                paren_depth = paren_depth + 1
                            else if (token%text == ")") then
                                paren_depth = paren_depth - 1
                            end if
                        end if
                        if (paren_depth > 0) then
                            index_text = index_text // trim(token%text)
                        end if
                        token = parser%consume()
                    end do
                    index_text = index_text // ")"

                    ! Check if followed by %
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == "%") then
                        ! This was array indexing, include in name
                        subroutine_name = subroutine_name // index_text
                    else
                        ! This is the final argument list, restore position
                        parser%current_token = saved_pos
                        exit
                    end if
                end block
            else
                exit
            end if
        end do

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            call parse_call_arguments(parser, arena, arg_indices)
        else
            allocate (arg_indices(0))
        end if

        stmt_index = push_subroutine_call(arena, subroutine_name, arg_indices, &
                                          line, column)
    end function parse_call_statement

end module parser_call_module
