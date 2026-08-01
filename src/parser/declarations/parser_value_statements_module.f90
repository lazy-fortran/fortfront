module parser_value_statements_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD, TK_OPERATOR, &
        TK_WHITESPACE, TK_COMMENT, TK_NEWLINE, to_lower
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node
    use ast_factory, only: push_declaration
    implicit none
    private

    public :: parse_value_statement

contains

    integer function parse_value_statement(parser, arena) result(decl_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t) :: token
        character(len=:), allocatable :: lowered_keyword, var_name
        integer :: candidate_index

        decl_index = 0
        call skip_trivia(parser)

        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%kind == TK_KEYWORD) then
                lowered_keyword = to_lower(token%text)
                if (trim(lowered_keyword) == "value") then
                    token = parser%consume()
                    call skip_trivia(parser)
                end if
            end if
        end if

        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "::") then
                token = parser%consume()
                call skip_trivia(parser)
            end if
        end if

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
                call skip_trivia(parser)
                cycle
            end if
            if (.not. token_is_identifier(token)) exit

            token = parser%consume()
            var_name = adjustl(trim(token%text))
            candidate_index = apply_value_to_variable(arena, var_name)
            if (candidate_index <= 0) then
                candidate_index = push_declaration(arena, type_name="", &
                    names=[var_name], is_value=.true., line=token%line, &
                    column=token%column)
            end if
            if (decl_index == 0 .and. candidate_index > 0) then
                decl_index = candidate_index
            end if

            call skip_trivia(parser)
            if (parser%is_at_end()) exit
            token = parser%peek()
            if (.not. (token%kind == TK_OPERATOR .and. token%text == ",")) exit
        end do
    end function parse_value_statement

    subroutine skip_trivia(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                token = parser%consume()
            case default
                exit
            end select
        end do
    end subroutine skip_trivia

    logical function token_is_identifier(token) result(is_ident)
        type(token_t), intent(in) :: token
        is_ident = (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD)
    end function token_is_identifier

    integer function apply_value_to_variable(arena, name) result(decl_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: target, decl_name
        integer :: idx, i

        decl_index = 0
        target = to_lower(adjustl(trim(name)))

        do idx = arena%size, 1, -1
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (decl => arena%entries(idx)%node)
                type is (declaration_node)
                if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                    do i = 1, size(decl%var_names)
                        if (to_lower(trim(decl%var_names(i))) /= target) cycle
                        decl%is_value = .true.
                        arena%entries(idx)%node = decl
                        decl_index = idx
                        return
                    end do
                else if (allocated(decl%var_name)) then
                    decl_name = to_lower(trim(decl%var_name))
                    if (decl_name == target) then
                        decl%is_value = .true.
                        arena%entries(idx)%node = decl
                        decl_index = idx
                        return
                    end if
                end if
            end select
        end do
    end function apply_value_to_variable

end module parser_value_statements_module
