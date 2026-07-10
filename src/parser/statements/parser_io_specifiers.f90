module parser_io_specifiers
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD, TK_OPERATOR, &
        TK_WHITESPACE, TK_NEWLINE, TK_COMMENT, to_lower
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_comparison
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_io, only: io_specifier_t
    implicit none
    private

    public :: parse_io_specifier_nodes

contains

    subroutine parse_io_specifier_nodes(parser, arena, positional_names, &
            specifiers)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: positional_names(:)
        type(io_specifier_t), allocatable, intent(out) :: specifiers(:)
        type(token_t) :: token
        type(io_specifier_t) :: specifier
        integer :: position

        allocate (specifiers(0))
        position = 0
        do while (.not. parser%is_at_end())
            call skip_separators(parser)
            token = parser%peek()
            if (is_specifier_end(token)) exit
            call parse_one_specifier(parser, arena, positional_names, &
                position, specifier)
            specifiers = [specifiers, specifier]
        end do
    end subroutine parse_io_specifier_nodes

    subroutine parse_one_specifier(parser, arena, positional_names, position, &
            specifier)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: positional_names(:)
        integer, intent(inout) :: position
        type(io_specifier_t), intent(out) :: specifier
        integer :: first_token, last_token

        call parse_specifier_name(parser, positional_names, position, &
            specifier%name)
        first_token = parser%current_token
        specifier%value_node_index = parse_specifier_value(parser, arena)
        last_token = parser%current_token - 1
        specifier%value = compact_token_text(parser, first_token, last_token)
    end subroutine parse_one_specifier

    subroutine parse_specifier_name(parser, positional_names, position, name)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: positional_names(:)
        integer, intent(inout) :: position
        character(len=:), allocatable, intent(out) :: name
        type(parser_state_t) :: checkpoint
        type(token_t) :: token

        checkpoint = parser
        token = checkpoint%peek()
        if (is_name_token(token)) then
            token = checkpoint%consume()
            if (is_equals_token(checkpoint%peek())) then
                name = trim(to_lower(token%text))
                token = checkpoint%consume()
                parser = checkpoint
                return
            end if
        end if
        position = position + 1
        if (position <= size(positional_names)) then
            name = trim(positional_names(position))
        else
            name = ''
        end if
    end subroutine parse_specifier_name

    integer function parse_specifier_value(parser, arena) result(node_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t) :: token
        integer :: start_token

        start_token = parser%current_token
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == '*') then
            token = parser%consume()
            node_index = 0
            return
        end if
        node_index = parse_comparison(parser, arena)
        if (parser%current_token == start_token) token = parser%consume()
    end function parse_specifier_value

    subroutine skip_separators(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do
            token = parser%peek()
            if (token%kind == TK_WHITESPACE) then
                token = parser%consume()
            else if (token%kind == TK_OPERATOR .and. token%text == ',') then
                token = parser%consume()
            else
                exit
            end if
        end do
    end subroutine skip_separators

    logical function is_name_token(token)
        type(token_t), intent(in) :: token

        is_name_token = token%kind == TK_IDENTIFIER .or. &
            token%kind == TK_KEYWORD
    end function is_name_token

    logical function is_equals_token(token)
        type(token_t), intent(in) :: token

        is_equals_token = token%kind == TK_OPERATOR .and. token%text == '='
    end function is_equals_token

    logical function is_specifier_end(token)
        type(token_t), intent(in) :: token

        is_specifier_end = token%kind == TK_NEWLINE .or. &
            token%kind == TK_COMMENT
        if (token%kind == TK_OPERATOR) then
            is_specifier_end = is_specifier_end .or. token%text == ')' .or. &
                token%text == ';'
        end if
    end function is_specifier_end

    function compact_token_text(parser, first_token, last_token) result(text)
        type(parser_state_t), intent(in) :: parser
        integer, intent(in) :: first_token, last_token
        character(len=:), allocatable :: text
        integer :: i

        text = ''
        if (.not. associated(parser%tokens)) return
        do i = first_token, min(last_token, size(parser%tokens))
            if (parser%tokens(i)%kind == TK_WHITESPACE) cycle
            text = text//parser%tokens(i)%text
        end do
    end function compact_token_text

end module parser_io_specifiers
