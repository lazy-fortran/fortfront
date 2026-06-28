module parser_enum_statement_module
    ! Parse F2003 ENUM constructs into a structured enum_node:
    !   enum, bind(c)
    !       enumerator :: red, green = 5, blue
    !   end enum
    ! Enumerator values follow F2003 4.6: an explicit value sets the counter;
    ! an implicit value is the previous value plus one, starting at zero.
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_KEYWORD, TK_NEWLINE, &
        TK_COMMENT, TK_WHITESPACE, TK_OPERATOR, TK_NUMBER, to_lower
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: string_t
    use ast_factory, only: push_enum
    implicit none
    private

    public :: parse_enum_construct

contains

    integer function parse_enum_construct(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t) :: token
        type(string_t), allocatable :: names(:)
        integer, allocatable :: values(:)
        logical :: is_bind_c
        integer :: line, column, next_value

        allocate (names(0))
        allocate (values(0))
        is_bind_c = .false.
        next_value = 0

        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume() ! consume "enum"

        is_bind_c = consume_enum_header(parser)

        do while (.not. parser%is_at_end())
            call skip_trivia_and_newlines(parser)
            if (parser%is_at_end()) exit
            token = parser%peek()
            if (is_end_enum(parser)) then
                call consume_end_enum(parser)
                exit
            end if
            if ((token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER) .and. &
                to_lower(trim(token%text)) == "enumerator") then
                call parse_enumerator_line(parser, names, values, next_value)
            else
                token = parser%consume()
            end if
        end do

        stmt_index = push_enum(arena, names, values, is_bind_c, &
            line=line, column=column)
    end function parse_enum_construct

    logical function consume_enum_header(parser) result(is_bind_c)
        ! Consume the rest of the "enum[, bind(c)]" header line.
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        is_bind_c = .false.
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE .or. token%kind == TK_EOF) exit
            if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
                if (to_lower(trim(token%text)) == "bind") is_bind_c = .true.
            end if
            token = parser%consume()
        end do
    end function consume_enum_header

    subroutine parse_enumerator_line(parser, names, values, next_value)
        type(parser_state_t), intent(inout) :: parser
        type(string_t), allocatable, intent(inout) :: names(:)
        integer, allocatable, intent(inout) :: values(:)
        integer, intent(inout) :: next_value
        type(token_t) :: token
        integer :: this_value

        token = parser%consume() ! consume "enumerator"
        call skip_separator_tokens(parser) ! optional "::"

        do while (.not. parser%is_at_end())
            call skip_inline_trivia(parser)
            token = parser%peek()
            if (token%kind == TK_NEWLINE .or. token%kind == TK_EOF) exit
            if (token%kind == TK_IDENTIFIER) then
                names = [names, string_t(trim(token%text))]
                token = parser%consume()
                this_value = next_value
                call skip_inline_trivia(parser)
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. trim(token%text) == "=") then
                    token = parser%consume()
                    this_value = parse_int_value(parser)
                end if
                values = [values, this_value]
                next_value = this_value + 1
            else
                token = parser%consume()
            end if
        end do
    end subroutine parse_enumerator_line

    integer function parse_int_value(parser) result(value)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        integer :: sign_factor, ios

        value = 0
        sign_factor = 1
        call skip_inline_trivia(parser)
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. trim(token%text) == "-") then
            sign_factor = -1
            token = parser%consume()
            call skip_inline_trivia(parser)
            token = parser%peek()
        else if (token%kind == TK_OPERATOR .and. trim(token%text) == "+") then
            token = parser%consume()
            call skip_inline_trivia(parser)
            token = parser%peek()
        end if
        if (token%kind == TK_NUMBER) then
            read (token%text, *, iostat=ios) value
            if (ios /= 0) value = 0
            value = sign_factor * value
            token = parser%consume()
        end if
    end function parse_int_value

    logical function is_end_enum(parser) result(at_end)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        character(len=:), allocatable :: lowered

        at_end = .false.
        token = parser%peek()
        if (token%kind /= TK_KEYWORD) return
        lowered = to_lower(trim(token%text))
        if (lowered == "endenum") then
            at_end = .true.
        else if (lowered == "end") then
            at_end = .true.
        end if
    end function is_end_enum

    subroutine consume_end_enum(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        character(len=:), allocatable :: lowered

        token = parser%consume() ! "end" or "endenum"
        lowered = to_lower(trim(token%text))
        if (lowered == "end") then
            call skip_inline_trivia(parser)
            if (.not. parser%is_at_end()) then
                token = parser%peek()
                if (token%kind == TK_KEYWORD) then
                    if (to_lower(trim(token%text)) == "enum") token = parser%consume()
                end if
            end if
        end if
    end subroutine consume_end_enum

    subroutine skip_separator_tokens(parser)
        ! Skip whitespace and an optional "::" before the enumerator name list.
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        call skip_inline_trivia(parser)
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. trim(token%text) == "::") then
            token = parser%consume()
        end if
    end subroutine skip_separator_tokens

    subroutine skip_inline_trivia(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                token = parser%consume()
            case default
                exit
            end select
        end do
    end subroutine skip_inline_trivia

    subroutine skip_trivia_and_newlines(parser)
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
    end subroutine skip_trivia_and_newlines

end module parser_enum_statement_module
