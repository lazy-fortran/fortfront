module parser_instantiate_statement_module
    use lexer_core, only: token_t, TK_EOF, TK_NEWLINE, TK_COMMENT, TK_OPERATOR, &
        TK_IDENTIFIER, TK_KEYWORD, TK_NUMBER, TK_STRING
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_instantiate_statement
    implicit none
    private

    public :: parse_instantiate_statement

contains

    function parse_instantiate_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: spec_text
        character(len=:), allocatable :: template_name
        integer :: line, column
        logical :: have_name

        stmt_index = 0
        have_name = .false.

        token = parser%consume()
        line = token%line
        column = token%column

        spec_text = ""
        template_name = ""

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE .or. token%kind == TK_COMMENT .or. &
                token%kind == TK_EOF) then
                exit
            end if

            token = parser%consume()

            if ((token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) .and. &
                .not. have_name) then
                template_name = token%text
                have_name = .true.
            end if

            call append_instantiate_token(spec_text, token)
        end do

        stmt_index = push_instantiate_statement(arena, template_name, spec_text, &
            line, column)
    end function parse_instantiate_statement

    subroutine append_instantiate_token(spec_text, token)
        character(len=:), allocatable, intent(inout) :: spec_text
        type(token_t), intent(in) :: token
        logical :: need_space
        character(len=:), allocatable :: piece

        piece = token%text
        need_space = .false.

        if (len(spec_text) > 0) then
            need_space = token_needs_leading_space(spec_text, token)
        end if

        if (need_space) then
            spec_text = spec_text // " " // piece
        else
            spec_text = spec_text // piece
        end if
    end subroutine append_instantiate_token

    logical function token_needs_leading_space(spec_text, token) result(needs)
        character(len=*), intent(in) :: spec_text
        type(token_t), intent(in) :: token
        character(len=1) :: last_char
        needs = .false.

        if (len(spec_text) == 0) return

        last_char = spec_text(len(spec_text):len(spec_text))
        if (last_char == " " .or. last_char == "(" .or. last_char == "[" .or. &
            last_char == "{" .or. last_char == "," .or. last_char == ":" .or. &
            last_char == "%" .or. last_char == "/") then
            return
        end if

        select case (token%kind)
        case (TK_IDENTIFIER, TK_KEYWORD, TK_NUMBER, TK_STRING)
            needs = .true.
        case (TK_OPERATOR)
            needs = .false.
        case default
            needs = .false.
        end select
    end function token_needs_leading_space

end module parser_instantiate_statement_module

