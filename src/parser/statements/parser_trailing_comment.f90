module parser_trailing_comment_module
    ! Capture an inline (trailing) comment that follows a just-parsed statement
    ! or construct header, storing it on the statement/construct node so code
    ! generation can re-emit it on the same line.
    use lexer_core, only: token_t, TK_EOF, TK_NEWLINE, TK_COMMENT, TK_WHITESPACE
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    implicit none
    private

    public :: capture_trailing_comment
    public :: capture_trailing_comment_from_tokens

contains

    subroutine capture_trailing_comment_from_tokens(tokens, arena, stmt_idx)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_idx
        integer :: i

        if (stmt_idx <= 0 .or. stmt_idx > arena%size) return
        if (.not. arena%has_node_at(stmt_idx)) return

        do i = size(tokens), 1, -1
            select case (tokens(i)%kind)
            case (TK_EOF, TK_NEWLINE, TK_WHITESPACE)
                cycle
            case (TK_COMMENT)
                if (allocated(tokens(i)%text)) then
                    arena%entries(stmt_idx)%node%trailing_comment = tokens(i)%text
                end if
                return
            case default
                return
            end select
        end do
    end subroutine capture_trailing_comment_from_tokens

    subroutine capture_trailing_comment(parser, arena, stmt_idx)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_idx
        type(token_t) :: next_tok, discarded
        integer :: stmt_line, last_ws_line

        if (stmt_idx <= 0 .or. stmt_idx > arena%size) return
        if (.not. arena%has_node_at(stmt_idx)) return
        stmt_line = arena%entries(stmt_idx)%node%line
        last_ws_line = stmt_line

        do while (.not. parser%is_at_end())
            next_tok = parser%peek()
            if (next_tok%kind == TK_EOF) return
            if (next_tok%kind == TK_NEWLINE) return
            if (next_tok%kind == TK_WHITESPACE) then
                last_ws_line = next_tok%line
                discarded = parser%consume()
                cycle
            end if
            if (next_tok%kind /= TK_COMMENT) return
            if (next_tok%line == stmt_line .or. next_tok%line == last_ws_line) then
                if (allocated(next_tok%text)) then
                    arena%entries(stmt_idx)%node%trailing_comment = next_tok%text
                end if
                discarded = parser%consume()
            end if
            return
        end do
    end subroutine capture_trailing_comment

end module parser_trailing_comment_module
