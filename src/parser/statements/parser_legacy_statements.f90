module parser_legacy_statements_module
    use lexer_core, only: token_t, TK_EOF, TK_NEWLINE
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_misc, only: comment_node
    use uid_generator, only: generate_uid
    implicit none
    private

    public :: parse_legacy_statement

contains

    integer function parse_legacy_statement(lowered, parser, arena) result(stmt_index)
        character(len=*), intent(in) :: lowered
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: start_pos, end_pos, i, line, column
        type(token_t) :: token, first_token
        character(len=:), allocatable :: text
        type(comment_node) :: comment

        stmt_index = 0

        select case (trim(lowered))
        case ("common", "equivalence")
            ! Legacy statements handled uniformly
        case default
            return
        end select

        if (parser%current_token > size(parser%tokens)) then
            return
        end if

        start_pos = parser%current_token
        first_token = parser%peek()
        line = first_token%line
        column = first_token%column

        end_pos = start_pos
        do i = start_pos, size(parser%tokens)
            token = parser%tokens(i)
            if (token%kind == TK_EOF .or. token%kind == TK_NEWLINE) then
                end_pos = i - 1
                exit
            end if
            end_pos = i
        end do

        if (end_pos < start_pos) end_pos = start_pos

        call reconstruct_legacy_text(parser%tokens, start_pos, end_pos, text)

        comment%text = "    " // trim(text)
        comment%line = line
        comment%column = column
        comment%uid = generate_uid()
        call arena%push(comment, "comment")
        stmt_index = arena%size

        parser%current_token = end_pos + 1
    end function parse_legacy_statement

    pure subroutine reconstruct_legacy_text(tokens, start_idx, end_idx, text)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_idx, end_idx
        character(len=:), allocatable, intent(out) :: text
        integer :: i, total_len

        if (end_idx < start_idx) then
            allocate (character(len=0) :: text)
            return
        end if

        total_len = 0
        do i = start_idx, end_idx
            total_len = total_len + len_trim(tokens(i)%text)
            if (i < end_idx) total_len = total_len + 1
        end do

        allocate (character(len=total_len) :: text)
        text = ""

        do i = start_idx, end_idx
            text = trim(text) // trim(tokens(i)%text)
            if (i < end_idx .and. len_trim(text) > 0) then
                text = trim(text) // " "
            end if
        end do
    end subroutine reconstruct_legacy_text

end module parser_legacy_statements_module
