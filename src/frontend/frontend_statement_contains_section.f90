module frontend_statement_contains_section
    use lexer_core, only: token_t, TK_COMMENT, TK_IDENTIFIER, TK_KEYWORD, &
                          TK_NEWLINE, TK_OPERATOR, TK_WHITESPACE, to_lower
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use ast_arena_modern, only: ast_arena_t
    use frontend_statement_contains_section_helpers, only: &
        push_implicit_contains_statement, scan_contains_section

    implicit none
    private

    public :: is_structural_contains
    public :: parse_implicit_contains_section

contains

    logical function is_structural_contains(tokens, stmt_start, stmt_end) &
        result(is_contains)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start, stmt_end
        integer :: idx
        character(len=:), allocatable :: lowered

        is_contains = .false.
        idx = stmt_start

        do while (idx <= stmt_end)
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE)
                idx = idx + 1
            case default
                exit
            end select
        end do

        if (idx > stmt_end) return
        if (tokens(idx)%kind /= TK_KEYWORD .and. &
            tokens(idx)%kind /= TK_IDENTIFIER) return

        lowered = to_lower(trim(tokens(idx)%text))
        if (lowered /= "contains") return

        idx = idx + 1
        do while (idx <= stmt_end)
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                idx = idx + 1
            case (TK_OPERATOR)
                if (tokens(idx)%text == "=" .or. tokens(idx)%text == "(" .or. &
                    tokens(idx)%text == "&") then
                    return
                end if
                idx = idx + 1
            case default
                return
            end select
        end do

        is_contains = .true.
    end function is_structural_contains

    subroutine parse_implicit_contains_section(tokens, start_pos, arena, &
                                               body_indices, end_pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: body_indices(:)
        integer, intent(out) :: end_pos

        type(parser_prefix_buffer_t) :: prefix_buffer
        call push_implicit_contains_statement(arena, body_indices)
        call scan_contains_section(tokens, start_pos, arena, prefix_buffer, &
                                   body_indices, end_pos)
    end subroutine parse_implicit_contains_section

end module frontend_statement_contains_section
