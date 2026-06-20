module parser_inline_instantiation_module
    use lexer_core, only: token_t
    use lexer_token_types, only: TK_OPERATOR
    use parser_state_module, only: parser_state_t
    use parser_token_views_module, only: token_view_t, view_peek_token, &
                                         view_consume_token
    implicit none
    private

    public :: consume_inline_instantiation

contains

    subroutine consume_inline_instantiation(parser, inst_text, view)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: inst_text
        type(token_view_t), intent(in), optional :: view

        if (present(view)) then
            call consume_inline_instantiation_core(parser, inst_text, view)
        else
            call consume_inline_instantiation_core(parser, inst_text)
        end if
    end subroutine consume_inline_instantiation

    subroutine consume_inline_instantiation_core(parser, inst_text, view)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: inst_text
        type(token_view_t), intent(in), optional :: view
        type(token_t) :: token
        type(token_t), allocatable :: tokens(:)
        integer :: count

        if (allocated(inst_text)) deallocate (inst_text)

        token = peek(parser, view)

        if (token%kind /= TK_OPERATOR) return

        if (token%text == "{") then
            call scan_balanced(parser, "{", "}", "braces", tokens, count, view)
        else if (token%text == "^") then
            call scan_caret(parser, tokens, count, view)
        else
            return
        end if

        if (count > 0) call join_tokens(tokens, count, inst_text)
    end subroutine consume_inline_instantiation_core

    ! Caret form name^(T): consume the caret, then scan the balanced (...) group.
    subroutine scan_caret(parser, tokens, count, view)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), allocatable, intent(out) :: tokens(:)
        integer, intent(out) :: count
        type(token_view_t), intent(in), optional :: view
        type(token_t) :: token, caret_token
        type(token_t), allocatable :: arg_tokens(:)
        integer :: arg_count

        count = 0
        caret_token = consume(parser, view)

        token = peek(parser, view)
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            call parser%errors%add_error_with_token( &
                "Caret inline instantiation must be followed by ( ...)", &
                caret_token, suggestion="Use name^(type-list)(...)")
            return
        end if

        call scan_balanced(parser, "(", ")", "parentheses", arg_tokens, &
                           arg_count, view)
        if (arg_count == 0) return

        allocate (tokens(arg_count + 1))
        tokens(1) = caret_token
        tokens(2:arg_count + 1) = arg_tokens(1:arg_count)
        count = arg_count + 1
    end subroutine scan_caret

    ! Collect tokens of a balanced open/close group starting at the open token.
    ! On unbalanced input reports an error and leaves count == 0.
    subroutine scan_balanced(parser, open_ch, close_ch, group_name, tokens, &
                             count, view)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: open_ch, close_ch, group_name
        type(token_t), allocatable, intent(out) :: tokens(:)
        integer, intent(out) :: count
        type(token_view_t), intent(in), optional :: view
        type(token_t) :: token, start_token
        integer :: nesting

        start_token = peek(parser, view)
        nesting = 0
        count = 0
        allocate (tokens(16))
        do while (.not. parser%is_at_end())
            token = consume(parser, view)

            if (token%kind == TK_OPERATOR) then
                if (token%text == open_ch) nesting = nesting + 1
                if (token%text == close_ch) nesting = nesting - 1
            end if

            call append_token(tokens, count, token)

            if (nesting == 0) exit
        end do

        if (nesting /= 0) then
            count = 0
            call parser%errors%add_error_with_token( &
                "Unbalanced inline instantiation "//group_name//": expected "// &
                close_ch//" before end of file", start_token, &
                suggestion="Add a matching "//close_ch// &
                " to close the instantiation")
        end if
    end subroutine scan_balanced

    subroutine append_token(tokens, count, token)
        type(token_t), allocatable, intent(inout) :: tokens(:)
        integer, intent(inout) :: count
        type(token_t), intent(in) :: token
        type(token_t), allocatable :: tmp(:)

        if (count == size(tokens)) then
            allocate (tmp(size(tokens) * 2))
            tmp(1:count) = tokens(1:count)
            call move_alloc(tmp, tokens)
        end if
        count = count + 1
        tokens(count) = token
    end subroutine append_token

    subroutine join_tokens(tokens, count, inst_text)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: count
        character(len=:), allocatable, intent(out) :: inst_text
        integer :: total_len, pos, i

        total_len = 0
        do i = 1, count
            total_len = total_len + len(tokens(i)%text)
        end do

        allocate (character(len=total_len) :: inst_text)
        pos = 1
        do i = 1, count
            inst_text(pos:pos + len(tokens(i)%text) - 1) = tokens(i)%text
            pos = pos + len(tokens(i)%text)
        end do
    end subroutine join_tokens

    function peek(parser, view) result(token)
        type(parser_state_t), intent(inout) :: parser
        type(token_view_t), intent(in), optional :: view
        type(token_t) :: token

        if (present(view)) then
            token = view_peek_token(view, parser)
        else
            token = parser%peek()
        end if
    end function peek

    function consume(parser, view) result(token)
        type(parser_state_t), intent(inout) :: parser
        type(token_view_t), intent(in), optional :: view
        type(token_t) :: token

        if (present(view)) then
            token = view_consume_token(view, parser)
        else
            token = parser%consume()
        end if
    end function consume

end module parser_inline_instantiation_module
