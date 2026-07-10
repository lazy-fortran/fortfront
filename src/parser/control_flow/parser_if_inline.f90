module parser_if_inline_module
    ! Parser module for inline IF statements (without THEN keyword)
    !
    ! This module handles the special case of single-line IF statements:
    !   IF (condition) statement
    ! as opposed to block IF constructs with THEN/ENDIF.
    !
    ! ISO/IEC 1539-1:2018 Section 11.1.8.3 specifies the IF-stmt syntax:
    !   IF (scalar-logical-expr) action-stmt
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF, TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, &
        TK_COMMENT, TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t
    use parser_statement_core_module, only: statement_callbacks_t, &
        find_statement_end, &
        allocate_stmt_tokens_with_eof, &
        parse_basic_statement_core
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_if
    implicit none
    private

    public :: parse_inline_if
    public :: handle_eof_inline_if
    public :: detect_malformed_block_if
    public :: check_newline_continuation
    public :: parse_inline_if_body
    public :: skip_inline_if_leading_tokens

contains

    ! Parse inline IF (no THEN keyword)
    function parse_inline_if(parser, arena, condition_index, if_token, &
            then_token, parent_index, callbacks) result(if_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: condition_index
        type(token_t), intent(in) :: if_token
        type(token_t), intent(in) :: then_token
        integer, intent(in), optional :: parent_index
        type(statement_callbacks_t), intent(in) :: callbacks
        integer :: if_index

        integer, allocatable :: then_body_indices(:), else_body_indices(:)
        integer, allocatable :: elseif_indices(:)
        logical :: inline_has_continuation

        ! Handle EOF case
        if (then_token%kind == TK_EOF .or. parser%is_at_end()) then
            if_index = handle_eof_inline_if(parser, condition_index)
            return
        end if

        ! Check if this looks like a malformed multi-line if construct
        inline_has_continuation = .false.
        if (detect_malformed_block_if(parser, inline_has_continuation)) then
            write (error_unit, '(A)') &
                "  Suggestion: Use 'IF (condition) THEN' for multi-line blocks"
            call parser%error( &
                "IF construct Missing 'then' keyword (e.g., 'if x > 0' needs 'then')", &
                "Use 'IF (condition) THEN' for multi-line blocks")
            if_index = 0
            return
        end if

        ! Valid one-line if statement (no then keyword)
        allocate (then_body_indices(1))
        then_body_indices(1) = 0
        call parse_inline_if_body(parser, arena, inline_has_continuation, &
            then_body_indices, callbacks)

        ! Create if node with no elseif/else blocks
        allocate (elseif_indices(0))
        allocate (else_body_indices(0))
        if_index = push_if(arena, condition_index, then_body_indices, &
            elseif_indices=elseif_indices, &
            else_body_indices=else_body_indices, &
            line=if_token%line, column=if_token%column, &
            parent_index=parent_index)
    end function parse_inline_if

    ! Handle EOF case for inline IF
    function handle_eof_inline_if(parser, condition_index) result(if_index)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(in) :: condition_index
        integer :: if_index

        if_index = 0
        if (condition_index > 0) then
            write (error_unit, '(A)') &
                "  Suggestion: Use 'IF (condition) THEN' for multi-line blocks"
            call parser%error( &
                "IF construct Missing 'then' keyword (e.g., 'if x > 0' needs 'then')", &
                "Use 'IF (condition) THEN' for multi-line blocks")
        end if
    end function handle_eof_inline_if

    ! Detect if current position looks like a malformed block IF
    function detect_malformed_block_if(parser, inline_has_continuation) &
            result(is_malformed)
        type(parser_state_t), intent(in) :: parser
        logical, intent(inout) :: inline_has_continuation
        logical :: is_malformed

        type(token_t) :: check_tok
        integer :: check_idx

        is_malformed = .false.
        check_idx = parser%current_token

        do while (check_idx <= size(parser%tokens))
            check_tok = parser%tokens(check_idx)

            ! Check for line continuation character
            if (check_tok%kind == TK_OPERATOR .and. check_tok%text == "&") then
                inline_has_continuation = .true.
                check_idx = check_idx + 1
                cycle
            end if

            ! If we hit a newline, peek ahead to see if there's code after it
            if (check_tok%kind == TK_NEWLINE) then
                is_malformed = check_newline_continuation(parser%tokens, check_idx, &
                    inline_has_continuation)
                exit
            end if

            ! Skip whitespace and comments
            if (check_tok%kind == TK_WHITESPACE .or. &
                check_tok%kind == TK_COMMENT) then
                check_idx = check_idx + 1
                cycle
            end if

            ! Check for end if or endif later in the code
            if (check_tok%kind == TK_KEYWORD) then
                if (to_lower(check_tok%text) == "end" .or. &
                    to_lower(check_tok%text) == "endif") then
                    is_malformed = .true.
                    exit
                end if
            end if

            ! Found a non-whitespace token on same line, might be valid one-liner
            exit
        end do
    end function detect_malformed_block_if

    ! Check if code after newline indicates continuation or block IF
    function check_newline_continuation(tokens, check_idx, inline_has_continuation) &
            result(looks_like_block_if)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: check_idx
        logical, intent(inout) :: inline_has_continuation
        logical :: looks_like_block_if

        integer :: peek_idx
        type(token_t) :: peek_tok
        logical :: found_code_after_newline

        looks_like_block_if = .false.
        found_code_after_newline = .false.
        peek_idx = check_idx + 1

        ! Skip any following whitespace/comments/newlines
        do while (peek_idx <= size(tokens))
            peek_tok = tokens(peek_idx)
            if (peek_tok%kind == TK_WHITESPACE .or. &
                peek_tok%kind == TK_COMMENT .or. &
                peek_tok%kind == TK_NEWLINE) then
                peek_idx = peek_idx + 1
                cycle
            end if
            ! Found a non-trivia token
            if (peek_tok%kind /= TK_EOF) then
                found_code_after_newline = .true.
            end if
            exit
        end do

        if (found_code_after_newline) then
            inline_has_continuation = .true.
        end if

        ! If theres code immediately after newline, its likely a continued inline IF
        ! Otherwise, it looks like a block IF
        if (.not. found_code_after_newline .and. .not. inline_has_continuation) then
            looks_like_block_if = .true.
        end if
    end function check_newline_continuation

    ! Parse the body of an inline IF statement
    subroutine parse_inline_if_body(parser, arena, inline_has_continuation, &
            then_body_indices, callbacks)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(in) :: inline_has_continuation
        integer, allocatable, intent(inout) :: then_body_indices(:)
        type(statement_callbacks_t), intent(in) :: callbacks

        integer :: stmt_start, stmt_end
        integer, allocatable :: stmt_indices(:)
        type(token_t), allocatable :: stmt_tokens(:)
        type(token_t) :: tok

        call skip_inline_if_leading_tokens(parser, inline_has_continuation)
        stmt_start = parser%current_token
        if (stmt_start <= size(parser%tokens)) then
            stmt_end = find_statement_end(parser%tokens, stmt_start)
            if (stmt_end < stmt_start) stmt_end = stmt_start
            call allocate_stmt_tokens_with_eof(stmt_tokens, parser%tokens, &
                stmt_start, stmt_end)

            stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                callbacks=callbacks, parent_parser=parser)
            if (allocated(stmt_indices)) then
                if (size(stmt_indices) > 0 .and. stmt_indices(1) > 0) then
                    then_body_indices(1) = stmt_indices(1)
                end if
            end if

            if (stmt_end < size(parser%tokens)) then
                parser%current_token = stmt_end + 1
            else
                parser%current_token = size(parser%tokens)
            end if
        end if

        ! Advance parser to end of statement to prevent re-parsing
        do while (.not. parser%is_at_end())
            tok = parser%peek()
            if (tok%kind == TK_NEWLINE .or. tok%kind == TK_EOF) exit
            tok = parser%consume()
        end do
    end subroutine parse_inline_if_body

    subroutine skip_inline_if_leading_tokens(parser, allow_newlines)
        type(parser_state_t), intent(inout) :: parser
        logical, intent(in) :: allow_newlines
        type(token_t) :: tok

        do while (.not. parser%is_at_end())
            tok = parser%peek()
            select case (tok%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                tok = parser%consume()
            case (TK_NEWLINE)
                if (allow_newlines) then
                    tok = parser%consume()
                else
                    return
                end if
            case (TK_OPERATOR)
                if (tok%text == "&") then
                    tok = parser%consume()
                else
                    return
                end if
            case default
                return
            end select
        end do
    end subroutine skip_inline_if_leading_tokens

end module parser_if_inline_module
