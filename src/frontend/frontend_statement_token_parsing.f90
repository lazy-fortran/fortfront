module frontend_statement_token_parsing
    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_NEWLINE, &
        TK_OPERATOR, TK_IDENTIFIER, TK_WHITESPACE, TK_NUMBER, TK_COMMENT, to_lower
    use parser_dispatcher_module, only: parse_statement_dispatcher, &
        get_additional_indices, &
        clear_additional_indices, &
        get_last_parser_errors
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use ast_arena_modern, only: ast_arena_t
    use error_reporting, only: error_collection_t
    use parser_label_validation_module, only: validate_label_context, &
        is_statement_label_text

    implicit none
    private

    public :: parse_explicit_program_unit
    public :: process_comment_statement
    public :: process_regular_statement
    public :: is_prefix_only_statement

    character(len=:), allocatable :: captured_trailing_comment

contains

    subroutine process_comment_statement(tokens, i, arena, prefix_buffer, stmt_index, &
            body_indices, diagnostic_sink)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: i
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer, intent(out) :: stmt_index
        integer, allocatable, intent(inout) :: body_indices(:)
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        type(token_t), allocatable, target :: stmt_tokens(:)

        allocate (stmt_tokens(2))
        stmt_tokens(1) = tokens(i)
        stmt_tokens(2)%kind = TK_EOF
        stmt_tokens(2)%text = ""
        stmt_tokens(2)%line = tokens(i)%line
        stmt_tokens(2)%column = tokens(i)%column + len(tokens(i)%text)

        stmt_index = parse_statement_dispatcher(stmt_tokens, arena, prefix_buffer, &
            diagnostic_sink)
        if (stmt_index > 0) then
            body_indices = [body_indices, stmt_index]
        end if
    end subroutine process_comment_statement

    subroutine process_regular_statement(tokens, stmt_start, stmt_end, arena, &
            prefix_buffer, stmt_index, body_indices, diagnostic_sink)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start, stmt_end
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer, intent(out) :: stmt_index
        integer, allocatable, intent(inout) :: body_indices(:)
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        type(token_t), allocatable, target :: stmt_tokens(:)
        integer :: effective_start

        if (stmt_end < stmt_start) then
            stmt_index = 0
            return
        end if

        call check_label_separator(tokens, stmt_start, stmt_end)
        effective_start = compute_effective_statement_start(tokens, stmt_start, &
            stmt_end)
        call build_statement_tokens(tokens, effective_start, stmt_end, stmt_tokens)
        call capture_trailing_comment(stmt_tokens, arena, 0)
        call strip_trailing_comment_from_tokens(stmt_tokens)
        call parse_statement_tokens_with_optional_label(stmt_tokens, arena, &
            prefix_buffer, stmt_index, diagnostic_sink)
        if (stmt_index <= 0) return

        call attach_captured_trailing_comment(arena, stmt_index)
        body_indices = [body_indices, stmt_index]
        call append_additional_indices_from_dispatcher(body_indices)
    end subroutine process_regular_statement

    ! A free-form statement label is separated from the statement by blanks;
    ! a digit string used as a construct name (`10: a=10`) is invalid.
    subroutine check_label_separator(tokens, stmt_start, stmt_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start
        integer, intent(in) :: stmt_end
        integer :: i, label_idx

        label_idx = 0
        do i = stmt_start, stmt_end
            if (tokens(i)%kind == TK_WHITESPACE) cycle
            if (tokens(i)%kind == TK_NUMBER) label_idx = i
            exit
        end do

        if (label_idx == 0) return
        if (.not. is_statement_label_text(tokens(label_idx)%text)) return

        do i = label_idx + 1, stmt_end
            if (tokens(i)%kind == TK_WHITESPACE) cycle
            if (tokens(i)%kind /= TK_OPERATOR) return
            if (tokens(i)%text /= ":") return
            call validate_label_context(tokens(label_idx)%text, .true., .true., &
                tokens(label_idx)%line, tokens(label_idx)%column)
            return
        end do
    end subroutine check_label_separator

    integer function compute_effective_statement_start(tokens, stmt_start, stmt_end) &
            result(effective_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start
        integer, intent(in) :: stmt_end
        integer :: i, colon_pos, eq_pos, paren_depth
        logical :: saw_keyword

        effective_start = stmt_start
        colon_pos = 0
        eq_pos = 0
        saw_keyword = .false.
        paren_depth = 0

        do i = stmt_start, stmt_end
            if (tokens(i)%kind == TK_KEYWORD) saw_keyword = .true.
            if (tokens(i)%kind == TK_OPERATOR) then
                if (tokens(i)%text == "=") then
                    eq_pos = i
                    exit
                end if
            end if
        end do

        if (eq_pos <= 0) return

        do i = stmt_start, eq_pos - 1
            if (tokens(i)%kind /= TK_OPERATOR) cycle
            select case (tokens(i)%text)
            case ("(")
                paren_depth = paren_depth + 1
            case (")")
                if (paren_depth > 0) paren_depth = paren_depth - 1
            case (":")
                if (paren_depth == 0) then
                    colon_pos = i
                    exit
                end if
            end select
        end do

        if (colon_pos > 0 .and. .not. saw_keyword) then
            effective_start = colon_pos + 1
        end if
    end function compute_effective_statement_start

    subroutine build_statement_tokens(tokens, stmt_start, stmt_end, stmt_tokens)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start
        integer, intent(in) :: stmt_end
        type(token_t), allocatable, intent(out), target :: stmt_tokens(:)

        allocate (stmt_tokens(stmt_end - stmt_start + 2))
        stmt_tokens(1:stmt_end - stmt_start + 1) = tokens(stmt_start:stmt_end)
        stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
        stmt_tokens(stmt_end - stmt_start + 2)%text = ""
        stmt_tokens(stmt_end - stmt_start + 2)%line = tokens(stmt_end)%line
        stmt_tokens(stmt_end - stmt_start + 2)%column = tokens(stmt_end)%column + 1
    end subroutine build_statement_tokens

    subroutine parse_statement_tokens_with_optional_label(stmt_tokens, arena, &
            prefix_buffer, stmt_index, diagnostic_sink)
        type(token_t), allocatable, intent(inout), target :: stmt_tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer, intent(out) :: stmt_index
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        character(len=:), allocatable :: stmt_label
        integer :: label_end_idx, i
        type(token_t), allocatable :: tokens_without_label(:)

        label_end_idx = 0
        do i = 1, size(stmt_tokens)
            if (stmt_tokens(i)%kind == TK_WHITESPACE) cycle
            if (stmt_tokens(i)%kind == TK_NUMBER) then
                stmt_label = trim(stmt_tokens(i)%text)
                label_end_idx = i
            end if
            exit
        end do

        if (label_end_idx > 0) then
            call check_statement_label(stmt_tokens, label_end_idx, stmt_label)
            allocate (tokens_without_label(size(stmt_tokens) - label_end_idx))
            tokens_without_label = stmt_tokens(label_end_idx + 1:size(stmt_tokens))
            call move_alloc(tokens_without_label, stmt_tokens)
        end if

        stmt_index = parse_statement_dispatcher(stmt_tokens, arena, prefix_buffer, &
            diagnostic_sink)
        if (stmt_index <= 0 .or. .not. allocated(stmt_label)) return

        if (stmt_index <= arena%size) then
            if (allocated(arena%entries(stmt_index)%node)) then
                arena%entries(stmt_index)%node%stmt_label = stmt_label
            end if
        end if
    end subroutine parse_statement_tokens_with_optional_label

    ! Validate the digits of a leading statement label and require that the
    ! label is attached to a statement.
    subroutine check_statement_label(stmt_tokens, label_end_idx, stmt_label)
        type(token_t), intent(in) :: stmt_tokens(:)
        integer, intent(in) :: label_end_idx
        character(len=*), intent(in) :: stmt_label
        logical :: has_statement
        integer :: i

        has_statement = .false.
        do i = label_end_idx + 1, size(stmt_tokens)
            select case (stmt_tokens(i)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT, TK_EOF)
                cycle
            case default
                has_statement = .true.
                exit
            end select
        end do

        call validate_label_context(stmt_label, has_statement, .false., &
            stmt_tokens(label_end_idx)%line, stmt_tokens(label_end_idx)%column)
    end subroutine check_statement_label

    subroutine append_additional_indices_from_dispatcher(body_indices)
        integer, allocatable, intent(inout) :: body_indices(:)
        integer, allocatable :: extra_indices(:)

        extra_indices = get_additional_indices()
        if (size(extra_indices) > 0) then
            body_indices = [body_indices, extra_indices]
        end if
        call clear_additional_indices()
    end subroutine append_additional_indices_from_dispatcher

    function parse_explicit_program_unit(tokens, arena, error_msg, diagnostic_sink) &
            result(prog_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        character(len=:), allocatable, intent(out), optional :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        integer :: prog_index
        type(parser_prefix_buffer_t) :: prefix_buffer
        prog_index = parse_statement_dispatcher(tokens, arena, prefix_buffer, &
            diagnostic_sink, error_msg)
    end function parse_explicit_program_unit

    logical function is_prefix_only_statement(tokens, start_idx, end_idx) &
            result(is_prefix)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_idx, end_idx
        integer :: idx
        character(len=:), allocatable :: lowered

        is_prefix = .false.
        if (start_idx < 1 .or. end_idx < start_idx) return

        do idx = start_idx, end_idx
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE)
                cycle
            case (TK_IDENTIFIER)
                lowered = to_lower(tokens(idx)%text)
                select case (trim(lowered))
                case ('elemental', 'pure', 'impure', 'recursive', 'module', &
                        'nonrecursive', 'non_recursive')
                    is_prefix = .true.
                case default
                    is_prefix = .false.
                    return
                end select
            case default
                is_prefix = .false.
                return
            end select
        end do
    end function is_prefix_only_statement

    subroutine strip_trailing_comment_from_tokens(stmt_tokens)
        type(token_t), intent(inout) :: stmt_tokens(:)
        integer :: i

        do i = size(stmt_tokens) - 1, 2, -1
            if (stmt_tokens(i)%kind == TK_COMMENT) then
                stmt_tokens(i)%kind = TK_EOF
                stmt_tokens(i)%text = ''
                return
            end if
            if (stmt_tokens(i)%kind == TK_EOF) exit
            if (stmt_tokens(i)%kind == TK_WHITESPACE) cycle
            exit
        end do
    end subroutine strip_trailing_comment_from_tokens

    subroutine capture_trailing_comment(stmt_tokens, arena, node_index)
        type(token_t), intent(in) :: stmt_tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        integer :: i
        character(len=:), allocatable :: comment_text

        comment_text = ''
        do i = size(stmt_tokens), 1, -1
            if (stmt_tokens(i)%kind == TK_COMMENT) then
                if (allocated(stmt_tokens(i)%text)) then
                    comment_text = stmt_tokens(i)%text
                end if
                exit
            end if
            if (stmt_tokens(i)%kind == TK_EOF) cycle
            exit
        end do

        if (allocated(captured_trailing_comment)) deallocate (captured_trailing_comment)
        if (len_trim(comment_text) > 0) then
            captured_trailing_comment = comment_text
        end if
    end subroutine capture_trailing_comment

    subroutine attach_captured_trailing_comment(arena, node_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. arena%has_node_at(node_index)) return
        if (.not. allocated(captured_trailing_comment)) return
        if (len_trim(captured_trailing_comment) == 0) return

        arena%entries(node_index)%node%trailing_comment = captured_trailing_comment
        deallocate (captured_trailing_comment)
    end subroutine attach_captured_trailing_comment

end module frontend_statement_token_parsing
