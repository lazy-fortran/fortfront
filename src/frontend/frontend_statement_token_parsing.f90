module frontend_statement_token_parsing
    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_NEWLINE, &
        TK_OPERATOR, TK_IDENTIFIER, TK_WHITESPACE, TK_NUMBER, TK_COMMENT, to_lower
    use parser_dispatcher_module, only: parse_statement_dispatcher, &
        get_additional_indices, &
        clear_additional_indices, &
        get_last_parser_errors
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use parser_statement_label_module, only: validate_statement_label
    use ast_arena_modern, only: ast_arena_t
    use error_reporting, only: error_collection_t

    implicit none
    private

    public :: parse_explicit_program_unit
    public :: process_comment_statement
    public :: process_regular_statement
    public :: is_prefix_only_statement
    public :: clear_statement_label_error
    public :: get_statement_label_error

    character(len=:), allocatable :: captured_trailing_comment
    character(len=:), allocatable :: statement_label_error

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

        call validate_leading_statement_label(tokens, stmt_start, stmt_end, &
            diagnostic_sink)
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

    subroutine clear_statement_label_error()
        if (allocated(statement_label_error)) deallocate (statement_label_error)
    end subroutine clear_statement_label_error

    function get_statement_label_error() result(message)
        character(len=:), allocatable :: message

        if (allocated(statement_label_error)) then
            message = statement_label_error
        else
            message = ""
        end if
    end function get_statement_label_error

    ! Reject an invalid statement label before the statement is dispatched.
    ! The raw token range is used because later stages drop the label tokens.
    subroutine validate_leading_statement_label(tokens, stmt_start, stmt_end, &
            diagnostic_sink)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: stmt_start, stmt_end
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        character(len=:), allocatable :: message
        type(token_t) :: next_token
        character(len=64) :: location
        integer :: label_idx

        label_idx = stmt_start
        do while (label_idx <= stmt_end)
            if (tokens(label_idx)%kind /= TK_WHITESPACE) exit
            label_idx = label_idx + 1
        end do
        if (label_idx > stmt_end) return
        if (tokens(label_idx)%kind /= TK_NUMBER) return

        if (label_idx < stmt_end) then
            next_token = tokens(label_idx + 1)
        else
            next_token%kind = TK_EOF
            next_token%text = ""
            next_token%line = tokens(label_idx)%line
            next_token%column = tokens(label_idx)%column + &
                len_trim(tokens(label_idx)%text)
        end if

        call validate_statement_label(tokens(label_idx), next_token, message)
        if (.not. allocated(message)) return

        write (location, '(A,I0,A,I0,A)') "ERROR at line ", &
            tokens(label_idx)%line, ", column ", tokens(label_idx)%column, ":"
        if (.not. allocated(statement_label_error)) then
            statement_label_error = trim(location)//" "//message
        end if
        if (present(diagnostic_sink)) then
            call diagnostic_sink%add_error_with_token(message, tokens(label_idx))
        end if
    end subroutine validate_leading_statement_label

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
