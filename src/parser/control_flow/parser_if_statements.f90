module parser_if_statements_module
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_KEYWORD, TK_WHITESPACE, TK_NEWLINE, TK_EOF
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_do_constructs_module, only: parse_do_loop
    use parser_statement_detection_module, only: extend_do_statement_end
    use parser_expressions_module, only: parse_range
    use parser_statement_utilities_module, only: parse_statement_in_if_block
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_if
    use error_reporting, only: error_collection_t
    implicit none
    private

    public :: parse_if_statement_tokens

contains

    function parse_if_statement_tokens(stmt_tokens, arena, diagnostic_sink) &
            result(if_index)
        type(token_t), intent(in) :: stmt_tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        integer :: if_index
        integer :: token_count
        integer :: then_pos, else_pos, end_pos
        integer :: condition_index
        integer, allocatable :: then_body_indices(:), else_body_indices(:)

        token_count = size(stmt_tokens)
        if (token_count <= 1) then
            if_index = 0
            return
        end if
        token_count = token_count - 1

        call locate_if_statement_sections(stmt_tokens, token_count, then_pos, &
            else_pos, end_pos)
        if (then_pos < 0 .or. end_pos < 0) then
            if_index = 0
            return
        end if

        condition_index = build_if_condition(stmt_tokens, then_pos, arena, &
            diagnostic_sink)

        call parse_then_branch(stmt_tokens, then_pos, else_pos, end_pos, arena, &
            then_body_indices, diagnostic_sink)
        call parse_else_branch(stmt_tokens, else_pos, end_pos, arena, &
            else_body_indices, diagnostic_sink)

        if_index = push_if(arena, condition_index, then_body_indices, &
            else_body_indices=else_body_indices, &
            line=stmt_tokens(1)%line, column=stmt_tokens(1)%column)
    end function parse_if_statement_tokens

    subroutine locate_if_statement_sections(stmt_tokens, token_count, then_pos, &
            else_pos, end_pos)
        type(token_t), intent(in) :: stmt_tokens(:)
        integer, intent(in) :: token_count
        integer, intent(out) :: then_pos, else_pos, end_pos

        integer :: i
        character(len=:), allocatable :: lowered_text

        then_pos = -1
        else_pos = -1
        end_pos = -1

        do i = 2, token_count
            if (stmt_tokens(i)%kind /= TK_KEYWORD) cycle
            lowered_text = to_lower(stmt_tokens(i)%text)
            select case (trim(lowered_text))
            case ("then")
                if (then_pos < 0) then_pos = i
            case ("else")
                if (else_pos < 0) else_pos = i
            case ("end")
                if (i < token_count) then
                    if (stmt_tokens(i + 1)%kind == TK_KEYWORD) then
                        lowered_text = to_lower(stmt_tokens(i + 1)%text)
                        if (trim(lowered_text) == "if") then
                            end_pos = i
                            exit
                        end if
                    end if
                end if
            end select
        end do
    end subroutine locate_if_statement_sections

    integer function build_if_condition(stmt_tokens, then_pos, arena, &
            diagnostic_sink) &
            result(condition_index)
        type(token_t), intent(in) :: stmt_tokens(:)
        integer, intent(in) :: then_pos
        type(ast_arena_t), intent(inout) :: arena
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink

        integer :: condition_length
        type(token_t), allocatable, target :: condition_tokens(:)
        type(parser_state_t) :: condition_parser

        condition_index = 0
        condition_length = then_pos - 2
        if (condition_length < 1) return

        allocate (condition_tokens(condition_length + 1))
        condition_tokens(1:condition_length) = stmt_tokens(2:then_pos - 1)
        condition_tokens(condition_length + 1)%kind = TK_EOF
        condition_tokens(condition_length + 1)%text = ""
        condition_tokens(condition_length + 1)%line = stmt_tokens(2)%line
        condition_tokens(condition_length + 1)%column = stmt_tokens(2)%column

        condition_parser = create_parser_state(condition_tokens, diagnostic_sink)
        condition_index = parse_range(condition_parser, arena)
    end function build_if_condition

    subroutine parse_then_branch(stmt_tokens, then_pos, else_pos, end_pos, arena, &
            then_body_indices, diagnostic_sink)
        type(token_t), intent(in) :: stmt_tokens(:)
        integer, intent(in) :: then_pos, else_pos, end_pos
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: then_body_indices(:)
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink

        integer :: then_start, then_end

        then_start = then_pos + 1
        if (else_pos > 0) then
            then_end = else_pos - 1
        else
            then_end = end_pos - 1
        end if

        then_body_indices = parse_if_body_tokens(stmt_tokens, then_start, &
            then_end, arena, diagnostic_sink)
    end subroutine parse_then_branch

    subroutine parse_else_branch(stmt_tokens, else_pos, end_pos, arena, &
            else_body_indices, diagnostic_sink)
        type(token_t), intent(in) :: stmt_tokens(:)
        integer, intent(in) :: else_pos, end_pos
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: else_body_indices(:)
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink

        integer :: else_start, else_end

        if (else_pos <= 0) then
            allocate (else_body_indices(0))
            return
        end if

        else_start = else_pos + 1
        else_end = end_pos - 1
        else_body_indices = parse_if_body_tokens(stmt_tokens, else_start, &
            else_end, arena, diagnostic_sink)
    end subroutine parse_else_branch

    function parse_if_body_tokens(stmt_tokens, start_idx, end_idx, arena, &
            diagnostic_sink) &
            result(body_indices)
        type(token_t), intent(in) :: stmt_tokens(:)
        integer, intent(in) :: start_idx, end_idx
        type(ast_arena_t), intent(inout) :: arena
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        integer, allocatable :: body_indices(:)

        type(token_t), allocatable, target :: body_tokens(:)

        if (end_idx < start_idx) then
            allocate (body_indices(0))
            return
        end if

        call allocate_if_body_tokens(stmt_tokens, start_idx, end_idx, body_tokens)
        call parse_if_body_statements(body_tokens, arena, body_indices, &
            diagnostic_sink)
    end function parse_if_body_tokens

    subroutine allocate_if_body_tokens(stmt_tokens, start_idx, end_idx, body_tokens)
        type(token_t), intent(in) :: stmt_tokens(:)
        integer, intent(in) :: start_idx, end_idx
        type(token_t), allocatable, intent(out), target :: body_tokens(:)

        integer :: body_len

        body_len = end_idx - start_idx + 1
        if (body_len <= 0) then
            allocate (body_tokens(0))
            return
        end if

        allocate (body_tokens(body_len + 1))
        body_tokens(1:body_len) = stmt_tokens(start_idx:end_idx)
        body_tokens(body_len + 1)%kind = TK_EOF
        body_tokens(body_len + 1)%text = ""
        body_tokens(body_len + 1)%line = stmt_tokens(start_idx)%line
        body_tokens(body_len + 1)%column = stmt_tokens(start_idx)%column
    end subroutine allocate_if_body_tokens

    subroutine parse_if_body_statements(body_tokens, arena, body_indices, &
            diagnostic_sink)
        type(token_t), intent(in) :: body_tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: body_indices(:)
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink

        type(parser_state_t) :: body_parser
        integer :: stmt_start, stmt_end
        integer :: capacity, count
        integer, allocatable :: temp_indices(:)

        if (size(body_tokens) == 0) then
            allocate (body_indices(0))
            return
        end if

        body_parser = create_parser_state(body_tokens, diagnostic_sink)

        ! Pre-allocate with initial capacity to avoid O(n²) growth
        capacity = 64
        count = 0
        allocate (body_indices(capacity))

        do while (.not. body_parser%is_at_end())
            call skip_if_body_padding(body_parser)
            if (body_parser%is_at_end()) exit
            stmt_start = body_parser%current_token
            stmt_end = find_if_body_line_end(body_tokens, stmt_start)
            if (is_do_statement_start(body_tokens, stmt_start)) then
                stmt_end = extend_do_statement_end(body_tokens, stmt_start, &
                    stmt_end)
            end if
            call parse_if_body_line_efficient(body_tokens, stmt_start, stmt_end, &
                arena, body_indices, count, capacity, diagnostic_sink)
            body_parser%current_token = stmt_end + 1
        end do

        ! Trim to actual size
        if (count == 0) then
            deallocate(body_indices)
            allocate(body_indices(0))
        else if (count < capacity) then
            allocate(temp_indices(count))
            temp_indices = body_indices(1:count)
            call move_alloc(temp_indices, body_indices)
        end if
    end subroutine parse_if_body_statements

    subroutine skip_if_body_padding(body_parser)
        type(parser_state_t), intent(inout) :: body_parser
        type(token_t) :: token

        do while (.not. body_parser%is_at_end())
            token = body_parser%peek()
            if (token%kind == TK_NEWLINE .or. token%kind == TK_WHITESPACE) then
                token = body_parser%consume()
            else
                exit
            end if
        end do
    end subroutine skip_if_body_padding

    integer function find_if_body_line_end(body_tokens, stmt_start) result(stmt_end)
        type(token_t), intent(in) :: body_tokens(:)
        integer, intent(in) :: stmt_start

        integer :: i
        type(token_t) :: first_token

        stmt_end = stmt_start
        if (stmt_start > size(body_tokens)) return

        first_token = body_tokens(stmt_start)
        do i = stmt_start, size(body_tokens)
            if (body_tokens(i)%kind == TK_EOF) exit
            if (i > stmt_start) then
                if (body_tokens(i)%line /= first_token%line) exit
            end if
            stmt_end = i
        end do
    end function find_if_body_line_end

    logical function is_do_statement_start(body_tokens, stmt_start) result(is_do)
        type(token_t), intent(in) :: body_tokens(:)
        integer, intent(in) :: stmt_start

        is_do = .false.
        if (stmt_start < 1 .or. stmt_start > size(body_tokens)) return
        is_do = body_tokens(stmt_start)%kind == TK_KEYWORD .and. &
            to_lower(trim(body_tokens(stmt_start)%text)) == 'do'
    end function is_do_statement_start

    subroutine parse_if_body_line_efficient(body_tokens, stmt_start, stmt_end, &
            arena, body_indices, count, capacity, diagnostic_sink)
        type(token_t), intent(in) :: body_tokens(:)
        integer, intent(in) :: stmt_start, stmt_end
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: body_indices(:)
        integer, intent(inout) :: count, capacity
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink

        type(token_t), allocatable, target :: line_tokens(:)
        type(parser_state_t) :: line_parser
        integer :: stmt_size
        integer :: stmt_index
        integer, allocatable :: temp_indices(:)

        stmt_index = 0
        stmt_size = stmt_end - stmt_start + 1
        if (stmt_size <= 0) return

        allocate (line_tokens(stmt_size + 1))
        line_tokens(1:stmt_size) = body_tokens(stmt_start:stmt_end)
        line_tokens(stmt_size + 1)%kind = TK_EOF
        line_tokens(stmt_size + 1)%text = ""
        line_tokens(stmt_size + 1)%line = body_tokens(stmt_start)%line
        line_tokens(stmt_size + 1)%column = body_tokens(stmt_start)%column

        if (is_do_statement_start(line_tokens, 1)) then
            stmt_index = parse_do_statement_tokens(line_tokens, arena, &
                diagnostic_sink)
        else
            line_parser = create_parser_state(line_tokens, diagnostic_sink)
            call skip_if_body_line_padding(line_parser)

            if (.not. line_parser%is_at_end()) then
                stmt_index = parse_statement_in_if_block(line_parser, arena, &
                    line_parser%peek())
            end if
        end if
        if (stmt_index > 0) then
            ! Grow array if needed
            if (count >= capacity) then
                capacity = capacity * 2
                allocate(temp_indices(capacity))
                temp_indices(1:count) = body_indices(1:count)
                call move_alloc(temp_indices, body_indices)
            end if

            count = count + 1
            body_indices(count) = stmt_index
        end if
    end subroutine parse_if_body_line_efficient

    integer function parse_do_statement_tokens(stmt_tokens, arena, &
            diagnostic_sink) result(do_index)
        type(token_t), intent(in) :: stmt_tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        type(parser_state_t) :: do_parser

        if (present(diagnostic_sink)) then
            do_parser = create_parser_state(stmt_tokens, diagnostic_sink)
        else
            do_parser = create_parser_state(stmt_tokens)
        end if
        do_index = parse_do_loop(do_parser, arena)
    end function parse_do_statement_tokens

    subroutine skip_if_body_line_padding(line_parser)
        type(parser_state_t), intent(inout) :: line_parser
        type(token_t) :: token

        do while (.not. line_parser%is_at_end())
            token = line_parser%peek()
            if (token%kind == TK_WHITESPACE .or. token%kind == TK_NEWLINE) then
                token = line_parser%consume()
            else
                exit
            end if
        end do
    end subroutine skip_if_body_line_padding

end module parser_if_statements_module
