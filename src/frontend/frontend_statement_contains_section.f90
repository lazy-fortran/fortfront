module frontend_statement_contains_section
    use lexer_core, only: token_t, TK_COMMENT, TK_EOF, TK_IDENTIFIER, TK_KEYWORD, &
                          TK_NEWLINE, TK_NUMBER, TK_OPERATOR, TK_WHITESPACE, to_lower
    use parser_definition_statements_module, only: parse_function_definition, &
                                                   parse_subroutine_definition
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use parser_state_module, only: create_parser_state, parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_misc, only: contains_node

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

    subroutine push_implicit_contains_statement(arena, body_indices)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: body_indices(:)
        type(contains_node) :: contains_stmt

        contains_stmt%line = 0
        contains_stmt%column = 0
        call arena%push(contains_stmt, "contains", 0)
        body_indices = [body_indices, arena%size]
    end subroutine push_implicit_contains_statement

    subroutine scan_contains_section(tokens, start_pos, arena, prefix_buffer, &
                                     body_indices, end_pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer, allocatable, intent(inout) :: body_indices(:)
        integer, intent(out) :: end_pos
        integer :: i
        character(len=:), allocatable :: lowered
        character(len=16), allocatable :: prefix_list(:)

        i = start_pos
        end_pos = size(tokens)
        allocate (prefix_list(0))

        do while (i <= size(tokens))
            if (tokens(i)%kind == TK_EOF) exit

            if (token_is_ignorable(tokens(i)%kind)) then
                i = i + 1
                cycle
            end if

            if (.not. token_is_word(tokens(i)%kind)) then
                i = i + 1
                cycle
            end if

            lowered = to_lower(trim(tokens(i)%text))

            if (handle_contains_end(lowered, i, end_pos)) exit

            if (handle_contains_proc_prefix(lowered, i, prefix_list)) cycle

            if (is_contains_type_prefix_keyword(lowered)) then
                call handle_type_prefixed_contains(tokens, i, arena, prefix_buffer, &
                                                   prefix_list, body_indices)
                cycle
            end if

            if (handle_contains_procedure_keyword(tokens, lowered, i, arena, &
                                                  prefix_buffer, prefix_list, &
                                                  body_indices)) cycle

            i = i + 1
        end do

        if (allocated(prefix_list)) deallocate (prefix_list)
    end subroutine scan_contains_section

    logical function token_is_ignorable(kind) result(ignorable)
        integer, intent(in) :: kind
        ignorable = (kind == TK_WHITESPACE .or. kind == TK_NEWLINE .or. &
                     kind == TK_COMMENT)
    end function token_is_ignorable

    logical function token_is_word(kind) result(is_word)
        integer, intent(in) :: kind
        is_word = (kind == TK_KEYWORD .or. kind == TK_IDENTIFIER)
    end function token_is_word

    logical function handle_contains_end(lowered, pos, end_pos) result(should_end)
        character(len=*), intent(in) :: lowered
        integer, intent(in) :: pos
        integer, intent(out) :: end_pos

        should_end = (lowered == "end")
        if (should_end) end_pos = pos
    end function handle_contains_end

    logical function handle_contains_proc_prefix(lowered, pos, prefix_list) &
        result(handled)
        character(len=*), intent(in) :: lowered
        integer, intent(inout) :: pos
        character(len=16), allocatable, intent(inout) :: prefix_list(:)

        handled = .false.
        if (.not. is_contains_proc_prefix_keyword(lowered)) return
        call append_contains_prefix(prefix_list, lowered)
        pos = pos + 1
        handled = .true.
    end function handle_contains_proc_prefix

    subroutine append_contains_prefix(prefix_list, lowered)
        character(len=16), allocatable, intent(inout) :: prefix_list(:)
        character(len=*), intent(in) :: lowered
        prefix_list = [prefix_list, adjustl(trim(lowered))]
    end subroutine append_contains_prefix

    logical function handle_contains_procedure_keyword(tokens, lowered, pos, arena, &
                                                       prefix_buffer, prefix_list, &
                                                       body_indices) result(handled)
        type(token_t), intent(in) :: tokens(:)
        character(len=*), intent(in) :: lowered
        integer, intent(inout) :: pos
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), allocatable, intent(inout) :: prefix_list(:)
        integer, allocatable, intent(inout) :: body_indices(:)

        integer :: proc_end, proc_start

        handled = .false.

        if (lowered == "function") then
            proc_start = pos
            call find_procedure_end(tokens, proc_start, "function", proc_end)
            call parse_contains_function_span(tokens, proc_start, proc_end, arena, &
                                              prefix_buffer, prefix_list, body_indices)
            pos = proc_end + 1
            handled = .true.
            return
        end if

        if (lowered == "subroutine") then
            proc_start = pos
            call find_procedure_end(tokens, proc_start, "subroutine", proc_end)
            call parse_contains_subroutine_span(tokens, proc_start, proc_end, arena, &
                                                prefix_buffer, prefix_list, &
                                                body_indices)
            pos = proc_end + 1
            handled = .true.
            return
        end if
    end function handle_contains_procedure_keyword

    subroutine handle_type_prefixed_contains(tokens, pos, arena, prefix_buffer, &
                                             prefix_list, body_indices)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: pos
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), allocatable, intent(inout) :: prefix_list(:)
        integer, allocatable, intent(inout) :: body_indices(:)

        integer :: proc_end
        logical :: parsed_procedure

        call try_parse_typed_function_in_contains(tokens, pos, arena, &
                                                  prefix_buffer, prefix_list, &
                                                  body_indices, parsed_procedure, &
                                                  proc_end)
        if (parsed_procedure) then
            pos = proc_end + 1
            return
        end if

        call skip_type_spec(tokens, pos)
    end subroutine handle_type_prefixed_contains

    logical function is_contains_proc_prefix_keyword(lowered) result(is_prefix)
        character(len=*), intent(in) :: lowered
        is_prefix = (lowered == "pure" .or. lowered == "elemental" .or. &
                     lowered == "impure" .or. lowered == "recursive" .or. &
                     lowered == "module" .or. lowered == "nonrecursive")
    end function is_contains_proc_prefix_keyword

    logical function is_contains_type_prefix_keyword(lowered) result(is_type_prefix)
        character(len=*), intent(in) :: lowered
        is_type_prefix = (lowered == "integer" .or. lowered == "real" .or. &
                          lowered == "logical" .or. lowered == "character" .or. &
                          lowered == "complex" .or. lowered == "double" .or. &
                          lowered == "type" .or. lowered == "class")
    end function is_contains_type_prefix_keyword

    subroutine reset_contains_prefix_list(prefix_list)
        character(len=16), allocatable, intent(inout) :: prefix_list(:)
        if (allocated(prefix_list)) deallocate (prefix_list)
        allocate (prefix_list(0))
    end subroutine reset_contains_prefix_list

    subroutine try_parse_typed_function_in_contains(tokens, type_start, arena, &
                                                    prefix_buffer, prefix_list, &
                                                    body_indices, parsed, proc_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: type_start
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), allocatable, intent(inout) :: prefix_list(:)
        integer, allocatable, intent(inout) :: body_indices(:)
        logical, intent(out) :: parsed
        integer, intent(out) :: proc_end
        integer :: proc_start
        character(len=:), allocatable :: lowered

        parsed = .false.
        proc_end = type_start
        proc_start = find_proc_keyword_after_type(tokens, type_start)
        if (proc_start <= 0) return

        lowered = to_lower(trim(tokens(proc_start)%text))
        if (lowered /= "function") return

        call find_procedure_end(tokens, proc_start, "function", proc_end)
        call parse_contains_function_span(tokens, type_start, proc_end, arena, &
                                          prefix_buffer, prefix_list, body_indices)
        parsed = .true.
    end subroutine try_parse_typed_function_in_contains

    subroutine parse_contains_function_span(tokens, span_start, proc_end, arena, &
                                            prefix_buffer, prefix_list, body_indices)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: span_start
        integer, intent(in) :: proc_end
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), allocatable, intent(inout) :: prefix_list(:)
        integer, allocatable, intent(inout) :: body_indices(:)
        type(parser_state_t) :: parser
        type(token_t), allocatable :: proc_tokens(:)
        integer :: proc_index

        allocate (proc_tokens(proc_end - span_start + 2))
        proc_tokens(1:proc_end - span_start + 1) = tokens(span_start:proc_end)
        proc_tokens(proc_end - span_start + 2)%kind = TK_EOF
        proc_tokens(proc_end - span_start + 2)%text = ""
        call prefix_buffer%clear()
        parser = create_parser_state(proc_tokens)

        if (size(prefix_list) > 0) then
            proc_index = parse_function_definition(parser, arena, prefix_buffer, &
                                                   prefix_list)
        else
            proc_index = parse_function_definition(parser, arena, prefix_buffer)
        end if

        if (proc_index > 0) body_indices = [body_indices, proc_index]
        deallocate (proc_tokens)
        call reset_contains_prefix_list(prefix_list)
    end subroutine parse_contains_function_span

    subroutine parse_contains_subroutine_span(tokens, proc_start, proc_end, arena, &
                                              prefix_buffer, prefix_list, &
                                              body_indices)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: proc_start
        integer, intent(in) :: proc_end
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), allocatable, intent(inout) :: prefix_list(:)
        integer, allocatable, intent(inout) :: body_indices(:)
        type(parser_state_t) :: parser
        type(token_t), allocatable :: proc_tokens(:)
        integer :: proc_index

        allocate (proc_tokens(proc_end - proc_start + 2))
        proc_tokens(1:proc_end - proc_start + 1) = tokens(proc_start:proc_end)
        proc_tokens(proc_end - proc_start + 2)%kind = TK_EOF
        proc_tokens(proc_end - proc_start + 2)%text = ""
        call prefix_buffer%clear()
        parser = create_parser_state(proc_tokens)

        if (size(prefix_list) > 0) then
            proc_index = parse_subroutine_definition(parser, arena, prefix_buffer, &
                                                     prefix_list)
        else
            proc_index = parse_subroutine_definition(parser, arena, prefix_buffer)
        end if

        if (proc_index > 0) body_indices = [body_indices, proc_index]
        deallocate (proc_tokens)
        call reset_contains_prefix_list(prefix_list)
    end subroutine parse_contains_subroutine_span

    integer function find_proc_keyword_after_type(tokens, start_pos) result(proc_pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer :: pos, paren_depth
        character(len=:), allocatable :: lowered

        proc_pos = 0
        pos = start_pos

        pos = pos + 1

        do while (pos <= size(tokens))
            select case (tokens(pos)%kind)
            case (TK_WHITESPACE)
                pos = pos + 1
            case (TK_KEYWORD, TK_IDENTIFIER)
                lowered = to_lower(trim(tokens(pos)%text))
                if (lowered == "function" .or. lowered == "subroutine") then
                    proc_pos = pos
                    return
                else if (lowered == "precision" .or. lowered == "complex") then
                    pos = pos + 1
                else
                    pos = pos + 1
                end if
            case (TK_OPERATOR)
                if (tokens(pos)%text == "(") then
                    paren_depth = 1
                    pos = pos + 1
                    do while (pos <= size(tokens) .and. paren_depth > 0)
                        if (tokens(pos)%kind == TK_OPERATOR) then
                            if (tokens(pos)%text == "(") paren_depth = paren_depth + 1
                            if (tokens(pos)%text == ")") paren_depth = paren_depth - 1
                        end if
                        pos = pos + 1
                    end do
                else if (tokens(pos)%text == "*") then
                    pos = pos + 1
                    if (pos <= size(tokens)) then
                        if (tokens(pos)%kind == TK_NUMBER) pos = pos + 1
                    end if
                else
                    pos = pos + 1
                end if
            case (TK_NEWLINE, TK_COMMENT, TK_EOF)
                return
            case default
                pos = pos + 1
            end select
        end do
    end function find_proc_keyword_after_type

    subroutine skip_type_spec(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: pos
        integer :: paren_depth
        character(len=:), allocatable :: lowered

        pos = pos + 1

        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_WHITESPACE) pos = pos + 1
            if (pos <= size(tokens)) then
                if (tokens(pos)%kind == TK_KEYWORD .or. &
                    tokens(pos)%kind == TK_IDENTIFIER) then
                    lowered = to_lower(trim(tokens(pos)%text))
                    if (lowered == "precision" .or. lowered == "complex") then
                        pos = pos + 1
                    end if
                end if
            end if
        end if

        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_WHITESPACE) pos = pos + 1
            if (pos <= size(tokens)) then
                if (tokens(pos)%kind == TK_OPERATOR) then
                    if (tokens(pos)%text == "(") then
                        paren_depth = 1
                        pos = pos + 1
                        do while (pos <= size(tokens) .and. paren_depth > 0)
                            if (tokens(pos)%kind == TK_OPERATOR) then
                                if (tokens(pos)%text == "(") paren_depth = &
                                    paren_depth + 1
                                if (tokens(pos)%text == ")") paren_depth = &
                                    paren_depth - 1
                            end if
                            pos = pos + 1
                        end do
                    else if (tokens(pos)%text == "*") then
                        pos = pos + 1
                        if (pos <= size(tokens)) then
                            if (tokens(pos)%kind == TK_NUMBER) pos = pos + 1
                        end if
                    end if
                end if
            end if
        end if
    end subroutine skip_type_spec

    subroutine find_procedure_end(tokens, start_pos, proc_type, end_pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        character(len=*), intent(in) :: proc_type
        integer, intent(out) :: end_pos
        integer :: i, next_idx
        character(len=:), allocatable :: lowered, next_lower, combined

        end_pos = start_pos
        combined = "end" // trim(proc_type)

        do i = start_pos + 1, size(tokens)
            if (tokens(i)%kind == TK_EOF) then
                end_pos = i - 1
                exit
            end if

            if (tokens(i)%kind == TK_KEYWORD .or. tokens(i)%kind == TK_IDENTIFIER) then
                lowered = to_lower(trim(tokens(i)%text))

                if (lowered == combined) then
                    end_pos = i
                    next_idx = i + 1
                    do while (next_idx <= size(tokens))
                        if (tokens(next_idx)%kind == TK_WHITESPACE) then
                            next_idx = next_idx + 1
                        else if (tokens(next_idx)%kind == TK_IDENTIFIER .or. &
                                 tokens(next_idx)%kind == TK_KEYWORD) then
                            end_pos = next_idx
                            exit
                        else
                            exit
                        end if
                    end do
                    exit
                end if

                if (lowered == "end") then
                    next_idx = i + 1
                    do while (next_idx <= size(tokens))
                        if (tokens(next_idx)%kind == TK_WHITESPACE) then
                            next_idx = next_idx + 1
                        else
                            exit
                        end if
                    end do

                    if (next_idx <= size(tokens)) then
                        if (tokens(next_idx)%kind == TK_KEYWORD .or. &
                            tokens(next_idx)%kind == TK_IDENTIFIER) then
                            next_lower = to_lower(trim(tokens(next_idx)%text))
                            if (next_lower == proc_type) then
                                end_pos = next_idx
                                next_idx = next_idx + 1
                                do while (next_idx <= size(tokens))
                                    if (tokens(next_idx)%kind == TK_WHITESPACE) then
                                        next_idx = next_idx + 1
                                    else if (tokens(next_idx)%kind == &
                                             TK_IDENTIFIER .or. &
                                             tokens(next_idx)%kind == TK_KEYWORD) then
                                        end_pos = next_idx
                                        exit
                                    else
                                        exit
                                    end if
                                end do
                                exit
                            end if
                        end if
                    end if
                end if
            end if

            end_pos = i
        end do
    end subroutine find_procedure_end

end module frontend_statement_contains_section
