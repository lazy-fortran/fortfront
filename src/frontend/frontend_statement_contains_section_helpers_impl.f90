module frontend_statement_contains_section_helpers
    use lexer_core, only: token_t, TK_EOF, to_lower
    use frontend_statement_token_walking_helpers, only: find_proc_keyword_after_type, &
        find_procedure_end, &
        skip_type_spec, &
        token_is_ignorable, &
        token_is_word
    use parser_definition_statements_module, only: parse_function_definition, &
        parse_subroutine_definition
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use parser_state_module, only: create_parser_state, parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_misc, only: contains_node
    use error_reporting, only: error_collection_t

    implicit none
    private

    public :: push_implicit_contains_statement
    public :: scan_contains_section

contains

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
            body_indices, end_pos, diagnostic_sink)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer, allocatable, intent(inout) :: body_indices(:)
        integer, intent(out) :: end_pos
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
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
                    prefix_list, body_indices, diagnostic_sink)
                cycle
            end if

            if (handle_contains_procedure_keyword(tokens, lowered, i, arena, &
                prefix_buffer, prefix_list, &
                body_indices, diagnostic_sink)) cycle

            i = i + 1
        end do

        if (allocated(prefix_list)) deallocate (prefix_list)
    end subroutine scan_contains_section

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
        character(len=16) :: padded_prefix

        padded_prefix = adjustl(trim(lowered))
        prefix_list = [prefix_list, padded_prefix]
    end subroutine append_contains_prefix

    logical function handle_contains_procedure_keyword(tokens, lowered, pos, arena, &
            prefix_buffer, prefix_list, &
            body_indices, diagnostic_sink) result(handled)
        type(token_t), intent(in) :: tokens(:)
        character(len=*), intent(in) :: lowered
        integer, intent(inout) :: pos
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), allocatable, intent(inout) :: prefix_list(:)
        integer, allocatable, intent(inout) :: body_indices(:)
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink

        integer :: proc_end, proc_start

        handled = .false.

        if (lowered == "function") then
            proc_start = pos
            call find_procedure_end(tokens, proc_start, "function", proc_end)
            call parse_contains_function_span(tokens, proc_start, proc_end, arena, &
                prefix_buffer, prefix_list, body_indices, diagnostic_sink)
            pos = proc_end + 1
            handled = .true.
            return
        end if

        if (lowered == "subroutine") then
            proc_start = pos
            call find_procedure_end(tokens, proc_start, "subroutine", proc_end)
            call parse_contains_subroutine_span(tokens, proc_start, proc_end, arena, &
                prefix_buffer, prefix_list, &
                body_indices, diagnostic_sink)
            pos = proc_end + 1
            handled = .true.
            return
        end if
    end function handle_contains_procedure_keyword

    subroutine handle_type_prefixed_contains(tokens, pos, arena, prefix_buffer, &
            prefix_list, body_indices, diagnostic_sink)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: pos
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), allocatable, intent(inout) :: prefix_list(:)
        integer, allocatable, intent(inout) :: body_indices(:)
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink

        integer :: proc_end
        logical :: parsed_procedure

        call try_parse_typed_function_in_contains(tokens, pos, arena, &
            prefix_buffer, prefix_list, &
            body_indices, parsed_procedure, &
            proc_end, diagnostic_sink)
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
            body_indices, parsed, proc_end, diagnostic_sink)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: type_start
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), allocatable, intent(inout) :: prefix_list(:)
        integer, allocatable, intent(inout) :: body_indices(:)
        logical, intent(out) :: parsed
        integer, intent(out) :: proc_end
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
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
            prefix_buffer, prefix_list, body_indices, diagnostic_sink)
        parsed = .true.
    end subroutine try_parse_typed_function_in_contains

    subroutine parse_contains_function_span(tokens, span_start, proc_end, arena, &
            prefix_buffer, prefix_list, body_indices, diagnostic_sink)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: span_start
        integer, intent(in) :: proc_end
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), allocatable, intent(inout) :: prefix_list(:)
        integer, allocatable, intent(inout) :: body_indices(:)
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        type(parser_state_t) :: parser
        type(token_t), allocatable :: proc_tokens(:)
        integer :: proc_index

        allocate (proc_tokens(proc_end - span_start + 2))
        proc_tokens(1:proc_end - span_start + 1) = tokens(span_start:proc_end)
        proc_tokens(proc_end - span_start + 2)%kind = TK_EOF
        proc_tokens(proc_end - span_start + 2)%text = ""
        call prefix_buffer%clear()
        parser = create_parser_state(proc_tokens, diagnostic_sink)

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
            body_indices, diagnostic_sink)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: proc_start
        integer, intent(in) :: proc_end
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), allocatable, intent(inout) :: prefix_list(:)
        integer, allocatable, intent(inout) :: body_indices(:)
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        type(parser_state_t) :: parser
        type(token_t), allocatable :: proc_tokens(:)
        integer :: proc_index

        allocate (proc_tokens(proc_end - proc_start + 2))
        proc_tokens(1:proc_end - proc_start + 1) = tokens(proc_start:proc_end)
        proc_tokens(proc_end - proc_start + 2)%kind = TK_EOF
        proc_tokens(proc_end - proc_start + 2)%text = ""
        call prefix_buffer%clear()
        parser = create_parser_state(proc_tokens, diagnostic_sink)

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
end module frontend_statement_contains_section_helpers
