    function detect_explicit_program_unit(tokens) result(has_explicit_program_unit)
        type(token_t), intent(in) :: tokens(:)
        logical :: has_explicit_program_unit
        integer :: i
        integer :: next_idx
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: next_lower

        has_explicit_program_unit = .false.
        do i = 1, size(tokens)
            ! Check both TK_KEYWORD and TK_IDENTIFIER (keywords can be mixed case)
            if (tokens(i)%kind /= TK_KEYWORD .and. &
                tokens(i)%kind /= TK_IDENTIFIER) cycle

            lowered = to_lower(trim(tokens(i)%text))
            select case (lowered)
            case ("program", "module", "function", "subroutine", "interface")
                has_explicit_program_unit = .true.
                exit
            case ("abstract")
                next_idx = find_next_nontrivial_index(tokens, i)
                if (next_idx > 0 .and. next_idx <= size(tokens)) then
                    if (tokens(next_idx)%kind == TK_KEYWORD .or. &
                        tokens(next_idx)%kind == TK_IDENTIFIER) then
                        next_lower = to_lower(trim(tokens(next_idx)%text))
                        if (next_lower == "interface") then
                            has_explicit_program_unit = .true.
                            exit
                        end if
                    end if
                end if
            end select
        end do
    end function detect_explicit_program_unit

    function is_inside_module(tokens, pos) result(inside_module)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        logical :: inside_module
        integer :: i
        logical :: in_module

        inside_module = .false.
        in_module = .false.

        do i = 1, min(pos, size(tokens))
            if (tokens(i)%kind == TK_KEYWORD) then
                if (tokens(i)%text == "module") then
                    in_module = .true.
                else if (tokens(i)%text == "end") then
                    if (i < size(tokens) .and. tokens(i + 1)%kind == TK_KEYWORD .and. &
                        tokens(i + 1)%text == "module") then
                        in_module = .false.
                    end if
                end if
            end if
        end do

        inside_module = in_module
    end function is_inside_module

    function is_program_unit_start(tokens, i) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: i
        logical :: is_start
        integer :: lookahead
        integer :: keyword_idx
        integer :: prev_idx
        logical :: label_allowed

        is_start = .false.
        if (i > size(tokens)) return
        keyword_idx = i

        if (tokens(keyword_idx)%kind == TK_NUMBER) then
            label_allowed = .true.
            prev_idx = keyword_idx - 1
            do while (prev_idx >= 1)
                select case (tokens(prev_idx)%kind)
                case (TK_WHITESPACE)
                    prev_idx = prev_idx - 1
                    cycle
                case default
                    exit
                end select
            end do

            if (prev_idx >= 1) then
                select case (tokens(prev_idx)%kind)
                case (TK_NEWLINE, TK_COMMENT)
                    label_allowed = .true.
                case default
                    label_allowed = .false.
                end select
            end if

            if (.not. label_allowed) return
            keyword_idx = keyword_idx + 1
        end if

        do while (keyword_idx <= size(tokens))
            select case (tokens(keyword_idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                keyword_idx = keyword_idx + 1
                cycle
            case default
                exit
            end select
        end do

        if (keyword_idx > size(tokens)) return
        ! Check both TK_KEYWORD and TK_IDENTIFIER (keywords can be mixed case)
        if (tokens(keyword_idx)%kind /= TK_KEYWORD .and. &
            tokens(keyword_idx)%kind /= TK_IDENTIFIER) return

        select case (to_lower(trim(tokens(keyword_idx)%text)))
        case ("program", "module", "function", "subroutine", "interface", "template")
            is_start = .true.
        case ("abstract")
            lookahead = keyword_idx + 1
            do while (lookahead <= size(tokens))
                select case (tokens(lookahead)%kind)
                case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                    lookahead = lookahead + 1
                    cycle
                case (TK_KEYWORD, TK_IDENTIFIER)
                    if (to_lower(trim(tokens(lookahead)%text)) == "interface") then
                        is_start = .true.
                    end if
                    exit
                case default
                    exit
                end select
            end do
        case ("block")
            lookahead = keyword_idx + 1
            do while (lookahead <= size(tokens))
                select case (tokens(lookahead)%kind)
                case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                    lookahead = lookahead + 1
                    cycle
                case (TK_KEYWORD, TK_IDENTIFIER)
                    if (to_lower(trim(tokens(lookahead)%text)) == "data") then
                        is_start = .true.
                    end if
                    exit
                case default
                    exit
                end select
            end do
        case default
            lookahead = find_procedure_keyword_after_prefix(tokens, keyword_idx)
            if (lookahead > 0) is_start = .true.
        end select
    end function is_program_unit_start

    integer function find_procedure_keyword_after_prefix(tokens, start_idx) &
        result(proc_idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_idx
        integer :: idx
        character(len=:), allocatable :: lowered

        proc_idx = 0
        idx = start_idx

        do while (idx <= size(tokens))
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT, TK_OPERATOR, &
                  TK_IDENTIFIER, TK_NUMBER)
                idx = idx + 1
                cycle
            case (TK_KEYWORD)
                lowered = to_lower(trim(tokens(idx)%text))
                if (lowered == "function" .or. lowered == "subroutine") then
                    proc_idx = idx
                    return
                else if (is_procedure_attribute_keyword(lowered)) then
                    idx = idx + 1
                    cycle
                else if (is_intrinsic_type_keyword(lowered)) then
                    idx = idx + 1
                    cycle
                else if ((lowered == "type" .or. lowered == "class") .and. &
                         is_type_spec_prefix(tokens, idx)) then
                    idx = idx + 1
                    cycle
                else
                    return
                end if
            case default
                return
            end select
        end do
    end function find_procedure_keyword_after_prefix

    logical function is_intrinsic_type_keyword(word) result(is_type_kw)
        character(len=*), intent(in) :: word

        select case (trim(word))
        case ("integer", "real", "double", "precision", "logical", "character", &
              "complex")
            is_type_kw = .true.
        case default
            is_type_kw = .false.
        end select
    end function is_intrinsic_type_keyword

    logical function is_procedure_attribute_keyword(word) result(is_attr)
        character(len=*), intent(in) :: word

        select case (trim(word))
        case ("pure", "impure", "elemental", "recursive", "nonrecursive", &
              "non_recursive", "module")
            is_attr = .true.
        case default
            is_attr = .false.
        end select
    end function is_procedure_attribute_keyword

    logical function is_type_spec_prefix(tokens, idx) result(is_spec)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: idx
        integer :: next_idx

        is_spec = .false.
        next_idx = find_next_nontrivial_index(tokens, idx)
        if (next_idx <= 0) return
        if (tokens(next_idx)%kind == TK_OPERATOR) then
            if (trim(tokens(next_idx)%text) == "(") is_spec = .true.
        end if
    end function is_type_spec_prefix

    function unit_has_meaningful_content(tokens, unit_start, unit_end) &
        result(has_content)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start, unit_end
        logical :: has_content
        integer :: i

        has_content = .false.
        do i = unit_start, min(unit_end, size(tokens))
            if (tokens(i)%kind /= TK_COMMENT .and. tokens(i)%kind /= TK_EOF .and. &
                tokens(i)%kind /= TK_NEWLINE) then
                has_content = .true.
                exit
            end if
        end do
    end function unit_has_meaningful_content

    function should_process_unit(tokens, unit_start, unit_end) result(should_process)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start, unit_end
        logical :: should_process

        should_process = unit_has_meaningful_content(tokens, unit_start, unit_end)
    end function should_process_unit

    subroutine process_program_unit(tokens, unit_start, unit_end, arena, &
            unit_index, has_explicit_program, error_msg, diagnostic_sink)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start, unit_end
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: unit_index
        logical, intent(in) :: has_explicit_program
        character(len=:), allocatable, intent(out), optional :: error_msg
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink

        type(token_t), allocatable, target :: unit_tokens(:)
        character(len=:), allocatable :: parse_error

        unit_index = 0
        if (present(error_msg)) error_msg = ""

        if (.not. should_process_unit(tokens, unit_start, unit_end)) then
            return
        end if

        allocate (unit_tokens(unit_end - unit_start + 2))
        unit_tokens(1:unit_end - unit_start + 1) = tokens(unit_start:unit_end)
        unit_tokens(unit_end - unit_start + 2)%kind = TK_EOF
        unit_tokens(unit_end - unit_start + 2)%text = ""
        unit_tokens(unit_end - unit_start + 2)%line = tokens(unit_end)%line
        unit_tokens(unit_end - unit_start + 2)%column = tokens(unit_end)%column + 1

        if (present(diagnostic_sink)) then
            unit_index = parse_program_unit(unit_tokens, arena, &
                has_explicit_program, parse_error, diagnostic_sink)
        else
            unit_index = parse_program_unit(unit_tokens, arena, &
                has_explicit_program, parse_error)
        end if

        if (present(error_msg) .and. allocated(parse_error)) then
            if (len_trim(parse_error) > 0) error_msg = parse_error
        end if

        deallocate (unit_tokens)
    end subroutine process_program_unit

    logical function procedure_has_entry(arena, proc_index) result(has_entry)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_index

        has_entry = .false.
        if (proc_index <= 0) return
        if (proc_index > arena%size) return
        if (.not. allocated(arena%entries(proc_index)%node)) return

        select type (proc => arena%entries(proc_index)%node)
        type is (function_def_node)
            if (.not. allocated(proc%body_indices)) return
            has_entry = body_indices_have_entry(arena, proc%body_indices)
        type is (subroutine_def_node)
            if (.not. allocated(proc%body_indices)) return
            has_entry = body_indices_have_entry(arena, proc%body_indices)
        class default
            has_entry = .false.
        end select
    end function procedure_has_entry

    logical function body_indices_have_entry(arena, body_indices) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer :: i, idx

        found = .false.
        if (size(body_indices) == 0) return

        do i = 1, size(body_indices)
            idx = body_indices(i)
            if (idx <= 0) cycle
            if (idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (stmt => arena%entries(idx)%node)
            type is (entry_node)
                found = .true.
                return
            end select
        end do
    end function body_indices_have_entry
