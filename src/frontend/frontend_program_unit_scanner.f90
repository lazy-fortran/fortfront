module frontend_program_unit_scanner
    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_COMMENT, TK_NEWLINE, &
                          TK_OPERATOR, TK_IDENTIFIER, TK_NUMBER, TK_WHITESPACE, &
                          to_lower
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_data, only: module_node, block_data_node
    use ast_nodes_misc, only: interface_block_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_transfer, only: entry_node
    use frontend_program_units, only: parse_program_unit

    implicit none
    private

    public :: detect_explicit_program_unit, is_inside_module, is_program_unit_start
    public :: unit_has_meaningful_content, should_process_unit, process_program_unit
    public :: find_program_unit_boundary, procedure_has_entry
    public :: find_next_nontrivial_index, token_precedes_identifier
    public :: token_requires_identifier_after, token_follows_identifier_context
    public :: keyword_can_be_identifier, token_is_block_keyword

contains

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
                                    unit_index, has_explicit_program, error_msg)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start, unit_end
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: unit_index
        logical, intent(in) :: has_explicit_program
        character(len=:), allocatable, intent(out), optional :: error_msg

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

        unit_index = parse_program_unit(unit_tokens, arena, has_explicit_program, &
                                        parse_error)

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

    subroutine find_program_unit_boundary(tokens, start_pos, unit_start, unit_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: unit_start, unit_end

        character(len=:), allocatable :: unit_type
        integer :: block_keyword_pos
        logical :: is_interface_unit
        integer :: effective_start

        unit_start = start_pos
        unit_end = start_pos
        block_keyword_pos = start_pos

        effective_start = skip_to_first_meaningful_token(tokens, start_pos)
        if (effective_start == 0) return

        unit_start = effective_start
        call classify_unit_type(tokens, unit_start, unit_type, block_keyword_pos, &
                                is_interface_unit)

        if (is_interface_unit) then
            call handle_interface_unit(tokens, unit_start, unit_end)
        else if (unit_type == "module") then
            call handle_module_unit(tokens, unit_start, unit_end)
        else if (unit_type == "template") then
            call handle_template_unit(tokens, unit_start, unit_end)
        else if (unit_type == "submodule") then
            call handle_submodule_unit(tokens, unit_start, unit_end)
        else if (unit_type == "subroutine" .or. unit_type == "function") then
            call handle_procedure_unit(tokens, unit_start, unit_type, unit_end)
        else if (unit_type == "program") then
            call handle_program_unit(tokens, unit_start, unit_end)
        else if (unit_type == "blockdata") then
            call handle_block_data_unit(tokens, block_keyword_pos, unit_end)
        else
            call handle_generic_unit(tokens, unit_start, unit_end)
        end if

        if (unit_end > size(tokens)) unit_end = size(tokens)
    end subroutine find_program_unit_boundary

    integer function skip_to_first_meaningful_token(tokens, start_pos) result(idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos

        idx = start_pos
        do while (idx <= size(tokens))
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                idx = idx + 1
                cycle
            case default
                return
            end select
        end do
        idx = 0
    end function skip_to_first_meaningful_token

    subroutine classify_unit_type(tokens, unit_start, unit_type, block_keyword_pos, &
                                  is_interface_unit)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start
        character(len=:), allocatable, intent(out) :: unit_type
        integer, intent(out) :: block_keyword_pos
        logical, intent(out) :: is_interface_unit
        integer :: next_pos

        unit_type = ""
        block_keyword_pos = unit_start
        is_interface_unit = .false.

        if (unit_start <= size(tokens)) then
            select case (tokens(unit_start)%kind)
                ! Check both TK_KEYWORD and TK_IDENTIFIER (keywords can be mixed case)
            case (TK_KEYWORD, TK_IDENTIFIER)
                unit_type = to_lower(trim(tokens(unit_start)%text))
                if (unit_type == "block") block_keyword_pos = unit_start
            case (TK_NUMBER)
                next_pos = unit_start + 1
                do while (next_pos <= size(tokens))
                    select case (tokens(next_pos)%kind)
                    case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                        next_pos = next_pos + 1
                        cycle
                        ! Check both TK_KEYWORD and TK_IDENTIFIER
                    case (TK_KEYWORD, TK_IDENTIFIER)
                        unit_type = to_lower(trim(tokens(next_pos)%text))
                        if (unit_type == "block") block_keyword_pos = next_pos
                        exit
                    case default
                        exit
                    end select
                end do
            end select
        end if

        if (unit_type == "block") then
            next_pos = block_keyword_pos + 1
            do while (next_pos <= size(tokens))
                select case (tokens(next_pos)%kind)
                case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                    next_pos = next_pos + 1
                    cycle
                case (TK_KEYWORD, TK_IDENTIFIER)
                    if (to_lower(trim(tokens(next_pos)%text)) == "data") then
                        unit_type = "blockdata"
                    end if
                    exit
                case default
                    exit
                end select
            end do
        else if (unit_type == "abstract") then
            next_pos = unit_start + 1
            do while (next_pos <= size(tokens))
                select case (tokens(next_pos)%kind)
                case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                    next_pos = next_pos + 1
                    cycle
                case (TK_KEYWORD)
                    if (to_lower(trim(tokens(next_pos)%text)) == "interface") then
                        unit_type = "interface"
                        is_interface_unit = .true.
                    end if
                    exit
                case default
                    exit
                end select
            end do
        else if (unit_type == "interface") then
            is_interface_unit = .true.
        end if
    end subroutine classify_unit_type

    subroutine handle_interface_unit(tokens, unit_start, unit_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start
        integer, intent(out) :: unit_end
        integer :: i, nesting_level
        integer :: next_pos
        integer :: name_pos
        character(len=:), allocatable :: keyword_text
        character(len=:), allocatable :: next_keyword

        unit_end = unit_start
        nesting_level = 1
        do i = unit_start + 1, size(tokens)
            if (tokens(i)%kind == TK_EOF) then
                unit_end = i - 1
                exit
            else if (tokens(i)%kind == TK_KEYWORD) then
                keyword_text = to_lower(trim(tokens(i)%text))
                select case (keyword_text)
                case ("interface")
                    nesting_level = nesting_level + 1
                case ("end")
                    next_pos = i + 1
                    do while (next_pos <= size(tokens))
                        select case (tokens(next_pos)%kind)
                        case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                            next_pos = next_pos + 1
                            cycle
                        case default
                            exit
                        end select
                    end do
                    if (next_pos <= size(tokens)) then
                        if (tokens(next_pos)%kind == TK_KEYWORD) then
                            next_keyword = to_lower(trim(tokens(next_pos)%text))
                            if (next_keyword == "interface") then
                                nesting_level = nesting_level - 1
                                unit_end = next_pos
                                name_pos = next_pos + 1
                                if (name_pos <= size(tokens)) then
                                    if (tokens(name_pos)%kind == TK_IDENTIFIER) then
                                        unit_end = name_pos
                                    end if
                                end if
                                if (nesting_level == 0) exit
                            end if
                        end if
                    end if
                end select
            end if
            unit_end = i
        end do
    end subroutine handle_interface_unit

    subroutine handle_module_unit(tokens, unit_start, unit_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start
        integer, intent(out) :: unit_end
        integer :: i, nesting_level
        logical :: in_module_contains
        character(len=:), allocatable :: keyword_text

        in_module_contains = .false.
        nesting_level = 1
        unit_end = unit_start

        do i = unit_start + 1, size(tokens)
            if (tokens(i)%kind == TK_EOF) then
                unit_end = i - 1
                exit
            else if (tokens(i)%kind == TK_KEYWORD) then
                keyword_text = to_lower(trim(tokens(i)%text))
                if (keyword_text == "contains") then
                    in_module_contains = .true.
                else if (keyword_text == "end") then
                    if (i + 1 <= size(tokens)) then
                        if (tokens(i + 1)%kind == TK_KEYWORD) then
                            if (to_lower(trim(tokens(i + 1)%text)) == "module") then
                                nesting_level = nesting_level - 1
                                if (nesting_level == 0) then
                                    unit_end = i + 1
                                    if (i + 2 <= size(tokens)) then
                                        if (tokens(i + 2)%kind == TK_IDENTIFIER) then
                                            unit_end = i + 2
                                        end if
                                    end if
                                    exit
                                end if
                            end if
                        end if
                    end if
                else if (keyword_text == "module" .and. .not. in_module_contains) then
                    if (i + 1 <= size(tokens)) then
                        if (tokens(i + 1)%kind == TK_IDENTIFIER) then
                            nesting_level = nesting_level + 1
                        end if
                    end if
                end if
            end if
            unit_end = i
        end do
    end subroutine handle_module_unit

    subroutine handle_template_unit(tokens, unit_start, unit_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start
        integer, intent(out) :: unit_end
        integer :: i, nesting_level
        logical :: in_template_contains
        character(len=:), allocatable :: keyword_text

        in_template_contains = .false.
        nesting_level = 1
        unit_end = unit_start

        do i = unit_start + 1, size(tokens)
            if (tokens(i)%kind == TK_EOF) then
                unit_end = i - 1
                exit
            else if (tokens(i)%kind == TK_KEYWORD) then
                keyword_text = to_lower(trim(tokens(i)%text))
                if (keyword_text == "contains") then
                    in_template_contains = .true.
                else if (keyword_text == "endtemplate") then
                    nesting_level = nesting_level - 1
                    if (nesting_level == 0) then
                        unit_end = i
                        if (i + 1 <= size(tokens)) then
                            if (tokens(i + 1)%kind == TK_IDENTIFIER) then
                                unit_end = i + 1
                            end if
                        end if
                        exit
                    end if
                else if (keyword_text == "end") then
                    if (i + 1 <= size(tokens)) then
                        if (tokens(i + 1)%kind == TK_KEYWORD) then
                            if (to_lower(trim(tokens(i + 1)%text)) == "template") then
                                nesting_level = nesting_level - 1
                                if (nesting_level == 0) then
                                    unit_end = i + 1
                                    if (i + 2 <= size(tokens)) then
                                        if (tokens(i + 2)%kind == TK_IDENTIFIER) then
                                            unit_end = i + 2
                                        end if
                                    end if
                                    exit
                                end if
                            end if
                        end if
                    end if
                else if (keyword_text == "template" .and. .not. &
                         in_template_contains) then
                    if (i + 1 <= size(tokens)) then
                        if (tokens(i + 1)%kind == TK_IDENTIFIER) then
                            nesting_level = nesting_level + 1
                        end if
                    end if
                end if
            end if
            unit_end = i
        end do
    end subroutine handle_template_unit

    subroutine handle_submodule_unit(tokens, unit_start, unit_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start
        integer, intent(out) :: unit_end
        integer :: i, nesting_level
        character(len=:), allocatable :: keyword_text
        character(len=:), allocatable :: next_keyword

        unit_end = unit_start
        nesting_level = 1

        do i = unit_start + 1, size(tokens)
            if (tokens(i)%kind == TK_EOF) then
                unit_end = i - 1
                exit
            else if (tokens(i)%kind == TK_KEYWORD) then
                keyword_text = to_lower(trim(tokens(i)%text))
                select case (keyword_text)
                case ("submodule")
                    nesting_level = nesting_level + 1
                case ("endsubmodule")
                    nesting_level = nesting_level - 1
                    if (nesting_level == 0) then
                        unit_end = i
                        if (i + 1 <= size(tokens)) then
                            if (tokens(i + 1)%kind == TK_IDENTIFIER) then
                                unit_end = i + 1
                            end if
                        end if
                        exit
                    end if
                case ("end")
                    if (i + 1 <= size(tokens)) then
                        if (tokens(i + 1)%kind == TK_KEYWORD) then
                            next_keyword = to_lower(trim(tokens(i + 1)%text))
                            if (next_keyword == "submodule") then
                                nesting_level = nesting_level - 1
                                if (nesting_level == 0) then
                                    unit_end = i + 1
                                    if (i + 2 <= size(tokens)) then
                                        if (tokens(i + 2)%kind == TK_IDENTIFIER) then
                                            unit_end = i + 2
                                        end if
                                    end if
                                    exit
                                end if
                            end if
                        end if
                    end if
                end select
            end if
            unit_end = i
        end do
    end subroutine handle_submodule_unit

    subroutine handle_procedure_unit(tokens, unit_start, unit_type, unit_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start
        character(len=*), intent(in) :: unit_type
        integer, intent(out) :: unit_end
        integer :: i

        unit_end = unit_start
        do i = unit_start + 1, size(tokens)
            if (tokens(i)%kind == TK_EOF) then
                unit_end = i - 1
                exit
                ! Check both TK_KEYWORD and TK_IDENTIFIER
            else if (tokens(i)%kind == TK_KEYWORD .or. &
                     tokens(i)%kind == TK_IDENTIFIER) then
                if (to_lower(trim(tokens(i)%text)) == "end") then
                    if (i + 1 <= size(tokens)) then
                        ! Check both TK_KEYWORD and TK_IDENTIFIER
                        if (tokens(i + 1)%kind == TK_KEYWORD .or. &
                            tokens(i + 1)%kind == TK_IDENTIFIER) then
                            if (to_lower(trim(tokens(i + 1)%text)) == unit_type) then
                                unit_end = i + 1
                                if (i + 2 <= size(tokens)) then
                                    if (tokens(i + 2)%kind == TK_IDENTIFIER .or. &
                                        tokens(i + 2)%kind == TK_KEYWORD) then
                                        unit_end = i + 2
                                    end if
                                end if
                                exit
                            end if
                        end if
                    end if
                    if (i == size(tokens) .or. (i + 1 <= size(tokens) .and. &
                                                tokens(i + 1)%kind /= TK_KEYWORD .and. &
                                                tokens(i + 1)%kind /= &
                                                TK_IDENTIFIER)) then
                        unit_end = i
                        exit
                    end if
                end if
            end if
            unit_end = i
        end do
    end subroutine handle_procedure_unit

    subroutine handle_program_unit(tokens, unit_start, unit_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start
        integer, intent(out) :: unit_end
        integer :: i, nesting_level
        integer :: next_pos
        integer :: name_pos
        character(len=:), allocatable :: keyword_text
        character(len=:), allocatable :: next_keyword

        unit_end = unit_start
        nesting_level = 1

        do i = unit_start + 1, size(tokens)
            if (tokens(i)%kind == TK_EOF) then
                unit_end = i - 1
                exit
            else if (tokens(i)%kind == TK_KEYWORD) then
                keyword_text = to_lower(trim(tokens(i)%text))
                select case (keyword_text)
                case ("program")
                    nesting_level = nesting_level + 1
                case ("end")
                    next_pos = i + 1
                    do while (next_pos <= size(tokens))
                        select case (tokens(next_pos)%kind)
                        case (TK_WHITESPACE)
                            next_pos = next_pos + 1
                            cycle
                        case (TK_NEWLINE, TK_COMMENT)
                            exit
                        case default
                            exit
                        end select
                    end do

                    if (next_pos > size(tokens)) then
                        nesting_level = nesting_level - 1
                        if (nesting_level == 0) then
                            unit_end = i
                            exit
                        end if
                    else if (tokens(next_pos)%kind == TK_KEYWORD) then
                        next_keyword = to_lower(trim(tokens(next_pos)%text))
                        if (next_keyword == "program") then
                            nesting_level = nesting_level - 1
                            if (nesting_level == 0) then
                                unit_end = next_pos
                                name_pos = next_pos + 1
                                do while (name_pos <= size(tokens))
                                    select case (tokens(name_pos)%kind)
                                    case (TK_WHITESPACE)
                                        name_pos = name_pos + 1
                                    case (TK_IDENTIFIER)
                                        unit_end = name_pos
                                        exit
                                    case default
                                        exit
                                    end select
                                end do
                                exit
                            end if
                        end if
                    else
                        select case (tokens(next_pos)%kind)
                        case (TK_NEWLINE, TK_COMMENT, TK_EOF)
                            nesting_level = nesting_level - 1
                            if (nesting_level == 0) then
                                unit_end = i
                                exit
                            end if
                        end select
                    end if
                end select
            end if
            unit_end = i
        end do
    end subroutine handle_program_unit

    subroutine handle_block_data_unit(tokens, block_keyword_pos, unit_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: block_keyword_pos
        integer, intent(out) :: unit_end
        integer :: i

        unit_end = block_keyword_pos
        do i = block_keyword_pos + 2, size(tokens)
            if (tokens(i)%kind == TK_EOF) then
                unit_end = i - 1
                exit
            else if (tokens(i)%kind == TK_KEYWORD) then
                if (to_lower(trim(tokens(i)%text)) == "end") then
                    if (i + 2 <= size(tokens)) then
                        if (tokens(i + 1)%kind == TK_KEYWORD .and. &
                            to_lower(trim(tokens(i + 1)%text)) == "block" .and. &
                            tokens(i + 2)%kind == TK_KEYWORD .and. &
                            to_lower(trim(tokens(i + 2)%text)) == "data") then
                            unit_end = i + 2
                            if (i + 3 <= size(tokens)) then
                                if (tokens(i + 3)%kind == TK_IDENTIFIER) then
                                    unit_end = i + 3
                                end if
                            end if
                            exit
                        end if
                    end if
                end if
            end if
            unit_end = i
        end do
    end subroutine handle_block_data_unit

    subroutine handle_generic_unit(tokens, unit_start, unit_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: unit_start
        integer, intent(out) :: unit_end
        integer :: i
        logical :: preceded_by_end

        unit_end = unit_start
        do i = unit_start, size(tokens)
            if (tokens(i)%kind == TK_EOF) then
                unit_end = i - 1
                exit
            else
                preceded_by_end = .false.
                if (i > 1) then
                    if (tokens(i - 1)%kind == TK_KEYWORD .and. &
                        tokens(i - 1)%text == "end") then
                        preceded_by_end = .true.
                    end if
                end if
                if (i > unit_start .and. is_program_unit_start(tokens, i) .and. &
                    .not. preceded_by_end) then
                    unit_end = i - 1
                    exit
                else
                    unit_end = i
                end if
            end if
        end do
    end subroutine handle_generic_unit

    integer function find_next_nontrivial_index(tokens, pos) result(idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        integer :: i

        idx = 0
        if (pos >= size(tokens)) return

        do i = pos + 1, size(tokens)
            select case (tokens(i)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                cycle
            case default
                idx = i
                return
            end select
        end do
    end function find_next_nontrivial_index

    logical function token_precedes_identifier(token) result(is_valid)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        is_valid = .false.
        if (token%kind /= TK_OPERATOR) return

        lowered = trim(token%text)
        select case (lowered)
        case ("%", "::", ",", "=", "=>", "(")
            is_valid = .true.
        end select
    end function token_precedes_identifier

    logical function token_requires_identifier_after(token) result(requires)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        requires = .false.
        if (token%kind /= TK_KEYWORD) return

        lowered = to_lower(trim(token%text))
        if (lowered == "call") requires = .true.
    end function token_requires_identifier_after

    logical function token_follows_identifier_context(token) result(is_valid)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        is_valid = .false.
        if (token%kind /= TK_OPERATOR) return

        lowered = trim(token%text)
        select case (lowered)
        case ("=", "=>", "(", "%")
            is_valid = .true.
        end select
    end function token_follows_identifier_context

    logical function keyword_can_be_identifier(keyword) result(can_be_id)
        character(len=*), intent(in) :: keyword

        select case (keyword)
        case ("call", "cycle", "exit", "entry", "select", "goto", "go", &
              "common", "dimension", "program", "module", "contains", &
              "stop", "pause", "return", "continue")
            can_be_id = .true.
        case default
            can_be_id = .false.
        end select
    end function keyword_can_be_identifier

    logical function token_is_block_keyword(token) result(is_block)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        is_block = .false.
        if (token%kind /= TK_KEYWORD) return

        lowered = to_lower(trim(token%text))
        select case (lowered)
        case ("type", "module", "subroutine", "function", "program", "interface", &
              "procedure", "select", "if", "do", "forall", "where", "associate", &
              "block", "team", "critical", "blockdata")
            is_block = .true.
        end select
    end function token_is_block_keyword

end module frontend_program_unit_scanner
