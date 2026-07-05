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
        integer :: next_idx
        logical :: in_module_contains

        in_module_contains = .false.
        nesting_level = 1
        unit_end = unit_start

        do i = unit_start + 1, size(tokens)
            if (tokens(i)%kind == TK_EOF) then
                unit_end = i - 1
                exit
            else if (tokens(i)%kind == TK_KEYWORD .or. &
                    tokens(i)%kind == TK_IDENTIFIER) then
                if (token_is_word_at(tokens, i, "contains")) then
                    in_module_contains = .true.
                else if (token_is_word_at(tokens, i, "end")) then
                    next_idx = find_next_nontrivial_index(tokens, i)
                    if (next_idx > 0 .and. next_idx <= size(tokens)) then
                        if (token_is_word_at(tokens, next_idx, "module")) then
                                nesting_level = nesting_level - 1
                                if (nesting_level == 0) then
                                    unit_end = next_idx
                                    next_idx = find_next_nontrivial_index(tokens, next_idx)
                                    if (next_idx > 0 .and. next_idx <= size(tokens)) then
                                        if (tokens(next_idx)%kind == TK_IDENTIFIER) then
                                            unit_end = next_idx
                                        end if
                                    end if
                                    exit
                                end if
                        end if
                    end if
                else if (token_is_word_at(tokens, i, "module") .and. &
                        .not. in_module_contains) then
                    next_idx = find_next_nontrivial_index(tokens, i)
                    if (next_idx > 0 .and. next_idx <= size(tokens)) then
                        if (tokens(next_idx)%kind == TK_IDENTIFIER) then
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
        integer :: i, interface_nesting
        character(len=:), allocatable :: lowered

        unit_end = unit_start
        interface_nesting = 0
        do i = unit_start + 1, size(tokens)
            if (tokens(i)%kind == TK_EOF) then
                unit_end = i - 1
                exit
                ! Check both TK_KEYWORD and TK_IDENTIFIER
            else if (tokens(i)%kind == TK_KEYWORD .or. &
                     tokens(i)%kind == TK_IDENTIFIER) then
                lowered = to_lower(trim(tokens(i)%text))
                if (lowered == "interface" .and. interface_nesting == 0) then
                    interface_nesting = interface_nesting + 1
                else if (lowered == "end") then
                    if (i + 1 <= size(tokens)) then
                        ! Check both TK_KEYWORD and TK_IDENTIFIER
                        if (tokens(i + 1)%kind == TK_KEYWORD .or. &
                            tokens(i + 1)%kind == TK_IDENTIFIER) then
                            if (to_lower(trim(tokens(i + 1)%text)) == "interface") then
                                if (interface_nesting > 0) then
                                    interface_nesting = interface_nesting - 1
                                end if
                            else if (to_lower(trim(tokens(i + 1)%text)) == &
                                     unit_type .and. interface_nesting == 0) then
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
                    if (interface_nesting == 0) then
                        if (i == size(tokens) .or. &
                            (i + 1 <= size(tokens) .and. &
                             tokens(i + 1)%kind /= TK_KEYWORD .and. &
                             tokens(i + 1)%kind /= TK_IDENTIFIER)) then
                            unit_end = i
                            exit
                        end if
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
                    if (token_starts_bare_end_statement(tokens, i)) then
                        unit_end = i
                        exit
                    end if
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
        logical :: in_contains_section
        integer :: proc_start, proc_end
        character(len=:), allocatable :: lowered

        unit_end = unit_start
        in_contains_section = .false.
        i = unit_start
        do while (i <= size(tokens))
            if (tokens(i)%kind == TK_EOF) then
                unit_end = i - 1
                exit
            else
                if (token_starts_structural_contains(tokens, i)) then
                    in_contains_section = .true.
                end if

                if (in_contains_section) then
                    proc_start = find_contains_procedure_start(tokens, i)
                    if (proc_start > 0) then
                        lowered = to_lower(trim(tokens(proc_start)%text))
                        call handle_procedure_unit(tokens, proc_start, &
                                                   lowered, proc_end)
                        unit_end = proc_end
                        i = proc_end + 1
                        cycle
                    end if

                    if (token_starts_bare_end_statement(tokens, i)) then
                        unit_end = i
                        exit
                    end if

                    unit_end = i
                    i = i + 1
                    cycle
                end if

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
            i = i + 1
        end do
    end subroutine handle_generic_unit
