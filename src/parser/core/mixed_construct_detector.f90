module mixed_construct_detector
    ! Mixed construct detection for Issue #511 support
    ! Analyzes token stream to identify mixed constructs requiring module generation

    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_EOF, TK_NEWLINE, &
        TK_WHITESPACE, TK_COMMENT, TK_OPERATOR, to_lower
    implicit none
    private

    ! Mixed construct analysis result
    type, public :: mixed_construct_result_t
        logical :: has_mixed_constructs = .false.
        integer, allocatable :: implicit_ranges(:, :) ! [start, end] pairs
        integer, allocatable :: explicit_ranges(:, :) ! [start, end] pairs
        integer :: num_implicit_ranges = 0
        integer :: num_explicit_ranges = 0
    end type mixed_construct_result_t

    public :: detect_mixed_constructs
    public :: is_top_level_declaration
    public :: is_explicit_program_unit
    public :: function_follows_type_spec

contains

    ! Main detection routine
    subroutine detect_mixed_constructs(tokens, result)
        type(token_t), intent(in) :: tokens(:)
        type(mixed_construct_result_t), intent(out) :: result

        integer :: i, range_start, range_end
        logical :: in_implicit_construct, in_explicit_construct

        ! Initialize result
        result%has_mixed_constructs = .false.
        result%num_implicit_ranges = 0
        result%num_explicit_ranges = 0
        allocate (result%implicit_ranges(100, 2)) ! Max 100 ranges
        allocate (result%explicit_ranges(100, 2))

        i = 1
        do while (i <= size(tokens))
            ! Skip EOF tokens between lines
            if (tokens(i)%kind == TK_EOF .or. tokens(i)%kind == TK_NEWLINE) then
                i = i + 1
                cycle
            end if

            ! Detect construct type
            if (is_top_level_declaration(tokens, i)) then
                ! Found implicit declaration - find its range
                range_start = i
                call find_declaration_range(tokens, i, range_end)

                ! Add to implicit ranges
                result%num_implicit_ranges = result%num_implicit_ranges + 1
                result%implicit_ranges(result%num_implicit_ranges, 1) = range_start
                result%implicit_ranges(result%num_implicit_ranges, 2) = range_end

                i = range_end + 1

            else if (is_explicit_program_unit(tokens, i)) then
                ! Found explicit program unit - find its range
                range_start = i
                call find_program_unit_range(tokens, i, range_end)

                ! Add to explicit ranges
                result%num_explicit_ranges = result%num_explicit_ranges + 1
                result%explicit_ranges(result%num_explicit_ranges, 1) = range_start
                result%explicit_ranges(result%num_explicit_ranges, 2) = range_end

                i = range_end + 1

            else
                ! Unknown construct - skip
                i = i + 1
            end if
        end do

        ! Check if we have mixed constructs
        result%has_mixed_constructs = (result%num_implicit_ranges > 0 .and. &
            result%num_explicit_ranges > 0)

        ! Resize arrays to actual size
        if (result%num_implicit_ranges > 0) then
            block
                integer, allocatable :: trimmed(:, :)
                allocate (trimmed(result%num_implicit_ranges, 2))
                trimmed = result%implicit_ranges(1:result%num_implicit_ranges, :)
                call move_alloc(trimmed, result%implicit_ranges)
            end block
        else
            block
                integer, allocatable :: empty(:, :)
                allocate (empty(0, 2))
                call move_alloc(empty, result%implicit_ranges)
            end block
        end if

        if (result%num_explicit_ranges > 0) then
            block
                integer, allocatable :: trimmed(:, :)
                allocate (trimmed(result%num_explicit_ranges, 2))
                trimmed = result%explicit_ranges(1:result%num_explicit_ranges, :)
                call move_alloc(trimmed, result%explicit_ranges)
            end block
        else
            block
                integer, allocatable :: empty(:, :)
                allocate (empty(0, 2))
                call move_alloc(empty, result%explicit_ranges)
            end block
        end if
    end subroutine detect_mixed_constructs

    ! Check if token sequence represents a top-level declaration
    function is_top_level_declaration(tokens, start_pos) result(is_declaration)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        logical :: is_declaration

        is_declaration = .false.

        if (start_pos > size(tokens)) return

        ! Check for type declarations: type, integer, real, character, etc.
        if (tokens(start_pos)%kind == TK_KEYWORD) then
            select case (trim(tokens(start_pos)%text))
            case ("type")
                ! Could be "type :: name" or "type(name)" - both are declarations
                is_declaration = .not. function_follows_type_spec(tokens, start_pos)
            case ("integer", "real", "character", "logical", "complex", "procedure")
                ! Basic type declarations
                is_declaration = .not. function_follows_type_spec(tokens, start_pos)
            case ("parameter")
                ! Parameter declarations
                is_declaration = .true.
            end select
        end if
    end function is_top_level_declaration

    ! True when "function" follows a leading return-type keyword at start_pos,
    ! skipping an optional balanced kind specifier such as "(8)".
    function function_follows_type_spec(tokens, start_pos) result(is_typed_function)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        logical :: is_typed_function
        integer :: idx, depth
        logical :: is_type_keyword

        is_typed_function = .false.
            if (start_pos > size(tokens)) return
            if (tokens(start_pos)%kind /= TK_KEYWORD) return
            is_type_keyword = .false.
            select case (to_lower(trim(tokens(start_pos)%text)))
            case ("integer", "real", "character", "logical", "complex", &
                    "procedure", "type", "class", "double")
                is_type_keyword = .true.
            end select
            if (.not. is_type_keyword) return

            idx = start_pos + 1
            do while (idx <= size(tokens))
                select case (tokens(idx)%kind)
                case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                    idx = idx + 1
                    cycle
                case default
                    exit
                end select
            end do
            if (idx > size(tokens)) return

            if (to_lower(trim(tokens(start_pos)%text)) == "double" .and. &
                tokens(idx)%kind == TK_KEYWORD) then
                select case (to_lower(trim(tokens(idx)%text)))
                case ("precision", "complex")
                    idx = idx + 1
                    do while (idx <= size(tokens))
                        select case (tokens(idx)%kind)
                        case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                            idx = idx + 1
                            cycle
                        case default
                            exit
                        end select
                    end do
                    if (idx > size(tokens)) return
                end select
            end if

            if (tokens(idx)%kind == TK_OPERATOR .and. tokens(idx)%text == "(") then
                depth = 0
                do while (idx <= size(tokens))
                    if (tokens(idx)%kind == TK_OPERATOR .and. tokens(idx)%text == "(") then
                        depth = depth + 1
                    else if (tokens(idx)%kind == TK_OPERATOR .and. &
                            tokens(idx)%text == ")") then
                        depth = depth - 1
                        if (depth == 0) then
                            idx = idx + 1
                            exit
                        end if
                    end if
                    idx = idx + 1
                end do
            end if

            do while (idx <= size(tokens))
                select case (tokens(idx)%kind)
                case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                    idx = idx + 1
                    cycle
                case default
                    exit
                end select
            end do
            if (idx > size(tokens)) return

            if (tokens(idx)%kind == TK_KEYWORD .and. &
                to_lower(trim(tokens(idx)%text)) == "function") then
                is_typed_function = .true.
                end if
            end function function_follows_type_spec

            ! Check if token sequence represents explicit program unit
            function is_explicit_program_unit(tokens, start_pos) result(is_program_unit)
                type(token_t), intent(in) :: tokens(:)
                integer, intent(in) :: start_pos
                logical :: is_program_unit

                is_program_unit = .false.

                if (start_pos > size(tokens)) return

                ! Check for explicit program unit starters
                if (start_pos > size(tokens)) return

                select case (tokens(start_pos)%kind)
                case (TK_KEYWORD, TK_IDENTIFIER)
                    block
                        integer :: lookahead
                        character(len=:), allocatable :: first_word
                        character(len=:), allocatable :: next_word

                        first_word = to_lower(trim(tokens(start_pos)%text))
                        select case (first_word)
                        case ("program", "module", "subroutine", "function")
                            is_program_unit = .true.
                            return
                        case ("integer", "real", "character", "logical", "complex", &
                                "procedure", "type", "class", "double")
                            if (function_follows_type_spec(tokens, start_pos)) then
                                is_program_unit = .true.
                            end if
                            return
                        case ("block")
                            lookahead = start_pos + 1
                            do while (lookahead <= size(tokens))
                                select case (tokens(lookahead)%kind)
                                case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                                    lookahead = lookahead + 1
                                    cycle
                                case (TK_KEYWORD, TK_IDENTIFIER)
                                    next_word = to_lower(trim(tokens(lookahead)%text))
                                    if (next_word == "data") then
                                        is_program_unit = .true.
                                    end if
                                    return
                                case default
                                    return
                                end select
                            end do
                        end select
                    end block
                end select
            end function is_explicit_program_unit

            ! Find the end of a declaration construct
            subroutine find_declaration_range(tokens, start_pos, end_pos)
                type(token_t), intent(in) :: tokens(:)
                integer, intent(in) :: start_pos
                integer, intent(out) :: end_pos

                integer :: i, depth

                i = start_pos
                depth = 0

                ! For type declarations, find "end type"
                if (i <= size(tokens) .and. tokens(i)%kind == TK_KEYWORD .and. &
                    trim(tokens(i)%text) == "type") then

                    ! Find matching "end type"
                    do i = start_pos + 1, size(tokens)
                        if (tokens(i)%kind == TK_KEYWORD) then
                            if (trim(tokens(i)%text) == "type") then
                                depth = depth + 1
                            else if (trim(tokens(i)%text) == "end") then
                                ! Check next token for "type"
                                if (i + 1 <= size(tokens) .and. tokens(i + 1)%kind == &
                                    TK_KEYWORD .and. &
                                    trim(tokens(i + 1)%text) == "type") then
                                    if (depth == 0) then
                                        end_pos = i + 1
                                        return
                                    else
                                        depth = depth - 1
                                    end if
                                end if
                            end if
                        end if
                    end do
                else
                    ! For simple declarations, stop at next declaration, program unit,
                    ! or newline that does not continue
                    do i = start_pos + 1, size(tokens)
                        select case (tokens(i)%kind)
                        case (TK_EOF)
                            end_pos = i
                            return
                        case (TK_NEWLINE)
                            if (.not. line_continues(tokens, i)) then
                                end_pos = i
                                return
                            end if
                        end select
                        ! Check for start of new construct
                        if (is_top_level_declaration(tokens, i) .or. &
                            is_explicit_program_unit(tokens, i)) then
                            end_pos = i - 1
                            return
                        end if
                    end do
                end if

                ! Default to end of tokens
                end_pos = size(tokens)
            end subroutine find_declaration_range

            logical function line_continues(tokens, newline_pos) result(continues)
                type(token_t), intent(in) :: tokens(:)
                integer, intent(in) :: newline_pos
                integer :: j
                character(len=:), allocatable :: text

                continues = .false.

                ! Check for trailing ampersand before newline
                j = newline_pos - 1
                do while (j >= 1)
                    select case (tokens(j)%kind)
                    case (TK_WHITESPACE, TK_COMMENT)
                        j = j - 1
                        cycle
                    case (TK_OPERATOR)
                        text = trim(tokens(j)%text)
                        if (text == "&") then
                            continues = .true.
                        end if
                        return
                    case default
                        exit
                    end select
                    exit
                end do

                ! Check for leading ampersand after newline
                j = newline_pos + 1
                do while (j <= size(tokens))
                    select case (tokens(j)%kind)
                    case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                        j = j + 1
                        cycle
                    case (TK_OPERATOR)
                        text = trim(tokens(j)%text)
                        if (text == "&") then
                            continues = .true.
                        end if
                        return
                    case default
                        return
                    end select
                end do
            end function line_continues

            ! Find the end of a program unit construct
            subroutine find_program_unit_range(tokens, start_pos, end_pos)
                type(token_t), intent(in) :: tokens(:)
                integer, intent(in) :: start_pos
                integer, intent(out) :: end_pos

                integer :: i, lookahead, depth
                character(len=:), allocatable :: start_keyword, end_keyword
                character(len=:), allocatable :: next_word, current_word

                if (start_pos > size(tokens)) then
                    end_pos = start_pos
                    return
                end if

                start_keyword = to_lower(trim(tokens(start_pos)%text))

                ! A return-type prefix (e.g. "integer(8) function") delimits a function.
                select case (start_keyword)
                case ("integer", "real", "character", "logical", "complex", &
                        "procedure", "type", "class", "double")
                    if (function_follows_type_spec(tokens, start_pos)) then
                        start_keyword = "function"
                    end if
                end select

                ! Determine expected end keyword
                select case (start_keyword)
                case ("program")
                    end_keyword = "program"
                case ("module")
                    end_keyword = "module"
                case ("subroutine")
                    end_keyword = "subroutine"
                case ("function")
                    end_keyword = "function"
                case ("block")
                    lookahead = start_pos + 1
                    do while (lookahead <= size(tokens))
                        select case (tokens(lookahead)%kind)
                        case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                            lookahead = lookahead + 1
                            cycle
                        case (TK_KEYWORD, TK_IDENTIFIER)
                            next_word = to_lower(trim(tokens(lookahead)%text))
                            if (next_word == "data") then
                                call find_block_data_end(tokens, lookahead + 1, end_pos)
                                return
                            else
                                end_pos = start_pos
                                return
                            end if
                        case default
                            end_pos = start_pos
                            return
                        end select
                    end do
                    end_pos = start_pos
                    return
                case default
                    end_pos = start_pos
                    return
                end select

                ! Find matching end statement with depth tracking for nested procedures
                depth = 0
                do i = start_pos + 1, size(tokens)
                    if (tokens(i)%kind == TK_KEYWORD) then
                        current_word = to_lower(trim(tokens(i)%text))

                        ! Check if previous keyword was end
                        block
                            integer :: prev_pos
                            character(len=:), allocatable :: prev_word
                            logical :: is_after_end

                            is_after_end = .false.
                            if (i > 1) then
                                prev_pos = i - 1
                                do while (prev_pos > 0)
                                    if (tokens(prev_pos)%kind == TK_KEYWORD) then
                                        prev_word = to_lower(trim(tokens(prev_pos)%text))
                                        if (prev_word == "end") is_after_end = .true.
                                        exit
                                    else if (tokens(prev_pos)%kind /= TK_WHITESPACE .and. &
                                            tokens(prev_pos)%kind /= TK_NEWLINE .and. &
                                            tokens(prev_pos)%kind /= TK_COMMENT) then
                                        exit
                                    end if
                                    prev_pos = prev_pos - 1
                                end do
                            end if

                            ! Track nesting depth by counting start keywords
                            ! But skip if this is part of end function
                            if (current_word == start_keyword .and. .not. is_after_end) then
                                depth = depth + 1
                            end if
                        end block

                        if (current_word == "end") then
                            ! Check if next token matches our end keyword
                            if (i + 1 <= size(tokens) .and. &
                                tokens(i + 1)%kind == TK_KEYWORD .and. &
                                to_lower(trim(tokens(i + 1)%text)) == end_keyword) then

                                if (depth == 0) then
                                    ! Found matching end for this unit
                                    end_pos = i + 1
                                    return
                                else
                                    ! Found end for nested unit, decrease depth
                                    depth = depth - 1
                                end if
                            end if
                        end if
                    end if
                end do

                ! Default to end of tokens if no matching end found
                end_pos = size(tokens)
            end subroutine find_program_unit_range

            subroutine find_block_data_end(tokens, start_pos, end_pos)
                type(token_t), intent(in) :: tokens(:)
                integer, intent(in) :: start_pos
                integer, intent(out) :: end_pos

                integer :: i, lookahead
                character(len=:), allocatable :: word

                end_pos = size(tokens)
                do i = start_pos, size(tokens)
                    if (tokens(i)%kind /= TK_KEYWORD) cycle
                    if (to_lower(trim(tokens(i)%text)) /= "end") cycle

                    lookahead = i + 1
                    do while (lookahead <= size(tokens))
                        select case (tokens(lookahead)%kind)
                        case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                            lookahead = lookahead + 1
                            cycle
                        case (TK_KEYWORD, TK_IDENTIFIER)
                            word = to_lower(trim(tokens(lookahead)%text))
                            if (word == "block") then
                                lookahead = lookahead + 1
                                do while (lookahead <= size(tokens))
                                    select case (tokens(lookahead)%kind)
                                    case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                                        lookahead = lookahead + 1
                                        cycle
                                    case (TK_KEYWORD, TK_IDENTIFIER)
                                        if (to_lower(trim(tokens(lookahead)%text)) == &
                                            "data") then
                                            end_pos = lookahead
                                            lookahead = lookahead + 1
                                            do while (lookahead <= size(tokens))
                                                select case (tokens(lookahead)%kind)
                                                case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                                                    lookahead = lookahead + 1
                                                    cycle
                                                case (TK_IDENTIFIER)
                                                    end_pos = lookahead
                                                end select
                                                exit
                                            end do
                                            return
                                        else
                                            return
                                        end if
                                    case default
                                        return
                                    end select
                                end do
                            else
                                exit
                            end if
                        case default
                            exit
                        end select
                    end do
                end do
            end subroutine find_block_data_end

        end module mixed_construct_detector
