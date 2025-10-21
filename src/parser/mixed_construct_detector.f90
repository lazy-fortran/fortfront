module mixed_construct_detector
    ! Mixed construct detection for Issue #511 support
    ! Analyzes token stream to identify mixed constructs requiring module generation

    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_EOF, TK_NEWLINE, &
                          TK_WHITESPACE, TK_COMMENT, to_lower
    implicit none
    private

    ! Mixed construct analysis result
    type, public :: mixed_construct_result_t
        logical :: has_mixed_constructs = .false.
        integer, allocatable :: implicit_ranges(:, :)  ! [start, end] pairs
        integer, allocatable :: explicit_ranges(:, :)  ! [start, end] pairs
        integer :: num_implicit_ranges = 0
        integer :: num_explicit_ranges = 0
    end type mixed_construct_result_t

    public :: detect_mixed_constructs
    public :: is_top_level_declaration
    public :: is_explicit_program_unit

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
        allocate (result%implicit_ranges(100, 2))  ! Max 100 ranges
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
                is_declaration = .true.
            case ("integer", "real", "character", "logical", "complex")
                ! Basic type declarations
                is_declaration = .true.
            case ("parameter")
                ! Parameter declarations
                is_declaration = .true.
            end select
        end if
    end function is_top_level_declaration

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
            ! For simple declarations, just find end of statement
            do i = start_pos + 1, size(tokens)
                if (tokens(i)%kind == TK_EOF) then
                    end_pos = i
                    return
                end if
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

    ! Find the end of a program unit construct
    subroutine find_program_unit_range(tokens, start_pos, end_pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: end_pos

        integer :: i, lookahead
        character(len=:), allocatable :: start_keyword, end_keyword
        character(len=:), allocatable :: next_word

        if (start_pos > size(tokens)) then
            end_pos = start_pos
            return
        end if

        start_keyword = to_lower(trim(tokens(start_pos)%text))

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

        ! Find matching end statement
        do i = start_pos + 1, size(tokens)
            if (tokens(i)%kind == TK_KEYWORD .and. trim(tokens(i)%text) == "end") then
                ! Check if next token matches our end keyword
                if (i + 1 <= size(tokens) .and. tokens(i + 1)%kind == TK_KEYWORD .and. &
                    trim(tokens(i + 1)%text) == end_keyword) then
                    end_pos = i + 1
                    return
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
