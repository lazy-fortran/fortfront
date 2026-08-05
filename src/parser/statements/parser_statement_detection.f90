module parser_statement_detection_module
    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_OPERATOR, TK_IDENTIFIER, &
        TK_NEWLINE, TK_COMMENT, TK_WHITESPACE, to_lower
    implicit none
    private

    type :: statement_tracker_t
        integer :: if_depth = 0
        integer :: select_depth = 0
        integer :: do_depth = 0
        integer :: where_depth = 0
        integer :: assoc_depth = 0
        integer :: forall_depth = 0
        logical :: first_processed = .false.
        logical :: block_if = .false.
        character(len=16) :: first_keyword = ""
    end type statement_tracker_t

    public :: is_block_if, at_top_level, next_significant_index
    public :: inline_where_parenthetical, inline_where_colon, is_inline_where
    public :: find_statement_end, extend_if_statement_end, extend_do_statement_end
    public :: extend_block_statement_end

contains

    logical function is_block_if(tokens, start_index) result(is_block)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_index
        integer :: i

        is_block = .false.
        do i = start_index + 1, size(tokens)
            select case (tokens(i)%kind)
            case (TK_KEYWORD)
                if (to_lower(tokens(i)%text) == "then") then
                    is_block = .true.
                    return
                else
                    return
                end if
            case (TK_OPERATOR)
                if (tokens(i)%text == ";") return
            case (TK_NEWLINE, TK_EOF)
                return
            case (TK_COMMENT, TK_WHITESPACE)
                cycle
            case default
                cycle
            end select
        end do
    end function is_block_if

    pure logical function at_top_level(if_depth, select_depth, do_depth, &
            where_depth, assoc_depth, forall_depth) &
            result(is_top_level)
        integer, intent(in) :: if_depth, select_depth, do_depth
        integer, intent(in) :: where_depth, assoc_depth, forall_depth

        is_top_level = (if_depth == 0 .and. select_depth == 0 .and. &
            do_depth == 0 .and. where_depth == 0 .and. &
            assoc_depth == 0 .and. forall_depth == 0)
    end function at_top_level

    pure integer function next_significant_index(tokens, start_index) result(idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_index

        idx = start_index
        do while (idx <= size(tokens))
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                idx = idx + 1
            case default
                return
            end select
        end do
        idx = 0
    end function next_significant_index

    logical function inline_where_parenthetical(tokens, start_index) &
            result(is_inline)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_index
        integer :: i, depth

        is_inline = .false.
        depth = 1
        i = start_index
        do while (i <= size(tokens) .and. depth > 0)
            if (tokens(i)%kind == TK_OPERATOR) then
                select case (tokens(i)%text)
                case ("(")
                    depth = depth + 1
                case (")")
                    depth = depth - 1
                end select
            end if
            if (tokens(i)%kind == TK_EOF) return
            i = i + 1
        end do
        if (depth > 0) return

        i = next_significant_index(tokens, i)
        if (i == 0) return
        select case (tokens(i)%kind)
        case (TK_NEWLINE, TK_EOF)
            return
        case default
            is_inline = .true.
        end select
    end function inline_where_parenthetical

    logical function inline_where_colon(tokens, start_index) result(is_inline)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_index
        integer :: i, colon_index

        is_inline = .false.
        colon_index = 0
        i = start_index
        do while (i <= size(tokens))
            select case (tokens(i)%kind)
            case (TK_NEWLINE, TK_EOF)
                return
            case (TK_OPERATOR)
                if (tokens(i)%text == ":") then
                    colon_index = i + 1
                    exit
                end if
            end select
            i = i + 1
        end do
        if (colon_index == 0) return

        i = next_significant_index(tokens, colon_index)
        if (i == 0) return
        select case (tokens(i)%kind)
        case (TK_NEWLINE, TK_EOF)
            return
        case default
            is_inline = .true.
        end select
    end function inline_where_colon

    logical function is_inline_where(tokens, where_index) result(is_inline)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: where_index
        integer :: first_token

        is_inline = .false.
        if (where_index < 1 .or. where_index > size(tokens)) return
        first_token = next_significant_index(tokens, where_index + 1)
        if (first_token == 0) return

        if (tokens(first_token)%kind == TK_OPERATOR .and. &
            tokens(first_token)%text == "(") then
            is_inline = inline_where_parenthetical(tokens, first_token + 1)
        else
            is_inline = inline_where_colon(tokens, first_token)
        end if
    end function is_inline_where

    integer function find_statement_end(tokens, start_index) result(end_index)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_index

        type(statement_tracker_t) :: tracker
        integer :: idx
        type(token_t) :: token
        logical :: done, restart_iteration

        end_index = start_index
        if (start_index > size(tokens)) return

        idx = start_index
        do while (idx <= size(tokens))
            token = tokens(idx)
            done = .false.
            restart_iteration = .false.

            select case (token%kind)
            case (TK_EOF)
                end_index = idx - 1
                exit
            case (TK_NEWLINE)
                if (tracker_at_top_level(tracker)) then
                    end_index = idx - 1
                    exit
                end if
            case (TK_OPERATOR)
                if (token%text == ";" .and. tracker_at_top_level(tracker)) then
                    end_index = idx - 1
                    exit
                end if
            case (TK_COMMENT, TK_WHITESPACE)
                ! Skip insignificant tokens
            case (TK_KEYWORD)
                if (token%text == "end") then
                    call process_end_keyword(tokens, idx, tracker, end_index, &
                        done, restart_iteration)
                else
                    done = handle_regular_keyword(tokens, idx, tracker, end_index)
                end if
            case default
                call mark_first_processed(tracker)
            end select

            if (done) exit
            if (restart_iteration) cycle

            end_index = idx
            idx = idx + 1
        end do
    end function find_statement_end

    pure logical function tracker_at_top_level(tracker) result(is_top_level)
        type(statement_tracker_t), intent(in) :: tracker

        is_top_level = at_top_level(tracker%if_depth, tracker%select_depth, &
            tracker%do_depth, tracker%where_depth, tracker%assoc_depth, &
            tracker%forall_depth)
    end function tracker_at_top_level

    subroutine mark_first_processed(tracker)
        type(statement_tracker_t), intent(inout) :: tracker

        if (.not. tracker%first_processed) then
            tracker%first_processed = .true.
        end if
    end subroutine mark_first_processed

    subroutine set_first_keyword(tracker, keyword)
        type(statement_tracker_t), intent(inout) :: tracker
        character(len=*), intent(in) :: keyword

        if (.not. tracker%first_processed) then
            tracker%first_processed = .true.
            tracker%first_keyword = keyword
        end if
    end subroutine set_first_keyword

    subroutine record_generic_keyword(tracker, text)
        type(statement_tracker_t), intent(inout) :: tracker
        character(len=*), intent(in) :: text

        call set_first_keyword(tracker, to_lower(text))
    end subroutine record_generic_keyword

    subroutine handle_if_keyword(tokens, idx, tracker)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: idx
        type(statement_tracker_t), intent(inout) :: tracker
        logical :: block_if_stmt

        block_if_stmt = is_block_if(tokens, idx)
        if (.not. tracker%first_processed) then
            tracker%first_processed = .true.
            tracker%block_if = block_if_stmt
            if (block_if_stmt) tracker%if_depth = tracker%if_depth + 1
            tracker%first_keyword = "if"
        else
            if (block_if_stmt) tracker%if_depth = tracker%if_depth + 1
        end if
    end subroutine handle_if_keyword

    subroutine handle_select_keyword(tokens, idx, tracker)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: idx
        type(statement_tracker_t), intent(inout) :: tracker
        logical :: select_stmt

        select_stmt = is_select_construct(tokens, idx)
        if (select_stmt) then
            call set_first_keyword(tracker, "select")
            tracker%select_depth = tracker%select_depth + 1
        else
            call record_generic_keyword(tracker, tokens(idx)%text)
        end if
    end subroutine handle_select_keyword

    subroutine push_construct(tracker, keyword, depth)
        type(statement_tracker_t), intent(inout) :: tracker
        character(len=*), intent(in) :: keyword
        integer, intent(inout) :: depth

        call set_first_keyword(tracker, keyword)
        depth = depth + 1
    end subroutine push_construct

    subroutine handle_where_keyword(tokens, idx, tracker)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: idx
        type(statement_tracker_t), intent(inout) :: tracker
        logical :: inline_where_stmt

        call set_first_keyword(tracker, "where")
        inline_where_stmt = is_inline_where(tokens, idx)
        if (.not. inline_where_stmt) tracker%where_depth = tracker%where_depth + 1
    end subroutine handle_where_keyword

    logical function handle_else_keyword(tracker, idx, end_index) result(done)
        type(statement_tracker_t), intent(in) :: tracker
        integer, intent(in) :: idx
        integer, intent(inout) :: end_index

        done = .false.
        if (tracker%block_if) then
            if (tracker%if_depth == 1) then
                end_index = idx - 1
                done = .true.
            end if
        else if (tracker_at_top_level(tracker)) then
            end_index = idx - 1
            done = .true.
        end if
    end function handle_else_keyword

    logical function handle_elseif_keyword(tracker, idx, end_index) result(done)
        type(statement_tracker_t), intent(in) :: tracker
        integer, intent(in) :: idx
        integer, intent(inout) :: end_index

        done = .false.
        if (tracker%block_if .and. tracker%if_depth == 1) then
            end_index = idx - 1
            done = .true.
        else if (tracker_at_top_level(tracker)) then
            end_index = idx - 1
            done = .true.
        end if
    end function handle_elseif_keyword

    logical function close_if_construct(tracker, idx, end_index) result(done)
        type(statement_tracker_t), intent(inout) :: tracker
        integer, intent(in) :: idx
        integer, intent(inout) :: end_index

        done = .false.
        if (tracker%if_depth > 0) then
            tracker%if_depth = tracker%if_depth - 1
            if (tracker%if_depth == 0 .and. tracker%block_if) then
                end_index = idx
                done = .true.
            end if
        else
            end_index = idx - 1
            done = .true.
        end if
    end function close_if_construct

    logical function try_close_depth(depth, expected_keyword, first_keyword, idx, &
            end_index) result(done)
        integer, intent(inout) :: depth
        character(len=*), intent(in) :: expected_keyword
        character(len=*), intent(in) :: first_keyword
        integer, intent(in) :: idx
        integer, intent(inout) :: end_index

        done = .false.
        if (depth > 0) then
            depth = depth - 1
            if (depth == 0 .and. first_keyword == expected_keyword) then
                end_index = idx
                done = .true.
            end if
        else
            end_index = idx - 1
            done = .true.
        end if
    end function try_close_depth

    logical function handle_regular_keyword(tokens, idx, tracker, end_index) &
            result(done)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: idx
        type(statement_tracker_t), intent(inout) :: tracker
        integer, intent(inout) :: end_index
        type(token_t) :: token

        token = tokens(idx)
        done = .false.
        select case (token%text)
        case ("if")
            call handle_if_keyword(tokens, idx, tracker)
        case ("select")
            call handle_select_keyword(tokens, idx, tracker)
        case ("do")
            call push_construct(tracker, "do", tracker%do_depth)
        case ("where")
            call handle_where_keyword(tokens, idx, tracker)
        case ("forall")
            call push_construct(tracker, "forall", tracker%forall_depth)
        case ("associate")
            call push_construct(tracker, "associate", tracker%assoc_depth)
        case ("else")
            done = handle_else_keyword(tracker, idx, end_index)
        case ("elseif", "else if")
            done = handle_elseif_keyword(tracker, idx, end_index)
        case ("endif")
            done = close_if_construct(tracker, idx, end_index)
        case ("endselect")
            done = try_close_depth(tracker%select_depth, "select", &
                tracker%first_keyword, idx, end_index)
        case ("enddo")
            done = try_close_depth(tracker%do_depth, "do", tracker%first_keyword, &
                idx, end_index)
        case ("endwhere")
            done = try_close_depth(tracker%where_depth, "where", &
                tracker%first_keyword, idx, end_index)
        case ("endforall")
            done = try_close_depth(tracker%forall_depth, "forall", &
                tracker%first_keyword, idx, end_index)
        case ("endassociate")
            done = try_close_depth(tracker%assoc_depth, "associate", &
                tracker%first_keyword, idx, end_index)
        case default
            call record_generic_keyword(tracker, token%text)
        end select
    end function handle_regular_keyword

    subroutine process_end_keyword(tokens, idx, tracker, end_index, done, &
            restart_iteration)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: idx
        type(statement_tracker_t), intent(inout) :: tracker
        integer, intent(inout) :: end_index
        logical, intent(out) :: done
        logical, intent(out) :: restart_iteration
        integer :: next_idx
        type(token_t) :: next_token

        done = .false.
        restart_iteration = .false.
        next_idx = idx + 1
        if (next_idx > size(tokens)) then
            if (tracker_at_top_level(tracker)) then
                end_index = idx - 1
                done = .true.
            end if
            return
        end if

        next_token = tokens(next_idx)
        if (next_token%kind /= TK_KEYWORD) then
            if (tracker_at_top_level(tracker)) then
                end_index = idx - 1
                done = .true.
            end if
            return
        end if

        select case (next_token%text)
        case ("if")
            idx = next_idx
            restart_iteration = .true.
            done = close_if_construct(tracker, idx, end_index)
        case ("select")
            idx = next_idx
            restart_iteration = .true.
            done = try_close_depth(tracker%select_depth, "select", &
                tracker%first_keyword, idx, end_index)
        case ("do")
            idx = next_idx
            restart_iteration = .true.
            done = try_close_depth(tracker%do_depth, "do", tracker%first_keyword, &
                idx, end_index)
        case ("associate")
            idx = next_idx
            restart_iteration = .true.
            done = try_close_depth(tracker%assoc_depth, "associate", &
                tracker%first_keyword, idx, end_index)
        case ("where")
            idx = next_idx
            restart_iteration = .true.
            done = try_close_depth(tracker%where_depth, "where", &
                tracker%first_keyword, idx, end_index)
        case ("forall")
            idx = next_idx
            restart_iteration = .true.
            done = try_close_depth(tracker%forall_depth, "forall", &
                tracker%first_keyword, idx, end_index)
        case default
            if (tracker_at_top_level(tracker)) then
                end_index = idx - 1
                done = .true.
            end if
        end select
    end subroutine process_end_keyword

    pure logical function is_select_construct(tokens, select_index) &
            result(is_select)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: select_index
        integer :: idx, max_idx
        character(len=:), allocatable :: lowered

        is_select = .false.
        if (select_index < 1 .or. select_index > size(tokens)) return

        idx = select_index + 1
        max_idx = min(size(tokens), select_index + 100)
        do while (idx <= max_idx)
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                idx = idx + 1
            case (TK_OPERATOR)
                if (tokens(idx)%text == "&") then
                    idx = idx + 1
                else
                    return
                end if
            case (TK_KEYWORD)
                lowered = to_lower(tokens(idx)%text)
                if (lowered == "case" .or. lowered == "type" .or. &
                    lowered == "rank") then
                    is_select = .true.
                end if
                return
            case default
                return
            end select
        end do
    end function is_select_construct

    integer function extend_do_statement_end(tokens, start_index, initial_end) &
            result(end_index)
        !! Extend a statement slice over a whole DO construct.
        !!
        !! An if body is parsed one statement at a time, and a block `if` is
        !! already extended to cover its `end if` so the construct is handed
        !! over whole. A `do` had no such extension: only its header was taken,
        !! and its body and `end do` were parsed as loose statements. A single
        !! loop happened to resynchronise; two nested ones did not, and the
        !! `end if` that followed was reported as an unrecognised statement.
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_index
        integer, intent(in) :: initial_end
        integer :: idx, depth
        type(token_t) :: token

        end_index = initial_end
        if (start_index < 1 .or. start_index > size(tokens)) return
        if (initial_end < start_index) return
        if (tokens(start_index)%kind /= TK_KEYWORD) return
        if (to_lower(trim(tokens(start_index)%text)) /= "do") return

        depth = 0
        idx = start_index
        do while (idx <= size(tokens))
            token = tokens(idx)
            if (token%kind == TK_KEYWORD) then
                select case (to_lower(trim(token%text)))
                case ("do")
                    depth = depth + 1
                case ("enddo")
                    depth = depth - 1
                    if (depth <= 0) then
                        end_index = idx
                        return
                    end if
                case ("end")
                    if (idx + 1 <= size(tokens)) then
                        if (tokens(idx + 1)%kind == TK_KEYWORD) then
                            if (to_lower(trim(tokens(idx + 1)%text)) == "do") then
                                depth = depth - 1
                                if (depth <= 0) then
                                    end_index = idx + 1
                                    return
                                end if
                                idx = idx + 1
                            end if
                        end if
                    end if
                end select
            end if
            idx = idx + 1
        end do
    end function extend_do_statement_end

    integer function extend_block_statement_end(tokens, start_index, initial_end) &
            result(end_index)
        !! Extend a statement slice over a whole BLOCK construct.
        !!
        !! Same reason as the DO extension above. Only the bare `block` header
        !! was taken, so the construct's declarations and its `end block` were
        !! parsed as loose statements and reported as unrecognized.
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_index
        integer, intent(in) :: initial_end
        integer :: idx, depth
        type(token_t) :: token

        end_index = initial_end
        if (start_index < 1 .or. start_index > size(tokens)) return
        if (initial_end < start_index) return
        if (tokens(start_index)%kind /= TK_KEYWORD) return
        if (to_lower(trim(tokens(start_index)%text)) /= "block") return
        ! `block data` is a program unit, not an executable construct.
        if (next_keyword_is(tokens, start_index, "data")) return

        depth = 0
        idx = start_index
        do while (idx <= size(tokens))
            token = tokens(idx)
            if (token%kind == TK_KEYWORD) then
                select case (to_lower(trim(token%text)))
                case ("block")
                    if (.not. next_keyword_is(tokens, idx, "data")) then
                        depth = depth + 1
                    end if
                case ("endblock")
                    depth = depth - 1
                    if (depth <= 0) then
                        end_index = idx
                        return
                    end if
                case ("end")
                    if (next_keyword_is(tokens, idx, "block")) then
                        depth = depth - 1
                        if (depth <= 0) then
                            end_index = idx + 1
                            return
                        end if
                        idx = idx + 1
                    end if
                end select
            end if
            idx = idx + 1
        end do
    end function extend_block_statement_end

    logical function next_keyword_is(tokens, idx, word) result(matches)
        !! Whether the next token, skipping trivia, is the given keyword.
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: idx
        character(len=*), intent(in) :: word

        integer :: j

        matches = .false.
        j = idx + 1
        do while (j <= size(tokens))
            select case (tokens(j)%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                j = j + 1
                cycle
            case (TK_KEYWORD, TK_IDENTIFIER)
                matches = to_lower(trim(tokens(j)%text)) == word
                return
            case default
                return
            end select
        end do
    end function next_keyword_is

    integer function extend_if_statement_end(tokens, start_index, initial_end) &
            result(end_index)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_index
        integer, intent(in) :: initial_end

        integer :: idx, depth
        type(token_t) :: token, next_token
        logical :: block_if_stmt, last_token_was_else

        end_index = initial_end
        if (start_index < 1 .or. start_index > size(tokens)) return
        if (initial_end < start_index) return

        block_if_stmt = is_block_if(tokens, start_index)
        if (.not. block_if_stmt) return

        depth = 0
        last_token_was_else = .false.

        idx = start_index
        do while (idx <= size(tokens))
            token = tokens(idx)

            select case (token%kind)
            case (TK_KEYWORD)
                select case (token%text)
                case ("if")
                    if (last_token_was_else) then
                        last_token_was_else = .false.
                    else if (is_block_if(tokens, idx)) then
                        depth = depth + 1
                    end if
                case ("else")
                    last_token_was_else = .true.
                case ("elseif", "else if")
                    last_token_was_else = .false.
                case ("endif")
                    last_token_was_else = .false.
                    if (depth > 0) then
                        depth = depth - 1
                        if (depth == 0) then
                            end_index = idx
                            return
                        end if
                    end if
                case ("end")
                    last_token_was_else = .false.
                    if (idx + 1 <= size(tokens)) then
                        next_token = tokens(idx + 1)
                        if (next_token%kind == TK_KEYWORD .and. &
                            next_token%text == "if") then
                            if (depth > 0) then
                                depth = depth - 1
                                if (depth == 0) then
                                    end_index = idx + 1
                                    return
                                end if
                            end if
                            idx = idx + 1
                        end if
                    end if
                case default
                    last_token_was_else = .false.
                end select
            case (TK_WHITESPACE, TK_COMMENT)
                ! Preserve last_token_was_else for whitespace/comments
            case default
                last_token_was_else = .false.
            end select

            idx = idx + 1
        end do
    end function extend_if_statement_end

end module parser_statement_detection_module
