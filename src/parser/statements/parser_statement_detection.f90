module parser_statement_detection_module
    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD, TK_OPERATOR, &
                          TK_NEWLINE, TK_COMMENT, TK_WHITESPACE, to_lower
    implicit none
    private

    public :: is_block_if, at_top_level, next_significant_index
    public :: inline_where_parenthetical, inline_where_colon, is_inline_where
    public :: find_statement_end, extend_if_statement_end

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

        integer :: idx
        integer :: if_depth, select_depth, do_depth, where_depth, assoc_depth
        integer :: forall_depth
        logical :: first_processed, block_if
        character(len=16) :: first_keyword
        type(token_t) :: token, next_token

        end_index = start_index
        if (start_index > size(tokens)) return

        if_depth = 0
        select_depth = 0
        do_depth = 0
        where_depth = 0
        assoc_depth = 0
        forall_depth = 0
        first_processed = .false.
        block_if = .false.
        first_keyword = ""

        idx = start_index
        do while (idx <= size(tokens))
            token = tokens(idx)

            select case (token%kind)
            case (TK_EOF)
                end_index = idx - 1
                exit
            case (TK_NEWLINE)
                if (at_top_level( &
                    if_depth, select_depth, do_depth, where_depth, &
                    assoc_depth, forall_depth)) then
                    end_index = idx - 1
                    exit
                end if
            case (TK_OPERATOR)
                if (token%text == ";") then
                    if (at_top_level( &
                        if_depth, select_depth, do_depth, where_depth, &
                        assoc_depth, forall_depth)) then
                        end_index = idx - 1
                        exit
                    end if
                end if
            case (TK_COMMENT, TK_WHITESPACE)
                ! Ignore spacing tokens
            case (TK_KEYWORD)
                select case (token%text)
                case ("if")
                    if (.not. first_processed) then
                        first_processed = .true.
                        block_if = is_block_if(tokens, idx)
                        if (block_if) if_depth = if_depth + 1
                        first_keyword = "if"
                    else
                        if (is_block_if(tokens, idx)) then
                            if_depth = if_depth + 1
                        end if
                    end if
                case ("select")
                    if (.not. is_select_construct(tokens, idx)) then
                        if (.not. first_processed) then
                            first_processed = .true.
                            first_keyword = to_lower(token%text)
                        end if
                        cycle
                    end if
                    if (.not. first_processed) then
                        first_processed = .true.
                        first_keyword = "select"
                    end if
                    select_depth = select_depth + 1
                case ("do")
                    if (.not. first_processed) then
                        first_processed = .true.
                        first_keyword = "do"
                    end if
                    do_depth = do_depth + 1
                case ("where")
                    if (.not. first_processed) then
                        first_processed = .true.
                        first_keyword = "where"
                    end if
                    block
                        logical :: inline_where_stmt
                        inline_where_stmt = is_inline_where(tokens, idx)
                        if (.not. inline_where_stmt) then
                            where_depth = where_depth + 1
                        end if
                    end block
                case ("forall")
                    if (.not. first_processed) then
                        first_processed = .true.
                        first_keyword = "forall"
                    end if
                    forall_depth = forall_depth + 1
                case ("associate")
                    if (.not. first_processed) then
                        first_processed = .true.
                        first_keyword = "associate"
                    end if
                    assoc_depth = assoc_depth + 1
                case ("else")
                    if (block_if) then
                        if (idx + 1 <= size(tokens)) then
                            if (tokens(idx + 1)%kind == TK_KEYWORD .and. &
                                tokens(idx + 1)%text == "if") then
                                if (if_depth == 1) then
                                    end_index = idx - 1
                                    exit
                                end if
                            else
                                if (if_depth == 1) then
                                    end_index = idx - 1
                                    exit
                                end if
                            end if
                        else
                            if (if_depth == 1) then
                                end_index = idx - 1
                                exit
                            end if
                        end if
                    else if (at_top_level( &
                             if_depth, select_depth, do_depth, where_depth, &
                             assoc_depth, forall_depth)) then
                        end_index = idx - 1
                        exit
                    end if
                case ("elseif", "else if")
                    if (block_if .and. if_depth == 1) then
                        end_index = idx - 1
                        exit
                    else if (at_top_level( &
                             if_depth, select_depth, do_depth, where_depth, &
                             assoc_depth, forall_depth)) then
                        end_index = idx - 1
                        exit
                    end if
                case ("endif")
                    if (if_depth > 0) then
                        if_depth = if_depth - 1
                        if (if_depth == 0 .and. block_if) then
                            end_index = idx
                            exit
                        end if
                    else
                        end_index = idx - 1
                        exit
                    end if
                case ("endselect")
                    if (select_depth > 0) then
                        select_depth = select_depth - 1
                        if (select_depth == 0 .and. first_keyword == "select") then
                            end_index = idx
                            exit
                        end if
                    else
                        end_index = idx - 1
                        exit
                    end if
                case ("enddo")
                    if (do_depth > 0) then
                        do_depth = do_depth - 1
                        if (do_depth == 0 .and. first_keyword == "do") then
                            end_index = idx
                            exit
                        end if
                    else
                        end_index = idx - 1
                        exit
                    end if
                case ("endwhere")
                    if (where_depth > 0) then
                        where_depth = where_depth - 1
                        if (where_depth == 0 .and. first_keyword == "where") then
                            end_index = idx
                            exit
                        end if
                    else
                        end_index = idx - 1
                        exit
                    end if
                case ("endforall")
                    if (forall_depth > 0) then
                        forall_depth = forall_depth - 1
                        if (forall_depth == 0 .and. first_keyword == "forall") then
                            end_index = idx
                            exit
                        end if
                    else
                        end_index = idx - 1
                        exit
                    end if
                case ("endassociate")
                    if (assoc_depth > 0) then
                        assoc_depth = assoc_depth - 1
                        if (assoc_depth == 0 .and. first_keyword == "associate") then
                            end_index = idx
                            exit
                        end if
                    else
                        end_index = idx - 1
                        exit
                    end if
                case ("end")
                    if (idx + 1 <= size(tokens)) then
                        if (tokens(idx + 1)%kind == TK_KEYWORD) then
                            next_token = tokens(idx + 1)
                            select case (next_token%text)
                            case ("if")
                                if (if_depth > 0) then
                                    if_depth = if_depth - 1
                                    if (if_depth == 0 .and. block_if) then
                                        end_index = idx + 1
                                        exit
                                    end if
                                else
                                    end_index = idx - 1
                                    exit
                                end if
                                idx = idx + 1
                                cycle
                            case ("select")
                                if (select_depth > 0) then
                                    select_depth = select_depth - 1
                                    if (select_depth == 0 .and. &
                                        first_keyword == "select") then
                                        end_index = idx + 1
                                        exit
                                    end if
                                else
                                    end_index = idx - 1
                                    exit
                                end if
                                idx = idx + 1
                                cycle
                            case ("do")
                                if (do_depth > 0) then
                                    do_depth = do_depth - 1
                                    if (do_depth == 0 .and. first_keyword == "do") then
                                        end_index = idx + 1
                                        exit
                                    end if
                                else
                                    end_index = idx - 1
                                    exit
                                end if
                                idx = idx + 1
                                cycle
                            case ("associate")
                                if (assoc_depth > 0) then
                                    assoc_depth = assoc_depth - 1
                                    if (assoc_depth == 0 .and. &
                                        first_keyword == "associate") then
                                        end_index = idx + 1
                                        exit
                                    end if
                                else
                                    end_index = idx - 1
                                    exit
                                end if
                                idx = idx + 1
                                cycle
                            case ("where")
                                if (where_depth > 0) then
                                    where_depth = where_depth - 1
                                    if (where_depth == 0 .and. &
                                        first_keyword == "where") then
                                        end_index = idx + 1
                                        exit
                                    end if
                                else
                                    end_index = idx - 1
                                    exit
                                end if
                                idx = idx + 1
                                cycle
                            case ("forall")
                                if (forall_depth > 0) then
                                    forall_depth = forall_depth - 1
                                    if (forall_depth == 0 .and. &
                                        first_keyword == "forall") then
                                        end_index = idx + 1
                                        exit
                                    end if
                                else
                                    end_index = idx - 1
                                    exit
                                end if
                                idx = idx + 1
                                cycle
                            case default
                                if (at_top_level( &
                                    if_depth, select_depth, do_depth, where_depth, &
                                    assoc_depth, forall_depth)) then
                                    end_index = idx - 1
                                    exit
                                end if
                            end select
                        else
                            if (at_top_level( &
                                if_depth, select_depth, do_depth, where_depth, &
                                assoc_depth, forall_depth)) then
                                end_index = idx - 1
                                exit
                            end if
                        end if
                    else
                        if (at_top_level( &
                            if_depth, select_depth, do_depth, where_depth, &
                            assoc_depth, forall_depth)) then
                            end_index = idx - 1
                            exit
                        end if
                    end if
                case default
                    if (.not. first_processed) then
                        first_processed = .true.
                        first_keyword = to_lower(token%text)
                    end if
                end select
            case default
                if (.not. first_processed) then
                    first_processed = .true.
                end if
            end select

            end_index = idx
            idx = idx + 1
        end do
    end function find_statement_end

    pure logical function is_select_construct(tokens, select_index) &
        result(is_select)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: select_index
        integer :: idx
        logical :: pending_continuation
        character(len=:), allocatable :: lowered

        is_select = .false.
        if (select_index < 1 .or. select_index > size(tokens)) return

        idx = select_index + 1
        pending_continuation = .false.
        do while (idx <= size(tokens))
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                idx = idx + 1
            case (TK_OPERATOR)
                if (tokens(idx)%text == "&") then
                    pending_continuation = .true.
                    idx = idx + 1
                else
                    return
                end if
            case (TK_NEWLINE)
                if (.not. pending_continuation) return
                pending_continuation = .false.
                idx = idx + 1
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
