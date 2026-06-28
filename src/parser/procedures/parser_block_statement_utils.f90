module parser_block_statement_utils_module
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, &
        TK_WHITESPACE, TK_COMMENT
    implicit none
    private

    public :: is_if_statement_start
    public :: locate_block_statement_end
    public :: locate_single_line_end

contains

    logical function is_if_statement_start(first_token) result(is_if_start)
        type(token_t), intent(in) :: first_token
        character(len=:), allocatable :: token_lower

        is_if_start = first_token%kind == TK_KEYWORD
        if (is_if_start) then
            token_lower = to_lower(first_token%text)
            is_if_start = trim(token_lower) == "if" .or. &
                trim(token_lower) == "do" .or. &
                trim(token_lower) == "select" .or. &
                trim(token_lower) == "associate" .or. &
                trim(token_lower) == "block"
        end if
    end function is_if_statement_start

    logical function handle_if_block_keyword(all_tokens, pos, depth, stmt_end) &
            result(completed)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: pos
        integer, intent(inout) :: depth
        integer, intent(inout) :: stmt_end

        logical :: preceded_by_end
        logical :: preceded_by_else

        completed = .false.
        select case (all_tokens(pos)%text)
        case ("if")
            preceded_by_end = .false.
            preceded_by_else = .false.
            if (pos > 1) then
                if (all_tokens(pos - 1)%kind == TK_KEYWORD) then
                    preceded_by_end = all_tokens(pos - 1)%text == "end"
                    preceded_by_else = all_tokens(pos - 1)%text == "else"
                end if
            end if
            if (.not. preceded_by_end .and. .not. preceded_by_else) then
                depth = depth + 1
            end if
        case ("end")
            if (pos < size(all_tokens)) then
                if (all_tokens(pos + 1)%kind == TK_KEYWORD) then
                    if (all_tokens(pos + 1)%text == "if") then
                        depth = depth - 1
                        if (depth <= 0) then
                            stmt_end = min(size(all_tokens), pos + 1)
                            completed = .true.
                        end if
                    end if
                end if
            end if
        end select
    end function handle_if_block_keyword

    logical function handle_do_block_keyword(all_tokens, pos, depth, stmt_end) &
            result(completed)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: pos
        integer, intent(inout) :: depth
        integer, intent(inout) :: stmt_end

        completed = .false.
        select case (all_tokens(pos)%text)
        case ("do")
            depth = depth + 1
        case ("enddo", "end do")
            depth = depth - 1
            if (depth <= 0) then
                stmt_end = min(size(all_tokens), pos)
                completed = .true.
            end if
        case ("end")
            if (pos + 1 <= size(all_tokens)) then
                if (all_tokens(pos + 1)%kind == TK_KEYWORD .and. &
                    all_tokens(pos + 1)%text == "do") then
                    depth = depth - 1
                    if (depth <= 0) then
                        stmt_end = min(size(all_tokens), pos + 1)
                        completed = .true.
                    end if
                end if
            end if
        end select
    end function handle_do_block_keyword

    logical function handle_select_block_keyword(all_tokens, pos, depth, stmt_end) &
            result(completed)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: pos
        integer, intent(inout) :: depth
        integer, intent(inout) :: stmt_end

        completed = .false.
        select case (to_lower(all_tokens(pos)%text))
        case ("select")
            depth = depth + 1
        case ("end")
            if (pos + 1 <= size(all_tokens)) then
                if (all_tokens(pos + 1)%kind == TK_KEYWORD .and. &
                    to_lower(all_tokens(pos + 1)%text) == "select") then
                    depth = depth - 1
                    if (depth <= 0) then
                        stmt_end = min(size(all_tokens), pos + 1)
                        completed = .true.
                    end if
                end if
            end if
        end select
    end function handle_select_block_keyword

    logical function handle_associate_block_keyword(all_tokens, pos, depth, &
            stmt_end) result(completed)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: pos
        integer, intent(inout) :: depth
        integer, intent(inout) :: stmt_end

        completed = .false.
        select case (to_lower(all_tokens(pos)%text))
        case ("associate")
            depth = depth + 1
        case ("end")
            if (pos + 1 <= size(all_tokens)) then
                if (all_tokens(pos + 1)%kind == TK_KEYWORD .and. &
                    to_lower(all_tokens(pos + 1)%text) == "associate") then
                    depth = depth - 1
                    if (depth <= 0) then
                        stmt_end = min(size(all_tokens), pos + 1)
                        completed = .true.
                    end if
                end if
            end if
        end select
    end function handle_associate_block_keyword

    logical function handle_block_construct_keyword(all_tokens, pos, depth, &
            stmt_end) result(completed)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: pos
        integer, intent(inout) :: depth
        integer, intent(inout) :: stmt_end

        completed = .false.
        select case (to_lower(all_tokens(pos)%text))
        case ("block")
            depth = depth + 1
        case ("endblock")
            depth = depth - 1
            if (depth <= 0) then
                stmt_end = min(size(all_tokens), pos)
                completed = .true.
            end if
        case ("end")
            if (pos + 1 <= size(all_tokens)) then
                if (all_tokens(pos + 1)%kind == TK_KEYWORD .and. &
                    to_lower(all_tokens(pos + 1)%text) == "block") then
                    depth = depth - 1
                    if (depth <= 0) then
                        stmt_end = min(size(all_tokens), pos + 1)
                        completed = .true.
                    end if
                end if
            end if
        end select
    end function handle_block_construct_keyword

    integer function locate_block_statement_end(all_tokens, stmt_start, &
            stmt_type) result(stmt_end)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: stmt_start
        character(len=*), intent(in) :: stmt_type

        integer :: pos
        integer :: depth

        stmt_end = stmt_start
        pos = stmt_start

        if (stmt_type == "if") then
            if (is_single_line_if_statement(all_tokens, stmt_start)) then
                stmt_end = locate_single_line_end(all_tokens, stmt_start, &
                    all_tokens(stmt_start)%line)
                return
            end if
        end if

        if (stmt_type == "do" .or. to_lower(stmt_type) == "select" .or. &
            to_lower(stmt_type) == "associate" .or. &
            to_lower(stmt_type) == "block") then
            depth = 1
            pos = stmt_start + 1
        else
            depth = 0
            pos = stmt_start
        end if

        do while (pos <= size(all_tokens))
            if (all_tokens(pos)%kind == TK_KEYWORD) then
                select case (to_lower(stmt_type))
                case ("if")
                    if (handle_if_block_keyword(all_tokens, pos, depth, stmt_end)) &
                        return
                case ("do")
                    if (handle_do_block_keyword(all_tokens, pos, depth, stmt_end)) &
                        return
                case ("select")
                    if (handle_select_block_keyword(all_tokens, pos, depth, &
                        stmt_end)) return
                case ("associate")
                    if (handle_associate_block_keyword(all_tokens, pos, depth, &
                        stmt_end)) return
                case ("block")
                    if (handle_block_construct_keyword(all_tokens, pos, depth, &
                        stmt_end)) return
                end select
            end if
            stmt_end = pos
            pos = pos + 1
        end do
    end function locate_block_statement_end

    logical function is_single_line_if_statement(all_tokens, stmt_start) &
            result(is_single_line)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: stmt_start

        integer :: pos
        integer :: paren_depth
        logical :: pending_continuation
        character(len=:), allocatable :: token_text

        is_single_line = .true.
        paren_depth = 0
        pending_continuation = .false.

        do pos = stmt_start + 1, size(all_tokens)
            select case (all_tokens(pos)%kind)
            case (TK_OPERATOR)
                token_text = all_tokens(pos)%text
                if (token_text == "(") then
                    paren_depth = paren_depth + 1
                else if (token_text == ")") then
                    paren_depth = max(0, paren_depth - 1)
                else if (paren_depth == 0) then
                    if (token_text == "&") then
                        pending_continuation = .true.
                    else
                        pending_continuation = .false.
                    end if
                end if
            case (TK_KEYWORD)
                if (paren_depth == 0) then
                    token_text = all_tokens(pos)%text
                    select case (token_text)
                    case ("then")
                        is_single_line = .false.
                        return
                    case ("if")
                    case default
                    end select
                end if
            case (TK_NEWLINE, TK_COMMENT)
                if (paren_depth == 0 .and. .not. pending_continuation) then
                    return
                end if
            case default
            end select
        end do
    end function is_single_line_if_statement

    integer function locate_single_line_end(all_tokens, stmt_start, stmt_line) &
            result(stmt_end)
        type(token_t), intent(in) :: all_tokens(:)
        integer, intent(in) :: stmt_start
        integer, intent(in) :: stmt_line

        integer :: pos

        stmt_end = stmt_start
        do pos = stmt_start, size(all_tokens)
            if (all_tokens(pos)%line /= stmt_line) then
                stmt_end = pos - 1
                return
            end if
        end do
        stmt_end = size(all_tokens)
    end function locate_single_line_end

end module parser_block_statement_utils_module
