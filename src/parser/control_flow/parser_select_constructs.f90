module parser_select_constructs_module
    ! Parser module for SELECT CASE and SELECT TYPE constructs
    use, intrinsic :: iso_fortran_env, only: error_unit
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
                          TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, TK_WHITESPACE
    use ast_types, only: LITERAL_STRING
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_expressions_module, only: parse_expression_until
    use parser_io_statements_module, only: &
        parse_print_statement, parse_write_statement, parse_read_statement
    use parser_control_statements_module, only: &
        parse_cycle_statement, parse_exit_statement, parse_return_statement, &
        parse_stop_statement, parse_goto_statement, parse_error_stop_statement
    use parser_declarations, only: parse_declaration, parse_multi_declaration
    use parser_utils, only: analyze_declaration_structure
    use parser_basic_statement_module, only: parse_statement_body
    use parser_statement_core_module, only: statement_callbacks_t, &
                                            null_statement_callbacks
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_select_case, push_select_case_with_default, &
                           push_case_block, push_case_range, push_case_default, &
                           push_select_type, push_select_type_with_default, &
                           push_type_guard_block, &
                           push_select_rank, push_select_rank_with_default, &
                           push_rank_block, &
                           push_identifier, push_literal, push_assignment, &
                           push_pointer_assignment
    implicit none
    private

    public :: parse_select_case, parse_select_type, parse_select_rank

contains

    subroutine parse_case_value_list(parser, arena, case_token, value_indices, success)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: case_token
        integer, allocatable, intent(out) :: value_indices(:)
        logical, intent(out) :: success

        type(token_t), allocatable :: tokens_buffer(:)
        type(token_t) :: token, sub_token
        type(parser_state_t) :: sub_parser
        integer :: lower_index, upper_index, range_index
        integer :: start_pos, end_pos, depth, i, buffer_len

        success = .true.
        allocate (value_indices(0))

        start_pos = parser%current_token
        depth = 1
        end_pos = start_pos - 1

        do i = start_pos, size(parser%tokens)
            token = parser%tokens(i)
            if (token%kind == TK_OPERATOR) then
                select case (token%text)
                case ("(")
                    depth = depth + 1
                case (")")
                    depth = depth - 1
                    if (depth == 0) then
                        end_pos = i - 1
                        exit
                    end if
                end select
            end if
        end do

        if (end_pos < start_pos) then
            success = .false.
            if (depth == 0) parser%current_token = end_pos + 2
            return
        end if

        buffer_len = end_pos - start_pos + 2
        allocate (tokens_buffer(buffer_len))
        tokens_buffer(1:buffer_len - 1) = parser%tokens(start_pos:end_pos)
        tokens_buffer(buffer_len)%kind = TK_EOF
        tokens_buffer(buffer_len)%text = ""
        tokens_buffer(buffer_len)%line = parser%tokens(end_pos)%line
        tokens_buffer(buffer_len)%column = parser%tokens(end_pos)%column

        sub_parser = create_parser_state(tokens_buffer)

        success = .true.

        do
            sub_token = sub_parser%peek()
            if (sub_token%kind == TK_OPERATOR .and. sub_token%text == ":") then
                ! Leading colon: one-sided range with no lower bound (:hi).
                lower_index = 0
            else
                lower_index = parse_expression_until(sub_parser, arena, &
                                                     [":", ","])
                if (lower_index <= 0) then
                    success = .false.
                    exit
                end if
                sub_token = sub_parser%peek()
            end if

            if (sub_token%kind == TK_OPERATOR .and. sub_token%text == ":") then
                sub_token = sub_parser%consume()
                upper_index = parse_expression_until(sub_parser, arena, [","])
                if (upper_index <= 0) then
                    upper_index = 0
                end if
                range_index = push_case_range( &
                              arena, lower_index, upper_index, line=case_token%line, &
                              column=case_token%column)
                value_indices = [value_indices, range_index]
                sub_token = sub_parser%peek()
            else
                value_indices = [value_indices, lower_index]
            end if

            if (sub_token%kind == TK_OPERATOR .and. sub_token%text == ",") then
                sub_token = sub_parser%consume()
                cycle
            else
                exit
            end if
        end do

        if (.not. success .or. size(value_indices) == 0) then
            value_indices = [integer ::]
            success = .false.
        end if

        parser%current_token = end_pos + 2
    end subroutine parse_case_value_list

    recursive function parse_select_case(parser, arena) result(select_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: select_index

        type(token_t) :: select_token, case_token, lparen_token, rparen_token
        integer :: expr_index, default_index
        integer, allocatable :: case_indices(:)
        integer :: case_count, line, column
        character(len=:), allocatable :: keyword
        character(len=20), dimension(2) :: end_keywords
        type(statement_callbacks_t) :: callbacks
        logical :: values_ok

        ! Consume select
        select_token = parser%consume()
        line = select_token%line
        column = select_token%column

        ! Expect case
        case_token = parser%consume()
        if (case_token%kind /= TK_KEYWORD .or. to_lower(case_token%text) /= "case") then
            ! Error: expected case after select
            select_index = 0
            return
        end if

        ! Expect (
        lparen_token = parser%consume()
        if (lparen_token%kind /= TK_OPERATOR .or. lparen_token%text /= "(") then
            ! Error: expected ( after select case
            select_index = 0
            return
        end if

        ! Parse expression to match
        expr_index = parse_expression_until(parser, arena, [")"])
        if (expr_index <= 0) then
            select_index = 0
            return
        end if

        rparen_token = parser%peek()
        if (rparen_token%kind /= TK_OPERATOR .or. rparen_token%text /= ")") then
            select_index = 0
            return
        end if
        rparen_token = parser%consume()

        ! Parse case blocks
        allocate (case_indices(0))
        case_count = 0
        default_index = 0

        ! Define end keywords for statement parsing. "end select" terminates a
        ! case body so the last arm (which has no following "case") stops there
        ! instead of consuming statements after the construct. The "end select"
        ! form matches only end + select, not nested "end if"/"end do", which
        ! are consumed inside their own recursive statement parse.
        end_keywords = [character(len=20) :: "case", "end select"]

        callbacks = null_statement_callbacks()
        callbacks%parse_select_case => parse_select_case

        do while (parser%current_token <= size(parser%tokens))
            case_token = parser%peek()

            if (case_token%kind == TK_KEYWORD) then
                keyword = trim(to_lower(case_token%text))
                select case (keyword)
                case ("case")
                    ! Parse case block
                    block
                        type(token_t) :: value_token
                        integer :: case_block_index
                        integer, allocatable :: value_indices(:), body_indices(:)

                        case_token = parser%consume()  ! consume case

                        ! Check for default case
                        value_token = parser%peek()
                        do while (value_token%kind == TK_WHITESPACE .or. &
                                  value_token%kind == TK_COMMENT .or. &
                                  value_token%kind == TK_NEWLINE)
                            value_token = parser%consume()
                            if (parser%is_at_end()) exit
                            value_token = parser%peek()
                        end do

                        if (value_token%kind == TK_KEYWORD .and. &
                            trim(to_lower(value_token%text)) == "default") then
                            value_token = parser%consume()  ! consume default

                            ! Skip rest of current line
                            block
                                integer :: current_line
                                type(token_t) :: skip_token
                                current_line = value_token%line
                                do while (parser%current_token <= size(parser%tokens))
                                    skip_token = parser%peek()
                                    if (skip_token%line == current_line) then
                                        skip_token = parser%consume()
                                    else
                                        exit
                                    end if
                                end do
                            end block

                            ! Parse default case body using parse_statement_body
                            body_indices = parse_statement_body(parser, arena, &
                                                                end_keywords, callbacks)

                            ! Store default case index
                            default_index = push_case_default(arena, body_indices, &
                                                              line=case_token%line, &
                                                              column=case_token%column)
                        else
                            ! Regular case with values
                            allocate (value_indices(0))
                            if (value_token%kind == TK_OPERATOR .and. &
                                value_token%text == "(") then

                                value_token = parser%consume()  ! consume (
                                call parse_case_value_list(parser, arena, case_token, &
                                                           value_indices, values_ok)
                                if (.not. values_ok) then
                                    value_indices = [integer ::]
                                    select_index = 0
                                    return
                                end if
                            else
                                value_indices = [integer ::]
                                select_index = 0
                                return
                            end if

                            ! Skip rest of current line
                            block
                                integer :: current_line
                                type(token_t) :: skip_token

                                ! Get the current line number
                                if (parser%current_token > 1) then
                                    current_line = parser%tokens( &
                                                   parser%current_token - 1)%line
                                else
                                    current_line = 1
                                end if

                                ! Skip all tokens on current line
                                do while (parser%current_token <= size(parser%tokens))
                                    skip_token = parser%peek()
                                    if (skip_token%line == current_line) then
                                        skip_token = parser%consume()
                                    else
                                        exit
                                    end if
                                end do
                            end block

                            ! Parse case body statements using parse_statement_body
                            body_indices = parse_statement_body(parser, arena, &
                                                                end_keywords, callbacks)

                            ! Create case block node and add to list
                            if (size(value_indices) > 0) then
                                case_block_index = push_case_block( &
                                                   arena, value_indices, body_indices, &
                                                   line=case_token%line, &
                                                   column=case_token%column)
                                case_indices = [case_indices, case_block_index]
                                case_count = case_count + 1
                            end if
                        end if
                    end block
                case ("end")
                    ! Check for end select (skip whitespace/comments)
                    block
                        type(token_t) :: next_token

                        case_token = parser%consume()  ! consume end

                        next_token = parser%peek()
                        do while (next_token%kind == TK_WHITESPACE .or. &
                                  next_token%kind == TK_COMMENT .or. &
                                  next_token%kind == TK_NEWLINE)
                            next_token = parser%consume()
                            if (parser%is_at_end()) exit
                            next_token = parser%peek()
                        end do

                        if (next_token%kind == TK_KEYWORD .and. &
                            trim(to_lower(next_token%text)) == "select") then
                            next_token = parser%consume()  ! consume select
                            exit
                        end if
                    end block
                case default
                    ! Other keyword, skip
                    parser%current_token = parser%current_token + 1
                end select
            else
                ! Not a keyword, skip
                parser%current_token = parser%current_token + 1
            end if
        end do

        ! Create select case node
        if (default_index > 0) then
            select_index = push_select_case_with_default( &
                           arena, expr_index, case_indices, default_index, &
                           line=line, column=column)
        else
            select_index = push_select_case(arena, expr_index, case_indices, &
                                            line=line, column=column)
        end if
    end function parse_select_case

    function parse_select_type_selector(parser, arena) result(selector_index)
        ! Parse the selector part of SELECT TYPE:
        !   SELECT TYPE (x)           - just an expression
        !   SELECT TYPE (y => x)      - associate_name => selector
        ! For the associate syntax, we create an assignment node representing y => x
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: selector_index

        type(token_t) :: token, next_token
        integer :: name_index
        integer :: lookahead_idx
        logical :: has_associate

        selector_index = 0

        ! Skip whitespace to find the first real token
        token = parser%peek()
        do while (token%kind == TK_WHITESPACE .or. token%kind == TK_COMMENT .or. &
                  token%kind == TK_NEWLINE)
            token = parser%consume()
            if (parser%is_at_end()) return
            token = parser%peek()
        end do

        ! Check if this is associate syntax: identifier => selector
        has_associate = .false.
        if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
            ! Look ahead for =>
            lookahead_idx = parser%current_token + 1
            do while (lookahead_idx <= size(parser%tokens))
                next_token = parser%tokens(lookahead_idx)
                if (next_token%kind == TK_WHITESPACE .or. &
                    next_token%kind == TK_COMMENT) then
                    lookahead_idx = lookahead_idx + 1
                    cycle
                else if (next_token%kind == TK_OPERATOR .and. &
                         next_token%text == "=>") then
                    has_associate = .true.
                    exit
                else
                    exit
                end if
            end do
        end if

        if (has_associate) then
            ! Parse: associate_name => selector
            ! First, consume the associate name
            name_index = push_identifier(arena, token%text, line=token%line, &
                                         column=token%column)
            token = parser%consume()  ! consume the identifier

            ! Skip whitespace to get to =>
            token = parser%peek()
            do while (token%kind == TK_WHITESPACE .or. token%kind == TK_COMMENT)
                token = parser%consume()
                if (parser%is_at_end()) return
                token = parser%peek()
            end do

            ! Consume =>
            if (token%kind == TK_OPERATOR .and. token%text == "=>") then
                token = parser%consume()
            else
                return  ! Error: expected =>
            end if

            ! Parse the selector expression after =>
            selector_index = parse_expression_until(parser, arena, [")"])
            if (selector_index <= 0) return

            ! Create a pointer assignment node to represent y => x
            selector_index = push_pointer_assignment(arena, name_index, &
                                                     selector_index, &
                                                     line=parser%tokens(1)%line, &
                                                     column=parser%tokens(1)%column)
        else
            ! Simple selector (just an expression)
            selector_index = parse_expression_until(parser, arena, [")"])
        end if
    end function parse_select_type_selector

    ! Consume an optional kind selector following a guard type name, e.g. the
    ! "(8)" in "type is (real(8))". Returns the parenthesized text (including
    ! the parentheses) so the caller can fold it into the type name; returns an
    ! empty string when no kind selector is present. Leaves the parser position
    ! unchanged in that case.
    subroutine consume_guard_kind_suffix(parser, kind_suffix)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: kind_suffix

        type(token_t) :: tok
        integer :: depth, saved_pos

        kind_suffix = ""
        saved_pos = parser%current_token

        tok = parser%peek()
        do while (tok%kind == TK_WHITESPACE .or. tok%kind == TK_COMMENT .or. &
                  tok%kind == TK_NEWLINE)
            tok = parser%consume()
            if (parser%is_at_end()) then
                parser%current_token = saved_pos
                return
            end if
            tok = parser%peek()
        end do

        if (tok%kind /= TK_OPERATOR .or. tok%text /= "(") then
            parser%current_token = saved_pos
            return
        end if

        depth = 0
        do while (.not. parser%is_at_end())
            tok = parser%consume()
            select case (tok%kind)
            case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                ! drop layout inside the selector
            case default
                if (tok%kind == TK_OPERATOR .and. tok%text == "(") then
                    depth = depth + 1
                    kind_suffix = kind_suffix // "("
                else if (tok%kind == TK_OPERATOR .and. tok%text == ")") then
                    depth = depth - 1
                    kind_suffix = kind_suffix // ")"
                    if (depth == 0) return
                else
                    kind_suffix = kind_suffix // tok%text
                end if
            end select
        end do

        ! Unbalanced parentheses: restore and report no suffix
        parser%current_token = saved_pos
        kind_suffix = ""
    end subroutine consume_guard_kind_suffix

    recursive function parse_select_type(parser, arena) result(select_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: select_index

        type(token_t) :: select_token, type_token, lparen_token, rparen_token
        type(token_t) :: guard_token, is_token, type_name_token
        integer :: selector_index, default_index
        integer, allocatable :: guard_indices(:)
        integer :: line, column
        character(len=:), allocatable :: guard_keyword
        character(len=20), dimension(4) :: end_keywords
        type(statement_callbacks_t) :: callbacks

        ! Consume select
        select_token = parser%consume()
        line = select_token%line
        column = select_token%column

        ! Expect type
        type_token = parser%consume()
        if (type_token%kind /= TK_KEYWORD .or. to_lower(type_token%text) /= "type") then
            select_index = 0
            return
        end if

        ! Expect (
        lparen_token = parser%consume()
        if (lparen_token%kind /= TK_OPERATOR .or. lparen_token%text /= "(") then
            select_index = 0
            return
        end if

        ! Parse selector expression (may include associate_name => selector syntax)
        selector_index = parse_select_type_selector(parser, arena)
        if (selector_index <= 0) then
            select_index = 0
            return
        end if

        rparen_token = parser%peek()
        if (rparen_token%kind /= TK_OPERATOR .or. rparen_token%text /= ")") then
            select_index = 0
            return
        end if
        rparen_token = parser%consume()

        ! Parse type guard blocks
        allocate (guard_indices(0))
        default_index = 0

        end_keywords(1) = "type"
        end_keywords(2) = "class"
        end_keywords(3) = "contains"
        ! "end select" specifically (matched as end followed by select) terminates
        ! a guard body so a trailing statement after end select is a sibling of
        ! the select node, not absorbed into the last arm. A bare end or
        ! end if/end do inside an arm is not matched.
        end_keywords(4) = "end select"

        callbacks = null_statement_callbacks()
        callbacks%parse_select_case => parse_select_case
        callbacks%parse_select_type => parse_select_type
        callbacks%parse_select_rank => parse_select_rank

        do while (parser%current_token <= size(parser%tokens))
            guard_token = parser%peek()

            if (guard_token%kind == TK_KEYWORD) then
                guard_keyword = trim(to_lower(guard_token%text))
                select case (guard_keyword)
                case ("type", "class")
                    ! Parse type guard block
                    block
                        type(token_t) :: next_token
                        integer :: guard_block_index, type_name_index
                        integer, allocatable :: body_indices(:)
                        character(len=20) :: guard_type
                        character(len=:), allocatable :: guard_type_keyword
                        integer :: name_line, name_column

                        guard_type_keyword = guard_keyword
                        guard_token = parser%consume()  ! consume type or class

                        is_token = parser%peek()
                        do while (is_token%kind == TK_WHITESPACE .or. &
                                  is_token%kind == TK_COMMENT .or. &
                                  is_token%kind == TK_NEWLINE)
                            is_token = parser%consume()
                            if (parser%is_at_end()) exit
                            is_token = parser%peek()
                        end do

                        if (guard_type_keyword == "class") then
                            if ((is_token%kind == TK_KEYWORD .or. &
                                 is_token%kind == TK_IDENTIFIER) .and. &
                                trim(to_lower(is_token%text)) == "default") then
                                is_token = parser%consume()  ! consume default

                                ! Skip rest of current line
                                block
                                    integer :: current_line
                                    type(token_t) :: skip_token

                                    current_line = is_token%line
                                    do while (parser%current_token <= &
                                              size(parser%tokens))
                                        skip_token = parser%peek()
                                        if (skip_token%line == current_line) then
                                            skip_token = parser%consume()
                                        else
                                            exit
                                        end if
                                    end do
                                end block

                                ! Parse default guard body
                                body_indices = parse_statement_body(parser, arena, &
                                                                    end_keywords, &
                                                                    callbacks)

                                ! Create default guard
                                default_index = push_type_guard_block( &
                                                arena, "class_default", 0, &
                                                body_indices, &
                                                line=guard_token%line, &
                                                column=guard_token%column)
                                cycle
                            end if
                        end if

                        ! Expect is
                        if ((is_token%kind /= TK_KEYWORD .and. &
                             is_token%kind /= TK_IDENTIFIER) .or. &
                            trim(to_lower(is_token%text)) /= "is") then
                            select_index = 0
                            return
                        end if
                        is_token = parser%consume()  ! consume is

                        ! Expect (
                        next_token = parser%peek()
                        do while (next_token%kind == TK_WHITESPACE .or. &
                                  next_token%kind == TK_COMMENT .or. &
                                  next_token%kind == TK_NEWLINE)
                            next_token = parser%consume()
                            if (parser%is_at_end()) exit
                            next_token = parser%peek()
                        end do

                        if (next_token%kind /= TK_OPERATOR .or. &
                            next_token%text /= "(") then
                            select_index = 0
                            return
                        end if
                        next_token = parser%consume()  ! consume (

                        ! Parse type name
                        type_name_token = parser%peek()
                        do while (type_name_token%kind == TK_WHITESPACE .or. &
                                  type_name_token%kind == TK_COMMENT .or. &
                                  type_name_token%kind == TK_NEWLINE)
                            type_name_token = parser%consume()
                            if (parser%is_at_end()) exit
                            type_name_token = parser%peek()
                        end do

                        if (type_name_token%kind == TK_KEYWORD .and. &
                            trim(to_lower(type_name_token%text)) == "default") then
                            ! CLASS DEFAULT case
                            type_name_token = parser%consume()  ! consume default

                            ! Expect )
                            next_token = parser%peek()
                            do while (next_token%kind == TK_WHITESPACE .or. &
                                      next_token%kind == TK_COMMENT .or. &
                                      next_token%kind == TK_NEWLINE)
                                next_token = parser%consume()
                                if (parser%is_at_end()) exit
                                next_token = parser%peek()
                            end do

                            if (next_token%kind /= TK_OPERATOR .or. &
                                next_token%text /= ")") then
                                select_index = 0
                                return
                            end if
                            next_token = parser%consume()  ! consume )

                            ! Skip rest of current line
                            block
                                integer :: current_line
                                type(token_t) :: skip_token
                                current_line = type_name_token%line
                                do while (parser%current_token <= size(parser%tokens))
                                    skip_token = parser%peek()
                                    if (skip_token%line == current_line) then
                                        skip_token = parser%consume()
                                    else
                                        exit
                                    end if
                                end do
                            end block

                            ! Parse default guard body
                            body_indices = parse_statement_body(parser, arena, &
                                                                end_keywords, callbacks)

                            ! Create default guard
                            default_index = push_type_guard_block( &
                                            arena, "class_default", 0, body_indices, &
                                            line=guard_token%line, &
                                            column=guard_token%column)
                        else
                            ! Regular TYPE IS or CLASS IS
                            if (type_name_token%kind /= TK_IDENTIFIER .and. &
                                type_name_token%kind /= TK_KEYWORD) then
                                select_index = 0
                                return
                            end if

                            name_line = type_name_token%line
                            name_column = type_name_token%column
                            block
                                character(len=:), allocatable :: full_type_name
                                character(len=:), allocatable :: kind_suffix

                                full_type_name = type_name_token%text
                                type_name_token = parser%consume()

                                ! Capture an optional kind selector, e.g. real(8)
                                ! or real(kind=8), so real and real(8) become
                                ! distinct guards instead of colliding.
                                call consume_guard_kind_suffix(parser, kind_suffix)
                                if (len(kind_suffix) > 0) then
                                    full_type_name = full_type_name // kind_suffix
                                end if

                                type_name_index = push_identifier(arena, &
                                                                  full_type_name, &
                                                                  line=name_line, &
                                                                  column=name_column)
                            end block

                            ! Expect )
                            next_token = parser%peek()
                            do while (next_token%kind == TK_WHITESPACE .or. &
                                      next_token%kind == TK_COMMENT .or. &
                                      next_token%kind == TK_NEWLINE)
                                next_token = parser%consume()
                                if (parser%is_at_end()) exit
                                next_token = parser%peek()
                            end do

                            if (next_token%kind /= TK_OPERATOR .or. &
                                next_token%text /= ")") then
                                select_index = 0
                                return
                            end if
                            next_token = parser%consume()  ! consume )

                            ! Skip rest of current line
                            block
                                integer :: current_line
                                type(token_t) :: skip_token

                                if (parser%current_token > 1) then
                                    current_line = parser%tokens( &
                                                   parser%current_token - 1)%line
                                else
                                    current_line = 1
                                end if

                                do while (parser%current_token <= size(parser%tokens))
                                    skip_token = parser%peek()
                                    if (skip_token%line == current_line) then
                                        skip_token = parser%consume()
                                    else
                                        exit
                                    end if
                                end do
                            end block

                            ! Parse guard body statements
                            body_indices = parse_statement_body(parser, arena, &
                                                                end_keywords, callbacks)

                            ! Determine guard type
                            if (guard_type_keyword == "type") then
                                guard_type = "type_is"
                            else
                                guard_type = "class_is"
                            end if

                            ! Create guard block node
                            guard_block_index = push_type_guard_block( &
                                                arena, guard_type, type_name_index, &
                                                body_indices, &
                                                line=guard_token%line, &
                                                column=guard_token%column)
                            guard_indices = [guard_indices, guard_block_index]
                        end if
                    end block
                case ("end")
                    ! Check for end select (skip whitespace/comments)
                    block
                        type(token_t) :: next_token

                        guard_token = parser%consume()  ! consume end

                        ! Skip whitespace, comments, newlines
                        next_token = parser%peek()
                        do while (next_token%kind == TK_WHITESPACE .or. &
                                  next_token%kind == TK_COMMENT .or. &
                                  next_token%kind == TK_NEWLINE)
                            next_token = parser%consume()
                            if (parser%is_at_end()) exit
                            next_token = parser%peek()
                        end do

                        if (next_token%kind == TK_KEYWORD .and. &
                            trim(to_lower(next_token%text)) == "select") then
                            next_token = parser%consume()  ! consume select
                            exit
                        end if
                        ! If not end select, we already consumed end and
                        ! will continue to next iteration to parse what follows
                    end block
                case ("contains", "function", "subroutine")
                    ! Weve reached something that shouldnt be inside
                    ! select type - likely we missed an end select.
                    ! Dont consume; let the parent parser handle it.
                    exit
                case default
                    parser%current_token = parser%current_token + 1
                end select
            else
                parser%current_token = parser%current_token + 1
            end if
        end do

        ! Create select type node
        if (default_index > 0) then
            select_index = push_select_type_with_default( &
                           arena, selector_index, guard_indices, default_index, &
                           line=line, column=column)
        else
            select_index = push_select_type(arena, selector_index, guard_indices, &
                                            line=line, column=column)
        end if
    end function parse_select_type

    recursive function parse_select_rank(parser, arena) result(select_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: select_index

        type(token_t) :: select_token, rank_token, lparen_token, rparen_token
        type(token_t) :: rank_block_token, value_token, next_token
        integer :: selector_index, default_index
        integer, allocatable :: rank_indices(:)
        integer :: line, column
        character(len=:), allocatable :: rank_keyword
        character(len=20), dimension(2) :: end_keywords
        type(statement_callbacks_t) :: callbacks

        ! Consume select
        select_token = parser%consume()
        line = select_token%line
        column = select_token%column

        ! Expect rank
        rank_token = parser%consume()
        if (rank_token%kind /= TK_KEYWORD .or. to_lower(rank_token%text) /= "rank") then
            select_index = 0
            return
        end if

        ! Expect (
        lparen_token = parser%consume()
        if (lparen_token%kind /= TK_OPERATOR .or. lparen_token%text /= "(") then
            select_index = 0
            return
        end if

        ! Parse selector expression
        selector_index = parse_expression_until(parser, arena, [")"])
        if (selector_index <= 0) then
            select_index = 0
            return
        end if

        rparen_token = parser%peek()
        if (rparen_token%kind /= TK_OPERATOR .or. rparen_token%text /= ")") then
            select_index = 0
            return
        end if
        rparen_token = parser%consume()

        ! Parse rank blocks
        allocate (rank_indices(0))
        default_index = 0

        end_keywords(1) = "rank"
        ! "end select" specifically (matched as end followed by select) terminates
        ! a rank body so a trailing statement after end select is a sibling of
        ! the select node, not absorbed into the last arm. A bare end or
        ! end if/end do inside an arm is not matched (check_end_keyword_match
        ! requires the select suffix), which is why a plain "end" stays excluded.
        end_keywords(2) = "end select"

        callbacks = null_statement_callbacks()
        callbacks%parse_select_rank => parse_select_rank

        do while (parser%current_token <= size(parser%tokens))
            rank_block_token = parser%peek()

            if (rank_block_token%kind == TK_KEYWORD) then
                rank_keyword = trim(to_lower(rank_block_token%text))
                select case (rank_keyword)
                case ("rank")
                    ! Parse rank block
                    block
                        integer :: rank_block_index, rank_value
                        integer, allocatable :: body_indices(:)
                        integer :: block_line, block_column

                        rank_block_token = parser%consume()  ! consume rank
                        block_line = rank_block_token%line
                        block_column = rank_block_token%column

                        ! Check for default or star rank
                        value_token = parser%peek()
                        do while (value_token%kind == TK_WHITESPACE .or. &
                                  value_token%kind == TK_COMMENT .or. &
                                  value_token%kind == TK_NEWLINE)
                            value_token = parser%consume()
                            if (parser%is_at_end()) exit
                            value_token = parser%peek()
                        end do

                        if (value_token%kind == TK_KEYWORD .and. &
                            trim(to_lower(value_token%text)) == "default") then
                            ! RANK DEFAULT case
                            value_token = parser%consume()  ! consume default

                            ! Skip rest of current line
                            block
                                integer :: current_line
                                type(token_t) :: skip_token
                                current_line = value_token%line
                                do while (parser%current_token <= size(parser%tokens))
                                    skip_token = parser%peek()
                                    if (skip_token%line == current_line) then
                                        skip_token = parser%consume()
                                    else
                                        exit
                                    end if
                                end do
                            end block

                            ! Parse default rank body
                            body_indices = parse_statement_body(parser, arena, &
                                                                end_keywords, callbacks)

                            ! Create default rank block
                            default_index = push_rank_block(arena, -1, body_indices, &
                                                            line=block_line, &
                                                            column=block_column)
                        else if (value_token%kind == TK_OPERATOR .and. &
                                 value_token%text == "(") then
                            ! Regular RANK (n) or RANK (*)
                            value_token = parser%consume()  ! consume (

                            ! Get rank value
                            value_token = parser%peek()
                            do while (value_token%kind == TK_WHITESPACE .or. &
                                      value_token%kind == TK_COMMENT .or. &
                                      value_token%kind == TK_NEWLINE)
                                value_token = parser%consume()
                                if (parser%is_at_end()) exit
                                value_token = parser%peek()
                            end do

                            if (value_token%kind == TK_OPERATOR .and. &
                                value_token%text == "*") then
                                ! RANK (*) - star rank
                                value_token = parser%consume()
                                rank_value = -2  ! Use -2 for star rank
                            else if (value_token%kind == TK_NUMBER) then
                                ! RANK (n) - specific rank
                                read (value_token%text, *) rank_value
                                value_token = parser%consume()
                            else
                                select_index = 0
                                return
                            end if

                            ! Expect )
                            next_token = parser%peek()
                            do while (next_token%kind == TK_WHITESPACE .or. &
                                      next_token%kind == TK_COMMENT .or. &
                                      next_token%kind == TK_NEWLINE)
                                next_token = parser%consume()
                                if (parser%is_at_end()) exit
                                next_token = parser%peek()
                            end do

                            if (next_token%kind /= TK_OPERATOR .or. &
                                next_token%text /= ")") then
                                select_index = 0
                                return
                            end if
                            next_token = parser%consume()  ! consume )

                            ! Skip rest of current line
                            block
                                integer :: current_line
                                type(token_t) :: skip_token

                                if (parser%current_token > 1) then
                                    current_line = parser%tokens( &
                                                   parser%current_token - 1)%line
                                else
                                    current_line = 1
                                end if

                                do while (parser%current_token <= size(parser%tokens))
                                    skip_token = parser%peek()
                                    if (skip_token%line == current_line) then
                                        skip_token = parser%consume()
                                    else
                                        exit
                                    end if
                                end do
                            end block

                            ! Parse rank body statements
                            body_indices = parse_statement_body(parser, arena, &
                                                                end_keywords, callbacks)

                            ! Create rank block node
                            rank_block_index = push_rank_block(arena, rank_value, &
                                                               body_indices, &
                                                               line=block_line, &
                                                               column=block_column)
                            rank_indices = [rank_indices, rank_block_index]
                        end if
                    end block
                case ("end")
                    ! Check for end select (skip whitespace/comments)
                    block
                        type(token_t) :: next_token

                        rank_block_token = parser%consume()  ! consume end

                        ! Skip whitespace, comments, newlines
                        next_token = parser%peek()
                        do while (next_token%kind == TK_WHITESPACE .or. &
                                  next_token%kind == TK_COMMENT .or. &
                                  next_token%kind == TK_NEWLINE)
                            next_token = parser%consume()
                            if (parser%is_at_end()) exit
                            next_token = parser%peek()
                        end do

                        if (next_token%kind == TK_KEYWORD .and. &
                            trim(to_lower(next_token%text)) == "select") then
                            next_token = parser%consume()  ! consume select
                            exit
                        end if
                        ! If not end select, we already consumed end and
                        ! will continue to next iteration to parse what follows
                    end block
                case default
                    parser%current_token = parser%current_token + 1
                end select
            else
                parser%current_token = parser%current_token + 1
            end if
        end do

        ! Create select rank node
        if (default_index > 0) then
            select_index = push_select_rank_with_default( &
                           arena, selector_index, rank_indices, default_index, &
                           line=line, column=column)
        else
            select_index = push_select_rank(arena, selector_index, rank_indices, &
                                            line=line, column=column)
        end if
    end function parse_select_rank

end module parser_select_constructs_module
