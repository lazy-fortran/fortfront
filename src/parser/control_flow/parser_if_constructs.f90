module parser_if_constructs_module
    ! Parser module for IF constructs (if/then/else/elseif/endif)
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF, TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, &
                          TK_COMMENT, TK_WHITESPACE, TK_NUMBER, TK_IDENTIFIER, to_lower
    use parser_state_module
    use parser_expressions_module, only: parse_expression
    use parser_statement_core_module, only: parse_basic_statement_core, &
                                            statement_callbacks_t, &
                                            null_statement_callbacks, &
                                            find_statement_end, &
                                            extend_if_statement_end, &
                                            allocate_stmt_tokens_with_eof, &
                                            skip_whitespace_and_semicolons
    use parser_forall_module, only: parse_forall
    use parser_select_constructs_module, only: parse_select_case
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_control, only: if_node
    use ast_nodes_transfer, only: create_goto
    use ast_base, only: LITERAL_INTEGER
    use ast_factory, only: push_if, push_goto, push_binary_op, push_literal
    implicit none
    private

    public :: parse_if, parse_if_condition, parse_if_body, parse_elseif_block
    public :: register_parse_do_loop

    abstract interface
        function parse_do_loop_interface(parser, arena) result(loop_index)
            import :: parser_state_t, ast_arena_t
            type(parser_state_t), intent(inout) :: parser
            type(ast_arena_t), intent(inout) :: arena
            integer :: loop_index
        end function parse_do_loop_interface
    end interface

    procedure(parse_do_loop_interface), pointer :: parse_do_loop_proc => null()

    interface
        subroutine ensure_if_do_registration_bridge()
        end subroutine ensure_if_do_registration_bridge
    end interface

contains

    subroutine ensure_do_parser_ready()
        if (.not. associated(parse_do_loop_proc)) then
            call ensure_if_do_registration_bridge()
        end if
    end subroutine ensure_do_parser_ready

    subroutine register_parse_do_loop(proc)
        procedure(parse_do_loop_interface) :: proc
        parse_do_loop_proc => proc
    end subroutine register_parse_do_loop

    function build_if_body_callbacks() result(callbacks)
        type(statement_callbacks_t) :: callbacks

        callbacks = null_statement_callbacks()
        callbacks%parse_if => parse_if
        callbacks%parse_do_loop => parse_do_loop_callback
        callbacks%parse_select_case => parse_select_case
        callbacks%parse_forall => parse_forall
    end function build_if_body_callbacks

    integer function parse_do_loop_callback(parser, arena) result(loop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena

        loop_index = 0
        call ensure_do_parser_ready()
        if (associated(parse_do_loop_proc)) then
            loop_index = parse_do_loop_proc(parser, arena)
        end if
    end function parse_do_loop_callback

    ! Helper function to detect arithmetic IF: IF (expr) label1, label2, label3
    function is_arithmetic_if(parser) result(is_arith_if)
        type(parser_state_t), intent(in) :: parser
        logical :: is_arith_if
        type(token_t) :: tok
        integer :: idx, label_count, comma_count, total_tokens
        logical :: expect_label
        integer :: start_line

        is_arith_if = .false.
        if (.not. associated(parser%tokens)) return

        total_tokens = size(parser%tokens)
        idx = parser%current_token
        if (idx < 1 .or. idx > total_tokens) return

        ! Skip whitespace/comments before evaluating pattern
        do while (idx <= total_tokens)
            tok = parser%tokens(idx)
            if (tok%kind == TK_WHITESPACE .or. tok%kind == TK_COMMENT) then
                idx = idx + 1
                cycle
            end if
            exit
        end do
        if (idx > total_tokens) return
        if (parser%tokens(idx)%kind == TK_NEWLINE) return

        start_line = parser%tokens(idx)%line
        expect_label = .true.
        label_count = 0
        comma_count = 0

        do while (idx <= total_tokens)
            tok = parser%tokens(idx)
            if (tok%kind == TK_WHITESPACE .or. tok%kind == TK_COMMENT) then
                idx = idx + 1
                cycle
            end if
            if (tok%kind == TK_NEWLINE) exit
            if (tok%line /= start_line) exit

            if (expect_label) then
                if (tok%kind == TK_NUMBER .or. tok%kind == TK_IDENTIFIER) then
                    label_count = label_count + 1
                    expect_label = .false.
                    if (label_count > 3) return
                else
                    return
                end if
            else
                if (tok%kind == TK_OPERATOR .and. tok%text == ",") then
                    comma_count = comma_count + 1
                    expect_label = .true.
                else
                    return
                end if
            end if

            idx = idx + 1
            if (.not. expect_label .and. label_count == 3) exit
        end do

        if (label_count == 3 .and. comma_count == 2 .and. .not. expect_label) then
            ! Ensure no additional tokens on the same line (besides whitespace/comment)
            do while (idx <= total_tokens)
                tok = parser%tokens(idx)
                if (tok%kind == TK_WHITESPACE .or. tok%kind == TK_COMMENT) then
                    idx = idx + 1
                    cycle
                end if
                if (tok%kind == TK_NEWLINE) exit
                if (tok%line /= start_line) exit
                return
            end do
            is_arith_if = .true.
        end if
    end function is_arithmetic_if

    ! Parse arithmetic IF: IF (expr) label1, label2, label3
    ! Transforms to: IF (expr < 0) GOTO label1; ELSEIF (expr == 0) GOTO label2; ELSE GOTO label3
    function parse_arithmetic_if(parser, arena, condition_index, if_token, parent_index) &
        result(if_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: condition_index
        type(token_t), intent(in) :: if_token
        integer, intent(in), optional :: parent_index
        integer :: if_index

        character(len=:), allocatable :: label1, label2, label3
        integer :: zero_index, cond_lt_zero_index, cond_eq_zero_index
        integer :: goto1_index, goto2_index, goto3_index
        integer, allocatable :: then_body_indices(:), else_body_indices(:)
        integer, allocatable :: elseif_indices(:)

        if_index = 0

        ! Parse arithmetic IF label triplet
        if (.not. consume_label_text(label1)) return
        if (.not. consume_comma_token()) return
        if (.not. consume_label_text(label2)) return
        if (.not. consume_comma_token()) return
        if (.not. consume_label_text(label3)) return

        ! Build: IF (expr < 0) GOTO label1
        zero_index = push_literal(arena, "0", LITERAL_INTEGER, if_token%line, if_token%column)
        cond_lt_zero_index = push_binary_op(arena, condition_index, zero_index, "<", &
            if_token%line, if_token%column)
        goto1_index = push_goto(arena, label=label1, line=if_token%line, column=if_token%column)
        allocate (then_body_indices(1))
        then_body_indices(1) = goto1_index

        ! Build: ELSEIF (expr == 0) GOTO label2
        cond_eq_zero_index = push_binary_op(arena, condition_index, zero_index, "==", &
            if_token%line, if_token%column)
        goto2_index = push_goto(arena, label=label2, line=if_token%line, column=if_token%column)
        allocate (elseif_indices(2))
        elseif_indices(1) = cond_eq_zero_index
        elseif_indices(2) = goto2_index

        ! Build: ELSE GOTO label3
        goto3_index = push_goto(arena, label=label3, line=if_token%line, column=if_token%column)
        allocate (else_body_indices(1))
        else_body_indices(1) = goto3_index

        ! Create IF node
        if_index = push_if(arena, cond_lt_zero_index, then_body_indices, &
            elseif_indices=elseif_indices, else_body_indices=else_body_indices, &
            line=if_token%line, column=if_token%column, parent_index=parent_index)
    contains

        logical function fetch_next_token(token)
            type(token_t), intent(out) :: token

            fetch_next_token = .false.
            do
                if (parser%is_at_end()) return
                token = parser%peek()
                if (token%kind == TK_WHITESPACE .or. token%kind == TK_COMMENT) then
                    token = parser%consume()
                    cycle
                end if
                if (token%kind == TK_NEWLINE) return
                fetch_next_token = .true.
                return
            end do
        end function fetch_next_token

        logical function consume_label_text(label_text)
            character(len=:), allocatable, intent(out) :: label_text
            type(token_t) :: next_token

            if (.not. fetch_next_token(next_token)) then
                label_text = ""
                consume_label_text = .false.
                return
            end if

            if (next_token%kind == TK_NUMBER .or. next_token%kind == TK_IDENTIFIER) then
                label_text = trim(next_token%text)
                next_token = parser%consume()
                consume_label_text = .true.
            else
                label_text = ""
                consume_label_text = .false.
            end if
        end function consume_label_text

        logical function consume_comma_token()
            type(token_t) :: next_token

            if (.not. fetch_next_token(next_token)) then
                consume_comma_token = .false.
                return
            end if

            if (next_token%kind == TK_OPERATOR .and. next_token%text == ",") then
                next_token = parser%consume()
                consume_comma_token = .true.
            else
                consume_comma_token = .false.
            end if
        end function consume_comma_token
    end function parse_arithmetic_if

    ! Parse if statement
    recursive function parse_if(parser, arena, parent_index) result(if_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer :: if_index

        type(token_t) :: if_token, then_token
        integer :: condition_index
        integer, allocatable :: then_body_indices(:), else_body_indices(:)
        integer, allocatable :: elseif_indices(:)
        integer :: elseif_count
        logical :: inline_has_continuation

        ! Consume 'if' keyword
        if_token = parser%consume()

        ! Parse condition (should be in parentheses for standard if/then/endif)
        condition_index = parse_if_condition(parser, arena)

        ! Check for arithmetic IF: IF (expr) label1, label2, label3
        if (is_arithmetic_if(parser)) then
            if_index = parse_arithmetic_if(parser, arena, condition_index, if_token, parent_index)
            return
        end if

        ! Look for 'then' keyword
        then_token = parser%peek()
        if (then_token%kind == TK_KEYWORD .and. to_lower(then_token%text) == "then") then
            ! Standard if/then/endif block
            then_token = parser%consume()

            ! Skip optional semicolon after 'then'
            if (.not. parser%is_at_end()) then
                then_token = parser%peek()
                if (then_token%kind == TK_OPERATOR .and. then_token%text == ";") then
                    then_token = parser%consume()  ! Skip the semicolon
                end if
            end if

            ! Create if node placeholder first to get the parent index
            if_index = push_if(arena, condition_index, [integer ::], &
                               line=if_token%line, column=if_token%column, &
                               parent_index=parent_index)

            ! Parse then body statements with the if node as parent
            then_body_indices = parse_if_body(parser, arena, if_index)

            ! Check for elseif/else blocks
            elseif_count = 0
            allocate (elseif_indices(0))

            block
                integer :: safety_counter
                safety_counter = 0
                do while (.not. parser%is_at_end() .and. safety_counter < 10000)
                    safety_counter = safety_counter + 1
                    then_token = parser%peek()

                    if (then_token%kind == TK_KEYWORD) then
                        if (to_lower(then_token%text) == "elseif" .or. to_lower(then_token%text) == &
                            "else if") then
                            ! Parse elseif block
                            block
                                integer :: elseif_pair(2)
                                elseif_pair = parse_elseif_block(parser, arena)
                                elseif_indices = [elseif_indices, elseif_pair]
                                elseif_count = elseif_count + 1
                            end block
                        else if (to_lower(then_token%text) == "else") then
                            ! Check if next token is "if" (for "else if")
                            if (parser%current_token + 1 <= size(parser%tokens)) then
                                if (parser%tokens(parser%current_token + 1)%kind == &
                                    TK_KEYWORD .and. &
                                    to_lower(parser%tokens(parser%current_token + 1)%text) &
                                    == "if") then
                                    ! Parse as elseif block
                                    block
                                        integer :: elseif_pair(2)
                                        elseif_pair = parse_elseif_block(parser, arena)
                                        elseif_indices = [elseif_indices, elseif_pair]
                                        elseif_count = elseif_count + 1
                                    end block
                                    cycle  ! Continue looking for more elseif/else blocks
                                end if
                            end if
                            ! Parse else block
                            then_token = parser%consume()  ! consume 'else'

                            ! Skip optional semicolon after 'else'
                            if (.not. parser%is_at_end()) then
                                then_token = parser%peek()
                                if (then_token%kind == TK_OPERATOR .and. &
                                    then_token%text == ";") then
                                    then_token = parser%consume()  ! Skip the semicolon
                                end if
                            end if

                            else_body_indices = parse_if_body(parser, arena, if_index)
                            exit
                        else if (to_lower(then_token%text) == "endif") then
                            ! End of if statement (single keyword)
                            then_token = parser%consume()
                            exit
                        else if (to_lower(then_token%text) == "end") then
                            ! Check if next token is "if" (for "end if"), ignoring trivia
                            block
                                integer :: lookahead_pos
                                type(token_t) :: lookahead

                                lookahead_pos = parser%current_token + 1
                                do while (lookahead_pos <= size(parser%tokens))
                                    lookahead = parser%tokens(lookahead_pos)
                                    if (lookahead%kind == TK_WHITESPACE .or. &
                                        lookahead%kind == TK_NEWLINE .or. &
                                        lookahead%kind == TK_COMMENT) then
                                        lookahead_pos = lookahead_pos + 1
                                        cycle
                                    end if
                                    exit
                                end do

                                if (lookahead_pos <= size(parser%tokens)) then
                                    lookahead = parser%tokens(lookahead_pos)
                                    if (lookahead%kind == TK_KEYWORD .and. &
                                        to_lower(lookahead%text) == "if") then
                                        then_token = parser%consume()  ! consume "end"
                                        do
                                            if (parser%is_at_end()) exit
                                            lookahead = parser%peek()
                                            if (lookahead%kind == TK_WHITESPACE .or. &
                                                lookahead%kind == TK_NEWLINE .or. &
                                                lookahead%kind == TK_COMMENT) then
                                                then_token = parser%consume()
                                            else
                                                exit
                                            end if
                                        end do
                                        if (.not. parser%is_at_end()) then
                                            lookahead = parser%peek()
                                            if (lookahead%kind == TK_KEYWORD .and. &
                                                to_lower(lookahead%text) == "if") then
                                                then_token = parser%consume()
                                            end if
                                        end if
                                        exit
                                    end if
                                end if
                            end block
                        else
                            ! Other statement, stop parsing if block
                            exit
                        end if
                    else
                        ! Not a keyword, continue parsing body
                        exit
                    end if
                end do
            end block

            ! Update the if node with the actual body indices
            if (allocated(arena%entries(if_index)%node)) then
                select type (node => arena%entries(if_index)%node)
                type is (if_node)
                    if (allocated(then_body_indices)) then
                        node%then_body_indices = then_body_indices
                    end if
                    if (allocated(else_body_indices)) then
                        node%else_body_indices = else_body_indices
                    end if
                    if (allocated(elseif_indices)) then
                        if (size(elseif_indices) > 0 .and. &
                            mod(size(elseif_indices), 2) == 0) then
                            allocate (node%elseif_blocks(size(elseif_indices) / 2))
                            do elseif_count = 1, size(elseif_indices) / 2
                                node%elseif_blocks(elseif_count)%condition_index = &
                                    elseif_indices(2 * elseif_count - 1)
                                node%elseif_blocks(elseif_count)%body_indices = &
                                    [elseif_indices(2 * elseif_count)]
                            end do
                        end if
                    end if
                end select
            end if
        else
            ! No 'then' keyword found
            ! If we're at EOF, this is only an error if we parsed a real condition
            ! (not just "end if" cleanup parsing)
            if (then_token%kind == TK_EOF .or. parser%is_at_end()) then
                ! Only report error if the condition actually consumed tokens
                ! (i.e., this was a real IF statement without THEN, not "end if" cleanup)
                if (condition_index > 0) then
                    write (error_unit, '(A)') "  Suggestion: Use 'IF (condition) THEN' for multi-line blocks"
                    call parser%error("IF construct Missing 'then' keyword (e.g., 'if x > 0' needs 'then')", &
                                    "Use 'IF (condition) THEN' for multi-line blocks")
                    if_index = 0
                    return
                end if
                ! Return with no node
                if_index = 0
                return
            end if

            ! Check if this looks like a malformed multi-line if construct
            inline_has_continuation = .false.
            block
                logical :: looks_like_block_if
                type(token_t) :: check_tok
                integer :: check_idx

                looks_like_block_if = .false.

                ! Scan ahead to see if there's a newline before any statement
                ! or if there's an "end if" later
                check_idx = parser%current_token
                do while (check_idx <= size(parser%tokens))
                    check_tok = parser%tokens(check_idx)

                    ! Check for line continuation character
                    if (check_tok%kind == TK_OPERATOR .and. check_tok%text == "&") then
                        inline_has_continuation = .true.
                        check_idx = check_idx + 1
                        cycle
                    end if

                    ! If we hit a newline, peek ahead to see if there's code after it
                    if (check_tok%kind == TK_NEWLINE) then
                        ! Save current position and scan ahead
                        block
                            integer :: peek_idx
                            type(token_t) :: peek_tok
                            logical :: found_code_after_newline

                            found_code_after_newline = .false.
                            peek_idx = check_idx + 1

                            ! Skip any following whitespace/comments/newlines
                            do while (peek_idx <= size(parser%tokens))
                                peek_tok = parser%tokens(peek_idx)
                                if (peek_tok%kind == TK_WHITESPACE .or. &
                                    peek_tok%kind == TK_COMMENT .or. &
                                    peek_tok%kind == TK_NEWLINE) then
                                    peek_idx = peek_idx + 1
                                    cycle
                                end if
                                ! Found a non-trivia token
                                if (peek_tok%kind /= TK_EOF) then
                                    found_code_after_newline = .true.
                                end if
                                exit
                            end do

                            if (found_code_after_newline) then
                                inline_has_continuation = .true.
                            end if

                            ! If there's code immediately after newline, it's likely a continued inline IF
                            ! Otherwise, it looks like a block IF
                            if (.not. found_code_after_newline .and. &
                                .not. inline_has_continuation) then
                                looks_like_block_if = .true.
                            end if
                        end block
                        exit
                    end if

                    ! Skip whitespace and comments
                    if (check_tok%kind == TK_WHITESPACE .or. &
                        check_tok%kind == TK_COMMENT) then
                        check_idx = check_idx + 1
                        cycle
                    end if

                    ! Check for "end if" or "endif" later in the code
                    if (check_tok%kind == TK_KEYWORD) then
                        if (to_lower(check_tok%text) == "end" .or. &
                            to_lower(check_tok%text) == "endif") then
                            looks_like_block_if = .true.
                            exit
                        end if
                    end if

                    ! Found a non-whitespace token on same line, might be valid one-liner
                    exit
                end do

                if (looks_like_block_if) then
                    ! This is a malformed block if Missing 'then' - report error
                    write (error_unit, '(A)') "  Suggestion: Use 'IF (condition) THEN' for multi-line blocks"
                    call parser%error("IF construct Missing 'then' keyword (e.g., 'if x > 0' needs 'then')", &
                                    "Use 'IF (condition) THEN' for multi-line blocks")
                    if_index = 0
                    return
                end if
            end block

            ! Valid one-line if statement (no then keyword)
            allocate (then_body_indices(1))

            ! Parse the single statement
            block
                integer :: stmt_start, stmt_end
                integer, allocatable :: stmt_indices(:)
                type(token_t), allocatable :: stmt_tokens(:)
                type(token_t) :: tok
                type(statement_callbacks_t) :: callbacks

                call skip_inline_if_leading_tokens(parser, inline_has_continuation)
                stmt_start = parser%current_token
                if (stmt_start <= size(parser%tokens)) then
                    stmt_end = find_statement_end(parser%tokens, stmt_start)
                    if (stmt_end < stmt_start) stmt_end = stmt_start
                    call allocate_stmt_tokens_with_eof(stmt_tokens, parser%tokens, &
                                                       stmt_start, stmt_end)

                    callbacks = build_if_body_callbacks()
                    stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                                                              callbacks=callbacks)
                    if (allocated(stmt_indices)) then
                        if (size(stmt_indices) > 0 .and. stmt_indices(1) > 0) then
                            then_body_indices(1) = stmt_indices(1)
                        end if
                    end if

                    if (stmt_end < size(parser%tokens)) then
                        parser%current_token = stmt_end + 1
                    else
                        parser%current_token = size(parser%tokens)
                    end if
                end if

                ! Advance parser to end of statement to prevent re-parsing
                do while (.not. parser%is_at_end())
                    tok = parser%peek()
                    if (tok%kind == TK_NEWLINE .or. tok%kind == TK_EOF) then
                        exit
                    end if
                    tok = parser%consume()
                end do
            end block

            ! Create if node with no elseif/else blocks
            allocate (elseif_indices(0))
            allocate (else_body_indices(0))
            if_index = push_if(arena, condition_index, then_body_indices, &
                               elseif_indices=elseif_indices, &
                               else_body_indices=else_body_indices, &
                               line=if_token%line, column=if_token%column, &
                               parent_index=parent_index)
        end if

    end function parse_if

    ! Parse if condition (handles parentheses if present)
    function parse_if_condition(parser, arena) result(condition_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: condition_index
        type(token_t) :: paren_token
        type(token_t), allocatable, target :: remaining_tokens(:)
        integer :: i, n
        integer :: paren_depth

        ! Check for opening parenthesis
        paren_token = parser%peek()
        if (paren_token%kind == TK_OPERATOR .and. paren_token%text == "(") then
            paren_token = parser%consume()  ! consume '('
            paren_depth = 1

            ! Count remaining tokens
            n = 0
            do i = parser%current_token, size(parser%tokens)
                n = n + 1
            end do

            ! Extract remaining tokens
            allocate (remaining_tokens(n))
            remaining_tokens = parser%tokens(parser%current_token:)

            ! Parse the condition expression
            condition_index = parse_expression(remaining_tokens, arena)

            ! Advance parser past the condition tokens
            ! Simple approach: advance until we find the closing paren or 'then'
            do while (.not. parser%is_at_end())
                paren_token = parser%peek()

                ! Check for 'then' keyword
                if (paren_token%kind == TK_KEYWORD .and. to_lower(paren_token%text) == &
                         "then") then
                    exit  ! Don't consume 'then', let caller handle it
                end if

                ! Check for newline (preserve for continuation handling)
                if (paren_token%kind == TK_NEWLINE) then
                    exit  ! Don't consume newline, let caller handle continuation logic
                end if

                ! Skip whitespace and comments
                if (paren_token%kind == TK_WHITESPACE .or. paren_token%kind == TK_COMMENT) then
                    paren_token = parser%consume()
                    cycle
                end if

                ! Handle operators
                if (paren_token%kind == TK_OPERATOR) then
                    if (paren_token%text == "(") then
                        paren_depth = paren_depth + 1
                        paren_token = parser%consume()
                        cycle
                    else if (paren_token%text == ")") then
                        paren_depth = paren_depth - 1
                        paren_token = parser%consume()  ! consume ')'
                        if (paren_depth <= 0) exit
                        cycle
                    end if
                end if

                ! Consume any other token (operators, identifiers, etc. within condition)
                paren_token = parser%consume()
            end do
        else
            ! No parentheses, just parse the expression
            ! Count remaining tokens
            n = 0
            do i = parser%current_token, size(parser%tokens)
                n = n + 1
            end do

            ! Extract remaining tokens
            allocate (remaining_tokens(n))
            remaining_tokens = parser%tokens(parser%current_token:)

            condition_index = parse_expression(remaining_tokens, arena)

            ! Advance parser past the condition tokens until 'then'
            do while (.not. parser%is_at_end())
                paren_token = parser%peek()

                ! Check for 'then' keyword
                if (paren_token%kind == TK_KEYWORD .and. to_lower(paren_token%text) == &
                    "then") then
                    exit  ! Don't consume 'then', let caller handle it
                end if

                ! Check for newline (preserve for continuation handling)
                if (paren_token%kind == TK_NEWLINE) then
                    exit  ! Don't consume newline, let caller handle continuation logic
                end if

                ! Skip whitespace and comments
                if (paren_token%kind == TK_WHITESPACE .or. paren_token%kind == TK_COMMENT) then
                    paren_token = parser%consume()
                    cycle
                end if

                ! Consume any other token
                paren_token = parser%consume()
            end do
        end if

    end function parse_if_condition

    ! Parse if/elseif/else body statements
    function parse_if_body(parser, arena, parent_index) result(body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer, allocatable :: body_indices(:)
        type(token_t) :: token, lookahead
        integer :: stmt_count, lookahead_pos
        character(len=:), allocatable :: keyword_text

        allocate (body_indices(0))
        stmt_count = 0

        block
            integer :: safety_counter
            safety_counter = 0

            do while (.not. parser%is_at_end() .and. safety_counter < 10000)
                safety_counter = safety_counter + 1

                call skip_whitespace_and_semicolons(parser)
                if (parser%current_token > size(parser%tokens)) exit

                token = parser%peek()

                ! Check for end of body
                if (token%kind == TK_KEYWORD) then
                    keyword_text = to_lower(trim(token%text))
                    if (keyword_text == "elseif" .or. keyword_text == "else if" .or. &
                        keyword_text == "endif" .or. keyword_text == "end if") then
                        exit
                    else if (keyword_text == "else") then
                        lookahead_pos = parser%current_token + 1
                        do while (lookahead_pos <= size(parser%tokens))
                            lookahead = parser%tokens(lookahead_pos)
                            if (lookahead%kind == TK_WHITESPACE .or. &
                                lookahead%kind == TK_NEWLINE .or. &
                                lookahead%kind == TK_COMMENT) then
                                lookahead_pos = lookahead_pos + 1
                                cycle
                            end if
                            exit
                        end do
                        if (lookahead_pos <= size(parser%tokens)) then
                            lookahead = parser%tokens(lookahead_pos)
                            if (lookahead%kind == TK_KEYWORD .and. &
                                to_lower(trim(lookahead%text)) == "if") then
                                exit  ! Found "else if"
                            end if
                        end if
                        exit
                    else if (keyword_text == "end") then
                        lookahead_pos = parser%current_token + 1
                        do while (lookahead_pos <= size(parser%tokens))
                            lookahead = parser%tokens(lookahead_pos)
                            if (lookahead%kind == TK_WHITESPACE .or. &
                                lookahead%kind == TK_NEWLINE .or. &
                                lookahead%kind == TK_COMMENT) then
                                lookahead_pos = lookahead_pos + 1
                                cycle
                            end if
                            exit
                        end do
                        if (lookahead_pos <= size(parser%tokens)) then
                            lookahead = parser%tokens(lookahead_pos)
                            if (lookahead%kind == TK_KEYWORD .and. &
                                to_lower(trim(lookahead%text)) == "if") then
                                exit  ! Found "end if"
                            end if
                        end if
                    end if
                end if

                ! Parse statement until end of line (same approach as do loop)
                block
                    type(token_t), allocatable, target :: stmt_tokens(:)
                    type(token_t) :: first_stmt_token
                    integer, allocatable :: stmt_indices(:)
                    integer :: remaining_count, consumed_tokens, k
                    integer :: stmt_end, last_token_index
                    type(statement_callbacks_t) :: callbacks

                    stmt_end = find_statement_end(parser%tokens, parser%current_token)
                    first_stmt_token = parser%tokens(parser%current_token)
                    if (first_stmt_token%kind == TK_KEYWORD) then
                        if (first_stmt_token%text == "if") then
                            stmt_end = extend_if_statement_end(parser%tokens, &
                                                               parser%current_token, &
                                                               stmt_end)
                        end if
                    end if
                    if (stmt_end < parser%current_token) then
                        stmt_end = parser%current_token
                    end if

                    remaining_count = stmt_end - parser%current_token + 1
                    if (remaining_count <= 0) exit

                    call allocate_stmt_tokens_with_eof(stmt_tokens, parser%tokens, &
                                                       parser%current_token, stmt_end)
                    last_token_index = stmt_end
                    stmt_tokens(remaining_count + 1)%line = &
                        parser%tokens(last_token_index)%line
                    stmt_tokens(remaining_count + 1)%column = &
                        parser%tokens(last_token_index)%column + 1

                    callbacks = build_if_body_callbacks()
                    if (present(parent_index)) then
                        stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                                                            parent_index=parent_index, &
                                                                  callbacks=callbacks, &
                                                         consumed_count=consumed_tokens)
                    else
                        stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                                                                  callbacks=callbacks, &
                                                         consumed_count=consumed_tokens)
                    end if

                    if (allocated(stmt_indices) .and. size(stmt_indices) > 0) then
                        do k = 1, size(stmt_indices)
                            if (stmt_indices(k) > 0) then
                                body_indices = [body_indices, stmt_indices(k)]
                                stmt_count = stmt_count + 1
                            end if
                        end do
                    end if

                    parser%current_token = stmt_end + 1

                    block
                        type(token_t), allocatable, target :: temp(:)
                        call move_alloc(stmt_tokens, temp)
                    end block
                end block
            end do
        end block

    end function parse_if_body

    ! Parse elseif block
    function parse_elseif_block(parser, arena) result(elseif_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: elseif_indices(2)  ! condition_index, body_indices_start
        type(token_t) :: elseif_token

        ! Consume 'elseif' or 'else if'
        elseif_token = parser%consume()
        ! Check if we consumed 'else' and need to consume 'if' as well
        if (to_lower(elseif_token%text) == "else") then
            ! This should be "else if", consume the "if" token too
            elseif_token = parser%consume()
        end if

        ! Parse condition
        elseif_indices(1) = parse_if_condition(parser, arena)

        ! Look for 'then' keyword
        elseif_token = parser%peek()
        if (elseif_token%kind == TK_KEYWORD .and. to_lower(elseif_token%text) == "then") then
            elseif_token = parser%consume()
        end if

        ! Parse body
        block
            integer, allocatable :: body_indices(:)
            body_indices = parse_if_body(parser, arena)
            if (allocated(body_indices) .and. size(body_indices) > 0) then
                elseif_indices(2) = body_indices(1)  ! Store first body statement index
            else
                elseif_indices(2) = 0
            end if
        end block

    end function parse_elseif_block

    subroutine skip_inline_if_leading_tokens(parser, allow_newlines)
        type(parser_state_t), intent(inout) :: parser
        logical, intent(in) :: allow_newlines
        type(token_t) :: tok

        do while (.not. parser%is_at_end())
            tok = parser%peek()
            select case (tok%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                tok = parser%consume()
            case (TK_NEWLINE)
                if (allow_newlines) then
                    tok = parser%consume()
                else
                    return
                end if
            case (TK_OPERATOR)
                if (tok%text == "&") then
                    tok = parser%consume()
                else
                    return
                end if
            case default
                return
            end select
        end do
    end subroutine skip_inline_if_leading_tokens

end module parser_if_constructs_module
