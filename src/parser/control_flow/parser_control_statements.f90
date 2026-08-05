module parser_control_statements_module
    ! Parser module for control flow statements such as stop, return,
    ! goto, cycle, exit, and error stop
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
        TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
        TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_comparison
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_stop, push_return, push_entry, push_continue, &
        push_end_statement, push_goto, push_error_stop, &
        push_cycle, push_exit, push_pause, push_nullify
    use ast_factory
    implicit none
    private

    public :: parse_stop_statement, parse_return_statement, parse_entry_statement, &
        parse_continue_statement, parse_end_statement
    public :: parse_goto_statement, parse_error_stop_statement
    public :: parse_cycle_statement, parse_exit_statement, parse_pause_statement
    public :: parse_nullify_statement

contains

    function parse_stop_statement(parser, arena) result(stop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stop_index

        type(token_t) :: token
        integer :: line, column, stop_code_index
        character(len=:), allocatable :: stop_message

        ! Consume 'stop' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()

        ! Check for optional stop code or message
        token = parser%peek()
        stop_code_index = 0
        stop_message = ""

        if (token%kind == TK_STRING) then
            ! String literal message
            stop_message = token%text
            token = parser%consume()
        else if (token%kind == TK_NUMBER .or. token%kind == TK_IDENTIFIER) then
            ! Integer expression or variable
            stop_code_index = parse_comparison(parser, arena)
        end if

        ! F2018 allows a QUIET= specifier after the stop code, as in
        ! `stop 1, quiet=.true.`. Consume it so the trailing comma does not
        ! leave the statement unrecognized.
        call skip_stop_specifiers(parser, arena)

        ! Create STOP node
        if (len_trim(stop_message) > 0) then
            stop_index = push_stop(arena, stop_message=stop_message, &
                line=line, column=column)
        else
            stop_index = push_stop(arena, stop_code_index=stop_code_index, &
                line=line, column=column)
        end if
    end function parse_stop_statement

    subroutine skip_stop_specifiers(parser, arena)
        !! Consume `, quiet= <scalar-logical-expr>` after a STOP or ERROR STOP
        !! stop-code. The specifier does not change control flow, so it carries
        !! no AST node; parsing it exists so the statement is recognized at all.
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena

        type(token_t) :: token
        integer :: discarded

        do
            token = parser%peek()
            if (token%kind /= TK_OPERATOR) return
            if (token%text /= ',') return
            token = parser%consume()

            token = parser%peek()
            if (token%kind /= TK_IDENTIFIER .and. token%kind /= TK_KEYWORD) return
            if (to_lower(token%text) /= 'quiet') return
            token = parser%consume()

            token = parser%peek()
            if (token%kind /= TK_OPERATOR) return
            if (token%text /= '=') return
            token = parser%consume()

            discarded = parse_comparison(parser, arena)
        end do
    end subroutine skip_stop_specifiers

    function parse_return_statement(parser, arena, parent_index) result(return_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer :: return_index

        type(token_t) :: token
        integer :: line, column, selector_index

        ! Consume 'return' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()

        ! Optional alternate-return selector: RETURN <scalar-int-expr>
        selector_index = 0
        token = parser%peek()
        ! A bare identifier after RETURN is not treated as a selector: lazy
        ! Fortran uses `return <name>` for a result value.
        if (token%line == line) then
            if (token%kind == TK_NUMBER) then
                selector_index = parse_comparison(parser, arena)
            else if (token%kind == TK_OPERATOR .and. token%text == "(") then
                selector_index = parse_comparison(parser, arena)
            end if
        end if

        ! Create RETURN node
        return_index = push_return(arena, line=line, column=column, &
            parent_index=parent_index, selector_index=selector_index)
    end function parse_return_statement

    function parse_entry_statement(parser, arena, parent_index) result(entry_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer :: entry_index

        type(token_t) :: token
        integer :: line, column
        character(len=:), allocatable :: entry_name, params_text
        integer :: paren_depth, i

        ! Consume ENTRY keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()

        ! Get entry point name
        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER) then
            entry_index = 0
            return
        end if
        entry_name = trim(token%text)
        token = parser%consume()

        ! Check for optional parameter list
        token = parser%peek()
        params_text = ""
        if (token%kind == TK_OPERATOR .and. trim(token%text) == "(") then
            ! Capture parameter list text
            paren_depth = 0
            i = parser%current_token
            do while (i <= size(parser%tokens))
                token = parser%tokens(i)
                params_text = params_text//trim(token%text)
                if (token%kind == TK_OPERATOR .and. trim(token%text) == "(") then
                    paren_depth = paren_depth + 1
                else if (token%kind == TK_OPERATOR .and. trim(token%text) == ")") then
                    paren_depth = paren_depth - 1
                    if (paren_depth == 0) then
                        i = i + 1
                        exit
                    end if
                end if
                i = i + 1
            end do
            parser%current_token = i
        end if

        ! Check for optional result clause
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER .and. &
            to_lower(trim(token%text)) == "result") then
            params_text = params_text//" result"
            token = parser%consume()
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. trim(token%text) == "(") then
                paren_depth = 0
                i = parser%current_token
                do while (i <= size(parser%tokens))
                    token = parser%tokens(i)
                    params_text = params_text//trim(token%text)
                    if (token%kind == TK_OPERATOR .and. trim(token%text) == "(") then
                        paren_depth = paren_depth + 1
                    else if (token%kind == TK_OPERATOR .and. &
                            trim(token%text) == ")") then
                        paren_depth = paren_depth - 1
                        if (paren_depth == 0) then
                            i = i + 1
                            exit
                        end if
                    end if
                    i = i + 1
                end do
                parser%current_token = i
            end if
        end if

        ! Create ENTRY node
        entry_index = push_entry(arena, name=entry_name, params_text=params_text, &
            line=line, column=column, parent_index=parent_index)
    end function parse_entry_statement

    function parse_continue_statement(parser, arena, parent_index) &
            result(continue_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer :: continue_index

        type(token_t) :: token
        integer :: line, column

        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()

        continue_index = push_continue(arena, line=line, column=column, &
            parent_index=parent_index)
    end function parse_continue_statement

    function parse_end_statement(parser, arena, parent_index) result(end_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer :: end_index

        type(token_t) :: token
        integer :: line, column

        ! Consume 'end' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()

        ! Create END statement node
        end_index = push_end_statement(arena, line=line, column=column, &
            parent_index=parent_index)
    end function parse_end_statement

    function parse_goto_statement(parser, arena) result(goto_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: goto_index

        type(token_t) :: token
        integer :: line, column, selector_index
        character(len=:), allocatable :: label, label_list, label_item
        logical :: is_computed

        ! Consume 'go' or 'goto' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        is_computed = .false.
        selector_index = 0
        label_list = ""

        select case (trim(to_lower(token%text)))
        case ("goto")
            token = parser%consume()
            token = parser%peek()

            ! Check for computed GOTO: goto (label_list), selector
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                is_computed = .true.
                token = parser%consume()

                ! Parse label list
                do
                    token = parser%peek()
                    if (token%kind == TK_NUMBER .or. token%kind == TK_IDENTIFIER) then
                        label_item = trim(token%text)
                        if (len_trim(label_list) > 0) then
                            label_list = label_list//", "//label_item
                        else
                            label_list = label_item
                        end if
                        token = parser%consume()

                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == ",") then
                            token = parser%consume()
                        else
                            exit
                        end if
                    else
                        exit
                    end if
                end do

                ! Consume closing parenthesis
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()

                    ! Consume optional comma before selector expression
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == ",") then
                        token = parser%consume()
                    end if

                    ! Parse selector expression (required for computed GOTO)
                    selector_index = parse_comparison(parser, arena)
                end if
            else if (token%kind == TK_NUMBER .or. token%kind == TK_IDENTIFIER) then
                label = trim(token%text)
                token = parser%consume()
            else
                label = ""
            end if
        case default
            token = parser%consume()

            token = parser%peek()
            ! Accept 'to' as either keyword or identifier (to is not a reserved word)
            if ((token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER) .and. &
                trim(to_lower(token%text)) == "to") then
                token = parser%consume()

                token = parser%peek()
                ! Check for computed GOTO: go to (label_list), selector
                if (token%kind == TK_OPERATOR .and. token%text == "(") then
                    is_computed = .true.
                    token = parser%consume()

                    ! Parse label list
                    do
                        token = parser%peek()
                        if (token%kind == TK_NUMBER .or. &
                            token%kind == TK_IDENTIFIER) then
                            label_item = trim(token%text)
                            if (len_trim(label_list) > 0) then
                                label_list = label_list//", "//label_item
                            else
                                label_list = label_item
                            end if
                            token = parser%consume()

                            token = parser%peek()
                            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                                token = parser%consume()
                            else
                                exit
                            end if
                        else
                            exit
                        end if
                    end do

                    ! Consume closing parenthesis
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == ")") then
                        token = parser%consume()

                        ! Consume optional comma before selector expression
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == ",") then
                            token = parser%consume()
                        end if

                        ! Parse selector expression (required for computed GOTO)
                        selector_index = parse_comparison(parser, arena)
                    end if
                else if (token%kind == TK_NUMBER .or. token%kind == TK_IDENTIFIER) then
                    label = trim(token%text)
                    token = parser%consume()
                else
                    label = ""
                end if
            else
                label = ""
            end if
        end select

        ! Create GOTO node
        if (is_computed) then
            if (len_trim(label_list) > 0 .and. selector_index > 0) then
                goto_index = push_goto(arena, label_list=label_list, &
                    selector_index=selector_index, line=line, &
                    column=column)
            else
                ! Invalid computed GOTO
                goto_index = push_goto(arena, label="INVALID_LABEL", line=line, &
                    column=column)
            end if
        else
            ! Simple GOTO
            if (.not. allocated(label) .or. len_trim(label) == 0) then
                label = "INVALID_LABEL"
            end if
            goto_index = push_goto(arena, label=label, line=line, column=column)
        end if
    end function parse_goto_statement

    function parse_error_stop_statement(parser, arena) result(error_stop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: error_stop_index

        type(token_t) :: token
        integer :: line, column, error_code_index
        character(len=:), allocatable :: error_message

        ! Consume 'error' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()

        ! Consume 'stop' keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "stop") then
            token = parser%consume()
        else
            ! This shouldn't happen if called correctly, but handle gracefully
            error_stop_index = push_error_stop(arena, line=line, column=column)
            return
        end if

        ! Check for optional error code or message
        token = parser%peek()
        error_code_index = 0
        error_message = ""

        if (token%kind == TK_STRING) then
            ! String literal message
            error_message = token%text
            token = parser%consume()
        else if (token%kind == TK_NUMBER .or. token%kind == TK_IDENTIFIER) then
            ! Integer expression or variable
            error_code_index = parse_comparison(parser, arena)
            if (error_code_index <= 0) then
                ! Failed to parse error code expression - create basic error stop
                error_code_index = 0
            end if
        end if

        call skip_stop_specifiers(parser, arena)

        ! Create ERROR STOP node
        error_stop_index = push_error_stop(arena, error_code_index, error_message, &
            line=line, column=column)
    end function parse_error_stop_statement

    function parse_pause_statement(parser, arena) result(pause_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: pause_index

        type(token_t) :: token
        integer :: line, column, pause_code_index
        character(len=:), allocatable :: pause_message

        ! Consume 'pause' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()

        ! Check for optional pause code or message
        token = parser%peek()
        pause_code_index = 0
        pause_message = ""

        if (token%kind == TK_STRING) then
            ! String literal message
            pause_message = token%text
            token = parser%consume()
        else if (token%kind == TK_NUMBER .or. token%kind == TK_IDENTIFIER) then
            ! Integer expression or variable
            pause_code_index = parse_comparison(parser, arena)
        end if

        ! Create PAUSE node
        if (len_trim(pause_message) > 0) then
            pause_index = push_pause(arena, pause_message=pause_message, &
                line=line, column=column)
        else
            pause_index = push_pause(arena, pause_code_index=pause_code_index, &
                line=line, column=column)
        end if
    end function parse_pause_statement

    function parse_cycle_statement(parser, arena) result(cycle_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: cycle_index

        type(token_t) :: token
        integer :: line, column
        character(len=:), allocatable :: loop_label

        ! Consume 'cycle' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()

        ! Check for optional loop label
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            loop_label = token%text
            token = parser%consume()
        else
            loop_label = ""
        end if

        ! Create CYCLE node
        if (len_trim(loop_label) > 0) then
            cycle_index = push_cycle(arena, loop_label=loop_label, &
                line=line, column=column)
        else
            cycle_index = push_cycle(arena, line=line, column=column)
        end if
    end function parse_cycle_statement

    function parse_exit_statement(parser, arena) result(exit_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: exit_index

        type(token_t) :: token
        integer :: line, column
        character(len=:), allocatable :: loop_label

        ! Consume 'exit' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()

        ! Check for optional loop label
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            loop_label = token%text
            token = parser%consume()
        else
            loop_label = ""
        end if

        ! Create EXIT node
        if (len_trim(loop_label) > 0) then
            exit_index = push_exit(arena, loop_label=loop_label, &
                line=line, column=column)
        else
            exit_index = push_exit(arena, line=line, column=column)
        end if
    end function parse_exit_statement

    function parse_nullify_statement(parser, arena) result(nullify_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: nullify_index

        type(token_t) :: token
        integer :: line, column
        integer, allocatable :: pointer_indices(:)
        integer, allocatable :: temp_indices(:)
        integer :: var_index, idx_count

        ! Consume 'nullify' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()

        ! Expect opening parenthesis
        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            nullify_index = 0
            return
        end if
        token = parser%consume()

        ! Parse comma-separated list of pointer variables
        allocate (pointer_indices(0))
        idx_count = 0

        do
            token = parser%peek()

            ! Check for closing parenthesis
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            end if

            ! Parse variable reference
            var_index = parse_comparison(parser, arena)
            if (var_index > 0) then
                idx_count = idx_count + 1
                allocate (temp_indices(idx_count))
                if (idx_count > 1) temp_indices(1:idx_count - 1) = pointer_indices
                temp_indices(idx_count) = var_index
                call move_alloc(temp_indices, pointer_indices)
            end if

            ! Check for comma
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
            else if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            else
                exit
            end if
        end do

        ! Create NULLIFY node
        nullify_index = push_nullify(arena, pointer_indices=pointer_indices, &
            line=line, column=column)
    end function parse_nullify_statement

end module parser_control_statements_module
