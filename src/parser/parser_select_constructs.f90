module parser_select_constructs_module
    ! Parser module for SELECT CASE constructs
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
                          TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, TK_WHITESPACE
    use ast_types, only: LITERAL_STRING
    use parser_state_module
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
                           push_identifier, push_literal, push_assignment
    implicit none
    private

    public :: parse_select_case

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
            lower_index = parse_expression_until(sub_parser, arena, [":", ","])
            if (lower_index <= 0) then
                success = .false.
                exit
            end if

            sub_token = sub_parser%peek()

            if (sub_token%kind == TK_OPERATOR .and. sub_token%text == ":") then
                sub_token = sub_parser%consume()
                upper_index = parse_expression_until(sub_parser, arena, [","])
                if (upper_index <= 0) then
                    success = .false.
                    exit
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
        character(len=20), dimension(2) :: end_keywords
        type(statement_callbacks_t) :: callbacks
        logical :: values_ok

        ! Consume 'select'
        select_token = parser%consume()
        line = select_token%line
        column = select_token%column

        ! Expect 'case'
        case_token = parser%consume()
        if (case_token%kind /= TK_KEYWORD .or. case_token%text /= "case") then
            ! Error: expected 'case' after 'select'
            select_index = 0
            return
        end if

        ! Expect '('
        lparen_token = parser%consume()
        if (lparen_token%kind /= TK_OPERATOR .or. lparen_token%text /= "(") then
            ! Error: expected '(' after 'select case'
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

        ! Define end keywords for statement parsing
        end_keywords = [character(len=20) :: "case", "end"]

        callbacks = null_statement_callbacks()
        callbacks%parse_select_case => parse_select_case

        do while (parser%current_token <= size(parser%tokens))
            case_token = parser%peek()

            if (case_token%kind == TK_KEYWORD) then
                if (case_token%text == "case") then
                    ! Parse case block
                    block
                        type(token_t) :: value_token
                        integer :: case_block_index
                        integer, allocatable :: value_indices(:), body_indices(:)

                        case_token = parser%consume()  ! consume 'case'

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
                            value_token%text == "default") then
                            value_token = parser%consume()  ! consume 'default'

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

                                value_token = parser%consume()  ! consume '('
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
                else if (case_token%text == "end") then
                    ! Check for 'end select'
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == &
                        TK_KEYWORD .and. &
                        parser%tokens(parser%current_token + 1)%text == "select") then
                        ! Found 'end select', consume both tokens and exit
                        case_token = parser%consume()  ! consume 'end'
                        case_token = parser%consume()  ! consume 'select'
                        exit
                    end if
                    end if
                else
                    ! Other keyword, skip
                    parser%current_token = parser%current_token + 1
                end if
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

end module parser_select_constructs_module
