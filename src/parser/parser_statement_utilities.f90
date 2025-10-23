module parser_statement_utilities_module
    ! Parser utility functions for statement parsing within function/subroutine bodies
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
                          TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
                          TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_declarations, only: parse_declaration
    use parser_expressions_module, only: parse_comparison
    use parser_io_statements_module, only: parse_print_statement, &
                                           parse_write_statement, &
                                           parse_open_statement, &
                                           parse_close_statement, &
                                           parse_read_statement
    use parser_control_statements_module, only: parse_stop_statement, &
                                                parse_return_statement, &
                                                parse_entry_statement, &
                                                parse_goto_statement, &
                                                parse_error_stop_statement, &
                                                parse_cycle_statement, &
                                                parse_exit_statement, &
                                                parse_nullify_statement
    use parser_memory_statements_module, only: parse_allocate_statement, &
                                               parse_deallocate_statement
    use parser_assignment_module, only: parse_assignment_statement
    use parser_call_module, only: parse_call_statement
    use parser_statement_data_module, only: parse_data_statement
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_associate, push_if, push_literal
    use ast_factory
    use ast_types, only: LITERAL_STRING
    use ast_nodes_control, only: association_t
    use parser_legacy_statements_module, only: parse_legacy_statement
    implicit none
    private

    public :: parse_statement_in_if_block

contains

    ! Statement parsing for if blocks - moved here to break circular dependency
    function parse_statement_in_if_block(parser, arena, token) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: token
        integer :: stmt_index
        character(len=:), allocatable :: lowered_text
        type(token_t) :: next_token

        ! Handle "goto" as identifier (Fortran allows both "go to" and "goto")
        lowered_text = trim(to_lower(token%text))
        if (lowered_text == "goto") then
            ! Parser is pointing AT the goto token (caller peeked but didn't consume)
            ! Consume it here, then parse the label
            next_token = parser%consume()  ! Consume "goto"
            next_token = parser%peek()
            if (next_token%kind == TK_NUMBER .or. next_token%kind == &
                TK_IDENTIFIER) then
                stmt_index = push_goto(arena, label=trim(next_token%text), &
                                       line=token%line, column=token%column)
                next_token = parser%consume()  ! Consume the label
            else
                ! Invalid goto - missing label
                stmt_index = push_goto(arena, label="INVALID_LABEL", &
                                       line=token%line, column=token%column)
            end if
            return
        end if

        ! Simplified statement parsing for if blocks
        select case (token%kind)
        case (TK_KEYWORD)
            select case (trim(to_lower(token%text)))
            case ("print")
                stmt_index = parse_print_statement(parser, arena)
            case ("write")
                stmt_index = parse_write_statement(parser, arena)
            case ("open")
                stmt_index = parse_open_statement(parser, arena)
            case ("close")
                stmt_index = parse_close_statement(parser, arena)
            case ("read")
                stmt_index = parse_read_statement(parser, arena)
            case ("data")
                stmt_index = parse_data_statement(parser, arena)
            case ("call")
                stmt_index = parse_call_statement(parser, arena)
            case ("integer", "real", "logical", "character", "complex", &
                  "double", "type", "class")
                stmt_index = parse_declaration(parser, arena)
            case ("allocate")
                stmt_index = parse_allocate_statement(parser, arena)
            case ("deallocate")
                stmt_index = parse_deallocate_statement(parser, arena)
            case ("if")
                ! Forward to parse_if_from_definition to avoid circular dependency
                stmt_index = parse_if_from_definition(parser, arena)
            case ("stop")
                stmt_index = parse_stop_statement(parser, arena)
            case ("return")
                stmt_index = parse_return_statement(parser, arena)
            case ("entry")
                stmt_index = parse_entry_statement(parser, arena)
            case ("goto", "go")
                stmt_index = parse_goto_statement(parser, arena)
            case ("error")
                stmt_index = parse_error_stop_statement(parser, arena)
            case ("cycle")
                stmt_index = parse_cycle_statement(parser, arena)
            case ("exit")
                stmt_index = parse_exit_statement(parser, arena)
            case ("nullify")
                stmt_index = parse_nullify_statement(parser, arena)
            case ("associate")
                stmt_index = parse_associate_from_definition(parser, arena)
            case ("import")
                stmt_index = parse_import_stmt_inline(parser, arena)
            case ("equivalence", "common")
                stmt_index = parse_legacy_statement(trim(to_lower(token%text)), &
                                                    parser, arena)
            case default
                stmt_index = skip_unknown_statement(parser)
            end select
        case default
            stmt_index = parse_assignment_simple(parser, arena)
        end select
    end function parse_statement_in_if_block

    ! Simple assignment parser (utility function)
    function parse_assignment_simple(parser, arena) result(assign_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: assign_index

        ! Use the full assignment parser which handles multi-variable assignments
        integer, allocatable :: extra_indices(:)

        call parse_assignment_statement(parser, arena, assign_index, extra_indices)
    end function parse_assignment_simple

    ! Skip unknown statement (utility function)
    function skip_unknown_statement(parser) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        integer :: stmt_index

        type(token_t) :: token

        ! Skip tokens until end of statement
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE .or. &
                (token%kind == TK_KEYWORD .and. &
                 (token%text == "end" .or. token%text == "endif"))) then
                exit
            end if
            token = parser%consume()
        end do

        stmt_index = 0  ! No valid statement created
    end function skip_unknown_statement

    ! Simple if statement parser for function/subroutine bodies
    ! This avoids circular dependency with parser_control_flow_module
    function parse_if_from_definition(parser, arena) result(if_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: if_index

        type(token_t) :: if_token, then_token, token
        integer :: condition_index
        integer, allocatable :: then_body_indices(:), else_body_indices(:)

        ! Consume 'if' keyword
        if_token = parser%consume()

        ! Parse condition (parentheses)
        condition_index = parse_comparison(parser, arena)

        ! Look for 'then' keyword
        then_token = parser%peek()
        if (then_token%kind == TK_KEYWORD .and. then_token%text == "then") then
            token = parser%consume()

            ! Parse then body
            allocate (then_body_indices(0))
            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_KEYWORD) then
                    if (token%text == "else" .or. token%text == "end") then
                        exit
                    end if
                end if

                ! Parse a statement
                block
                    integer :: stmt_index
                    stmt_index = parse_statement_in_if_block(parser, arena, token)
                    if (stmt_index > 0) then
                        then_body_indices = [then_body_indices, stmt_index]
                    else
                        token = parser%consume()  ! Skip unknown statement
                    end if
                end block
            end do

            ! Check for else
            allocate (else_body_indices(0))
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "else") then
                token = parser%consume()

                ! Parse else body
                do while (.not. parser%is_at_end())
                    token = parser%peek()
                    if (token%kind == TK_KEYWORD .and. token%text == "end") then
                        exit
                    end if

                    ! Parse a statement
                    block
                        integer :: stmt_index
                        stmt_index = parse_statement_in_if_block(parser, arena, token)
                        if (stmt_index > 0) then
                            else_body_indices = [else_body_indices, stmt_index]
                        else
                            token = parser%consume()  ! Skip unknown statement
                        end if
                    end block
                end do
            end if

            ! Consume "end if"
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                token = parser%consume()
                token = parser%peek()
                if (token%kind == TK_KEYWORD .and. token%text == "if") then
                    token = parser%consume()
                end if
            end if

            ! Create if node
            if_index = push_if(arena, condition_index, then_body_indices, &
                               else_body_indices=else_body_indices, &
                               line=if_token%line, column=if_token%column)
        else
            ! Single-line if statement - parse the single statement
            allocate (then_body_indices(1))

            ! Parse the single statement
            token = parser%peek()
            block
                integer :: stmt_index
                stmt_index = parse_statement_in_if_block(parser, arena, token)
                if (stmt_index > 0) then
                    then_body_indices(1) = stmt_index
                else
                    ! Failed to parse statement, create empty body
                    deallocate (then_body_indices)
                    allocate (then_body_indices(0))
                end if
            end block

            ! Create if node with single statement in then body
            if_index = push_if(arena, condition_index, then_body_indices, &
                               line=if_token%line, column=if_token%column)
        end if

    end function parse_if_from_definition

    ! Simple associate statement parser for function/subroutine bodies
    ! This avoids circular dependency with parser_control_flow_module
    function parse_associate_from_definition(parser, arena) result(assoc_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: assoc_index

        type(token_t) :: token, first_token
        type(association_t), allocatable :: associations(:)
        integer, allocatable :: body_indices(:)
        integer :: i, assoc_count, line, column

        ! Consume 'associate' keyword
        first_token = parser%consume()
        line = first_token%line
        column = first_token%column

        ! Parse associations (simplified)
        allocate (associations(0))
        allocate (body_indices(0))

        ! Look for opening parenthesis
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()

            ! Parse associations
            assoc_count = 0
            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()
                    exit
                end if

                ! Parse association: name => expr
                if (token%kind == TK_IDENTIFIER) then
                    block
                        character(len=:), allocatable :: assoc_name
                        integer :: target_index
                        type(association_t) :: new_assoc

                        assoc_name = token%text
                        token = parser%consume()

                        ! Look for =>
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "=>") then
                            token = parser%consume()

                            ! Parse target expression
                            target_index = parse_comparison(parser, arena)

                            ! Create association
                            new_assoc%name = assoc_name
                            new_assoc%expr_index = target_index

                            ! Add to associations array
                            associations = [associations, new_assoc]
                            assoc_count = assoc_count + 1
                        end if
                    end block
                end if

                ! Check for comma
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()
                else if (token%kind /= TK_OPERATOR .or. token%text /= ")") then
                    ! Skip unexpected token
                    token = parser%consume()
                end if
            end do
        end if

        ! Parse body statements until 'end associate'
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                ! Look ahead for "associate"
                block
                    integer :: saved_pos
                    saved_pos = parser%current_token
                    token = parser%consume()
                    token = parser%peek()
                    if (token%kind == TK_KEYWORD .and. token%text == "associate") then
                        token = parser%consume()
                        exit
                    else
                        parser%current_token = saved_pos
                    end if
                end block
            end if

            ! Parse a statement
            block
                integer :: stmt_index
                stmt_index = parse_statement_in_if_block(parser, arena, token)
                if (stmt_index > 0) then
                    body_indices = [body_indices, stmt_index]
                else
                    token = parser%consume()  ! Skip unknown statement
                end if
            end block
        end do

        ! Create associate node
        assoc_index = push_associate(arena, associations, body_indices, line, column)

    end function parse_associate_from_definition

    function parse_import_stmt_inline(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index

        character(len=:), allocatable :: import_names(:)
        logical :: has_double_colon, is_all, is_none
        integer :: line, column
        type(token_t) :: token
        character(len=:), allocatable :: lowered_text
        integer :: name_count

        stmt_index = 0
        has_double_colon = .false.
        is_all = .false.
        is_none = .false.
        name_count = 0
        allocate (character(len=100) :: import_names(0))

        token = parser%consume()
        line = token%line
        column = token%column

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume()
            token = parser%peek()
            if (token%kind == TK_KEYWORD) then
                lowered_text = to_lower(token%text)
                if (trim(lowered_text) == "all") then
                    is_all = .true.
                    token = parser%consume()
                else if (trim(lowered_text) == "none") then
                    is_none = .true.
                    token = parser%consume()
                end if
            end if
        else if (token%kind == TK_OPERATOR .and. token%text == "::") then
            has_double_colon = .true.
            token = parser%consume()
        end if

        if (.not. is_all .and. .not. is_none) then
            do while (.not. parser%is_at_end())
                token = parser%peek()
                select case (token%kind)
                case (TK_IDENTIFIER, TK_KEYWORD)
                    name_count = name_count + 1
                    block
                        character(len=:), allocatable :: tmp_names(:)
                        integer :: i
                        allocate (character(len=100) :: tmp_names(name_count))
                        do i = 1, name_count - 1
                            tmp_names(i) = import_names(i)
                        end do
                        tmp_names(name_count) = trim(token%text)
                        call move_alloc(tmp_names, import_names)
                    end block
                    token = parser%consume()
                case (TK_OPERATOR)
                    if (trim(token%text) == ",") then
                        token = parser%consume()
                    else
                        exit
                    end if
                case (TK_WHITESPACE)
                    token = parser%consume()
                case (TK_NEWLINE, TK_COMMENT)
                    exit
                case default
                    exit
                end select
            end do
        end if

        if (name_count > 0) then
            stmt_index = push_import_statement(arena, import_names(1:name_count), &
                                               has_double_colon=has_double_colon, &
                                               line=line, column=column)
        else
            stmt_index = push_import_statement(arena, &
                                               has_double_colon=has_double_colon, &
                                               is_all=is_all, is_none=is_none, &
                                               line=line, column=column)
        end if
    end function parse_import_stmt_inline

end module parser_statement_utilities_module
