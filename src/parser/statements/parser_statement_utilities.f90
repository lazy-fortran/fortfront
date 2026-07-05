module parser_statement_utilities_module
    ! Parser utility functions for statement parsing within function/subroutine bodies
    use lexer_core, only: token_t, TK_IDENTIFIER, &
        TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
        TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t
    use parser_declarations, only: parse_declaration, parse_multi_declaration
    use parser_parameter_statements_module, only: parse_parameter_statement
    use parser_utils, only: analyze_declaration_structure
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
        parse_nullify_statement, &
        parse_continue_statement, &
        parse_pause_statement
    use parser_memory_statements_module, only: parse_allocate_statement, &
        parse_deallocate_statement
    use parser_assignment_module, only: parse_assignment_statement
    use parser_call_module, only: parse_call_statement
    use parser_statement_data_module, only: parse_data_statement
    use parser_import_resolution_module, only: parse_use_statement
    use parser_keyword_disambiguation_module, only: keyword_should_parse_as_identifier
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_associate, push_if, &
        push_import_statement
    use ast_nodes_control, only: association_t
    use ast_nodes_misc, only: directive_node, comment_node
    use parser_legacy_statements_module, only: parse_legacy_statement
    use parser_common_statement_module, only: parse_common_statement
    use uid_generator, only: generate_uid
    implicit none
    private

    integer, allocatable :: stmt_util_additional_indices(:)

    public :: parse_statement_in_if_block, parse_comment_or_directive
    public :: get_stmt_util_additional_indices, clear_stmt_util_additional_indices
    public :: parse_if_from_definition
    public :: parse_associate_from_definition

contains

    function get_stmt_util_additional_indices() result(indices)
        integer, allocatable :: indices(:)
        if (allocated(stmt_util_additional_indices)) then
            allocate (indices(size(stmt_util_additional_indices)))
            indices = stmt_util_additional_indices
        else
            allocate (indices(0))
        end if
    end function get_stmt_util_additional_indices

    subroutine clear_stmt_util_additional_indices()
        if (allocated(stmt_util_additional_indices)) then
            deallocate (stmt_util_additional_indices)
        end if
    end subroutine clear_stmt_util_additional_indices

    ! Statement parsing for if blocks - moved here to break circular dependency
    function parse_statement_in_if_block(parser, arena, token) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: token
        integer :: stmt_index
        character(len=:), allocatable :: lowered_text
        type(token_t) :: next_token

        if (token%kind == TK_KEYWORD) then
            if (keyword_should_parse_as_identifier(token, parser)) then
                stmt_index = parse_assignment_simple(parser, arena)
                return
            end if
        end if

        ! Handle "goto" specially: treat as identifier when assignment is present
        lowered_text = trim(to_lower(token%text))
        if (lowered_text == "goto") then
            if (keyword_should_parse_as_identifier(token, parser)) then
                stmt_index = parse_assignment_simple(parser, arena)
            else
                stmt_index = parse_goto_statement(parser, arena)
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
            case ("use")
                stmt_index = parse_use_statement(parser, arena)
            case ("integer", "real", "logical", "character", "complex", &
                    "double", "type", "class", "procedure")
                ! Check if this is actually an assignment like "double = 5"
                next_token = parser%get_token_at_index(parser%current_token + 1)
                if (next_token%kind == TK_OPERATOR .and. &
                    (next_token%text == "=" .or. next_token%text == "=>")) then
                    stmt_index = parse_assignment_simple(parser, arena)
                else
                    block
                        logical :: has_initializer, has_comma
                        integer, allocatable :: decl_indices(:)
                        call analyze_declaration_structure(parser, has_initializer, &
                            has_comma)
                        if (has_comma) then
                            decl_indices = parse_multi_declaration(parser, arena)
                            if (allocated(decl_indices) .and. size(decl_indices) > 0) &
                                then
                                stmt_index = decl_indices(1)
                                if (size(decl_indices) > 1) then
                                    if (allocated(stmt_util_additional_indices)) then
                                        deallocate (stmt_util_additional_indices)
                                    end if
                                    allocate (stmt_util_additional_indices( &
                                        size(decl_indices) - 1))
                                    stmt_util_additional_indices = decl_indices(2:)
                                end if
                            else
                                stmt_index = 0
                            end if
                        else
                            stmt_index = parse_declaration(parser, arena)
                        end if
                    end block
                end if
            case ("parameter")
                if (parse_parameter_statement(parser, arena)) then
                    stmt_index = -1
                else
                    stmt_index = 0
                end if
            case ("allocate")
                stmt_index = parse_allocate_statement(parser, arena)
            case ("deallocate")
                stmt_index = parse_deallocate_statement(parser, arena)
            case ("if")
                ! Forward to parse_if_from_definition to avoid circular dependency
                stmt_index = parse_if_from_definition(parser, arena)
            case ("stop")
                stmt_index = parse_stop_statement(parser, arena)
            case ("pause")
                stmt_index = parse_pause_statement(parser, arena)
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
            case ("continue")
                stmt_index = parse_continue_statement(parser, arena)
            case ("associate")
                stmt_index = parse_associate_from_definition(parser, arena)
            case ("import")
                stmt_index = parse_import_stmt_inline(parser, arena)
            case ("equivalence")
                stmt_index = parse_legacy_statement(trim(to_lower(token%text)), &
                    parser, arena)
            case ("common")
                stmt_index = parse_common_statement(parser, arena)
            case default
                ! Check if this might be an assignment with a keyword as target
                ! (e.g., "double = 5" where "double" is both a keyword and a variable)
                next_token = parser%get_token_at_index(parser%current_token + 1)
                if (next_token%kind == TK_OPERATOR .and. &
                    (next_token%text == "=" .or. next_token%text == "=>")) then
                    stmt_index = parse_assignment_simple(parser, arena)
                else
                    stmt_index = skip_unknown_statement(parser)
                end if
            end select
        case default
            stmt_index = parse_assignment_simple(parser, arena)
        end select
    end function parse_statement_in_if_block

    ! Shared handling for OpenMP/OpenACC directives and regular comments
    function parse_comment_or_directive(parser, arena, comment_token) &
            result(node_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: comment_token
        integer :: node_index
        type(directive_node) :: directive
        type(comment_node) :: comment
        character(len=:), allocatable :: lowered_text

        node_index = 0

        if (allocated(comment_token%text)) then
            lowered_text = to_lower(adjustl(comment_token%text))
            if (len(lowered_text) >= 5) then
                if (lowered_text(1:5) == "!$omp" .or. lowered_text(1:5) == "!$acc") then
                    directive%uid = generate_uid()
                    directive%line = comment_token%line
                    directive%column = comment_token%column
                    if (allocated(comment_token%text)) then
                        directive%text = comment_token%text
                    else
                        directive%text = "!"
                    end if
                    if (lowered_text(1:5) == "!$omp") directive%is_openmp = .true.
                    if (lowered_text(1:5) == "!$acc") directive%is_openacc = .true.
                    call arena%push(directive, "directive")
                    node_index = arena%size
                    block
                        type(token_t) :: ignored_token
                        ignored_token = parser%consume()
                    end block
                    return
                end if
            end if
        end if

        comment%uid = generate_uid()
        if (allocated(comment_token%text)) then
            comment%text = comment_token%text
        else
            comment%text = "!"
        end if
        comment%line = comment_token%line
        comment%column = comment_token%column
        call arena%push(comment, "comment")
        node_index = arena%size
        block
            type(token_t) :: ignored_token
            ignored_token = parser%consume()
        end block
    end function parse_comment_or_directive

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

        stmt_index = 0 ! No valid statement created
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
        integer :: then_capacity, then_count, else_capacity, else_count
        integer, allocatable :: temp_indices(:)

        ! Consume 'if' keyword
        if_token = parser%consume()

        ! Parse condition (parentheses)
        condition_index = parse_comparison(parser, arena)

        ! Look for 'then' keyword
        then_token = parser%peek()
        if (then_token%kind == TK_KEYWORD .and. to_lower(then_token%text) == &
            "then") then
            token = parser%consume()

            ! Parse then body with efficient growth
            then_capacity = 64
            then_count = 0
            allocate (then_body_indices(then_capacity))

            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_KEYWORD) then
                    if (to_lower(token%text) == "else" .or. to_lower(token%text) == &
                        "end") then
                        exit
                    end if
                end if

                ! Parse a statement
                block
                    integer :: stmt_index
                    stmt_index = parse_statement_in_if_block(parser, arena, token)
                    if (stmt_index > 0) then
                        ! Grow array if needed
                        if (then_count >= then_capacity) then
                            then_capacity = then_capacity * 2
                            allocate (temp_indices(then_capacity))
                            temp_indices(1:then_count) = then_body_indices(1:then_count)
                            call move_alloc(temp_indices, then_body_indices)
                        end if
                        then_count = then_count + 1
                        then_body_indices(then_count) = stmt_index
                    else
                        token = parser%consume() ! Skip unknown statement
                    end if
                end block
            end do

            ! Trim then body to actual size
            if (then_count == 0) then
                deallocate (then_body_indices)
                allocate (then_body_indices(0))
            else if (then_count < then_capacity) then
                allocate (temp_indices(then_count))
                temp_indices = then_body_indices(1:then_count)
                call move_alloc(temp_indices, then_body_indices)
            end if

            ! Check for else
            else_capacity = 64
            else_count = 0
            allocate (else_body_indices(else_capacity))

            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "else") then
                token = parser%consume()

                ! Parse else body with efficient growth
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
                            ! Grow array if needed
                            if (else_count >= else_capacity) then
                                else_capacity = else_capacity * 2
                                allocate (temp_indices(else_capacity))
                                temp_indices(1:else_count) = &
                                    else_body_indices(1:else_count)
                                call move_alloc(temp_indices, else_body_indices)
                            end if
                            else_count = else_count + 1
                            else_body_indices(else_count) = stmt_index
                        else
                            token = parser%consume() ! Skip unknown statement
                        end if
                    end block
                end do
            end if

            ! Trim else body to actual size
            if (else_count == 0) then
                deallocate (else_body_indices)
                allocate (else_body_indices(0))
            else if (else_count < else_capacity) then
                allocate (temp_indices(else_count))
                temp_indices = else_body_indices(1:else_count)
                call move_alloc(temp_indices, else_body_indices)
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
            if (token%kind == TK_KEYWORD .and. to_lower(token%text) == "end") then
                ! Look ahead for "associate"
                block
                    integer :: saved_pos
                    saved_pos = parser%current_token
                    token = parser%consume()
                    token = parser%peek()
                    if (token%kind == TK_KEYWORD .and. &
                        to_lower(token%text) == "associate") then
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
                    token = parser%consume() ! Skip unknown statement
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
