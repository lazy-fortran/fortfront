
    ! Parse a statement by dispatching to appropriate parsing module
    function parse_statement_dispatcher(tokens, arena, prefix_buffer) &
        result(stmt_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer :: stmt_index
        character(len=:), allocatable :: lowered_keyword
        type(parser_state_t) :: parser
        type(token_t) :: first_token, second_token, next_token
        integer :: target_index, value_index

        call init_interface_procedure_parser()

        parser = create_parser_state(tokens)
        first_token = parser%peek()
        ! Dispatch based on first token
        select case (first_token%kind)
        case (TK_KEYWORD)
            lowered_keyword = to_lower(trim(first_token%text))
            select case (lowered_keyword)
            case ("use")
                stmt_index = parse_use_statement(parser, arena)
            case ("intrinsic")
                stmt_index = parse_intrinsic_statement(parser, arena)
            case ("include")
                stmt_index = parse_include_statement(parser, arena)
            case ("print")
                stmt_index = parse_print_statement(parser, arena)
            case ("write")
                stmt_index = parse_write_statement(parser, arena)
            case ("read")
                stmt_index = parse_read_statement(parser, arena)
            case ("open")
                stmt_index = parse_open_statement(parser, arena)
            case ("close")
                stmt_index = parse_close_statement(parser, arena)
            case ("format")
                stmt_index = parse_format_statement(parser, arena)
            case ("inquire")
                stmt_index = parse_inquire_statement(parser, arena)
            case ("backspace")
                stmt_index = parse_backspace_statement(parser, arena)
            case ("rewind")
                stmt_index = parse_rewind_statement(parser, arena)
            case ("endfile")
                stmt_index = parse_endfile_statement(parser, arena)
            case ("allocate")
                stmt_index = parse_allocate_statement(parser, arena)
            case ("deallocate")
                stmt_index = parse_deallocate_statement(parser, arena)
            case ("if", "do", "where", "select", "forall", "associate")
                if (keyword_should_parse_as_identifier(first_token, parser)) then
                    stmt_index = parse_assignment_or_expression(parser, arena)
                else
                    stmt_index = route_control_flow(parser, arena)
                end if
            case ("function")
                stmt_index = parse_function_definition(parser, arena, prefix_buffer)
            case ("subroutine")
                stmt_index = parse_subroutine_definition(parser, arena, prefix_buffer)
            case ("interface")
                stmt_index = parse_interface_block(parser, arena, prefix_buffer)
            case ("abstract")
                block
                    integer :: lookahead_idx
                    type(token_t) :: next_token
                    logical :: is_abstract_interface

                    is_abstract_interface = .false.
                    lookahead_idx = parser%current_token + 1
                    do while (lookahead_idx <= size(parser%tokens))
                        next_token = parser%tokens(lookahead_idx)
                        select case (next_token%kind)
                        case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                            lookahead_idx = lookahead_idx + 1
                            cycle
                        case (TK_KEYWORD, TK_IDENTIFIER)
                            if (to_lower(trim(next_token%text)) == "interface") then
                                is_abstract_interface = .true.
                            end if
                            exit
                        case default
                            exit
                        end select
                    end do

                    if (is_abstract_interface) then
                        next_token = parser%consume()
                        stmt_index = parse_interface_block(parser, arena, &
                                                           prefix_buffer, &
                                                           is_abstract=.true.)
                    else
                        stmt_index = parse_type_or_declaration(parser, arena, &
                                                               prefix_buffer)
                    end if
                end block
            case ("module")
                ! Check if this is an identifier assignment like "module = 42"
                if (keyword_should_parse_as_identifier(first_token, parser)) then
                    stmt_index = parse_assignment_or_expression(parser, arena)
                else
                    stmt_index = parse_module(parser, arena)
                end if
            case ("submodule")
                stmt_index = parse_submodule_statement(parser, arena)
            case ("block")
                block
                    integer :: lookahead
                    logical :: is_block_data
                    type(token_t) :: lookahead_token

                    is_block_data = .false.
                    lookahead = parser%current_token + 1

                    do while (lookahead <= size(parser%tokens))
                        lookahead_token = parser%tokens(lookahead)
                        select case (lookahead_token%kind)
                        case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                            lookahead = lookahead + 1
                            cycle
                        case (TK_KEYWORD, TK_IDENTIFIER)
                            if (to_lower(trim(lookahead_token%text)) == "data") then
                                is_block_data = .true.
                            end if
                            exit
                        case default
                            exit
                        end select
                        lookahead = lookahead + 1
                    end do

                    if (is_block_data) then
                        stmt_index = parse_block_data(parser, arena)
                    else
                        stmt_index = route_control_flow(parser, arena)
                    end if
                end block
            case ("program")
                ! Check if this is an identifier assignment like "program = 42"
                if (keyword_should_parse_as_identifier(first_token, parser)) then
                    stmt_index = parse_assignment_or_expression(parser, arena)
                else
                    stmt_index = parse_program_statement(parser, arena)
                end if
            case ("type")
                stmt_index = parse_type_or_declaration(parser, arena, prefix_buffer)
            case ("real", "integer", "logical", "character", "complex", "double", &
                  "class", "procedure")
                ! Check if this is actually an assignment like "double = 5"
                next_token = parser%get_token_at_index(parser%current_token + 1)
                if (next_token%kind == TK_OPERATOR .and. &
                    (next_token%text == "=" .or. next_token%text == "=>")) then
                    stmt_index = parse_assignment_or_expression(parser, arena)
                else
                    stmt_index = parse_type_or_declaration(parser, arena, &
                                                           prefix_buffer)
                end if
            case ("call")
                ! Check if this is an identifier assignment like "call = 42"
                if (keyword_should_parse_as_identifier(first_token, parser)) then
                    stmt_index = parse_assignment_or_expression(parser, arena)
                else
                    stmt_index = parse_call_statement(parser, arena)
                end if
            case ("implicit")
                if (keyword_should_parse_as_identifier(first_token, parser)) then
                    stmt_index = parse_assignment_or_expression(parser, arena)
                else
                    stmt_index = parse_implicit_statement(parser, arena)
                    block
                        integer, allocatable :: extra_indices(:)
                        extra_indices = take_implicit_additional_indices()
                        if (size(extra_indices) > 0) then
                            if (allocated(additional_indices)) then
                                block
                                    integer, allocatable :: temp(:)
                                    call move_alloc(additional_indices, temp)
                                end block
                            end if
                            call move_alloc(extra_indices, additional_indices)
                        end if
                    end block
                end if
            case ("stop")
                ! Check if this is an identifier assignment like "stop = 42"
                if (keyword_should_parse_as_identifier(first_token, parser)) then
                    stmt_index = parse_assignment_or_expression(parser, arena)
                else
                    stmt_index = parse_stop_statement(parser, arena)
                end if
            case ("pause")
                stmt_index = parse_pause_statement(parser, arena)
            case ("return")
                ! Check if this is an identifier assignment like "return = 42"
                if (keyword_should_parse_as_identifier(first_token, parser)) then
                    stmt_index = parse_assignment_or_expression(parser, arena)
                else
                    stmt_index = parse_return_statement(parser, arena)
                end if
            case ("entry")
                ! Check if this is an identifier assignment like "entry = 42"
                if (keyword_should_parse_as_identifier(first_token, parser)) then
                    stmt_index = parse_assignment_or_expression(parser, arena)
                else
                    stmt_index = parse_entry_statement(parser, arena)
                end if
            case ("go", "goto")
                ! Check if this is an identifier assignment like "goto = 42"
                if (keyword_should_parse_as_identifier(first_token, parser)) then
                    stmt_index = parse_assignment_or_expression(parser, arena)
                else
                    stmt_index = parse_goto_statement(parser, arena)
                end if
            case ("error")
                stmt_index = parse_error_stop_statement(parser, arena)
            case ("cycle")
                ! Check if this is an identifier assignment like "cycle = 42"
                if (keyword_should_parse_as_identifier(first_token, parser)) then
                    stmt_index = parse_assignment_or_expression(parser, arena)
                else
                    stmt_index = parse_cycle_statement(parser, arena)
                end if
            case ("exit")
                ! Check if this is an identifier assignment like "exit = 42"
                if (keyword_should_parse_as_identifier(first_token, parser)) then
                    stmt_index = parse_assignment_or_expression(parser, arena)
                else
                    stmt_index = parse_exit_statement(parser, arena)
                end if
            case ("nullify")
                stmt_index = parse_nullify_statement(parser, arena)
            case ("end")
                stmt_index = parse_end_statement(parser, arena)
            case ("data")
                if (looks_like_data_statement(parser%tokens, parser%current_token)) then
                    stmt_index = parse_data_statement(parser, arena)
                    block
                        integer, allocatable :: extra_indices(:)
                        extra_indices = get_data_additional_indices()
                        if (size(extra_indices) > 0) then
                            if (allocated(additional_indices)) then
                                block
                                    integer, allocatable :: temp(:)
                                    call move_alloc(additional_indices, temp)
                                end block
                            end if
                            call move_alloc(extra_indices, additional_indices)
                        end if
                    end block
                else
                    stmt_index = parse_assignment_or_expression(parser, arena)
                end if
            case ("namelist")
                stmt_index = parse_namelist_statement(parser, arena)
            case ("equivalence", "common")
                if (keyword_should_parse_as_identifier(first_token, parser)) then
                    stmt_index = parse_assignment_or_expression(parser, arena)
                else
                    stmt_index = parse_legacy_statement(lowered_keyword, parser, &
                                                        arena)
                end if
            case ("enum", "enumerator")
                stmt_index = parse_unsupported_statement(lowered_keyword, &
                                                         parser, arena)
            case default
                if (.not. try_handle_prefix_sequence(parser, arena, prefix_buffer, &
                                                     stmt_index)) then
                    stmt_index = parse_as_expression(tokens, arena)
                end if
            end select
        case (TK_NUMBER)
            block
                integer :: lookahead_idx
                logical :: matched_block_data

                matched_block_data = .false.
                lookahead_idx = parser%current_token + 1
                do while (lookahead_idx <= size(parser%tokens))
                    next_token = parser%tokens(lookahead_idx)
                    select case (next_token%kind)
                    case (TK_WHITESPACE, TK_COMMENT)
                        lookahead_idx = lookahead_idx + 1
                        cycle
                    case (TK_NEWLINE)
                        exit
                    case (TK_KEYWORD)
                        if (to_lower(trim(next_token%text)) == "block") then
                            lookahead_idx = lookahead_idx + 1
                            do while (lookahead_idx <= size(parser%tokens))
                                next_token = parser%tokens(lookahead_idx)
                                select case (next_token%kind)
                                case (TK_WHITESPACE, TK_COMMENT)
                                    lookahead_idx = lookahead_idx + 1
                                    cycle
                                case (TK_KEYWORD, TK_IDENTIFIER)
                                    if (to_lower(trim(next_token%text)) == &
                                        "data") then
                                        matched_block_data = .true.
                                    end if
                                    exit
                                case default
                                    exit
                                end select
                            end do
                        end if
                        exit
                    case default
                        exit
                    end select
                end do

                if (matched_block_data) then
                    stmt_index = parse_block_data(parser, arena)
                else
                    if (.not. try_handle_prefix_sequence(parser, arena, &
                                                         prefix_buffer, &
                                                         stmt_index)) then
                        stmt_index = parse_as_expression(tokens, arena)
                    end if
                end if
            end block
        case (TK_IDENTIFIER)
            lowered_keyword = to_lower(trim(first_token%text))
            if (lowered_keyword == "class") then
                stmt_index = parse_type_or_declaration(parser, arena, prefix_buffer)
            else if (lowered_keyword == "goto") then
                stmt_index = parse_goto_statement(parser, arena)
            else
                if (.not. try_handle_prefix_sequence(parser, arena, prefix_buffer, &
                                                     stmt_index)) then
                    stmt_index = parse_assignment_or_expression(parser, arena)
                end if
            end if
        case (TK_COMMENT)
            ! Parse sentinel directives before general comments
            if (allocated(first_token%text)) then
                if (is_directive_comment(first_token%text)) then
                    stmt_index = parse_directive(parser, arena)
                else
                    stmt_index = parse_comment(parser, arena)
                end if
            else
                stmt_index = parse_comment(parser, arena)
            end if
        case (TK_NEWLINE)
            ! Parse blank line (newline token)
            stmt_index = parse_blank_line(parser, arena)
        case default
            ! Parse as expression
            stmt_index = parse_as_expression(tokens, arena)
        end select

        ! Extract parser errors before parser goes out of scope
        ! Only overwrite if there are new errors (preserve previous errors)
        if (parser%has_errors()) then
            last_parser_errors = parser%get_error_messages()
        end if
        ! Don't clear errors if this call didn't have any - preserve previous errors

    end function parse_statement_dispatcher

    ! Parse type declaration or derived type definition
    function parse_type_or_declaration(parser, arena, prefix_buffer) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer :: stmt_index
        type(token_t) :: first_token
        logical :: is_derived_type_def

        first_token = parser%peek()
        if (first_token%text == "type") then
            is_derived_type_def = parser_is_at_type_definition(parser)
            if (is_derived_type_def) then
                stmt_index = parse_derived_type_def(parser, arena)
            else
                stmt_index = parse_declaration(parser, arena)
            end if
        else
            ! Other type keywords - assume it's a declaration unless proven otherwise
            ! This handles cases like "real(kind=real64) :: x" where kind parameters
            ! might interfere with double colon detection
            if (has_double_colon(parser)) then
                ! Confirmed declaration with :: - check if single or multi-variable
                block
                    logical :: has_initializer, has_comma
                    integer, allocatable :: decl_indices(:)

                    call analyze_declaration_structure(parser, &
                                                       has_initializer, has_comma)

                    if (has_initializer .and. .not. has_comma) then
                        ! Single variable with initializer - use parse_declaration
                        stmt_index = parse_declaration(parser, arena)
                    else if (has_comma) then
                        ! Multi-variable declaration - use parse_multi_declaration
                        decl_indices = parse_multi_declaration(parser, arena)
                        if (allocated(decl_indices)) then
                            if (size(decl_indices) > 0) then
                                ! Return first declaration index
                                stmt_index = decl_indices(1)

                                ! Store additional indices if any
                                if (size(decl_indices) > 1) then
                                    additional_indices = decl_indices(2:)
                                end if
                            else
                                stmt_index = parse_declaration(parser, arena)
                                ! Fallback
                            end if
                        else
                            stmt_index = parse_declaration(parser, arena)  ! Fallback
                        end if
                    else
                        ! Single variable without initializer - use parse_declaration
                        stmt_index = parse_declaration(parser, arena)
                    end if
                end block
            else
                ! Check if this looks like a function definition
                if (looks_like_function_definition(parser)) then
                    stmt_index = parse_function_or_expression(parser, arena, &
                                                              prefix_buffer)
                else
                    ! Default to declaration parsing for type keywords
                    ! This handles "real(kind=real64) :: x" where :: detection fails
                    stmt_index = parse_declaration(parser, arena)
                end if
            end if
        end if

    end function parse_type_or_declaration

    ! Parse assignment or expression
    function parse_assignment_or_expression(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        integer, allocatable :: extra_indices(:)

        ! Delegate to full assignment parser which handles subscripts, components, etc
        call parse_assignment_statement(parser, arena, stmt_index, extra_indices)

        ! Store additional indices from multi-assignment if any
        if (allocated(extra_indices)) then
            if (size(extra_indices) > 0) then
                if (allocated(additional_indices)) then
                    block
                        integer, allocatable :: temp(:)
                        call move_alloc(additional_indices, temp)
                    end block
                end if
                additional_indices = extra_indices
            end if
        end if

    end function parse_assignment_or_expression

    ! Parse function definition or expression
    function parse_function_or_expression(parser, arena, prefix_buffer) &
        result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer :: stmt_index
        type(token_t) :: first_token, second_token

        first_token = parser%peek()

        ! Look ahead to see if next token is "function"
        if (parser%current_token + 1 <= size(parser%tokens)) then
            second_token = parser%tokens(parser%current_token + 1)
            if (second_token%kind == TK_KEYWORD .and. second_token%text == &
                "function") then
                stmt_index = parse_function_definition(parser, arena, prefix_buffer)
                return
            end if
        end if

        ! Not a function definition, parse as expression
        stmt_index = parse_as_expression(parser%tokens, arena)

    end function parse_function_or_expression

    ! Parse as expression
    function parse_as_expression(tokens, arena) result(stmt_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index

        stmt_index = parse_expression(tokens, arena)
    end function parse_as_expression

    ! Helper function to check for double colon
    logical function has_double_colon(parser)
        type(parser_state_t), intent(inout) :: parser
        integer :: i, paren_depth

        has_double_colon = .false.
        paren_depth = 0

        do i = parser%current_token + 1, min(parser%current_token + 50, &
                                             size(parser%tokens))
            if (parser%tokens(i)%kind == TK_OPERATOR) then
                if (parser%tokens(i)%text == "(") then
                    paren_depth = paren_depth + 1
                else if (parser%tokens(i)%text == ")") then
                    paren_depth = paren_depth - 1
                else if (parser%tokens(i)%text == "::" .and. paren_depth == 0) then
                    has_double_colon = .true.
                    exit
                end if
            else if (parser%tokens(i)%kind == TK_EOF) then
                exit  ! Stop on EOF
            else if (parser%tokens(i)%kind == TK_KEYWORD .and. paren_depth == 0) then
                ! Only stop on keywords outside of parentheses
                ! Allow declaration attribute keywords to continue search
                if (parser%tokens(i)%text == "parameter" .or. &
                    parser%tokens(i)%text == "optional" .or. &
                    parser%tokens(i)%text == "intent" .or. &
                    parser%tokens(i)%text == "allocatable" .or. &
                    parser%tokens(i)%text == "pointer" .or. &
                    parser%tokens(i)%text == "target" .or. &
                    parser%tokens(i)%text == "dimension" .or. &
                    parser%tokens(i)%text == "in" .or. &
                    parser%tokens(i)%text == "out" .or. &
                    parser%tokens(i)%text == "inout") then
                    cycle  ! Continue searching
                else
                    exit  ! Stop on other keywords outside parentheses
                end if
            end if
        end do
    end function has_double_colon

    ! Check if this looks like a function definition (e.g., "real function foo()")
    logical function looks_like_function_definition(parser)
        type(parser_state_t), intent(inout) :: parser
        integer :: i

        looks_like_function_definition = .false.

        ! Look for "function" keyword within the next few tokens
        do i = parser%current_token + 1, min(parser%current_token + 10, &
                                             size(parser%tokens))
            if (parser%tokens(i)%kind == TK_KEYWORD .and. parser%tokens(i)%text == &
                "function") then
                looks_like_function_definition = .true.
                exit
            else if (parser%tokens(i)%kind == TK_OPERATOR .and. parser%tokens(i)%text &
                     == "::") then
                ! Found :: before function - this is a declaration
                exit
            else if (parser%tokens(i)%kind == TK_EOF) then
                exit
            end if
        end do
    end function looks_like_function_definition

    ! Parse a comment token
    function parse_comment(parser, arena) result(comment_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: comment_index
        type(token_t) :: token
        type(comment_node) :: comment

        token = parser%consume()
        comment%uid = generate_uid()
        comment%text = token%text
        comment%line = token%line
        comment%column = token%column
        call arena%push(comment, "comment")
        comment_index = arena%size
    end function parse_comment

    ! Parse OpenMP/OpenACC sentinel directives
    function parse_directive(parser, arena) result(directive_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: directive_index
        type(token_t) :: token
        type(directive_node) :: directive
        character(len=:), allocatable :: lowered_text

        token = parser%consume()
        directive%uid = generate_uid()
        directive%line = token%line
        directive%column = token%column
        if (allocated(token%text)) then
            directive%text = token%text
            lowered_text = to_lower(adjustl(token%text))
        else
            directive%text = "!"
            lowered_text = "!"
        end if

        if (len(lowered_text) >= 5) then
            if (lowered_text(1:5) == "!$omp") directive%is_openmp = .true.
            if (lowered_text(1:5) == "!$acc") directive%is_openacc = .true.
        end if

        call arena%push(directive, "directive")
        directive_index = arena%size
    end function parse_directive

    pure logical function is_directive_comment(text) result(is_directive)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lowered
        integer :: length_text

        lowered = to_lower(adjustl(text))
        length_text = len(lowered)
        if (length_text < 5) then
            is_directive = .false.
            return
        end if

        is_directive = lowered(1:5) == "!$omp" .or. lowered(1:5) == "!$acc"
    end function is_directive_comment

    ! Parse a blank line (newline token)
    function parse_blank_line(parser, arena) result(blank_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: blank_index
        type(token_t) :: token
        type(blank_line_node) :: blank_line
        integer :: count

        ! Count consecutive newlines
        count = 0
        do while (parser%current_token <= size(parser%tokens))
            token = parser%peek()
            if (token%kind /= TK_NEWLINE) exit
            count = count + 1
            token = parser%consume()
        end do

        ! Create blank line node with count of consecutive lines
        blank_line%uid = generate_uid()
        blank_line%count = count
        blank_line%line = token%line
        blank_line%column = token%column
        call arena%push(blank_line, "blank_line")
        blank_index = arena%size
    end function parse_blank_line

    ! Get additional indices from multi-declaration parsing
    function get_additional_indices() result(indices)
        integer, allocatable :: indices(:)

        if (allocated(additional_indices)) then
            allocate (indices(size(additional_indices)))
            indices = additional_indices
        else
            allocate (indices(0))
        end if
    end function get_additional_indices

    ! Clear additional indices after use
    subroutine clear_additional_indices()
        if (allocated(additional_indices)) then
            block
                integer, allocatable :: temp(:)
                call move_alloc(additional_indices, temp)
            end block
        end if
    end subroutine clear_additional_indices

    logical function looks_like_data_statement(tokens, keyword_idx) &
        result(is_data_statement)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: keyword_idx
        integer :: idx

        is_data_statement = .false.
        if (keyword_idx < 1 .or. keyword_idx > size(tokens)) return

        idx = keyword_idx + 1
        do while (idx <= size(tokens))
            select case (tokens(idx)%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                idx = idx + 1
                cycle
            case (TK_NEWLINE)
                if (.not. newline_has_continuation(tokens, idx)) return
                idx = idx + 1
                cycle
            case (TK_OPERATOR)
                select case (trim(tokens(idx)%text))
                case ("/")
                    is_data_statement = .true.
                    return
                case ("&")
                    idx = idx + 1
                    cycle
                case (";")
                    return
                case default
                    idx = idx + 1
                end select
            case (TK_EOF)
                return
            case default
                idx = idx + 1
            end select
        end do
    end function looks_like_data_statement

    logical function newline_has_continuation(tokens, newline_idx) &
        result(has_continuation)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: newline_idx
        integer :: scan_idx

        has_continuation = .false.
        if (newline_idx < 1 .or. newline_idx > size(tokens)) return

        scan_idx = newline_idx - 1
        do while (scan_idx >= 1)
            select case (tokens(scan_idx)%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                scan_idx = scan_idx - 1
                cycle
            case (TK_OPERATOR)
                if (trim(tokens(scan_idx)%text) == "&") then
                    has_continuation = .true.
                end if
            end select
            exit
        end do
        if (has_continuation) return

        scan_idx = newline_idx + 1
        do while (scan_idx <= size(tokens))
            select case (tokens(scan_idx)%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                scan_idx = scan_idx + 1
                cycle
            case (TK_OPERATOR)
                if (trim(tokens(scan_idx)%text) == "&") then
                    has_continuation = .true.
                end if
            end select
            exit
        end do
    end function newline_has_continuation

    logical function try_handle_prefix_sequence(parser, arena, prefix_buffer, &
                                                stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer, intent(out) :: stmt_index
        character(len=16), allocatable :: local_prefixes(:)
        character(len=16), allocatable :: combined_prefixes(:)
        character(len=16), allocatable :: stored_prefixes(:)
        integer :: start_position, i
        type(token_t) :: token_after_prefix

        start_position = parser%current_token
        allocate (character(len=16) :: local_prefixes(0))
        call collect_prefix_keywords(parser, local_prefixes)

        allocate (character(len=16) :: combined_prefixes(0))
        call prefix_buffer%get_all(stored_prefixes)
        if (allocated(stored_prefixes)) then
            do i = 1, size(stored_prefixes)
                call append_prefix_token(combined_prefixes, stored_prefixes(i))
            end do
        end if
        if (allocated(local_prefixes)) then
            do i = 1, size(local_prefixes)
                call append_prefix_token(combined_prefixes, local_prefixes(i))
            end do
        end if

        if (size(combined_prefixes) > 0) then
            do
                token_after_prefix = parser%peek()
                if (token_after_prefix%kind == TK_WHITESPACE .or. &
                    token_after_prefix%kind == TK_NEWLINE) then
                    token_after_prefix = parser%consume()
                    cycle
                end if
                exit
            end do

            if (token_after_prefix%kind == TK_KEYWORD .and. &
                trim(to_lower(token_after_prefix%text)) == "function") then
                parser%current_token = start_position
                stmt_index = parse_function_definition(parser, arena, prefix_buffer, &
                                                       combined_prefixes)
            else
                parser%current_token = start_position
                call prefix_buffer%set(combined_prefixes)
                stmt_index = 0
            end if
            try_handle_prefix_sequence = .true.
        else
            parser%current_token = start_position
            stmt_index = 0
            try_handle_prefix_sequence = .false.
        end if
    end function try_handle_prefix_sequence

    subroutine collect_prefix_keywords(parser, prefixes)
        type(parser_state_t), intent(inout) :: parser
        character(len=16), allocatable, intent(inout) :: prefixes(:)
        type(token_t) :: token
        character(len=:), allocatable :: lowered

        do
            token = parser%peek()
            lowered = to_lower(token%text)
            if (token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER) then
                select case (trim(lowered))
                case ("elemental", "pure", "impure", "recursive", &
                      "nonrecursive", "non_recursive", "module")
                    call append_prefix_token(prefixes, trim(lowered))
                    token = parser%consume()
                case default
                    exit
                end select
            else
                exit
            end if
        end do
    end subroutine collect_prefix_keywords

    function parse_unsupported_statement(keyword, parser, arena) result(stmt_index)
        use ast_factory, only: push_error_node
        character(len=*), intent(in) :: keyword
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: error_msg
        integer :: line, column

        token = parser%consume()
        line = token%line
        column = token%column

        error_msg = "Unsupported Fortran feature: " // trim(keyword) // &
                    " constructs are not supported"

        stmt_index = push_error_node(arena, error_msg, keyword, line, column)
    end function parse_unsupported_statement
