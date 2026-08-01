
    ! Parse a statement by dispatching to appropriate parsing module
    function parse_statement_dispatcher(tokens, arena, prefix_buffer, &
            diagnostic_sink, error_msg) &
        result(stmt_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        character(len=:), allocatable, intent(out), optional :: error_msg
        integer :: stmt_index
        type(parser_state_t) :: parser
        type(token_t) :: first_token

        if (present(error_msg)) error_msg = ""

        call init_interface_procedure_parser()
        if (present(diagnostic_sink)) then
            parser = create_parser_state(tokens, diagnostic_sink)
        else
            parser = create_parser_state(tokens)
        end if
        first_token = parser%peek()

        select case (first_token%kind)
        case (TK_KEYWORD)
            call dispatch_keyword(parser, arena, prefix_buffer, tokens, &
                                  first_token, stmt_index)
        case (TK_NUMBER)
            call dispatch_number_token(parser, arena, prefix_buffer, tokens, &
                                       stmt_index)
        case (TK_IDENTIFIER)
            call dispatch_identifier_token(parser, arena, prefix_buffer, &
                                           first_token, stmt_index)
        case (TK_COMMENT)
            call dispatch_comment_token(parser, arena, first_token, stmt_index)
        case (TK_NEWLINE)
            stmt_index = parse_blank_line(parser, arena)
        case default
            stmt_index = parse_as_expression(tokens, arena, parser)
        end select

        if (stmt_index > 0 .and. .not. parser%has_errors()) then
            select case (to_lower(trim(first_token%text)))
            case ("integer", "real", "logical", "character", "complex", &
                    "double", "class", "procedure", "type", "data")
                call reject_unconsumed_tokens(parser)
            end select
        end if

        if (parser%has_errors()) then
            if (present(error_msg)) error_msg = parser%get_error_messages()
            last_parser_errors = parser%get_error_messages()
        end if
    end function parse_statement_dispatcher

    subroutine dispatch_keyword(parser, arena, prefix_buffer, tokens, &
                                first_token, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        type(token_t), intent(in) :: tokens(:), first_token
        integer, intent(out) :: stmt_index
        character(len=:), allocatable :: lowered_keyword

        lowered_keyword = to_lower(trim(first_token%text))
        if (try_dispatch_io_keyword(parser, arena, lowered_keyword, stmt_index)) &
            return
        if (try_dispatch_control_keyword(parser, arena, first_token, &
                                         lowered_keyword, stmt_index)) return
        if (try_dispatch_definition_keyword(parser, arena, prefix_buffer, &
                                            first_token, lowered_keyword, &
                                                stmt_index)) return
        if (try_dispatch_declaration_keyword(parser, arena, prefix_buffer, &
                                             lowered_keyword, stmt_index)) return
        if (try_dispatch_execution_keyword(parser, arena, first_token, &
                                           lowered_keyword, stmt_index)) return
        if (try_dispatch_data_keyword(parser, arena, first_token, &
                                      lowered_keyword, stmt_index)) return
        if (.not. try_handle_prefix_sequence(parser, arena, prefix_buffer, &
                                             stmt_index)) then
            stmt_index = parse_as_expression(tokens, arena, parser)
        end if
    end subroutine dispatch_keyword

    logical function try_dispatch_io_keyword(parser, arena, keyword, &
                                             stmt_index) result(handled)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: keyword
        integer, intent(out) :: stmt_index
        type(token_t) :: first_token

        ! An I/O keyword used as an ordinary variable (e.g. "read = x") is
        ! not reserved in Fortran; let expression parsing handle it.
        select case (keyword)
        case ("print", "write", "read", "open", "close", "inquire", &
              "backspace", "rewind", "endfile")
            first_token = parser%peek()
            if (keyword_should_parse_as_identifier(first_token, parser)) then
                handled = .false.
                stmt_index = 0
                return
            end if
        end select

        handled = .true.
        select case (keyword)
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
        case default
            handled = .false.
        end select
    end function try_dispatch_io_keyword

    logical function try_dispatch_control_keyword(parser, arena, first_token, &
                                                  keyword, stmt_index) result(handled)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        character(len=*), intent(in) :: keyword
        integer, intent(out) :: stmt_index

        handled = .true.
        select case (keyword)
        case ("if", "do", "where", "select", "forall", "associate")
            if (keyword_should_parse_as_identifier(first_token, parser)) then
                stmt_index = parse_assignment_or_expression(parser, arena)
            else
                stmt_index = route_control_flow(parser, arena)
            end if
        case ("block")
            call handle_block_keyword(parser, arena, stmt_index)
        case default
            handled = .false.
        end select
    end function try_dispatch_control_keyword

    logical function try_dispatch_definition_keyword(parser, arena, &
                                                     prefix_buffer, first_token, &
                                                         keyword, stmt_index) &
                                                         result(handled)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        type(token_t), intent(in) :: first_token
        character(len=*), intent(in) :: keyword
        integer, intent(out) :: stmt_index

        handled = .true.
        select case (keyword)
        case ("use")
            stmt_index = parse_use_statement(parser, arena)
        case ("intrinsic")
            stmt_index = parse_intrinsic_statement(parser, arena)
        case ("external")
            stmt_index = parse_external_statement(parser, arena)
        case ("include")
            stmt_index = parse_include_statement(parser, arena)
        case ("instantiate")
            stmt_index = parse_instantiate_statement(parser, arena)
        case ("function")
            stmt_index = parse_function_definition(parser, arena, prefix_buffer)
        case ("subroutine")
            stmt_index = parse_subroutine_definition(parser, arena, prefix_buffer)
        case ("interface")
            stmt_index = parse_interface_block(parser, arena, prefix_buffer)
        case ("abstract")
            call handle_abstract_keyword(parser, arena, prefix_buffer, stmt_index)
        case ("module")
            call handle_module_keyword(parser, arena, first_token, stmt_index)
        case ("template")
            stmt_index = parse_template_block(parser, arena)
        case ("trait")
            stmt_index = parse_trait_block(parser, arena)
        case ("requirement")
            stmt_index = parse_requirement_block(parser, arena)
        case ("implements")
            stmt_index = parse_implements_block(parser, arena)
        case ("submodule")
            stmt_index = parse_submodule_statement(parser, arena)
        case ("program")
            call handle_program_keyword(parser, arena, first_token, stmt_index)
        case default
            handled = .false.
        end select
    end function try_dispatch_definition_keyword

    logical function try_dispatch_declaration_keyword(parser, arena, &
                                                      prefix_buffer, keyword, &
                                                          stmt_index) result(handled)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=*), intent(in) :: keyword
        integer, intent(out) :: stmt_index
        type(token_t) :: next_token

        handled = .true.
        select case (keyword)
        case ("type")
            stmt_index = parse_type_or_declaration(parser, arena, prefix_buffer)
        case ("real", "integer", "logical", "character", "complex", "double", &
              "class", "procedure")
            next_token = parser%get_token_at_index(parser%current_token + 1)
            if (next_token%kind == TK_OPERATOR .and. &
                (next_token%text == "=" .or. next_token%text == "=>")) then
                stmt_index = parse_assignment_or_expression(parser, arena)
            else
                stmt_index = parse_type_or_declaration(parser, arena, prefix_buffer)
            end if
        case ("implicit")
            call handle_implicit_keyword(parser, arena, stmt_index)
        case default
            handled = .false.
        end select
    end function try_dispatch_declaration_keyword

    logical function try_dispatch_execution_keyword(parser, arena, first_token, &
                                                    keyword, stmt_index) result(handled)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        character(len=*), intent(in) :: keyword
        integer, intent(out) :: stmt_index

        handled = .true.
        select case (keyword)
        case ("call")
            call handle_call_keyword(parser, arena, first_token, stmt_index)
        case ("stop")
            call handle_stop_keyword(parser, arena, first_token, stmt_index)
        case ("pause")
            stmt_index = parse_pause_statement(parser, arena)
        case ("return")
            call handle_return_keyword(parser, arena, first_token, stmt_index)
        case ("entry")
            call handle_entry_keyword(parser, arena, first_token, stmt_index)
        case ("go", "goto")
            call handle_goto_keyword(parser, arena, first_token, stmt_index)
        case ("error")
            stmt_index = parse_error_stop_statement(parser, arena)
        case ("cycle")
            call handle_cycle_keyword(parser, arena, first_token, stmt_index)
        case ("exit")
            call handle_exit_keyword(parser, arena, first_token, stmt_index)
        case ("nullify")
            stmt_index = parse_nullify_statement(parser, arena)
        case ("continue")
            stmt_index = parse_continue_statement(parser, arena)
        case ("end")
            stmt_index = parse_end_statement(parser, arena)
        case default
            handled = .false.
        end select
    end function try_dispatch_execution_keyword

    logical function try_dispatch_data_keyword(parser, arena, first_token, &
                                               keyword, stmt_index) result(handled)
        use parser_parameter_statements_module, only: parse_parameter_statement
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        character(len=*), intent(in) :: keyword
        integer, intent(out) :: stmt_index
        logical :: param_success

        handled = .true.
        select case (keyword)
        case ("data")
            call handle_data_keyword(parser, arena, stmt_index)
        case ("namelist")
            stmt_index = parse_namelist_statement(parser, arena)
        case ("parameter")
            param_success = parse_parameter_statement(parser, arena)
            if (param_success) then
                stmt_index = 0
            else
                stmt_index = parse_assignment_or_expression(parser, arena)
            end if
        case ("equivalence")
            call handle_legacy_keyword(parser, arena, first_token, keyword, &
                                       stmt_index)
        case ("common")
            call handle_common_keyword(parser, arena, first_token, stmt_index)
        case ("dimension")
            call handle_dimension_keyword(parser, arena, first_token, stmt_index)
        case ("value")
            stmt_index = parse_value_statement(parser, arena)
        case ("enum")
            stmt_index = parse_enum_construct(parser, arena)
        case ("enumerator")
            stmt_index = parse_unsupported_statement(keyword, parser, arena)
        case default
            handled = .false.
        end select
    end function try_dispatch_data_keyword

    subroutine handle_abstract_keyword(parser, arena, prefix_buffer, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer, intent(out) :: stmt_index
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
            stmt_index = parse_interface_block(parser, arena, prefix_buffer, &
                                               is_abstract=.true.)
        else
            stmt_index = parse_type_or_declaration(parser, arena, prefix_buffer)
        end if
    end subroutine handle_abstract_keyword

    subroutine handle_block_keyword(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
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
    end subroutine handle_block_keyword

    subroutine handle_module_keyword(parser, arena, first_token, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        integer, intent(out) :: stmt_index

        if (keyword_should_parse_as_identifier(first_token, parser)) then
            stmt_index = parse_assignment_or_expression(parser, arena)
        else
            stmt_index = parse_module(parser, arena)
        end if
    end subroutine handle_module_keyword

    subroutine handle_program_keyword(parser, arena, first_token, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        integer, intent(out) :: stmt_index

        if (keyword_should_parse_as_identifier(first_token, parser)) then
            stmt_index = parse_assignment_or_expression(parser, arena)
        else
            stmt_index = parse_program_statement(parser, arena)
        end if
    end subroutine handle_program_keyword

    subroutine handle_implicit_keyword(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        integer, allocatable :: extra_indices(:)

        stmt_index = parse_implicit_statement(parser, arena)
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
    end subroutine handle_implicit_keyword

    subroutine handle_call_keyword(parser, arena, first_token, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        integer, intent(out) :: stmt_index

        if (keyword_should_parse_as_identifier(first_token, parser)) then
            stmt_index = parse_assignment_or_expression(parser, arena)
        else
            stmt_index = parse_call_statement(parser, arena)
        end if
    end subroutine handle_call_keyword

    subroutine handle_stop_keyword(parser, arena, first_token, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        integer, intent(out) :: stmt_index

        if (keyword_should_parse_as_identifier(first_token, parser)) then
            stmt_index = parse_assignment_or_expression(parser, arena)
        else
            stmt_index = parse_stop_statement(parser, arena)
        end if
    end subroutine handle_stop_keyword

    subroutine handle_return_keyword(parser, arena, first_token, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        integer, intent(out) :: stmt_index

        if (keyword_should_parse_as_identifier(first_token, parser)) then
            stmt_index = parse_assignment_or_expression(parser, arena)
        else
            stmt_index = parse_return_statement(parser, arena)
        end if
    end subroutine handle_return_keyword

    subroutine handle_entry_keyword(parser, arena, first_token, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        integer, intent(out) :: stmt_index

        if (keyword_should_parse_as_identifier(first_token, parser)) then
            stmt_index = parse_assignment_or_expression(parser, arena)
        else
            stmt_index = parse_entry_statement(parser, arena)
        end if
    end subroutine handle_entry_keyword

    subroutine handle_goto_keyword(parser, arena, first_token, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        integer, intent(out) :: stmt_index

        if (keyword_should_parse_as_identifier(first_token, parser)) then
            stmt_index = parse_assignment_or_expression(parser, arena)
        else
            stmt_index = parse_goto_statement(parser, arena)
        end if
    end subroutine handle_goto_keyword

    subroutine handle_cycle_keyword(parser, arena, first_token, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        integer, intent(out) :: stmt_index

        if (keyword_should_parse_as_identifier(first_token, parser)) then
            stmt_index = parse_assignment_or_expression(parser, arena)
        else
            stmt_index = parse_cycle_statement(parser, arena)
        end if
    end subroutine handle_cycle_keyword

    subroutine handle_exit_keyword(parser, arena, first_token, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        integer, intent(out) :: stmt_index

        if (keyword_should_parse_as_identifier(first_token, parser)) then
            stmt_index = parse_assignment_or_expression(parser, arena)
        else
            stmt_index = parse_exit_statement(parser, arena)
        end if
    end subroutine handle_exit_keyword

    subroutine handle_data_keyword(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        integer, allocatable :: extra_indices(:)

        if (looks_like_data_statement(parser%tokens, parser%current_token)) then
            stmt_index = parse_data_statement(parser, arena)
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
        else
            stmt_index = parse_assignment_or_expression(parser, arena)
        end if
    end subroutine handle_data_keyword

    subroutine handle_legacy_keyword(parser, arena, first_token, keyword, &
                                     stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        character(len=*), intent(in) :: keyword
        integer, intent(out) :: stmt_index

        if (keyword_should_parse_as_identifier(first_token, parser)) then
            stmt_index = parse_assignment_or_expression(parser, arena)
        else
            stmt_index = parse_legacy_statement(keyword, parser, arena)
        end if
    end subroutine handle_legacy_keyword

    subroutine handle_common_keyword(parser, arena, first_token, stmt_index)
        use parser_common_statement_module, only: parse_common_statement
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        integer, intent(out) :: stmt_index

        if (keyword_should_parse_as_identifier(first_token, parser)) then
            stmt_index = parse_assignment_or_expression(parser, arena)
        else
            stmt_index = parse_common_statement(parser, arena)
        end if
    end subroutine handle_common_keyword

    subroutine handle_dimension_keyword(parser, arena, first_token, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        integer, intent(out) :: stmt_index

        if (keyword_should_parse_as_identifier(first_token, parser)) then
            stmt_index = parse_assignment_or_expression(parser, arena)
        else
            stmt_index = parse_dimension_statement(parser, arena)
        end if
    end subroutine handle_dimension_keyword

    subroutine dispatch_number_token(parser, arena, prefix_buffer, tokens, &
                                     stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        type(token_t), intent(in) :: tokens(:)
        integer, intent(out) :: stmt_index
        integer :: lookahead_idx
        logical :: matched_block_data
        type(token_t) :: next_token

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
                            if (to_lower(trim(next_token%text)) == "data") then
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
            if (.not. try_handle_prefix_sequence(parser, arena, prefix_buffer, &
                                                 stmt_index)) then
                stmt_index = parse_as_expression(tokens, arena, parser)
            end if
        end if
    end subroutine dispatch_number_token

    subroutine dispatch_identifier_token(parser, arena, prefix_buffer, &
                                         first_token, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        type(token_t), intent(in) :: first_token
        integer, intent(out) :: stmt_index
        character(len=:), allocatable :: lowered_keyword

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

        if (stmt_index == 0 .and. .not. parser%has_errors()) then
            call parser%error_at_token("Unclassifiable statement", first_token)
        end if
    end subroutine dispatch_identifier_token

    subroutine dispatch_comment_token(parser, arena, first_token, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: first_token
        integer, intent(out) :: stmt_index

        if (allocated(first_token%text)) then
            if (is_directive_comment(first_token%text)) then
                stmt_index = parse_directive(parser, arena)
            else
                stmt_index = parse_comment(parser, arena)
            end if
        else
            stmt_index = parse_comment(parser, arena)
        end if
    end subroutine dispatch_comment_token

    ! Parse type declaration or derived type definition
    function parse_type_or_declaration(parser, arena, prefix_buffer) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer :: stmt_index
        type(token_t) :: first_token
        logical :: is_derived_type_def

        first_token = parser%peek()
        if (to_lower(trim(first_token%text)) == "type") then
            is_derived_type_def = parser_is_at_type_definition(parser)
            if (is_derived_type_def) then
                stmt_index = parse_derived_type_def(parser, arena)
            else if (looks_like_function_definition(parser)) then
                ! type(xxx) function foo() - parse as function with type prefix
                stmt_index = parse_function_or_expression(parser, arena, &
                                                          prefix_buffer)
            else
                stmt_index = parse_declaration(parser, arena)
            end if
        else
            if (has_double_colon(parser)) then
                call handle_declaration_with_colon(parser, arena, stmt_index)
            else
                if (looks_like_function_definition(parser)) then
                    stmt_index = parse_function_or_expression(parser, arena, &
                                                              prefix_buffer)
                else
                    call handle_declaration_with_colon(parser, arena, stmt_index)
                end if
            end if
        end if
    end function parse_type_or_declaration

    subroutine handle_declaration_with_colon(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        logical :: has_initializer, has_comma
        integer, allocatable :: decl_indices(:)

        call analyze_declaration_structure(parser, has_initializer, has_comma)
        if (has_initializer .and. .not. has_comma) then
            stmt_index = parse_declaration(parser, arena)
        else if (has_comma) then
            decl_indices = parse_multi_declaration(parser, arena)
            if (allocated(decl_indices)) then
                if (size(decl_indices) > 0) then
                    stmt_index = decl_indices(1)
                    if (size(decl_indices) > 1) then
                        additional_indices = decl_indices(2:)
                    end if
                else
                    stmt_index = parse_declaration(parser, arena)
                end if
            else
                stmt_index = parse_declaration(parser, arena)
            end if
        else
            stmt_index = parse_declaration(parser, arena)
        end if
    end subroutine handle_declaration_with_colon

    ! Parse assignment or expression
    function parse_assignment_or_expression(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        integer, allocatable :: extra_indices(:)

        call parse_assignment_statement(parser, arena, stmt_index, extra_indices)
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
        type(token_t) :: keyword_token

        if (function_follows_type_spec(parser%tokens, parser%current_token)) then
            stmt_index = parse_function_definition(parser, arena, prefix_buffer)
            return
        end if
        stmt_index = parse_as_expression(parser%tokens, arena, parser)
    end function parse_function_or_expression

    ! Parse as expression
    function parse_as_expression(tokens, arena, parent_parser) result(stmt_index)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        type(parser_state_t), intent(in) :: parent_parser
        integer :: stmt_index

        stmt_index = parse_expression(tokens, arena, parent_parser)
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
                exit
            else if (parser%tokens(i)%kind == TK_KEYWORD .and. paren_depth == 0) then
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
                    cycle
                else
                    exit
                end if
            end if
        end do
    end function has_double_colon

    ! Check if this looks like a function definition, e.g. real function foo()
    logical function looks_like_function_definition(parser)
        type(parser_state_t), intent(inout) :: parser
        integer :: i

        looks_like_function_definition = .false.
        do i = parser%current_token + 1, min(parser%current_token + 10, &
                                             size(parser%tokens))
            if (parser%tokens(i)%kind == TK_KEYWORD .and. parser%tokens(i)%text == &
                "function") then
                looks_like_function_definition = .true.
                exit
            else if (parser%tokens(i)%kind == TK_OPERATOR .and. parser%tokens(i)%text &
                     == "::") then
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

        count = 0
        do while (parser%current_token <= size(parser%tokens))
            token = parser%peek()
            if (token%kind /= TK_NEWLINE) exit
            count = count + 1
            token = parser%consume()
        end do

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
                case ("=", "(")
                    ! Assignment or function call - not a DATA statement
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
