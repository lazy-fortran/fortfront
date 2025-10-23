module parser_execution_statements_module
    ! Parser module for execution statement types (call, program)
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, &
                          TK_STRING, TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, &
                          TK_COMMENT, TK_WHITESPACE, to_lower
    use lexer_token_types, only: TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER, &
                                 TK_STRING, TK_NEWLINE, TK_KEYWORD
    use parser_state_module
    use parser_expressions_module, only: parse_range
    use parser_declarations, only: parse_declaration, parse_multi_declaration, &
                                   parse_derived_type_def, parser_is_at_type_definition
    use parser_definition_statements_module, only: parse_function_definition, &
                                                   parse_subroutine_definition, &
                                                   parse_interface_block
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t, append_prefix_token
    use parser_assignment_module, only: parse_assignment_statement
    use parser_utils, only: analyze_declaration_structure
    use parser_io_statements_module, only: parse_print_statement, &
                                           parse_write_statement, &
                                           parse_read_statement, &
                                           parse_format_statement, &
                                           parse_open_statement, &
                                           parse_close_statement, &
                                           parse_inquire_statement, &
                                           parse_backspace_statement, &
                                           parse_rewind_statement, &
                                           parse_endfile_statement
    use parser_memory_statements_module, only: parse_allocate_statement, &
                                               parse_deallocate_statement
    use parser_control_statements_module, only: parse_stop_statement, &
                                                parse_goto_statement, &
                                                parse_error_stop_statement, &
                                                parse_return_statement, &
                                                parse_entry_statement, &
                                                parse_continue_statement, &
                                                parse_cycle_statement, &
                                                parse_exit_statement, &
                                                parse_nullify_statement
    use parser_control_flow_router_module, only: route_control_flow, &
                                                 is_control_flow_keyword
    use parser_statement_data_module, only: parse_data_statement, &
                                            parse_namelist_statement
    use parser_call_module, only: parse_call_statement
    use parser_import_statements_module, only: parse_use_statement
    use parser_type_specifications_module, only: parse_implicit_statement
    use parser_dimension_statements_module, only: parse_dimension_statement
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_program, &
                           push_declaration, push_implicit_statement, push_goto
    use ast_types, only: LITERAL_STRING, LITERAL_INTEGER, LITERAL_REAL, LITERAL_LOGICAL
    use parser_legacy_statements_module, only: parse_legacy_statement
    implicit none
    private

    public :: parse_call_statement, parse_program_statement

    ! Module variable to store additional indices from multi-declaration parsing
    integer, allocatable :: additional_execution_indices(:)

contains

    function parse_program_statement(parser, arena) result(prog_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: prog_index
        type(token_t) :: token, name_token
        character(len=:), allocatable :: program_name
        integer :: line, column
        integer, allocatable :: body_indices(:)
        integer :: stmt_index

        prog_index = 0
        allocate (body_indices(0))

        ! Check if we're already at 'program' keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "program") then
            ! Consume 'program' keyword
            token = parser%consume()
        else
            ! Not at program keyword, return 0
            prog_index = 0
            return
        end if

        line = token%line
        column = token%column

        ! Get program name (optional in lazy fortran, required in standard)
        name_token = parser%peek()
        if (name_token%kind == TK_IDENTIFIER) then
            name_token = parser%consume()
            program_name = name_token%text
        else
            program_name = "main"
        end if

        ! Parse program body until 'end program'
        ! Use a simpler approach that delegates to individual statement parsing
        call parse_program_body(parser, arena, body_indices)

        ! Create program node
        prog_index = push_program(arena, program_name, body_indices, line, column)

    end function parse_program_statement

    ! Parse the body of a program until 'end program'
    ! Simplified approach that handles the basic case
    subroutine parse_program_body(parser, arena, body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: body_indices(:)
        type(token_t) :: token
        type(parser_prefix_buffer_t) :: prefix_buffer
        character(len=16), allocatable :: pending_prefixes(:)
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: stmt_label
        integer :: stmt_index

        call prefix_buffer%clear()

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_KEYWORD) then
                lowered = trim(to_lower(token%text))
            else
                lowered = ""
            end if

            if (end_program_encountered(token)) then
                call consume_end_program(parser)
                exit
            end if

            stmt_index = 0
            ! Clear label for this statement
            if (allocated(stmt_label)) deallocate (stmt_label)

            ! Check for numeric statement label like 10  i = i + 1
            if (token%kind == TK_NUMBER) then
                ! Save the label text
                stmt_label = trim(token%text)
                ! Consume the label and get next token
                token = parser%consume()
                token = parser%peek()
                if (token%kind == TK_KEYWORD) then
                    lowered = trim(to_lower(token%text))
                else
                    lowered = ""
                end if
            end if

            ! After potentially consuming label, check for end program again
            if (token%kind == TK_KEYWORD .and. end_program_encountered(token)) then
                call consume_end_program(parser)
                exit
            end if

            if (token%kind == TK_KEYWORD .and. is_control_flow_keyword(lowered)) then
                call flush_pending_prefixes()
                stmt_index = route_control_flow(parser, arena)
            else
                select case (token%kind)
                case (TK_KEYWORD)
                    stmt_index = handle_keyword_token(lowered, parser, arena)
                case (TK_IDENTIFIER)
                    stmt_index = handle_identifier_token(parser, arena, token)
                case (TK_NEWLINE, TK_COMMENT)
                    call consume_trivia(parser)
                case default
                    call consume_misc(parser)
                end select
            end if

            ! Set label on the created statement node if we have one
            if (stmt_index > 0 .and. allocated(stmt_label)) then
                if (stmt_index <= arena%size) then
                    if (allocated(arena%entries(stmt_index)%node)) then
                        arena%entries(stmt_index)%node%stmt_label = stmt_label
                    end if
                end if
            end if

            call append_statement(stmt_index, body_indices)
        end do

    contains
        subroutine reset_pending_prefixes()
            if (allocated(pending_prefixes)) then
                block
                    character(len=16), allocatable :: temp(:)
                    call move_alloc(pending_prefixes, temp)
                end block
            end if
        end subroutine reset_pending_prefixes

        subroutine flush_pending_prefixes()
            call reset_pending_prefixes()
            call prefix_buffer%clear()
        end subroutine flush_pending_prefixes

        logical function end_program_encountered(current_token) result(is_end)
            type(token_t), intent(in) :: current_token
            is_end = .false.
            if (current_token%kind /= TK_KEYWORD) return

            ! Handle "endprogram" (single keyword)
            if (current_token%text == "endprogram") then
                is_end = .true.
                return
            end if

            ! Handle "end program" (two keywords)
            if (current_token%text /= "end") return
            if (parser%current_token + 1 > size(parser%tokens)) return
            if (parser%tokens(parser%current_token + 1)%kind /= TK_KEYWORD) return
            if (parser%tokens(parser%current_token + 1)%text /= "program") return
            is_end = .true.
        end function end_program_encountered

        subroutine consume_end_program(parser_ref)
            type(parser_state_t), intent(inout) :: parser_ref
            type(token_t) :: local_token

            ! Consume "endprogram" or "end" "program"
            local_token = parser_ref%consume()
            if (local_token%text /= "endprogram") then
                ! Must be "end", consume "program" next
                local_token = parser_ref%consume()
            end if

            ! Check for optional program name
            local_token = parser_ref%peek()
            if (local_token%kind == TK_IDENTIFIER) then
                local_token = parser_ref%consume()
            end if
        end subroutine consume_end_program

        integer function handle_keyword_token(lowered, parser_ref, arena_ref) &
            result(stmt_index)
            character(len=*), intent(in) :: lowered
            type(parser_state_t), intent(inout) :: parser_ref
            type(ast_arena_t), intent(inout) :: arena_ref

            select case (lowered)
            case ("elemental", "pure", "impure", "recursive", "nonrecursive", &
                  "non_recursive", "module")
                call append_prefix_token(pending_prefixes, lowered)
                block
                    type(token_t) :: ignored_token
                    ignored_token = parser_ref%consume()
                end block
                stmt_index = 0
            case ("contains")
                block
                    type(token_t) :: ignored_token
                    ignored_token = parser_ref%consume()
                end block
                call flush_pending_prefixes()
                stmt_index = 0
            case ("function")
                stmt_index = parse_function_with_prefixes(parser_ref, arena_ref)
            case ("subroutine")
                stmt_index = parse_subroutine_with_prefixes(parser_ref, arena_ref)
            case default
                stmt_index = parse_general_keyword(lowered, parser_ref, arena_ref)
            end select
        end function handle_keyword_token

        integer function parse_function_with_prefixes(parser_ref, arena_ref) &
            result(stmt_index)
            type(parser_state_t), intent(inout) :: parser_ref
            type(ast_arena_t), intent(inout) :: arena_ref
            if (allocated(pending_prefixes)) then
                call prefix_buffer%set(pending_prefixes)
                stmt_index = parse_function_definition(parser_ref, arena_ref, &
                                                       prefix_buffer, &
                                                       pending_prefixes)
                call reset_pending_prefixes()
            else
                call prefix_buffer%clear()
                stmt_index = parse_function_definition(parser_ref, arena_ref, &
                                                       prefix_buffer)
            end if
            call prefix_buffer%clear()
        end function parse_function_with_prefixes

        integer function parse_subroutine_with_prefixes(parser_ref, arena_ref) &
            result(stmt_index)
            type(parser_state_t), intent(inout) :: parser_ref
            type(ast_arena_t), intent(inout) :: arena_ref
            if (allocated(pending_prefixes)) then
                call prefix_buffer%set(pending_prefixes)
                call reset_pending_prefixes()
            else
                call prefix_buffer%clear()
            end if
            stmt_index = parse_subroutine_definition(parser_ref, arena_ref, &
                                                     prefix_buffer)
            call prefix_buffer%clear()
        end function parse_subroutine_with_prefixes

        integer function parse_general_keyword(lowered, parser_ref, arena_ref) &
            result(stmt_index)
            character(len=*), intent(in) :: lowered
            type(parser_state_t), intent(inout) :: parser_ref
            type(ast_arena_t), intent(inout) :: arena_ref

            call flush_pending_prefixes()
            select case (lowered)
            case ("implicit")
                stmt_index = parse_implicit_statement(parser_ref, arena_ref)
            case ("real", "integer", "logical", "character", "complex", "double", &
                  "class")
                call handle_variable_declaration(parser_ref, arena_ref, stmt_index)
            case ("type")
                call handle_type_declaration(parser_ref, arena_ref, stmt_index)
            case ("print")
                stmt_index = parse_print_statement(parser_ref, arena_ref)
            case ("data")
                stmt_index = parse_data_statement(parser_ref, arena_ref)
            case ("use")
                stmt_index = parse_use_statement(parser_ref, arena_ref)
            case ("write")
                stmt_index = parse_write_statement(parser_ref, arena_ref)
            case ("read")
                stmt_index = parse_read_statement(parser_ref, arena_ref)
            case ("open")
                stmt_index = parse_open_statement(parser_ref, arena_ref)
            case ("close")
                stmt_index = parse_close_statement(parser_ref, arena_ref)
            case ("inquire")
                stmt_index = parse_inquire_statement(parser_ref, arena_ref)
            case ("backspace")
                stmt_index = parse_backspace_statement(parser_ref, arena_ref)
            case ("rewind")
                stmt_index = parse_rewind_statement(parser_ref, arena_ref)
            case ("endfile")
                stmt_index = parse_endfile_statement(parser_ref, arena_ref)
            case ("format")
                stmt_index = parse_format_statement(parser_ref, arena_ref)
            case ("allocate")
                stmt_index = parse_allocate_statement(parser_ref, arena_ref)
            case ("deallocate")
                stmt_index = parse_deallocate_statement(parser_ref, arena_ref)
            case ("stop")
                stmt_index = parse_stop_statement(parser_ref, arena_ref)
            case ("go", "goto")
                stmt_index = parse_goto_statement(parser_ref, arena_ref)
            case ("error")
                stmt_index = parse_error_stop_statement(parser_ref, arena_ref)
            case ("return")
                stmt_index = parse_return_statement(parser_ref, arena_ref)
            case ("entry")
                stmt_index = parse_entry_statement(parser_ref, arena_ref)
            case ("continue")
                stmt_index = parse_continue_statement(parser_ref, arena_ref)
            case ("cycle")
                stmt_index = parse_cycle_statement(parser_ref, arena_ref)
            case ("exit")
                stmt_index = parse_exit_statement(parser_ref, arena_ref)
            case ("nullify")
                stmt_index = parse_nullify_statement(parser_ref, arena_ref)
            case ("call")
                stmt_index = parse_call_statement(parser_ref, arena_ref)
            case ("interface")
                stmt_index = parse_interface_block(parser_ref, arena_ref, &
                                                   prefix_buffer)
            case ("dimension")
                block
                    type(token_t) :: ignored_token
                    logical :: success
                    ignored_token = parser_ref%consume()
                    success = parse_dimension_statement(parser_ref, arena_ref)
                end block
                stmt_index = 0
            case ("namelist")
                stmt_index = parse_namelist_statement(parser_ref, arena_ref)
            case ("equivalence", "common")
                stmt_index = parse_legacy_statement(lowered, parser_ref, arena_ref)
            case default
                block
                    type(token_t) :: ignored_token
                    ignored_token = parser_ref%consume()
                end block
                stmt_index = 0
            end select
        end function parse_general_keyword

        integer function handle_identifier_token(parser_ref, arena_ref, &
                                                 current_token) &
            result(stmt_index)
            type(parser_state_t), intent(inout) :: parser_ref
            type(ast_arena_t), intent(inout) :: arena_ref
            type(token_t), intent(in) :: current_token
            character(len=:), allocatable :: lowered_identifier
            type(token_t) :: next_token

            call flush_pending_prefixes()
            lowered_identifier = trim(to_lower(current_token%text))
            if (lowered_identifier == "class") then
                call handle_variable_declaration(parser_ref, arena_ref, stmt_index)
            else if (lowered_identifier == "goto") then
                ! Handle "goto" as identifier (Fortran allows both "go to" and "goto")
                ! Parser is at the goto token; caller already
                ! peeked without consuming it
                next_token = parser_ref%consume()  ! Consume "goto"
                next_token = parser_ref%peek()
                if (next_token%kind == TK_NUMBER .or. &
                    next_token%kind == TK_IDENTIFIER) then
                    stmt_index = push_goto(arena_ref, label=trim(next_token%text), &
                                           line=current_token%line, &
                                           column=current_token%column)
                    next_token = parser_ref%consume()  ! Consume the label
                else
                    ! Invalid goto - missing label
                    stmt_index = push_goto(arena_ref, label="INVALID_LABEL", &
                                           line=current_token%line, &
                                           column=current_token%column)
                end if
            else if (lowered_identifier == "continue") then
                stmt_index = parse_continue_statement(parser_ref, arena_ref)
            else
                call parse_assignment_statement(parser_ref, arena_ref, stmt_index, &
                                                additional_execution_indices)
            end if
        end function handle_identifier_token

        subroutine consume_trivia(parser_ref)
            type(parser_state_t), intent(inout) :: parser_ref
            block
                type(token_t) :: ignored_token
                ignored_token = parser_ref%consume()
            end block
        end subroutine consume_trivia

        subroutine consume_misc(parser_ref)
            type(parser_state_t), intent(inout) :: parser_ref
            call flush_pending_prefixes()
            block
                type(token_t) :: ignored_token
                ignored_token = parser_ref%consume()
            end block
        end subroutine consume_misc

        subroutine append_statement(stmt_index, indices)
            integer, intent(in) :: stmt_index
            integer, allocatable, intent(inout) :: indices(:)
            if (stmt_index <= 0) return
            indices = [indices, stmt_index]
            if (allocated(additional_execution_indices)) then
                if (size(additional_execution_indices) > 0) then
                    indices = [indices, additional_execution_indices]
                end if
                block
                    integer, allocatable :: temp(:)
                    call move_alloc(additional_execution_indices, temp)
                end block
            end if
        end subroutine append_statement
    end subroutine parse_program_body

    ! Parse a simple if statement with optional else block

    ! Parse a simple assignment statement or multi-variable assignment
    ! Parse a simple variable declaration
    ! Parse a simple implicit statement
    ! Handle single vs multi-variable declarations (duplicate of dispatcher logic)
    subroutine handle_variable_declaration(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        logical :: has_initializer, has_comma
        integer, allocatable :: decl_indices(:)

        ! Analyze the declaration structure
        call analyze_declaration_structure(parser, has_initializer, has_comma)

        if (has_initializer .and. .not. has_comma) then
            ! Single variable with initializer - use parse_declaration
            stmt_index = parse_declaration(parser, arena)
        else if (has_comma) then
            ! Multi-variable declaration - use parse_multi_declaration
            decl_indices = parse_multi_declaration(parser, arena)
            if (allocated(decl_indices) .and. size(decl_indices) > 0) then
                stmt_index = decl_indices(1)  ! Return first declaration index

                ! Store additional indices if any
                if (size(decl_indices) > 1) then
                    allocate (additional_execution_indices(size(decl_indices) - 1))
                    additional_execution_indices = decl_indices(2:)
                end if
            else
                stmt_index = parse_declaration(parser, arena)  ! Fallback
            end if
        else
            ! Single variable without initializer - use parse_declaration
            stmt_index = parse_declaration(parser, arena)
        end if
    end subroutine handle_variable_declaration

    ! Handle type definitions and derived type variable declarations
    subroutine handle_type_declaration(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        type(token_t) :: first_token

        stmt_index = 0
        first_token = parser%peek()

        if (first_token%text == "type" .and. parser_is_at_type_definition(parser)) then
            stmt_index = parse_derived_type_def(parser, arena)
        else
            call handle_variable_declaration(parser, arena, stmt_index)
        end if
    end subroutine handle_type_declaration

end module parser_execution_statements_module
