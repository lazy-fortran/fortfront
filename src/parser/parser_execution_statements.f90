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
                                                   parse_subroutine_definition
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t, append_prefix_token
    use parser_assignment_module, only: parse_assignment_statement
    use parser_utils, only: analyze_declaration_structure
    use parser_io_statements_module, only: parse_print_statement, parse_write_statement
    use parser_memory_statements_module, only: parse_allocate_statement, &
                                               parse_deallocate_statement
    use parser_control_statements_module, only: parse_stop_statement, &
                                                parse_goto_statement, &
                                                parse_error_stop_statement, &
                                                parse_return_statement, &
                                                parse_cycle_statement, &
                                                parse_exit_statement
    use parser_control_flow_router_module, only: route_control_flow, &
                                                 is_control_flow_keyword
    use parser_call_module, only: parse_call_statement
    use parser_import_statements_module, only: parse_use_statement
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_program, &
                           push_declaration, push_implicit_statement
    use ast_types, only: LITERAL_STRING, LITERAL_INTEGER, LITERAL_REAL, LITERAL_LOGICAL
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
        integer :: stmt_index

        call prefix_buffer%clear()

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_KEYWORD) then
                lowered = trim(to_lower(token%text))
            else
                lowered = ""
            end if

            ! Check for 'end program'
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == &
                        TK_KEYWORD .and. &
                        parser%tokens(parser%current_token + 1)%text == "program") then
                        ! Found 'end program', consume both tokens
                        token = parser%consume()  ! end
                        token = parser%consume()  ! program

                        ! Optional program name after 'end program'
                        token = parser%peek()
                        if (token%kind == TK_IDENTIFIER) then
                            token = parser%consume()
                        end if
                        exit
                    end if
                end if
            end if

            ! Parse statements directly without complex boundary detection
            stmt_index = 0

            if (token%kind == TK_KEYWORD .and. is_control_flow_keyword(lowered)) then
                if (allocated(pending_prefixes)) then
                    deallocate (pending_prefixes)
                    call prefix_buffer%clear()
                end if
                stmt_index = route_control_flow(parser, arena)
            else
                select case (token%kind)
                case (TK_KEYWORD)
                    select case (lowered)
                    case ("elemental", "pure", "impure", "recursive", "nonrecursive", &
                          "non_recursive", "module")
                        call append_prefix_token(pending_prefixes, lowered)
                        token = parser%consume()
                        stmt_index = 0
                    case ("contains")
                        token = parser%consume()
                        stmt_index = 0
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                        end if
                        call prefix_buffer%clear()
                    case ("function")
                        if (allocated(pending_prefixes)) then
                            call prefix_buffer%set(pending_prefixes)
                            deallocate (pending_prefixes)
                        else
                            call prefix_buffer%clear()
                        end if
                        stmt_index = parse_function_definition(parser, arena, &
                                                               prefix_buffer)
                        call prefix_buffer%clear()
                    case ("subroutine")
                        if (allocated(pending_prefixes)) then
                            call prefix_buffer%set(pending_prefixes)
                            deallocate (pending_prefixes)
                        else
                            call prefix_buffer%clear()
                        end if
                        stmt_index = parse_subroutine_definition(parser, arena, &
                                                                 prefix_buffer)
                        call prefix_buffer%clear()
                    case ("implicit")
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                            call prefix_buffer%clear()
                        end if
                        call parse_simple_implicit(parser, arena, stmt_index)
                    case ("real", "integer", "logical", "character", "complex", &
                          "double", "class")
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                            call prefix_buffer%clear()
                        end if
                        call handle_variable_declaration(parser, arena, stmt_index)
                    case ("type")
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                            call prefix_buffer%clear()
                        end if
                        call handle_type_declaration(parser, arena, stmt_index)
                    case ("print")
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                            call prefix_buffer%clear()
                        end if
                        stmt_index = parse_print_statement(parser, arena)
                    case ("use")
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                            call prefix_buffer%clear()
                        end if
                        stmt_index = parse_use_statement(parser, arena)
                    case ("write")
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                            call prefix_buffer%clear()
                        end if
                        stmt_index = parse_write_statement(parser, arena)
                    case ("allocate")
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                            call prefix_buffer%clear()
                        end if
                        stmt_index = parse_allocate_statement(parser, arena)
                    case ("deallocate")
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                            call prefix_buffer%clear()
                        end if
                        stmt_index = parse_deallocate_statement(parser, arena)
                    case ("stop")
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                            call prefix_buffer%clear()
                        end if
                        stmt_index = parse_stop_statement(parser, arena)
                    case ("go", "goto")
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                            call prefix_buffer%clear()
                        end if
                        stmt_index = parse_goto_statement(parser, arena)
                    case ("error")
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                            call prefix_buffer%clear()
                        end if
                        stmt_index = parse_error_stop_statement(parser, arena)
                    case ("return")
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                            call prefix_buffer%clear()
                        end if
                        stmt_index = parse_return_statement(parser, arena)
                    case ("cycle")
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                            call prefix_buffer%clear()
                        end if
                        stmt_index = parse_cycle_statement(parser, arena)
                    case ("exit")
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                            call prefix_buffer%clear()
                        end if
                        stmt_index = parse_exit_statement(parser, arena)
                    case ("call")
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                            call prefix_buffer%clear()
                        end if
                        stmt_index = parse_call_statement(parser, arena)
                    case default
                        if (allocated(pending_prefixes)) then
                            deallocate (pending_prefixes)
                            call prefix_buffer%clear()
                        end if
                        token = parser%consume()
                        stmt_index = 0
                    end select
                case (TK_IDENTIFIER)
                    if (allocated(pending_prefixes)) then
                        deallocate (pending_prefixes)
                        call prefix_buffer%clear()
                    end if
                    if (trim(to_lower(token%text)) == 'class') then
                        call handle_variable_declaration(parser, arena, stmt_index)
                    else
                        call parse_assignment_statement(parser, arena, stmt_index, &
                                                        additional_execution_indices)
                    end if
                case (TK_NEWLINE, TK_COMMENT)
                    token = parser%consume()
                    stmt_index = 0
                case default
                    token = parser%consume()
                    stmt_index = 0
                end select
            end if

            if (stmt_index > 0) then
                body_indices = [body_indices, stmt_index]

                ! Handle additional indices from multi-declaration parsing
                if (allocated(additional_execution_indices)) then
                    if (size(additional_execution_indices) > 0) then
                        body_indices = [body_indices, additional_execution_indices]
                    end if
                    deallocate (additional_execution_indices)
                end if
            end if
        end do
    end subroutine parse_program_body

    ! Parse a simple if statement with optional else block

    ! Parse a simple assignment statement or multi-variable assignment
    ! Parse a simple variable declaration
    ! Parse a simple implicit statement
    subroutine parse_simple_implicit(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        type(token_t) :: implicit_token, none_token
        character(len=:), allocatable :: implicit_type

        stmt_index = 0

        ! Get implicit keyword
        implicit_token = parser%consume()

        ! Check for 'none'
        none_token = parser%peek()
        if (none_token%kind == TK_KEYWORD .and. none_token%text == "none") then
            none_token = parser%consume()
            implicit_type = "none"
        else
            implicit_type = "default"
        end if

        ! Create implicit statement node
        if (implicit_type == "none") then
            stmt_index = push_implicit_statement(arena, .true., &
                                                 line=implicit_token%line, &
                                                 column=implicit_token%column)
        else
            stmt_index = push_implicit_statement(arena, .false., &
                                                 line=implicit_token%line, &
                                                 column=implicit_token%column)
        end if
    end subroutine parse_simple_implicit

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
