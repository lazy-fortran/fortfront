module parser_module_structures_module
    ! Module structure parsing for module definitions and bodies
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
                          TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
                          TK_WHITESPACE, to_lower
    use parser_state_module
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_module_structured, push_implicit_statement, &
                           push_assignment, push_identifier, push_literal, &
                           push_visibility_statement, push_namelist_statement, &
                           push_error_node
    use parser_namelist_shared_module, only: consume_namelist_group, append_name
    use parser_declarations, only: parse_declaration, parse_derived_type_def, &
                                   parser_is_at_type_definition
    use parser_procedure_definitions_module, only: parse_function_definition, &
                                                   parse_subroutine_definition, &
                                                   parse_interface_block
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t, append_prefix_token
    use ast_types, only: LITERAL_STRING
    use parser_type_specifications_module, only: parse_implicit_statement
    ! Temporarily removed to avoid circular dependency
    ! Will be added back after refactoring is complete
    implicit none
    private

    public :: parse_module

contains

    function parse_module(parser, arena) result(module_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: module_index
        type(parser_prefix_buffer_t) :: prefix_buffer
        type(token_t) :: token, lookahead
        character(len=:), allocatable :: module_name
        integer :: line, column
        integer, allocatable :: declaration_indices(:), procedure_indices(:)
        logical :: has_contains, in_contains_section
        integer :: stmt_index
        character(len=:), allocatable :: lookahead_lower

        ! Consume 'module' keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Get module name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            module_name = token%text
        else
            module_name = "unnamed_module"
        end if

        ! Initialize arrays
        allocate (declaration_indices(0))
        allocate (procedure_indices(0))
        has_contains = .false.
        in_contains_section = .false.

        ! Minimal parsing to detect structure and consume tokens
        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for end of module
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                ! Look ahead for "module"
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == &
                        TK_KEYWORD .and. &
                        parser%tokens(parser%current_token + 1)%text == "module") then
                        ! Consume "end module"
                        token = parser%consume()  ! consume "end"
                        token = parser%consume()  ! consume "module"
                        exit
                    end if
                end if
            end if

            ! Check for contains keyword
            if (token%kind == TK_KEYWORD .and. token%text == "contains") then
                has_contains = .true.
                in_contains_section = .true.
                token = parser%consume()  ! consume "contains"
                cycle  ! Continue to next iteration
            end if

            ! Parse declarations in module body (before contains)
            if (.not. in_contains_section) then
                if (token%kind == TK_KEYWORD) then
                    select case (token%text)
                    case ("public", "private")
                        stmt_index = parse_visibility_statement(parser, arena)
                        if (stmt_index > 0) then
                            declaration_indices = [declaration_indices, stmt_index]
                        end if
                        cycle
                    case ("namelist")
                        stmt_index = parse_namelist_statement(parser, arena)
                        if (stmt_index > 0) then
                            declaration_indices = [declaration_indices, stmt_index]
                        end if
                        cycle
                    case ("integer", "real", "logical", "character", "complex", &
                          "procedure")
                        stmt_index = parse_declaration(parser, arena)
                        if (stmt_index > 0) then
                            declaration_indices = [declaration_indices, stmt_index]
                        end if
                        cycle  ! Continue to next iteration
                    case ("type")
                        if (parser_is_at_type_definition(parser)) then
                            stmt_index = parse_derived_type_def(parser, arena)
                        else
                            stmt_index = parse_declaration(parser, arena)
                        end if
                        if (stmt_index > 0) then
                            declaration_indices = [declaration_indices, stmt_index]
                        end if
                        cycle
                    case ("class")
                        stmt_index = parse_declaration(parser, arena)
                        if (stmt_index > 0) then
                            declaration_indices = [declaration_indices, stmt_index]
                        end if
                        cycle
                    case ("implicit")
                        ! Parse implicit statement
                        call parse_simple_implicit_in_module(parser, arena, stmt_index)
                        if (stmt_index > 0) then
                            declaration_indices = [declaration_indices, stmt_index]
                        end if
                        cycle  ! Continue to next iteration
                    case ("interface")
                        stmt_index = parse_interface_block(parser, arena, &
                                                           prefix_buffer)
                        if (stmt_index > 0) then
                            declaration_indices = [declaration_indices, stmt_index]
                        end if
                        cycle
                    case ("enum", "enumerator")
                        stmt_index = handle_enum_construct(parser, arena, &
                                                           to_lower(token%text))
                        if (stmt_index > 0) then
                            declaration_indices = [declaration_indices, stmt_index]
                        end if
                        cycle
                    end select
                else if (token%kind == TK_IDENTIFIER) then
                    ! Handle assignments and other statements in module body
                    ! For now, parse as a simple assignment statement
                    block
                        type(token_t) :: id_token, eq_token
                        integer :: target_index, assign_index
                        character(len=:), allocatable :: assignment_op

                        id_token = parser%consume()  ! Get the identifier

                        ! Check if it's an assignment
                        eq_token = parser%peek()
                        if (eq_token%kind == TK_OPERATOR .and. &
                            (eq_token%text == "=" .or. eq_token%text == "=>")) then
                            eq_token = parser%consume()  ! Consume '=' or '=>'
                            assignment_op = eq_token%text

                            ! Create target identifier
                            target_index = push_identifier(arena, id_token%text, &
                                                           id_token%line, &
                                                           id_token%column)

                            ! For simple module assignments, just consume the rest of the line
                            ! We'll create a simple identifier node for the RHS
                            block
                                type(token_t) :: rhs_token
                                integer :: rhs_index

                                rhs_token = parser%peek()
                                if (rhs_token%kind == TK_NUMBER .or. &
                                    rhs_token%kind == &
                                    TK_IDENTIFIER) then
                                    rhs_token = parser%consume()

                                    ! Create identifier or literal node for RHS
                                    if (rhs_token%kind == TK_IDENTIFIER) then
                                        rhs_index = push_identifier(arena, &
                                                                    rhs_token%text, &
                                                                    rhs_token%line, &
                                                                    rhs_token%column)
                                    else
                                        ! For numbers, create as literal
                                        rhs_index = push_literal(arena, &
                                                                 rhs_token%text, &
                                                                 rhs_token%line, &
                                                                 rhs_token%column, &
                                                                 LITERAL_STRING)
                                    end if

                                    if (rhs_index > 0 .and. target_index > 0) then
                                        ! Create assignment node
                                        if (.not. allocated(assignment_op)) &
                                            assignment_op = "="
                                        assign_index = push_assignment( &
                                                       arena, target_index, &
                                                       rhs_index, &
                                                       id_token%line, &
                                                       id_token%column, &
                                                       operator_text=assignment_op)
                                        if (assign_index > 0) then
                                            declaration_indices = &
                                                [declaration_indices, &
                                                 assign_index]
                                        end if
                                    end if
                                end if
                            end block
                        else
                            ! Not an assignment, just consume the identifier
                            ! This handles other statement types that we don't parse fully yet
                        end if
                    end block
                    cycle  ! Continue to next iteration
                end if
            end if

            if (in_contains_section .and. token%kind == TK_KEYWORD .and. &
                token%text == "interface") then
                stmt_index = parse_interface_block(parser, arena, prefix_buffer)
                if (stmt_index > 0) then
                    procedure_indices = [procedure_indices, stmt_index]
                end if
                cycle
            end if

            ! Parse subroutine definitions for contains section
            if (in_contains_section .and. token%kind == TK_KEYWORD .and. &
                token%text == &
                "subroutine") then
                ! Parse the subroutine and add to procedure list
                block
                    integer :: proc_index
                    proc_index = parse_subroutine_definition(parser, arena, &
                                                             prefix_buffer)
                    if (proc_index > 0) then
                        procedure_indices = [procedure_indices, proc_index]
                    end if
                end block
                cycle
            end if

            ! Parse function definitions for contains section
            if (in_contains_section .and. token%kind == TK_KEYWORD .and. &
                token%text == "function") then
                ! Parse the function and add to procedure list
                block
                    integer :: proc_index
                    proc_index = parse_function_definition(parser, arena, &
                                                           prefix_buffer)
                    if (proc_index > 0) then
                        procedure_indices = [procedure_indices, proc_index]
                    end if
                end block
                cycle
            end if

            ! Handle comments specially - skip them without disrupting module parsing
            if (token%kind == TK_COMMENT .or. token%kind == TK_NEWLINE) then
                token = parser%consume()  ! Skip comment/newline
                cycle  ! Continue to next iteration
            end if

            if (in_contains_section) then
                if (token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER) then
                    block
                        character(len=:), allocatable :: lowered
                        character(len=16), allocatable :: stored(:)
                        lowered = to_lower(token%text)
                        select case (trim(lowered))
                        case ("pure", "elemental", "impure", "recursive", &
                              "nonrecursive", "non_recursive", "module")
                            call prefix_buffer%get_all(stored)
                            call append_prefix_token(stored, trim(lowered))
                            call prefix_buffer%set(stored)
                            if (allocated(stored)) deallocate (stored)
                            token = parser%consume()
                            cycle
                        case ("integer", "real", "logical", "character", "complex", &
                              "double", "procedure")
                            ! Type keyword - might be function return type
                            call prefix_buffer%get_all(stored)
                            if (trim(lowered) == "double") then
                                lookahead = parser%get_token_at_index(parser%current_token + 1)
                                lookahead_lower = to_lower(trim(lookahead%text))
                                select case (trim(lookahead_lower))
                                case ("precision", "complex")
                                    call append_prefix_token(stored, trim(token%text)//" "//trim(lookahead%text))
                                    call prefix_buffer%set(stored)
                                    if (allocated(stored)) deallocate (stored)
                                    token = parser%consume()
                                    token = parser%consume()
                                    cycle
                                end select
                            end if
                            call append_prefix_token(stored, trim(token%text))
                            call prefix_buffer%set(stored)
                            if (allocated(stored)) deallocate (stored)
                            token = parser%consume()
                            cycle
                        end select
                    end block
                end if
                ! Don't consume function/subroutine - let them be handled by the checks above
                if (.not. (token%kind == TK_KEYWORD .and. &
                           (token%text == "function" .or. token%text == &
                            "subroutine"))) then
                    token = parser%consume()
                end if
            else
                ! Default: consume unhandled token in module body
                token = parser%consume()
            end if
        end do

        ! Create module node with proper structure
        module_index = push_module_structured(arena, module_name, &
                                              declaration_indices, &
                                              procedure_indices, has_contains, &
                                              line, column)
    end function parse_module

    integer function handle_enum_construct(parser, arena, keyword) &
        result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: keyword
        type(token_t) :: token
        character(len=:), allocatable :: normalized_keyword
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: lowered_text
        integer :: line, column

        stmt_index = 0
        normalized_keyword = trim(keyword)

        token = parser%consume()
        line = token%line
        column = token%column

        error_msg = "Unsupported Fortran feature: " // normalized_keyword // &
                    " constructs are not supported"

        if (normalized_keyword == "enum") then
            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_KEYWORD) then
                    lowered_text = to_lower(trim(token%text))
                    if (lowered_text == "end" .or. lowered_text == "endenum") then
                        token = parser%consume()
                        if (lowered_text == "end") then
                            if (.not. parser%is_at_end()) then
                                token = parser%peek()
                                if (token%kind == TK_KEYWORD) then
                                    lowered_text = to_lower(trim(token%text))
                                    if (lowered_text == "enum") then
                                        token = parser%consume()
                                    end if
                                end if
                            end if
                        end if
                        exit
                    end if
                end if
                token = parser%consume()
            end do
        else
            do while (.not. parser%is_at_end())
                token = parser%peek()
                if (token%kind == TK_NEWLINE .or. token%kind == TK_COMMENT) then
                    exit
                end if
                token = parser%consume()
            end do
        end if

        stmt_index = push_error_node(arena, error_msg, normalized_keyword, &
                                     line, column)
    end function handle_enum_construct

    function parse_visibility_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: keyword_token, token
        logical :: is_private
        logical :: has_double_colon
        character(len=:), allocatable :: names(:)

        stmt_index = 0
        if (parser%is_at_end()) return

        keyword_token = parser%consume()
        is_private = to_lower(keyword_token%text) == "private"
        has_double_colon = .false.

        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "::") then
                token = parser%consume()
                has_double_colon = .true.
            end if
        end if

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_IDENTIFIER)
                call append_name(names, token%text)
                token = parser%consume()
            case (TK_OPERATOR)
                if (token%text == ",") then
                    token = parser%consume()
                    cycle
                else if (token%text == "::" .and. .not. has_double_colon) then
                    token = parser%consume()
                    has_double_colon = .true.
                    cycle
                else
                    exit
                end if
            case (TK_NEWLINE)
                token = parser%consume()
                exit
            case (TK_COMMENT)
                exit
            case default
                exit
            end select
        end do

        if (allocated(names)) then
            stmt_index = push_visibility_statement(arena, is_private, names, &
                                                   keyword_token%line, &
                                                   keyword_token%column, &
                                                   has_double_colon=has_double_colon)
        else
            stmt_index = push_visibility_statement(arena, is_private, &
                                                   line=keyword_token%line, &
                                                   column=keyword_token%column, &
                                                   has_double_colon=has_double_colon)
        end if
    end function parse_visibility_statement

    function parse_namelist_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: keyword_token
        character(len=:), allocatable :: group_name
        character(len=:), allocatable :: names(:)
        integer :: line, column
        logical :: has_group

        stmt_index = 0
        if (parser%is_at_end()) return

        keyword_token = parser%consume()
        line = keyword_token%line
        column = keyword_token%column

        has_group = consume_namelist_group(parser, group_name, names)
        if (.not. has_group) return

        if (allocated(names)) then
            stmt_index = push_namelist_statement(arena, group_name, names, &
                                                 line, column)
        else
            stmt_index = push_namelist_statement(arena, group_name, line=line, &
                                                 column=column)
        end if
    end function parse_namelist_statement

    ! Parse a simple implicit statement in module context
    subroutine parse_simple_implicit_in_module(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index

        stmt_index = parse_implicit_statement(parser, arena)
    end subroutine parse_simple_implicit_in_module

    ! Temporary helper to skip procedure bodies during refactoring
    subroutine skip_procedure_body(parser, proc_type)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: proc_type
        type(token_t) :: token
        integer :: nesting_level

        nesting_level = 1  ! We're already inside a procedure

        ! Consume the procedure keyword
        token = parser%consume()

        ! Skip the procedure name if present
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
        end if

        ! Skip until matching "end <proc_type>"
        do while (.not. parser%is_at_end() .and. nesting_level > 0)
            token = parser%peek()

            if (token%kind == TK_KEYWORD) then
                if (token%text == proc_type) then
                    nesting_level = nesting_level + 1
                else if (token%text == "end") then
                    ! Check if next token is our procedure type
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                        if (parser%tokens(parser%current_token + 1)%kind == &
                            TK_KEYWORD .and. &
                            parser%tokens(parser%current_token + 1)%text == &
                            proc_type) then
                            nesting_level = nesting_level - 1
                        end if
                    end if
                end if
            end if

            token = parser%consume()
        end do
    end subroutine skip_procedure_body

end module parser_module_structures_module
