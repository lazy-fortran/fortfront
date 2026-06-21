module parser_submodule_structures_module
    ! Submodule structure parsing for Fortran 2008 submodule definitions
    ! ISO/IEC 1539-1:2008 Section 11.2
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, &
                          TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
                          TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_submodule_structured
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use parser_type_specifications_module, only: take_implicit_additional_indices
    use parser_keyword_disambiguation_module, only: keyword_should_parse_as_identifier
    use parser_submodule_helpers_module, only: parse_submodule_declaration_statement, &
                                               parse_contains_section_item_submodule, &
                                               handle_submodule_identifier_assignment
    implicit none
    private

    public :: parse_submodule

contains

    function parse_parent_identifier(parser) result(parent_id)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable :: parent_id
        type(token_t) :: token

        parent_id = ""

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") return
        token = parser%consume()

        do while (.not. parser%is_at_end())
            token = parser%peek()

            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            end if

            if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
                if (len(parent_id) > 0) then
                    parent_id = parent_id//token%text
                else
                    parent_id = token%text
                end if
                token = parser%consume()
            else if (token%kind == TK_OPERATOR .and. token%text == ":") then
                parent_id = parent_id//":"
                token = parser%consume()
            else
                token = parser%consume()
            end if
        end do
    end function parse_parent_identifier

    function handle_contains_keyword_in_submodule(parser, arena, has_contains, &
                                                  in_contains_section, &
                                                  declaration_indices) &
        result(should_cycle)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        logical, intent(inout) :: has_contains, in_contains_section
        integer, allocatable, intent(inout) :: declaration_indices(:)
        logical :: should_cycle
        type(token_t) :: token, next_token
        logical :: is_assignment

        should_cycle = .false.
        token = parser%peek()

        is_assignment = .false.
        if (parser%current_token + 1 <= size(parser%tokens)) then
            next_token = parser%tokens(parser%current_token + 1)
            if (next_token%kind == TK_OPERATOR .and. &
                (next_token%text == "=" .or. next_token%text == "=>")) then
                is_assignment = .true.
            end if
        end if

        if (.not. is_assignment) then
            has_contains = .true.
            in_contains_section = .true.
            token = parser%consume()
            should_cycle = .true.
        end if
    end function handle_contains_keyword_in_submodule

    function check_submodule_end(parser) result(at_end)
        type(parser_state_t), intent(inout) :: parser
        logical :: at_end
        type(token_t) :: token, lookahead
        character(len=:), allocatable :: lookahead_lower

        at_end = .false.
        token = parser%peek()

        if (token%kind == TK_KEYWORD) then
            lookahead_lower = to_lower(trim(token%text))
            select case (trim(lookahead_lower))
            case ("endsubmodule")
                token = parser%consume()
                if (.not. parser%is_at_end()) then
                    lookahead = parser%peek()
                    if (lookahead%kind == TK_IDENTIFIER) then
                        token = parser%consume()
                    end if
                end if
                at_end = .true.
            case ("end")
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    lookahead = parser%tokens(parser%current_token + 1)
                    lookahead_lower = to_lower(trim(lookahead%text))
                    if (lookahead%kind == TK_KEYWORD .and. &
                        lookahead_lower == "submodule") then
                        token = parser%consume()
                        token = parser%consume()
                        if (.not. parser%is_at_end()) then
                            lookahead = parser%peek()
                            if (lookahead%kind == TK_IDENTIFIER) then
                                token = parser%consume()
                            end if
                        end if
                        at_end = .true.
                    else if (lookahead%kind == TK_NEWLINE .or. &
                             lookahead%kind == TK_COMMENT .or. &
                             lookahead%kind == TK_EOF) then
                        token = parser%consume()
                        at_end = .true.
                    end if
                else
                    token = parser%consume()
                    at_end = .true.
                end if
            end select
        end if
    end function check_submodule_end

    function handle_specification_statement(parser, arena, prefix_buffer, &
                                            declaration_indices) result(handled)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer, allocatable, intent(inout) :: declaration_indices(:)
        logical :: handled
        type(token_t) :: token
        integer :: stmt_index

        handled = .false.
        token = parser%peek()

        if (token%kind == TK_KEYWORD) then
            if (keyword_should_parse_as_identifier(token, parser)) then
                call handle_submodule_identifier_assignment(parser, arena, &
                                                            declaration_indices)
                handled = .true.
                return
            end if

            stmt_index = parse_submodule_declaration_statement(parser, arena, &
                                                               prefix_buffer)
            if (stmt_index > 0) then
                declaration_indices = [declaration_indices, stmt_index]
                handled = .true.
            else if (stmt_index == -1) then
                block
                    integer, allocatable :: extra_indices(:)
                    extra_indices = take_implicit_additional_indices()
                    if (size(extra_indices) > 0) then
                        declaration_indices = [declaration_indices, extra_indices]
                    end if
                end block
                handled = .true.
            end if
        else if (token%kind == TK_IDENTIFIER) then
            call handle_submodule_identifier_assignment(parser, arena, &
                                                        declaration_indices)
            handled = .true.
        end if
    end function handle_specification_statement

    function handle_contains_section_statement(parser, arena, prefix_buffer, &
                                               procedure_indices, lowered) &
        result(handled)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer, allocatable, intent(inout) :: procedure_indices(:)
        character(len=*), intent(in) :: lowered
        logical :: handled
        type(token_t) :: token
        integer :: stmt_index

        handled = .false.
        token = parser%peek()

        stmt_index = parse_contains_section_item_submodule(parser, arena, &
                                                           prefix_buffer)
        if (stmt_index > 0) then
            procedure_indices = [procedure_indices, stmt_index]
            handled = .true.
        else if (stmt_index == -1) then
            handled = .true.
        else
            if (.not. (token%kind == TK_KEYWORD .and. &
                       (trim(lowered) == "function" .or. &
                        trim(lowered) == "subroutine"))) then
                token = parser%consume()
                handled = .true.
            end if
        end if
    end function handle_contains_section_statement

    subroutine parse_submodule_body(parser, arena, prefix_buffer, &
                                    has_contains, in_contains_section, &
                                    declaration_indices, procedure_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        logical, intent(inout) :: has_contains, in_contains_section
        integer, allocatable, intent(inout) :: declaration_indices(:)
        integer, allocatable, intent(inout) :: procedure_indices(:)
        type(token_t) :: token
        character(len=:), allocatable :: lowered

        do while (.not. parser%is_at_end())
            if (check_submodule_end(parser)) exit

            token = parser%peek()
            select case (token%kind)
            case (TK_KEYWORD, TK_IDENTIFIER)
                lowered = to_lower(token%text)
            case default
                lowered = ""
            end select

            if ((token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER) .and. &
                trim(lowered) == "contains") then
                if (handle_contains_keyword_in_submodule(parser, arena, &
                                                         has_contains, &
                                                         in_contains_section, &
                                                         declaration_indices)) then
                    cycle
                end if
            end if

            if (token%kind == TK_COMMENT .or. token%kind == TK_NEWLINE) then
                token = parser%consume()
                cycle
            end if

            if (.not. in_contains_section) then
                if (handle_specification_statement(parser, arena, prefix_buffer, &
                                                   declaration_indices)) cycle
                token = parser%consume()
            else
                if (handle_contains_section_statement(parser, arena, prefix_buffer, &
                                                      procedure_indices, lowered)) &
                    cycle
            end if
        end do
    end subroutine parse_submodule_body

    recursive function parse_submodule(parser, arena) result(submod_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: submod_index
        type(parser_prefix_buffer_t) :: prefix_buffer
        type(token_t) :: token
        character(len=:), allocatable :: submod_name
        character(len=:), allocatable :: parent_id
        integer :: line, column
        integer, allocatable :: declaration_indices(:), procedure_indices(:)
        logical :: has_contains, in_contains_section

        token = parser%consume()
        line = token%line
        column = token%column

        parent_id = parse_parent_identifier(parser)

        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            submod_name = token%text
        else
            submod_name = "unnamed_submodule"
        end if

        allocate (declaration_indices(0))
        allocate (procedure_indices(0))
        has_contains = .false.
        in_contains_section = .false.

        call parse_submodule_body(parser, arena, prefix_buffer, &
                                  has_contains, in_contains_section, &
                                  declaration_indices, procedure_indices)

        submod_index = push_submodule_structured(arena, submod_name, &
                                                 parent_id, &
                                                 declaration_indices, &
                                                 procedure_indices, has_contains, &
                                                 line, column)
    end function parse_submodule

end module parser_submodule_structures_module
