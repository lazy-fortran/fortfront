module parser_submodule_helpers_module
    ! Helper procedures for submodule parsing
    ! Handles statement-level parsing within submodule bodies
    ! ISO/IEC 1539-1:2008 Section 11.2
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_NUMBER, &
        TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, &
        TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_visibility_statement, push_namelist_statement, &
        push_assignment, push_identifier, push_literal, &
        push_error_node, push_subroutine_def
    use parser_namelist_shared_module, only: consume_namelist_group, append_name
    use parser_declarations, only: parse_declaration, parse_derived_type_def, &
        parser_is_at_type_definition
    use parser_procedure_definitions_module, only: parse_function_definition, &
        parse_subroutine_definition, &
        parse_interface_block
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t, append_prefix_token
    use parser_procedure_shared_module, only: consume_optional_kind_spec
    use parser_procedure_definition_bodies_module, only: parse_procedure_body
    use parser_import_resolution_module, only: parse_use_statement
    use ast_types, only: LITERAL_STRING
    use parser_type_specifications_module, only: parse_implicit_statement, &
        take_implicit_additional_indices
    use parser_keyword_disambiguation_module, only: keyword_should_parse_as_identifier
    implicit none
    private

    public :: parse_submodule_declaration_statement
    public :: parse_contains_section_item_submodule
    public :: parse_visibility_statement_in_submodule
    public :: parse_namelist_statement_in_submodule
    public :: parse_simple_implicit_in_submodule
    public :: handle_submodule_identifier_assignment
    public :: parse_abstract_interface_in_submodule
    public :: parse_separate_module_procedure

    private :: handle_procedure_keyword_in_contains
    private :: handle_prefix_modifier_in_contains
    private :: handle_type_prefix_in_contains
    private :: is_separate_module_procedure_start

contains

    function parse_submodule_declaration_statement(parser, arena, &
            prefix_buffer) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer :: stmt_index
        type(token_t) :: token
        integer, allocatable :: extra_indices(:)

        stmt_index = 0
        token = parser%peek()

        if (keyword_should_parse_as_identifier(token, parser)) then
            return
        end if

        select case (token%text)
        case ("public", "private")
            stmt_index = parse_visibility_statement_in_submodule(parser, arena)
        case ("use")
            stmt_index = parse_use_statement(parser, arena)
        case ("namelist")
            stmt_index = parse_namelist_statement_in_submodule(parser, arena)
        case ("integer", "real", "logical", "character", "complex", "procedure")
            stmt_index = parse_declaration(parser, arena)
        case ("type")
            if (parser_is_at_type_definition(parser)) then
                stmt_index = parse_derived_type_def(parser, arena)
            else
                stmt_index = parse_declaration(parser, arena)
            end if
        case ("class")
            stmt_index = parse_declaration(parser, arena)
        case ("implicit")
            call parse_simple_implicit_in_submodule(parser, arena, stmt_index)
            if (stmt_index > 0) then
                extra_indices = take_implicit_additional_indices()
                if (size(extra_indices) > 0) then
                    stmt_index = -1
                end if
            end if
        case ("interface")
            stmt_index = parse_interface_block(parser, arena, prefix_buffer)
        case ("abstract")
            stmt_index = parse_abstract_interface_in_submodule(parser, arena, &
                prefix_buffer)
        end select
    end function parse_submodule_declaration_statement

    function handle_procedure_keyword_in_contains(parser, arena, prefix_buffer, &
            lowered) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=*), intent(in) :: lowered
        integer :: stmt_index
        type(token_t) :: token

        stmt_index = 0
        token = parser%peek()

        if (token%kind /= TK_KEYWORD) return

        select case (trim(lowered))
        case ("interface")
            stmt_index = parse_interface_block(parser, arena, prefix_buffer)
        case ("subroutine")
            stmt_index = parse_subroutine_definition(parser, arena, prefix_buffer)
        case ("function")
            stmt_index = parse_function_definition(parser, arena, prefix_buffer)
        end select
    end function handle_procedure_keyword_in_contains

    function handle_prefix_modifier_in_contains(parser, prefix_buffer, lowered) &
            result(handled)
        type(parser_state_t), intent(inout) :: parser
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=*), intent(in) :: lowered
        logical :: handled
        type(token_t) :: token
        character(len=16), allocatable :: stored(:)

        handled = .false.

        select case (trim(lowered))
        case ("pure", "elemental", "impure", "recursive", &
                "nonrecursive", "non_recursive")
            call prefix_buffer%get_all(stored)
            call append_prefix_token(stored, trim(lowered))
            call prefix_buffer%set(stored)
            if (allocated(stored)) deallocate (stored)
            token = parser%consume()
            handled = .true.
        case ("module")
            ! Check if this is a separate module procedure definition
            ! ISO/IEC 1539-1:2008 Section 12.6.2.5
            ! module procedure <name> with body (not interface declaration)
            if (.not. is_separate_module_procedure_start(parser)) then
                call prefix_buffer%get_all(stored)
                call append_prefix_token(stored, trim(lowered))
                call prefix_buffer%set(stored)
                if (allocated(stored)) deallocate (stored)
                token = parser%consume()
                handled = .true.
            end if
            ! If it IS a separate module procedure, do NOT handle it here
            ! Let parse_contains_section_item_submodule call
            ! parse_separate_module_procedure
        end select
    end function handle_prefix_modifier_in_contains

    logical function is_separate_module_procedure_start(parser) result(is_sep_proc)
        ! Check if current position starts a separate module procedure definition
        ! Pattern: module procedure <identifier> (not module procedure :: ...)
        type(parser_state_t), intent(in) :: parser
        integer :: idx
        type(token_t) :: token
        character(len=:), allocatable :: lowered
        logical :: found_procedure, found_identifier

        is_sep_proc = .false.
        found_procedure = .false.
        found_identifier = .false.
        idx = parser%current_token + 1

        ! Skip whitespace after module
        do while (idx <= parser%get_token_count())
            token = parser%get_token_at_index(idx)
            if (token%kind /= TK_WHITESPACE .and. token%kind /= TK_NEWLINE) exit
            idx = idx + 1
        end do

        if (idx > parser%get_token_count()) return

        ! Check for procedure keyword
        token = parser%get_token_at_index(idx)
        if (token%kind /= TK_KEYWORD) return
        lowered = to_lower(trim(token%text))
        if (lowered /= "procedure") return
        found_procedure = .true.
        idx = idx + 1

        ! Skip whitespace after procedure
        do while (idx <= parser%get_token_count())
            token = parser%get_token_at_index(idx)
            if (token%kind /= TK_WHITESPACE .and. token%kind /= TK_NEWLINE) exit
            idx = idx + 1
        end do

        if (idx > parser%get_token_count()) return

        ! Check next token - if it is identifier (not :: or (), this is
        ! a separate module procedure definition
        token = parser%get_token_at_index(idx)
        if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
            ! This is module procedure <name> - a separate module procedure
            is_sep_proc = .true.
        else if (token%kind == TK_OPERATOR) then
            ! If :: or ( follows, this is NOT a separate module procedure
            ! module procedure :: name1, name2 is interface declaration
            ! module procedure(iface) is procedure declaration
            is_sep_proc = .false.
        end if
    end function is_separate_module_procedure_start

    function handle_type_prefix_in_contains(parser, prefix_buffer, lowered) &
            result(handled)
        type(parser_state_t), intent(inout) :: parser
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=*), intent(in) :: lowered
        logical :: handled
        type(token_t) :: token, lookahead
        character(len=:), allocatable :: lookahead_lower, type_with_kind
        character(len=16), allocatable :: stored(:)

        handled = .false.

        select case (trim(lowered))
        case ("integer", "real", "logical", "character", "complex", &
                "double", "procedure")
            call prefix_buffer%get_all(stored)
            if (trim(lowered) == "double") then
                lookahead = parser%get_token_at_index(parser%current_token + 1)
                lookahead_lower = to_lower(trim(lookahead%text))
                select case (trim(lookahead_lower))
                case ("precision", "complex")
                    token = parser%peek()
                    type_with_kind = trim(token%text)//" "// &
                        trim(lookahead%text)
                    token = parser%consume()
                    token = parser%consume()
                    call consume_optional_kind_spec(parser, type_with_kind)
                    call append_prefix_token(stored, type_with_kind)
                    call prefix_buffer%set(stored)
                    if (allocated(stored)) deallocate (stored)
                    handled = .true.
                    return
                end select
            end if
            token = parser%peek()
            type_with_kind = trim(token%text)
            token = parser%consume()
            call consume_optional_kind_spec(parser, type_with_kind)
            call append_prefix_token(stored, type_with_kind)
            call prefix_buffer%set(stored)
            if (allocated(stored)) deallocate (stored)
            handled = .true.
        case ("type", "class")
            lookahead = parser%get_token_at_index(parser%current_token + 1)
            if (lookahead%kind == TK_OPERATOR .and. lookahead%text == "(") then
                call prefix_buffer%get_all(stored)
                token = parser%peek()
                type_with_kind = trim(token%text)
                token = parser%consume()
                call consume_optional_kind_spec(parser, type_with_kind)
                call append_prefix_token(stored, type_with_kind)
                call prefix_buffer%set(stored)
                if (allocated(stored)) deallocate (stored)
                handled = .true.
            end if
        end select
    end function handle_type_prefix_in_contains

    function parse_contains_section_item_submodule(parser, arena, prefix_buffer) &
            result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: lowered

        stmt_index = 0
        token = parser%peek()
        lowered = to_lower(token%text)

        stmt_index = handle_procedure_keyword_in_contains(parser, arena, &
            prefix_buffer, lowered)
        if (stmt_index /= 0) return

        ! Check for separate module procedure definition
        ! ISO/IEC 1539-1:2008 Section 12.6.2.5
        if (token%kind == TK_KEYWORD .and. lowered == "module") then
            if (is_separate_module_procedure_start(parser)) then
                stmt_index = parse_separate_module_procedure(parser, arena, &
                    prefix_buffer)
                return
            end if
        end if

        if (token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER) then
            if (handle_prefix_modifier_in_contains(parser, prefix_buffer, &
                lowered)) then
                stmt_index = -1
                return
            end if
            if (handle_type_prefix_in_contains(parser, prefix_buffer, lowered)) &
                then
                stmt_index = -1
            end if
        end if
    end function parse_contains_section_item_submodule

    function parse_separate_module_procedure(parser, arena, prefix_buffer) &
            result(proc_index)
        ! Parse a separate module procedure definition
        ! ISO/IEC 1539-1:2008 Section 12.6.2.5
        ! Syntax: module procedure <name>
        !             <body>
        !         end procedure [<name>]
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer :: proc_index

        type(token_t) :: token
        character(len=:), allocatable :: procedure_name
        integer :: line, column
        integer, allocatable :: body_indices(:)
        character(len=16), allocatable :: prefix_keywords(:)
        character(len=16), allocatable :: stored(:)
        logical :: infer_recursive

        proc_index = 0
        infer_recursive = .false.

        ! Get any accumulated prefix keywords
        call prefix_buffer%get_all(stored)
        if (allocated(stored) .and. size(stored) > 0) then
            allocate (prefix_keywords(size(stored) + 1))
            prefix_keywords(1:size(stored)) = stored
            prefix_keywords(size(stored) + 1) = "module"
        else
            allocate (prefix_keywords(1))
            prefix_keywords(1) = "module"
        end if
        call prefix_buffer%clear()

        ! Consume module keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Skip whitespace
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind /= TK_WHITESPACE) exit
            token = parser%consume()
        end do

        ! Consume procedure keyword
        token = parser%peek()
        if (token%kind /= TK_KEYWORD) return
        if (to_lower(trim(token%text)) /= "procedure") return
        token = parser%consume()

        ! Skip whitespace
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind /= TK_WHITESPACE) exit
            token = parser%consume()
        end do

        ! Get procedure name
        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER .and. token%kind /= TK_KEYWORD) then
            proc_index = push_error_node(arena, "Expected procedure name after " &
                //"module procedure", &
                line=line, column=column)
            return
        end if
        procedure_name = trim(token%text)
        token = parser%consume()

        ! Parse the procedure body until end procedure
        call parse_procedure_body(parser, arena, procedure_name, "procedure", &
            body_indices, infer_recursive)

        ! Create subroutine def node with module prefix
        ! Note: module procedure has no parameters - they come from the interface
        proc_index = push_subroutine_def(arena, procedure_name, &
            body_indices=body_indices, &
            line=line, column=column, &
            prefix_keywords=prefix_keywords)
    end function parse_separate_module_procedure

    function parse_visibility_statement_in_submodule(parser, arena) result(stmt_index)
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
    end function parse_visibility_statement_in_submodule

    function parse_namelist_statement_in_submodule(parser, arena) result(stmt_index)
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
    end function parse_namelist_statement_in_submodule

    subroutine parse_simple_implicit_in_submodule(parser, arena, stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index

        stmt_index = parse_implicit_statement(parser, arena)
    end subroutine parse_simple_implicit_in_submodule

    subroutine handle_submodule_identifier_assignment(parser, arena, &
            declaration_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: declaration_indices(:)
        type(token_t) :: id_token, eq_token, rhs_token
        integer :: target_index, rhs_index, assign_index
        character(len=:), allocatable :: assignment_op

        assign_index = 0
        id_token = parser%consume()

        eq_token = parser%peek()
        if (eq_token%kind /= TK_OPERATOR) return
        if (eq_token%text /= "=" .and. eq_token%text /= "=>") return

        eq_token = parser%consume()
        assignment_op = eq_token%text

        target_index = push_identifier(arena, id_token%text, id_token%line, &
            id_token%column)

        rhs_token = parser%peek()
        if (rhs_token%kind /= TK_NUMBER .and. rhs_token%kind /= TK_IDENTIFIER) then
            return
        end if

        rhs_token = parser%consume()
        if (rhs_token%kind == TK_IDENTIFIER) then
            rhs_index = push_identifier(arena, rhs_token%text, rhs_token%line, &
                rhs_token%column)
        else
            rhs_index = push_literal(arena, rhs_token%text, rhs_token%line, &
                rhs_token%column, LITERAL_STRING)
        end if

        if (rhs_index <= 0 .or. target_index <= 0) return

        if (.not. allocated(assignment_op)) assignment_op = "="
        assign_index = push_assignment(arena, target_index, rhs_index, &
            id_token%line, id_token%column, &
            operator_text=assignment_op)
        if (assign_index > 0) then
            declaration_indices = [declaration_indices, assign_index]
        end if
    end subroutine handle_submodule_identifier_assignment

    function parse_abstract_interface_in_submodule(parser, arena, prefix_buffer) &
            result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer :: stmt_index
        type(token_t) :: token, next_token
        integer :: lookahead_idx
        logical :: is_abstract_interface

        stmt_index = 0
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
            token = parser%consume()
            stmt_index = parse_interface_block(parser, arena, prefix_buffer, &
                is_abstract=.true.)
        end if
    end function parse_abstract_interface_in_submodule

end module parser_submodule_helpers_module
