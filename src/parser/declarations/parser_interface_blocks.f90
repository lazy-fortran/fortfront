module parser_interface_blocks_module
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_OPERATOR, TK_COMMENT, &
                          TK_NEWLINE, TK_WHITESPACE
    use parser_state_module, only: parser_state_t
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_interface_block
    use parser_interface_block_headers_module, only: begin_interface_block, &
                                                     handle_interface_end
    use parser_interface_prefix_module, only: is_procedure_prefix, &
                                              is_interface_return_type_keyword, &
                                              is_interface_return_type_with_parens, &
                                              collect_interface_return_type, &
                                              append_interface_prefix
    use parser_interface_module_procedures_module, only: &
        parse_module_procedure_statement, parse_plain_procedure_statement
    use parser_interface_import_module, only: parse_import_statement
    implicit none
    private

    public :: parse_interface_block
    public :: parse_interface_procedure
    public :: set_interface_procedure_parser

    abstract interface
        function interface_proc_parser_t(parser, arena, prefix_buffer) result(proc_idx)
            import :: parser_state_t, ast_arena_t, parser_prefix_buffer_t
            type(parser_state_t), intent(inout) :: parser
            type(ast_arena_t), intent(inout) :: arena
            type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
            integer :: proc_idx
        end function interface_proc_parser_t
    end interface

    procedure(interface_proc_parser_t), pointer :: proc_parser_callback => null()

contains

    function parse_interface_block(parser, arena, prefix_buffer, is_abstract) &
        result(interface_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        logical, intent(in), optional :: is_abstract
        integer :: interface_index

        character(len=:), allocatable :: interface_name, interface_kind, operator_symbol
        integer :: line, column
        integer, allocatable :: body_indices(:)
        type(token_t) :: token
        integer :: stmt_index
        logical :: is_abstract_interface

        is_abstract_interface = .false.
        if (present(is_abstract)) is_abstract_interface = is_abstract

        call begin_interface_block(parser, interface_name, interface_kind, &
                                   operator_symbol, line, column, is_abstract_interface)
        call prefix_buffer%clear()

        allocate (body_indices(0))

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (handle_interface_end(parser, token)) exit

            stmt_index = 0
            if (try_parse_interface_procedure(parser, arena, prefix_buffer, token, &
                                              stmt_index)) then
                if (stmt_index > 0) then
                    body_indices = [body_indices, stmt_index]
                end if
                cycle
            end if

            if (process_interface_body_token(parser, arena, token, body_indices, &
                                             prefix_buffer)) cycle

            call parser%error("Unexpected token '"//trim(token%text)// &
                              "' in interface block.")
            token = parser%consume()
        end do

        interface_index = push_interface_block( &
                          arena, interface_name, body_indices, line, column, &
                          is_abstract=is_abstract_interface, kind=interface_kind, &
                          operator_symbol=operator_symbol)
    end function parse_interface_block

    logical function try_parse_interface_procedure(parser, arena, prefix_buffer, &
                                                   token, stmt_index) result(handled)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        type(token_t), intent(in) :: token
        integer, intent(out) :: stmt_index

        character(len=:), allocatable :: lowered_text
        type(token_t) :: consumed_prefix

        handled = .false.
        stmt_index = 0

        if (.not. (token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER)) return

        lowered_text = to_lower(token%text)

        if (is_procedure_prefix(lowered_text)) then
            call append_interface_prefix(prefix_buffer, lowered_text)
            consumed_prefix = parser%consume()
            handled = .true.
            return
        else if (is_interface_return_type_keyword(lowered_text)) then
            call collect_interface_return_type(parser, prefix_buffer, lowered_text)
            handled = .true.
            return
        else if (is_interface_return_type_with_parens(parser, lowered_text)) then
            call collect_interface_return_type(parser, prefix_buffer, lowered_text)
            handled = .true.
            return
        end if

        if (token%kind /= TK_KEYWORD) return

        if (trim(lowered_text) /= "subroutine" .and. &
            trim(lowered_text) /= "function") return

        stmt_index = parse_interface_procedure(parser, arena, prefix_buffer)
        handled = .true.
    end function try_parse_interface_procedure

    function parse_interface_procedure(parser, arena, prefix_buffer) &
        result(proc_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer :: proc_index

        if (associated(proc_parser_callback)) then
            proc_index = proc_parser_callback(parser, arena, prefix_buffer)
        else
            proc_index = 0
        end if
    end function parse_interface_procedure

    subroutine set_interface_procedure_parser(parser_func)
        procedure(interface_proc_parser_t) :: parser_func
        proc_parser_callback => parser_func
    end subroutine set_interface_procedure_parser

    logical function process_interface_body_token(parser, arena, token, &
                                                  body_indices, prefix_buffer) &
        result(handled)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: token
        integer, allocatable, intent(inout) :: body_indices(:)
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer

        integer :: stmt_index
        type(token_t) :: consumed_token
        character(len=:), allocatable :: lowered_text

        handled = .false.
        if (token%kind == TK_KEYWORD) then
            lowered_text = to_lower(token%text)
            if (trim(lowered_text) == "module") then
                ! Check if this is module procedure :: name1, name2 (declaration)
                ! or module subroutine/function (procedure definition with prefix)
                if (is_module_procedure_declaration(parser)) then
                    stmt_index = parse_module_procedure_statement(parser, arena)
                    if (stmt_index > 0) then
                        body_indices = [body_indices, stmt_index]
                    end if
                    handled = .true.
                    return
                else
                    ! Treat module as procedure prefix for module subroutine/function
                    call append_interface_prefix(prefix_buffer, lowered_text)
                    consumed_token = parser%consume()
                    handled = .true.
                    return
                end if
            else if (trim(lowered_text) == "import") then
                stmt_index = parse_import_statement(parser, arena)
                if (stmt_index > 0) then
                    body_indices = [body_indices, stmt_index]
                end if
                handled = .true.
                return
            else if (trim(lowered_text) == "procedure") then
                stmt_index = parse_plain_procedure_statement(parser, arena)
                if (stmt_index > 0) then
                    body_indices = [body_indices, stmt_index]
                end if
                handled = .true.
                return
            else if (is_procedure_prefix(lowered_text)) then
                ! Collect procedure prefix keywords (pure, elemental, recursive, etc.)
                ! These will be used when we encounter the actual function/subroutine
                call append_interface_prefix(prefix_buffer, lowered_text)
                consumed_token = parser%consume()
                handled = .true.
                return
            else if (is_interface_return_type_keyword(lowered_text)) then
                call collect_interface_return_type(parser, prefix_buffer, lowered_text)
                handled = .true.
                return
            else if (is_interface_return_type_with_parens(parser, lowered_text)) then
                call collect_interface_return_type(parser, prefix_buffer, lowered_text)
                handled = .true.
                return
            end if
        end if

        if (token%kind == TK_IDENTIFIER) then
            lowered_text = to_lower(token%text)
            if (is_procedure_prefix(lowered_text)) then
                call append_interface_prefix(prefix_buffer, lowered_text)
                consumed_token = parser%consume()
                handled = .true.
                return
            else if (is_interface_return_type_keyword(lowered_text)) then
                call collect_interface_return_type(parser, prefix_buffer, lowered_text)
                handled = .true.
                return
            end if
        end if

        ! A semicolon separates statements on one line; skip it like a newline.
        if (token%kind == TK_OPERATOR .and. trim(token%text) == ";") then
            consumed_token = parser%consume()
            handled = .true.
            return
        end if

        select case (token%kind)
        case (TK_NEWLINE, TK_COMMENT)
            consumed_token = parser%consume()
            handled = .true.
        case (TK_WHITESPACE)
            consumed_token = parser%consume()
            handled = .true.
        end select
    end function process_interface_body_token

    logical function is_module_procedure_declaration(parser) result(is_decl)
        ! Check if current position is a module procedure declaration
        ! Pattern: module procedure [::] name1, name2 (NOT module subroutine/function)
        type(parser_state_t), intent(in) :: parser
        integer :: idx
        type(token_t) :: token
        character(len=:), allocatable :: lowered

        is_decl = .false.
        idx = parser%current_token + 1

        ! Skip whitespace after module
        do while (idx <= parser%get_token_count())
            token = parser%get_token_at_index(idx)
            if (token%kind /= TK_WHITESPACE .and. token%kind /= TK_NEWLINE) exit
            idx = idx + 1
        end do

        if (idx > parser%get_token_count()) return

        ! Check next token
        token = parser%get_token_at_index(idx)
        if (token%kind /= TK_KEYWORD) return

        lowered = to_lower(trim(token%text))

        ! If followed by procedure, this is module procedure declaration
        ! If followed by subroutine/function, this is a procedure definition
        if (lowered == "procedure") then
            is_decl = .true.
        else if (lowered == "subroutine" .or. lowered == "function") then
            is_decl = .false.
        end if
    end function is_module_procedure_declaration

end module parser_interface_blocks_module
