module parser_interface_blocks_module
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_OPERATOR, TK_COMMENT, &
                          TK_NEWLINE, TK_WHITESPACE
    use parser_state_module, only: parser_state_t
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use parser_procedure_shared_module, only: consume_optional_kind_spec
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_interface_block, push_module_procedure, &
                           push_import_statement
    use ast_base, only: string_t
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

        handled = .false.
        stmt_index = 0

        if (token%kind /= TK_KEYWORD) return

        lowered_text = to_lower(token%text)
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

    subroutine begin_interface_block(parser, interface_name, interface_kind, &
                                     operator_symbol, line, column, is_abstract)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: interface_name
        character(len=:), allocatable, intent(out) :: interface_kind
        character(len=:), allocatable, intent(out) :: operator_symbol
        integer, intent(out) :: line, column
        logical, intent(in), optional :: is_abstract

        type(token_t) :: token
        character(len=:), allocatable :: lowered

        token = parser%consume()
        line = token%line
        column = token%column

        interface_kind = "interface"
        operator_symbol = ""

        token = parser%peek()
        if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
            lowered = to_lower(trim(token%text))
            if (trim(lowered) == "operator" .or. trim(lowered) == "assignment") then
                interface_kind = trim(lowered)
                token = parser%consume()
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. trim(token%text) == "(") then
                    token = parser%consume()
                    token = parser%peek()
                    operator_symbol = trim(token%text)
                    token = parser%consume()
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. trim(token%text) == ")") then
                        token = parser%consume()
                    end if
                end if
                interface_name = ""
            else
                token = parser%consume()
                interface_name = token%text
            end if
        else
            interface_name = ""
        end if
    end subroutine begin_interface_block

    logical function handle_interface_end(parser, first_token) result(is_end)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), intent(in) :: first_token

        type(token_t) :: next_token
        character(len=:), allocatable :: lowered_text

        is_end = .false.
        if (first_token%kind /= TK_KEYWORD) return

        lowered_text = to_lower(first_token%text)
        if (trim(lowered_text) /= "end") return

        next_token = parser%get_token_at_index(parser%current_token + 1)
        if (next_token%kind /= TK_KEYWORD) return

        lowered_text = to_lower(next_token%text)
        if (trim(lowered_text) /= "interface") return

        next_token = parser%consume()
        next_token = parser%consume()

        next_token = parser%peek()
        if (next_token%kind == TK_IDENTIFIER) then
            next_token = parser%consume()
        end if

        is_end = .true.
    end function handle_interface_end

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
        character(len=16), allocatable :: prefix_array(:)

        handled = .false.
        if (token%kind == TK_KEYWORD) then
            lowered_text = to_lower(token%text)
            if (trim(lowered_text) == "module") then
                stmt_index = parse_module_procedure_statement(parser, arena)
                if (stmt_index > 0) then
                    body_indices = [body_indices, stmt_index]
                end if
                handled = .true.
                return
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
                allocate (character(len=16) :: prefix_array(1))
                prefix_array(1) = trim(lowered_text)
                call prefix_buffer%append_all(prefix_array)
                consumed_token = parser%consume()
                handled = .true.
                return
            else if (is_interface_return_type_keyword(lowered_text)) then
                call collect_interface_return_type(parser, prefix_buffer, lowered_text)
                handled = .true.
                return
            end if
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

    logical function is_procedure_prefix(lowered_text) result(is_prefix)
        character(len=*), intent(in) :: lowered_text

        is_prefix = trim(lowered_text) == "pure" .or. &
                    trim(lowered_text) == "elemental" .or. &
                    trim(lowered_text) == "recursive" .or. &
                    trim(lowered_text) == "impure" .or. &
                    trim(lowered_text) == "nonrecursive" .or. &
                    trim(lowered_text) == "non_recursive"
    end function is_procedure_prefix

    logical function is_interface_return_type_keyword(lowered_text) result(is_type)
        character(len=*), intent(in) :: lowered_text

        is_type = trim(lowered_text) == "integer" .or. &
                  trim(lowered_text) == "real" .or. &
                  trim(lowered_text) == "logical" .or. &
                  trim(lowered_text) == "character" .or. &
                  trim(lowered_text) == "complex" .or. &
                  trim(lowered_text) == "double"
    end function is_interface_return_type_keyword

    subroutine collect_interface_return_type(parser, prefix_buffer, lowered_text)
        type(parser_state_t), intent(inout) :: parser
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=*), intent(in) :: lowered_text

        type(token_t) :: type_token, lookahead_token
        character(len=:), allocatable :: type_with_kind
        character(len=:), allocatable :: lookahead_lower
        character(len=16), allocatable :: prefix_array(:)

        if (trim(lowered_text) == "double") then
            lookahead_token = parser%get_token_at_index(parser%current_token + 1)
            lookahead_lower = to_lower(trim(lookahead_token%text))
            if (trim(lookahead_lower) == "precision" .or. &
                trim(lookahead_lower) == "complex") then
                type_token = parser%consume()
                lookahead_token = parser%consume()
                type_with_kind = trim(type_token%text)//" "// &
                                 trim(lookahead_token%text)
                call consume_optional_kind_spec(parser, type_with_kind)
                allocate (character(len=16) :: prefix_array(1))
                prefix_array(1) = trim(type_with_kind)
                call prefix_buffer%append_all(prefix_array)
                return
            end if
        end if

        type_token = parser%consume()
        type_with_kind = trim(type_token%text)
        call consume_optional_kind_spec(parser, type_with_kind)
        allocate (character(len=16) :: prefix_array(1))
        prefix_array(1) = trim(type_with_kind)
        call prefix_buffer%append_all(prefix_array)
    end subroutine collect_interface_return_type

    function parse_module_procedure_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index

        type(string_t), allocatable :: procedure_names(:)
        integer :: line, column
        logical :: has_double_colon

        stmt_index = 0
        allocate (procedure_names(0))

        if (.not. consume_module_procedure_header(parser, line, column, &
                                                  has_double_colon)) return

        call collect_module_procedure_names(parser, procedure_names)

        if (size(procedure_names) > 0) then
            stmt_index = push_module_procedure(arena, procedure_names, line, &
                                               column, &
                                               has_double_colon=has_double_colon)
        end if
    end function parse_module_procedure_statement

    function parse_plain_procedure_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index

        type(string_t), allocatable :: procedure_names(:)
        integer :: line, column
        logical :: has_double_colon

        stmt_index = 0
        allocate (procedure_names(0))

        if (.not. consume_plain_procedure_header(parser, line, column, &
                                                 has_double_colon)) return

        call collect_module_procedure_names(parser, procedure_names)

        if (size(procedure_names) > 0) then
            stmt_index = push_module_procedure(arena, procedure_names, line, column, &
                                               has_module_prefix=.false., &
                                               has_double_colon=has_double_colon)
        end if
    end function parse_plain_procedure_statement

    logical function consume_module_procedure_header(parser, line, column, &
                                                     has_double_colon) &
        result(is_valid)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(out) :: line, column
        logical, intent(out) :: has_double_colon

        type(token_t) :: token
        character(len=:), allocatable :: lowered_text

        token = parser%consume()
        line = token%line
        column = token%column
        has_double_colon = .false.

        token = parser%peek()
        if (token%kind /= TK_KEYWORD) then
            is_valid = .false.
            return
        end if

        lowered_text = to_lower(token%text)
        if (trim(lowered_text) /= "procedure") then
            is_valid = .false.
            return
        end if

        token = parser%consume()

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "::") then
            token = parser%consume()
            has_double_colon = .true.
        end if

        is_valid = .true.
    end function consume_module_procedure_header

    logical function consume_plain_procedure_header(parser, line, column, &
                                                    has_double_colon) &
        result(is_valid)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(out) :: line, column
        logical, intent(out) :: has_double_colon

        type(token_t) :: token
        character(len=:), allocatable :: lowered_text

        is_valid = .false.
        has_double_colon = .false.
        token = parser%consume()
        line = token%line
        column = token%column

        lowered_text = to_lower(token%text)
        if (trim(lowered_text) /= "procedure") return

        call skip_interface_trivia(parser)
        call skip_procedure_interface_spec(parser)
        has_double_colon = skip_procedure_attribute_list(parser)
        is_valid = .true.
    end function consume_plain_procedure_header

    subroutine collect_module_procedure_names(parser, procedure_names)
        type(parser_state_t), intent(inout) :: parser
        type(string_t), allocatable, intent(inout) :: procedure_names(:)

        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_IDENTIFIER)
                call append_procedure_name(procedure_names, token%text)
                token = parser%consume()
            case (TK_OPERATOR)
                if (trim(token%text) == ",") then
                    token = parser%consume()
                else
                    call parser%error("Unexpected operator '"// &
                                      trim(token%text)// &
                                      "' in module procedure list.")
                    token = parser%consume()
                    exit
                end if
            case (TK_COMMENT, TK_NEWLINE, TK_KEYWORD)
                exit
            case (TK_WHITESPACE)
                token = parser%consume()
            case default
                call parser%error("Unexpected token '"//trim(token%text)// &
                                  "' in module procedure list.")
                token = parser%consume()
                exit
            end select
        end do
    end subroutine collect_module_procedure_names

    subroutine skip_procedure_interface_spec(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        integer :: depth

        call skip_interface_trivia(parser)
        token = parser%peek()
        if (token%kind /= TK_OPERATOR) return
        if (trim(token%text) /= "(") return

        depth = 0
        do while (.not. parser%is_at_end())
            token = parser%consume()
            if (token%kind /= TK_OPERATOR) cycle
            select case (trim(token%text))
            case ("(")
                depth = depth + 1
            case (")")
                depth = depth - 1
                if (depth <= 0) exit
            end select
        end do
    end subroutine skip_procedure_interface_spec

    logical function skip_procedure_attribute_list(parser) result(has_double_colon)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        character(len=:), allocatable :: lowered_text

        has_double_colon = .false.
        do while (.not. parser%is_at_end())
            call skip_interface_trivia(parser)
            token = parser%peek()
            select case (token%kind)
            case (TK_OPERATOR)
                select case (trim(token%text))
                case ("::")
                    token = parser%consume()
                    has_double_colon = .true.
                    return
                case (",")
                    token = parser%consume()
                case ("(")
                    call skip_balanced_parentheses_tokens(parser)
                case default
                    return
                end select
            case (TK_IDENTIFIER, TK_KEYWORD)
                lowered_text = to_lower(trim(token%text))
                if (.not. is_procedure_attribute_keyword(lowered_text)) return
                token = parser%consume()
                if (lowered_text == "pass" .or. lowered_text == "bind") then
                    call skip_attribute_argument(parser)
                end if
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                token = parser%consume()
            case default
                return
            end select
        end do
    end function skip_procedure_attribute_list

    subroutine skip_attribute_argument(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        call skip_interface_trivia(parser)
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. trim(token%text) == "(") then
            call skip_balanced_parentheses_tokens(parser)
        end if
    end subroutine skip_attribute_argument

    subroutine skip_balanced_parentheses_tokens(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        integer :: depth

        call skip_interface_trivia(parser)
        token = parser%peek()
        if (token%kind /= TK_OPERATOR) return
        if (trim(token%text) /= "(") return

        depth = 0
        do while (.not. parser%is_at_end())
            token = parser%consume()
            if (token%kind /= TK_OPERATOR) cycle
            select case (trim(token%text))
            case ("(")
                depth = depth + 1
            case (")")
                depth = depth - 1
                if (depth <= 0) exit
            end select
        end do
    end subroutine skip_balanced_parentheses_tokens

    logical function is_procedure_attribute_keyword(text) result(is_attr)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lowered

        lowered = trim(text)
        is_attr = lowered == "pass" .or. lowered == "nopass" .or. &
                  lowered == "pointer" .or. lowered == "protected" .or. &
                  lowered == "private" .or. lowered == "public" .or. &
                  lowered == "save" .or. lowered == "bind"
    end function is_procedure_attribute_keyword

    subroutine skip_interface_trivia(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                token = parser%consume()
            case default
                return
            end select
        end do
    end subroutine skip_interface_trivia

    subroutine append_procedure_name(list, value)
        type(string_t), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: value
        type(string_t), allocatable :: tmp(:)
        integer :: n

        if (len_trim(value) == 0) return

        if (.not. allocated(list)) then
            allocate (list(1))
            list(1)%s = trim(value)
            return
        end if

        n = size(list)
        allocate (tmp(n + 1))
        if (n > 0) tmp(1:n) = list
        tmp(n + 1)%s = trim(value)
        call move_alloc(tmp, list)
    end subroutine append_procedure_name

    function parse_import_statement(parser, arena) result(stmt_index)
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
    end function parse_import_statement

end module parser_interface_blocks_module
