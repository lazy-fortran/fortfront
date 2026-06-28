module parser_interface_module_procedures_module
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: string_t
    use ast_factory, only: push_module_procedure
    use parser_state_module, only: parser_state_t
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_OPERATOR, &
        TK_WHITESPACE, TK_NEWLINE, TK_COMMENT
    implicit none
    private
    public :: parse_module_procedure_statement
    public :: parse_plain_procedure_statement
contains

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
                else if (trim(token%text) == ";") then
                    ! Statement separator ends the list; leave it for the caller.
                    exit
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

end module parser_interface_module_procedures_module
