module parser_interface_blocks_module
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_OPERATOR, TK_COMMENT, &
                          TK_NEWLINE, TK_WHITESPACE
    use parser_state_module, only: parser_state_t
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_interface_block, push_module_procedure
    use ast_base, only: string_t
    implicit none
    private

    public :: parse_interface_block

contains

    function parse_interface_block(parser, arena, prefix_buffer) &
        result(interface_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        integer :: interface_index

        character(len=:), allocatable :: interface_name
        integer :: line, column
        integer, allocatable :: body_indices(:)
        type(token_t) :: token
        integer :: stmt_index

        call begin_interface_block(parser, interface_name, line, column)
        call prefix_buffer%clear()

        allocate (body_indices(0))

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (handle_interface_end(parser, token)) exit
            if (process_interface_body_token(parser, arena, token, body_indices)) cycle

            call parser%error("Unexpected token '"//trim(token%text)// &
                              "' in interface block.")
            token = parser%consume()
        end do

        interface_index = push_interface_block(arena, interface_name, body_indices, &
                                               line, column)
    end function parse_interface_block

    subroutine begin_interface_block(parser, interface_name, line, column)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: interface_name
        integer, intent(out) :: line, column

        type(token_t) :: token

        token = parser%consume()
        line = token%line
        column = token%column

        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            interface_name = token%text
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
                                                  body_indices) result(handled)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: token
        integer, allocatable, intent(inout) :: body_indices(:)

        integer :: stmt_index
        type(token_t) :: consumed_token
        character(len=:), allocatable :: lowered_text

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
            end if
        end if

        select case (token%kind)
        case (TK_NEWLINE, TK_COMMENT)
            consumed_token = parser%consume()
            handled = .true.
        end select
    end function process_interface_body_token

    function parse_module_procedure_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index

        type(string_t), allocatable :: procedure_names(:)
        integer :: line, column

        stmt_index = 0
        allocate (procedure_names(0))

        if (.not. consume_module_procedure_header(parser, line, column)) return

        call collect_module_procedure_names(parser, procedure_names)

        if (size(procedure_names) > 0) then
            stmt_index = push_module_procedure(arena, procedure_names, line, column)
        end if
    end function parse_module_procedure_statement

    logical function consume_module_procedure_header(parser, line, column) &
        result(is_valid)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(out) :: line, column

        type(token_t) :: token
        character(len=:), allocatable :: lowered_text

        token = parser%consume()
        line = token%line
        column = token%column

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
        end if

        is_valid = .true.
    end function consume_module_procedure_header

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

end module parser_interface_blocks_module
