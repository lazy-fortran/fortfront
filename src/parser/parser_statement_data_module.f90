module parser_statement_data_module
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_OPERATOR, TK_NEWLINE, &
                          TK_COMMENT, TK_WHITESPACE
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_expression_until
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: array_literal_node
    use ast_factory, only: push_assignment, push_identifier, push_array_literal, &
                           push_namelist_statement
    use type_system_unified, only: create_mono_type, TARRAY
    implicit none
    private

    public :: parse_data_statement
    public :: parse_namelist_statement

contains

    integer function parse_data_statement(parser, arena, parent_index) &
        result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(token_t) :: token
        type(token_t) :: target_token
        integer :: target_index
        integer, allocatable :: element_indices(:)
        integer :: value_index
        logical :: expect_value

        stmt_index = 0

        token = parser%consume()
        call skip_trivia(parser)

        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER) then
            call parser%error("Expected identifier after DATA keyword")
            return
        end if

        target_token = parser%consume()
        if (present(parent_index)) then
            target_index = push_identifier(arena, trim(target_token%text), &
                                           target_token%line, target_token%column, &
                                           parent_index)
        else
            target_index = push_identifier(arena, trim(target_token%text), &
                                           target_token%line, target_token%column)
        end if
        if (target_index <= 0) return

        call skip_trivia(parser)
        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. trim(token%text) /= "/") then
            call parser%error("Expected '/' to begin DATA value list")
            return
        end if
        token = parser%consume()

        expect_value = .true.
        call skip_trivia(parser)

        do
            token = parser%peek()
            if (token%kind == TK_EOF) then
                call parser%error("Unexpected end of statement inside DATA value list")
                return
            end if

            if (token%kind == TK_OPERATOR .and. trim(token%text) == "/") then
                if (expect_value) then
                    call parser%error("DATA statement value list cannot be empty")
                    return
                end if
                exit
            end if

            value_index = parse_expression_until(parser, arena, [",", "/"])
            if (value_index <= 0) return
            call append_index(element_indices, value_index)

            call skip_trivia(parser)
            token = parser%peek()
            if (token%kind == TK_OPERATOR) then
                select case (trim(token%text))
                case (",")
                    token = parser%consume()
                    expect_value = .true.
                    call skip_trivia(parser)
                    cycle
                case ("/")
                    expect_value = .false.
                case default
                    call parser%error("Unexpected token in DATA statement value list")
                    return
                end select
            else
                call parser%error("Unexpected token in DATA statement value list")
                return
            end if
        end do

        token = parser%consume()

        if (.not. allocated(element_indices)) then
            call parser%error("DATA statement requires at least one value")
            return
        end if

        block
            integer :: array_index
            integer :: element_count
            element_count = size(element_indices)
            if (present(parent_index)) then
                array_index = push_array_literal(arena, element_indices, &
                                                 target_token%line, &
                                                 target_token%column, &
                                                 parent_index, syntax_style="legacy")
            else
                array_index = push_array_literal(arena, element_indices, &
                                                 target_token%line, &
                                                 target_token%column, &
                                                 syntax_style="legacy")
            end if
            if (array_index <= 0) return

            call annotate_array_literal(arena, array_index, element_count)

            if (present(parent_index)) then
                stmt_index = push_assignment(arena, target_index, array_index, &
                                             target_token%line, target_token%column, &
                                             parent_index)
            else
                stmt_index = push_assignment(arena, target_index, array_index, &
                                             target_token%line, target_token%column)
            end if
        end block
    contains
        subroutine append_index(indices, value)
            integer, allocatable, intent(inout) :: indices(:)
            integer, intent(in) :: value
            integer, allocatable :: tmp(:)

            if (value <= 0) return
            if (.not. allocated(indices)) then
                allocate (indices(1))
                indices(1) = value
            else
                allocate (tmp(size(indices) + 1))
                tmp(1:size(indices)) = indices
                tmp(size(indices) + 1) = value
                call move_alloc(tmp, indices)
            end if
        end subroutine append_index

        subroutine annotate_array_literal(arena, array_index, element_count)
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in) :: array_index
            integer, intent(in) :: element_count

            if (array_index <= 0 .or. array_index > arena%size) return
            if (.not. allocated(arena%entries(array_index)%node)) return

            select type (array_node => arena%entries(array_index)%node)
            type is (array_literal_node)
                array_node%inferred_type = create_mono_type(TARRAY, &
                                                            array_size=element_count)
            end select
        end subroutine annotate_array_literal
    end function parse_data_statement

    integer function parse_namelist_statement(parser, arena, parent_index) &
        result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(token_t) :: token
        character(len=:), allocatable :: group_name
        character(len=:), allocatable :: names(:)
        integer :: line
        integer :: column

        stmt_index = 0
        token = parser%peek()
        line = token%line
        column = token%column

        token = parser%consume()

        if (parser%is_at_end()) return
        token = parser%peek()
        if (.not. (token%kind == TK_OPERATOR .and. token%text == "/")) return
        token = parser%consume()

        if (parser%is_at_end()) return
        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER) return
        group_name = trim(token%text)
        token = parser%consume()

        if (parser%is_at_end()) return
        token = parser%peek()
        if (.not. (token%kind == TK_OPERATOR .and. token%text == "/")) return
        token = parser%consume()

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

        if (.not. allocated(group_name)) return

        if (present(parent_index)) then
            if (allocated(names)) then
                stmt_index = push_namelist_statement(arena, group_name, names, &
                                                     line, column, parent_index)
            else
                stmt_index = push_namelist_statement(arena, group_name, line=line, &
                                                     column=column, &
                                                     parent_index=parent_index)
            end if
        else
            if (allocated(names)) then
                stmt_index = push_namelist_statement(arena, group_name, names, &
                                                     line, column)
            else
                stmt_index = push_namelist_statement(arena, group_name, line=line, &
                                                     column=column)
            end if
        end if
    contains
        subroutine append_name(list, value)
            character(len=:), allocatable, intent(inout) :: list(:)
            character(len=*), intent(in) :: value
            character(len=:), allocatable :: temp(:)
            integer :: n
            integer :: current_len
            integer :: target_len

            if (.not. allocated(list)) then
                allocate (character(len=len_trim(value)) :: list(1))
                list(1) = trim(value)
            else
                n = size(list)
                current_len = len(list)
                target_len = len_trim(value)
                target_len = max(current_len, target_len)
                allocate (character(len=target_len) :: temp(n + 1))
                temp(1:n) = list
                temp(n + 1) = trim(value)
                call move_alloc(temp, list)
            end if
        end subroutine append_name
    end function parse_namelist_statement

    subroutine skip_trivia(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: current

        do
            current = parser%peek()
            if (current%kind == TK_WHITESPACE .or. current%kind == TK_COMMENT) then
                current = parser%consume()
            else
                exit
            end if
        end do
    end subroutine skip_trivia

end module parser_statement_data_module
