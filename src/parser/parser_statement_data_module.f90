module parser_statement_data_module
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_OPERATOR, TK_NEWLINE, &
                          TK_COMMENT, TK_WHITESPACE
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_expression_until
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: array_literal_node, binary_op_node, literal_node
    use ast_factory, only: push_assignment, push_identifier, push_array_literal, &
                           push_namelist_statement
    use type_system_unified, only: create_mono_type, TARRAY
    use parser_namelist_shared_module, only: consume_namelist_group
    use ast_base, only: LITERAL_INTEGER
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
            call expand_repeat_count(arena, value_index, element_indices)

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
        subroutine expand_repeat_count(arena, value_index, indices)
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in) :: value_index
            integer, allocatable, intent(inout) :: indices(:)
            integer :: repeat_count
            integer :: loop_index
            integer :: read_status
            integer :: underscore_pos
            character(len=:), allocatable :: literal_text
            character(len=:), allocatable :: count_text

            if (value_index <= 0 .or. value_index > arena%size) return
            if (.not. allocated(arena%entries(value_index)%node)) then
                call append_index(indices, value_index)
                return
            end if

            select type (node => arena%entries(value_index)%node)
            type is (binary_op_node)
                if (.not. allocated(node%operator)) then
                    call append_index(indices, value_index)
                    return
                end if
                if (trim(node%operator) /= "*") then
                    call append_index(indices, value_index)
                    return
                end if

                if (node%left_index <= 0 .or. node%left_index > arena%size) then
                    call append_index(indices, value_index)
                    return
                end if
                if (.not. allocated(arena%entries(node%left_index)%node)) then
                    call append_index(indices, value_index)
                    return
                end if

                if (node%right_index <= 0 .or. node%right_index > arena%size) then
                    call append_index(indices, value_index)
                    return
                end if

                select type (left_node => arena%entries(node%left_index)%node)
                type is (literal_node)
                    if (.not. allocated(left_node%value)) then
                        call append_index(indices, value_index)
                        return
                    end if
                    if (left_node%literal_kind /= LITERAL_INTEGER) then
                        call append_index(indices, value_index)
                        return
                    end if

                    literal_text = trim(left_node%value)
                    if (len(literal_text) == 0) then
                        call append_index(indices, value_index)
                        return
                    end if

                    underscore_pos = index(literal_text, "_")
                    if (underscore_pos > 0) then
                        if (underscore_pos == 1) then
                            call append_index(indices, value_index)
                            return
                        end if
                        count_text = literal_text(:underscore_pos - 1)
                    else
                        count_text = literal_text
                    end if

                    count_text = trim(adjustl(count_text))
                    if (len(count_text) == 0) then
                        call append_index(indices, value_index)
                        return
                    end if

                    read (count_text, *, iostat=read_status) repeat_count
                    if (read_status /= 0) then
                        call append_index(indices, value_index)
                        return
                    end if

                    if (repeat_count <= 0) then
                        call append_index(indices, value_index)
                        return
                    end if

                    do loop_index = 1, repeat_count
                        call append_index(indices, node%right_index)
                    end do
                    return
                class default
                    call append_index(indices, value_index)
                    return
                end select
            class default
                call append_index(indices, value_index)
                return
            end select
        end subroutine expand_repeat_count

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
        logical :: has_group

        stmt_index = 0
        token = parser%peek()
        line = token%line
        column = token%column

        token = parser%consume()

        has_group = consume_namelist_group(parser, group_name, names)
        if (.not. has_group) return

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
