module parser_statement_data_module
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_OPERATOR, TK_NEWLINE, &
                          TK_COMMENT, TK_WHITESPACE
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_expression_until
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: array_literal_node, binary_op_node, literal_node
    use ast_factory, only: push_assignment, push_identifier, push_array_literal, &
                           push_namelist_statement, push_data_statement
    use type_system_unified, only: create_mono_type, TARRAY
    use parser_namelist_shared_module, only: consume_namelist_group
    use ast_base, only: LITERAL_INTEGER
    implicit none
    private

    public :: parse_data_statement
    public :: parse_namelist_statement
    public :: get_data_additional_indices

    integer, allocatable :: pending_data_assignment_indices(:)

contains

    integer function parse_data_statement(parser, arena, parent_index) &
        result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(token_t) :: token
        type(token_t) :: data_token
        integer, allocatable :: object_indices(:)
        integer, allocatable :: element_indices(:)
        integer, allocatable :: assignment_indices(:)
        integer :: initial_size
        logical :: objects_ok
        logical :: values_ok
        logical :: emit_ok
        logical :: have_assignments

        stmt_index = 0
        call reset_pending_assignments()

        data_token = parser%consume()
        call skip_trivia(parser)

        allocate (assignment_indices(0))
        have_assignments = .false.

        do
            initial_size = size(assignment_indices)

            call parse_object_list(object_indices, objects_ok)
            if (.not. objects_ok) return

            call skip_trivia(parser)
            token = parser%peek()
            if (token%kind /= TK_OPERATOR .or. trim(token%text) /= "/") then
                call parser%error("Expected '/' to begin DATA value list")
                return
            end if
            token = parser%consume()
            call skip_trivia(parser)

            call parse_value_list(element_indices, values_ok)
            if (.not. values_ok) return

            token = parser%peek()
            if (token%kind /= TK_OPERATOR .or. trim(token%text) /= "/") then
                call parser%error("Expected '/' to end DATA value list")
                return
            end if
            token = parser%consume()

            call create_data_node(object_indices, element_indices, &
                                  assignment_indices, emit_ok)
            if (.not. emit_ok) return
            if (size(assignment_indices) > initial_size) have_assignments = .true.

            call skip_trivia(parser)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. trim(token%text) == ",") then
                token = parser%consume()
                call skip_trivia(parser)
            else
                exit
            end if
        end do

        if (.not. have_assignments) then
            call parser%error("DATA statement produced no assignments")
            return
        end if

        if (size(assignment_indices) == 0) then
            call parser%error("DATA statement produced no assignments")
            return
        end if

        stmt_index = assignment_indices(1)
        call store_pending_assignments(assignment_indices)
    contains
        subroutine reset_pending_assignments()
            if (allocated(pending_data_assignment_indices)) then
                deallocate (pending_data_assignment_indices)
            end if
        end subroutine reset_pending_assignments

        subroutine store_pending_assignments(assignments)
            integer, intent(in) :: assignments(:)
            integer :: count

            if (allocated(pending_data_assignment_indices)) then
                deallocate (pending_data_assignment_indices)
            end if

            count = size(assignments)
            if (count > 1) then
                allocate (pending_data_assignment_indices(count - 1))
                pending_data_assignment_indices = assignments(2:)
            end if
        end subroutine store_pending_assignments

        subroutine parse_object_list(objects, success)
            integer, allocatable, intent(out) :: objects(:)
            logical, intent(out) :: success
            type(token_t) :: current
            integer :: object_index
            logical :: expect_object

            allocate (objects(0))
            success = .false.
            expect_object = .true.

            do
                current = parser%peek()
                if (current%kind == TK_EOF) then
                    call parser%error("Unexpected end of statement inside DATA "// &
                                      "object list")
                    return
                end if

                if (current%kind == TK_OPERATOR .and. trim(current%text) == "/") then
                    if (expect_object) then
                        call parser%error("DATA statement object list cannot be empty")
                        return
                    end if
                    exit
                end if

                object_index = parse_expression_until(parser, arena, [",", "/"])
                if (object_index <= 0) return
                call append_index(objects, object_index)
                expect_object = .false.

                call skip_trivia(parser)
                current = parser%peek()
                if (current%kind == TK_OPERATOR) then
                    select case (trim(current%text))
                    case (",")
                        current = parser%consume()
                        call skip_trivia(parser)
                        expect_object = .true.
                        cycle
                    case ("/")
                        exit
                    case default
                        call parser%error("Unexpected token in DATA object list")
                        return
                    end select
                else
                    call parser%error("Unexpected token in DATA object list")
                    return
                end if
            end do

            if (expect_object) then
                call parser%error("DATA statement object list cannot be empty")
                return
            end if

            success = .true.
        end subroutine parse_object_list

        subroutine parse_value_list(values, success)
            integer, allocatable, intent(out) :: values(:)
            logical, intent(out) :: success
            type(token_t) :: current
            integer :: value_index
            logical :: expect_value

            allocate (values(0))
            success = .false.
            expect_value = .true.

            do
                current = parser%peek()
                if (current%kind == TK_EOF) then
                    call parser%error("Unexpected end of statement inside DATA "// &
                                      "value list")
                    return
                end if

                if (current%kind == TK_OPERATOR .and. trim(current%text) == "/") then
                    if (expect_value) then
                        call parser%error("DATA statement value list cannot be empty")
                        return
                    end if
                    exit
                end if

                value_index = parse_expression_until(parser, arena, [",", "/"])
                if (value_index <= 0) return
                call expand_repeat_count(arena, value_index, values)

                call skip_trivia(parser)
                current = parser%peek()
                if (current%kind == TK_OPERATOR) then
                    select case (trim(current%text))
                    case (",")
                        current = parser%consume()
                        expect_value = .true.
                        call skip_trivia(parser)
                        cycle
                    case ("/")
                        expect_value = .false.
                    case default
                        call parser%error("Unexpected token in DATA statement "// &
                                          "value list")
                        return
                    end select
                else
                    call parser%error("Unexpected token in DATA statement value list")
                    return
                end if
            end do

            if (expect_value) then
                call parser%error("DATA statement value list cannot be empty")
                return
            end if

            success = .true.
        end subroutine parse_value_list

        subroutine create_data_node(objects, values, assignments, success)
            integer, allocatable, intent(in) :: objects(:)
            integer, allocatable, intent(in) :: values(:)
            integer, allocatable, intent(inout) :: assignments(:)
            logical, intent(out) :: success
            integer :: data_index
            integer :: node_line
            integer :: node_column
            success = .false.

            if (size(objects) == 0) then
                call parser%error("DATA statement requires at least one object")
                return
            end if

            if (size(values) == 0) then
                call parser%error("DATA statement requires at least one value")
                return
            end if

            node_line = data_token%line
            node_column = data_token%column

            if (present(parent_index)) then
                data_index = push_data_statement(arena, objects, values, &
                                                 node_line, node_column, parent_index)
            else
                data_index = push_data_statement(arena, objects, values, &
                                                 node_line, node_column)
            end if

            if (data_index <= 0) return
            call append_index(assignments, data_index)
            success = .true.
        end subroutine create_data_node

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

    function get_data_additional_indices() result(indices)
        integer, allocatable :: indices(:)

        if (allocated(pending_data_assignment_indices)) then
            call move_alloc(pending_data_assignment_indices, indices)
        else
            allocate (indices(0))
        end if
    end function get_data_additional_indices

end module parser_statement_data_module
