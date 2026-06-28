module parser_statement_data_module
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_OPERATOR, &
        TK_COMMENT, TK_WHITESPACE
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_expression_until, parse_comparison
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: binary_op_node, literal_node, &
        identifier_node
    use ast_factory, only: push_assignment, push_array_literal, &
        push_namelist_statement, push_data_statement, &
        push_io_implied_do
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

        ! Parse DATA implied-do: (object-list, var = start, end [, step])
        function try_parse_data_implied_do() result(expr_index)
            integer :: expr_index
            type(token_t) :: token
            integer :: saved_pos
            integer, allocatable :: object_exprs(:)
            integer :: value_expr_index
            integer :: start_index, end_index, step_index
            character(len=:), allocatable :: var_name
            integer :: line, column
            logical :: objects_ok

            expr_index = 0
            saved_pos = parser%current_token

            token = parser%peek()
            if (token%kind /= TK_OPERATOR .or. token%text /= "(") return
            line = token%line
            column = token%column
            token = parser%consume() ! consume '('

            objects_ok = parse_implied_do_object_list(object_exprs)
            if (.not. objects_ok) then
                parser%current_token = saved_pos
                return
            end if

            if (.not. allocated(object_exprs) .or. size(object_exprs) == 0) then
                parser%current_token = saved_pos
                return
            end if
            value_expr_index = object_exprs(1)

            call skip_trivia(parser)
            token = parser%peek()
            if (token%kind /= TK_OPERATOR .or. token%text /= ",") then
                parser%current_token = saved_pos
                return
            end if
            token = parser%consume()
            call skip_trivia(parser)

            if (.not. parse_implied_do_control(var_name, start_index, end_index, &
                step_index, saved_pos)) return

            expr_index = create_implied_do_node_helper(object_exprs, &
                value_expr_index, var_name, &
                start_index, end_index, &
                step_index, line, column)

        end function try_parse_data_implied_do

        logical function parse_implied_do_control(var_name, start_index, end_index, &
                step_index, saved_pos) result(success)
            character(len=:), allocatable, intent(out) :: var_name
            integer, intent(out) :: start_index, end_index, step_index
            integer, intent(in) :: saved_pos
            type(token_t) :: token

            success = .false.

            token = parser%peek()
            if (token%kind /= TK_IDENTIFIER) then
                parser%current_token = saved_pos
                return
            end if
            var_name = token%text
            token = parser%consume()

            token = parser%peek()
            if (token%kind /= TK_OPERATOR .or. token%text /= "=") then
                parser%current_token = saved_pos
                return
            end if
            token = parser%consume()

            start_index = parse_comparison(parser, arena)
            if (start_index <= 0) then
                parser%current_token = saved_pos
                return
            end if

            token = parser%peek()
            if (token%kind /= TK_OPERATOR .or. token%text /= ",") then
                parser%current_token = saved_pos
                return
            end if
            token = parser%consume()

            end_index = parse_comparison(parser, arena)
            if (end_index <= 0) then
                parser%current_token = saved_pos
                return
            end if

            step_index = 0
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
                step_index = parse_comparison(parser, arena)
                if (step_index <= 0) then
                    parser%current_token = saved_pos
                    return
                end if
            end if

            token = parser%peek()
            if (token%kind /= TK_OPERATOR .or. token%text /= ")") then
                parser%current_token = saved_pos
                return
            end if
            token = parser%consume()

            success = .true.
        end function parse_implied_do_control

        function create_implied_do_node_helper(object_exprs, value_expr_index, &
                var_name, start_index, end_index, &
                step_index, line, column) &
                result(node_index)
            integer, allocatable, intent(in) :: object_exprs(:)
            integer, intent(in) :: value_expr_index
            character(len=*), intent(in) :: var_name
            integer, intent(in) :: start_index, end_index, step_index
            integer, intent(in) :: line, column
            integer :: node_index

            if (step_index > 0) then
                if (allocated(object_exprs)) then
                    node_index = push_io_implied_do( &
                        arena, value_expr_index, var_name, &
                        start_expr_index=start_index, &
                        end_expr_index=end_index, &
                        step_expr_index=step_index, line=line, column=column, &
                        object_indices=object_exprs)
                else
                    node_index = push_io_implied_do( &
                        arena, value_expr_index, var_name, &
                        start_expr_index=start_index, &
                        end_expr_index=end_index, &
                        step_expr_index=step_index, line=line, column=column)
                end if
            else
                if (allocated(object_exprs)) then
                    node_index = push_io_implied_do( &
                        arena, value_expr_index, var_name, &
                        start_expr_index=start_index, &
                        end_expr_index=end_index, &
                        line=line, column=column, object_indices=object_exprs)
                else
                    node_index = push_io_implied_do( &
                        arena, value_expr_index, var_name, &
                        start_expr_index=start_index, &
                        end_expr_index=end_index, &
                        line=line, column=column)
                end if
            end if
        end function create_implied_do_node_helper

        logical function parse_implied_do_object_list(object_exprs) result(success)
            integer, allocatable, intent(out) :: object_exprs(:)
            integer :: object_index
            type(token_t) :: local_token

            success = .false.
            allocate (object_exprs(0))

            do
                call skip_trivia(parser)
                object_index = try_parse_data_implied_do()
                if (object_index == 0) then
                    object_index = parse_expression_until(parser, arena, [","])
                    if (object_index <= 0) then
                        if (allocated(object_exprs)) then
                            deallocate (object_exprs)
                        end if
                        return
                    end if
                end if
                call append_index(object_exprs, object_index)

                call skip_trivia(parser)
                if (loop_control_ahead()) then
                    success = .true.
                    return
                end if

                local_token = parser%peek()
                if (local_token%kind /= TK_OPERATOR .or. local_token%text /= ",") then
                    if (allocated(object_exprs)) then
                        deallocate (object_exprs)
                    end if
                    return
                end if
                local_token = parser%consume()
            end do
        end function parse_implied_do_object_list

        logical function loop_control_ahead() result(is_control)
            integer :: saved_pos_local
            type(token_t) :: look

            is_control = .false.
            saved_pos_local = parser%current_token

            look = parser%peek()
            if (look%kind /= TK_OPERATOR .or. look%text /= ",") then
                parser%current_token = saved_pos_local
                return
            end if
            look = parser%consume()
            call skip_trivia(parser)

            look = parser%peek()
            if (look%kind /= TK_IDENTIFIER) then
                parser%current_token = saved_pos_local
                return
            end if
            look = parser%consume()
            call skip_trivia(parser)

            look = parser%peek()
            if (look%kind == TK_OPERATOR .and. look%text == "=") then
                is_control = .true.
            end if
            parser%current_token = saved_pos_local
        end function loop_control_ahead

        subroutine parse_object_list(objects, success)
            integer, allocatable, intent(out) :: objects(:)
            logical, intent(out) :: success
            type(token_t) :: current
            integer :: object_index

            allocate (objects(0))
            success = .false.

            do
                current = parser%peek()
                if (current%kind == TK_EOF) then
                    call parser%error("Unexpected end of statement inside DATA "// &
                        "object list")
                    return
                end if

                if (current%kind == TK_OPERATOR .and. trim(current%text) == "/") then
                    if (size(objects) == 0) then
                        call parser%error("DATA statement object list cannot be empty")
                        return
                    end if
                    exit
                end if

                ! Try parsing as implied-do first if we see '('
                current = parser%peek()
                if (current%kind == TK_OPERATOR .and. trim(current%text) == "(") then
                    object_index = try_parse_data_implied_do()
                    if (object_index > 0) then
                        call append_index(objects, object_index)
                        call skip_trivia(parser)
                        current = parser%peek()
                        if (current%kind == TK_OPERATOR) then
                            select case (trim(current%text))
                            case (",")
                                current = parser%consume()
                                call skip_trivia(parser)
                                current = parser%peek()
                                if (current%kind == TK_OPERATOR .and. &
                                    trim(current%text) == "/") then
                                    ! Accept trailing comma as extension (common)
                                    ! compiler tolerance. Parser continues; codegen
                                    ! emits standard Fortran without trailing comma.
                                    exit
                                end if
                                cycle
                            case ("/")
                                exit
                            case default
                                call parser%error("Unexpected token in DATA "// &
                                    "object list")
                                return
                            end select
                        else
                            call parser%error("Unexpected token in DATA object list")
                            return
                        end if
                    end if
                end if

                ! Fall back to regular expression parsing
                object_index = parse_expression_until(parser, arena, [",", "/"])
                if (object_index <= 0) return
                call append_index(objects, object_index)

                call skip_trivia(parser)
                current = parser%peek()
                if (current%kind == TK_OPERATOR) then
                    select case (trim(current%text))
                    case (",")
                        current = parser%consume()
                        call skip_trivia(parser)
                        current = parser%peek()
                        if (current%kind == TK_OPERATOR .and. &
                            trim(current%text) == "/") then
                            ! Accept trailing comma as extension (common compiler
                            ! tolerance). Parser continues; codegen emits standard
                            ! Fortran without trailing comma.
                            exit
                        end if
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

            success = .true.
        end subroutine parse_object_list

        subroutine parse_value_list(values, success)
            integer, allocatable, intent(out) :: values(:)
            logical, intent(out) :: success
            type(token_t) :: current
            integer :: value_index

            allocate (values(0))
            success = .false.

            do
                current = parser%peek()
                if (current%kind == TK_EOF) then
                    call parser%error("Unexpected end of statement inside DATA "// &
                        "value list")
                    return
                end if

                if (current%kind == TK_OPERATOR .and. trim(current%text) == "/") then
                    if (size(values) == 0) then
                        call parser%error("DATA statement value list cannot be empty")
                        return
                    end if
                    exit
                end if

                ! Try parsing implied-do values before falling back to scalars
                if (current%kind == TK_OPERATOR .and. trim(current%text) == "(") then
                    value_index = try_parse_data_implied_do()
                    if (value_index > 0) then
                        call append_index(values, value_index)

                        call skip_trivia(parser)
                        current = parser%peek()
                        if (current%kind == TK_OPERATOR) then
                            select case (trim(current%text))
                            case (",")
                                current = parser%consume()
                                call skip_trivia(parser)
                                current = parser%peek()
                                if (current%kind == TK_OPERATOR .and. &
                                    trim(current%text) == "/") then
                                    ! Accept trailing comma as extension (common
                                    ! compiler tolerance). Parser continues; codegen
                                    ! emits standard Fortran without trailing comma.
                                    exit
                                end if
                                cycle
                            case ("/")
                                exit
                            case default
                                call parser%error("Unexpected token in DATA "// &
                                    "statement value list")
                                return
                            end select
                        else
                            call parser%error("Unexpected token in DATA "// &
                                "statement value list")
                            return
                        end if
                    end if
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
                        call skip_trivia(parser)
                        current = parser%peek()
                        if (current%kind == TK_OPERATOR .and. &
                            trim(current%text) == "/") then
                            ! Accept trailing comma as extension (common compiler
                            ! tolerance). Parser continues; codegen emits standard
                            ! Fortran without trailing comma.
                            exit
                        end if
                        cycle
                    case ("/")
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
            call append_hidden_assignments(objects, values, assignments)
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

        subroutine append_hidden_assignments(objects, values, assignments)
            integer, allocatable, intent(in) :: objects(:)
            integer, allocatable, intent(in) :: values(:)
            integer, allocatable, intent(inout) :: assignments(:)
            logical :: can_aggregate
            integer :: assign_index

            if (.not. allocated(objects)) return
            if (.not. allocated(values)) return
            if (size(objects) == 0 .or. size(values) == 0) return

            can_aggregate = size(objects) == 1 .and. size(values) > 1 .and. &
                is_plain_identifier(objects(1))
            if (can_aggregate) then
                assign_index = create_array_assignment(objects(1), values)
                if (assign_index > 0) call append_index(assignments, assign_index)
            else
                call append_scalar_assignments(objects, values, assignments)
            end if
        end subroutine append_hidden_assignments

        logical function is_plain_identifier(node_index) result(is_ident)
            integer, intent(in) :: node_index

            is_ident = .false.
            if (.not. arena%has_node_at(node_index)) return

            select type (obj_node => arena%entries(node_index)%node)
                type is (identifier_node)
                is_ident = .true.
            class default
                is_ident = .false.
            end select
        end function is_plain_identifier

        integer function create_array_assignment(target_index, values) &
                result(assign_index)
            integer, intent(in) :: target_index
            integer, allocatable, intent(in) :: values(:)
            integer :: array_index

            assign_index = 0
            if (.not. allocated(values)) return
            if (size(values) == 0) return

            if (present(parent_index)) then
                array_index = push_array_literal(arena, values, data_token%line, &
                    data_token%column, parent_index)
            else
                array_index = push_array_literal(arena, values, data_token%line, &
                    data_token%column)
            end if
            if (array_index <= 0) return

            if (present(parent_index)) then
                assign_index = push_assignment(arena, target_index, array_index, &
                    data_token%line, data_token%column, &
                    parent_index, suppress_codegen=.true.)
            else
                assign_index = push_assignment(arena, target_index, array_index, &
                    data_token%line, data_token%column, &
                    suppress_codegen=.true.)
            end if
        end function create_array_assignment

        subroutine append_scalar_assignments(objects, values, assignments)
            integer, allocatable, intent(in) :: objects(:)
            integer, allocatable, intent(in) :: values(:)
            integer, allocatable, intent(inout) :: assignments(:)
            integer :: value_pos
            integer :: obj_idx
            integer :: assign_index

            if (.not. allocated(objects)) return
            if (.not. allocated(values)) return
            if (size(objects) == 0 .or. size(values) == 0) return

            value_pos = 1
            do while (value_pos <= size(values))
                do obj_idx = 1, size(objects)
                    if (value_pos > size(values)) exit
                    assign_index = create_scalar_assignment(objects(obj_idx), &
                        values(value_pos))
                    if (assign_index > 0) call append_index(assignments, assign_index)
                    value_pos = value_pos + 1
                end do
            end do
        end subroutine append_scalar_assignments

        integer function create_scalar_assignment(target_index, value_index) &
                result(assign_index)
            integer, intent(in) :: target_index
            integer, intent(in) :: value_index

            assign_index = 0
            if (target_index <= 0) return
            if (value_index <= 0) return

            if (present(parent_index)) then
                assign_index = push_assignment(arena, target_index, value_index, &
                    data_token%line, data_token%column, &
                    parent_index, suppress_codegen=.true.)
            else
                assign_index = push_assignment(arena, target_index, value_index, &
                    data_token%line, data_token%column, &
                    suppress_codegen=.true.)
            end if
        end function create_scalar_assignment

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
