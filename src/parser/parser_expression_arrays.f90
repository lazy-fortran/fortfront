module parser_expression_arrays_module
    use lexer_core, only: token_t, TK_OPERATOR, TK_IDENTIFIER, TK_NEWLINE, TK_COMMENT
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: component_access_node, identifier_node
    use ast_factory, only: push_array_literal, push_do_loop, &
                           push_call_or_subscript_with_slice_detection
    implicit none
    private

    abstract interface
        integer function parse_expr_proc(parser, arena)
            import :: parser_state_t, ast_arena_t
            type(parser_state_t), intent(inout) :: parser
            type(ast_arena_t), intent(inout) :: arena
        end function parse_expr_proc
    end interface

    type :: array_parse_helpers_t
        procedure(parse_expr_proc), pointer, nopass :: parse_comparison => null()
        procedure(parse_expr_proc), pointer, nopass :: parse_unary => null()
        procedure(parse_expr_proc), pointer, nopass :: parse_logical_eqv => null()
        procedure(parse_expr_proc), pointer, nopass :: parse_range => null()
    end type array_parse_helpers_t

    public :: array_parse_helpers_t
    public :: parse_modern_array_literal
    public :: parse_legacy_array_literal
    public :: parse_array_indexing_postfix
    public :: parse_square_indexing_postfix

contains

    function parse_simple_array_elements(parser, arena, terminator, style, &
                                         start_token, helpers) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: terminator
        character(len=*), intent(in) :: style
        type(token_t), intent(in) :: start_token
        type(array_parse_helpers_t), intent(in) :: helpers
        integer :: expr_index
        integer, allocatable :: temp_indices(:)
        integer, allocatable :: element_indices(:)
        integer :: element_count
        type(token_t) :: current
        integer :: value_index

        if (.not. associated(helpers%parse_comparison) .or. &
            .not. associated(helpers%parse_unary)) then
            expr_index = 0
            return
        end if

        element_count = 0
        allocate (temp_indices(20))

        do
            element_count = element_count + 1
            if (element_count > size(temp_indices)) then
                block
                    integer, allocatable :: new_indices(:)
                    allocate (new_indices(size(temp_indices) + 20))
                    new_indices(1:size(temp_indices)) = temp_indices
                    call move_alloc(new_indices, temp_indices)
                end block
            end if

            if (style == "modern") then
                value_index = helpers%parse_comparison(parser, arena)
            else
                value_index = helpers%parse_unary(parser, arena)
            end if

            if (value_index <= 0) then
                expr_index = 0
                return
            end if
            temp_indices(element_count) = value_index

            current = parser%peek()
            if (current%text == ",") then
                current = parser%consume()
            else if (current%text == terminator) then
                current = parser%consume()
                exit
            else
                expr_index = 0
                return
            end if
        end do

        allocate (element_indices(element_count))
        element_indices = temp_indices(1:element_count)
        expr_index = push_array_literal(arena, element_indices, &
                                        start_token%line, start_token%column, &
                                        syntax_style=style)
    end function parse_simple_array_elements

    function parse_legacy_array_literal(parser, arena, helpers) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(array_parse_helpers_t), intent(in) :: helpers
        integer :: expr_index

        expr_index = 0
        if (.not. associated(helpers%parse_comparison) .or. &
            .not. associated(helpers%parse_unary)) return

        block
            type(token_t) :: paren_token
            type(token_t) :: current
            integer, allocatable :: element_indices(:)

            paren_token = parser%consume()
            current = parser%consume()
            current = parser%peek()

            if (current%text == "/") then
                current = parser%consume()
                current = parser%peek()
                if (current%text == ")") then
                    current = parser%consume()
                    allocate (element_indices(0))
                    expr_index = push_array_literal(arena, element_indices, &
                                                    paren_token%line, &
                                                    paren_token%column, &
                                                    syntax_style="legacy")
                end if
                return
            end if

            expr_index = parse_simple_array_elements(parser, arena, "/", "legacy", &
                                                     paren_token, helpers)
            if (expr_index <= 0) return

            current = parser%peek()
            if (current%text /= ")") then
                expr_index = 0
                return
            end if
            current = parser%consume()
        end block
    end function parse_legacy_array_literal

    function parse_modern_array_literal(parser, arena, start_token, helpers) &
        result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: start_token
        type(array_parse_helpers_t), intent(in) :: helpers
        integer :: expr_index
        integer, allocatable :: temp_indices(:)
        integer, allocatable :: element_indices(:)
        integer :: element_count
        type(token_t) :: current
        type(token_t) :: peek_token
        integer :: saved_pos

        expr_index = 0
        if (.not. associated(helpers%parse_comparison) .or. &
            .not. associated(helpers%parse_unary)) return

        element_count = 0
        allocate (temp_indices(20))

        do
            peek_token = parser%peek()
            if (peek_token%text == "]") then
                current = parser%consume()  ! Consume the closing bracket
                exit
            else if (peek_token%text == "(") then
                saved_pos = parser%current_token
                expr_index = parse_implied_do_constructor(parser, arena, start_token, &
                                                          helpers)
                if (expr_index > 0) return
                parser%current_token = saved_pos
                expr_index = 0
            end if

            expr_index = helpers%parse_comparison(parser, arena)
            if (expr_index <= 0) return

            element_count = element_count + 1
            if (element_count > size(temp_indices)) then
                block
                    integer, allocatable :: new_indices(:)
                    allocate (new_indices(size(temp_indices) + 20))
                    new_indices(1:size(temp_indices)) = temp_indices
                    call move_alloc(new_indices, temp_indices)
                end block
            end if
            temp_indices(element_count) = expr_index

            expr_index = try_parse_implied_do_loop(parser, arena, temp_indices, &
                                                   element_count, start_token)
            if (expr_index > 0) exit

            current = parser%peek()
            if (current%text == ",") then
                current = parser%consume()
            else if (current%text == "]") then
                current = parser%consume()  ! Consume the closing bracket
                exit
            else
                expr_index = 0
                return
            end if
        end do

        if (expr_index > 0) return
        allocate (element_indices(element_count))
        element_indices = temp_indices(1:element_count)
        expr_index = push_array_literal(arena, element_indices, start_token%line, &
                                        start_token%column, syntax_style="modern")
    end function parse_modern_array_literal

    logical function parse_implied_do_header(parser, arena, expr_elem_index, &
                                             var_name, start_index, end_index, &
                                             step_index, helpers) result(success)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: expr_elem_index
        character(len=:), allocatable, intent(out) :: var_name
        integer, intent(out) :: start_index
        integer, intent(out) :: end_index
        integer, intent(out) :: step_index
        type(array_parse_helpers_t), intent(in) :: helpers
        type(token_t) :: current

        success = .false.
        if (.not. associated(helpers%parse_comparison)) return

        current = parser%consume()
        expr_elem_index = helpers%parse_comparison(parser, arena)

        current = parser%peek()
        if (current%text /= ",") return
        current = parser%consume()

        current = parser%peek()
        if (current%kind /= TK_IDENTIFIER) return
        var_name = current%text
        current = parser%consume()

        current = parser%peek()
        if (current%text /= "=") return
        current = parser%consume()

        start_index = helpers%parse_comparison(parser, arena)

        current = parser%peek()
        if (current%text /= ",") return
        current = parser%consume()

        end_index = helpers%parse_comparison(parser, arena)

        step_index = 0
        current = parser%peek()
        if (current%text == ",") then
            current = parser%consume()
            step_index = helpers%parse_comparison(parser, arena)
        end if

        success = .true.
    end function parse_implied_do_header

    logical function consume_implied_do_closers(parser) result(success)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: current

        success = .false.
        current = parser%peek()
        if (current%text /= ")") return
        current = parser%consume()

        current = parser%peek()
        if (current%text /= "]") return
        current = parser%consume()

        success = .true.
    end function consume_implied_do_closers

    integer function build_implied_do_node(arena, bracket_token, expr_elem_index, &
                                           var_name, start_index, end_index, &
                                           step_index) result(expr_index)
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: bracket_token
        integer, intent(in) :: expr_elem_index
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: start_index
        integer, intent(in) :: end_index
        integer, intent(in) :: step_index
        integer :: do_index
        integer, allocatable :: body_indices(:)
        integer, allocatable :: element_indices(:)

        allocate (body_indices(1))
        body_indices(1) = expr_elem_index

        do_index = push_do_loop(arena, var_name, start_index, end_index, step_index, &
                                body_indices, "", bracket_token%line, &
                                bracket_token%column)

        allocate (element_indices(1))
        element_indices(1) = do_index
        expr_index = push_array_literal(arena, element_indices, &
                                        bracket_token%line, bracket_token%column, &
                                        syntax_style="implied_do")
    end function build_implied_do_node

    function parse_implied_do_constructor(parser, arena, bracket_token, helpers) &
        result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: bracket_token
        type(array_parse_helpers_t), intent(in) :: helpers
        integer :: expr_index
        integer :: expr_elem_index
        integer :: start_index
        integer :: end_index
        integer :: step_index
        character(len=:), allocatable :: var_name

        expr_index = 0
        if (.not. parse_implied_do_header(parser, arena, expr_elem_index, var_name, &
                                          start_index, end_index, step_index, &
                                          helpers)) return
        if (.not. consume_implied_do_closers(parser)) return

        expr_index = build_implied_do_node(arena, bracket_token, expr_elem_index, &
                                           var_name, start_index, end_index, &
                                           step_index)
    end function parse_implied_do_constructor

    function try_parse_implied_do_loop(parser, arena, temp_indices, element_count, &
                                       bracket_token) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: temp_indices(:)
        integer, intent(in) :: element_count
        type(token_t), intent(in) :: bracket_token
        integer :: expr_index
        type(token_t) :: next1
        type(token_t) :: next2
        integer :: saved_pos

        next1 = parser%peek()
        if (next1%kind == TK_IDENTIFIER) then
            saved_pos = parser%current_token
            next1 = parser%consume()
            next2 = parser%peek()

            if (next2%kind == TK_OPERATOR .and. next2%text == "=") then
                block
                    integer, allocatable :: element_indices(:)
                    allocate (element_indices(element_count))
                    element_indices = temp_indices(1:element_count)
                    expr_index = push_array_literal(arena, element_indices, &
                                                    bracket_token%line, &
                                                    bracket_token%column, &
                                                    syntax_style="modern")
                end block
                return
            else
                parser%current_token = saved_pos
            end if
        end if

        expr_index = 0
    end function try_parse_implied_do_loop

    function parse_array_indexing_postfix(parser, arena, base_expr, helpers) &
        result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: base_expr
        type(array_parse_helpers_t), intent(in) :: helpers
        integer :: expr_index
        expr_index = apply_index_postfix(parser, arena, base_expr, helpers, ")")
    end function parse_array_indexing_postfix

    function parse_square_indexing_postfix(parser, arena, base_expr, helpers) &
        result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: base_expr
        type(array_parse_helpers_t), intent(in) :: helpers
        integer :: expr_index
        expr_index = apply_index_postfix(parser, arena, base_expr, helpers, "]")
    end function parse_square_indexing_postfix

    function apply_index_postfix(parser, arena, base_expr, helpers, closing_char) &
        result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: base_expr
        type(array_parse_helpers_t), intent(in) :: helpers
        character(len=*), intent(in) :: closing_char
        integer :: expr_index
        type(token_t) :: open_token, close_token
        integer, allocatable :: arg_indices(:)
        character(len=:), allocatable :: call_name

        expr_index = base_expr
        if (.not. associated(helpers%parse_range)) return
        open_token = parser%consume()

        call collect_index_arguments(parser, arena, helpers, closing_char, &
                                     arg_indices, close_token)
        if (.not. allocated(arg_indices)) return

        call extract_target_name(arena, expr_index, call_name)
        if (.not. allocated(call_name)) return

        expr_index = push_call_or_subscript_with_slice_detection( &
                     arena, call_name, arg_indices, close_token%line, &
                     close_token%column)
    end function apply_index_postfix

    subroutine collect_index_arguments(parser, arena, helpers, closing_char, &
                                       arg_indices, close_token)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(array_parse_helpers_t), intent(in) :: helpers
        character(len=*), intent(in) :: closing_char
        integer, allocatable, intent(out) :: arg_indices(:)
        type(token_t), intent(out) :: close_token
        type(token_t) :: token
        integer :: arg_index

        if (parser%is_at_end()) then
            close_token = parser%peek()
            return
        end if

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= closing_char) then
            arg_index = helpers%parse_range(parser, arena)
            if (arg_index > 0) then
                allocate (arg_indices(1))
                arg_indices(1) = arg_index

                do
                    token = parser%peek()
                    if (token%kind /= TK_OPERATOR .or. token%text /= ",") exit
                    token = parser%consume()
                    arg_index = helpers%parse_range(parser, arena)
                    if (arg_index > 0) then
                        arg_indices = [arg_indices, arg_index]
                    else
                        exit
                    end if
                end do
            end if
        end if

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == closing_char) then
            close_token = parser%consume()
        else
            close_token = token
        end if

        if (.not. allocated(arg_indices)) then
            allocate (arg_indices(0))
        end if
    end subroutine collect_index_arguments

    subroutine extract_target_name(arena, expr_index, call_name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=:), allocatable, intent(out) :: call_name

        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return

        select type (node => arena%entries(expr_index)%node)
        type is (component_access_node)
            if (allocated(node%component_name)) then
                call_name = node%component_name
            end if
        type is (identifier_node)
            if (allocated(node%name)) then
                call_name = node%name
            end if
        end select
    end subroutine extract_target_name

end module parser_expression_arrays_module
