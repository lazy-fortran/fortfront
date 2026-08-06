module parser_expression_arrays_module
    use lexer_core, only: token_t, TK_OPERATOR, TK_IDENTIFIER, TK_KEYWORD, &
        TK_NEWLINE, TK_COMMENT
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: call_or_subscript_node, component_access_node, &
        identifier_node
    use ast_factory, only: push_array_literal, push_do_loop, &
        push_call_or_subscript_with_slice_detection
    implicit none
    private

    abstract interface
        recursive integer function parse_expr_proc(parser, arena)
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
            start_token, helpers, type_spec) &
            result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: terminator
        character(len=*), intent(in) :: style
        type(token_t), intent(in) :: start_token
        type(array_parse_helpers_t), intent(in) :: helpers
        character(len=*), intent(in), optional :: type_spec
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

        do while (element_count < 100000)
            element_count = element_count + 1
            if (element_count > size(temp_indices)) then
                block
                    integer, allocatable :: new_indices(:)
                    allocate (new_indices(size(temp_indices) + 20))
                    new_indices(1:size(temp_indices)) = temp_indices
                    call move_alloc(new_indices, temp_indices)
                end block
            end if

            ! Use parse_comparison for both modern and legacy styles to support
            ! binary expressions like 2.0*x in array constructors
            value_index = helpers%parse_comparison(parser, arena)

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
        if (present(type_spec)) then
            expr_index = push_array_literal(arena, element_indices, &
                start_token%line, start_token%column, &
                syntax_style=style, &
                type_spec=type_spec)
        else
            expr_index = push_array_literal(arena, element_indices, &
                start_token%line, start_token%column, &
                syntax_style=style)
        end if
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
            integer :: saved_pos
            character(len=:), allocatable :: type_spec_text
            logical :: has_type_spec

            paren_token = parser%consume()
            current = parser%consume()
            current = parser%peek()
            type_spec_text = ""
            has_type_spec = .false.

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

            ! Check for type specification: (/ type-spec :: elements /)
            ! e.g., (/ real :: 1, 2, 3 /) or (/ integer(kind=4) :: 1, 2 /)
            if (current%kind == TK_IDENTIFIER .or. current%kind == TK_KEYWORD) then
                saved_pos = parser%current_token
                block
                    type(token_t) :: spec_token
                    integer :: paren_depth

                    spec_token = parser%consume()
                    type_spec_text = spec_token%text
                    current = parser%peek()

                    if (current%text == "(") then
                        spec_token = parser%consume()
                        type_spec_text = trim(type_spec_text) // spec_token%text
                        paren_depth = 1
                        do while (paren_depth > 0)
                            current = parser%peek()
                            if (current%text == "(") then
                                paren_depth = paren_depth + 1
                            else if (current%text == ")") then
                                paren_depth = paren_depth - 1
                            end if
                            spec_token = parser%consume()
                            type_spec_text = type_spec_text // spec_token%text
                        end do
                        current = parser%peek()
                    end if

                    if (current%text == "::") then
                        spec_token = parser%consume()
                        has_type_spec = .true.
                        type_spec_text = trim(adjustl(type_spec_text))
                        current = parser%peek()
                    else
                        parser%current_token = saved_pos
                        type_spec_text = ""
                        has_type_spec = .false.
                        current = parser%peek()
                    end if
                end block
            end if

            if (current%text == "(") then
                if (is_malformed_parenthesized_item(parser)) then
                    expr_index = 0
                    return
                end if
                saved_pos = parser%current_token
                if (has_type_spec) then
                    expr_index = parse_legacy_implied_do_constructor(parser, arena, &
                        paren_token, &
                        helpers, &
                        type_spec_text)
                else
                    expr_index = parse_legacy_implied_do_constructor(parser, arena, &
                        paren_token, &
                        helpers)
                end if
                if (expr_index > 0) return
                parser%current_token = saved_pos
                expr_index = 0
            end if

            if (has_type_spec) then
                expr_index = parse_simple_array_elements(parser, arena, "/", &
                    "legacy", paren_token, &
                    helpers, type_spec_text)
            else
                expr_index = parse_simple_array_elements(parser, arena, "/", &
                    "legacy", paren_token, &
                    helpers)
            end if
            if (expr_index <= 0) return

            current = parser%peek()
            if (current%text /= ")") then
                expr_index = 0
                return
            end if
            current = parser%consume()
        end block
    end function parse_legacy_array_literal

    recursive function parse_modern_array_literal(parser, arena, start_token, &
            helpers) result(expr_index)
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
        type(token_t) :: spec_token
        integer :: paren_depth
        character(len=:), allocatable :: type_spec_text
        logical :: has_type_spec

        expr_index = 0
        if (.not. associated(helpers%parse_comparison) .or. &
            .not. associated(helpers%parse_unary)) return

        element_count = 0
        allocate (temp_indices(20))
        type_spec_text = ""
        has_type_spec = .false.

        do
            peek_token = parser%peek()
            if (peek_token%kind /= TK_NEWLINE .and. peek_token%kind /= &
                TK_COMMENT) exit
            current = parser%consume()
        end do

        peek_token = parser%peek()
        if (peek_token%kind == TK_IDENTIFIER .or. &
            peek_token%kind == TK_KEYWORD) then
            saved_pos = parser%current_token
            spec_token = parser%consume()
            type_spec_text = spec_token%text
            peek_token = parser%peek()

            if (peek_token%text == "(") then
                spec_token = parser%consume()
                type_spec_text = trim(type_spec_text) // spec_token%text
                paren_depth = 1
                block
                    integer :: paren_count
                    integer, parameter :: MAX_PAREN_DEPTH = 1000
                    paren_count = 0
                    do while (paren_depth > 0 .and. paren_count < MAX_PAREN_DEPTH)
                        paren_count = paren_count + 1
                        peek_token = parser%peek()
                        if (peek_token%text == "(") then
                            paren_depth = paren_depth + 1
                        else if (peek_token%text == ")") then
                            paren_depth = paren_depth - 1
                        end if
                        spec_token = parser%consume()
                        type_spec_text = type_spec_text // spec_token%text
                    end do
                end block
                peek_token = parser%peek()
            end if

            if (peek_token%text == "::") then
                current = parser%consume()
                has_type_spec = .true.
                type_spec_text = trim(adjustl(type_spec_text))
                do
                    peek_token = parser%peek()
                    if (peek_token%kind /= TK_NEWLINE .and. &
                        peek_token%kind /= TK_COMMENT) exit
                    current = parser%consume()
                end do
            else
                parser%current_token = saved_pos
                type_spec_text = ""
                has_type_spec = .false.
            end if
        end if

        do while (element_count < 100000)
            ! Skip newlines and comments inside array literals
            do
                peek_token = parser%peek()
                if (peek_token%kind /= TK_NEWLINE .and. peek_token%kind /= &
                    TK_COMMENT) &
                    exit
                current = parser%consume()
            end do

            peek_token = parser%peek()
            if (peek_token%text == "]") then
                current = parser%consume() ! Consume the closing bracket
                exit
            else if (peek_token%text == "(") then
                if (is_malformed_parenthesized_item(parser)) then
                    expr_index = 0
                    return
                end if
                saved_pos = parser%current_token
                if (has_type_spec) then
                    expr_index = parse_implied_do_constructor(parser, arena, &
                        start_token, &
                        helpers, &
                        type_spec_text)
                else
                    expr_index = parse_implied_do_constructor(parser, arena, &
                        start_token, &
                        helpers)
                end if
                if (expr_index > 0) return
                parser%current_token = saved_pos
                expr_index = 0
            else if (peek_token%text == "[") then
                current = parser%consume()
                expr_index = parse_modern_array_literal(parser, arena, &
                    current, helpers)
                if (expr_index <= 0) return
            end if

            if (expr_index <= 0) then
                expr_index = helpers%parse_comparison(parser, arena)
                if (expr_index <= 0) return
            end if

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

            if (has_type_spec) then
                expr_index = try_parse_implied_do_loop(parser, arena, &
                    temp_indices, &
                    element_count, &
                    start_token, &
                    type_spec_text)
            else
                expr_index = try_parse_implied_do_loop(parser, arena, &
                    temp_indices, &
                    element_count, &
                    start_token)
            end if
            if (expr_index > 0) exit

            current = parser%peek()
            if (current%text == ",") then
                current = parser%consume()
                ! Skip newlines and comments after comma
                do
                    peek_token = parser%peek()
                    if (peek_token%kind /= TK_NEWLINE .and. &
                        peek_token%kind /= TK_COMMENT) exit
                    current = parser%consume()
                end do
            else if (current%text == "]") then
                current = parser%consume() ! Consume the closing bracket
                exit
            else
                expr_index = 0
                return
            end if
        end do

        if (expr_index > 0) return
        allocate (element_indices(element_count))
        element_indices = temp_indices(1:element_count)
        if (has_type_spec) then
            expr_index = push_array_literal(arena, element_indices, &
                start_token%line, &
                start_token%column, &
                syntax_style="modern", &
                type_spec=type_spec_text)
        else
            expr_index = push_array_literal(arena, element_indices, &
                start_token%line, &
                start_token%column, &
                syntax_style="modern")
        end if
    end function parse_modern_array_literal

    ! F2018 R770: an ac-value that starts with "(" is either a parenthesised
    ! expression, a complex literal (exactly two items), or an ac-implied-do
    ! (which carries "=" for its do-variable). A parenthesised list of three or
    ! more items without "=" matches none of those, so reject it here instead of
    ! letting the fallback expression parser swallow part of it.
    logical function is_malformed_parenthesized_item(parser) result(malformed)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: open_paren
        type(token_t) :: token
        integer :: pos, depth, item_count
        logical :: has_equals

        malformed = .false.
        open_paren = parser%peek()
        if (open_paren%kind /= TK_OPERATOR) return
        if (open_paren%text /= "(") return

        ! An old-style array constructor `(/ ... /)` nested as an ac-value
        ! opens with the same "(" but is not a parenthesised item.
        token = parser%get_token_at_index(parser%current_token + 1)
        if (token%kind == TK_OPERATOR) then
            if (token%text == "/") return
        end if

        pos = parser%current_token + 1
        depth = 1
        item_count = 1
        has_equals = .false.

        do while (pos <= parser%get_token_count())
            token = parser%get_token_at_index(pos)
            if (token%kind == TK_OPERATOR) then
                select case (token%text)
                case ("(", "[")
                    depth = depth + 1
                case (")", "]")
                    depth = depth - 1
                    if (depth == 0) exit
                case (",")
                    if (depth == 1) item_count = item_count + 1
                case ("=")
                    if (depth == 1) has_equals = .true.
                end select
            end if
            pos = pos + 1
        end do

        if (depth /= 0) return
        if (has_equals) return
        if (item_count < 3) return

        malformed = .true.
        call parser%error_at_token( &
            "Syntax error in COMPLEX constant: a parenthesised array "// &
            "constructor item needs exactly two parts, or an implied-do "// &
            "with a do-variable", open_paren)
    end function is_malformed_parenthesized_item

    recursive function parse_nested_implied_do(parser, arena, helpers) &
            result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(array_parse_helpers_t), intent(in) :: helpers
        integer :: expr_index
        type(token_t) :: current
        type(token_t) :: peek_token
        type(token_t) :: lookahead_token
        integer :: expr_elem_index
        character(len=:), allocatable :: var_name
        integer :: start_index, end_index, step_index
        integer, allocatable :: body_indices(:)
        integer :: saved_pos
        logical :: is_legacy_array

        expr_index = 0

        current = parser%peek()
        if (current%text /= "(") return
        current = parser%consume()

        saved_pos = parser%current_token
        peek_token = parser%peek()
        ! Check if this is a nested implied-do or a legacy array constructor.
        ! A legacy array starts with (/ so we should NOT try to recurse into it.
        is_legacy_array = .false.
        if (peek_token%text == "(") then
            lookahead_token = parser%get_token_at_index(parser%current_token + 1)
            if (lookahead_token%text == "/") then
                is_legacy_array = .true.
            end if
        end if

        if (peek_token%text == "(" .and. .not. is_legacy_array) then
            expr_elem_index = parse_nested_implied_do(parser, arena, helpers)
            if (expr_elem_index <= 0) then
                parser%current_token = saved_pos
                expr_elem_index = helpers%parse_comparison(parser, arena)
            end if
        else
            expr_elem_index = helpers%parse_comparison(parser, arena)
        end if
        if (expr_elem_index <= 0) return

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
        if (start_index <= 0) return

        current = parser%peek()
        if (current%text /= ",") return
        current = parser%consume()

        end_index = helpers%parse_comparison(parser, arena)
        if (end_index <= 0) return

        step_index = 0
        current = parser%peek()
        if (current%text == ",") then
            current = parser%consume()
            step_index = helpers%parse_comparison(parser, arena)
            if (step_index <= 0) return
        end if

        current = parser%peek()
        if (current%text /= ")") return
        current = parser%consume()

        allocate (body_indices(1))
        body_indices(1) = expr_elem_index

        expr_index = push_do_loop(arena, var_name, start_index, end_index, &
            step_index=step_index, body_indices=body_indices, &
            line=0, column=0)
    end function parse_nested_implied_do

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
        type(token_t) :: peek_ahead
        type(token_t) :: lookahead_token
        integer :: saved_pos
        logical :: is_legacy_array

        success = .false.
        if (.not. associated(helpers%parse_comparison)) return

        current = parser%consume()

        peek_ahead = parser%peek()
        ! Check if this is a nested implied-do or a legacy array constructor.
        ! A legacy array starts with (/ so we should NOT try to recurse into it.
        is_legacy_array = .false.
        if (peek_ahead%text == "(") then
            lookahead_token = parser%get_token_at_index(parser%current_token + 1)
            if (lookahead_token%text == "/") then
                is_legacy_array = .true.
            end if
        end if

        if (peek_ahead%text == "(" .and. .not. is_legacy_array) then
            saved_pos = parser%current_token
            expr_elem_index = parse_nested_implied_do(parser, arena, helpers)
            if (expr_elem_index <= 0) then
                parser%current_token = saved_pos
                expr_elem_index = helpers%parse_comparison(parser, arena)
            end if
        else
            expr_elem_index = helpers%parse_comparison(parser, arena)
        end if

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

        ! Past "name =" the construct can only be an implied-DO control
        ! (F2023 R777 ac-implied-do-control), so a malformed remainder is a
        ! syntax error in the iterator rather than a speculative parse failure.
        start_index = helpers%parse_comparison(parser, arena)
        if (start_index <= 0) then
            call parser%error("Syntax error in iterator: expected an initial "// &
                "bound expression")
            return
        end if

        current = parser%peek()
        if (current%text /= ",") then
            call parser%error_at_token("Syntax error in iterator: expected "// &
                "',' after the initial bound", current, &
                suggestion="write the iterator as (expr, var = start, end)")
            return
        end if
        current = parser%consume()

        end_index = helpers%parse_comparison(parser, arena)
        if (end_index <= 0) then
            call parser%error("Syntax error in iterator: expected a terminal "// &
                "bound expression")
            return
        end if

        step_index = 0
        current = parser%peek()
        if (current%text == ",") then
            current = parser%consume()
            step_index = helpers%parse_comparison(parser, arena)
            if (step_index <= 0) then
                call parser%error("Syntax error in iterator: expected a "// &
                    "stride expression")
                return
            end if
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
            step_index, type_spec) result(expr_index)
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: bracket_token
        integer, intent(in) :: expr_elem_index
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: start_index
        integer, intent(in) :: end_index
        integer, intent(in) :: step_index
        character(len=*), intent(in), optional :: type_spec
        integer :: do_index
        integer, allocatable :: body_indices(:)
        integer, allocatable :: element_indices(:)

        allocate (body_indices(1))
        body_indices(1) = expr_elem_index

        do_index = push_do_loop(arena, var_name, start_index, end_index, &
            step_index=step_index, body_indices=body_indices, &
            line=bracket_token%line, column=bracket_token%column)

        allocate (element_indices(1))
        element_indices(1) = do_index
        if (present(type_spec)) then
            expr_index = push_array_literal(arena, element_indices, &
                bracket_token%line, bracket_token%column, &
                syntax_style="implied_do", &
                type_spec=type_spec)
        else
            expr_index = push_array_literal(arena, element_indices, &
                bracket_token%line, bracket_token%column, &
                syntax_style="implied_do")
        end if
    end function build_implied_do_node

    function parse_implied_do_constructor(parser, arena, bracket_token, helpers, &
            type_spec) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: bracket_token
        type(array_parse_helpers_t), intent(in) :: helpers
        character(len=*), intent(in), optional :: type_spec
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

        if (present(type_spec)) then
            expr_index = build_implied_do_node(arena, bracket_token, &
                expr_elem_index, var_name, &
                start_index, end_index, &
                step_index, type_spec)
        else
            expr_index = build_implied_do_node(arena, bracket_token, &
                expr_elem_index, var_name, &
                start_index, end_index, &
                step_index)
        end if
    end function parse_implied_do_constructor

    function parse_legacy_implied_do_constructor(parser, arena, paren_token, &
            helpers, type_spec) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: paren_token
        type(array_parse_helpers_t), intent(in) :: helpers
        character(len=*), intent(in), optional :: type_spec
        integer :: expr_index
        integer :: expr_elem_index
        integer :: start_index
        integer :: end_index
        integer :: step_index
        character(len=:), allocatable :: var_name
        type(token_t) :: current

        expr_index = 0
        if (.not. parse_implied_do_header(parser, arena, expr_elem_index, var_name, &
            start_index, end_index, step_index, &
            helpers)) return

        current = parser%peek()
        if (current%text /= ")") return
        current = parser%consume()

        current = parser%peek()
        if (current%text /= "/") return
        current = parser%consume()

        current = parser%peek()
        if (current%text /= ")") return
        current = parser%consume()

        if (present(type_spec)) then
            expr_index = build_implied_do_node(arena, paren_token, &
                expr_elem_index, var_name, &
                start_index, end_index, &
                step_index, type_spec)
        else
            expr_index = build_implied_do_node(arena, paren_token, &
                expr_elem_index, var_name, &
                start_index, end_index, &
                step_index)
        end if
    end function parse_legacy_implied_do_constructor

    function try_parse_implied_do_loop(parser, arena, temp_indices, element_count, &
            bracket_token, type_spec) result(expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: temp_indices(:)
        integer, intent(in) :: element_count
        type(token_t), intent(in) :: bracket_token
        character(len=*), intent(in), optional :: type_spec
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
                    if (present(type_spec)) then
                        expr_index = push_array_literal(arena, element_indices, &
                            bracket_token%line, &
                            bracket_token%column, &
                            syntax_style="modern", &
                            type_spec=type_spec)
                    else
                        expr_index = push_array_literal(arena, element_indices, &
                            bracket_token%line, &
                            bracket_token%column, &
                            syntax_style="modern")
                    end if
                end block
                return
            else
                parser%current_token = saved_pos
            end if
        end if

        expr_index = 0
    end function try_parse_implied_do_loop

    recursive function parse_array_indexing_postfix(parser, arena, base_expr, helpers) &
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

    recursive function apply_index_postfix(parser, arena, base_expr, helpers, closing_char) &
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
        integer :: base_expr_for_call

        expr_index = base_expr
        if (.not. associated(helpers%parse_range)) return
        open_token = parser%consume()

        call collect_index_arguments(parser, arena, helpers, closing_char, &
            arg_indices, close_token)
        if (.not. allocated(arg_indices)) return

        call extract_target_name(arena, expr_index, call_name)
        if (.not. allocated(call_name)) return

        ! Preserve a compound designator when this postfix contains a range.
        ! In particular, c(2)(1:3) must keep c(2) as the slice base instead of
        ! flattening back to the spelling c; otherwise the range is orphaned
        ! and the frontend silently lowers the whole array element. A plain
        ! identifier/literal remains a normal call/subscript: ffc uses
        ! base_expr_index for derived-component access, so setting it on c(1)
        ! would misclassify an ordinary character-array element.
        base_expr_for_call = 0
        if (base_expr > 0 .and. base_expr <= arena%size) then
            if (allocated(arena%entries(base_expr)%node)) then
                select type (base_node => arena%entries(base_expr)%node)
                    type is (component_access_node)
                    base_expr_for_call = base_expr
                    type is (call_or_subscript_node)
                    base_expr_for_call = base_expr
                end select
            end if
        end if

        expr_index = push_call_or_subscript_with_slice_detection( &
            arena, call_name, arg_indices, close_token%line, &
            close_token%column, base_expr_index=base_expr_for_call)
    end function apply_index_postfix

    recursive subroutine collect_index_arguments(parser, arena, helpers, closing_char, &
            arg_indices, close_token)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(array_parse_helpers_t), intent(in) :: helpers
        character(len=*), intent(in) :: closing_char
        integer, allocatable, intent(out) :: arg_indices(:)
        type(token_t), intent(out) :: close_token
        type(token_t) :: token
        integer :: arg_index
        integer :: arg_count
        integer, parameter :: MAX_ARGUMENTS = 10000

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
                arg_count = 1

                do while (arg_count < MAX_ARGUMENTS)
                    arg_count = arg_count + 1
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

        if (.not. arena%has_node_at(expr_index)) return

        select type (node => arena%entries(expr_index)%node)
            type is (call_or_subscript_node)
            if (allocated(node%name)) then
                call_name = node%name
            end if
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
