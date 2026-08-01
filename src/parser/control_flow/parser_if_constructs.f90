module parser_if_constructs_module
    ! Parser module for IF constructs (if/then/else/elseif/endif)
    use lexer_core, only: token_t, TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, &
        TK_COMMENT, TK_WHITESPACE, to_lower
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_expression
    use parser_statement_core_module, only: statement_callbacks_t, &
        null_statement_callbacks, &
        skip_whitespace_and_semicolons
    use parser_forall_module, only: parse_forall
    use parser_select_constructs_module, only: parse_select_case
    use parser_arithmetic_if_module, only: is_arithmetic_if, parse_arithmetic_if
    use parser_if_inline_module, only: parse_inline_if
    use parser_if_body_module, only: parse_if_body
    use ast_arena_modern, only: ast_arena_t, link_children_to_parent
    use ast_nodes_control, only: if_node
    use ast_factory, only: push_if
    use parser_trailing_comment_module, only: capture_trailing_comment
    implicit none
    private

    public :: parse_if, parse_if_condition, parse_elseif_block
    public :: register_parse_do_loop, parse_do_loop_callback

    abstract interface
        function parse_do_loop_interface(parser, arena) result(loop_index)
            import :: parser_state_t, ast_arena_t
            type(parser_state_t), intent(inout) :: parser
            type(ast_arena_t), intent(inout) :: arena
            integer :: loop_index
        end function parse_do_loop_interface
    end interface

    procedure(parse_do_loop_interface), pointer :: parse_do_loop_proc => null()

    interface
        subroutine ensure_if_do_registration_bridge()
        end subroutine ensure_if_do_registration_bridge
    end interface

contains

    subroutine ensure_do_parser_ready()
        if (.not. associated(parse_do_loop_proc)) then
            call ensure_if_do_registration_bridge()
        end if
    end subroutine ensure_do_parser_ready

    subroutine register_parse_do_loop(proc)
        procedure(parse_do_loop_interface) :: proc
        parse_do_loop_proc => proc
    end subroutine register_parse_do_loop

    function build_if_body_callbacks() result(callbacks)
        type(statement_callbacks_t) :: callbacks

        callbacks = null_statement_callbacks()
        callbacks%parse_if => parse_if
        callbacks%parse_do_loop => parse_do_loop_callback
        callbacks%parse_select_case => parse_select_case
        callbacks%parse_forall => parse_forall
    end function build_if_body_callbacks

    integer function parse_do_loop_callback(parser, arena) result(loop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena

        loop_index = 0
        call ensure_do_parser_ready()
        if (associated(parse_do_loop_proc)) then
            loop_index = parse_do_loop_proc(parser, arena)
        end if
    end function parse_do_loop_callback

    ! Parse if statement
    recursive function parse_if(parser, arena, parent_index) result(if_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer :: if_index

        type(token_t) :: if_token, then_token
        integer :: condition_index
        type(statement_callbacks_t) :: callbacks

        ! Consume 'if' keyword
        if_token = parser%consume()

        ! Parse condition (should be in parentheses for standard if/then/endif)
        condition_index = parse_if_condition(parser, arena)

        ! Check for arithmetic IF: IF (expr) label1, label2, label3
        if (is_arithmetic_if(parser)) then
            if_index = parse_arithmetic_if(parser, arena, condition_index, if_token, &
                parent_index)
            return
        end if

        ! Look for 'then' keyword
        then_token = parser%peek()
        callbacks = build_if_body_callbacks()
        if (then_token%kind == TK_KEYWORD .and. to_lower(then_token%text) == &
            "then") then
            if_index = parse_block_if(parser, arena, condition_index, if_token, &
                parent_index, callbacks)
        else
            if_index = parse_inline_if(parser, arena, condition_index, if_token, &
                then_token, parent_index, callbacks)
        end if
    end function parse_if

    ! Parse block IF with THEN/ELSEIF/ELSE/ENDIF
    recursive function parse_block_if(parser, arena, condition_index, if_token, &
            parent_index, callbacks) result(if_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: condition_index
        type(token_t), intent(in) :: if_token
        integer, intent(in), optional :: parent_index
        type(statement_callbacks_t), intent(in) :: callbacks
        integer :: if_index

        type(token_t) :: then_token
        integer, allocatable :: then_body_indices(:), else_body_indices(:)
        integer, allocatable :: elseif_indices(:)

        ! Consume 'then' keyword
        then_token = parser%consume()

        ! Skip optional semicolon after 'then'
        if (.not. parser%is_at_end()) then
            then_token = parser%peek()
            if (then_token%kind == TK_OPERATOR .and. then_token%text == ";") then
                then_token = parser%consume()
            end if
        end if

        ! Create if node placeholder first to get the parent index
        if_index = push_if(arena, condition_index, [integer ::], &
            line=if_token%line, column=if_token%column, &
            parent_index=parent_index)

        if (if_index > 0) then
            call capture_trailing_comment(parser, arena, if_index)
        end if

        ! Parse then body statements with the if node as parent
        then_body_indices = parse_if_body(parser, arena, if_index, callbacks)

        ! Parse elseif/else blocks and endif
        allocate (elseif_indices(0))
        call parse_elseif_else_chain(parser, arena, if_index, elseif_indices, &
            else_body_indices, callbacks)

        ! Update the if node with the actual body indices
        call update_if_node_bodies(arena, if_index, then_body_indices, &
            elseif_indices, else_body_indices)
    end function parse_block_if

    ! Parse ELSEIF/ELSE chain and ENDIF
    recursive subroutine parse_elseif_else_chain(parser, arena, if_index, &
            elseif_indices, else_body_indices, &
            callbacks)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: if_index
        integer, allocatable, intent(inout) :: elseif_indices(:)
        integer, allocatable, intent(out) :: else_body_indices(:)
        type(statement_callbacks_t), intent(in) :: callbacks

        type(token_t) :: token
        integer :: safety_counter

        safety_counter = 0
        do while (.not. parser%is_at_end() .and. safety_counter < 10000)
            safety_counter = safety_counter + 1
            call skip_whitespace_and_semicolons(parser)
            if (parser%is_at_end()) exit

            token = parser%peek()

            if (token%kind /= TK_KEYWORD) exit

            select case (to_lower(token%text))
            case ("elseif", "else if")
                call append_elseif_block(parser, arena, elseif_indices)
            case ("else")
                if (is_else_if_construct(parser)) then
                    call append_elseif_block(parser, arena, elseif_indices)
                    cycle
                end if
                call parse_else_body(parser, arena, if_index, else_body_indices, &
                    callbacks)
                cycle
            case ("endif")
                token = parser%consume()
                exit
            case ("end")
                if (consume_end_if(parser)) exit
            case default
                exit
            end select
        end do
    end subroutine parse_elseif_else_chain

    ! Check if current position is else if construct
    function is_else_if_construct(parser) result(is_else_if)
        type(parser_state_t), intent(in) :: parser
        logical :: is_else_if

        is_else_if = .false.
        if (parser%current_token + 1 > size(parser%tokens)) return
        if (parser%tokens(parser%current_token + 1)%kind /= TK_KEYWORD) return
        is_else_if = to_lower(parser%tokens(parser%current_token + 1)%text) == "if"
    end function is_else_if_construct

    ! Append an elseif block to the indices array
    subroutine append_elseif_block(parser, arena, elseif_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: elseif_indices(:)

        integer :: elseif_pair(2)

    elseif_pair = parse_elseif_block(parser, arena)
    elseif_indices = [elseif_indices, elseif_pair]
    end subroutine append_elseif_block

    ! Parse else body block
    subroutine parse_else_body(parser, arena, if_index, else_body_indices, callbacks)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: if_index
        integer, allocatable, intent(out) :: else_body_indices(:)
        type(statement_callbacks_t), intent(in) :: callbacks

        type(token_t) :: token

        token = parser%consume() ! consume 'else'

        ! Skip optional semicolon after 'else'
        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ";") then
                token = parser%consume()
            end if
        end if

        else_body_indices = parse_if_body(parser, arena, if_index, callbacks)
    end subroutine parse_else_body

    ! Consume end if keyword pair if present, returns true if consumed
    function consume_end_if(parser) result(consumed)
        type(parser_state_t), intent(inout) :: parser
        logical :: consumed

        integer :: lookahead_pos
        type(token_t) :: lookahead, token

        consumed = .false.
        lookahead_pos = parser%current_token + 1

        ! Skip trivia to find next meaningful token
        do while (lookahead_pos <= size(parser%tokens))
            lookahead = parser%tokens(lookahead_pos)
            if (lookahead%kind == TK_WHITESPACE .or. &
                lookahead%kind == TK_NEWLINE .or. &
                lookahead%kind == TK_COMMENT) then
                lookahead_pos = lookahead_pos + 1
                cycle
            end if
            exit
        end do

        if (lookahead_pos > size(parser%tokens)) return
        lookahead = parser%tokens(lookahead_pos)
        if (lookahead%kind /= TK_KEYWORD) return
        if (to_lower(lookahead%text) /= "if") return

        ! Consume end keyword
        token = parser%consume()

        ! Skip trivia between end and if
        do while (.not. parser%is_at_end())
            lookahead = parser%peek()
            if (lookahead%kind == TK_WHITESPACE .or. &
                lookahead%kind == TK_NEWLINE .or. &
                lookahead%kind == TK_COMMENT) then
                token = parser%consume()
            else
                exit
            end if
        end do

        ! Consume if keyword
        if (.not. parser%is_at_end()) then
            lookahead = parser%peek()
            if (lookahead%kind == TK_KEYWORD .and. &
                to_lower(lookahead%text) == "if") then
                token = parser%consume()
            end if
        end if

        consumed = .true.
    end function consume_end_if

    ! Update if_node with body indices
    subroutine update_if_node_bodies(arena, if_index, then_body_indices, &
            elseif_indices, else_body_indices)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: if_index
        integer, allocatable, intent(in) :: then_body_indices(:)
        integer, allocatable, intent(in) :: elseif_indices(:)
        integer, allocatable, intent(in) :: else_body_indices(:)

        integer :: i

        if (.not. allocated(arena%entries(if_index)%node)) return

        select type (node => arena%entries(if_index)%node)
            type is (if_node)
            if (allocated(then_body_indices)) then
                node%then_body_indices = then_body_indices
                call link_children_to_parent(arena, if_index, then_body_indices)
            end if
            if (allocated(else_body_indices)) then
                node%else_body_indices = else_body_indices
                call link_children_to_parent(arena, if_index, else_body_indices)
            end if
            if (allocated(elseif_indices)) then
                if (size(elseif_indices) > 0 .and. &
                    mod(size(elseif_indices), 2) == 0) then
                    call link_children_to_parent(arena, if_index, elseif_indices)
                    allocate (node%elseif_blocks(size(elseif_indices) / 2))
                    do i = 1, size(elseif_indices) / 2
                        node%elseif_blocks(i)%condition_index = &
                            elseif_indices(2 * i - 1)
                        node%elseif_blocks(i)%body_indices = &
                            [elseif_indices(2 * i)]
                    end do
                end if
            end if
        end select
    end subroutine update_if_node_bodies

    ! Parse if condition (handles parentheses if present)
    function parse_if_condition(parser, arena) result(condition_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: condition_index
        type(token_t) :: paren_token

        paren_token = parser%peek()
        if (paren_token%kind == TK_OPERATOR .and. paren_token%text == "(") then
            condition_index = parse_if_condition_parenthesized(parser, arena)
        else
            condition_index = parse_if_condition_no_paren(parser, arena)
        end if
    end function parse_if_condition

    function parse_if_condition_parenthesized(parser, arena) result(condition_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: condition_index
        type(token_t) :: paren_token
        type(token_t), allocatable, target :: remaining_tokens(:)
        integer :: i, n
        integer :: paren_depth

        paren_token = parser%consume()
        paren_depth = 1

        n = 0
        do i = parser%current_token, size(parser%tokens)
            n = n + 1
        end do

        allocate (remaining_tokens(n))
        remaining_tokens = parser%tokens(parser%current_token:)

        condition_index = parse_expression(remaining_tokens, arena, parser)

        do while (.not. parser%is_at_end())
            paren_token = parser%peek()

            if (paren_token%kind == TK_KEYWORD .and. to_lower(paren_token%text) == &
                "then") then
                exit
            end if

            if (paren_token%kind == TK_NEWLINE) then
                exit
            end if

            if (paren_token%kind == TK_WHITESPACE .or. paren_token%kind == &
                TK_COMMENT) then
                paren_token = parser%consume()
                cycle
            end if

            if (paren_token%kind == TK_OPERATOR) then
                if (paren_token%text == "(") then
                    paren_depth = paren_depth + 1
                    paren_token = parser%consume()
                    cycle
                else if (paren_token%text == ")") then
                    paren_depth = paren_depth - 1
                    paren_token = parser%consume()
                    if (paren_depth <= 0) exit
                    cycle
                end if
            end if

            paren_token = parser%consume()
        end do
    end function parse_if_condition_parenthesized

    function parse_if_condition_no_paren(parser, arena) result(condition_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: condition_index
        type(token_t) :: paren_token
        type(token_t), allocatable, target :: remaining_tokens(:)
        integer :: i, n

        n = 0
        do i = parser%current_token, size(parser%tokens)
            n = n + 1
        end do

        allocate (remaining_tokens(n))
        remaining_tokens = parser%tokens(parser%current_token:)

        condition_index = parse_expression(remaining_tokens, arena, parser)

        do while (.not. parser%is_at_end())
            paren_token = parser%peek()

            if (paren_token%kind == TK_KEYWORD .and. to_lower(paren_token%text) == &
                "then") then
                exit
            end if

            if (paren_token%kind == TK_NEWLINE) then
                exit
            end if

            if (paren_token%kind == TK_WHITESPACE .or. paren_token%kind == &
                TK_COMMENT) then
                paren_token = parser%consume()
                cycle
            end if

            paren_token = parser%consume()
        end do
    end function parse_if_condition_no_paren

    ! Parse elseif block
    function parse_elseif_block(parser, arena) result(elseif_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: elseif_indices(2) ! condition_index, body_indices_start
        type(token_t) :: elseif_token
        type(statement_callbacks_t) :: callbacks

        ! Consume 'elseif' or 'else if'
    elseif_token = parser%consume()
        ! Check if we consumed 'else' and need to consume 'if' as well
        if (to_lower(elseif_token%text) == "else") then
            ! This should be "else if", consume the "if" token too
        elseif_token = parser%consume()
        end if

        ! Parse condition
    elseif_indices(1) = parse_if_condition(parser, arena)

        ! Look for 'then' keyword
    elseif_token = parser%peek()
        if (elseif_token%kind == TK_KEYWORD .and. to_lower(elseif_token%text) == &
            "then") then
        elseif_token = parser%consume()
        end if

        ! Parse body
        callbacks = build_if_body_callbacks()
        block
            integer, allocatable :: body_indices(:)
            body_indices = parse_if_body(parser, arena, callbacks=callbacks)
            if (allocated(body_indices) .and. size(body_indices) > 0) then
            elseif_indices(2) = body_indices(1) ! Store first body statement index
            else
            elseif_indices(2) = 0
            end if
        end block

    end function parse_elseif_block

end module parser_if_constructs_module
