module parser_if_constructs_module
    ! Parser module for IF constructs (if/then/else/elseif/endif)
    use lexer_core, only: token_t, TK_EOF, TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, &
                          TK_COMMENT, TK_WHITESPACE
    use parser_state_module
    use parser_expressions_module, only: parse_expression
    use parser_statement_core_module, only: parse_basic_statement_core, &
                                            statement_callbacks_t, &
                                            null_statement_callbacks, &
                                            find_statement_end, &
                                            extend_if_statement_end, &
                                            allocate_stmt_tokens_with_eof, &
                                            skip_whitespace_and_semicolons
    use parser_forall_module, only: parse_forall
    use parser_select_constructs_module, only: parse_select_case
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_control, only: if_node
    use ast_factory, only: push_if
    implicit none
    private

    public :: parse_if, parse_if_condition, parse_if_body, parse_elseif_block
    public :: register_parse_do_loop

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
    function parse_if(parser, arena, parent_index) result(if_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer :: if_index

        type(token_t) :: if_token, then_token
        integer :: condition_index
        integer, allocatable :: then_body_indices(:), else_body_indices(:)
        integer, allocatable :: elseif_indices(:)
        integer :: elseif_count

        ! Consume 'if' keyword
        if_token = parser%consume()

        ! Parse condition (should be in parentheses for standard if/then/endif)
        condition_index = parse_if_condition(parser, arena)

        ! Look for 'then' keyword
        then_token = parser%peek()
        if (then_token%kind == TK_KEYWORD .and. then_token%text == "then") then
            ! Standard if/then/endif block
            then_token = parser%consume()

            ! Skip optional semicolon after 'then'
            if (.not. parser%is_at_end()) then
                then_token = parser%peek()
                if (then_token%kind == TK_OPERATOR .and. then_token%text == ";") then
                    then_token = parser%consume()  ! Skip the semicolon
                end if
            end if

            ! Create if node placeholder first to get the parent index
            if_index = push_if(arena, condition_index, [integer ::], &
                               line=if_token%line, column=if_token%column, &
                               parent_index=parent_index)

            ! Parse then body statements with the if node as parent
            then_body_indices = parse_if_body(parser, arena, if_index)

            ! Check for elseif/else blocks
            elseif_count = 0
            allocate (elseif_indices(0))

            block
                integer :: safety_counter
                safety_counter = 0
                do while (.not. parser%is_at_end() .and. safety_counter < 10000)
                    safety_counter = safety_counter + 1
                    then_token = parser%peek()

                    if (then_token%kind == TK_KEYWORD) then
                        if (then_token%text == "elseif" .or. then_token%text == &
                            "else if") then
                            ! Parse elseif block
                            block
                                integer :: elseif_pair(2)
                                elseif_pair = parse_elseif_block(parser, arena)
                                elseif_indices = [elseif_indices, elseif_pair]
                                elseif_count = elseif_count + 1
                            end block
                        else if (then_token%text == "else") then
                            ! Check if next token is "if" (for "else if")
                            if (parser%current_token + 1 <= size(parser%tokens)) then
                                if (parser%tokens(parser%current_token + 1)%kind == &
                                    TK_KEYWORD .and. &
                                    parser%tokens(parser%current_token + 1)%text &
                                    == "if") then
                                    ! Parse as elseif block
                                    block
                                        integer :: elseif_pair(2)
                                        elseif_pair = parse_elseif_block(parser, arena)
                                        elseif_indices = [elseif_indices, elseif_pair]
                                        elseif_count = elseif_count + 1
                                    end block
                                    cycle  ! Continue looking for more elseif/else blocks
                                end if
                            end if
                            ! Parse else block
                            then_token = parser%consume()  ! consume 'else'

                            ! Skip optional semicolon after 'else'
                            if (.not. parser%is_at_end()) then
                                then_token = parser%peek()
                                if (then_token%kind == TK_OPERATOR .and. &
                                    then_token%text == ";") then
                                    then_token = parser%consume()  ! Skip the semicolon
                                end if
                            end if

                            else_body_indices = parse_if_body(parser, arena, if_index)
                            exit
                        else if (then_token%text == "endif" .or. then_token%text == &
                                 "end if") then
                            ! End of if statement
                            then_token = parser%consume()
                            exit
                        else
                            ! Other statement, stop parsing if block
                            exit
                        end if
                    else
                        ! Not a keyword, continue parsing body
                        exit
                    end if
                end do
            end block

            ! Update the if node with the actual body indices
            if (allocated(arena%entries(if_index)%node)) then
                select type (node => arena%entries(if_index)%node)
                type is (if_node)
                    if (allocated(then_body_indices)) then
                        node%then_body_indices = then_body_indices
                    end if
                    if (allocated(else_body_indices)) then
                        node%else_body_indices = else_body_indices
                    end if
                    if (allocated(elseif_indices)) then
                        if (size(elseif_indices) > 0 .and. &
                            mod(size(elseif_indices), 2) == 0) then
                            allocate (node%elseif_blocks(size(elseif_indices) / 2))
                            do elseif_count = 1, size(elseif_indices) / 2
                                node%elseif_blocks(elseif_count)%condition_index = &
                                    elseif_indices(2 * elseif_count - 1)
                                node%elseif_blocks(elseif_count)%body_indices = &
                                    [elseif_indices(2 * elseif_count)]
                            end do
                        end if
                    end if
                end select
            end if
        else
            ! One-line if statement (no then keyword)
            allocate (then_body_indices(1))

            ! Parse the single statement
            block
                type(token_t), allocatable, target :: remaining_tokens(:)
                integer :: i, n

                ! Count remaining tokens
                n = 0
                do i = parser%current_token, size(parser%tokens)
                    n = n + 1
                end do

                ! Extract remaining tokens
                allocate (remaining_tokens(n))
                remaining_tokens = parser%tokens(parser%current_token:)

                ! Parse single statement using shared core
                block
                    integer, allocatable :: stmt_indices(:)
                    type(statement_callbacks_t) :: callbacks

                    callbacks = build_if_body_callbacks()
                    stmt_indices = parse_basic_statement_core(remaining_tokens, arena, &
                                                              callbacks=callbacks)
                    if (size(stmt_indices) > 0 .and. stmt_indices(1) > 0) then
                        then_body_indices(1) = stmt_indices(1)
                    end if
                end block

                ! Advance parser to end of statement to prevent re-parsing
                do while (.not. parser%is_at_end())
                    block
                        type(token_t) :: tok
                        tok = parser%peek()
                        if (tok%kind == TK_NEWLINE .or. tok%kind == TK_EOF) then
                            exit
                        end if
                        tok = parser%consume()
                    end block
                end do
            end block

            ! Create if node with no elseif/else blocks
            allocate (elseif_indices(0))
            allocate (else_body_indices(0))
            if_index = push_if(arena, condition_index, then_body_indices, &
                               elseif_indices=elseif_indices, &
                               else_body_indices=else_body_indices, &
                               line=if_token%line, column=if_token%column, &
                               parent_index=parent_index)
        end if

    end function parse_if

    ! Parse if condition (handles parentheses if present)
    function parse_if_condition(parser, arena) result(condition_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: condition_index
        type(token_t) :: paren_token
        type(token_t), allocatable, target :: remaining_tokens(:)
        integer :: i, n

        ! Check for opening parenthesis
        paren_token = parser%peek()
        if (paren_token%kind == TK_OPERATOR .and. paren_token%text == "(") then
            paren_token = parser%consume()  ! consume '('

            ! Count remaining tokens
            n = 0
            do i = parser%current_token, size(parser%tokens)
                n = n + 1
            end do

            ! Extract remaining tokens
            allocate (remaining_tokens(n))
            remaining_tokens = parser%tokens(parser%current_token:)

            ! Parse the condition expression
            condition_index = parse_expression(remaining_tokens, arena)

            ! Advance parser past the condition tokens
            ! Simple approach: advance until we find the closing paren or 'then'
            do while (.not. parser%is_at_end())
                paren_token = parser%peek()
                if (paren_token%kind == TK_OPERATOR .and. paren_token%text == ")") then
                    paren_token = parser%consume()  ! consume the ')'
                    exit
                else if (paren_token%kind == TK_KEYWORD .and. paren_token%text == &
                         "then") then
                    exit  ! Don't consume 'then', let caller handle it
                end if
                paren_token = parser%consume()
            end do
        else
            ! No parentheses, just parse the expression
            ! Count remaining tokens
            n = 0
            do i = parser%current_token, size(parser%tokens)
                n = n + 1
            end do

            ! Extract remaining tokens
            allocate (remaining_tokens(n))
            remaining_tokens = parser%tokens(parser%current_token:)

            condition_index = parse_expression(remaining_tokens, arena)

            ! Advance parser past the condition tokens until 'then'
            do while (.not. parser%is_at_end())
                paren_token = parser%peek()
                if (paren_token%kind == TK_KEYWORD .and. paren_token%text == "then") then
                    exit  ! Don't consume 'then', let caller handle it
                end if
                paren_token = parser%consume()
            end do
        end if

    end function parse_if_condition

    ! Parse if/elseif/else body statements
    function parse_if_body(parser, arena, parent_index) result(body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer, allocatable :: body_indices(:)
        type(token_t) :: token
        integer :: stmt_count

        allocate (body_indices(0))
        stmt_count = 0

        block
            integer :: safety_counter
            safety_counter = 0

            do while (.not. parser%is_at_end() .and. safety_counter < 10000)
                safety_counter = safety_counter + 1

                call skip_whitespace_and_semicolons(parser)
                if (parser%current_token > size(parser%tokens)) exit

                token = parser%peek()

                ! Check for end of body
                if (token%kind == TK_KEYWORD) then
                    if (token%text == "elseif" .or. token%text == "else if" .or. &
                        token%text == "endif" .or. token%text == "end if") then
                        exit
                    else if (token%text == "else") then
                        ! Check if next token is "if" (for "else if")
                        if (parser%current_token + 1 <= size(parser%tokens)) then
                            block
                                type(token_t) :: lookahead
                                lookahead = parser%tokens(parser%current_token + 1)
                                if (lookahead%kind == TK_KEYWORD .and. &
                                    lookahead%text == "if") then
                                    exit  ! Found "else if"
                                end if
                            end block
                        end if
                        ! If not "else if", it's just "else" - also exit
                        exit
                    else if (token%text == "end") then
                        ! Check if next token is "if"
                        if (parser%current_token + 1 <= size(parser%tokens)) then
                            block
                                type(token_t) :: lookahead
                                lookahead = parser%tokens(parser%current_token + 1)
                                if (lookahead%kind == TK_KEYWORD .and. &
                                    lookahead%text == "if") then
                                    exit  ! Found "end if"
                                end if
                            end block
                        end if
                    end if
                end if

                ! Parse statement until end of line (same approach as do loop)
                block
                    type(token_t), allocatable, target :: stmt_tokens(:)
                    type(token_t) :: first_stmt_token
                    integer, allocatable :: stmt_indices(:)
                    integer :: remaining_count, consumed_tokens, k
                    integer :: stmt_end, last_token_index
                    type(statement_callbacks_t) :: callbacks

                    stmt_end = find_statement_end(parser%tokens, parser%current_token)
                    first_stmt_token = parser%tokens(parser%current_token)
                    if (first_stmt_token%kind == TK_KEYWORD) then
                        if (first_stmt_token%text == "if") then
                            stmt_end = extend_if_statement_end(parser%tokens, &
                                                               parser%current_token, &
                                                               stmt_end)
                        end if
                    end if
                    if (stmt_end < parser%current_token) then
                        stmt_end = parser%current_token
                    end if

                    remaining_count = stmt_end - parser%current_token + 1
                    if (remaining_count <= 0) exit

                    call allocate_stmt_tokens_with_eof(stmt_tokens, parser%tokens, &
                                                       parser%current_token, stmt_end)
                    last_token_index = stmt_end
                    stmt_tokens(remaining_count + 1)%line = &
                        parser%tokens(last_token_index)%line
                    stmt_tokens(remaining_count + 1)%column = &
                        parser%tokens(last_token_index)%column + 1

                    callbacks = build_if_body_callbacks()
                    if (present(parent_index)) then
                        stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                                         parent_index=parent_index, callbacks=callbacks, &
                                                           consumed_count=consumed_tokens)
                    else
                        stmt_indices = parse_basic_statement_core(stmt_tokens, arena, &
                                      callbacks=callbacks, consumed_count=consumed_tokens)
                    end if

                    if (allocated(stmt_indices) .and. size(stmt_indices) > 0) then
                        do k = 1, size(stmt_indices)
                            if (stmt_indices(k) > 0) then
                                body_indices = [body_indices, stmt_indices(k)]
                                stmt_count = stmt_count + 1
                            end if
                        end do
                    end if

                    parser%current_token = stmt_end + 1

                    block
                        type(token_t), allocatable, target :: temp(:)
                        call move_alloc(stmt_tokens, temp)
                    end block
                end block
            end do
        end block

    end function parse_if_body

    ! Parse elseif block
    function parse_elseif_block(parser, arena) result(elseif_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: elseif_indices(2)  ! condition_index, body_indices_start
        type(token_t) :: elseif_token

        ! Consume 'elseif' or 'else if'
        elseif_token = parser%consume()
        ! Check if we consumed 'else' and need to consume 'if' as well
        if (elseif_token%text == "else") then
            ! This should be "else if", consume the "if" token too
            elseif_token = parser%consume()
        end if

        ! Parse condition
        elseif_indices(1) = parse_if_condition(parser, arena)

        ! Look for 'then' keyword
        elseif_token = parser%peek()
        if (elseif_token%kind == TK_KEYWORD .and. elseif_token%text == "then") then
            elseif_token = parser%consume()
        end if

        ! Parse body
        block
            integer, allocatable :: body_indices(:)
            body_indices = parse_if_body(parser, arena)
            if (allocated(body_indices) .and. size(body_indices) > 0) then
                elseif_indices(2) = body_indices(1)  ! Store first body statement index
            else
                elseif_indices(2) = 0
            end if
        end block

    end function parse_elseif_block

end module parser_if_constructs_module
