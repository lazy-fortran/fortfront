module parser_basic_statement_module
    ! Parser module for basic statement parsing and utilities
    use lexer_core, only: token_t, TK_EOF, TK_KEYWORD
    use parser_state_module
    use parser_expressions_module, only: parse_range
    use parser_statement_core_module, only: parse_basic_statement_core, &
                                            statement_callbacks_t, &
                                            null_statement_callbacks
    use ast_arena_modern, only: ast_arena_t
    implicit none
    private

    public :: parse_basic_statement_multi, parse_statement_body
    public :: parse_expression_length

contains

    ! Parse basic statement with support for multi-variable declarations
    function parse_basic_statement_multi(tokens, arena, parent_index, callbacks) &
            result(stmt_indices)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        type(statement_callbacks_t), intent(in), optional :: callbacks
        integer, allocatable :: stmt_indices(:)
        type(statement_callbacks_t) :: local_callbacks

        if (present(callbacks)) then
            local_callbacks = callbacks
        else
            local_callbacks = null_statement_callbacks()
        end if

        stmt_indices = parse_basic_statement_core(tokens, arena, parent_index, &
                                                 local_callbacks)
    end function parse_basic_statement_multi

    ! Unified function for parsing statement bodies (used by if blocks, &
    ! do while loops, etc.)
    function parse_statement_body(parser, arena, end_keywords, callbacks) &
            result(body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: end_keywords(:)
        type(statement_callbacks_t), intent(in), optional :: callbacks
        integer, allocatable :: body_indices(:)

        type(token_t) :: token
        integer :: stmt_count, stmt_index
        integer :: stmt_start, stmt_end, j
        type(token_t), allocatable, target :: stmt_tokens(:)
        logical :: found_end
        type(statement_callbacks_t) :: local_callbacks

        allocate (body_indices(0))
        stmt_count = 0

        if (present(callbacks)) then
            local_callbacks = callbacks
        else
            local_callbacks = null_statement_callbacks()
        end if

        block
            integer :: safety_counter
            safety_counter = 0
            do while (.not. parser%is_at_end() .and. safety_counter < 10000)
                safety_counter = safety_counter + 1
                token = parser%peek()

            ! Check for end keywords
            found_end = .false.
            if (token%kind == TK_KEYWORD) then
                do j = 1, size(end_keywords)
                    if (token%text == trim(end_keywords(j))) then
                        found_end = .true.
                        exit
                    end if
                end do
                if (found_end) exit
            end if

            ! Parse statement until end of line
            stmt_start = parser%current_token
            stmt_end = stmt_start

            ! Find end of current statement (same line)
            do j = stmt_start, size(parser%tokens)
                if (parser%tokens(j)%kind == TK_EOF) then
                    stmt_end = j
                    exit
                end if
                if (j > stmt_start .and. parser%tokens(j)%line > &
                        parser%tokens(stmt_start)%line) then
                    stmt_end = j - 1
                    exit
                end if
                stmt_end = j
            end do

            ! Extract and parse statement tokens
            if (stmt_end >= stmt_start) then
                allocate (stmt_tokens(stmt_end - stmt_start + 2))
                stmt_tokens(1:stmt_end - stmt_start + 1) = &
                    parser%tokens(stmt_start:stmt_end)
                stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
                stmt_tokens(stmt_end - stmt_start + 2)%text = ""
                stmt_tokens(stmt_end - stmt_start + 2)%line = &
                    parser%tokens(stmt_end)%line
                stmt_tokens(stmt_end - stmt_start + 2)%column = &
                    parser%tokens(stmt_end)%column + 1

                ! Parse statement; multi-variable declarations may expand results
                block
                    integer, allocatable :: stmt_indices(:)
                    integer :: k
                    stmt_indices = parse_basic_statement_multi(stmt_tokens, arena, &
                                                             callbacks=local_callbacks)

                    ! Add all parsed statements to body
                    do k = 1, size(stmt_indices)
                        if (stmt_indices(k) > 0) then
                            body_indices = [body_indices, stmt_indices(k)]
                            stmt_count = stmt_count + 1
                        end if
                    end do
                end block

                deallocate (stmt_tokens)
            end if

            parser%current_token = stmt_end + 1
            end do
        end block
    end function parse_statement_body

    ! Helper function to determine how many tokens an expression consumes
    function parse_expression_length(parser, arena) result(length)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: length
        integer :: start_pos, expr_index
        
        start_pos = parser%current_token
        expr_index = parse_range(parser, arena)
        length = parser%current_token - start_pos
        if (length == 0) length = 1  ! At least one token
    end function parse_expression_length

end module parser_basic_statement_module
