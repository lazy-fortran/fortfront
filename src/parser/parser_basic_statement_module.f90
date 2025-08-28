module parser_basic_statement_module
    ! Parser module for basic statement parsing and utilities
    use iso_fortran_env, only: error_unit
    use lexer_core
    use ast_types, only: LITERAL_STRING
    use parser_state_module
    use parser_expressions_module, only: parse_expression, parse_range
    use parser_io_statements_module, only: parse_print_statement, parse_write_statement, &
                                           parse_read_statement
    use parser_control_statements_module, only: parse_cycle_statement, parse_exit_statement, &
                                                parse_return_statement, parse_stop_statement, &
                                                parse_goto_statement, parse_error_stop_statement
    use parser_declarations, only: parse_declaration, parse_multi_declaration
    use parser_utils, only: analyze_declaration_structure
    use ast_core
    use ast_factory, only: push_assignment, push_identifier, push_literal
    implicit none
    private

    public :: parse_basic_statement_multi, parse_statement_body
    public :: parse_expression_length

contains

    ! Parse basic statement with support for multi-variable declarations
    function parse_basic_statement_multi(tokens, arena, parent_index) result(stmt_indices)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer, allocatable :: stmt_indices(:)
        type(parser_state_t) :: parser
        type(token_t) :: first_token
        integer :: stmt_index

        parser = create_parser_state(tokens)
        first_token = parser%peek()

        ! Check if this is a declaration that might have multiple variables
        if (first_token%kind == TK_KEYWORD) then
            if (first_token%text == "real" .or. first_token%text == "integer" .or. &
                first_token%text == "logical" .or. first_token%text == "character") then
                ! Check if this is a single declaration with initializer
                ! If so, use parse_declaration for proper initializer handling
                block
                    logical :: has_initializer, has_comma
                    integer :: lookahead_pos
                    type(token_t) :: lookahead_token
                    
                    ! Look ahead to check for = (initializer) and comma (multi-var)
                    has_initializer = .false.
                    has_comma = .false.
                    lookahead_pos = parser%current_token
                    
                    call analyze_declaration_structure(parser, has_initializer, has_comma)
                    
                    
                    if (has_initializer .and. .not. has_comma) then
                        ! Single variable with initializer - use parse_declaration
                        allocate (stmt_indices(1))
                        stmt_indices(1) = parse_declaration(parser, arena)
                        return
                    else
                        ! Multi-variable declaration - use parse_multi_declaration
                        stmt_indices = parse_multi_declaration(parser, arena)
                        return
                    end if
                end block
            end if
        end if

        ! For all other statements, parse as single statement
        allocate (stmt_indices(1))
        stmt_index = 0

        ! Handle different statement types
        if (first_token%kind == TK_KEYWORD) then
            select case (first_token%text)
            case ("if", "do")
                ! Skip control flow statements to avoid circular dependency
                ! They will be handled by higher-level parsers
                stmt_index = 0
            case ("print")
                ! Parse print statement
                stmt_index = parse_print_statement(parser, arena)
            case ("write")
                ! Parse write statement
                stmt_index = parse_write_statement(parser, arena)
            case ("read")
                ! Parse read statement
                stmt_index = parse_read_statement(parser, arena)
            case ("cycle")
                ! Parse cycle statement
                stmt_index = parse_cycle_statement(parser, arena)
            case ("exit")
                ! Parse exit statement
                stmt_index = parse_exit_statement(parser, arena)
            case ("return")
                ! Parse return statement
                stmt_index = parse_return_statement(parser, arena, parent_index)
            case ("call")
                ! Skip call statements in control flow contexts
                ! They will be handled by higher-level parsers to avoid circular dependency
                stmt_index = 0
                first_token = parser%consume()  ! consume "call" to avoid infinite loop
            case ("stop")
                ! Parse stop statement
                stmt_index = parse_stop_statement(parser, arena)
            case ("go")
                ! Parse goto statement
                stmt_index = parse_goto_statement(parser, arena)
            case ("error")
                ! Parse error stop statement
                stmt_index = parse_error_stop_statement(parser, arena)
            case default
                ! Unknown keyword
                stmt_index = 0
            end select
        else if (first_token%kind == TK_IDENTIFIER) then
            block
                type(token_t) :: id_token, op_token
                integer :: target_index, value_index

                id_token = parser%consume()
                op_token = parser%peek()

                if (op_token%kind == TK_OPERATOR .and. op_token%text == "=") then
                    op_token = parser%consume()  ! consume '='
    target_index = push_identifier(arena, id_token%text, id_token%line, id_token%column, parent_index)
                    ! Get remaining tokens for expression parsing
                    block
                        type(token_t), allocatable :: expr_tokens(:)
                        integer :: remaining_count
                        remaining_count = size(tokens) - parser%current_token + 1
                        if (remaining_count > 0) then
                            allocate (expr_tokens(remaining_count))
                            expr_tokens = tokens(parser%current_token:)
                            value_index = parse_expression(expr_tokens, arena)
                            if (value_index > 0) then
                        stmt_index = push_assignment(arena, target_index, value_index, &
                                                         id_token%line, id_token%column, &
                                                         parent_index)
                            end if
                        end if
                    end block
                end if
            end block
        end if

        ! If we couldn't parse it, create a placeholder with debug info
        if (stmt_index == 0) then
            block
                character(len=256) :: debug_msg
                character(len=64) :: token_text
                integer :: debug_len, i

                debug_msg = "! Unparsed: "
                debug_len = len_trim(debug_msg)

                ! Add first few tokens for debugging
                do i = 1, min(3, size(tokens))
                    if (tokens(i)%kind == TK_EOF) exit
                    if (len_trim(tokens(i)%text) > 0) then
                        token_text = trim(tokens(i)%text)
                        if (debug_len + len_trim(token_text) + 1 < 250) then
                            debug_msg = debug_msg(1:debug_len)//" "//trim(token_text)
                            debug_len = len_trim(debug_msg)
                        end if
                    end if
                end do

                stmt_index = push_literal(arena, trim(debug_msg), LITERAL_STRING, &
                                          first_token%line, first_token%column)
            end block
        end if

        stmt_indices(1) = stmt_index

    end function parse_basic_statement_multi

    ! Unified function for parsing statement bodies (used by if blocks, &
    ! do while loops, etc.)
    function parse_statement_body(parser, arena, end_keywords) result(body_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: end_keywords(:)
        integer, allocatable :: body_indices(:)

        type(token_t) :: token
        integer :: stmt_count, stmt_index
        integer :: stmt_start, stmt_end, j
        type(token_t), allocatable :: stmt_tokens(:)
        logical :: found_end

        allocate (body_indices(0))
        stmt_count = 0

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
   if (j > stmt_start .and. parser%tokens(j)%line > parser%tokens(stmt_start)%line) then
                    stmt_end = j - 1
                    exit
                end if
                stmt_end = j
            end do

            ! Extract and parse statement tokens
            if (stmt_end >= stmt_start) then
                allocate (stmt_tokens(stmt_end - stmt_start + 2))
           stmt_tokens(1:stmt_end - stmt_start + 1) = parser%tokens(stmt_start:stmt_end)
                stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
                stmt_tokens(stmt_end - stmt_start + 2)%text = ""
              stmt_tokens(stmt_end - stmt_start + 2)%line = parser%tokens(stmt_end)%line
      stmt_tokens(stmt_end - stmt_start + 2)%column = parser%tokens(stmt_end)%column + 1

                ! Parse the statement (may return multiple indices for multi-variable declarations)
                block
                    integer, allocatable :: stmt_indices(:)
                    integer :: k
                    stmt_indices = parse_basic_statement_multi(stmt_tokens, arena)

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