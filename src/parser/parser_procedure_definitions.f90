module parser_procedure_definitions_module
    ! Parser module for function, subroutine, and interface definitions
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
                          TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, TK_WHITESPACE
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_parameter_handling_module, only: parse_typed_parameters, merge_parameter_attributes
    use parser_statement_utilities_module, only: parse_statement_in_if_block
    use parser_statement_core_module, only: find_statement_end
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_function_def, push_subroutine_def, push_interface_block
    use ast_factory
    implicit none
    private

    public :: parse_function_definition, parse_subroutine_definition, parse_interface_block

contains

    integer function next_non_whitespace_token(tokens, idx) result(next_idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: idx
        integer :: j

        next_idx = size(tokens) + 1
        do j = idx + 1, size(tokens)
            select case (tokens(j)%kind)
            case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                cycle
            case default
                next_idx = j
                return
            end select
        end do
    end function next_non_whitespace_token

    integer function prev_non_whitespace_token(tokens, idx) result(prev_idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: idx
        integer :: j

        prev_idx = 0
        do j = idx - 1, 1, -1
            select case (tokens(j)%kind)
            case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                cycle
            case default
                prev_idx = j
                return
            end select
        end do
    end function prev_non_whitespace_token

    logical function is_else_if_token(tokens, idx) result(is_else_if)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: idx
        integer :: prev_idx

        is_else_if = .false.
        if (idx < 1 .or. idx > size(tokens)) return

        prev_idx = prev_non_whitespace_token(tokens, idx)
        if (prev_idx < 1) return

        if (tokens(prev_idx)%kind == TK_KEYWORD .and. tokens(prev_idx)%text == "else") then
            is_else_if = .true.
        end if
    end function is_else_if_token

    logical function is_block_if_token(tokens, idx) result(is_block)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: idx
        integer :: j

        is_block = .false.
        if (idx < 1 .or. idx > size(tokens)) return

        do j = idx + 1, size(tokens)
            select case (tokens(j)%kind)
            case (TK_KEYWORD)
                if (tokens(j)%text == "then") then
                    is_block = .true.
                end if
                return
            case (TK_OPERATOR)
                if (tokens(j)%text == ";") return
            case (TK_NEWLINE, TK_EOF)
                return
            case (TK_COMMENT, TK_WHITESPACE)
                cycle
            case default
                cycle
            end select
        end do
    end function is_block_if_token

    integer function find_if_construct_end(tokens, start_index, initial_end) result(end_index)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_index, initial_end
        integer :: idx, depth, next_idx
        logical :: saw_if

        end_index = initial_end
        if (start_index < 1 .or. start_index > size(tokens)) return

        depth = 0
        saw_if = .false.

        idx = start_index
        do while (idx <= size(tokens))
            select case (tokens(idx)%kind)
            case (TK_KEYWORD)
                select case (tokens(idx)%text)
                case ("if")
                    if (.not. is_else_if_token(tokens, idx)) then
                        if (is_block_if_token(tokens, idx)) then
                            depth = depth + 1
                            saw_if = .true.
                        end if
                    end if
                case ("endif")
                    if (depth > 0) then
                        depth = depth - 1
                        if (depth == 0 .and. saw_if) then
                            end_index = idx
                            return
                        end if
                    end if
                case ("end")
                    next_idx = next_non_whitespace_token(tokens, idx)
                    if (next_idx <= size(tokens)) then
                        if (tokens(next_idx)%kind == TK_KEYWORD .and. tokens(next_idx)%text == "if") then
                            if (depth > 0) then
                                depth = depth - 1
                                if (depth == 0 .and. saw_if) then
                                    end_index = next_idx
                                    return
                                end if
                            end if
                            idx = next_idx
                        end if
                    end if
                end select
            case (TK_EOF)
                exit
            end select

            idx = idx + 1
        end do

        if (saw_if .and. depth == 0) then
            end_index = max(end_index, start_index)
        end if
    end function find_if_construct_end

    function parse_function_definition(parser, arena) result(func_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: func_index

        type(token_t) :: token
        character(len=:), allocatable :: function_name, return_type_str, result_variable_name
        integer :: line, column
        integer, allocatable :: param_indices(:), body_indices(:)

        ! Initialize
        return_type_str = ""
        result_variable_name = ""

        ! Check if we have a return type before "function"
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. &
            (token%text == "real" .or. token%text == "integer" .or. &
             token%text == "logical" .or. token%text == "character")) then
            return_type_str = token%text
            token = parser%consume()
        end if

        ! Consume function keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "function") then
            line = token%line
            column = token%column
            token = parser%consume()
        else
            func_index = 0
            return
        end if

        ! Get function name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            function_name = token%text
            token = parser%consume()
        else
            function_name = "unnamed_function"
        end if

        ! Parse parameters with protective error handling
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()
            ! Parse typed parameters safely
            call parse_typed_parameters(parser, arena, param_indices)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
            end if
        else
            allocate(param_indices(0))
        end if

        ! Check for result clause
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER .and. token%text == "result") then
            token = parser%consume()
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
                    result_variable_name = token%text
                    token = parser%consume()
                end if
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()
                end if
            end if
        end if

        ! Parse function body until "end function"
        allocate(body_indices(0))

        do while (.not. parser%is_at_end())
            do while (parser%current_token <= size(parser%tokens))
                select case (parser%tokens(parser%current_token)%kind)
                case (TK_NEWLINE, TK_WHITESPACE, TK_COMMENT)
                    parser%current_token = parser%current_token + 1
                case default
                    exit
                end select
            end do

            if (parser%current_token > size(parser%tokens)) exit

            token = parser%peek()

            ! Check for end function
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                        parser%tokens(parser%current_token + 1)%text == "function") then
                        token = parser%consume()
                        token = parser%consume()
                        if (.not. parser%is_at_end()) then
                            token = parser%peek()
                            if (token%kind == TK_IDENTIFIER .and. token%text == function_name) then
                                token = parser%consume()
                            end if
                        end if
                        exit
                    end if
                end if
            end if

            block
                type(token_t), allocatable, target :: stmt_tokens(:)
                integer :: stmt_start, stmt_end, stmt_size, stmt_index, extended_end
                type(parser_state_t) :: block_parser

                stmt_start = parser%current_token
                stmt_end = find_statement_end(parser%tokens, stmt_start)

                if (stmt_start > size(parser%tokens)) then
                    cycle
                end if

                if (stmt_start <= size(parser%tokens)) then
                    if (parser%tokens(stmt_start)%kind == TK_KEYWORD .and. &
                        parser%tokens(stmt_start)%text == "if") then
                        extended_end = find_if_construct_end(parser%tokens, stmt_start, stmt_end)
                        if (extended_end > stmt_end) stmt_end = extended_end
                    end if
                end if

                if (stmt_end < stmt_start) then
                    parser%current_token = parser%current_token + 1
                    cycle
                end if

                stmt_size = stmt_end - stmt_start + 1
                if (stmt_size > 0) then
                    allocate(stmt_tokens(stmt_size + 1))
                    stmt_tokens(1:stmt_size) = parser%tokens(stmt_start:stmt_end)
                    stmt_tokens(stmt_size + 1)%kind = TK_EOF
                    stmt_tokens(stmt_size + 1)%text = ''
                    stmt_tokens(stmt_size + 1)%line = parser%tokens(stmt_end)%line
                    stmt_tokens(stmt_size + 1)%column = parser%tokens(stmt_end)%column

                    block_parser = create_parser_state(stmt_tokens)
                    block
                        integer :: first_token_index
                        type(token_t) :: first_token

                        first_token_index = 1
                        do while (first_token_index <= stmt_size)
                            select case (stmt_tokens(first_token_index)%kind)
                            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                                first_token_index = first_token_index + 1
                            case default
                                exit
                            end select
                        end do

                        if (first_token_index > stmt_size) then
                            parser%current_token = stmt_end + 1
                            deallocate(stmt_tokens)
                            cycle
                        end if

                        block_parser%current_token = first_token_index
                        first_token = stmt_tokens(first_token_index)
                        stmt_index = parse_statement_in_if_block(block_parser, arena, first_token)
                    end block

                    if (stmt_index > 0) then
                        body_indices = [body_indices, stmt_index]
                    end if

                    parser%current_token = stmt_end + 1

                    deallocate(stmt_tokens)
                    cycle
                end if
            end block
        end do

        ! Merge parameter attributes from body declarations
        if (allocated(param_indices) .and. allocated(body_indices)) then
            if (size(param_indices) > 0 .and. size(body_indices) > 0) then
                call merge_parameter_attributes(arena, param_indices, body_indices)
            end if
        end if
        
        ! Create function node
        func_index = push_function_def(arena, function_name, param_indices, &
                                       return_type_str, body_indices, &
                                       line, column, result_variable=result_variable_name)
    end function parse_function_definition

    function parse_subroutine_definition(parser, arena) result(sub_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: sub_index

        type(token_t) :: token
        character(len=:), allocatable :: subroutine_name
        integer :: line, column
        integer, allocatable :: param_indices(:), body_indices(:)

        ! Consume subroutine keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Get subroutine name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            subroutine_name = token%text
            token = parser%consume()
        else
            subroutine_name = "unnamed_subroutine"
        end if

        ! Parse parameters
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()
            call parse_typed_parameters(parser, arena, param_indices)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
            end if
        else
            allocate(param_indices(0))
        end if

        ! Parse subroutine body until "end subroutine"
        allocate(body_indices(0))

        do while (.not. parser%is_at_end())
            do while (parser%current_token <= size(parser%tokens))
                select case (parser%tokens(parser%current_token)%kind)
                case (TK_NEWLINE, TK_WHITESPACE, TK_COMMENT)
                    parser%current_token = parser%current_token + 1
                case default
                    exit
                end select
            end do

            if (parser%current_token > size(parser%tokens)) exit

            token = parser%peek()

            ! Check for end of subroutine
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                        parser%tokens(parser%current_token + 1)%text == "subroutine") then
                        token = parser%consume()
                        token = parser%consume()
                        if (.not. parser%is_at_end()) then
                            token = parser%peek()
                            if (token%kind == TK_IDENTIFIER .and. token%text == subroutine_name) then
                                token = parser%consume()
                            end if
                        end if
                        exit
                    end if
                end if
            end if

            block
                type(token_t), allocatable, target :: stmt_tokens(:)
                integer :: stmt_start, stmt_end, stmt_size, stmt_index, extended_end
                type(parser_state_t) :: block_parser

                stmt_start = parser%current_token
                stmt_end = find_statement_end(parser%tokens, stmt_start)

                if (stmt_start > size(parser%tokens)) then
                    cycle
                end if

                if (stmt_start <= size(parser%tokens)) then
                    if (parser%tokens(stmt_start)%kind == TK_KEYWORD .and. &
                        parser%tokens(stmt_start)%text == "if") then
                        extended_end = find_if_construct_end(parser%tokens, stmt_start, stmt_end)
                        if (extended_end > stmt_end) stmt_end = extended_end
                    end if
                end if

                if (stmt_end < stmt_start) then
                    parser%current_token = parser%current_token + 1
                    cycle
                end if

                stmt_size = stmt_end - stmt_start + 1
                if (stmt_size > 0) then
                    allocate(stmt_tokens(stmt_size + 1))
                    stmt_tokens(1:stmt_size) = parser%tokens(stmt_start:stmt_end)
                    stmt_tokens(stmt_size + 1)%kind = TK_EOF
                    stmt_tokens(stmt_size + 1)%text = ''
                    stmt_tokens(stmt_size + 1)%line = parser%tokens(stmt_end)%line
                    stmt_tokens(stmt_size + 1)%column = parser%tokens(stmt_end)%column

                    block_parser = create_parser_state(stmt_tokens)
                    block
                        integer :: first_token_index
                        type(token_t) :: first_token

                        first_token_index = 1
                        do while (first_token_index <= stmt_size)
                            select case (stmt_tokens(first_token_index)%kind)
                            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                                first_token_index = first_token_index + 1
                            case default
                                exit
                            end select
                        end do

                        if (first_token_index > stmt_size) then
                            parser%current_token = stmt_end + 1
                            deallocate(stmt_tokens)
                            cycle
                        end if

                        block_parser%current_token = first_token_index
                        first_token = stmt_tokens(first_token_index)
                        stmt_index = parse_statement_in_if_block(block_parser, arena, first_token)
                    end block

                    if (stmt_index > 0) then
                        body_indices = [body_indices, stmt_index]
                    end if

                    parser%current_token = stmt_end + 1

                    deallocate(stmt_tokens)
                    cycle
                end if
            end block
        end do

        ! Merge parameter attributes from body declarations
        if (allocated(param_indices) .and. allocated(body_indices)) then
            if (size(param_indices) > 0 .and. size(body_indices) > 0) then
                call merge_parameter_attributes(arena, param_indices, body_indices)
            end if
        end if
        
        ! Create subroutine node
        sub_index = push_subroutine_def(arena, subroutine_name, param_indices, body_indices, &
                                        line, column)
    end function parse_subroutine_definition

    function parse_interface_block(parser, arena) result(interface_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: interface_index

        type(token_t) :: token
        character(len=:), allocatable :: interface_name
        integer :: line, column
        integer, allocatable :: body_indices(:)

        ! Consume interface keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Get interface name (optional)
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            interface_name = token%text
        else
            interface_name = ""
        end if

        ! Simplified parsing for refactoring
        allocate(body_indices(0))

        ! Create interface node
        interface_index = push_interface_block(arena, interface_name, body_indices, &
                                               line, column)
    end function parse_interface_block

end module parser_procedure_definitions_module
