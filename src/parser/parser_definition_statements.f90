module parser_definition_statements_module
    ! Parser module for definition statement types (function, subroutine, interface, derived types)
    use lexer_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_statements_module, only: parse_statement_in_if_block
    use ast_core
    use ast_factory
    use ast_types, only: LITERAL_STRING
    implicit none
    private

    public :: parse_derived_type, parse_function_definition, parse_subroutine_definition
    public :: parse_interface_block, parse_typed_parameters

contains

    ! Parse derived type parameters inside parentheses
    subroutine parse_derived_type_parameters(parser, arena, param_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: param_indices(:)

        type(token_t) :: token

        ! Initialize
        allocate (param_indices(0))

        ! Parse parameters separated by commas (simplified)
        do
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                exit
            end if

            if (token%kind == TK_IDENTIFIER) then
                ! Add parameter (simplified - would normally parse full parameter)
                token = parser%consume()
            end if

            ! Check for comma
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! consume ','
            else
                exit
            end if
        end do
    end subroutine parse_derived_type_parameters

    function parse_derived_type(parser, arena) result(type_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: type_index

        type(token_t) :: token
        character(len=:), allocatable :: type_name
        integer :: line, column
        logical :: has_parameters
        integer, allocatable :: param_indices(:)

        ! Consume 'type' keyword
        token = parser%consume()
        line = token%line
        column = token%column
        has_parameters = .false.

        ! Check for :: or just get type name
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "::") then
            ! Consume ::
            token = parser%consume()

            ! Get type name
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER) then
                token = parser%consume()
                type_name = token%text

                ! Check for parameters after type name
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "(") then
                    has_parameters = .true.
                    token = parser%consume()  ! consume '('
                    call parse_derived_type_parameters(parser, arena, param_indices)

                    ! Consume ')'
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == ")") then
                        token = parser%consume()
                    end if
                end if
            else
                type_name = "unnamed_type"
            end if
        else if (token%kind == TK_IDENTIFIER) then
            ! Direct type name
            token = parser%consume()
            type_name = token%text

            ! Check for parameters after type name
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                has_parameters = .true.
                token = parser%consume()  ! consume '('
                call parse_derived_type_parameters(parser, arena, param_indices)

                ! Consume ')'
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()
                end if
            end if
        else
            type_name = "unnamed_type"
        end if

        ! Create derived type node
        if (has_parameters .and. allocated(param_indices)) then
            type_index = push_derived_type(arena, type_name, &
                                            param_indices=param_indices, &
                                            line=line, column=column)
        else
            type_index = push_derived_type(arena, type_name, line=line, column=column)
        end if
    end function parse_derived_type

    ! Simplified typed parameters parsing
    subroutine parse_typed_parameters(parser, arena, param_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: param_indices(:)

        allocate(param_indices(0))
        ! Simplified implementation for refactoring
        ! Full implementation would require extensive parsing logic
    end subroutine parse_typed_parameters

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
            token = parser%peek()

            ! Check for end of function
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                ! Look ahead for "function"
                block
                    type(token_t), allocatable :: all_tokens(:)
                    integer :: next_idx
                    call parser%ensure_tokens_cached()
                    all_tokens = parser%get_all_tokens()
                    next_idx = parser%current_token + 1
                    if (next_idx <= size(all_tokens)) then
                        if (all_tokens(next_idx)%kind == TK_KEYWORD .and. &
                            all_tokens(next_idx)%text == "function") then
                            ! Consume "end function"
                            token = parser%consume()  ! consume "end"
                            token = parser%consume()  ! consume "function"
                            ! Optionally consume function name
                            if (.not. parser%is_at_end()) then
                                token = parser%peek()
                                if (token%kind == TK_IDENTIFIER .and. &
                                    token%text == function_name) then
                                    token = parser%consume()
                                end if
                            end if
                            exit  ! Exit the body parsing loop
                        end if
                    end if
                end block
            end if

            ! Skip empty lines
            if (token%kind == TK_NEWLINE) then
                token = parser%consume()
                cycle
            end if

            ! Collect tokens for the current statement line
            block
                type(token_t), allocatable :: stmt_tokens(:), all_tokens(:)
                integer :: stmt_start, stmt_end, i, stmt_size, stmt_index
                type(parser_state_t) :: block_parser
                
                call parser%ensure_tokens_cached()
                all_tokens = parser%get_all_tokens()
                stmt_start = parser%current_token
                stmt_end = stmt_start
                
                ! Find end of statement (end of line)
                do i = stmt_start, size(all_tokens)
                    if (i > stmt_start .and. all_tokens(i)%line /= token%line) exit
                    stmt_end = i
                end do
                
                ! Extract statement tokens
                stmt_size = stmt_end - stmt_start + 1
                if (stmt_size > 0) then
                    allocate(stmt_tokens(stmt_size + 1))
                    stmt_tokens(1:stmt_size) = all_tokens(stmt_start:stmt_end)
                    ! Add EOF token
                    stmt_tokens(stmt_size + 1)%kind = TK_EOF
                    stmt_tokens(stmt_size + 1)%text = ""
                    stmt_tokens(stmt_size + 1)%line = token%line
                    stmt_tokens(stmt_size + 1)%column = token%column + 1
                    
                    ! Parse the statement
                    block_parser = create_parser_state(stmt_tokens)
                    stmt_index = parse_statement_in_if_block(block_parser, arena, stmt_tokens(1))
                    
                    ! Add to body
                    if (stmt_index > 0) then
                        body_indices = [body_indices, stmt_index]
                    end if
                    
                    ! Advance parser position
                    parser%current_token = stmt_end + 1
                end if
            end block
        end do

        ! Create function node
        func_index = push_function_def(arena, function_name, param_indices, &
                                       result_variable_name, body_indices, &
                                       line, column)
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
            token = parser%peek()

            ! Check for end of subroutine
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                ! Look ahead for "subroutine"
                block
                    type(token_t), allocatable :: all_tokens(:)
                    integer :: next_idx
                    call parser%ensure_tokens_cached()
                    all_tokens = parser%get_all_tokens()
                    next_idx = parser%current_token + 1
                    if (next_idx <= size(all_tokens)) then
                        if (all_tokens(next_idx)%kind == TK_KEYWORD .and. &
                            all_tokens(next_idx)%text == "subroutine") then
                            ! Consume "end subroutine"
                            token = parser%consume()  ! consume "end"
                            token = parser%consume()  ! consume "subroutine"
                            ! Optionally consume subroutine name
                            if (.not. parser%is_at_end()) then
                                token = parser%peek()
                                if (token%kind == TK_IDENTIFIER .and. &
                                    token%text == subroutine_name) then
                                    token = parser%consume()
                                end if
                            end if
                            exit  ! Exit the body parsing loop
                        end if
                    end if
                end block
            end if

            ! Skip empty lines
            if (token%kind == TK_NEWLINE) then
                token = parser%consume()
                cycle
            end if

            ! Collect tokens for the current statement line
            block
                type(token_t), allocatable :: stmt_tokens(:), all_tokens(:)
                integer :: stmt_start, stmt_end, i, stmt_size, stmt_index
                type(parser_state_t) :: block_parser
                
                call parser%ensure_tokens_cached()
                all_tokens = parser%get_all_tokens()
                stmt_start = parser%current_token
                stmt_end = stmt_start
                
                ! Find end of statement (end of line)
                do i = stmt_start, size(all_tokens)
                    if (i > stmt_start .and. all_tokens(i)%line /= token%line) exit
                    stmt_end = i
                end do
                
                ! Extract statement tokens
                stmt_size = stmt_end - stmt_start + 1
                if (stmt_size > 0) then
                    allocate(stmt_tokens(stmt_size + 1))
                    stmt_tokens(1:stmt_size) = all_tokens(stmt_start:stmt_end)
                    ! Add EOF token
                    stmt_tokens(stmt_size + 1)%kind = TK_EOF
                    stmt_tokens(stmt_size + 1)%text = ""
                    stmt_tokens(stmt_size + 1)%line = token%line
                    stmt_tokens(stmt_size + 1)%column = token%column + 1
                    
                    ! Parse the statement
                    block_parser = create_parser_state(stmt_tokens)
                    stmt_index = parse_statement_in_if_block(block_parser, arena, stmt_tokens(1))
                    
                    ! Add to body
                    if (stmt_index > 0) then
                        body_indices = [body_indices, stmt_index]
                    end if
                    
                    ! Advance parser position
                    parser%current_token = stmt_end + 1
                end if
            end block
        end do

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

end module parser_definition_statements_module