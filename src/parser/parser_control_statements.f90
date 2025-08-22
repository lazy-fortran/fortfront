module parser_control_statements_module
    ! Parser module for control flow statement types (stop, return, goto, cycle, exit, error_stop)
    use lexer_core
    use parser_state_module
    use parser_expressions_module, only: parse_comparison
    use ast_core
    use ast_factory
    implicit none
    private

    public :: parse_stop_statement, parse_return_statement, parse_end_statement
    public :: parse_goto_statement, parse_error_stop_statement
    public :: parse_cycle_statement, parse_exit_statement

contains

    function parse_stop_statement(parser, arena) result(stop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stop_index
        
        type(token_t) :: token
        integer :: line, column, stop_code_index
        character(len=:), allocatable :: stop_message
        
        ! Consume 'stop' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()
        
        ! Check for optional stop code or message
        token = parser%peek()
        stop_code_index = 0
        stop_message = ""
        
        if (token%kind == TK_STRING) then
            ! String literal message
            stop_message = token%text
            token = parser%consume()
        else if (token%kind == TK_NUMBER .or. token%kind == TK_IDENTIFIER) then
            ! Integer expression or variable
            stop_code_index = parse_comparison(parser, arena)
        end if
        
        ! Create STOP node
        if (len_trim(stop_message) > 0) then
            stop_index = push_stop(arena, stop_message=stop_message, &
                                 line=line, column=column)
        else
            stop_index = push_stop(arena, stop_code_index=stop_code_index, &
                                 line=line, column=column)
        end if
    end function parse_stop_statement

    function parse_return_statement(parser, arena, parent_index) result(return_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer :: return_index
        
        type(token_t) :: token
        integer :: line, column
        
        ! Consume 'return' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()
        
        ! Create RETURN node
        return_index = push_return(arena, line=line, column=column, &
                                   parent_index=parent_index)
    end function parse_return_statement

    function parse_end_statement(parser, arena, parent_index) result(end_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: parent_index
        integer :: end_index
        
        type(token_t) :: token
        integer :: line, column
        
        ! Consume 'end' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()
        
        ! Create END statement node
        end_index = push_end_statement(arena, line=line, column=column, &
                                       parent_index=parent_index)
    end function parse_end_statement

    function parse_goto_statement(parser, arena) result(goto_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: goto_index
        
        type(token_t) :: token
        integer :: line, column
        character(len=:), allocatable :: label
        
        ! Consume 'go' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()
        
        ! Expect 'to' keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "to") then
            token = parser%consume()
            
            ! Expect label (number or identifier)
            token = parser%peek()
            if (token%kind == TK_NUMBER .or. token%kind == TK_IDENTIFIER) then
                label = token%text
                token = parser%consume()
            else
                ! Invalid goto - missing label
                label = ""
            end if
        else
            ! Invalid goto - missing 'to'
            label = ""
        end if
        
        ! Validate that we have a proper label
        if (.not. allocated(label) .or. len_trim(label) == 0) then
            ! Invalid GOTO statement - create a placeholder with error indication
            label = "INVALID_LABEL"
        end if
        
        ! Create GOTO node
        goto_index = push_goto(arena, label, line=line, column=column)
    end function parse_goto_statement

    function parse_error_stop_statement(parser, arena) result(error_stop_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: error_stop_index
        
        type(token_t) :: token
        integer :: line, column, error_code_index
        character(len=:), allocatable :: error_message
        
        ! Consume 'error' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()
        
        ! Consume 'stop' keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "stop") then
            token = parser%consume()
        else
            ! This shouldn't happen if called correctly, but handle gracefully
            error_stop_index = push_error_stop(arena, line=line, column=column)
            return
        end if
        
        ! Check for optional error code or message
        token = parser%peek()
        error_code_index = 0
        error_message = ""
        
        if (token%kind == TK_STRING) then
            ! String literal message
            error_message = token%text
            token = parser%consume()
        else if (token%kind == TK_NUMBER .or. token%kind == TK_IDENTIFIER) then
            ! Integer expression or variable
            error_code_index = parse_comparison(parser, arena)
            if (error_code_index <= 0) then
                ! Failed to parse error code expression - create basic error stop
                error_code_index = 0
            end if
        end if
        
        ! Create ERROR STOP node
        error_stop_index = push_error_stop(arena, error_code_index, error_message, &
                                          line=line, column=column)
    end function parse_error_stop_statement

    function parse_cycle_statement(parser, arena) result(cycle_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: cycle_index
        
        type(token_t) :: token
        integer :: line, column
        character(len=:), allocatable :: loop_label
        
        ! Consume 'cycle' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()
        
        ! Check for optional loop label
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            loop_label = token%text
            token = parser%consume()
        else
            loop_label = ""
        end if
        
        ! Create CYCLE node
        if (len_trim(loop_label) > 0) then
            cycle_index = push_cycle(arena, loop_label=loop_label, &
                                   line=line, column=column)
        else
            cycle_index = push_cycle(arena, line=line, column=column)
        end if
    end function parse_cycle_statement

    function parse_exit_statement(parser, arena) result(exit_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: exit_index
        
        type(token_t) :: token
        integer :: line, column
        character(len=:), allocatable :: loop_label
        
        ! Consume 'exit' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()
        
        ! Check for optional loop label
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            loop_label = token%text
            token = parser%consume()
        else
            loop_label = ""
        end if
        
        ! Create EXIT node
        if (len_trim(loop_label) > 0) then
            exit_index = push_exit(arena, loop_label=loop_label, &
                                 line=line, column=column)
        else
            exit_index = push_exit(arena, line=line, column=column)
        end if
    end function parse_exit_statement

end module parser_control_statements_module