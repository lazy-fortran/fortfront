module parser_statements_module
    ! Parser module for various statement types (use, include, print, etc.)
    use iso_fortran_env, only: error_unit
    use lexer_core
    use parser_state_module
    use parser_expressions_module, only: parse_comparison, parse_expression, parse_range
    use parser_declarations, only: parse_declaration, parse_multi_declaration
    use ast_core
    use ast_factory
    use ast_types, only: LITERAL_STRING
    implicit none
    private

    public :: parse_use_statement, parse_implicit_statement, parse_include_statement, parse_print_statement
    public :: parse_write_statement, parse_read_statement
    public :: parse_function_definition, parse_subroutine_definition
    public :: parse_interface_block, parse_module, parse_program_statement
    public :: parse_typed_parameters
    public :: parse_stop_statement, parse_return_statement
    public :: parse_goto_statement, parse_error_stop_statement
    public :: parse_cycle_statement, parse_exit_statement
    public :: parse_allocate_statement, parse_deallocate_statement
    public :: parse_call_statement

contains

    ! Parse use statement: use module_name [, only: item1, item2, new_name => old_name]
    function parse_use_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: module_name
        character(len=:), allocatable :: only_list(:)
        character(len=:), allocatable :: rename_list(:)
        logical :: has_only
        integer :: line, column

        ! Consume 'use' keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Get module name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            module_name = token%text
        else
            ! Invalid use statement - return placeholder
            stmt_index = push_literal(arena, "! Invalid use statement", &
                                       LITERAL_STRING, token%line, token%column)
            return
        end if

        has_only = .false.

        ! Check for optional only clause
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume()  ! consume ','

            ! Check for 'only' keyword
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "only") then
                token = parser%consume()  ! consume 'only'
                has_only = .true.

                ! Expect ':'
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ":") then
                    token = parser%consume()  ! consume ':'

                    ! Parse only list items
                    call parse_only_list(parser, only_list, rename_list)
                end if
            end if
        end if

        ! Create use statement node
        stmt_index = push_use_statement(arena, module_name, only_list, rename_list, &
                                        has_only, line, column, parent_index=0)

    end function parse_use_statement

    ! Parse implicit statement: implicit none | implicit type-spec (letter-spec-list)
    function parse_implicit_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: type_name
        integer :: kind_value, length_value
        logical :: has_kind, has_length, is_none
        character(len=64), allocatable :: letter_ranges(:)
        integer :: line, column, range_count, saved_pos
        logical :: is_type_spec
        
        ! Consume 'implicit' keyword
        token = parser%consume()
        line = token%line
        column = token%column
        
        ! Check for 'none'
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "none") then
            token = parser%consume()  ! consume 'none'
            is_none = .true.
            stmt_index = push_implicit_statement(arena, is_none, &
                                                line=line, column=column, parent_index=0)
            return
        end if
        
        ! Parse type specification
        is_none = .false.
        has_kind = .false.
        has_length = .false.
        kind_value = 0
        length_value = 0
        range_count = 0
        
        ! Get type name
        if (token%kind == TK_KEYWORD) then
            type_name = token%text
            token = parser%consume()
            
            ! Check for kind or length specification - but be careful to distinguish 
            ! from letter specification parentheses
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                ! Peek ahead to see if this looks like a type spec or letter spec
                ! Save parser position to backtrack if needed
                saved_pos = parser%current_token
                
                token = parser%consume()  ! consume '('
                token = parser%peek()
                
                is_type_spec = .false.
                
                if (type_name == "character") then
                    ! For character: check for "len" keyword or number
                    if ((token%kind == TK_KEYWORD .and. token%text == "len") .or. &
                        token%kind == TK_NUMBER) then
                        is_type_spec = .true.
                    end if
                else
                    ! For other types: check for number (kind specification)
                    if (token%kind == TK_NUMBER) then
                        is_type_spec = .true.
                    end if
                end if
                
                if (is_type_spec) then
                    ! Parse as type specification
                    if (type_name == "character") then
                        ! Handle character length: character(len=10) or character*10
                        if (token%kind == TK_KEYWORD .and. token%text == "len") then
                            token = parser%consume()  ! consume 'len'
                            token = parser%peek()
                            if (token%kind == TK_OPERATOR .and. token%text == "=") then
                                token = parser%consume()  ! consume '='
                                token = parser%peek()
                            end if
                        end if
                        if (token%kind == TK_NUMBER) then
                            read(token%text, *) length_value
                            has_length = .true.
                            token = parser%consume()
                        end if
                    else
                        ! Handle kind specification for other types
                        if (token%kind == TK_NUMBER) then
                            read(token%text, *) kind_value
                            has_kind = .true.
                            token = parser%consume()
                        end if
                    end if
                    
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == ")") then
                        token = parser%consume()  ! consume ')'
                    end if
                else
                    ! This is not a type specification, backtrack
                    parser%current_token = saved_pos
                end if
            end if
        end if
        
        ! Parse letter specification list in parentheses
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('
            
            ! Count letter ranges first
            call count_letter_ranges(parser, range_count)
            
            if (range_count > 0) then
                allocate(letter_ranges(range_count))
                call parse_letter_ranges(parser, letter_ranges)
            end if
            
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()  ! consume ')'
            end if
        end if
        
        ! Create implicit statement node
        stmt_index = push_implicit_statement(arena, is_none, type_name, kind_value, &
                                           has_kind, length_value, has_length, &
                                           letter_ranges, line, column, parent_index=0)
                                           
    end function parse_implicit_statement
    
    ! Helper to count letter ranges for allocation
    subroutine count_letter_ranges(parser, count)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(out) :: count
        type(token_t) :: token
        integer :: saved_pos
        
        count = 0
        saved_pos = parser%current_token
        
        do
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER .and. len(token%text) == 1) then
                count = count + 1
                token = parser%consume()
                
                ! Check for range (a-h)
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "-") then
                    token = parser%consume()  ! consume '-'
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER .and. len(token%text) == 1) then
                        token = parser%consume()  ! consume end letter
                    end if
                end if
                
                ! Check for comma
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()  ! consume ','
                else
                    exit
                end if
            else
                exit
            end if
        end do
        
        ! Restore parser position
        parser%current_token = saved_pos
    end subroutine count_letter_ranges
    
    ! Helper to parse letter ranges into array
    subroutine parse_letter_ranges(parser, letter_ranges)
        type(parser_state_t), intent(inout) :: parser
        character(len=64), intent(out) :: letter_ranges(:)
        type(token_t) :: token
        integer :: i
        
        i = 1
        do while (i <= size(letter_ranges))
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER .and. len(token%text) == 1) then
                letter_ranges(i) = token%text
                token = parser%consume()
                
                ! Check for range (a-h)  
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "-") then
                    token = parser%consume()  ! consume '-'
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER .and. len(token%text) == 1) then
                        letter_ranges(i) = trim(letter_ranges(i)) // "-" // token%text
                        token = parser%consume()  ! consume end letter
                    end if
                end if
                
                i = i + 1
                
                ! Check for comma
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then  
                    token = parser%consume()  ! consume ','
                else
                    exit
                end if
            else
                exit
            end if
        end do
    end subroutine parse_letter_ranges

    ! Parse include statement: include 'filename'
    function parse_include_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: filename
        integer :: line, column

        ! Consume "include" keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Expect filename (string literal)
        token = parser%consume()
        if (token%kind == TK_STRING) then
            filename = token%text
        else
            ! Error handling - for now just use empty string
            filename = ""
        end if

        ! Create include statement node
        stmt_index = push_include_statement(arena, filename, line, column)

    end function parse_include_statement

    ! Parse print statement: print format_spec, arg1, arg2, ...
    function parse_print_statement(parser, arena) result(print_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: print_index

        type(token_t) :: token
        integer, allocatable :: arg_indices(:)
        integer :: line, column, arg_count
        character(len=:), allocatable :: format_spec

        ! Consume 'print' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()

        ! Parse format spec (*, format string, or format variable)
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "*") then
            format_spec = "*"
            token = parser%consume()
        else if (token%kind == TK_STRING) then
            ! Format string like '(a,f5.1,a,f5.1,a,f5.1)'
            format_spec = token%text
            token = parser%consume()
        else if (token%kind == TK_IDENTIFIER) then
            ! Format variable
            format_spec = token%text
            token = parser%consume()
        else
            format_spec = "*"  ! Default
        end if

        ! Skip comma after format spec if present
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume()
        end if

        ! Parse all print arguments
        arg_count = 0
        allocate (arg_indices(0))

        if (.not. parser%is_at_end()) then
            block
                integer :: current_arg_index

                ! Parse first argument
                current_arg_index = parse_comparison(parser, arena)
                if (current_arg_index > 0) then
                    arg_count = 1
                    arg_indices = [current_arg_index]

                    ! Parse additional arguments separated by commas
                    do
                        token = parser%peek()
                        if (token%kind /= TK_OPERATOR .or. token%text /= ",") exit

                        ! Consume comma
                        token = parser%consume()

                        ! Parse next argument
                        current_arg_index = parse_comparison(parser, arena)
                        if (current_arg_index > 0) then
                            arg_indices = [arg_indices, current_arg_index]
                            arg_count = arg_count + 1
                        else
                            exit
                        end if
                    end do
                end if
            end block
        end if

        ! Create print statement node with parsed arguments
       print_index = push_print_statement(arena, format_spec, arg_indices, line, column)

    end function parse_print_statement
    
    ! Parse write statement: write(unit, format) arg1, arg2, ...
    function parse_write_statement(parser, arena) result(write_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: write_index

        type(token_t) :: token
        integer, allocatable :: arg_indices(:)
        integer :: line, column, arg_count
        character(len=:), allocatable :: unit_spec, format_spec

        ! Consume 'write' keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Expect opening parenthesis
        token = parser%consume()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            write(error_unit, *) "Error: Expected '(' after 'write' at line ", &
                token%line
            write_index = 0
            return
        end if

        ! Parse unit specifier
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "*") then
            unit_spec = "*"
            token = parser%consume()
        else if (token%kind == TK_NUMBER) then
            unit_spec = token%text
            token = parser%consume()
        else if (token%kind == TK_IDENTIFIER) then
            unit_spec = token%text
            token = parser%consume()
        else
            write(error_unit, *) &
                "Error: Expected unit specifier in write statement at line ", &
                token%line
            write_index = 0
            return
        end if

        ! Check for format specifier (optional)
        format_spec = ""
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume() ! consume comma
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "*") then
                format_spec = "*"
                token = parser%consume()
            else if (token%kind == TK_STRING) then
                format_spec = token%text
                token = parser%consume()
            else if (token%kind == TK_IDENTIFIER) then
                format_spec = token%text
                token = parser%consume()
            end if
        end if

        ! Expect closing parenthesis
        token = parser%consume()
        if (token%kind /= TK_OPERATOR .or. token%text /= ")") then
            write(error_unit, *) &
                "Error: Expected ')' after write unit and format at line ", &
                token%line
            write_index = 0
            return
        end if

        ! Parse all write arguments
        arg_count = 0
        allocate (arg_indices(0))

        if (.not. parser%is_at_end()) then
            block
                integer :: current_arg_index

                ! Parse first argument
                current_arg_index = parse_comparison(parser, arena)
                if (current_arg_index > 0) then
                    arg_count = arg_count + 1
                    arg_indices = [arg_indices, current_arg_index]
                end if

                ! Parse remaining arguments
                do
                    if (parser%is_at_end()) exit
                    token = parser%peek()
                    if (token%kind /= TK_OPERATOR .or. token%text /= ",") exit
                    
                    token = parser%consume() ! consume comma
                    current_arg_index = parse_comparison(parser, arena)
                    if (current_arg_index > 0) then
                        arg_count = arg_count + 1
                        arg_indices = [arg_indices, current_arg_index]
                    else
                        exit
                    end if
                end do
            end block
        end if

        ! Create write statement node with parsed arguments
        write_index = push_write_statement(arena, unit_spec, arg_indices, &
                                            format_spec, line, column)

    end function parse_write_statement
    
    ! Parse read statement: read(unit, format) var1, var2, ...
    function parse_read_statement(parser, arena) result(read_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: read_index

        type(token_t) :: token
        integer, allocatable :: var_indices(:)
        integer :: line, column, var_count
        character(len=:), allocatable :: unit_spec, format_spec

        ! Consume 'read' keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Expect opening parenthesis
        token = parser%consume()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            write(error_unit, *) "Error: Expected '(' after 'read' at line ", token%line
            read_index = 0
            return
        end if

        ! Parse unit specifier
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "*") then
            unit_spec = "*"
            token = parser%consume()
        else if (token%kind == TK_NUMBER) then
            unit_spec = token%text
            token = parser%consume()
        else if (token%kind == TK_IDENTIFIER) then
            unit_spec = token%text
            token = parser%consume()
        else
            write(error_unit, *) &
                "Error: Expected unit specifier in read statement at line ", &
                token%line
            read_index = 0
            return
        end if

        ! Check for format specifier (optional)
        format_spec = ""
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume() ! consume comma
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "*") then
                format_spec = "*"
                token = parser%consume()
            else if (token%kind == TK_STRING) then
                format_spec = token%text
                token = parser%consume()
            else if (token%kind == TK_IDENTIFIER) then
                format_spec = token%text
                token = parser%consume()
            end if
        end if

        ! Expect closing parenthesis
        token = parser%consume()
        if (token%kind /= TK_OPERATOR .or. token%text /= ")") then
            write(error_unit, *) &
                "Error: Expected ')' after read unit and format at line ", &
                token%line
            read_index = 0
            return
        end if

        ! Parse all read variables
        var_count = 0
        allocate (var_indices(0))

        if (.not. parser%is_at_end()) then
            block
                integer :: current_var_index

                ! Parse first variable
                current_var_index = parse_comparison(parser, arena)
                if (current_var_index > 0) then
                    var_count = var_count + 1
                    var_indices = [var_indices, current_var_index]
                end if

                ! Parse remaining variables
                do
                    if (parser%is_at_end()) exit
                    token = parser%peek()
                    if (token%kind /= TK_OPERATOR .or. token%text /= ",") exit
                    
                    token = parser%consume() ! consume comma
                    current_var_index = parse_comparison(parser, arena)
                    if (current_var_index > 0) then
                        var_count = var_count + 1
                        var_indices = [var_indices, current_var_index]
                    else
                        exit
                    end if
                end do
            end block
        end if

        ! Create read statement node with parsed variables
        read_index = push_read_statement(arena, unit_spec, var_indices, &
                                          format_spec, line, column)

    end function parse_read_statement
    
    ! Parse STOP statement: stop [stop-code]
    ! stop-code can be an integer expression or string literal
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
    
    ! Parse RETURN statement
    function parse_return_statement(parser, arena) result(return_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: return_index
        
        type(token_t) :: token
        integer :: line, column
        
        ! Consume 'return' keyword
        token = parser%peek()
        line = token%line
        column = token%column
        token = parser%consume()
        
        ! Create RETURN node
        return_index = push_return(arena, line=line, column=column)
    end function parse_return_statement
    
    ! Parse GOTO statement: go to label
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
    
    ! Parse ERROR STOP statement: error stop [stop-code | stop-string]
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
    
    ! Parse CYCLE statement: cycle [loop-label]
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
    
    ! Parse EXIT statement: exit [loop-label]
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

    ! Parse only list helper
    subroutine parse_only_list(parser, only_list, rename_list)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: only_list(:)
        character(len=:), allocatable, intent(out) :: rename_list(:)

        character(len=100) :: temp_only(50)
        character(len=100) :: temp_rename(50)
        type(token_t) :: token
        integer :: only_count, rename_count, i
        character(len=:), allocatable :: item_name, old_name

        only_count = 0
        rename_count = 0
        do i = 1, 50
            temp_only(i) = repeat(' ', 100)
            temp_rename(i) = repeat(' ', 100)
        end do

        ! Parse first item
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            item_name = token%text

            ! Check if this is a rename (new_name => old_name)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "=") then
                token = parser%consume()  ! consume '='
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ">") then
                    token = parser%consume()  ! consume '>'

                    ! Get old name
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER) then
                        token = parser%consume()
                        old_name = token%text

                        ! Store rename: "new_name => old_name"
                        rename_count = rename_count + 1
                        temp_rename(rename_count) = item_name//" => "//old_name
                    end if
                end if
            else
                ! Regular only item
                only_count = only_count + 1
                temp_only(only_count) = item_name
            end if
        end if

        ! Parse remaining items
        do
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! consume ','

                ! Get next item
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
                    token = parser%consume()
                    item_name = token%text

                    ! Check if this is a rename
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == "=") then
                        token = parser%consume()  ! consume '='
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == ">") then
                            token = parser%consume()  ! consume '>'

                            ! Get old name
                            token = parser%peek()
                            if (token%kind == TK_IDENTIFIER) then
                                token = parser%consume()
                                old_name = token%text

                                ! Store rename: "new_name => old_name"
                                rename_count = rename_count + 1
                                temp_rename(rename_count) = item_name//" => "//old_name
                            end if
                        end if
                    else
                        ! Regular only item
                        only_count = only_count + 1
                        temp_only(only_count) = item_name
                    end if
                else
                    ! No more items
                    exit
                end if
            else
                ! No more items
                exit
            end if
        end do

        ! Copy to output arrays with proper size
        if (only_count > 0) then
            allocate (character(len=100) :: only_list(only_count))
            do i = 1, only_count
                only_list(i) = trim(adjustl(temp_only(i)))
            end do
        else
            allocate (character(len=100) :: only_list(0))
        end if

        if (rename_count > 0) then
            allocate (character(len=100) :: rename_list(rename_count))
            do i = 1, rename_count
                rename_list(i) = trim(temp_rename(i))
            end do
        else
            allocate (character(len=100) :: rename_list(0))
        end if

    end subroutine parse_only_list

    ! Parse derived type definition: type :: type_name or type(params) :: type_name
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

                ! Check for parameters after type name: type :: matrix(n, m)
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "(") then
                    ! Parse parameters
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
            ! Direct type name (like "type point")
            token = parser%consume()
            type_name = token%text

            ! Check for parameters after type name: type matrix(n, m)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                ! Parse parameters
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

    ! Parse derived type parameters inside parentheses
    subroutine parse_derived_type_parameters(parser, arena, param_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: param_indices(:)

        type(token_t) :: token
        integer :: param_count

        ! Initialize
        param_count = 0
        allocate (param_indices(0))

        ! Parse parameters separated by commas
        do
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                ! End of parameters
                exit
            end if

            ! Parse a single parameter
            if (token%kind == TK_IDENTIFIER) then
                token = parser%consume()
                param_indices = [param_indices, push_identifier(arena, &
                    token%text, token%line, token%column)]
                param_count = param_count + 1
            else if (token%kind == TK_NUMBER) then
                token = parser%consume()
                param_indices = [param_indices, push_literal(arena, &
                    token%text, LITERAL_INTEGER, token%line, token%column)]
                param_count = param_count + 1
            else
                ! Unknown parameter type - consume it as identifier
                token = parser%consume()
                param_indices = [param_indices, push_identifier(arena, &
                    token%text, token%line, token%column)]
                param_count = param_count + 1
            end if

            ! Check for comma
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! Consume comma
                cycle
            else
                exit
            end if
        end do

    end subroutine parse_derived_type_parameters

    ! Parse function/subroutine parameters with explicit type declarations
    subroutine parse_typed_parameters(parser, arena, param_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: param_indices(:)

        type(token_t) :: token
        integer :: param_count
        logical :: parsing_type_spec
        character(len=:), allocatable :: current_type, current_kind_str
        integer :: current_kind, current_intent
        logical :: current_is_optional
        integer, allocatable :: temp_params(:)
        integer :: line, column
        character(len=:), allocatable :: type_expr
        integer :: paren_count

        param_count = 0
        allocate (param_indices(0))
        parsing_type_spec = .false.

        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for closing parenthesis
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                exit
            end if

            ! Check for comma (parameter separator)
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
                parsing_type_spec = .false.  ! Reset after comma
                cycle
            end if

            ! Check for type keywords (real, integer, etc.)
            if (token%kind == TK_KEYWORD .and. &
                (token%text == "real" .or. token%text == "integer" .or. &
                 token%text == "logical" .or. token%text == "character" .or. &
                 token%text == "type")) then

                parsing_type_spec = .true.
                current_type = token%text
                current_kind = 0
                current_intent = 0
                current_is_optional = .false.
                line = token%line
                column = token%column
                token = parser%consume()

                ! Check for kind specification (e.g., real(8)) or type &
                ! specification (e.g., type(name))
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "(") then
                    token = parser%consume()  ! consume '('

                    ! For type keywords, parse the complete type expression &
                    ! including nested parentheses
                    if (current_type == "type") then
                        type_expr = ""
                        paren_count = 1  ! We already consumed the opening parenthesis

                        do while (.not. parser%is_at_end() .and. paren_count > 0)
                            token = parser%peek()
                            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                                paren_count = paren_count + 1
                                type_expr = type_expr//token%text
                                token = parser%consume()
                        else if (token%kind == TK_OPERATOR .and. token%text == ")") then
                                paren_count = paren_count - 1
                                if (paren_count > 0) then
                                    type_expr = type_expr//token%text
                                end if
                                token = parser%consume()
                            else
                                type_expr = type_expr//token%text
                                token = parser%consume()
                            end if
                        end do

                        current_type = current_type//"("//type_expr//")"
                    else
                        ! For non-type keywords, expect a simple number for &
                        ! kind specification
                        token = parser%peek()
                        if (token%kind == TK_NUMBER) then
                            read (token%text, *) current_kind
                            token = parser%consume()
                        end if

                        ! Consume closing ')'
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == ")") then
                            token = parser%consume()
                        end if
                    end if
                end if

                ! Check for comma followed by attributes
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()  ! consume ','

                    ! Check for intent
                    token = parser%peek()
                    if ((token%kind == TK_KEYWORD .or. &
                         token%kind == TK_IDENTIFIER) .and. &
                        token%text == "intent") then
                        token = parser%consume()  ! consume 'intent'

                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "(") then
                            token = parser%consume()  ! consume '('

                            token = parser%peek()
                            if (token%kind == TK_KEYWORD .or. &
                                token%kind == TK_IDENTIFIER) then
                                if (token%text == "in") then
                                    current_intent = 1
                                else if (token%text == "out") then
                                    current_intent = 2
                                else if (token%text == "inout") then
                                    current_intent = 3
                                end if
                                token = parser%consume()
                            end if

                            ! Consume ')'
                            token = parser%peek()
                            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                                token = parser%consume()
                            end if
                        end if
                 else if (token%kind == TK_KEYWORD .and. token%text == "dimension") then
                        ! Skip dimension attribute for now
                        token = parser%consume()
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "(") then
                            ! Skip dimension specification
                            token = parser%consume()
                            do while (.not. parser%is_at_end())
                                token = parser%peek()
                             if (token%kind == TK_OPERATOR .and. token%text == ")") then
                                    token = parser%consume()
                                    exit
                                end if
                                token = parser%consume()
                            end do
                        end if
                    else if (token%kind == TK_KEYWORD .and. &
                             token%text == "optional") then
                        ! Mark current type as optional
                        current_is_optional = .true.
                        token = parser%consume()
                    end if
                end if

                ! Expect :: after type specification
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "::") then
                    token = parser%consume()
                end if

                ! Now collect all parameter names for this type
                allocate (temp_params(0))
                do while (.not. parser%is_at_end())
                    token = parser%peek()

                    if (token%kind == TK_IDENTIFIER) then
                        ! Parse parameter name and check for array specification
                        block
                            character(len=:), allocatable :: param_name
                            integer :: param_index
                            integer, allocatable :: dim_indices(:)
                            integer :: dim_count

                            param_name = token%text
                            token = parser%consume()

                            ! Check for array specification
                            allocate (dim_indices(0))
                            dim_count = 0
                            token = parser%peek()
                            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                                token = parser%consume()  ! consume '('

                                ! Parse array dimensions
                                do while (.not. parser%is_at_end())
                                    token = parser%peek()
                             if (token%kind == TK_OPERATOR .and. token%text == ")") then
                                        token = parser%consume()  ! consume ')'
                                        exit
                                    end if

                                    ! Parse dimension expression (for now, create &
                                    ! identifier or literal nodes)
                             if (token%kind == TK_OPERATOR .and. token%text == ":") then
                                        ! Assumed shape (:)
                                        block
                                            integer :: dim_index
                       dim_index = push_identifier(arena, ":", token%line, token%column)
                                            dim_indices = [dim_indices, dim_index]
                                            dim_count = dim_count + 1
                                        end block
                                        token = parser%consume()
                        else if (token%kind == TK_OPERATOR .and. token%text == "*") then
                                        ! Assumed size (*)
                                        block
                                            integer :: dim_index
                       dim_index = push_identifier(arena, "*", token%line, token%column)
                                            dim_indices = [dim_indices, dim_index]
                                            dim_count = dim_count + 1
                                        end block
                                        token = parser%consume()
                                    else if (token%kind == TK_NUMBER) then
                                        ! Explicit size
                                        block
                                            integer :: dim_index
                          dim_index = push_literal(arena, token%text, LITERAL_INTEGER, &
                                                               token%line, token%column)
                                            dim_indices = [dim_indices, dim_index]
                                            dim_count = dim_count + 1
                                        end block
                                        token = parser%consume()
                                    else if (token%kind == TK_IDENTIFIER) then
                                        ! Variable size
                                        block
                                            integer :: dim_index
                dim_index = push_identifier(arena, token%text, token%line, token%column)
                                            dim_indices = [dim_indices, dim_index]
                                            dim_count = dim_count + 1
                                        end block
                                        token = parser%consume()
                                    else
                                        ! Skip unrecognized tokens
                                        token = parser%consume()
                                    end if

                                    ! Check for comma (more dimensions)
                                    token = parser%peek()
                             if (token%kind == TK_OPERATOR .and. token%text == ",") then
                                        token = parser%consume()  ! consume ','
                                    end if
                                end do
                            end if

                            ! Create parameter declaration node with array info
                            if (dim_count > 0) then
             param_index = push_parameter_declaration(arena, param_name, current_type, &
                                                         current_kind, current_intent, &
                                                         current_is_optional, dim_indices, line, column)
                            else
             param_index = push_parameter_declaration(arena, param_name, current_type, &
                                                         current_kind, current_intent, &
                                                         current_is_optional, &
                                                               line=line, column=column)
                            end if
                            temp_params = [temp_params, param_index]
                        end block

                        ! Check for comma (more params of same type)
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == ",") then
                            ! Peek ahead to see if next is a type keyword
                            if (parser%current_token + 1 <= size(parser%tokens)) then
                                block
                                    type(token_t) :: next_token
                                    next_token = parser%tokens(parser%current_token + 1)
                                    if (next_token%kind == TK_KEYWORD .and. &
                     (next_token%text == "real" .or. next_token%text == "integer" .or. &
                 next_token%text == "logical" .or. next_token%text == "character")) then
                                        ! This comma separates different type groups
                                        exit
                                    else
                                        ! This comma separates params of same type
                                        token = parser%consume()  ! consume comma
                                        cycle
                                    end if
                                end block
                            else
                                token = parser%consume()  ! consume comma
                                cycle
                            end if
                        else
                            ! No more params of this type
                            exit
                        end if
                    else
                        ! Not an identifier, stop collecting params for this type
                        exit
                    end if
                end do

                ! Add collected params to main list
                param_indices = [param_indices, temp_params]
                param_count = param_count + size(temp_params)
                deallocate (temp_params)

            else if (token%kind == TK_IDENTIFIER .and. .not. parsing_type_spec) then
                ! Simple parameter without explicit type
                ! Create a parameter_declaration_node even without type info
                block
                    integer :: param_index
                    param_index = push_parameter_declaration(arena, &
                        name=token%text, &
                        type_name="", &  ! Empty type, will be inferred from body
                        kind_value=0, &
                        intent_value=0, &  ! No intent specified in parameter list
                        is_optional=.false., &  ! Not optional by default
                        line=token%line, &
                        column=token%column)
                    param_indices = [param_indices, param_index]
                    param_count = param_count + 1
                end block
                token = parser%consume()
            else
                ! Skip unexpected token
                token = parser%consume()
            end if
        end do

    end subroutine parse_typed_parameters

    function parse_function_definition(parser, arena) result(func_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: func_index

        character(len=:), allocatable :: return_type_str, func_name
        type(token_t) :: token
        integer, allocatable :: param_indices(:), body_indices(:)
        integer :: return_type_index
        integer :: line, column

        ! Initialize
        return_type_str = ""

        ! Check if we have a return type
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. &
            (token%text == "real" .or. token%text == "integer" .or. &
             token%text == "logical" .or. token%text == "character")) then
            return_type_str = token%text
            token = parser%consume()
        end if

        ! Expect "function" keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "function") then
            line = token%line
            column = token%column
            token = parser%consume()
        else
            ! Error - not a function definition
            func_index = push_literal(arena, &
                "! Error: Expected function keyword", LITERAL_STRING, 1, 1)
            return
        end if

        ! Get function name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            func_name = token%text
            token = parser%consume()
        else
            ! Error - missing function name, create empty function
            func_name = "unnamed_function"
        end if

        ! Parse parameters
        ! Look for opening parenthesis
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()

            ! Use new typed parameter parser
            call parse_typed_parameters(parser, arena, param_indices)

            ! Consume closing parenthesis if not already consumed
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
            end if
        else
            allocate (param_indices(0))
        end if

        ! Parse function body (collect all statements until "end function")
        block
            integer :: stmt_index
            integer :: body_count, i, stmt_start
            type(token_t), allocatable :: stmt_tokens(:)

            body_count = 0
            allocate (body_indices(0))

            ! Parse statements until we hit "end function"
            do while (.not. parser%is_at_end())
                token = parser%peek()

                ! Check for "end function" BEFORE trying to parse as statement
                if (token%kind == TK_KEYWORD .and. token%text == "end") then
                    ! Look ahead for "function"
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                        block
                            type(token_t) :: next_token
                            next_token = parser%tokens(parser%current_token + 1)
             if (next_token%kind == TK_KEYWORD .and. next_token%text == "function") then
                                ! Consume "end function" and any trailing &
                                ! tokens on same line
                                token = parser%consume()  ! consume "end"
                                token = parser%consume()  ! consume "function"

                                ! Also consume any optional function name on same line
                                if (.not. parser%is_at_end()) then
                                    token = parser%peek()
                                    if (token%kind == TK_IDENTIFIER) then
                    if (token%line == parser%tokens(parser%current_token - 1)%line) then
                                            token = parser%consume()  ! consume function name
                                        end if
                                    end if
                                end if

                                exit
                            end if
                        end block
                    end if
                end if

                ! Skip empty lines
                if (token%kind == TK_EOF .or. token%kind == TK_NEWLINE) then
                    token = parser%consume()
                    cycle
                end if

                ! Don't try to parse standalone "end" lines - skip them
                if (token%kind == TK_KEYWORD .and. token%text == "end") then
                    ! This is a standalone "end" that's not followed by "function"
                    ! Skip the entire line to avoid parser errors
                    do while (.not. parser%is_at_end())
                        token = parser%consume()
                        if (parser%current_token <= size(parser%tokens)) then
                        if (parser%tokens(parser%current_token)%line /= token%line) exit
                        else
                            exit
                        end if
                    end do
                    cycle
                end if

                ! Collect tokens for the current statement (until newline &
                ! or end of line)
                stmt_start = parser%current_token
                i = stmt_start
                do while (i <= size(parser%tokens))
                    if (parser%tokens(i)%line /= token%line) exit
                    i = i + 1
                end do

                ! Extract statement tokens
                if (i > stmt_start) then
                    allocate (stmt_tokens(i - stmt_start + 1))
                    stmt_tokens(1:i - stmt_start) = parser%tokens(stmt_start:i - 1)
                    ! Add EOF token
                    stmt_tokens(i - stmt_start + 1)%kind = TK_EOF
                    stmt_tokens(i - stmt_start + 1)%text = ""
                    stmt_tokens(i - stmt_start + 1)%line = parser%tokens(i - 1)%line
                stmt_tokens(i - stmt_start + 1)%column = parser%tokens(i - 1)%column + 1

                    ! Parse the statement (may return multiple indices for &
                    ! multi-variable declarations)
                    block
                        integer, allocatable :: stmt_indices(:)
                        integer :: j
                        stmt_indices = parse_basic_statement_multi(stmt_tokens, arena)

                        ! Add all parsed statements to body
                        do j = 1, size(stmt_indices)
                            if (stmt_indices(j) > 0) then
                                body_indices = [body_indices, stmt_indices(j)]
                                body_count = body_count + 1
                            end if
                        end do
                    end block

                    deallocate (stmt_tokens)

                    ! Advance parser to next statement
                    parser%current_token = i
                else
                    ! Skip to next token if we can't parse this statement
                    token = parser%consume()
                end if
            end do
        end block

        ! Create return type node from string
        if (len_trim(return_type_str) > 0) then
            return_type_index = push_identifier(arena, return_type_str, line, column)
        else
            return_type_index = push_identifier(arena, "", line, column)
        end if

        ! Create function definition node with body_indices from parsed function body
        func_index = push_function_def(arena, func_name, param_indices, &
                                       return_type_str, body_indices, line, &
                                       column)

    end function parse_function_definition

    function parse_subroutine_definition(parser, arena) result(sub_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: sub_index

        character(len=:), allocatable :: sub_name
        type(token_t) :: token
        integer, allocatable :: param_indices(:), body_indices(:)
        integer :: line, column

        ! Expect "subroutine" keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "subroutine") then
            line = token%line
            column = token%column
            token = parser%consume()
        else
            ! Error: expected subroutine
            return
        end if

        ! Get subroutine name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            sub_name = token%text
            token = parser%consume()
        else
            ! Error: expected subroutine name
            return
        end if

        ! Parse parameters
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('

            ! Use new typed parameter parser
            call parse_typed_parameters(parser, arena, param_indices)

            ! Consume closing parenthesis if not already consumed
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
            end if
        else
            ! No parameters
            allocate (param_indices(0))
        end if

        ! Parse subroutine body (collect all statements until "end subroutine")
        block
            integer :: stmt_index
            integer :: body_count, i, stmt_start
            type(token_t), allocatable :: stmt_tokens(:)

            body_count = 0
            allocate (body_indices(0))

            ! Parse statements until we hit "end subroutine"
            do while (.not. parser%is_at_end())
                token = parser%peek()

                ! Check for "end subroutine"
                if (token%kind == TK_KEYWORD .and. token%text == "end") then
                    ! Look ahead for "subroutine"
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                        block
                            type(token_t) :: next_token
                            next_token = parser%tokens(parser%current_token + 1)
           if (next_token%kind == TK_KEYWORD .and. next_token%text == "subroutine") then
                                ! Consume "end subroutine"
                                token = parser%consume()
                                token = parser%consume()
                                exit
                            end if
                        end block
                    end if
                end if

                ! Collect tokens for the current statement (until newline &
                ! or end of line)
                stmt_start = parser%current_token
                i = stmt_start
                do while (i <= size(parser%tokens))
                    if (parser%tokens(i)%line /= token%line) exit
                    i = i + 1
                end do

                ! Extract statement tokens
                if (i > stmt_start) then
                    allocate (stmt_tokens(i - stmt_start + 1))
                    stmt_tokens(1:i - stmt_start) = parser%tokens(stmt_start:i - 1)
                    ! Add EOF token
                    stmt_tokens(i - stmt_start + 1)%kind = TK_EOF
                    stmt_tokens(i - stmt_start + 1)%text = ""
                    stmt_tokens(i - stmt_start + 1)%line = parser%tokens(i - 1)%line
                stmt_tokens(i - stmt_start + 1)%column = parser%tokens(i - 1)%column + 1

                    ! Parse the statement (may return multiple indices for &
                    ! multi-variable declarations)
                    block
                        integer, allocatable :: stmt_indices(:)
                        integer :: j
                        stmt_indices = parse_basic_statement_multi(stmt_tokens, arena)

                        ! Add all parsed statements to body
                        do j = 1, size(stmt_indices)
                            if (stmt_indices(j) > 0) then
                                body_indices = [body_indices, stmt_indices(j)]
                                body_count = body_count + 1
                            end if
                        end do
                    end block

                    deallocate (stmt_tokens)

                    ! Advance parser to next statement
                    parser%current_token = i
                else
                    ! Skip to next token if we can't parse this statement
                    token = parser%consume()
                end if
            end do
        end block
        
        ! Update parameter nodes with attributes from body declarations
        if (allocated(param_indices) .and. allocated(body_indices)) then
            block
                use ast_nodes_data, only: parameter_declaration_node, declaration_node
                use ast_nodes_core, only: identifier_node
                integer :: param_idx, body_idx
                
                ! Iterate through each parameter
                do param_idx = 1, size(param_indices)
                    if (param_indices(param_idx) > 0 .and. &
                        param_indices(param_idx) <= arena%size) then
                        if (allocated(arena%entries(param_indices(param_idx))%node)) then
                            ! Get parameter node
                            select type (param_node => &
                                        arena%entries(param_indices(param_idx))%node)
                            type is (identifier_node)
                                ! Convert identifier_node to parameter_declaration_node
                                ! This handles the ambiguous case elegantly
                                block
                                    type(parameter_declaration_node) :: new_param_node
                                    logical :: name_matches
                                    integer :: name_idx
                                    
                                    ! Initialize new parameter node with identifier info
                                    new_param_node%name = param_node%name
                                    new_param_node%type_name = ""  ! Will be filled from body
                                    new_param_node%intent_type = 0  ! No intent by default
                                    new_param_node%is_optional = .false.
                                    new_param_node%line = param_node%line
                                    new_param_node%column = param_node%column
                                    
                                    ! Search body for matching declaration to fill attributes
                                    do body_idx = 1, size(body_indices)
                                        if (body_indices(body_idx) > 0 .and. &
                                            body_indices(body_idx) <= arena%size) then
                                            if (allocated(arena%entries(body_indices(body_idx))%node)) then
                                                select type (body_node => &
                                                            arena%entries(body_indices(body_idx))%node)
                                                type is (declaration_node)
                                                    name_matches = .false.
                                                    
                                                    ! Check single declaration
                                                    if (allocated(body_node%var_name)) then
                                                        if (new_param_node%name == body_node%var_name) then
                                                            name_matches = .true.
                                                        end if
                                                    end if
                                                    
                                                    ! Check multi-declaration var_names array
                                                    if (.not. name_matches .and. allocated(body_node%var_names)) then
                                                        do name_idx = 1, size(body_node%var_names)
                                                            if (new_param_node%name == trim(body_node%var_names(name_idx))) then
                                                                name_matches = .true.
                                                                exit
                                                            end if
                                                        end do
                                                    end if
                                                    
                                                    if (name_matches) then
                                                        ! Fill attributes from body declaration
                                                        new_param_node%type_name = body_node%type_name
                                                        if (body_node%has_intent) then
                                                            if (body_node%intent == "in") then
                                                                new_param_node%intent_type = 1  ! INTENT_IN
                                                            else if (body_node%intent == "out") then
                                                                new_param_node%intent_type = 2  ! INTENT_OUT
                                                            else if (body_node%intent == "inout") then
                                                                new_param_node%intent_type = 3  ! INTENT_INOUT
                                                            end if
                                                        end if
                                                        new_param_node%is_optional = body_node%is_optional
                                                        exit  ! Found matching declaration
                                                    end if
                                                end select
                                            end if
                                        end if
                                    end do
                                    
                                    ! Replace the identifier_node with parameter_declaration_node in arena
                                    deallocate(arena%entries(param_indices(param_idx))%node)
                                    allocate(arena%entries(param_indices(param_idx))%node, source=new_param_node)
                                end block
                                
                            type is (parameter_declaration_node)
                                ! Search body for matching declaration
                                do body_idx = 1, size(body_indices)
                                    if (body_indices(body_idx) > 0 .and. &
                                        body_indices(body_idx) <= arena%size) then
                                        if (allocated(arena%entries(body_indices(body_idx))%node)) then
                                            select type (body_node => &
                                                        arena%entries(body_indices(body_idx))%node)
                                            type is (declaration_node)
                                                ! Check if names match - handle both single and multi-declarations
                                                block
                                                    logical :: name_matches
                                                    integer :: name_idx
                                                    
                                                    name_matches = .false.
                                                    
                                                    ! Check single declaration
                                                    if (allocated(body_node%var_name)) then
                                                        if (param_node%name == body_node%var_name) then
                                                            name_matches = .true.
                                                        end if
                                                    end if
                                                    
                                                    ! Check multi-declaration var_names array
                                                    if (.not. name_matches .and. allocated(body_node%var_names)) then
                                                        do name_idx = 1, size(body_node%var_names)
                                                            if (param_node%name == trim(body_node%var_names(name_idx))) then
                                                                name_matches = .true.
                                                                exit
                                                            end if
                                                        end do
                                                    end if
                                                    
                                                    if (name_matches) then
                                                    ! Update parameter with body declaration attributes
                                                    if (body_node%has_intent) then
                                                        ! Convert intent string to enum
                                                        if (body_node%intent == "in") then
                                                            param_node%intent_type = 1  ! INTENT_IN
                                                        else if (body_node%intent == "out") then
                                                            param_node%intent_type = 2  ! INTENT_OUT
                                                        else if (body_node%intent == "inout") then
                                                            param_node%intent_type = 3  ! INTENT_INOUT
                                                        end if
                                                    end if
                                                    param_node%is_optional = body_node%is_optional
                                                    ! Update type if not already set
                                                    if (len_trim(param_node%type_name) == 0) then
                                                        param_node%type_name = body_node%type_name
                                                    end if
                                                    end if
                                                end block
                                            end select
                                        end if
                                    end if
                                end do
                            end select
                        end if
                    end if
                end do
            end block
        end if

        ! Create subroutine node with collected parameters and body
        sub_index = push_subroutine_def(arena, sub_name, param_indices, &
                                         body_indices, line, column)

    end function parse_subroutine_definition

    function parse_interface_block(parser, arena) result(interface_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: interface_index

        type(token_t) :: interface_token, token
        character(len=:), allocatable :: name, operator_symbol, kind
        integer :: line, column
        integer, allocatable :: procedure_indices(:)

        ! Consume 'interface' keyword
        interface_token = parser%consume()
        line = interface_token%line
        column = interface_token%column
        kind = "interface"

        ! Check what follows the interface keyword
        token = parser%peek()

        if (token%kind == TK_IDENTIFIER) then
            ! Named interface: interface name
            token = parser%consume()
            name = token%text
            kind = "generic"
        else if (token%kind == TK_KEYWORD .and. token%text == "operator") then
            ! Operator interface: interface operator(+)
            token = parser%consume()  ! consume 'operator'
            kind = "operator"

            ! Expect '('
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()  ! consume '('

                ! Get operator symbol
                token = parser%peek()
                if (token%kind == TK_OPERATOR) then
                    operator_symbol = token%text
                    token = parser%consume()
                end if

                ! Expect ')'
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()  ! consume ')'
                end if
            end if
        else if (token%kind == TK_KEYWORD .and. token%text == "assignment") then
            ! Assignment interface: interface assignment(=)
            token = parser%consume()  ! consume 'assignment'
            kind = "assignment"

            ! Expect '(=)'
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()  ! consume '('
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "=") then
                    token = parser%consume()  ! consume '='
                    operator_symbol = "="
                end if
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()  ! consume ')'
                end if
            end if
        end if

        ! Parse interface body (procedure declarations)
        ! For now, just skip until "end interface"
        do
            token = parser%peek()
            if (token%kind == TK_EOF) exit
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                token = parser%consume()  ! consume 'end'
                token = parser%peek()
                if (token%kind == TK_KEYWORD .and. token%text == "interface") then
                    token = parser%consume()  ! consume 'interface'
                    exit
                end if
            else
                token = parser%consume()  ! skip token
            end if
        end do

        ! Create interface block node
        block
            integer, allocatable :: procedure_indices(:)
            character(len=:), allocatable :: interface_name
            ! For now, use empty procedure list until full interface parsing &
            ! is implemented
            allocate (procedure_indices(0))
            if (allocated(name)) then
                interface_name = name
            else if (allocated(operator_symbol)) then
                interface_name = "operator("//operator_symbol//")"
            else
                interface_name = ""
            end if
            interface_index = push_interface_block(arena, interface_name, &
                                                    procedure_indices, line, column)
        end block

    end function parse_interface_block

    function parse_module(parser, arena) result(module_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: module_index

        type(token_t) :: module_token, token
        character(len=:), allocatable :: name
        integer :: line, column
        integer, allocatable :: declaration_indices(:)
        integer, allocatable :: procedure_indices(:)
        logical :: has_contains

        ! Consume 'module' keyword
        module_token = parser%consume()
        line = module_token%line
        column = module_token%column
        has_contains = .false.

        ! Get module name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            name = token%text
        else
            ! Error: expected module name
            name = "unknown"
        end if

        ! Parse module body (declarations)
        allocate(declaration_indices(0))
        
        do
            token = parser%peek()
            if (token%kind == TK_EOF) exit
            
            if (token%kind == TK_KEYWORD .and. token%text == "contains") then
                has_contains = .true.
                exit
            else if (token%kind == TK_KEYWORD .and. token%text == "end") then
                ! Check if this is end module
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                        parser%tokens(parser%current_token + 1)%text == "module") then
                        ! Don't consume yet, we've found the end
                        exit
                    end if
                end if
                ! Not end module, parse this statement
            end if
            
            ! Parse declaration statement
            block
                integer :: stmt_index
                
                if (token%kind == TK_KEYWORD .and. token%text == "implicit") then
                    stmt_index = parse_implicit_statement(parser, arena)
                    if (stmt_index > 0) then
                        declaration_indices = [declaration_indices, stmt_index]
                    end if
                else if (token%kind == TK_KEYWORD .and. &
                    (token%text == "integer" .or. token%text == "real" .or. &
                     token%text == "logical" .or. token%text == "character")) then
                    ! Parse declaration
                    stmt_index = parse_declaration(parser, arena)
                    if (stmt_index > 0) then
                        declaration_indices = [declaration_indices, stmt_index]
                    end if
                else
                    ! Skip unknown tokens
                    token = parser%consume()
                end if
            end block
        end do

        ! Parse procedures after contains
        allocate(procedure_indices(0))
        
        if (has_contains) then
            ! Consume 'contains'
            token = parser%consume()
            
            do
                token = parser%peek()
                if (token%kind == TK_EOF) exit
                
                if (token%kind == TK_KEYWORD .and. token%text == "end") then
                    ! Check if this is end module
                    if (parser%current_token + 1 <= size(parser%tokens)) then
                        if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                            parser%tokens(parser%current_token + 1)%text == "module") then
                            ! Don't consume yet, we've found the end
                            exit
                        end if
                    end if
                end if
                
                ! Parse procedures
                block
                    integer :: proc_index
                    proc_index = 0
                    
                    if (token%kind == TK_KEYWORD) then
                        if (token%text == "subroutine") then
                            proc_index = parse_subroutine_definition(parser, arena)
                        else if (token%text == "function") then
                            proc_index = parse_function_definition(parser, arena)
                        else
                            ! Skip unknown tokens
                            token = parser%consume()
                        end if
                        
                        if (proc_index > 0) then
                            procedure_indices = [procedure_indices, proc_index]
                        end if
                    else
                        ! Skip non-keyword tokens
                        token = parser%consume()
                    end if
                end block
            end do
        end if
        
        ! Consume end module if we haven't already
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "end") then
            token = parser%consume()  ! consume 'end'
            token = parser%peek()
            if (token%kind == TK_KEYWORD .and. token%text == "module") then
                token = parser%consume()  ! consume 'module'
                ! Optional module name
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
                    token = parser%consume()
                end if
            end if
        end if

        ! Create module node
        block
            type(module_node) :: mod_node
            if (.not. allocated(procedure_indices)) then
                allocate(procedure_indices(0))
            end if
            mod_node = create_module(name, &
                declaration_indices=declaration_indices, line=line, column=column)
            mod_node%has_contains = has_contains
            if (allocated(procedure_indices)) then
                mod_node%procedure_indices = procedure_indices
            end if
            call arena%push(mod_node, "module_node")
            module_index = arena%size
        end block

    end function parse_module

    ! Parse basic statement with support for multi-variable declarations
    function parse_basic_statement_multi(tokens, arena) result(stmt_indices)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
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
                ! Use multi-declaration parser
                stmt_indices = parse_multi_declaration(parser, arena)
                return
            else if (first_token%text == "call") then
                ! Parse call statement
                allocate (stmt_indices(1))
                stmt_indices(1) = parse_call_statement(parser, arena)
                return
            else if (first_token%text == "return") then
                ! Parse return statement
                allocate (stmt_indices(1))
                stmt_indices(1) = parse_return_statement(parser, arena)
                return
            else if (first_token%text == "stop") then
                ! Parse stop statement
                allocate (stmt_indices(1))
                stmt_indices(1) = parse_stop_statement(parser, arena)
                return
            else if (first_token%text == "if") then
                ! Parse if statement using parse_if_simple (defined in this module at line 2751)
                ! This avoids circular dependency with parser_control_flow_module
                allocate (stmt_indices(1))
                stmt_indices(1) = parse_if_simple(parser, arena)
                return
            else if (first_token%text == "print") then
                ! Parse print statement
                allocate (stmt_indices(1))
                stmt_indices(1) = parse_print_statement(parser, arena)
                if (stmt_indices(1) <= 0) then
                    deallocate(stmt_indices)
                    allocate (stmt_indices(0))  ! Return empty array on failure
                end if
                return
            else if (first_token%text == "associate") then
                ! Parse associate construct using local function to avoid circular dependency
                allocate (stmt_indices(1))
                stmt_indices(1) = parse_associate_simple(parser, arena)
                if (stmt_indices(1) <= 0) then
                    deallocate(stmt_indices)
                    allocate (stmt_indices(0))  ! Return empty array on failure
                end if
                return
            end if
        end if

        ! For all other statements, parse as single statement
        allocate (stmt_indices(1))
        stmt_index = 0

        ! Simple assignment statement: identifier = expression
        if (first_token%kind == TK_IDENTIFIER) then
            block
                type(token_t) :: id_token, op_token
                integer :: target_index, value_index

                id_token = parser%consume()
                op_token = parser%peek()

                if (op_token%kind == TK_OPERATOR .and. op_token%text == "=") then
                    op_token = parser%consume()  ! consume '='
    target_index = push_identifier(arena, id_token%text, id_token%line, id_token%column)
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
                                stmt_index = push_assignment(arena, &
                                    target_index, value_index, id_token%line, &
                                    id_token%column)
                            end if
                        end if
                    end block
                end if
            end block
        end if

        ! If we couldn't parse it, create a placeholder
        if (stmt_index == 0) then
            stmt_index = push_literal(arena, "! Unparsed statement", LITERAL_STRING, first_token%line, first_token%column)
        end if

        stmt_indices(1) = stmt_index

    end function parse_basic_statement_multi

    ! Parse program statement: program program_name
    function parse_program_statement(parser, arena) result(prog_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: prog_index
        type(token_t) :: token, name_token
        character(len=:), allocatable :: program_name
        integer :: line, column
        integer, allocatable :: body_indices(:)
        integer :: stmt_index

        prog_index = 0
        allocate (body_indices(0))

        ! Consume 'program' keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Get program name (optional in lazy fortran, required in standard)
        name_token = parser%peek()
        if (name_token%kind == TK_IDENTIFIER) then
            name_token = parser%consume()
            program_name = name_token%text
        else
            program_name = "main"
        end if

        ! Parse program body until 'end program'
        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for 'end program'
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                if (parser%current_token + 1 <= size(parser%tokens)) then
                  if (parser%tokens(parser%current_token + 1)%kind == TK_KEYWORD .and. &
                        parser%tokens(parser%current_token + 1)%text == "program") then
                        ! Found 'end program', consume both tokens
                        token = parser%consume()  ! end
                        token = parser%consume()  ! program

                        ! Optional program name after 'end program'
                        token = parser%peek()
                        if (token%kind == TK_IDENTIFIER) then
                            token = parser%consume()
                        end if
                        exit
                    end if
                end if
            end if

            ! Parse each statement in the program body
            block
                type(token_t) :: first_token
                first_token = parser%peek()

                ! Handle different statement types
                select case (first_token%kind)
                case (TK_KEYWORD)
                    select case (first_token%text)
                    case ("use")
                        stmt_index = parse_use_statement(parser, arena)
                    case ("implicit")
                        stmt_index = parse_implicit_statement(parser, arena)
                    case ("integer", "real", "logical", "character", "complex")
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
                            
                            do while (lookahead_pos <= size(parser%tokens))
                                if (lookahead_pos > size(parser%tokens)) exit
                                lookahead_token = parser%tokens(lookahead_pos)
                                if (lookahead_token%kind == TK_OPERATOR .and. lookahead_token%text == "=") then
                                    has_initializer = .true.
                                else if (lookahead_token%kind == TK_OPERATOR .and. lookahead_token%text == ",") then
                                    has_comma = .true.
                                    exit
                                else if (lookahead_token%kind == TK_EOF .or. &
                                        (lookahead_token%kind == TK_KEYWORD .and. lookahead_token%text == "end")) then
                                    exit
                                end if
                                lookahead_pos = lookahead_pos + 1
                            end do
                            
                            if (has_initializer .and. .not. has_comma) then
                                ! Single variable with initializer - use parse_declaration
                                stmt_index = parse_declaration(parser, arena)
                            else
                                ! Multi-variable declaration - use parse_multi_declaration
                                block
                                    integer, allocatable :: decl_indices(:)
                                    decl_indices = parse_multi_declaration(parser, arena)
                                if (allocated(decl_indices) .and. size(decl_indices) > 0) then
                                    ! Add all declarations to body
                                    body_indices = [body_indices, decl_indices]
                                    stmt_index = 0  ! Don't add again below
                                else
                                    stmt_index = 0
                                end if
                                end block
                            end if
                        end block
                    case ("print")
                        stmt_index = parse_print_statement(parser, arena)
                    case ("allocate")
                        stmt_index = parse_allocate_statement(parser, arena)
                    case ("deallocate")
                        stmt_index = parse_deallocate_statement(parser, arena)
                    case ("contains")
                        ! Skip contains keyword but continue parsing following procedures
                        token = parser%consume()
                        stmt_index = 0
                    case ("subroutine")
                        stmt_index = parse_subroutine_definition(parser, arena)
                    case ("function")
                        stmt_index = parse_function_definition(parser, arena)
                    case ("call")
                        stmt_index = parse_call_statement(parser, arena)
                    case ("if")
                        stmt_index = parse_if_simple(parser, arena)
                    case default
                        ! Skip unknown keywords
                        token = parser%consume()
                        stmt_index = 0
                    end select
                case (TK_IDENTIFIER)
                    ! Could be assignment
                    block
                        type(token_t) :: id_token, op_token
                        integer :: target_index, value_index

                        id_token = parser%consume()
                        op_token = parser%peek()

                       if (op_token%kind == TK_OPERATOR .and. op_token%text == "=") then
                            op_token = parser%consume()

                            ! Create target identifier
    target_index = push_identifier(arena, id_token%text, id_token%line, id_token%column)

                            ! Parse value expression
                            value_index = parse_range(parser, arena)
                            
                            if (value_index > 0) then
                                stmt_index = push_assignment(arena, &
                                    target_index, value_index, id_token%line, &
                                    id_token%column)
                            else
                                stmt_index = 0
                            end if
                        else
                            stmt_index = 0
                        end if
                    end block
                case default
                    ! Skip this token
                    if (.not. parser%is_at_end()) then
                        token = parser%consume()
                    end if
                    stmt_index = 0
                end select

                if (stmt_index > 0) then
                    body_indices = [body_indices, stmt_index]
                end if
            end block
        end do

        ! Create program node
        prog_index = push_program(arena, program_name, body_indices, line, column)

    end function parse_program_statement

    ! Parse allocate statement: allocate(var1, var2(size), source=src, mold=template, stat=status, errmsg=msg)
    function parse_allocate_statement(parser, arena) result(allocate_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: allocate_index
        
        type(token_t) :: token
        integer, allocatable :: var_indices(:)
        integer, allocatable :: shape_indices(:)
        integer :: stat_var_index, errmsg_var_index, source_expr_index, mold_expr_index
        integer :: line, column
        logical :: in_variable_list
        
        ! Initialize
        stat_var_index = 0
        errmsg_var_index = 0
        source_expr_index = 0
        mold_expr_index = 0
        line = 0
        column = 0
        in_variable_list = .true.
        allocate(var_indices(0))
        allocate(shape_indices(0))
        
        ! Consume 'allocate' keyword
        token = parser%consume()
        line = token%line
        column = token%column
        
        ! Expect opening parenthesis
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('
            
            ! Parse variable list and optional parameters
            do
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    exit
                end if
                
                ! Check for allocate parameters
                if (token%kind == TK_IDENTIFIER) then
                    select case (token%text)
                    case ("stat")
                        token = parser%consume()  ! consume 'stat'
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "=") then
                            token = parser%consume()  ! consume '='
                            stat_var_index = parse_comparison(parser, arena)
                        end if
                        in_variable_list = .false.
                    case ("errmsg")
                        token = parser%consume()  ! consume 'errmsg'
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "=") then
                            token = parser%consume()  ! consume '='
                            errmsg_var_index = parse_comparison(parser, arena)
                        end if
                        in_variable_list = .false.
                    case ("source")
                        token = parser%consume()  ! consume 'source'
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "=") then
                            token = parser%consume()  ! consume '='
                            source_expr_index = parse_comparison(parser, arena)
                        end if
                        in_variable_list = .false.
                    case ("mold")
                        token = parser%consume()  ! consume 'mold'
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "=") then
                            token = parser%consume()  ! consume '='
                            mold_expr_index = parse_comparison(parser, arena)
                        end if
                        in_variable_list = .false.
                    case default
                        if (in_variable_list) then
                            ! Parse variable (potentially with array dimensions)
                            call parse_allocate_variable(parser, arena, var_indices, shape_indices)
                        else
                            ! Skip unknown parameter
                            token = parser%consume()
                        end if
                    end select
                else if (in_variable_list) then
                    ! Parse variable (potentially with array dimensions)
                    call parse_allocate_variable(parser, arena, var_indices, shape_indices)
                else
                    ! Skip unexpected token
                    token = parser%consume()
                end if
                
                ! Check for comma to continue
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()  ! consume ','
                else
                    exit
                end if
            end do
            
            ! Expect closing parenthesis
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()  ! consume ')'
            end if
        end if
        
        ! Create allocate statement node
        allocate_index = push_allocate(arena, var_indices, shape_indices, stat_var_index, &
                                       errmsg_var_index, source_expr_index, mold_expr_index)
        
    end function parse_allocate_statement
    
    ! Parse deallocate statement: deallocate(var1, var2, stat=status, errmsg=msg)
    function parse_deallocate_statement(parser, arena) result(deallocate_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: deallocate_index
        
        type(token_t) :: token
        integer, allocatable :: var_indices(:)
        integer :: stat_var_index, errmsg_var_index, line, column
        logical :: in_variable_list
        
        ! Initialize
        stat_var_index = 0
        errmsg_var_index = 0
        line = 0
        column = 0
        in_variable_list = .true.
        allocate(var_indices(0))
        
        ! Consume 'deallocate' keyword
        token = parser%consume()
        line = token%line
        column = token%column
        
        ! Expect opening parenthesis
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('
            
            ! Parse variable list and optional parameters
            do
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    exit
                end if
                
                ! Check for deallocate parameters
                if (token%kind == TK_IDENTIFIER) then
                    select case (token%text)
                    case ("stat")
                        token = parser%consume()  ! consume 'stat'
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "=") then
                            token = parser%consume()  ! consume '='
                            stat_var_index = parse_comparison(parser, arena)
                        end if
                        in_variable_list = .false.
                    case ("errmsg")
                        token = parser%consume()  ! consume 'errmsg'
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "=") then
                            token = parser%consume()  ! consume '='
                            errmsg_var_index = parse_comparison(parser, arena)
                        end if
                        in_variable_list = .false.
                    case default
                        if (in_variable_list) then
                            ! Parse variable identifier
                            var_indices = [var_indices, parse_comparison(parser, arena)]
                        else
                            ! Skip unknown parameter
                            token = parser%consume()
                        end if
                    end select
                else if (in_variable_list) then
                    ! Parse variable identifier
                    var_indices = [var_indices, parse_comparison(parser, arena)]
                else
                    ! Skip unexpected token
                    token = parser%consume()
                end if
                
                ! Check for comma to continue
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()  ! consume ','
                else
                    exit
                end if
            end do
            
            ! Expect closing parenthesis
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()  ! consume ')'
            end if
        end if
        
        ! Create deallocate statement node
        deallocate_index = push_deallocate(arena, var_indices, stat_var_index, errmsg_var_index)
        
    end function parse_deallocate_statement
    
    ! Helper subroutine to parse allocate variable with optional dimensions
    subroutine parse_allocate_variable(parser, arena, var_indices, shape_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: var_indices(:)
        integer, allocatable, intent(inout) :: shape_indices(:)
        
        type(token_t) :: token
        integer :: var_index
        
        ! Parse variable identifier
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            var_index = parse_comparison(parser, arena)
            var_indices = [var_indices, var_index]
            
            ! Check for array dimensions: var(size1, size2, ...)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()  ! consume '('
                
                ! Parse dimension expressions
                do
                    shape_indices = [shape_indices, parse_comparison(parser, arena)]
                    
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == ",") then
                        token = parser%consume()  ! consume ','
                    else
                        exit
                    end if
                end do
                
                ! Expect closing parenthesis
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()  ! consume ')'
                end if
            end if
        end if
    end subroutine parse_allocate_variable

    ! Parse call statement: call subroutine_name(args)
    function parse_call_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token
        character(len=:), allocatable :: subroutine_name
        integer, allocatable :: arg_indices(:)
        integer :: line, column

        ! Consume 'call' keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Get subroutine name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            subroutine_name = token%text

            ! Check for arguments
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                ! Parse arguments
                call parse_call_arguments(parser, arena, arg_indices)
            else
                ! No arguments
                allocate (arg_indices(0))
            end if

            ! Create call node
            stmt_index = push_subroutine_call(arena, subroutine_name, arg_indices, &
                                              line, column)
        else
            ! Error: expected subroutine name
            stmt_index = push_literal(arena, "! Error: expected subroutine name after 'call'", &
                                      LITERAL_STRING, line, column)
        end if

    end function parse_call_statement
    
    ! Helper subroutine to parse call arguments
    subroutine parse_call_arguments(parser, arena, arg_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: arg_indices(:)
        type(token_t) :: token
        integer :: arg_index
        
        allocate(arg_indices(0))
        
        ! Consume opening parenthesis
        token = parser%consume()
        
        ! Parse arguments
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()  ! consume ')'
                exit
            end if
            
            ! Parse argument expression
            arg_index = parse_range(parser, arena)
            if (arg_index > 0) then
                arg_indices = [arg_indices, arg_index]
            end if
            
            ! Check for comma (more arguments)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! consume ','
                cycle
            else if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()  ! consume ')'
                exit
            else
                ! Unexpected token - try to continue
                exit
            end if
        end do
    end subroutine parse_call_arguments

    ! Simple if statement parser (to avoid circular dependency with control_flow module)
    function parse_if_simple(parser, arena) result(if_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: if_index
        
        type(token_t) :: if_token, then_token
        integer :: condition_index
        integer, allocatable :: then_body_indices(:), else_body_indices(:)
        integer, allocatable :: elseif_indices(:)
        
        ! Consume 'if' keyword
        if_token = parser%consume()
        
        ! Parse condition (should be in parentheses)
        condition_index = parse_if_condition_simple(parser, arena)
        
        ! Error handling: check if condition parsing failed
        if (condition_index <= 0) then
            ! Create error literal node for malformed condition
            condition_index = push_literal(arena, "! Error: malformed if condition", &
                                         LITERAL_STRING, if_token%line, if_token%column)
        end if
        
        ! Look for 'then' keyword
        then_token = parser%peek()
        if (then_token%kind == TK_KEYWORD .and. then_token%text == "then") then
            ! Standard if/then/endif block
            then_token = parser%consume()
            
            ! Parse then body statements
            allocate(then_body_indices(0))
            allocate(else_body_indices(0))
            allocate(elseif_indices(0))
            
            ! Parse all statements within if block until endif
            do while (.not. parser%is_at_end())
                then_token = parser%peek()
                
                if (then_token%kind == TK_KEYWORD) then
                    if (then_token%text == "endif" .or. then_token%text == "end if") then
                        ! End of if statement
                        then_token = parser%consume()
                        exit
                    else
                        ! Parse any statement type within if block
                        block
                            integer :: stmt_idx
                            stmt_idx = parse_statement_in_if_block(parser, arena, then_token)
                            if (stmt_idx > 0) then
                                then_body_indices = [then_body_indices, stmt_idx]
                            else
                                ! If parsing failed, skip this token to avoid infinite loop
                                then_token = parser%consume()
                            end if
                        end block
                    end if
                else if (then_token%kind == TK_IDENTIFIER) then
                    ! Could be assignment or other identifier-based statement
                    block
                        integer :: stmt_idx
                        stmt_idx = parse_assignment_simple(parser, arena)
                        if (stmt_idx > 0) then
                            then_body_indices = [then_body_indices, stmt_idx]
                        else
                            ! If parsing failed, skip this token to avoid infinite loop
                            then_token = parser%consume()
                        end if
                    end block
                else
                    ! Skip unexpected tokens
                    then_token = parser%consume()
                end if
            end do
            
            ! Create if node using ast_factory
            if_index = push_if(arena, condition_index, then_body_indices, &
                             elseif_indices=elseif_indices, &
                             else_body_indices=else_body_indices, &
                             line=if_token%line, column=if_token%column)
        else
            ! One-line if statement - not implemented for simplicity
            if_index = 0
        end if
        
    end function parse_if_simple
    
    ! Simple associate construct parser (to avoid circular dependency with control_flow module)
    ! This function duplicates some logic from parser_control_flow::parse_associate but is
    ! necessary to break the circular dependency between parser_statements and parser_control_flow.
    ! The function is intentionally self-contained to avoid further module dependencies.
    function parse_associate_simple(parser, arena) result(assoc_index)
        use ast_nodes_control, only: association_t
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: assoc_index
        
        type(token_t) :: token
        type(association_t), allocatable :: associations(:)
        integer, allocatable :: body_indices(:)
        integer :: assoc_count, body_count
        integer :: line, column
        
        ! Get position
        token = parser%peek()
        line = token%line
        column = token%column
        
        ! Consume 'associate'
        token = parser%consume()
        
        ! Expect opening parenthesis
        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            assoc_index = 0
            return
        end if
        token = parser%consume()
        
        ! Parse associations
        assoc_count = 0
        allocate(associations(10))  ! Initial allocation
        
        do while (.not. parser%is_at_end())
            ! Parse association name
            token = parser%peek()
            if (token%kind /= TK_IDENTIFIER) exit
            
            block
                character(len=:), allocatable :: assoc_name
                integer :: expr_index
                
                assoc_name = token%text
                token = parser%consume()
                
                ! Expect '=>'
                token = parser%peek()
                if (token%kind /= TK_OPERATOR .or. token%text /= "=>") then
                    deallocate(associations)
                    assoc_index = 0
                    return
                end if
                token = parser%consume()
                
                ! Parse expression
                expr_index = parse_expression(&
                    parser%tokens(parser%current_token:), arena)
                if (expr_index <= 0) then
                    deallocate(associations)
                    assoc_index = 0
                    return
                end if
                
                ! Advance parser position for the expression
                block
                    integer :: paren_depth
                    paren_depth = 0
                    ! Properly track expression boundaries considering nested parentheses
                    do while (parser%current_token <= size(parser%tokens))
                        token = parser%peek()
                        
                        ! Track parentheses depth for nested expressions
                        if (token%kind == TK_OPERATOR) then
                            if (token%text == "(") then
                                paren_depth = paren_depth + 1
                            else if (token%text == ")") then
                                if (paren_depth == 0) exit  ! End of associate list
                                paren_depth = paren_depth - 1
                            else if (token%text == "," .and. paren_depth == 0) then
                                exit  ! Next association
                            end if
                        else if (token%kind == TK_KEYWORD .and. paren_depth == 0) then
                            exit  ! End of expression
                        end if
                        
                        token = parser%consume()
                    end do
                end block
                
                ! Add association
                assoc_count = assoc_count + 1
                if (assoc_count > size(associations)) then
                    ! Resize array
                    block
                        type(association_t), allocatable :: temp(:)
                        allocate(temp(size(associations) * 2))
                        temp(1:size(associations)) = associations
                        call move_alloc(temp, associations)
                    end block
                end if
                
                associations(assoc_count)%name = assoc_name
                associations(assoc_count)%expr_index = expr_index
            end block
            
            ! Check for comma or closing parenthesis
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
                cycle
            else if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            else
                deallocate(associations)
                assoc_index = 0
                return
            end if
        end do
        
        ! Parse body statements until 'end associate'
        body_count = 0
        allocate(body_indices(100))  ! Initial allocation
        
        do while (.not. parser%is_at_end())
            token = parser%peek()
            
            ! Check for 'end associate'
            if (token%kind == TK_KEYWORD .and. token%text == "end") then
                if (parser%current_token + 1 <= size(parser%tokens)) then
                    if (parser%tokens(parser%current_token + 1)%kind == &
                        TK_KEYWORD .and. &
                        parser%tokens(parser%current_token + 1)%text == &
                        "associate") then
                        token = parser%consume()  ! consume 'end'
                        token = parser%consume()  ! consume 'associate'
                        exit
                    end if
                end if
            end if
            
            ! Handle EOF
            if (token%kind == TK_EOF) then
                exit
            end if
            
            ! Parse a single statement
            block
                integer :: stmt_index
                integer :: start_token
                
                start_token = parser%current_token
                
                ! Try to parse the statement based on its type
                if (token%kind == TK_KEYWORD) then
                    select case (token%text)
                    case ("print")
                        stmt_index = parse_print_statement(parser, arena)
                    case ("call")
                        stmt_index = parse_call_statement(parser, arena)
                    case ("if")
                        stmt_index = parse_if_simple(parser, arena)
                    case default
                        ! Skip this token and continue
                        token = parser%consume()
                        stmt_index = 0
                    end select
                else if (token%kind == TK_IDENTIFIER) then
                    ! Try to parse as assignment
                    block
                        type(token_t) :: id_token, op_token
                        integer :: target_index, value_index
                        
                        id_token = parser%consume()
                        op_token = parser%peek()
                        
                        if (op_token%kind == TK_OPERATOR .and. op_token%text == "=") then
                            op_token = parser%consume()  ! consume '='
                            target_index = push_identifier(arena, id_token%text, &
                                                         id_token%line, id_token%column)
                            ! Parse expression
                            block
                                type(token_t), allocatable :: expr_tokens(:)
                                integer :: remaining_count
                                remaining_count = size(parser%tokens) - parser%current_token + 1
                                if (remaining_count > 0) then
                                    allocate (expr_tokens(remaining_count))
                                    expr_tokens = parser%tokens(parser%current_token:)
                                    value_index = parse_expression(expr_tokens, arena)
                                    if (value_index > 0) then
                                        stmt_index = push_assignment(arena, &
                                            target_index, value_index, id_token%line, &
                                            id_token%column)
                                        ! Advance past the expression
                                        do while (parser%current_token <= size(parser%tokens))
                                            token = parser%peek()
                                            if (token%kind == TK_KEYWORD) exit
                                            if (token%kind == TK_EOF) exit
                                            token = parser%consume()
                                        end do
                                    else
                                        stmt_index = 0
                                    end if
                                end if
                            end block
                        else
                            stmt_index = 0
                        end if
                    end block
                else
                    ! Skip unknown token
                    token = parser%consume()
                    stmt_index = 0
                end if
                
                ! Add successful statement to body
                if (stmt_index > 0) then
                    body_count = body_count + 1
                    if (body_count > size(body_indices)) then
                        ! Resize array
                        block
                            integer, allocatable :: temp(:)
                            allocate(temp(size(body_indices) + 100))
                            temp(1:size(body_indices)) = body_indices
                            temp(size(body_indices)+1:) = 0
                            call move_alloc(temp, body_indices)
                        end block
                    end if
                    body_indices(body_count) = stmt_index
                end if
                
                ! Safety check to avoid infinite loop
                if (parser%current_token == start_token) then
                    token = parser%consume()
                end if
            end block
        end do
        
        ! Create ASSOCIATE node
        if (assoc_count > 0) then
            block
                type(association_t), allocatable :: final_assocs(:)
                integer, allocatable :: final_body(:)
                
                allocate(final_assocs(assoc_count))
                final_assocs = associations(1:assoc_count)
                
                if (body_count > 0) then
                    allocate(final_body(body_count))
                    final_body = body_indices(1:body_count)
                    assoc_index = push_associate(arena, final_assocs, &
                                                  final_body, line, column)
                else
                    assoc_index = push_associate(arena, final_assocs, &
                                                  line=line, column=column)
                end if
            end block
        else
            assoc_index = 0
        end if
        
        ! Clean up allocated arrays
        if (allocated(associations)) deallocate(associations)
        if (allocated(body_indices)) deallocate(body_indices)
    end function parse_associate_simple
    
    ! Simple if condition parser with error handling
    function parse_if_condition_simple(parser, arena) result(condition_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: condition_index
        
        type(token_t) :: token
        logical :: has_parens
        
        condition_index = 0
        has_parens = .false.
        
        ! Check for opening parenthesis
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('
            has_parens = .true.
        end if
        
        ! Parse the condition expression
        condition_index = parse_comparison(parser, arena)
        
        ! Error handling: check if expression parsing failed
        if (condition_index <= 0) then
            ! Return error - caller will handle it
            return
        end if
        
        if (has_parens) then
            ! Expect closing parenthesis
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()  ! consume ')'
            else
                ! Error: missing closing parenthesis
                ! Return error to indicate malformed condition
                condition_index = 0
                return
            end if
        end if
        
    end function parse_if_condition_simple

    ! Parse statement within if block - handles all statement types
    function parse_statement_in_if_block(parser, arena, token) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: token
        integer :: stmt_index
        
        ! Use the comprehensive statement parsing logic similar to parse_program_statement
        select case (token%text)
        ! I/O statements
        case ("print")
            stmt_index = parse_print_statement(parser, arena)
        case ("write")
            stmt_index = parse_write_statement(parser, arena)
        case ("read")
            stmt_index = parse_read_statement(parser, arena)
        
        ! Memory management
        case ("allocate")
            stmt_index = parse_allocate_statement(parser, arena)
        case ("deallocate")
            stmt_index = parse_deallocate_statement(parser, arena)
        
        ! Control flow
        case ("call")
            stmt_index = parse_call_statement(parser, arena)
        case ("stop")
            stmt_index = parse_stop_statement(parser, arena)
        case ("return")
            stmt_index = parse_return_statement(parser, arena)
        case ("cycle")
            stmt_index = parse_cycle_statement(parser, arena)
        case ("exit")
            stmt_index = parse_exit_statement(parser, arena)
        
        ! Nested control structures - avoid infinite recursion
        case ("if")
            ! For nested if statements, use skip_unknown_statement to handle properly
            stmt_index = skip_unknown_statement(parser)
        case ("do")
            ! For future: could add do loop parsing
            block
                type(token_t) :: skip_token
                skip_token = parser%consume()
            end block
            stmt_index = 0
        
        ! Declaration statements  
        case ("integer", "real", "logical", "character", "complex", "type")
            stmt_index = parse_declaration(parser, arena)
        
        ! Procedure definitions (shouldn't normally appear in if blocks, but handle gracefully)
        case ("subroutine")
            stmt_index = parse_subroutine_definition(parser, arena)
        case ("function")
            stmt_index = parse_function_definition(parser, arena)
        
        case default
            ! For unknown statements, skip to next logical boundary
            stmt_index = skip_unknown_statement(parser)
        end select
        
    end function parse_statement_in_if_block
    
    ! Simple assignment parser for identifier-based statements
    function parse_assignment_simple(parser, arena) result(assign_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: assign_index
        
        type(token_t) :: var_token, assign_token
        integer :: target_index, value_index
        
        ! Get variable name
        var_token = parser%peek()
        if (var_token%kind == TK_IDENTIFIER) then
            var_token = parser%consume()
            target_index = push_identifier(arena, var_token%text, var_token%line, var_token%column)
            
            ! Look for assignment operator
            assign_token = parser%peek()
            if (assign_token%kind == TK_OPERATOR .and. assign_token%text == "=") then
                assign_token = parser%consume()
                
                ! Parse value expression
                value_index = parse_comparison(parser, arena)
                
                if (value_index > 0) then
                    assign_index = push_assignment(arena, target_index, value_index, &
                                                 var_token%line, var_token%column)
                else
                    assign_index = 0
                end if
            else
                ! Not an assignment - might be a function call or other expression
                ! For now, skip it
                assign_index = 0
            end if
        else
            assign_index = 0
        end if
        
    end function parse_assignment_simple

    ! Skip unknown statement - simplified version to avoid infinite loops
    function skip_unknown_statement(parser) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        integer :: stmt_index
        
        type(token_t) :: token
        
        stmt_index = 0
        
        ! Simply consume the current token to move past unknown statement
        if (.not. parser%is_at_end()) then
            token = parser%consume()
        end if
        
    end function skip_unknown_statement

end module parser_statements_module
