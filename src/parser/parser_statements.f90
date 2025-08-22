module parser_statements_module
    ! Parser module for various statement types - delegates to specialized modules
    use parser_io_statements_module
    use parser_control_statements_module
    use parser_memory_statements_module
    use parser_execution_statements_module
    use parser_import_statements_module
    
    ! For backward compatibility, re-export all the parsing functions
    use parser_io_statements_module, only: &
        parse_print_statement, parse_write_statement, parse_read_statement
    use parser_control_statements_module, only: &
        parse_stop_statement, parse_return_statement, parse_end_statement, &
        parse_goto_statement, parse_error_stop_statement, &
        parse_cycle_statement, parse_exit_statement
    use parser_memory_statements_module, only: &
        parse_allocate_statement, parse_deallocate_statement
    use parser_execution_statements_module, only: &
        parse_call_statement, parse_program_statement
    use parser_import_statements_module, only: &
        parse_use_statement, parse_implicit_statement, parse_include_statement, &
        parse_module
    
    ! Additional dependencies for remaining utility functions
    use iso_fortran_env, only: error_unit
    use lexer_core
    use parser_state_module
    use parser_expressions_module, only: parse_comparison, parse_expression, parse_range
    use parser_declarations, only: parse_declaration, parse_multi_declaration
    use parser_utils, only: analyze_declaration_structure
    use ast_core
    use ast_factory
    use ast_types, only: LITERAL_STRING
    implicit none
    private

    ! Re-export all public functions for backward compatibility
    public :: parse_use_statement, parse_implicit_statement, parse_include_statement
    public :: parse_print_statement, parse_write_statement, parse_read_statement
    public :: parse_stop_statement, parse_return_statement, parse_end_statement
    public :: parse_goto_statement, parse_error_stop_statement
    public :: parse_cycle_statement, parse_exit_statement
    public :: parse_allocate_statement, parse_deallocate_statement
    public :: parse_call_statement

    ! Utility functions that remain in this module
    public :: parse_only_list, parse_basic_statement_multi
    public :: parse_if_simple, parse_associate_simple, parse_if_condition_simple
    public :: parse_statement_in_if_block, parse_assignment_simple, skip_unknown_statement

contains

    ! Parse only list helper (remaining utility function)
    subroutine parse_only_list(parser, only_list, rename_list)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: only_list(:)
        character(len=:), allocatable, intent(out) :: rename_list(:)

        character(len=:), allocatable :: temp_only(:)
        character(len=:), allocatable :: temp_rename(:)
        type(token_t) :: token
        integer :: count
        character(len=:), allocatable :: item_name, new_name

        ! Initialize
        count = 0
        allocate(character(len=0) :: temp_only(0))
        allocate(character(len=0) :: temp_rename(0))

        ! Parse items separated by commas
        do
            token = parser%peek()
            if (token%kind /= TK_IDENTIFIER) exit

            ! Get item name
            item_name = token%text
            token = parser%consume()

            ! Check for rename: new_name => old_name
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "=") then
                token = parser%consume()  ! consume '='
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ">") then
                    token = parser%consume()  ! consume '>'
                    ! This is a rename
                    new_name = item_name
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER) then
                        item_name = token%text
                        token = parser%consume()
                        count = count + 1
                        temp_only = [temp_only, item_name]
                        temp_rename = [temp_rename, new_name]
                    end if
                end if
            else
                ! Regular item (no rename)
                count = count + 1
                temp_only = [temp_only, item_name]
                temp_rename = [temp_rename, ""]
            end if

            ! Check for comma
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! consume ','
            else
                exit
            end if
        end do

        ! Copy to output arrays
        allocate(character(len=0) :: only_list(count))
        allocate(character(len=0) :: rename_list(count))
        if (count > 0) then
            only_list = temp_only
            rename_list = temp_rename
        end if
    end subroutine parse_only_list

    ! Parse multiple statements (utility function)
    function parse_basic_statement_multi(tokens, arena) result(stmt_indices)
        type(token_t), intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable :: stmt_indices(:)

        ! Simplified implementation for refactoring
        allocate(stmt_indices(0))
    end function parse_basic_statement_multi

    ! Simple if statement parser (utility function)
    function parse_if_simple(parser, arena) result(if_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: if_index

        type(token_t) :: token
        integer :: condition_index
        integer, allocatable :: then_body_indices(:)

        ! Consume 'if' keyword
        token = parser%consume()

        ! Parse condition
        condition_index = parse_if_condition_simple(parser, arena)

        ! Simplified if parsing
        allocate(then_body_indices(0))

        ! Create if node
        if_index = push_if(arena, condition_index, then_body_indices, &
                           line=token%line, column=token%column)
    end function parse_if_simple

    ! Simple associate parser (utility function)
    function parse_associate_simple(parser, arena) result(assoc_index)
        use ast_nodes_control, only: association_t
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: assoc_index

        type(token_t) :: token

        ! Consume 'associate' keyword
        token = parser%consume()

        ! Simplified associate parsing
        assoc_index = push_literal(arena, "! Simplified associate statement", &
                                   LITERAL_STRING, token%line, token%column)
    end function parse_associate_simple

    ! Parse if condition (utility function)
    function parse_if_condition_simple(parser, arena) result(condition_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: condition_index

        type(token_t) :: token

        ! Check for opening parenthesis
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()  ! consume '('
            condition_index = parse_comparison(parser, arena)

            ! Consume closing parenthesis
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()  ! consume ')'
            end if
        else
            ! No parentheses - parse condition directly
            condition_index = parse_comparison(parser, arena)
        end if
    end function parse_if_condition_simple

    ! Parse statement within if block (utility function)
    function parse_statement_in_if_block(parser, arena, token) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), intent(in) :: token
        integer :: stmt_index

        ! Simplified statement parsing for if blocks
        select case (token%kind)
        case (TK_KEYWORD)
            select case (token%text)
            case ("print")
                stmt_index = parse_print_statement(parser, arena)
            case ("call")
                stmt_index = parse_call_statement(parser, arena)
            case default
                stmt_index = skip_unknown_statement(parser)
            end select
        case default
            stmt_index = parse_assignment_simple(parser, arena)
        end select
    end function parse_statement_in_if_block

    ! Simple assignment parser (utility function)
    function parse_assignment_simple(parser, arena) result(assign_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: assign_index

        type(token_t) :: token
        integer :: lhs_index, rhs_index

        ! Parse left-hand side
        lhs_index = parse_comparison(parser, arena)

        ! Expect assignment operator
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "=") then
            token = parser%consume()  ! consume '='

            ! Parse right-hand side
            rhs_index = parse_comparison(parser, arena)

            ! Create assignment node
            assign_index = push_assignment(arena, lhs_index, rhs_index, &
                                           token%line, token%column)
        else
            ! Not an assignment - return the expression
            assign_index = lhs_index
        end if
    end function parse_assignment_simple

    ! Skip unknown statement (utility function)
    function skip_unknown_statement(parser) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        integer :: stmt_index

        type(token_t) :: token

        ! Skip tokens until end of statement
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE .or. &
                (token%kind == TK_KEYWORD .and. &
                 (token%text == "end" .or. token%text == "endif"))) then
                exit
            end if
            token = parser%consume()
        end do

        stmt_index = 0  ! No valid statement created
    end function skip_unknown_statement

end module parser_statements_module