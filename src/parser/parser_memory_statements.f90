module parser_memory_statements_module
    ! Parser module for memory management statement types (allocate, deallocate)
    use lexer_core
    use parser_state_module
    use parser_expressions_module, only: parse_comparison
    use ast_core
    use ast_factory
    implicit none
    private

    public :: parse_allocate_statement, parse_deallocate_statement

contains

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

    ! Parse allocate parameters (stat, errmsg, source, mold)
    subroutine parse_allocate_params(parser, arena, param_name, &
                                     stat_var_index, errmsg_var_index, &
                                     source_expr_index, mold_expr_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: param_name
        integer, intent(inout) :: stat_var_index, errmsg_var_index
        integer, intent(inout) :: source_expr_index, mold_expr_index
        type(token_t) :: token

        token = parser%consume()  ! consume parameter name
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "=") then
            token = parser%consume()  ! consume '='
            select case (param_name)
            case ("stat")
                stat_var_index = parse_comparison(parser, arena)
            case ("errmsg")
                errmsg_var_index = parse_comparison(parser, arena)
            case ("source")
                source_expr_index = parse_comparison(parser, arena)
            case ("mold")
                mold_expr_index = parse_comparison(parser, arena)
            end select
        end if
    end subroutine parse_allocate_params

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
        ! Initialize with proper empty arrays that can grow
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
                    case ("stat", "errmsg", "source", "mold")
                        call parse_allocate_params(parser, arena, token%text, &
                                                   stat_var_index, errmsg_var_index, &
                                                   source_expr_index, mold_expr_index)
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
                                       errmsg_var_index, source_expr_index, mold_expr_index, &
                                       line, column)
    end function parse_allocate_statement

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
        deallocate_index = push_deallocate(arena, var_indices, stat_var_index, errmsg_var_index, &
                                          line, column)
    end function parse_deallocate_statement

end module parser_memory_statements_module