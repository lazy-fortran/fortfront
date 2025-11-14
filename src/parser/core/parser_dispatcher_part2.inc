
    function parse_submodule_statement(parser, arena) result(stmt_index)
        use ast_factory, only: push_error_node
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index
        type(token_t) :: token, next_token
        character(len=:), allocatable :: error_msg, lowered_text
        integer :: line, column

        token = parser%consume()
        line = token%line
        column = token%column

        error_msg = &
            "Unsupported Fortran feature: submodule constructs are not supported"

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_KEYWORD) then
                lowered_text = to_lower(trim(token%text))
                select case (lowered_text)
                case ("endsubmodule")
                    token = parser%consume()
                    if (.not. parser%is_at_end()) then
                        next_token = parser%peek()
                        if (next_token%kind == TK_IDENTIFIER) then
                            token = parser%consume()
                        end if
                    end if
                    exit
                case ("end")
                    token = parser%consume()
                    if (.not. parser%is_at_end()) then
                        next_token = parser%peek()
                        if (next_token%kind == TK_KEYWORD) then
                            lowered_text = to_lower(trim(next_token%text))
                            if (lowered_text == "submodule") then
                                token = parser%consume()
                                if (.not. parser%is_at_end()) then
                                    next_token = parser%peek()
                                    if (next_token%kind == TK_IDENTIFIER) then
                                        token = parser%consume()
                                    end if
                                end if
                                exit
                            end if
                        end if
                    end if
                    cycle
                end select
            end if
            token = parser%consume()
        end do

        if (associated(parser%tokens)) then
            parser%current_token = size(parser%tokens)
        end if

        stmt_index = push_error_node(arena, error_msg, "submodule", line, column)
    end function parse_submodule_statement

    ! Get last parser errors from dispatcher
    function get_last_parser_errors() result(errors)
        use iso_fortran_env, only: error_unit
        character(len=:), allocatable :: errors
        if (allocated(last_parser_errors)) then
            errors = last_parser_errors
        else
            errors = ""
        end if
    end function get_last_parser_errors

    ! Clear parser errors
    subroutine clear_parser_errors()
        if (allocated(last_parser_errors)) deallocate (last_parser_errors)
        last_parser_errors = ""
    end subroutine clear_parser_errors
