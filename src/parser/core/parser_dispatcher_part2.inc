
    function parse_submodule_statement(parser, arena) result(stmt_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: stmt_index

        stmt_index = parse_submodule(parser, arena)
    end function parse_submodule_statement

    ! Get last parser errors from dispatcher
    function get_last_parser_errors() result(errors)
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
