module parser_definition_statements_module
    ! Parser module for definition statement types (function, subroutine, interface, derived types)
    use lexer_core
    use parser_state_module
    use ast_core
    use ast_factory
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
        character(len=:), allocatable :: function_name
        integer :: line, column
        integer, allocatable :: param_indices(:), body_indices(:)

        ! Consume function keyword
        token = parser%consume()
        line = token%line
        column = token%column

        ! Get function name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            function_name = token%text
        else
            function_name = "unnamed_function"
        end if

        ! Simplified parsing for refactoring
        allocate(param_indices(0))
        allocate(body_indices(0))

        ! Create function node
        func_index = push_function_def(arena, function_name, param_indices, "", body_indices, &
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
            token = parser%consume()
            subroutine_name = token%text
        else
            subroutine_name = "unnamed_subroutine"
        end if

        ! Simplified parsing for refactoring
        allocate(param_indices(0))
        allocate(body_indices(0))

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