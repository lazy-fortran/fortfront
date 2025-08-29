module parser_type_definitions_module
    ! Parser module for derived type definitions and type parameters
    use lexer_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use ast_core
    use ast_factory
    implicit none
    private

    public :: parse_derived_type, parse_derived_type_parameters

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

end module parser_type_definitions_module