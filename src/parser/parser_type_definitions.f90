module parser_type_definitions_module
    ! Parser module for derived type definitions and type parameters
    use lexer_core, only: token_t, TK_EOF, TK_IDENTIFIER, TK_NUMBER, TK_STRING, &
                          TK_OPERATOR, TK_KEYWORD, TK_NEWLINE, TK_COMMENT, TK_WHITESPACE
    use parser_state_module, only: parser_state_t, create_parser_state
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_derived_type
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

    subroutine parse_type_name_and_parameters(parser, arena, type_name, &
                                              has_parameters, param_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=:), allocatable, intent(out) :: type_name
        logical, intent(out) :: has_parameters
        integer, allocatable, intent(out) :: param_indices(:)
        type(token_t) :: token

        has_parameters = .false.

        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER) then
            type_name = "unnamed_type"
            return
        end if

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
    end subroutine parse_type_name_and_parameters

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
            ! Get type name and parameters
            call parse_type_name_and_parameters(parser, arena, type_name, &
                                                has_parameters, param_indices)
        else if (token%kind == TK_IDENTIFIER) then
            ! Direct type name
            call parse_type_name_and_parameters(parser, arena, type_name, &
                                                has_parameters, param_indices)
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
