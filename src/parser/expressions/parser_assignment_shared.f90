module parser_assignment_shared_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER, &
        TK_STRING, TK_KEYWORD, TK_NEWLINE
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_assignment, push_identifier, push_literal
    use ast_types, only: LITERAL_INTEGER, LITERAL_LOGICAL, LITERAL_REAL, &
        LITERAL_STRING
    implicit none
    private

    public :: parse_multi_variable_assignment_core

contains

    subroutine parse_multi_variable_assignment_core(parser, arena, stmt_index, &
            assignment_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        integer, allocatable, intent(out) :: assignment_indices(:)
        integer, allocatable :: var_indices(:)
        integer, allocatable :: value_indices(:)
        integer, allocatable :: local_assignment_indices(:)
        type(token_t) :: token
        integer :: num_vars, num_values, i
        integer :: target_index, value_index
        integer :: literal_type
        integer :: assignment_line, assignment_column

        stmt_index = 0
        allocate (var_indices(0))
        allocate (value_indices(0))
        allocate (local_assignment_indices(0))

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER) then
                token = parser%consume()
                target_index = push_identifier(arena, token%text, token%line, &
                    token%column)
                var_indices = [var_indices, target_index]

                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()
                    cycle
                else if (token%kind == TK_OPERATOR .and. token%text == "=") then
                    token = parser%consume()
                    exit
                else
                    call release_local_assignments(local_assignment_indices)
                    return
                end if
            else
                call release_local_assignments(local_assignment_indices)
                return
            end if
        end do

        num_vars = size(var_indices)
        if (num_vars == 0) then
            call release_local_assignments(local_assignment_indices)
            return
        end if

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NUMBER .or. token%kind == TK_STRING .or. &
                token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD .or. &
                is_logical_operator_literal(token)) then
                token = parser%consume()
                literal_type = determine_literal_type(token%kind, token%text)
                value_index = push_literal(arena, token%text, literal_type, &
                    token%line, token%column)
                value_indices = [value_indices, value_index]

                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()
                    cycle
                else
                    exit
                end if
            else if (token%kind == TK_NEWLINE .or. parser%is_at_end()) then
                exit
            else
                token = parser%consume()
                exit
            end if
        end do

        num_values = size(value_indices)
        if (num_values == 0) then
            call release_local_assignments(local_assignment_indices)
            return
        end if

        do i = 1, num_vars
            if (i <= num_values) then
                target_index = var_indices(i)
                value_index = value_indices(i)
            else
                target_index = var_indices(i)
                value_index = value_indices(num_values)
            end if

            if (target_index > 0 .and. value_index > 0) then
                assignment_line = parser%tokens(parser%current_token - 1)%line
                assignment_column = parser%tokens(parser%current_token - 1)%column
                local_assignment_indices = [local_assignment_indices, &
                    push_assignment(arena, target_index, &
                    value_index, &
                    assignment_line, &
                    assignment_column)]
            end if
        end do

        if (size(local_assignment_indices) == 0) then
            call release_local_assignments(local_assignment_indices)
            return
        end if

        stmt_index = local_assignment_indices(1)
        call move_alloc(local_assignment_indices, assignment_indices)
    contains

        logical function is_logical_operator_literal(current_token) result(is_logical)
            type(token_t), intent(in) :: current_token

            is_logical = current_token%kind == TK_OPERATOR .and. &
                (current_token%text == ".true." .or. &
                current_token%text == ".false.")
        end function is_logical_operator_literal

        subroutine release_local_assignments(indices)
            integer, allocatable, intent(inout) :: indices(:)
            if (allocated(indices)) then
                block
                    integer, allocatable :: temp(:)
                    call move_alloc(indices, temp)
                end block
            end if
        end subroutine release_local_assignments

    end subroutine parse_multi_variable_assignment_core

    integer function determine_literal_type(token_kind, token_text) &
            result(literal_type)
        integer, intent(in) :: token_kind
        character(len=*), intent(in) :: token_text

        select case (token_kind)
        case (TK_NUMBER)
            if (index(token_text, ".") > 0 .or. index(token_text, "e") > 0 .or. &
                index(token_text, "E") > 0 .or. index(token_text, "d") > 0 .or. &
                index(token_text, "D") > 0) then
                literal_type = LITERAL_REAL
            else
                literal_type = LITERAL_INTEGER
            end if
        case (TK_STRING)
            literal_type = LITERAL_STRING
        case (TK_OPERATOR, TK_KEYWORD)
            if (trim(token_text) == ".true." .or. trim(token_text) == ".false.") then
                literal_type = LITERAL_LOGICAL
            else
                literal_type = LITERAL_STRING
            end if
        case default
            if (trim(token_text) == "true" .or. trim(token_text) == "false") then
                literal_type = LITERAL_LOGICAL
            else
                literal_type = LITERAL_STRING
            end if
        end select
    end function determine_literal_type

end module parser_assignment_shared_module
