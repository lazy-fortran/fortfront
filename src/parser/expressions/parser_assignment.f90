module parser_assignment_module
    use lexer_core, only: token_t, TK_OPERATOR, TK_EOF, &
        TK_STRING, &
        TK_KEYWORD, TK_NEWLINE
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_expressions_module, only: parse_range, parse_logical_eqv, &
        at_statement_end
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_assignment, push_pointer_assignment, &
        push_identifier, push_literal, push_complex_literal
    use parser_assignment_shared_module, only: parse_multi_variable_assignment_core
    implicit none
    private

    public :: parse_assignment_statement

contains

    subroutine parse_assignment_statement(parser, arena, stmt_index, extra_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        integer, allocatable, intent(inout), optional :: extra_indices(:)
        type(token_t) :: id_token, op_token
        integer :: target_index, value_index
        character(len=:), allocatable :: assignment_op
        logical :: is_multi_assignment

        stmt_index = 0

        is_multi_assignment = is_multi_var_assignment(parser)

        if (is_multi_assignment) then
            call parse_multi_variable_assignment(parser, arena, stmt_index, &
                extra_indices)
        else
            id_token = parser%consume()
            op_token = parser%peek()

            if (allocated(assignment_op)) then
                block
                    character(len=:), allocatable :: temp
                    call move_alloc(assignment_op, temp)
                end block
            end if

            if (name_does_not_begin_a_statement(op_token)) then
                call parser%error_at_token( &
                    "Unclassifiable statement: '"//trim(id_token%text)// &
                    "' is a name, and a name can only start an assignment", &
                    id_token)
            else if (op_token%kind == TK_OPERATOR) then
                select case (op_token%text)
                case ("=")
                    op_token = parser%consume()
                    target_index = push_identifier(arena, id_token%text, &
                        id_token%line, &
                        id_token%column)
                    ! Use parse_logical_eqv to exclude = operator (prevents chained
                    ! assignment like a = b = c which is not valid Fortran)
                    value_index = parse_logical_eqv(parser, arena)
                    if (value_index > 0) then
                        stmt_index = push_assignment(arena, target_index, &
                            value_index, &
                            id_token%line, id_token%column)
                    end if
                case (":=")
                    ! Walrus declaration-with-inference (LFortran extension):
                    ! name := expr declares name and infers its type from expr.
                    op_token = parser%consume()
                    target_index = push_identifier(arena, id_token%text, &
                        id_token%line, &
                        id_token%column)
                    value_index = parse_logical_eqv(parser, arena)
                    if (value_index > 0) then
                        stmt_index = push_assignment(arena, target_index, &
                            value_index, &
                            id_token%line, id_token%column, &
                            is_walrus=.true.)
                    end if
                case ("=>")
                    op_token = parser%consume()
                    target_index = push_identifier(arena, id_token%text, &
                        id_token%line, &
                        id_token%column)
                    ! Use parse_logical_eqv to exclude = operator (prevents chained
                    ! pointer assignment)
                    value_index = parse_logical_eqv(parser, arena)
                    if (value_index > 0) then
                        stmt_index = push_pointer_assignment(arena, target_index, &
                            value_index, &
                            id_token%line, &
                            id_token%column)
                    end if
                case ("(", "%", ".")
                    block
                        integer :: start_pos, pos, paren_depth, left_end, lhs_len
                        type(token_t), allocatable, target :: lhs_tokens(:)
                        type(parser_state_t) :: lhs_parser

                        start_pos = parser%current_token - 1
                        paren_depth = 0
                        left_end = start_pos
                        do pos = parser%current_token, size(parser%tokens)
                            if (parser%tokens(pos)%kind == TK_EOF) exit
                            select case (parser%tokens(pos)%text)
                            case ("(")
                                paren_depth = paren_depth + 1
                            case (")")
                                if (paren_depth > 0) paren_depth = paren_depth - 1
                            case ("=", "=>")
                                if (paren_depth == 0) then
                                    left_end = pos - 1
                                    exit
                                end if
                            end select
                            left_end = pos
                        end do

                        lhs_len = (left_end - start_pos + 1) + 1
                        allocate (lhs_tokens(lhs_len))
                        lhs_tokens(1) = id_token
                        if (left_end >= parser%current_token) then
                            lhs_tokens(2:1 + (left_end - parser%current_token + 1)) = &
                                parser%tokens(parser%current_token:left_end)
                        end if
                        lhs_tokens(lhs_len)%kind = TK_EOF
                        lhs_tokens(lhs_len)%text = ""
                        lhs_tokens(lhs_len)%line = id_token%line
                        lhs_tokens(lhs_len)%column = id_token%column

                        lhs_parser = create_parser_state(lhs_tokens)
                        if (associated(parser%diagnostic_sink)) then
                            lhs_parser%diagnostic_sink => parser%diagnostic_sink
                        end if
                        target_index = parse_range(lhs_parser, arena)
                    end block

                    do while (.not. parser%is_at_end())
                        op_token = parser%peek()
                        if (op_token%kind == TK_OPERATOR .and. &
                            (op_token%text == "=" .or. op_token%text == "=>")) then
                            op_token = parser%consume()
                            assignment_op = op_token%text
                            exit
                        else
                            op_token = parser%consume()
                        end if
                    end do

                    ! Use parse_logical_eqv to exclude = operator (prevents chained
                    ! assignment in array/derived type assignments)
                    value_index = parse_logical_eqv(parser, arena)
                    if (value_index > 0 .and. target_index > 0) then
                        if (.not. allocated(assignment_op)) assignment_op = "="
                        if (assignment_op == "=>") then
                            stmt_index = push_pointer_assignment(arena, target_index, &
                                value_index, &
                                id_token%line, &
                                id_token%column)
                        else
                            stmt_index = push_assignment(arena, target_index, &
                                value_index, &
                                id_token%line, &
                                id_token%column, &
                                operator_text=assignment_op)
                        end if
                    end if
                end select
            end if
            if (stmt_index > 0) call reject_trailing_tokens(parser)
        end if
    end subroutine parse_assignment_statement

    ! A leading name reaches this parser only when no earlier dispatcher
    ! claimed the statement, so the only thing it can still be is the target
    ! of an assignment or a pointer assignment. One follower rules that out:
    ! a comma, because is_multi_var_assignment has already established that
    ! the statement holds no '=' at all, so this is not a multi-target
    ! assignment either.
    !
    ! Free form requires a blank between a statement keyword and a following
    ! name (Fortran 2023 6.3.2.1), so "printf, 'check'" lexes as the single
    ! name "printf" rather than PRINT with format f, which is what
    ! gfortran.dg/print_fmt_2.f90 diagnoses.
    !
    ! A name that simply ends the statement is deliberately not reported.
    ! It is equally unclassifiable, but the parser currently leaves the
    ! trailing name of constructs such as "end type derived_type" unconsumed
    ! and relies on the statement loop skipping it, so rejecting that shape
    ! would reject valid programs.
    logical function name_does_not_begin_a_statement(follower) result(is_bad)
        type(token_t), intent(in) :: follower

        is_bad = .false.
        if (follower%kind /= TK_OPERATOR) return
        is_bad = trim(follower%text) == ","
    end function name_does_not_begin_a_statement

    ! An assignment statement is complete once its right-hand expression is
    ! parsed; anything other than a statement terminator after it leaves the
    ! statement unclassifiable (gfortran.dg/pr56520.f90, "c = exp(a) )").
    ! Reporting it here keeps the stray tokens from being silently skipped by
    ! the statement loop.
    subroutine reject_trailing_tokens(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        if (at_statement_end(parser)) return

        token = parser%peek()
        call parser%error_at_token( &
            "Unclassifiable statement: unexpected '"//trim(token%text)// &
            "' after the end of the assignment", token)
    end subroutine reject_trailing_tokens

    ! Both scans stop at the end of the current statement. Reading on into the
    ! following lines made an unrelated later assignment supply the '=' that
    ! this statement lacks, so "printf, 'check'" followed anywhere below by
    ! "x = 1" was treated as the start of a multi-target assignment.
    logical function is_multi_var_assignment(parser)
        type(parser_state_t), intent(in) :: parser
        integer :: pos
        logical :: saw_equals

        is_multi_var_assignment = .false.
        saw_equals = .false.

        do pos = parser%current_token, size(parser%tokens)
            if (token_ends_statement(parser%tokens(pos))) exit
            select case (parser%tokens(pos)%kind)
            case (TK_OPERATOR)
                if (parser%tokens(pos)%text == "=") then
                    saw_equals = .true.
                    exit
                else if (parser%tokens(pos)%text == "=>") then
                    ! Pointer assignment, not multi-var assignment
                    return
                end if
            case default
                cycle
            end select
        end do

        if (.not. saw_equals) return

        pos = parser%current_token
        block
            integer :: paren_depth
            character(len=:), allocatable :: text
            paren_depth = 0

            do while (pos < size(parser%tokens))
                if (token_ends_statement(parser%tokens(pos))) exit
                if (parser%tokens(pos)%kind == TK_OPERATOR) then
                    text = parser%tokens(pos)%text
                    select case (text)
                    case ("(", "[")
                        paren_depth = paren_depth + 1
                    case (")", "]")
                        if (paren_depth > 0) paren_depth = paren_depth - 1
                    case (",")
                        if (paren_depth == 0) then
                            is_multi_var_assignment = .true.
                            return
                        end if
                    case ("=")
                        if (paren_depth == 0) exit
                    end select
                end if
                pos = pos + 1
            end do
        end block
    end function is_multi_var_assignment

    logical function token_ends_statement(token) result(is_end)
        type(token_t), intent(in) :: token

        select case (token%kind)
        case (TK_EOF, TK_NEWLINE)
            is_end = .true.
        case (TK_OPERATOR)
            is_end = trim(token%text) == ";"
        case default
            is_end = .false.
        end select
    end function token_ends_statement

    subroutine parse_multi_variable_assignment(parser, arena, stmt_index, &
            extra_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(out) :: stmt_index
        integer, allocatable, intent(inout), optional :: extra_indices(:)
        integer, allocatable :: assignment_indices(:)

        call parse_multi_variable_assignment_core(parser, arena, stmt_index, &
            assignment_indices)

        if (stmt_index == 0) then
            if (allocated(assignment_indices)) then
                block
                    integer, allocatable :: temp(:)
                    call move_alloc(assignment_indices, temp)
                end block
            end if
            return
        end if

        if (allocated(assignment_indices)) then
            if (size(assignment_indices) > 1) then
                if (present(extra_indices)) then
                    if (allocated(extra_indices)) then
                        block
                            integer, allocatable :: temp(:)
                            call move_alloc(extra_indices, temp)
                        end block
                    end if
                    extra_indices = assignment_indices(2:)
                end if
            end if
        end if
    end subroutine parse_multi_variable_assignment

end module parser_assignment_module
