module parser_submodule_placement_module
    ! Placement validation for the SUBMODULE statement.
    !
    ! A submodule is a program unit (Fortran 2018 R502 program-unit, R1116
    ! submodule, R1117 submodule-stmt). It can only appear at the top level of
    ! a program file. A SUBMODULE statement written inside another program unit
    ! or inside a derived-type definition is invalid; gfortran diagnoses it as
    ! "SUBMODULE declaration at (1) ...".
    !
    ! The recogniser below matches only the exact shape of R1117
    !     SUBMODULE ( parent-identifier [ : parent-submodule-name ] ) name
    ! so that ordinary uses of "submodule" as a variable name, such as
    ! "submodule = 3" or "submodule(i) = 3", are never rejected.
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD, TK_OPERATOR, &
        TK_NEWLINE, TK_COMMENT, TK_WHITESPACE, TK_EOF, to_lower
    use parser_state_module, only: parser_state_t
    implicit none
    private

    public :: at_submodule_statement
    public :: reject_misplaced_submodule

contains

    ! Index of the next token that carries syntax, or 0 when the statement ends.
    integer function next_significant(parser, from_index) result(idx)
        type(parser_state_t), intent(in) :: parser
        integer, intent(in) :: from_index
        type(token_t) :: token

        idx = from_index
        do while (idx <= parser%get_token_count())
            token = parser%get_token_at_index(idx)
            select case (token%kind)
            case (TK_WHITESPACE, TK_COMMENT)
                idx = idx + 1
            case (TK_NEWLINE, TK_EOF)
                idx = 0
                return
            case default
                return
            end select
        end do
        idx = 0
    end function next_significant

    logical function is_name_token(token) result(is_name)
        type(token_t), intent(in) :: token

        is_name = (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD)
    end function is_name_token

    logical function is_operator_token(token, text) result(matches)
        type(token_t), intent(in) :: token
        character(len=*), intent(in) :: text

        matches = .false.
        if (token%kind /= TK_OPERATOR) return
        if (.not. allocated(token%text)) return
        matches = (token%text == text)
    end function is_operator_token

    ! True when the parser sits on a complete R1117 submodule-stmt.
    logical function at_submodule_statement(parser) result(is_stmt)
        type(parser_state_t), intent(in) :: parser
        type(token_t) :: token
        integer :: idx

        is_stmt = .false.

        idx = parser%current_token
        if (idx < 1 .or. idx > parser%get_token_count()) return
        token = parser%get_token_at_index(idx)
        if (.not. is_name_token(token)) return
        if (.not. allocated(token%text)) return
        if (to_lower(trim(token%text)) /= "submodule") return

        idx = next_significant(parser, idx + 1)
        if (idx == 0) return
        if (.not. is_operator_token(parser%get_token_at_index(idx), "(")) return

        idx = next_significant(parser, idx + 1)
        if (idx == 0) return
        if (.not. is_name_token(parser%get_token_at_index(idx))) return

        idx = next_significant(parser, idx + 1)
        if (idx == 0) return
        if (is_operator_token(parser%get_token_at_index(idx), ":")) then
            idx = next_significant(parser, idx + 1)
            if (idx == 0) return
            if (.not. is_name_token(parser%get_token_at_index(idx))) return
            idx = next_significant(parser, idx + 1)
            if (idx == 0) return
        end if

        if (.not. is_operator_token(parser%get_token_at_index(idx), ")")) return

        idx = next_significant(parser, idx + 1)
        if (idx == 0) return
        if (.not. is_name_token(parser%get_token_at_index(idx))) return

        is_stmt = .true.
    end function at_submodule_statement

    ! Report and skip a SUBMODULE statement that is nested inside another
    ! construct. Returns .true. when a statement was rejected and consumed.
    logical function reject_misplaced_submodule(parser, context) result(rejected)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: context
        type(token_t) :: token

        rejected = .false.
        if (.not. at_submodule_statement(parser)) return

        token = parser%peek()
        call parser%error_at_token( &
            "SUBMODULE declaration is not allowed inside "//trim(context)// &
            "; a submodule is a program unit", token, &
            suggestion="move the submodule to file scope")

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE .or. token%kind == TK_EOF) exit
            token = parser%consume()
        end do

        rejected = .true.
    end function reject_misplaced_submodule

end module parser_submodule_placement_module
