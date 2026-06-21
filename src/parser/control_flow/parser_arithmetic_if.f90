module parser_arithmetic_if_module
    ! Parser module for arithmetic IF statements: IF (expr) label1, label2, label3
    !
    ! ISO/IEC 1539-1:2018 Compliance:
    ! - Arithmetic IF is a DELETED FEATURE per Annex B.3 item 1
    !   (deleted from Fortran 2008, carried forward to 2018 as deleted)
    ! - fortfront accepts arithmetic IF for legacy code compatibility
    ! - All output uses STANDARD block IF constructs per section 11.1.8
    !
    ! Transformation:
    !   IF (expr) label1, label2, label3
    ! becomes standard-conforming:
    !   IF (expr < 0) THEN
    !       GO TO label1
    !   ELSE IF (expr == 0) THEN
    !       GO TO label2
    !   ELSE
    !       GO TO label3
    !   END IF
    use lexer_core, only: token_t, TK_OPERATOR, TK_NEWLINE, TK_EOF, &
                          TK_COMMENT, TK_WHITESPACE, TK_NUMBER, TK_IDENTIFIER
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: LITERAL_INTEGER
    use ast_factory, only: push_if, push_goto, push_binary_op, push_literal
    implicit none
    private

    public :: is_arithmetic_if, parse_arithmetic_if

contains

    ! Detect arithmetic IF pattern: IF (expr) label1, label2, label3
    ! Per ISO/IEC 1539-1:2018 Annex B.3 item 1, this is a deleted feature.
    ! Returns true if token stream matches the arithmetic IF pattern:
    !   exactly 3 numeric labels separated by 2 commas after closing paren
    function is_arithmetic_if(parser) result(is_arith_if)
        type(parser_state_t), intent(in) :: parser
        logical :: is_arith_if
        type(token_t) :: tok
        integer :: idx, label_count, comma_count, total_tokens
        logical :: expect_label
        integer :: start_line

        is_arith_if = .false.
        if (.not. associated(parser%tokens)) return

        total_tokens = size(parser%tokens)
        idx = parser%current_token
        if (idx < 1 .or. idx > total_tokens) return

        ! Skip whitespace/comments before evaluating pattern
        do while (idx <= total_tokens)
            tok = parser%tokens(idx)
            if (tok%kind == TK_WHITESPACE .or. tok%kind == TK_COMMENT) then
                idx = idx + 1
                cycle
            end if
            exit
        end do
        if (idx > total_tokens) return
        if (parser%tokens(idx)%kind == TK_NEWLINE) return

        start_line = parser%tokens(idx)%line
        expect_label = .true.
        label_count = 0
        comma_count = 0

        do while (idx <= total_tokens)
            tok = parser%tokens(idx)
            if (tok%kind == TK_WHITESPACE .or. tok%kind == TK_COMMENT) then
                idx = idx + 1
                cycle
            end if
            if (tok%kind == TK_NEWLINE .or. tok%kind == TK_EOF) exit
            if (tok%line /= start_line) exit

            if (expect_label) then
                if (tok%kind == TK_NUMBER) then
                    label_count = label_count + 1
                    expect_label = .false.
                    if (label_count > 3) return
                else
                    return
                end if
            else
                if (tok%kind == TK_OPERATOR .and. tok%text == ",") then
                    comma_count = comma_count + 1
                    expect_label = .true.
                else
                    return
                end if
            end if

            idx = idx + 1
            if (.not. expect_label .and. label_count == 3) exit
        end do

        if (label_count == 3 .and. comma_count == 2 .and. .not. expect_label) then
            ! Ensure no additional tokens on the same line (besides whitespace/comment)
            do while (idx <= total_tokens)
                tok = parser%tokens(idx)
                if (tok%kind == TK_WHITESPACE .or. tok%kind == TK_COMMENT) then
                    idx = idx + 1
                    cycle
                end if
                if (tok%kind == TK_NEWLINE .or. tok%kind == TK_EOF) exit
                if (tok%line /= start_line) exit
                return
            end do
            is_arith_if = .true.
        end if
    end function is_arithmetic_if

    ! Parse and transform arithmetic IF to standard block IF construct
    ! Per ISO/IEC 1539-1:2018 section 11.1.8 (IF construct and statement)
    !
    ! Input (deleted feature per Annex B.3):
    !   IF (expr) label1, label2, label3
    !
    ! Output (standard-conforming):
    !   IF (expr < 0) THEN
    !       GO TO label1
    !   ELSE IF (expr == 0) THEN
    !       GO TO label2
    !   ELSE
    !       GO TO label3
    !   END IF
    function parse_arithmetic_if(parser, arena, condition_index, if_token, &
                                 parent_index) result(if_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: condition_index
        type(token_t), intent(in) :: if_token
        integer, intent(in), optional :: parent_index
        integer :: if_index

        character(len=:), allocatable :: label1, label2, label3
        integer :: zero_index, cond_lt_zero_index, cond_eq_zero_index
        integer :: goto1_index, goto2_index, goto3_index
        integer, allocatable :: then_body_indices(:), else_body_indices(:)
        integer, allocatable :: elseif_indices(:)

        if_index = 0

        ! Parse arithmetic IF label triplet
        if (.not. consume_arith_if_label(parser, label1)) return
        if (.not. consume_arith_if_comma(parser)) return
        if (.not. consume_arith_if_label(parser, label2)) return
        if (.not. consume_arith_if_comma(parser)) return
        if (.not. consume_arith_if_label(parser, label3)) return

        ! Build: IF (expr < 0) GOTO label1
        zero_index = push_literal(arena, "0", LITERAL_INTEGER, if_token%line, &
                                  if_token%column)
        cond_lt_zero_index = push_binary_op(arena, condition_index, zero_index, "<", &
                                            if_token%line, if_token%column)
        goto1_index = push_goto(arena, label=label1, line=if_token%line, &
                                column=if_token%column)
        allocate (then_body_indices(1))
        then_body_indices(1) = goto1_index

        ! Build: ELSEIF (expr == 0) GOTO label2
        cond_eq_zero_index = push_binary_op(arena, condition_index, zero_index, "==", &
                                            if_token%line, if_token%column)
        goto2_index = push_goto(arena, label=label2, line=if_token%line, &
                                column=if_token%column)
        allocate (elseif_indices(2))
        elseif_indices(1) = cond_eq_zero_index
        elseif_indices(2) = goto2_index

        ! Build: ELSE GOTO label3
        goto3_index = push_goto(arena, label=label3, line=if_token%line, &
                                column=if_token%column)
        allocate (else_body_indices(1))
        else_body_indices(1) = goto3_index

        ! Create IF node
        if_index = push_if(arena, cond_lt_zero_index, then_body_indices, &
                           elseif_indices=elseif_indices, &
                           else_body_indices=else_body_indices, &
                           line=if_token%line, column=if_token%column, &
                           parent_index=parent_index)
    end function parse_arithmetic_if

    logical function fetch_next_label_token(parser, token)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), intent(out) :: token

        fetch_next_label_token = .false.
        do
            if (parser%is_at_end()) return
            token = parser%peek()
            if (token%kind == TK_WHITESPACE .or. token%kind == TK_COMMENT) then
                token = parser%consume()
                cycle
            end if
            if (token%kind == TK_NEWLINE) return
            fetch_next_label_token = .true.
            return
        end do
    end function fetch_next_label_token

    logical function consume_arith_if_label(parser, label_text)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: label_text
        type(token_t) :: next_token

        if (.not. fetch_next_label_token(parser, next_token)) then
            label_text = ""
            consume_arith_if_label = .false.
            return
        end if

        if (next_token%kind == TK_NUMBER) then
            label_text = trim(next_token%text)
            next_token = parser%consume()
            consume_arith_if_label = .true.
        else
            label_text = ""
            consume_arith_if_label = .false.
        end if
    end function consume_arith_if_label

    logical function consume_arith_if_comma(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: next_token

        if (.not. fetch_next_label_token(parser, next_token)) then
            consume_arith_if_comma = .false.
            return
        end if

        if (next_token%kind == TK_OPERATOR .and. next_token%text == ",") then
            next_token = parser%consume()
            consume_arith_if_comma = .true.
        else
            consume_arith_if_comma = .false.
        end if
    end function consume_arith_if_comma

end module parser_arithmetic_if_module
