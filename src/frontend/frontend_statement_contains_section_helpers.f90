module frontend_statement_token_walking_helpers
    use lexer_core, only: token_t, TK_COMMENT, TK_EOF, TK_IDENTIFIER, TK_KEYWORD, &
        TK_NEWLINE, TK_NUMBER, TK_OPERATOR, TK_WHITESPACE, to_lower

    implicit none
    private

    public :: find_proc_keyword_after_type
    public :: find_procedure_end
    public :: skip_type_spec
    public :: token_is_ignorable
    public :: token_is_word

contains

    logical function token_is_ignorable(kind) result(ignorable)
        integer, intent(in) :: kind
        ignorable = (kind == TK_WHITESPACE .or. kind == TK_NEWLINE .or. &
            kind == TK_COMMENT)
    end function token_is_ignorable

    logical function token_is_word(kind) result(is_word)
        integer, intent(in) :: kind
        is_word = (kind == TK_KEYWORD .or. kind == TK_IDENTIFIER)
    end function token_is_word

    integer function find_proc_keyword_after_type(tokens, start_pos) result(proc_pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer :: pos, paren_depth
        character(len=:), allocatable :: lowered

        proc_pos = 0
        pos = start_pos

        pos = pos + 1

        do while (pos <= size(tokens))
            select case (tokens(pos)%kind)
            case (TK_WHITESPACE)
                pos = pos + 1
            case (TK_KEYWORD, TK_IDENTIFIER)
                lowered = to_lower(trim(tokens(pos)%text))
                if (lowered == "function" .or. lowered == "subroutine") then
                    proc_pos = pos
                    return
                else if (lowered == "precision" .or. lowered == "complex") then
                    pos = pos + 1
                else
                    pos = pos + 1
                end if
            case (TK_OPERATOR)
                if (tokens(pos)%text == "(") then
                    paren_depth = 1
                    pos = pos + 1
                    do while (pos <= size(tokens) .and. paren_depth > 0)
                        if (tokens(pos)%kind == TK_OPERATOR) then
                            if (tokens(pos)%text == "(") then
                                paren_depth = paren_depth + 1
                            end if
                            if (tokens(pos)%text == ")") then
                                paren_depth = paren_depth - 1
                            end if
                        end if
                        pos = pos + 1
                    end do
                else if (tokens(pos)%text == "*") then
                    pos = pos + 1
                    if (pos <= size(tokens)) then
                        if (tokens(pos)%kind == TK_NUMBER) pos = pos + 1
                    end if
                else
                    pos = pos + 1
                end if
            case (TK_NEWLINE, TK_COMMENT, TK_EOF)
                return
            case default
                pos = pos + 1
            end select
        end do
    end function find_proc_keyword_after_type

    subroutine skip_type_spec(tokens, pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(inout) :: pos
        integer :: paren_depth
        character(len=:), allocatable :: lowered

        pos = pos + 1

        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_WHITESPACE) pos = pos + 1
            if (pos <= size(tokens)) then
                if (tokens(pos)%kind == TK_KEYWORD .or. &
                    tokens(pos)%kind == TK_IDENTIFIER) then
                    lowered = to_lower(trim(tokens(pos)%text))
                    if (lowered == "precision" .or. lowered == "complex") then
                        pos = pos + 1
                    end if
                end if
            end if
        end if

        if (pos <= size(tokens)) then
            if (tokens(pos)%kind == TK_WHITESPACE) pos = pos + 1
            if (pos <= size(tokens)) then
                if (tokens(pos)%kind == TK_OPERATOR) then
                    if (tokens(pos)%text == "(") then
                        paren_depth = 1
                        pos = pos + 1
                        do while (pos <= size(tokens) .and. paren_depth > 0)
                            if (tokens(pos)%kind == TK_OPERATOR) then
                                if (tokens(pos)%text == "(") then
                                    paren_depth = paren_depth + 1
                                end if
                                if (tokens(pos)%text == ")") then
                                    paren_depth = paren_depth - 1
                                end if
                            end if
                            pos = pos + 1
                        end do
                    else if (tokens(pos)%text == "*") then
                        pos = pos + 1
                        if (pos <= size(tokens)) then
                            if (tokens(pos)%kind == TK_NUMBER) pos = pos + 1
                        end if
                    end if
                end if
            end if
        end if
    end subroutine skip_type_spec

    subroutine find_procedure_end(tokens, start_pos, proc_type, end_pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        character(len=*), intent(in) :: proc_type
        integer, intent(out) :: end_pos
        integer :: i, next_idx
        character(len=:), allocatable :: lowered, next_lower, combined

        end_pos = start_pos
        combined = "end" // trim(proc_type)

        do i = start_pos + 1, size(tokens)
            if (tokens(i)%kind == TK_EOF) then
                end_pos = i - 1
                exit
            end if

            if (tokens(i)%kind == TK_KEYWORD .or. tokens(i)%kind == TK_IDENTIFIER) then
                lowered = to_lower(trim(tokens(i)%text))

                if (lowered == combined) then
                    end_pos = i
                    next_idx = i + 1
                    do while (next_idx <= size(tokens))
                        if (tokens(next_idx)%kind == TK_WHITESPACE) then
                            next_idx = next_idx + 1
                        else if (tokens(next_idx)%kind == TK_IDENTIFIER .or. &
                                tokens(next_idx)%kind == TK_KEYWORD) then
                            end_pos = next_idx
                            exit
                        else
                            exit
                        end if
                    end do
                    exit
                end if

                if (lowered == "end") then
                    next_idx = i + 1
                    do while (next_idx <= size(tokens))
                        if (tokens(next_idx)%kind == TK_WHITESPACE) then
                            next_idx = next_idx + 1
                        else
                            exit
                        end if
                    end do

                    ! A bare END statement is the END of this procedure
                    ! (Fortran 2023 R1537/R1503: the subprogram name and even
                    ! the SUBROUTINE/FUNCTION keyword are optional). Without
                    ! this, the scan kept looking for "end subroutine" and
                    ! swallowed every following sibling procedure into this
                    ! one, so the host's CONTAINS section listed only the
                    ! first of them.
                    if (end_statement_is_bare(tokens, next_idx)) then
                        end_pos = i
                        exit
                    end if

                    if (next_idx <= size(tokens)) then
                        if (tokens(next_idx)%kind == TK_KEYWORD .or. &
                            tokens(next_idx)%kind == TK_IDENTIFIER) then
                            next_lower = to_lower(trim(tokens(next_idx)%text))
                            if (next_lower == proc_type) then
                                end_pos = next_idx
                                next_idx = next_idx + 1
                                do while (next_idx <= size(tokens))
                                    if (tokens(next_idx)%kind == TK_WHITESPACE) then
                                        next_idx = next_idx + 1
                                    else if (tokens(next_idx)%kind == &
                                            TK_IDENTIFIER .or. &
                                            tokens(next_idx)%kind == TK_KEYWORD) then
                                        end_pos = next_idx
                                        exit
                                    else
                                        exit
                                    end if
                                end do
                                exit
                            end if
                        end if
                    end if
                end if
            end if

            end_pos = i
        end do
    end subroutine find_procedure_end

    ! True when nothing but the statement terminator follows the END keyword,
    ! i.e. the statement is "END" on its own rather than "END SUBROUTINE",
    ! "END IF" or any other construct terminator.
    logical function end_statement_is_bare(tokens, next_idx) result(is_bare)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: next_idx

        is_bare = .true.
        if (next_idx > size(tokens)) return

        select case (tokens(next_idx)%kind)
        case (TK_NEWLINE, TK_COMMENT, TK_EOF)
            return
        case (TK_OPERATOR)
            is_bare = trim(tokens(next_idx)%text) == ";"
        case default
            is_bare = .false.
        end select
    end function end_statement_is_bare

end module frontend_statement_token_walking_helpers
