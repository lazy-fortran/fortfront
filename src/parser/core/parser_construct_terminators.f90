module parser_construct_terminators_module
    ! Token-level validation of matching construct terminators.
    !
    ! Fortran requires every construct to be closed by its own END statement.
    ! This validator runs before the recursive-descent parser, where the raw
    ! token stream still carries the exact terminator spelling, and rejects
    ! sources whose terminators do not match the construct they close.
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_OPERATOR, &
        TK_NUMBER, TK_NEWLINE, TK_COMMENT, TK_WHITESPACE, TK_EOF, &
        to_lower
    implicit none
    private

    public :: validate_construct_terminators
    ! Token-stream helpers shared with the statement placement validator so
    ! that both validators walk statements the same way.
    public :: is_trivia, is_statement_start, skip_statement_label
    public :: next_significant, continues_statement, diagnostic

    type :: open_construct_t
        character(len=:), allocatable :: kind
        character(len=:), allocatable :: spec
        integer :: line = 0
        integer :: column = 0
    end type open_construct_t

contains

    subroutine validate_construct_terminators(tokens, error_msg)
        type(token_t), intent(in) :: tokens(:)
        character(len=:), allocatable, intent(out) :: error_msg

        type(open_construct_t), allocatable :: stack(:)
        integer :: depth
        integer :: i, j, k
        logical :: at_statement_start
        character(len=:), allocatable :: lowered

        error_msg = ""
        allocate (stack(0))
        depth = 0

        i = 1
        do while (i <= size(tokens))
            if (is_trivia(tokens(i))) then
                i = i + 1
                cycle
            end if
            if (tokens(i)%kind == TK_EOF) exit

            at_statement_start = is_statement_start(tokens, i)
            call check_binding_label(tokens, i, error_msg)
            if (len(error_msg) > 0) return

            if (.not. at_statement_start) then
                i = i + 1
                cycle
            end if

            j = skip_statement_label(tokens, i)
            lowered = to_lower(trim(tokens(j)%text))

            if (lowered == "abstract") then
                k = next_significant(tokens, j + 1)
                if (k > 0) then
                    if (to_lower(trim(tokens(k)%text)) == "interface") j = k
                end if
                lowered = to_lower(trim(tokens(j)%text))
            end if

            call handle_statement(tokens, j, lowered, stack, depth, error_msg)
            if (len(error_msg) > 0) return

            i = i + 1
        end do
    end subroutine validate_construct_terminators

    subroutine handle_statement(tokens, j, lowered, stack, depth, error_msg)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j
        character(len=*), intent(in) :: lowered
        type(open_construct_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: depth
        character(len=:), allocatable, intent(inout) :: error_msg

        integer :: k

        select case (trim(lowered))
        case ("use")
            call check_use_completeness(tokens, j, error_msg)
        case ("block")
            if (.not. starts_block_construct(tokens, j)) return
            call push_construct(stack, depth, "block", "", tokens(j))
        case ("do")
            if (.not. starts_do_construct(tokens, j)) return
            call push_construct(stack, depth, "do", "", tokens(j))
        case ("interface")
            call push_construct(stack, depth, "interface", &
                statement_remainder(tokens, j + 1), tokens(j))
        case ("endblock")
            call close_construct(stack, depth, "block", "", tokens(j), error_msg)
        case ("enddo")
            call close_construct(stack, depth, "do", "", tokens(j), error_msg)
        case ("endinterface")
            call close_construct(stack, depth, "interface", &
                statement_remainder(tokens, j + 1), tokens(j), error_msg)
        case ("end")
            k = next_significant(tokens, j + 1)
            if (.not. continues_statement(tokens, j, k)) then
                call close_bare_end(stack, depth, tokens(j), error_msg)
                return
            end if
            select case (to_lower(trim(tokens(k)%text)))
            case ("block")
                call close_construct(stack, depth, "block", "", tokens(j), error_msg)
            case ("do")
                call close_construct(stack, depth, "do", "", tokens(j), error_msg)
            case ("interface")
                call close_construct(stack, depth, "interface", &
                    statement_remainder(tokens, k + 1), tokens(j), error_msg)
            end select
        end select
    end subroutine handle_statement

    subroutine push_construct(stack, depth, kind, spec, token)
        type(open_construct_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: depth
        character(len=*), intent(in) :: kind
        character(len=*), intent(in) :: spec
        type(token_t), intent(in) :: token

        type(open_construct_t), allocatable :: grown(:)

        if (depth >= size(stack)) then
            allocate (grown(depth + 16))
            if (depth > 0) grown(1:depth) = stack(1:depth)
            call move_alloc(grown, stack)
        end if
        depth = depth + 1
        stack(depth)%kind = kind
        stack(depth)%spec = spec
        stack(depth)%line = token%line
        stack(depth)%column = token%column
    end subroutine push_construct

    subroutine close_construct(stack, depth, kind, spec, token, error_msg)
        type(open_construct_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: depth
        character(len=*), intent(in) :: kind
        character(len=*), intent(in) :: spec
        type(token_t), intent(in) :: token
        character(len=:), allocatable, intent(inout) :: error_msg

        ! An unmatched terminator without any tracked construct is left to the
        ! parser; only a genuine mismatch against an open construct is a
        ! terminator error here.
        if (depth == 0) return

        if (stack(depth)%kind /= kind) then
            error_msg = diagnostic("Expecting END"// &
                upper_kind(stack(depth)%kind)//" statement", token)
            return
        end if

        if (kind == "interface") then
            if (len_trim(stack(depth)%spec) > 0 .and. len_trim(spec) > 0) then
                if (normalized_spec(stack(depth)%spec) /= &
                    normalized_spec(spec)) then
                    error_msg = diagnostic("Expecting END INTERFACE "// &
                        trim(stack(depth)%spec)//" statement", token)
                    return
                end if
            end if
        end if

        depth = depth - 1
    end subroutine close_construct

    subroutine close_bare_end(stack, depth, token, error_msg)
        type(open_construct_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: depth
        type(token_t), intent(in) :: token
        character(len=:), allocatable, intent(inout) :: error_msg

        if (depth == 0) return
        if (stack(depth)%kind == "interface") return

        error_msg = diagnostic("END"//upper_kind(stack(depth)%kind)// &
            " statement expected", token)
    end subroutine close_bare_end

    ! A relational operator has two spellings that denote the same generic
    ! spec, so compare END INTERFACE specs in one normalised form.
    function normalized_spec(spec) result(normalized)
        character(len=*), intent(in) :: spec
        character(len=:), allocatable :: normalized
        character(len=4), parameter :: dotted(6) = &
            [".gt.", ".lt.", ".ge.", ".le.", ".eq.", ".ne."]
        character(len=2), parameter :: symbols(6) = &
            ["> ", "< ", ">=", "<=", "==", "/="]
        integer :: i, pos

        normalized = to_lower(trim(spec))
        do i = 1, size(dotted)
            pos = index(normalized, dotted(i))
            if (pos > 0) then
                normalized = normalized(1:pos - 1)//trim(symbols(i))// &
                    normalized(pos + len(dotted(i)):)
            end if
        end do
    end function normalized_spec

    function upper_kind(kind) result(text)
        character(len=*), intent(in) :: kind
        character(len=:), allocatable :: text

        select case (trim(kind))
        case ("block")
            text = " BLOCK"
        case ("do")
            text = " DO"
        case default
            text = " INTERFACE"
        end select
    end function upper_kind

    subroutine check_use_completeness(tokens, j, error_msg)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j
        character(len=:), allocatable, intent(inout) :: error_msg

        integer :: k, last

        last = 0
        k = next_significant(tokens, j + 1)
        do while (k > 0)
            last = k
            if (.not. continues_statement(tokens, last, &
                next_significant(tokens, last + 1))) exit
            k = next_significant(tokens, last + 1)
        end do

        if (last == 0) return
        if (tokens(last)%kind /= TK_OPERATOR) return
        if (trim(tokens(last)%text) /= ",") return

        ! A trailing comma continued on a following line is a line continuation,
        ! not a truncated ONLY list.
        k = next_significant(tokens, last + 1)
        if (k > 0) then
            if (tokens(k)%kind /= TK_EOF) return
        end if

        error_msg = diagnostic("Missing generic specification in USE statement", &
            tokens(last))
    end subroutine check_use_completeness

    subroutine check_binding_label(tokens, i, error_msg)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: i
        character(len=:), allocatable, intent(inout) :: error_msg

        integer :: prev, lparen, name, after

        if (to_lower(trim(tokens(i)%text)) /= "bind") return
        if (tokens(i)%kind /= TK_KEYWORD .and. tokens(i)%kind /= TK_IDENTIFIER) return

        prev = previous_significant(tokens, i - 1)
        if (prev == 0) return
        if (tokens(prev)%kind /= TK_OPERATOR) return
        if (trim(tokens(prev)%text) /= ")" .and. trim(tokens(prev)%text) /= "," &
            .and. trim(tokens(prev)%text) /= "::") return

        lparen = next_significant(tokens, i + 1)
        if (lparen == 0) return
        if (tokens(lparen)%kind /= TK_OPERATOR) return
        if (trim(tokens(lparen)%text) /= "(") return

        name = next_significant(tokens, lparen + 1)
        if (name == 0) return
        if (to_lower(trim(tokens(name)%text)) == "c") then
            after = next_significant(tokens, name + 1)
            if (after == 0) return
            if (tokens(after)%kind /= TK_OPERATOR) return
            if (trim(tokens(after)%text) == ")" .or. &
                trim(tokens(after)%text) == ",") return
        end if

        error_msg = diagnostic("Missing closing paren for binding label", &
            tokens(name))
    end subroutine check_binding_label

    logical function starts_block_construct(tokens, j) result(is_construct)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j

        integer :: k

        is_construct = .false.
        if (tokens(j)%kind /= TK_KEYWORD) return
        k = next_significant(tokens, j + 1)
        ! BLOCK DATA is a program unit, not a BLOCK construct.
        if (k > 0) then
            if (to_lower(trim(tokens(k)%text)) == "data") return
        end if
        ! A bare BLOCK statement stands alone on its line; anything else is a
        ! use of BLOCK as an ordinary name.
        is_construct = .not. continues_statement(tokens, j, k)
    end function starts_block_construct

    logical function starts_do_construct(tokens, j) result(is_construct)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: j

        integer :: k

        is_construct = .false.
        if (tokens(j)%kind /= TK_KEYWORD) return
        k = next_significant(tokens, j + 1)
        ! A labelled DO is terminated by its label, not by END DO. The label
        ! belongs to the DO statement itself, so it must be on the same line.
        if (continues_statement(tokens, j, k)) then
            if (tokens(k)%kind == TK_NUMBER) return
        end if
        is_construct = .true.
    end function starts_do_construct

    function statement_remainder(tokens, start) result(text)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start
        character(len=:), allocatable :: text

        integer :: k, prev

        text = ""
        prev = start - 1
        k = next_significant(tokens, start)
        do while (k > 0)
            if (.not. continues_statement(tokens, prev, k)) exit
            text = text//to_lower(trim(tokens(k)%text))
            prev = k
            k = next_significant(tokens, k + 1)
        end do
    end function statement_remainder

    logical function continues_statement(tokens, prev, k) result(continues)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: prev
        integer, intent(in) :: k

        continues = .false.
        if (k <= 0) return
        if (tokens(k)%kind == TK_EOF) return
        if (tokens(k)%kind == TK_OPERATOR) then
            if (trim(tokens(k)%text) == ";") return
        end if
        if (prev >= 1) then
            if (tokens(k)%line /= tokens(prev)%line) return
        end if
        continues = .true.
    end function continues_statement

    logical function is_statement_start(tokens, i) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: i

        integer :: prev

        prev = previous_significant(tokens, i - 1)
        if (prev == 0) then
            is_start = .true.
            return
        end if
        if (tokens(prev)%kind == TK_OPERATOR) then
            if (trim(tokens(prev)%text) == ";") then
                is_start = .true.
                return
            end if
        end if
        is_start = tokens(prev)%line /= tokens(i)%line
    end function is_statement_start

    integer function skip_statement_label(tokens, i) result(j)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: i

        integer :: colon, after

        j = i
        ! A numeric statement label may precede any statement, including a
        ! construct terminator used as a branch target.
        if (tokens(i)%kind == TK_NUMBER) then
            after = next_significant(tokens, i + 1)
            if (after == 0) return
            if (.not. continues_statement(tokens, i, after)) return
            j = after
            return
        end if
        if (tokens(i)%kind /= TK_IDENTIFIER) return
        colon = next_significant(tokens, i + 1)
        if (colon == 0) return
        if (tokens(colon)%kind /= TK_OPERATOR) return
        if (trim(tokens(colon)%text) /= ":") return
        after = next_significant(tokens, colon + 1)
        if (after == 0) return
        if (.not. continues_statement(tokens, i, after)) return
        j = after
    end function skip_statement_label

    integer function next_significant(tokens, start) result(k)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start

        k = start
        do while (k >= 1 .and. k <= size(tokens))
            if (.not. is_trivia(tokens(k))) return
            k = k + 1
        end do
        k = 0
    end function next_significant

    integer function previous_significant(tokens, start) result(k)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start

        k = start
        do while (k >= 1)
            if (.not. is_trivia(tokens(k))) return
            k = k - 1
        end do
        k = 0
    end function previous_significant

    logical function is_trivia(token) result(trivia)
        type(token_t), intent(in) :: token

        trivia = token%kind == TK_WHITESPACE .or. token%kind == TK_NEWLINE .or. &
            token%kind == TK_COMMENT
    end function is_trivia

    function diagnostic(message, token) result(text)
        character(len=*), intent(in) :: message
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: text

        character(len=32) :: line_text, column_text

        write (line_text, '(I0)') token%line
        write (column_text, '(I0)') token%column
        text = trim(message)//" at line "//trim(line_text)//", column "// &
            trim(column_text)
    end function diagnostic

end module parser_construct_terminators_module
