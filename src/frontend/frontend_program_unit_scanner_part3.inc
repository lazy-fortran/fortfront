    logical function token_starts_structural_contains(tokens, pos) result(is_contains)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        integer :: i

        is_contains = .false.
        if (.not. token_is_first_on_line(tokens, pos)) return
        if (.not. token_is_word_at(tokens, pos, "contains")) return

        do i = pos + 1, size(tokens)
            select case (tokens(i)%kind)
            case (TK_WHITESPACE)
                cycle
            case (TK_COMMENT, TK_NEWLINE, TK_EOF)
                is_contains = .true.
                return
            case default
                return
            end select
        end do
    end function token_starts_structural_contains

    logical function token_starts_bare_end_statement(tokens, pos) result(is_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        integer :: i

        is_end = .false.
        if (.not. token_is_first_on_line(tokens, pos)) return
        if (.not. token_is_word_at(tokens, pos, "end")) return

        do i = pos + 1, size(tokens)
            select case (tokens(i)%kind)
            case (TK_WHITESPACE)
                cycle
            case (TK_COMMENT, TK_NEWLINE, TK_EOF)
                is_end = .true.
                return
            case default
                return
            end select
        end do
    end function token_starts_bare_end_statement

    integer function find_contains_procedure_start(tokens, pos) result(proc_pos)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        integer :: i
        character(len=:), allocatable :: lowered

        proc_pos = 0
        if (.not. token_is_first_on_line(tokens, pos)) return

        lowered = ""
        if (tokens(pos)%kind == TK_KEYWORD .or. tokens(pos)%kind == TK_IDENTIFIER) then
            lowered = to_lower(trim(tokens(pos)%text))
        end if

        if (lowered == "subroutine" .or. lowered == "function") then
            proc_pos = pos
            return
        end if

        if (.not. token_may_prefix_procedure(lowered)) return

        do i = pos + 1, size(tokens)
            select case (tokens(i)%kind)
            case (TK_WHITESPACE)
                cycle
            case (TK_COMMENT, TK_NEWLINE, TK_EOF)
                return
            case (TK_KEYWORD, TK_IDENTIFIER)
                lowered = to_lower(trim(tokens(i)%text))
                if (lowered == "subroutine" .or. lowered == "function") then
                    proc_pos = i
                end if
                return
            case default
                return
            end select
        end do
    end function find_contains_procedure_start

    logical function token_may_prefix_procedure(lowered) result(is_prefix)
        character(len=*), intent(in) :: lowered

        is_prefix = (lowered == "pure" .or. lowered == "elemental" .or. &
                     lowered == "impure" .or. lowered == "recursive" .or. &
                     lowered == "module" .or. lowered == "nonrecursive")
    end function token_may_prefix_procedure

    logical function token_is_word_at(tokens, pos, word) result(matches)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        character(len=*), intent(in) :: word

        matches = .false.
        if (pos < 1 .or. pos > size(tokens)) return
        if (tokens(pos)%kind /= TK_KEYWORD .and. tokens(pos)%kind /= TK_IDENTIFIER) &
            return
        matches = (to_lower(trim(tokens(pos)%text)) == word)
    end function token_is_word_at

    logical function token_is_first_on_line(tokens, pos) result(is_first)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        integer :: i

        is_first = .false.
        if (pos < 1 .or. pos > size(tokens)) return

        i = pos - 1
        do while (i >= 1)
            select case (tokens(i)%kind)
            case (TK_WHITESPACE)
                i = i - 1
            case (TK_COMMENT)
                i = i - 1
            case (TK_NEWLINE)
                is_first = .true.
                return
            case default
                return
            end select
        end do

        is_first = .true.
    end function token_is_first_on_line

    integer function find_next_nontrivial_index(tokens, pos) result(idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        integer :: i

        idx = 0
        if (pos >= size(tokens)) return

        do i = pos + 1, size(tokens)
            select case (tokens(i)%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                cycle
            case default
                idx = i
                return
            end select
        end do
    end function find_next_nontrivial_index

    logical function token_precedes_identifier(token) result(is_valid)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        is_valid = .false.
        if (token%kind /= TK_OPERATOR) return

        lowered = trim(token%text)
        select case (lowered)
        case ("%", "::", ",", "=", "=>", "(")
            is_valid = .true.
        end select
    end function token_precedes_identifier

    logical function token_requires_identifier_after(token) result(requires)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        requires = .false.
        if (token%kind /= TK_KEYWORD) return

        lowered = to_lower(trim(token%text))
        if (lowered == "call") requires = .true.
    end function token_requires_identifier_after

    logical function token_follows_identifier_context(token) result(is_valid)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        is_valid = .false.
        if (token%kind /= TK_OPERATOR) return

        lowered = trim(token%text)
        select case (lowered)
        case ("=", "=>", "(", "%")
            is_valid = .true.
        end select
    end function token_follows_identifier_context

    logical function keyword_can_be_identifier(keyword) result(can_be_id)
        character(len=*), intent(in) :: keyword

        select case (keyword)
        case ("call", "cycle", "exit", "entry", "select", "goto", "go", &
              "common", "dimension", "program", "module", "contains", &
              "stop", "pause", "return", "continue")
            can_be_id = .true.
        case default
            can_be_id = .false.
        end select
    end function keyword_can_be_identifier

    logical function token_is_block_keyword(token) result(is_block)
        type(token_t), intent(in) :: token
        character(len=:), allocatable :: lowered

        is_block = .false.
        if (token%kind /= TK_KEYWORD) return

        lowered = to_lower(trim(token%text))
        select case (lowered)
        case ("type", "module", "subroutine", "function", "program", "interface", &
              "procedure", "select", "if", "do", "forall", "where", "associate", &
              "block", "team", "critical", "blockdata")
            is_block = .true.
        end select
    end function token_is_block_keyword
