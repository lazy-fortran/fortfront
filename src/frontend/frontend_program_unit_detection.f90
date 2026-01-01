module frontend_program_unit_detection
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, to_lower
    implicit none
    private

    public :: detect_explicit_program_unit
    public :: tokens_have_explicit_program_unit

contains

    subroutine detect_explicit_program_unit(tokens, has_unit, unit_name)
        type(token_t), intent(in) :: tokens(:)
        logical, intent(out) :: has_unit
        character(len=:), allocatable, intent(out), optional :: unit_name
        integer :: i
        integer :: next_idx
        character(len=:), allocatable :: lowered

        has_unit = .false.

        do i = 1, size(tokens)
            if (tokens(i)%kind /= TK_KEYWORD .and. &
                tokens(i)%kind /= TK_IDENTIFIER) cycle
            if (.not. allocated(tokens(i)%text)) cycle

            lowered = to_lower(trim(tokens(i)%text))

            select case (lowered)
            case ("program")
                has_unit = .true.
                if (present(unit_name)) then
                    next_idx = find_next_identifier_token(tokens, i)
                    if (next_idx > 0) then
                        if (allocated(tokens(next_idx)%text)) then
                            unit_name = trim(tokens(next_idx)%text)
                        end if
                    end if
                end if
                return
            case ("module")
                next_idx = find_next_identifier_token(tokens, i)
                if (next_idx > 0) then
                    if (.not. allocated(tokens(next_idx)%text)) cycle
                    if (to_lower(trim(tokens(next_idx)%text)) == "procedure") cycle
                    has_unit = .true.
                    if (present(unit_name)) unit_name = trim(tokens(next_idx)%text)
                    return
                end if
                has_unit = .true.
                return
            case ("subroutine", "function")
                has_unit = .true.
                if (present(unit_name)) then
                    next_idx = find_next_identifier_token(tokens, i)
                    if (next_idx > 0) then
                        if (allocated(tokens(next_idx)%text)) then
                            unit_name = trim(tokens(next_idx)%text)
                        end if
                    end if
                end if
                return
            case ("submodule")
                has_unit = .true.
                return
            case ("block")
                next_idx = find_next_identifier_token(tokens, i)
                if (next_idx > 0) then
                    if (.not. allocated(tokens(next_idx)%text)) cycle
                    if (to_lower(trim(tokens(next_idx)%text)) == "data") then
                        has_unit = .true.
                        return
                    end if
                end if
            end select
        end do
    end subroutine detect_explicit_program_unit

    logical function tokens_have_explicit_program_unit(tokens) result(has_unit)
        type(token_t), intent(in) :: tokens(:)

        call detect_explicit_program_unit(tokens, has_unit)
    end function tokens_have_explicit_program_unit

    integer function find_next_identifier_token(tokens, start_pos) result(idx)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer :: i

        idx = 0
        do i = start_pos + 1, size(tokens)
            select case (tokens(i)%kind)
            case (TK_IDENTIFIER, TK_KEYWORD)
                idx = i
                return
            case default
                cycle
            end select
        end do
    end function find_next_identifier_token

end module frontend_program_unit_detection

