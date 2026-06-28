module parser_namelist_shared_module
    use lexer_core, only: token_t, TK_OPERATOR, TK_IDENTIFIER, TK_NEWLINE, &
        TK_COMMENT
    use parser_state_module, only: parser_state_t
    implicit none
    private

    public :: consume_namelist_group
    public :: append_name

contains

    logical function consume_namelist_group(parser, group_name, names) &
            result(success)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: group_name
        character(len=:), allocatable, intent(out) :: names(:)
        type(token_t) :: token

        success = .false.

        if (parser%is_at_end()) return
        token = parser%peek()
        if (.not. (token%kind == TK_OPERATOR .and. token%text == "/")) return
        token = parser%consume()

        if (parser%is_at_end()) return
        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER) return
        group_name = trim(token%text)
        token = parser%consume()

        if (parser%is_at_end()) return
        token = parser%peek()
        if (.not. (token%kind == TK_OPERATOR .and. token%text == "/")) return
        token = parser%consume()

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_IDENTIFIER)
                call append_name(names, token%text)
                token = parser%consume()
            case (TK_OPERATOR)
                if (token%text == ",") then
                    token = parser%consume()
                    cycle
                else
                    exit
                end if
            case (TK_NEWLINE)
                token = parser%consume()
                exit
            case (TK_COMMENT)
                exit
            case default
                exit
            end select
        end do

        success = allocated(group_name)
    end function consume_namelist_group

    subroutine append_name(list, value)
        character(len=:), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: temp(:)
        integer :: n
        integer :: current_len
        integer :: target_len

        if (.not. allocated(list)) then
            allocate (character(len=len_trim(value)) :: list(1))
            list(1) = trim(value)
        else
            n = size(list)
            current_len = len(list)
            target_len = len_trim(value)
            target_len = max(current_len, target_len)
            allocate (character(len=target_len) :: temp(n + 1))
            temp(1:n) = list
            temp(n + 1) = trim(value)
            call move_alloc(temp, list)
        end if
    end subroutine append_name

end module parser_namelist_shared_module
