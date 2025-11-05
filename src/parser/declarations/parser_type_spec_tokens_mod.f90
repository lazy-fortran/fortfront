module parser_type_spec_tokens_mod
    use lexer_core, only: token_t, TK_WHITESPACE, TK_NEWLINE, TK_COMMENT
    implicit none
    private

    public :: append_token
    public :: append_int
    public :: tokens_to_text
    public :: trim_token_sequence
    public :: strip_outer_parentheses
    public :: is_trivia_token

contains

    subroutine append_token(tokens, token)
        type(token_t), allocatable, intent(inout) :: tokens(:)
        type(token_t), intent(in) :: token
        type(token_t), allocatable :: temp(:)
        integer :: current_size

        if (.not. allocated(tokens)) then
            allocate (tokens(1))
            tokens(1) = token
        else
            current_size = size(tokens)
            allocate (temp(current_size + 1))
            temp(1:current_size) = tokens
            temp(current_size + 1) = token
            call move_alloc(temp, tokens)
        end if
    end subroutine append_token

    subroutine append_int(values, value)
        integer, allocatable, intent(inout) :: values(:)
        integer, intent(in) :: value
        integer, allocatable :: temp(:)
        integer :: n

        if (.not. allocated(values)) then
            allocate (values(1))
            values(1) = value
        else
            n = size(values)
            allocate (temp(n + 1))
            temp(1:n) = values
            temp(n + 1) = value
            call move_alloc(temp, values)
        end if
    end subroutine append_int

    function tokens_to_text(tokens) result(text)
        type(token_t), allocatable, intent(in) :: tokens(:)
        character(len=:), allocatable :: text
        integer :: i

        if (.not. allocated(tokens)) then
            text = ""
            return
        end if

        text = ""
        do i = 1, size(tokens)
            text = text // tokens(i)%text
        end do
    end function tokens_to_text

    pure logical function is_trivia_token(token) result(is_trivia)
        type(token_t), intent(in) :: token

        select case (token%kind)
        case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
            is_trivia = .true.
        case default
            is_trivia = .false.
        end select
    end function is_trivia_token

    subroutine trim_token_sequence(input_tokens, output_tokens)
        type(token_t), intent(in) :: input_tokens(:)
        type(token_t), allocatable, intent(out) :: output_tokens(:)
        integer :: first_token
        integer :: last_token

        if (size(input_tokens) == 0) then
            return
        end if

        first_token = 1
        last_token = size(input_tokens)

        do while (first_token <= size(input_tokens))
            if (.not. is_trivia_token(input_tokens(first_token))) then
                exit
            end if
            first_token = first_token + 1
        end do

        if (first_token > size(input_tokens)) then
            return
        end if

        do while (last_token >= first_token)
            if (.not. is_trivia_token(input_tokens(last_token))) then
                exit
            end if
            last_token = last_token - 1
        end do

        allocate (output_tokens(last_token - first_token + 1))
        output_tokens = input_tokens(first_token:last_token)
    end subroutine trim_token_sequence

    subroutine strip_outer_parentheses(tokens)
        type(token_t), allocatable, intent(inout) :: tokens(:)
        integer :: depth
        integer :: i

        if (.not. allocated(tokens)) return
        if (size(tokens) < 2) return
        if (tokens(1)%text /= "(") return

        depth = 0
        do i = 1, size(tokens)
            select case (tokens(i)%text)
            case ("(")
                depth = depth + 1
            case (")")
                depth = depth - 1
                if (depth == 0 .and. i < size(tokens)) return
                if (depth == 0) exit
            end select
        end do

        if (depth /= 0) return

        if (size(tokens) == 2) then
            block
                type(token_t), allocatable :: temp(:)
                call move_alloc(tokens, temp)
            end block
        else
            tokens = tokens(2:size(tokens) - 1)
        end if
    end subroutine strip_outer_parentheses

end module parser_type_spec_tokens_mod
