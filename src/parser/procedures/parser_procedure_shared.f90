module parser_procedure_shared_module
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_KEYWORD
    use parser_state_module, only: parser_state_t
    implicit none
    private

    public :: consume_optional_return_type
    public :: consume_optional_kind_spec
    public :: keyword_can_be_function_name

contains

    subroutine consume_optional_return_type(parser, return_type_str)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: return_type_str
        type(token_t) :: token, lookahead
        character(len=:), allocatable :: lowered, next_lower
        integer :: next_index

        return_type_str = ""

        token = parser%peek()
        if (token%kind == TK_KEYWORD) then
            lowered = to_lower(trim(token%text))
            select case (trim(lowered))
            case ("real", "integer", "logical", "character", "complex")
                return_type_str = token%text
                token = parser%consume()
                call consume_optional_kind_spec(parser, return_type_str)
            case ("double precision", "double complex")
                return_type_str = token%text
                token = parser%consume()
            case ("double")
                next_index = parser%current_token + 1
                lookahead = parser%get_token_at_index(next_index)
                next_lower = to_lower(trim(lookahead%text))
                if (trim(next_lower) == "precision" .or. trim(next_lower) == &
                    "complex") then
                    return_type_str = trim(token%text) // " " // trim(lookahead%text)
                    token = parser%consume()
                    token = parser%consume()
                end if
            case ("type", "class")
                ! Handle type(xxx) or class(xxx) as function return type
                next_index = parser%current_token + 1
                lookahead = parser%get_token_at_index(next_index)
                if (lookahead%text == "(") then
                    return_type_str = token%text
                    token = parser%consume()
                    call consume_optional_kind_spec(parser, return_type_str)
                end if
            end select
        end if
    end subroutine consume_optional_return_type

    subroutine consume_optional_kind_spec(parser, type_str)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(inout) :: type_str
        type(token_t) :: token
        character(len=:), allocatable :: kind_spec
        integer :: depth

        if (parser%is_at_end()) return

        token = parser%peek()
        if (token%text /= "(") return

        kind_spec = ""
        depth = 0

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%text)
            case (")")
                depth = depth - 1
                kind_spec = kind_spec // trim(token%text)
                token = parser%consume()
                if (depth == 0) exit
            case ("(")
                depth = depth + 1
                kind_spec = kind_spec // trim(token%text)
                token = parser%consume()
            case default
                kind_spec = kind_spec // trim(token%text)
                token = parser%consume()
            end select
        end do

        if (len_trim(kind_spec) > 0) then
            type_str = trim(type_str) // trim(kind_spec)
        end if
    end subroutine consume_optional_kind_spec

    logical function keyword_can_be_function_name(parser, token) result(can_use)
        type(parser_state_t), intent(in) :: parser
        type(token_t), intent(in) :: token
        type(token_t) :: lookahead
        character(len=len(token%text)) :: token_lower
        character(len=:), allocatable :: next_lower
        integer :: next_index

        token_lower = to_lower(token%text)
        can_use = .false.

        select case (trim(token_lower))
        case ("double")
            next_index = parser%current_token + 1
            lookahead = parser%get_token_at_index(next_index)
            next_lower = to_lower(trim(lookahead%text))
            if (next_lower /= "precision") then
                can_use = .true.
            end if
        case default
            can_use = .false.
        end select
    end function keyword_can_be_function_name

end module parser_procedure_shared_module
