module parser_procedure_shared_module
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_KEYWORD
    use parser_state_module, only: parser_state_t
    implicit none
    private

    public :: consume_optional_return_type
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
            case ("double precision", "double complex")
                return_type_str = token%text
                token = parser%consume()
            case ("double")
                next_index = parser%current_token + 1
                lookahead = parser%get_token_at_index(next_index)
                next_lower = to_lower(trim(lookahead%text))
                if (trim(next_lower) == "precision" .or. trim(next_lower) == "complex") then
                    return_type_str = trim(token%text)//" "//trim(lookahead%text)
                    token = parser%consume()
                    token = parser%consume()
                else
                    return_type_str = token%text
                    token = parser%consume()
                end if
            end select
        end if
    end subroutine consume_optional_return_type

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
