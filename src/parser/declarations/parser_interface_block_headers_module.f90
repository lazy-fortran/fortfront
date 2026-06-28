module parser_interface_block_headers_module
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_OPERATOR
    use parser_state_module, only: parser_state_t
    implicit none
    private
    public :: begin_interface_block
    public :: handle_interface_end
contains

    subroutine begin_interface_block(parser, interface_name, interface_kind, &
            operator_symbol, line, column, is_abstract)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: interface_name
        character(len=:), allocatable, intent(out) :: interface_kind
        character(len=:), allocatable, intent(out) :: operator_symbol
        integer, intent(out) :: line, column
        logical, intent(in), optional :: is_abstract

        type(token_t) :: token
        character(len=:), allocatable :: lowered

        token = parser%consume()
        line = token%line
        column = token%column

        interface_kind = "interface"
        operator_symbol = ""

        token = parser%peek()
        if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
            lowered = to_lower(trim(token%text))
            if (trim(lowered) == "operator" .or. trim(lowered) == "assignment") then
                interface_kind = trim(lowered)
                token = parser%consume()
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. trim(token%text) == "(") then
                    token = parser%consume()
                    token = parser%peek()
                    operator_symbol = trim(token%text)
                    token = parser%consume()
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. trim(token%text) == ")") then
                        token = parser%consume()
                    end if
                end if
                interface_name = ""
            else if (trim(lowered) == "read" .or. trim(lowered) == "write") then
                interface_kind = trim(lowered)
                token = parser%consume()
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. trim(token%text) == "(") then
                    token = parser%consume()
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
                        operator_symbol = trim(to_lower(token%text))
                        token = parser%consume()
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. trim(token%text) &
                            == ")") then
                            token = parser%consume()
                        end if
                    end if
                end if
                interface_name = ""
            else
                token = parser%consume()
                interface_name = token%text
            end if
        else
            interface_name = ""
        end if
    end subroutine begin_interface_block

    logical function handle_interface_end(parser, first_token) result(is_end)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), intent(in) :: first_token

        type(token_t) :: next_token
        character(len=:), allocatable :: lowered_text

        is_end = .false.
        ! Accept both TK_KEYWORD and TK_IDENTIFIER for end/endinterface
        if (first_token%kind /= TK_KEYWORD .and. &
            first_token%kind /= TK_IDENTIFIER) return

        lowered_text = to_lower(first_token%text)

        ! Check for endinterface as single keyword
        if (trim(lowered_text) == "endinterface") then
            next_token = parser%consume()
            ! Optionally consume interface name
            next_token = parser%peek()
            if (next_token%kind == TK_IDENTIFIER .or. &
                next_token%kind == TK_KEYWORD) then
                next_token = parser%consume()
            end if
            is_end = .true.
            return
        end if

        ! Check for end interface as two tokens
        if (trim(lowered_text) /= "end") return

        next_token = parser%get_token_at_index(parser%current_token + 1)
        ! Accept both TK_KEYWORD and TK_IDENTIFIER for interface
        if (next_token%kind /= TK_KEYWORD .and. &
            next_token%kind /= TK_IDENTIFIER) return

        lowered_text = to_lower(next_token%text)
        if (trim(lowered_text) /= "interface") return

        next_token = parser%consume()
        next_token = parser%consume()

        next_token = parser%peek()
        if (next_token%kind == TK_IDENTIFIER .or. &
            next_token%kind == TK_KEYWORD) then
            next_token = parser%consume()
        end if

        is_end = .true.
    end function handle_interface_end

end module parser_interface_block_headers_module
