module parser_interface_block_headers_module
    use string_utils_mod, only: to_lower
    use generic_spec_names, only: normalize_generic_operator
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

    logical function handle_interface_end(parser, first_token, interface_name, &
            interface_kind, operator_symbol) result(is_end)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), intent(in) :: first_token
        character(len=*), intent(in) :: interface_name
        character(len=*), intent(in) :: interface_kind
        character(len=*), intent(in) :: operator_symbol

        type(token_t) :: next_token
        character(len=:), allocatable :: lowered_text
        character(len=:), allocatable :: end_kind
        character(len=:), allocatable :: end_name
        character(len=:), allocatable :: end_symbol
        logical :: has_end_spec

        is_end = .false.
        ! Accept both TK_KEYWORD and TK_IDENTIFIER for end/endinterface
        if (first_token%kind /= TK_KEYWORD .and. &
            first_token%kind /= TK_IDENTIFIER) return

        lowered_text = to_lower(first_token%text)

        ! Check for endinterface as single keyword
        if (trim(lowered_text) == "endinterface") then
            next_token = parser%consume()
            call collect_end_generic_spec(parser, end_kind, end_name, end_symbol, &
                has_end_spec)
            call check_end_generic_spec(parser, interface_name, interface_kind, &
                operator_symbol, end_kind, end_name, end_symbol, has_end_spec)
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

        call collect_end_generic_spec(parser, end_kind, end_name, end_symbol, &
            has_end_spec)
        call check_end_generic_spec(parser, interface_name, interface_kind, &
            operator_symbol, end_kind, end_name, end_symbol, has_end_spec)

        is_end = .true.
    end function handle_interface_end

    ! Read the optional generic-spec of an end-interface-stmt: a generic name,
    ! or OPERATOR/ASSIGNMENT/READ/WRITE followed by a parenthesised designator.
    subroutine collect_end_generic_spec(parser, end_kind, end_name, end_symbol, &
            has_end_spec)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: end_kind
        character(len=:), allocatable, intent(out) :: end_name
        character(len=:), allocatable, intent(out) :: end_symbol
        logical, intent(out) :: has_end_spec

        type(token_t) :: token
        character(len=:), allocatable :: lowered

        end_kind = "interface"
        end_name = ""
        end_symbol = ""
        has_end_spec = .false.

        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER .and. token%kind /= TK_KEYWORD) return

        has_end_spec = .true.
        lowered = to_lower(trim(token%text))

        if (lowered /= "operator" .and. lowered /= "assignment" .and. &
            lowered /= "read" .and. lowered /= "write") then
            end_name = trim(token%text)
            token = parser%consume()
            return
        end if

        end_kind = lowered
        token = parser%consume()
        token = parser%peek()
        if (token%kind /= TK_OPERATOR) return
        if (trim(token%text) /= "(") return

        token = parser%consume()
        token = parser%peek()
        end_symbol = trim(token%text)
        token = parser%consume()
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. trim(token%text) == ")") then
            token = parser%consume()
        end if
    end subroutine collect_end_generic_spec

    ! F2018 R1503/R1504: when an end-interface-stmt carries a generic-spec it
    ! must be the generic-spec of the interface-stmt that opened the block.
    subroutine check_end_generic_spec(parser, interface_name, interface_kind, &
            operator_symbol, end_kind, end_name, end_symbol, has_end_spec)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: interface_name
        character(len=*), intent(in) :: interface_kind
        character(len=*), intent(in) :: operator_symbol
        character(len=*), intent(in) :: end_kind
        character(len=*), intent(in) :: end_name
        character(len=*), intent(in) :: end_symbol
        logical, intent(in) :: has_end_spec

        logical :: block_has_spec
        logical :: matches

        if (.not. has_end_spec) return

        block_has_spec = trim(interface_kind) /= "interface" .or. &
            len_trim(interface_name) > 0
        if (.not. block_has_spec) then
            call parser%error("END INTERFACE must not name a generic spec "// &
                "because the INTERFACE statement has none.")
            return
        end if

        matches = trim(end_kind) == trim(interface_kind)
        if (matches) then
            if (trim(interface_kind) == "interface") then
                matches = to_lower(trim(end_name)) == to_lower(trim(interface_name))
            else
                matches = normalize_generic_operator(end_symbol) == &
                    normalize_generic_operator(operator_symbol)
            end if
        end if

        if (matches) return

        call parser%error("END INTERFACE generic spec does not match the "// &
            "INTERFACE statement; expecting "//expected_end_text(interface_name, &
            interface_kind, operator_symbol)//".")
    end subroutine check_end_generic_spec

    function expected_end_text(interface_name, interface_kind, operator_symbol) &
            result(text)
        character(len=*), intent(in) :: interface_name
        character(len=*), intent(in) :: interface_kind
        character(len=*), intent(in) :: operator_symbol
        character(len=:), allocatable :: text

        if (trim(interface_kind) == "interface") then
            text = "END INTERFACE "//trim(interface_name)
        else
            text = "END INTERFACE "//upper_case(trim(interface_kind))// &
                " ("//trim(operator_symbol)//")"
        end if
    end function expected_end_text

    function upper_case(text) result(upper)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: upper
        integer :: i
        integer :: code

        upper = text
        do i = 1, len(upper)
            code = iachar(upper(i:i))
            if (code >= iachar("a") .and. code <= iachar("z")) then
                upper(i:i) = achar(code - 32)
            end if
        end do
    end function upper_case

end module parser_interface_block_headers_module
