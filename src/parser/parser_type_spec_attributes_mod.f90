module parser_type_spec_attributes_mod
    use lexer_core, only: token_t, TK_OPERATOR, TK_WHITESPACE, TK_NEWLINE
    use lexer_core, only: TK_COMMENT, TK_KEYWORD, TK_IDENTIFIER, TK_NUMBER
    use lexer_core, only: TK_STRING, to_lower
    use parser_state_module, only: parser_state_t
    use parser_type_spec_tokens_mod, only: append_token, trim_token_sequence, &
                                           tokens_to_text
    implicit none
    private

    public :: is_type_attribute_token
    public :: skip_type_definition_attributes
    public :: parser_is_at_type_definition

contains

    logical function is_type_attribute_token(text) result(is_attribute)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: normalized

        normalized = to_lower(trim(adjustl(text)))

        select case (normalized)
        case ("public", "private", "sequence", "abstract", "extends", "bind", &
              "protected", "non_overridable", "final", "deferred")
            is_attribute = .true.
        case default
            is_attribute = .false.
        end select
    end function is_type_attribute_token

    subroutine consume_parenthesized_content(parser, attr_tokens, &
                                             invalid_definition)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), allocatable, intent(inout) :: attr_tokens(:)
        logical, intent(inout) :: invalid_definition
        type(token_t) :: token
        integer :: depth

        token = parser%consume()
        call append_token(attr_tokens, token)
        depth = 1
        do while (.not. parser%is_at_end() .and. depth > 0)
            token = parser%consume()
            call append_token(attr_tokens, token)
            if (token%kind == TK_OPERATOR) then
                select case (token%text)
                case ("(")
                    depth = depth + 1
                case (")")
                    depth = depth - 1
                end select
            end if
        end do
        if (depth /= 0) invalid_definition = .true.
    end subroutine consume_parenthesized_content

    subroutine format_attribute_clause(attr_tokens, attribute_clause)
        type(token_t), allocatable, intent(in) :: attr_tokens(:)
        character(len=:), allocatable, intent(out) :: attribute_clause
        type(token_t), allocatable :: cleaned(:)
        character(len=:), allocatable :: clause_text
        character(len=:), allocatable :: sanitized
        integer :: i

        call trim_token_sequence(attr_tokens, cleaned)
        if (.not. allocated(cleaned)) return

        clause_text = trim(adjustl(tokens_to_text(cleaned)))
        if (len(clause_text) == 0) then
            block
                type(token_t), allocatable :: temp(:)
                call move_alloc(cleaned, temp)
            end block
            return
        end if

        sanitized = ""
        do i = 1, len(clause_text)
            sanitized = sanitized // clause_text(i:i)
            if (clause_text(i:i) == "," .and. i < len(clause_text)) then
                if (clause_text(i + 1:i + 1) /= " " .and. &
                    clause_text(i + 1:i + 1) /= new_line('A')) then
                    sanitized = sanitized // " "
                end if
            end if
        end do
        attribute_clause = trim(adjustl(sanitized))

        block
            character(len=:), allocatable :: temp1
            type(token_t), allocatable :: temp2(:)
            call move_alloc(sanitized, temp1)
            call move_alloc(clause_text, temp1)
            call move_alloc(cleaned, temp2)
        end block
    end subroutine format_attribute_clause

    subroutine skip_type_definition_attributes(parser, invalid_definition, &
                                               attribute_clause)
        type(parser_state_t), intent(inout) :: parser
        logical, intent(out) :: invalid_definition
        character(len=:), allocatable, intent(out), optional :: attribute_clause
        type(token_t) :: token
        type(token_t), allocatable :: attr_tokens(:)
        logical :: found_double_colon

        invalid_definition = .false.
        found_double_colon = .false.
        if (present(attribute_clause)) then
            if (allocated(attribute_clause)) then
                block
                    character(len=:), allocatable :: temp
                    call move_alloc(attribute_clause, temp)
                end block
            end if
        end if

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                token = parser%consume()
                call append_token(attr_tokens, token)
            case (TK_OPERATOR)
                select case (token%text)
                case (",")
                    token = parser%consume()
                    call append_token(attr_tokens, token)
                case ("::")
                    token = parser%consume()
                    found_double_colon = .true.
                    exit
                case ("(")
                    call consume_parenthesized_content(parser, attr_tokens, &
                                                       invalid_definition)
                    if (invalid_definition) exit
                case default
                    token = parser%consume()
                    call append_token(attr_tokens, token)
                end select
            case (TK_KEYWORD, TK_IDENTIFIER, TK_NUMBER, TK_STRING)
                token = parser%consume()
                call append_token(attr_tokens, token)
            case default
                invalid_definition = .true.
                exit
            end select
        end do

        if (.not. found_double_colon) invalid_definition = .true.

        if (present(attribute_clause) .and. .not. invalid_definition) then
            if (allocated(attr_tokens)) then
                call format_attribute_clause(attr_tokens, attribute_clause)
            end if
        end if

        if (allocated(attr_tokens)) then
            block
                type(token_t), allocatable :: temp(:)
                call move_alloc(attr_tokens, temp)
            end block
        end if
    end subroutine skip_type_definition_attributes

    logical function parser_is_at_type_definition(parser) result(is_type_def)
        type(parser_state_t), intent(in) :: parser
        integer :: pos
        integer :: depth
        integer :: token_count
        type(token_t) :: token
        character(len=:), allocatable :: last_attribute
        character(len=:), allocatable :: normalized_attribute

        is_type_def = .false.
        if (.not. associated(parser%tokens)) then
            return
        end if

        if (parser%current_token < 1 .or. parser%current_token > &
            size(parser%tokens)) then
            return
        end if

        token = parser%tokens(parser%current_token)
        if (trim(to_lower(token%text)) /= "type") then
            return
        end if

        if (parser%current_token > 1) then
            token = parser%tokens(parser%current_token - 1)
            if (token%kind == TK_KEYWORD) then
                if (trim(to_lower(token%text)) == "end") then
                    return
                end if
            end if
        end if

        token_count = size(parser%tokens)
        pos = parser%current_token + 1
        last_attribute = ""

        do while (pos <= token_count)
            token = parser%tokens(pos)
            select case (token%kind)
            case (TK_WHITESPACE, TK_NEWLINE, TK_COMMENT)
                pos = pos + 1
            case (TK_OPERATOR)
                select case (token%text)
                case (",")
                    last_attribute = ""
                    pos = pos + 1
                case ("::")
                    is_type_def = .true.
                    return
                case ("(")
                    if (len_trim(last_attribute) > 0) then
                        normalized_attribute = to_lower(trim(adjustl(last_attribute)))
                        select case (normalized_attribute)
                        case ("extends", "bind")
                            depth = 1
                            pos = pos + 1
                            do while (pos <= token_count .and. depth > 0)
                                token = parser%tokens(pos)
                                if (token%kind == TK_OPERATOR) then
                                    select case (token%text)
                                    case ("(")
                                        depth = depth + 1
                                    case (")")
                                        depth = depth - 1
                                    end select
                                end if
                                pos = pos + 1
                            end do
                            last_attribute = ""
                        case default
                            return
                        end select
                    else
                        return
                    end if
                case default
                    return
                end select
            case (TK_KEYWORD, TK_IDENTIFIER)
                if (is_type_attribute_token(token%text)) then
                    last_attribute = trim(adjustl(token%text))
                    pos = pos + 1
                else
                    is_type_def = .true.
                    return
                end if
            case default
                return
            end select
        end do
    end function parser_is_at_type_definition

end module parser_type_spec_attributes_mod
