module parser_declarations_type_spec_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_NUMBER, &
        TK_KEYWORD, TK_NEWLINE, TK_WHITESPACE, TK_COMMENT, &
        TK_STRING, to_lower
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use string_utils_mod, only: int_to_string
    use parser_declarations_type_spec_support_module, only: type_specifier_t, &
        & append_token, &
        & append_int, &
        & tokens_to_text, &
        & trim_token_sequence, &
        & strip_outer_parentheses, &
        & clear_derived_type_storage, &
        & initialize_type_specifier, &
        & split_derived_type_name_and_params, &
        & set_derived_type_name_info, &
        & process_derived_type_parameters, &
        & analyze_derived_type_tokens
    implicit none
    private

    public :: parse_type_specifier

contains

    subroutine maybe_expand_double_precision(parser, type_spec, base_lower)
        type(parser_state_t), intent(inout) :: parser
        type(type_specifier_t), intent(inout) :: type_spec
        character(len=:), allocatable, intent(inout) :: base_lower
        type(token_t) :: next_token
        character(len=:), allocatable :: next_lower

        if (base_lower /= "double") return
        if (parser%is_at_end()) return

        next_token = parser%peek()
        next_lower = to_lower(trim(next_token%text))

        if (next_lower == "precision") then
            next_token = parser%consume()
            type_spec%type_name = "double precision"
            type_spec%base_keyword = "double precision"
            base_lower = "double precision"
        end if
    end subroutine maybe_expand_double_precision

    subroutine maybe_consume_character_star(parser, type_spec)
        type(parser_state_t), intent(inout) :: parser
        type(type_specifier_t), intent(inout) :: type_spec
        type(token_t) :: token

        if (parser%is_at_end()) return

        token = parser%peek()
        if (token%text /= "*") return

        token = parser%consume()
        if (parser%is_at_end()) return

        token = parser%peek()
        select case (token%text)
        case ("*")
            type_spec%has_kind = .true.
            type_spec%kind_value = -1
            token = parser%consume()
            type_spec%has_character_length = .true.
            if (allocated(type_spec%character_length_expr)) then
                block
                    character(len=:), allocatable :: temp
                    call move_alloc(type_spec%character_length_expr, temp)
                end block
            end if
            type_spec%character_length_expr = "*"
        case default
            if (token%kind == TK_NUMBER) then
                read (token%text, *) type_spec%kind_value
                type_spec%has_kind = .true.
                token = parser%consume()
                type_spec%has_character_length = .true.
                if (allocated(type_spec%character_length_expr)) then
                    block
                        character(len=:), allocatable :: temp
                        call move_alloc(type_spec%character_length_expr, temp)
                    end block
                end if
                type_spec%character_length_expr = trim(adjustl(token%text))
            end if
        end select
    end subroutine maybe_consume_character_star

    subroutine parse_parenthesized_spec(parser, arena, base_lower, type_spec)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: base_lower
        type(type_specifier_t), intent(inout) :: type_spec
        type(token_t) :: token

        token = parser%consume()

        select case (base_lower)
        case ("type", "class")
            call parse_parenthesized_derived(parser, arena, type_spec)
        case ("character")
            call parse_parenthesized_character(parser, type_spec)
        case default
            call capture_parenthesized_content(parser, type_spec)
        end select
    end subroutine parse_parenthesized_spec

    subroutine parse_parenthesized_derived(parser, arena, type_spec)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(type_specifier_t), intent(inout) :: type_spec
        type(token_t), allocatable :: collected_tokens(:)
        character(len=:), allocatable :: derived_text

        call clear_derived_type_storage(type_spec)
        type_spec%is_derived_type = .true.

        call gather_derived_type_tokens(parser, collected_tokens)
        if (allocated(collected_tokens)) then
            call analyze_derived_type_tokens(type_spec, collected_tokens, arena, &
                parser)
            derived_text = tokens_to_text(collected_tokens)
            call move_alloc(collected_tokens, type_spec%derived_type_tokens)
        else
            derived_text = ""
        end if

        if (len_trim(derived_text) > 0) then
            if (type_spec%is_derived_type) then
                if (len_trim(type_spec%derived_type_name) == 0) then
                    type_spec%derived_type_name = trim(adjustl(derived_text))
                end if
            end if
            type_spec%type_name = trim(type_spec%base_keyword)//"("// &
                derived_text//")"
        else
            type_spec%type_name = trim(type_spec%base_keyword)//"()"
        end if
    end subroutine parse_parenthesized_derived

    subroutine gather_derived_type_tokens(parser, tokens)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), allocatable, intent(out) :: tokens(:)
        type(token_t) :: token
        integer :: depth

        depth = 0
        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%text)
            case (")")
                if (depth == 0) then
                    token = parser%consume()
                    exit
                else
                    depth = depth - 1
                    call append_token(tokens, token)
                    token = parser%consume()
                end if
            case ("(")
                depth = depth + 1
                call append_token(tokens, token)
                token = parser%consume()
            case default
                call append_token(tokens, token)
                token = parser%consume()
            end select
        end do
    end subroutine gather_derived_type_tokens

    subroutine parse_parenthesized_character(parser, type_spec)
        type(parser_state_t), intent(inout) :: parser
        type(type_specifier_t), intent(inout) :: type_spec
        type(token_t) :: token
        type(token_t), allocatable :: param_tokens(:)
        type(token_t), allocatable :: cleaned_tokens(:)
        character(len=:), allocatable :: collected_text
        character(len=:), allocatable :: param_text
        integer :: param_start, param_end
        logical :: first_param

        collected_text = ""
        first_param = .true.

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%text == ")") then
                token = parser%consume()
                exit
            end if

            param_start = parser%current_token
            call handle_character_parameter(parser, type_spec)
            param_end = parser%current_token - 1

            if (param_end >= param_start) then
                param_tokens = parser%tokens(param_start:param_end)
                call trim_token_sequence(param_tokens, cleaned_tokens)
                if (allocated(param_tokens)) then
                    block
                        type(token_t), allocatable :: temp(:)
                        call move_alloc(param_tokens, temp)
                    end block
                end if
                if (allocated(cleaned_tokens)) then
                    param_text = trim(adjustl(tokens_to_text(cleaned_tokens)))
                    block
                        type(token_t), allocatable :: temp(:)
                        call move_alloc(cleaned_tokens, temp)
                    end block
                    if (len_trim(param_text) > 0) then
                        if (.not. first_param) collected_text = collected_text//", "
                        collected_text = collected_text//param_text
                        first_param = .false.
                    end if
                end if
            end if

            call consume_comma_if_present(parser)
        end do

        if (len_trim(collected_text) > 0) then
            type_spec%type_name = trim(type_spec%base_keyword)//"("// &
                trim(collected_text)//")"
        else
            type_spec%type_name = trim(type_spec%base_keyword)
        end if
    end subroutine parse_parenthesized_character

    subroutine collect_character_parameter_tokens(parser, tokens)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), allocatable, intent(out) :: tokens(:)
        type(token_t) :: token
        integer :: depth

        depth = 0

        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%text)
            case (")")
                if (depth == 0) exit
            case (",")
                if (depth == 0) exit
            end select

            call append_token(tokens, token)
            token = parser%consume()

            select case (token%text)
            case ("(")
                depth = depth + 1
            case (")")
                if (depth > 0) depth = depth - 1
            end select
        end do
    end subroutine collect_character_parameter_tokens

    subroutine handle_character_parameter(parser, type_spec)
        type(parser_state_t), intent(inout) :: parser
        type(type_specifier_t), intent(inout) :: type_spec
        type(token_t) :: token
        character(len=:), allocatable :: normalized
        type(token_t), allocatable :: value_tokens(:)
        type(token_t), allocatable :: cleaned(:)
        character(len=:), allocatable :: value_text
        integer :: read_stat, numeric_value

        if (parser%is_at_end()) return

        token = parser%peek()
        normalized = to_lower(trim(adjustl(token%text)))

        select case (normalized)
        case ("len")
            token = parser%consume()
            call consume_optional_equals(parser)
            call collect_character_parameter_tokens(parser, value_tokens)
            if (allocated(value_tokens)) then
                call trim_token_sequence(value_tokens, cleaned)
                if (allocated(cleaned)) then
                    value_text = trim(adjustl(tokens_to_text(cleaned)))
                    block
                        type(token_t), allocatable :: temp(:)
                        call move_alloc(cleaned, temp)
                    end block
                else
                    value_text = ""
                end if
                block
                    type(token_t), allocatable :: temp(:)
                    call move_alloc(value_tokens, temp)
                end block
            else
                value_text = ""
            end if

            if (len_trim(value_text) > 0) then
                type_spec%has_character_length = .true.
                if (allocated(type_spec%character_length_expr)) then
                    block
                        character(len=:), allocatable :: temp
                        call move_alloc(type_spec%character_length_expr, temp)
                    end block
                end if
                type_spec%character_length_expr = trim(value_text)

                if (trim(value_text) == "*") then
                    type_spec%has_kind = .true.
                    type_spec%kind_value = -1
                else
                    read_stat = 0
                    read (value_text, *, iostat=read_stat) numeric_value
                    if (read_stat == 0) then
                        type_spec%has_kind = .true.
                        type_spec%kind_value = numeric_value
                    else
                        type_spec%has_kind = .false.
                        type_spec%kind_value = 0
                    end if
                end if
            end if
        case ("kind")
            token = parser%consume()
            call consume_optional_equals(parser)
            if (.not. parser%is_at_end()) then
                token = parser%consume()
            end if
        case default
            if (token%kind == TK_NUMBER) then
                read_stat = 0
                read (token%text, *, iostat=read_stat) numeric_value
                if (read_stat == 0) then
                    type_spec%kind_value = numeric_value
                    type_spec%has_kind = .true.
                    type_spec%character_length_expr = trim(token%text)
                    type_spec%has_character_length = .true.
                end if
                token = parser%consume()
            else if (token%text == "*") then
                type_spec%has_kind = .true.
                type_spec%kind_value = -1
                type_spec%character_length_expr = "*"
                type_spec%has_character_length = .true.
                token = parser%consume()
            else if (token%kind == TK_IDENTIFIER) then
                call collect_character_parameter_tokens(parser, value_tokens)
                if (allocated(value_tokens)) then
                    call trim_token_sequence(value_tokens, cleaned)
                    if (allocated(cleaned)) then
                        value_text = trim(adjustl(tokens_to_text(cleaned)))
                        block
                            type(token_t), allocatable :: temp(:)
                            call move_alloc(cleaned, temp)
                        end block
                    else
                        value_text = ""
                    end if
                    block
                        type(token_t), allocatable :: temp(:)
                        call move_alloc(value_tokens, temp)
                    end block
                else
                    value_text = ""
                end if
                if (len_trim(value_text) > 0) then
                    type_spec%character_length_expr = trim(value_text)
                    type_spec%has_character_length = .true.
                end if
            else
                token = parser%consume()
            end if
        end select
    end subroutine handle_character_parameter

    subroutine finalize_character_type_spec(type_spec)
        type(type_specifier_t), intent(inout) :: type_spec
        character(len=:), allocatable :: length_expr
        character(len=:), allocatable :: lowered_len
        character(len=64) :: buffer

        length_expr = ""

        if (type_spec%has_character_length) then
            if (allocated(type_spec%character_length_expr)) then
                length_expr = trim(type_spec%character_length_expr)
            end if
        else if (type_spec%has_kind) then
            select case (type_spec%kind_value)
            case (-1)
                length_expr = "*"
            case default
                if (type_spec%kind_value > 0) then
                    buffer = int_to_string(type_spec%kind_value)
                    length_expr = trim(buffer)
                end if
            end select
        end if

        if (len_trim(length_expr) == 0) then
            type_spec%type_name = trim(type_spec%base_keyword)
            return
        end if

        lowered_len = to_lower(trim(length_expr))
        if (index(lowered_len, "len=") == 1) then
            type_spec%type_name = trim(type_spec%base_keyword)//"("// &
                trim(length_expr)//")"
        else
            type_spec%type_name = trim(type_spec%base_keyword)//"(len="// &
                trim(length_expr)//")"
        end if
    end subroutine finalize_character_type_spec

    subroutine consume_optional_equals(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        if (parser%is_at_end()) return

        token = parser%peek()
        if (token%text == "=") then
            token = parser%consume()
        end if
    end subroutine consume_optional_equals

    subroutine consume_comma_if_present(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        if (parser%is_at_end()) return

        token = parser%peek()
        if (token%text == ",") then
            token = parser%consume()
        end if
    end subroutine consume_comma_if_present

    subroutine capture_parenthesized_content(parser, type_spec)
        type(parser_state_t), intent(inout) :: parser
        type(type_specifier_t), intent(inout) :: type_spec
        type(token_t) :: token
        type(token_t), allocatable :: collected_tokens(:)
        character(len=:), allocatable :: collected_text
        integer :: depth

        depth = 0
        do while (.not. parser%is_at_end())
            token = parser%peek()
            select case (token%text)
            case (")")
                if (depth == 0) then
                    token = parser%consume()
                    exit
                else
                    depth = depth - 1
                    call append_token(collected_tokens, token)
                    token = parser%consume()
                end if
            case ("(")
                depth = depth + 1
                call append_token(collected_tokens, token)
                token = parser%consume()
            case default
                call append_token(collected_tokens, token)
                token = parser%consume()
                if (depth == 0) call consume_comma_if_present(parser)
            end select
        end do

        if (allocated(collected_tokens)) then
            collected_text = tokens_to_text(collected_tokens)
            type_spec%type_name = trim(type_spec%base_keyword)//"("// &
                trim(adjustl(collected_text))//")"
            block
                type(token_t), allocatable :: temp(:)
                call move_alloc(collected_tokens, temp)
            end block
        else
            type_spec%type_name = trim(type_spec%base_keyword)//"()"
        end if
    end subroutine capture_parenthesized_content

    ! Parse type specifier (e.g., "integer(kind=8)", "character(len=*)")
    function parse_type_specifier(parser, arena) result(type_spec)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(type_specifier_t) :: type_spec

        type(token_t) :: token
        character(len=:), allocatable :: base_lower

        token = parser%consume()
        call initialize_type_specifier(type_spec, token)

        base_lower = to_lower(trim(type_spec%base_keyword))
        call maybe_expand_double_precision(parser, type_spec, base_lower)
        base_lower = to_lower(trim(type_spec%base_keyword))

        if (base_lower == "character") then
            call maybe_consume_character_star(parser, type_spec)
        end if

        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%text == "(") then
                call parse_parenthesized_spec(parser, arena, base_lower, type_spec)
            end if
        end if

        if (base_lower == "character") then
            call finalize_character_type_spec(type_spec)
        end if
    end function parse_type_specifier

end module parser_declarations_type_spec_module
