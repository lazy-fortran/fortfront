module parser_declarations_type_spec_support_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER, TK_EOF, &
                          TK_KEYWORD, TK_NEWLINE, TK_WHITESPACE, TK_COMMENT, &
                          TK_STRING, to_lower
    use parser_state_module, only: parser_state_t, create_parser_state
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_identifier
    use parser_expressions_module, only: parse_comparison
    implicit none
    private

    public :: type_specifier_t
    public :: append_token
    public :: append_int
    public :: tokens_to_text
    public :: trim_token_sequence
    public :: strip_outer_parentheses
    public :: clear_derived_type_storage
    public :: initialize_type_specifier
    public :: split_derived_type_name_and_params
    public :: set_derived_type_name_info
    public :: process_derived_type_parameters
    public :: analyze_derived_type_tokens
    public :: is_type_attribute_token
    public :: parser_is_at_type_definition
    public :: skip_type_definition_attributes

    ! Type specifier result type for structured type information
    type :: type_specifier_t
        character(len=:), allocatable :: type_name
        character(len=:), allocatable :: base_keyword
        character(len=:), allocatable :: derived_type_name
        character(len=:), allocatable :: derived_type_module
        type(token_t), allocatable :: derived_type_tokens(:)
        type(token_t), allocatable :: derived_parameter_tokens(:)
        integer, allocatable :: derived_parameter_nodes(:)
        integer :: derived_type_identifier = 0
        logical :: is_derived_type = .false.
        logical :: has_derived_type_parameters = .false.
        logical :: has_kind = .false.
        integer :: kind_value = 0
        integer :: line = 0
        integer :: column = 0
        logical :: has_character_length = .false.
        character(len=:), allocatable :: character_length_expr
    end type type_specifier_t

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
        do while (first_token <= size(input_tokens) .and. &
                  is_trivia_token(input_tokens(first_token)))
            first_token = first_token + 1
        end do

        if (first_token > size(input_tokens)) return

        last_token = size(input_tokens)
        do while (last_token >= first_token .and. &
                  is_trivia_token(input_tokens(last_token)))
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

    subroutine clear_derived_type_storage(type_spec)
        type(type_specifier_t), intent(inout) :: type_spec

        if (allocated(type_spec%derived_type_tokens)) then
            block
                type(token_t), allocatable :: temp(:)
                call move_alloc(type_spec%derived_type_tokens, temp)
            end block
        end if
        if (allocated(type_spec%derived_parameter_tokens)) then
            block
                type(token_t), allocatable :: temp(:)
                call move_alloc(type_spec%derived_parameter_tokens, temp)
            end block
        end if
        if (allocated(type_spec%derived_parameter_nodes)) then
            block
                integer, allocatable :: temp(:)
                call move_alloc(type_spec%derived_parameter_nodes, temp)
            end block
        end if
        type_spec%derived_type_name = ""
        type_spec%derived_type_module = ""
        type_spec%has_derived_type_parameters = .false.
        type_spec%derived_type_identifier = 0
    end subroutine clear_derived_type_storage

    subroutine initialize_type_specifier(type_spec, token)
        type(type_specifier_t), intent(inout) :: type_spec
        type(token_t), intent(in) :: token

        call clear_derived_type_storage(type_spec)
        type_spec%type_name = trim(token%text)
        type_spec%base_keyword = trim(token%text)
        type_spec%derived_type_name = ""
        type_spec%derived_type_module = ""
        type_spec%derived_type_identifier = 0
        type_spec%is_derived_type = .false.
        type_spec%has_derived_type_parameters = .false.
        type_spec%has_kind = .false.
        type_spec%kind_value = 0
        type_spec%line = token%line
        type_spec%column = token%column
        type_spec%has_character_length = .false.
        if (allocated(type_spec%character_length_expr)) then
            block
                character(len=:), allocatable :: temp
                call move_alloc(type_spec%character_length_expr, temp)
            end block
        end if
    end subroutine initialize_type_specifier

    subroutine split_derived_type_name_and_params(tokens, name_tokens, param_tokens)
        type(token_t), intent(in) :: tokens(:)
        type(token_t), allocatable, intent(out) :: name_tokens(:)
        type(token_t), allocatable, intent(out) :: param_tokens(:)
        integer :: i
        logical :: name_complete

        name_complete = .false.
        do i = 1, size(tokens)
            if (.not. name_complete) then
                if (is_trivia_token(tokens(i))) cycle
                if (tokens(i)%text == "(" .or. tokens(i)%text == ",") then
                    name_complete = .true.
                    call append_token(param_tokens, tokens(i))
                else
                    call append_token(name_tokens, tokens(i))
                end if
            else
                call append_token(param_tokens, tokens(i))
            end if
        end do
    end subroutine split_derived_type_name_and_params

    subroutine set_derived_type_name_info(type_spec, name_tokens, arena)
        type(type_specifier_t), intent(inout) :: type_spec
        type(token_t), allocatable, intent(in) :: name_tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        character(len=:), allocatable :: module_name
        character(len=:), allocatable :: base_name
        character(len=:), allocatable :: name_text
        integer :: line_ref
        integer :: column_ref
        integer :: last_sep
        integer :: i
        logical :: found_identifier

        module_name = ""
        base_name = ""
        line_ref = type_spec%line
        column_ref = type_spec%column
        found_identifier = .false.

        if (.not. allocated(name_tokens)) then
            type_spec%is_derived_type = .false.
            type_spec%derived_type_name = ""
            type_spec%derived_type_module = ""
            return
        end if

        name_text = ""
        do i = 1, size(name_tokens)
            if (.not. is_trivia_token(name_tokens(i))) then
                name_text = name_text // trim(name_tokens(i)%text)
            end if
        end do

        if (len(name_text) == 0) then
            type_spec%derived_type_name = ""
            type_spec%derived_type_module = ""
            return
        end if

        last_sep = 0
        do i = 1, len(name_text) - 1
            if (name_text(i:i + 1) == "::") last_sep = i
        end do

        if (last_sep > 0) then
            module_name = trim(adjustl(name_text(:last_sep - 1)))
            base_name = trim(adjustl(name_text(last_sep + 2:)))
        else
            module_name = ""
            base_name = trim(adjustl(name_text))
        end if

        do i = size(name_tokens), 1, -1
            if (name_tokens(i)%kind == TK_IDENTIFIER) then
                line_ref = name_tokens(i)%line
                column_ref = name_tokens(i)%column
                found_identifier = .true.
                exit
            end if
        end do

        if (len_trim(base_name) > 0) then
            if (trim(adjustl(base_name)) == "*") then
                type_spec%derived_type_name = ""
                type_spec%derived_type_module = ""
                type_spec%derived_type_identifier = 0
                type_spec%is_derived_type = .false.
                return
            end if

            type_spec%derived_type_name = base_name
            if (len_trim(module_name) > 0) then
                type_spec%derived_type_module = module_name
            end if

            if (found_identifier) then
                type_spec%derived_type_identifier = push_identifier( &
                    arena, base_name, line=line_ref, column=column_ref)
            else
                type_spec%derived_type_identifier = 0
            end if
        else
            type_spec%derived_type_name = ""
            type_spec%derived_type_module = ""
            type_spec%derived_type_identifier = 0
        end if
    end subroutine set_derived_type_name_info

    subroutine process_derived_type_parameters(type_spec, param_tokens, arena)
        type(type_specifier_t), intent(inout) :: type_spec
        type(token_t), allocatable, intent(in) :: param_tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), allocatable :: working(:)
        type(token_t), allocatable :: trimmed_working(:)
        type(token_t), allocatable :: current(:)
        type(token_t), allocatable :: cleaned(:)
        type(token_t), allocatable :: parser_tokens(:)
        type(token_t) :: eof_token
        type(parser_state_t) :: param_parser
        integer :: depth
        integer :: i
        integer :: expr_index

        if (.not. allocated(param_tokens)) return

        call trim_token_sequence(param_tokens, working)
        if (.not. allocated(working)) return

        call strip_outer_parentheses(working)
        call trim_token_sequence(working, trimmed_working)
        if (.not. allocated(trimmed_working)) return
        call move_alloc(trimmed_working, working)

        if (allocated(type_spec%derived_parameter_nodes)) then
            block
                integer, allocatable :: temp(:)
                call move_alloc(type_spec%derived_parameter_nodes, temp)
            end block
        end if

        depth = 0
        do i = 1, size(working)
            if (working(i)%kind == TK_OPERATOR) then
                select case (working(i)%text)
                case ("(")
                    depth = depth + 1
                case (")")
                    if (depth > 0) depth = depth - 1
                case (",")
                    if (depth == 0) then
                        call finalize_parameter()
                        cycle
                    end if
                end select
            end if
            call append_token(current, working(i))
        end do
        call finalize_parameter()

        if (allocated(type_spec%derived_parameter_nodes)) then
            type_spec%has_derived_type_parameters = &
                (size(type_spec%derived_parameter_nodes) > 0)
        else
            type_spec%has_derived_type_parameters = .false.
        end if

    contains

        subroutine finalize_parameter()
            if (.not. allocated(current)) return
            call trim_token_sequence(current, cleaned)
            if (.not. allocated(cleaned)) then
                call reset_current()
                return
            end if

            allocate (parser_tokens(size(cleaned) + 1))
            parser_tokens(1:size(cleaned)) = cleaned
            eof_token%kind = TK_EOF
            eof_token%text = ""
            parser_tokens(size(cleaned) + 1) = eof_token

            param_parser = create_parser_state(parser_tokens)
            expr_index = parse_comparison(param_parser, arena)
            if (expr_index > 0) then
                call append_int(type_spec%derived_parameter_nodes, expr_index)
            end if

            if (allocated(parser_tokens)) then
                block
                    type(token_t), allocatable :: temp(:)
                    call move_alloc(parser_tokens, temp)
                end block
            end if
            if (allocated(cleaned)) then
                block
                    type(token_t), allocatable :: temp(:)
                    call move_alloc(cleaned, temp)
                end block
            end if
            call reset_current()
        end subroutine finalize_parameter

        subroutine reset_current()
            if (allocated(current)) then
                block
                    type(token_t), allocatable :: temp(:)
                    call move_alloc(current, temp)
                end block
            end if
        end subroutine reset_current

    end subroutine process_derived_type_parameters

    subroutine analyze_derived_type_tokens(type_spec, tokens, arena)
        type(type_specifier_t), intent(inout) :: type_spec
        type(token_t), allocatable, intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), allocatable :: name_tokens(:)
        type(token_t), allocatable :: param_tokens(:)

        call split_derived_type_name_and_params(tokens, name_tokens, param_tokens)
        call set_derived_type_name_info(type_spec, name_tokens, arena)
        if (.not. type_spec%is_derived_type) then
            if (allocated(type_spec%derived_parameter_tokens)) then
                block
                    type(token_t), allocatable :: temp(:)
                    call move_alloc(type_spec%derived_parameter_tokens, temp)
                end block
            end if
            if (allocated(param_tokens)) then
                block
                    type(token_t), allocatable :: temp(:)
                    call move_alloc(param_tokens, temp)
                end block
            end if
            return
        end if
        call process_derived_type_parameters(type_spec, param_tokens, arena)

        if (allocated(type_spec%derived_parameter_tokens)) then
            block
                type(token_t), allocatable :: temp(:)
                call move_alloc(type_spec%derived_parameter_tokens, temp)
            end block
        end if
        if (allocated(param_tokens)) then
            allocate (type_spec%derived_parameter_tokens(size(param_tokens)))
            type_spec%derived_parameter_tokens = param_tokens
        end if
    end subroutine analyze_derived_type_tokens

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

    subroutine skip_type_definition_attributes(parser, invalid_definition, &
                                               attribute_clause)
        type(parser_state_t), intent(inout) :: parser
        logical, intent(out) :: invalid_definition
        character(len=:), allocatable, intent(out), optional :: attribute_clause

        type(token_t) :: token
        type(token_t), allocatable :: attr_tokens(:)
        type(token_t), allocatable :: cleaned(:)
        character(len=:), allocatable :: clause_text
        character(len=:), allocatable :: sanitized
        integer :: depth
        integer :: i
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
                    if (depth /= 0) then
                        invalid_definition = .true.
                        exit
                    end if
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

        if (.not. found_double_colon) then
            invalid_definition = .true.
        end if

        if (present(attribute_clause)) then
            if (.not. invalid_definition .and. allocated(attr_tokens)) then
                call trim_token_sequence(attr_tokens, cleaned)
                if (allocated(cleaned)) then
                    clause_text = trim(adjustl(tokens_to_text(cleaned)))
                    if (len(clause_text) > 0) then
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
                    end if
                    if (allocated(sanitized)) then
                        block
                            character(len=:), allocatable :: temp
                            call move_alloc(sanitized, temp)
                        end block
                    end if
                    if (allocated(clause_text)) then
                        block
                            character(len=:), allocatable :: temp
                            call move_alloc(clause_text, temp)
                        end block
                    end if
                    block
                        type(token_t), allocatable :: temp(:)
                        call move_alloc(cleaned, temp)
                    end block
                end if
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

end module parser_declarations_type_spec_support_module
