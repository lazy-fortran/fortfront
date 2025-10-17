module parser_declarations
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER, TK_EOF, TK_KEYWORD, &
                          TK_NEWLINE, TK_WHITESPACE, TK_COMMENT, TK_STRING, to_lower
    use parser_state_module, only: parser_state_t, create_parser_state
    use ast_arena_modern, only: ast_arena_t
    use ast_types, only: LITERAL_STRING
    use ast_nodes_data, only: INTENT_IN, INTENT_OUT, INTENT_INOUT
    use parser_expressions_module, only: parse_comparison, parse_range
   use parser_result_types, only: parse_result_t, success_parse_result, error_parse_result
    use error_handling, only: ERROR_PARSER
    use ast_factory, only: push_multi_declaration, push_declaration, push_identifier
    use parser_type_hooks_module, only: register_type_annotation
    implicit none
    private

    public :: parse_declaration, parse_multi_declaration, parse_declaration_with_result
    public :: parse_type_specifier
    public :: parse_derived_type_def, parse_derived_type_component
    public :: parse_array_dimensions
    public :: is_type_attribute_token, parser_is_at_type_definition

    ! Type specifier result type for structured type information
    type, public :: type_specifier_t
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
    end type type_specifier_t

    ! Declaration attributes result type for structured attribute information
    type, public :: declaration_attributes_t
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_target = .false.
        logical :: is_parameter = .false.
        logical :: is_external = .false.
        logical :: is_optional = .false.
        logical :: has_intent = .false.
        logical :: has_global_dimensions = .false.
        character(len=:), allocatable :: intent
        integer, allocatable :: global_dimension_indices(:)
    end type declaration_attributes_t

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
            if (allocated(output_tokens)) deallocate (output_tokens)
            return
        end if

        first_token = 1
        do while (first_token <= size(input_tokens) .and. is_trivia_token(input_tokens(first_token)))
            first_token = first_token + 1
        end do

        if (first_token > size(input_tokens)) then
            if (allocated(output_tokens)) deallocate (output_tokens)
            return
        end if

        last_token = size(input_tokens)
      do while (last_token >= first_token .and. is_trivia_token(input_tokens(last_token)))
            last_token = last_token - 1
        end do

        if (allocated(output_tokens)) deallocate (output_tokens)
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
            deallocate (tokens)
        else
            tokens = tokens(2:size(tokens) - 1)
        end if
    end subroutine strip_outer_parentheses

    subroutine clear_derived_type_storage(type_spec)
        type(type_specifier_t), intent(inout) :: type_spec

        if (allocated(type_spec%derived_type_tokens)) then
            deallocate (type_spec%derived_type_tokens)
        end if
        if (allocated(type_spec%derived_parameter_tokens)) then
            deallocate (type_spec%derived_parameter_tokens)
        end if
        if (allocated(type_spec%derived_parameter_nodes)) then
            deallocate (type_spec%derived_parameter_nodes)
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
    end subroutine initialize_type_specifier

    subroutine split_derived_type_name_and_params(tokens, name_tokens, param_tokens)
        type(token_t), intent(in) :: tokens(:)
        type(token_t), allocatable, intent(out) :: name_tokens(:)
        type(token_t), allocatable, intent(out) :: param_tokens(:)
        integer :: i
        logical :: name_complete

        if (allocated(name_tokens)) deallocate (name_tokens)
        if (allocated(param_tokens)) deallocate (param_tokens)

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
        if (allocated(working)) deallocate (working)
        if (.not. allocated(trimmed_working)) return
        working = trimmed_working
        if (allocated(trimmed_working)) deallocate (trimmed_working)

        if (allocated(type_spec%derived_parameter_nodes)) then
            deallocate (type_spec%derived_parameter_nodes)
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

            if (allocated(parser_tokens)) deallocate (parser_tokens)
            if (allocated(cleaned)) deallocate (cleaned)
            call reset_current()
        end subroutine finalize_parameter

        subroutine reset_current()
            if (allocated(current)) deallocate (current)
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
                deallocate (type_spec%derived_parameter_tokens)
            end if
            if (allocated(param_tokens)) deallocate (param_tokens)
            return
        end if
        call process_derived_type_parameters(type_spec, param_tokens, arena)

        if (allocated(type_spec%derived_parameter_tokens)) then
            deallocate (type_spec%derived_parameter_tokens)
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

  subroutine skip_type_definition_attributes(parser, invalid_definition, attribute_clause)
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
            if (allocated(attribute_clause)) deallocate (attribute_clause)
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
                    if (allocated(sanitized)) deallocate (sanitized)
                    if (allocated(clause_text)) deallocate (clause_text)
                    deallocate (cleaned)
                end if
            end if
        end if

        if (allocated(attr_tokens)) deallocate (attr_tokens)
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

        if (parser%current_token < 1 .or. parser%current_token > size(parser%tokens)) then
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
        case default
            if (token%kind == TK_NUMBER) then
                read (token%text, *) type_spec%kind_value
                type_spec%has_kind = .true.
                token = parser%consume()
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
            call analyze_derived_type_tokens(type_spec, collected_tokens, arena)
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
            type_spec%type_name = trim(type_spec%base_keyword) // "(" // &
                derived_text // ")"
        else
            type_spec%type_name = trim(type_spec%base_keyword) // "()"
        end if
    end subroutine parse_parenthesized_derived

    subroutine gather_derived_type_tokens(parser, tokens)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), allocatable, intent(out) :: tokens(:)
        type(token_t) :: token
        integer :: depth

        if (allocated(tokens)) deallocate (tokens)

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

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%text == ")") then
                token = parser%consume()
                exit
            end if

            call handle_character_parameter(parser, type_spec)
            call consume_comma_if_present(parser)
        end do
    end subroutine parse_parenthesized_character

    subroutine handle_character_parameter(parser, type_spec)
        type(parser_state_t), intent(inout) :: parser
        type(type_specifier_t), intent(inout) :: type_spec
        type(token_t) :: token
        character(len=:), allocatable :: normalized

        if (parser%is_at_end()) return

        token = parser%peek()
        normalized = to_lower(trim(adjustl(token%text)))

        select case (normalized)
        case ("len")
            token = parser%consume()
            call consume_optional_equals(parser)
            if (.not. parser%is_at_end()) then
                token = parser%peek()
                select case (token%text)
                case ("*")
                    type_spec%has_kind = .true.
                    type_spec%kind_value = -1
                    token = parser%consume()
                case default
                    if (token%kind == TK_NUMBER) then
                        read (token%text, *) type_spec%kind_value
                        type_spec%has_kind = .true.
                        token = parser%consume()
                    end if
                end select
            end if
        case ("kind")
            token = parser%consume()
            call consume_optional_equals(parser)
            if (.not. parser%is_at_end()) then
                token = parser%consume()
            end if
        case default
            if (token%kind == TK_NUMBER) then
                read (token%text, *) type_spec%kind_value
                type_spec%has_kind = .true.
                token = parser%consume()
            else if (token%text == "*") then
                type_spec%has_kind = .true.
                type_spec%kind_value = -1
                token = parser%consume()
            else
                token = parser%consume()
            end if
        end select
    end subroutine handle_character_parameter

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

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%text == ")") then
                token = parser%consume()
                exit
            end if
            call append_token(collected_tokens, token)
            token = parser%consume()
            call consume_comma_if_present(parser)
        end do

        if (allocated(collected_tokens)) then
            collected_text = tokens_to_text(collected_tokens)
            type_spec%type_name = trim(type_spec%base_keyword) // "(" // &
                                  trim(adjustl(collected_text)) // ")"
            deallocate (collected_tokens)
        else
            type_spec%type_name = trim(type_spec%base_keyword) // "()"
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
    end function parse_type_specifier

    ! Parse declaration attributes like allocatable, pointer, intent, etc.
    subroutine parse_declaration_attributes(parser, arena, attr_info)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(declaration_attributes_t), intent(out) :: attr_info

        type(token_t) :: token

        ! Initialize attributes
        attr_info%is_allocatable = .false.
        attr_info%is_pointer = .false.
        attr_info%is_target = .false.
        attr_info%is_parameter = .false.
        attr_info%is_external = .false.
        attr_info%is_optional = .false.
        attr_info%has_intent = .false.
        attr_info%has_global_dimensions = .false.

        ! Parse basic attributes (simplified)
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%text == ",") then
                token = parser%consume()
                token = parser%peek()

                select case (token%text)
                case ("allocatable")
                    attr_info%is_allocatable = .true.
                    token = parser%consume()
                case ("pointer")
                    attr_info%is_pointer = .true.
                    token = parser%consume()
                case ("parameter")
                    attr_info%is_parameter = .true.
                    token = parser%consume()
                case ("external")
                    attr_info%is_external = .true.
                    token = parser%consume()
                case ("dimension")
                    token = parser%consume()
                    if (.not. parser%is_at_end()) then
                        token = parser%peek()
                        if (token%text == "(") then
                            token = parser%consume()  ! consume '('
            call parse_array_dimensions(parser, arena, attr_info%global_dimension_indices)
                            attr_info%has_global_dimensions = .true.
                        end if
                    end if
                case ("intent")
                    token = parser%consume()  ! consume 'intent'
                    if (.not. parser%is_at_end()) then
                        token = parser%peek()
                        if (token%text == "(") then
                            token = parser%consume()  ! consume '('
                            if (.not. parser%is_at_end()) then
                                token = parser%peek()
                                select case (token%text)
                                case ("in")
                                    attr_info%intent = "in"
                                    attr_info%has_intent = .true.
                                    token = parser%consume()
                                case ("out")
                                    attr_info%intent = "out"
                                    attr_info%has_intent = .true.
                                    token = parser%consume()
                                case ("inout")
                                    attr_info%intent = "inout"
                                    attr_info%has_intent = .true.
                                    token = parser%consume()
                                end select
                                ! consume closing paren
                                if (.not. parser%is_at_end()) then
                                    token = parser%peek()
                                    if (token%text == ")") then
                                        token = parser%consume()
                                    end if
                                end if
                            end if
                        end if
                    end if
                case ("optional")
                    attr_info%is_optional = .true.
                    token = parser%consume()
                case ("target")
                    attr_info%is_target = .true.
                    token = parser%consume()
                case default
                    exit
                end select
            else
                exit
            end if
        end do
    end subroutine parse_declaration_attributes

    ! Parse single-variable declaration (e.g., real :: x)
    function parse_declaration(parser, arena) result(decl_index)
        use ast_factory, only: push_declaration
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: decl_index

        type(token_t) :: token
        type(type_specifier_t) :: type_spec
        type(declaration_attributes_t) :: attr_info
        integer :: initializer_index
        character(len=:), allocatable :: var_name
        integer, allocatable :: local_dimension_indices(:)
        logical :: has_local_dimensions

        decl_index = 0
        initializer_index = 0

        ! Parse type specifier
        type_spec = parse_type_specifier(parser, arena)
        if (.not. allocated(type_spec%type_name)) then
            return
        end if

        ! Parse declaration attributes
        call parse_declaration_attributes(parser, arena, attr_info)

        ! Check for :: separator
        token = parser%peek()
        if (token%text == "::") then
            token = parser%consume()
        end if

        ! Skip any newlines after ::
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE) then
                token = parser%consume()
            else
                exit
            end if
        end do

        ! Get variable name(s) - handle both single and multiple variables
        ! Removed is_at_end check - might prevent parsing after newlines

        token = parser%consume()
        if (token%kind /= TK_IDENTIFIER) then
            return
        end if

        ! Check if this is a multi-variable declaration by looking ahead for commas
        block
            character(len=64), allocatable :: var_names(:)
            integer :: var_count, i, temp_index
            character(len=64) :: first_var_name
            type(token_t) :: next_token
            logical :: is_multi_var

            first_var_name = trim(token%text)
            var_count = 1
            is_multi_var = .false.

            ! Look ahead for commas to detect multi-variable declaration
            if (.not. parser%is_at_end()) then
                next_token = parser%peek()
                if (next_token%text == ",") then
                    is_multi_var = .true.

                    ! Collect all variable names
                    allocate (var_names(10))  ! Start with reasonable size
                    var_names(1) = first_var_name

                    do while (.not. parser%is_at_end())
                        next_token = parser%peek()
                        if (next_token%text == ",") then
                            ! Consume comma
                            next_token = parser%consume()

                            ! Get next variable name
                            if (.not. parser%is_at_end()) then
                                next_token = parser%consume()
                                if (next_token%kind == TK_IDENTIFIER) then
                                    var_count = var_count + 1
                                    if (var_count > size(var_names)) then
                                        ! Extend array if needed
                                        block
                                           character(len=64), allocatable :: temp_names(:)
                                            integer :: old_size
                                            old_size = size(var_names)
                                            allocate (temp_names(old_size * 2))
                                            temp_names(1:old_size) = var_names(1:old_size)
                                            deallocate (var_names)
                                            call move_alloc(temp_names, var_names)
                                        end block
                                    end if
                                    var_names(var_count) = trim(next_token%text)
                                else
                                    exit
                                end if
                            else
                                exit
                            end if
                        else
                            exit
                        end if
                    end do
                end if
            end if

            if (is_multi_var) then
                ! Create multi-variable declaration preserving attributes
                if (type_spec%has_kind) then
                    if (attr_info%has_global_dimensions) then
                        temp_index = push_multi_declaration( &
                                     arena, &
                                     type_spec%type_name, &
                                     var_names(1:var_count), &
                                     kind_value=type_spec%kind_value, &
                                   dimension_indices=attr_info%global_dimension_indices, &
                                     is_allocatable=attr_info%is_allocatable, &
                                     is_pointer=attr_info%is_pointer, &
                                     is_parameter=attr_info%is_parameter &
                                     )
                    else
                        temp_index = push_multi_declaration( &
                                     arena, &
                                     type_spec%type_name, &
                                     var_names(1:var_count), &
                                     kind_value=type_spec%kind_value, &
                                     is_allocatable=attr_info%is_allocatable, &
                                     is_pointer=attr_info%is_pointer, &
                                     is_parameter=attr_info%is_parameter &
                                     )
                    end if
                else
                    if (attr_info%has_global_dimensions) then
                        temp_index = push_multi_declaration( &
                                     arena, &
                                     type_spec%type_name, &
                                     var_names(1:var_count), &
                                   dimension_indices=attr_info%global_dimension_indices, &
                                     is_allocatable=attr_info%is_allocatable, &
                                     is_pointer=attr_info%is_pointer, &
                                     is_parameter=attr_info%is_parameter &
                                     )
                    else
                        temp_index = push_multi_declaration( &
                                     arena, &
                                     type_spec%type_name, &
                                     var_names(1:var_count), &
                                     is_allocatable=attr_info%is_allocatable, &
                                     is_pointer=attr_info%is_pointer, &
                                     is_parameter=attr_info%is_parameter &
                                     )
                    end if
                end if
                decl_index = temp_index
                if (temp_index > 0) then
                    if (attr_info%has_global_dimensions) then
                        call register_type_annotation(temp_index, type_spec%type_name, &
                                    var_names(1:var_count), has_kind=type_spec%has_kind, &
                                                      kind_value=type_spec%kind_value, &
                                                    is_parameter=attr_info%is_parameter, &
                                                is_allocatable=attr_info%is_allocatable, &
                                                      is_pointer=attr_info%is_pointer, &
                                     dimension_indices=attr_info%global_dimension_indices)
                    else
                        call register_type_annotation(temp_index, type_spec%type_name, &
                                    var_names(1:var_count), has_kind=type_spec%has_kind, &
                                                      kind_value=type_spec%kind_value, &
                                                    is_parameter=attr_info%is_parameter, &
                                                is_allocatable=attr_info%is_allocatable, &
                                                      is_pointer=attr_info%is_pointer)
                    end if
                end if
                return
            end if

            var_name = token%text
            has_local_dimensions = .false.

            ! Per-variable dimensions: e.g., "integer :: arr(10)"
            if (.not. parser%is_at_end()) then
                token = parser%peek()
                if (token%text == "(") then
                    token = parser%consume()  ! consume '('
                    call parse_array_dimensions(parser, arena, local_dimension_indices)
                    has_local_dimensions = .true.
                end if
            end if

            ! Check for initialization
            if (.not. parser%is_at_end()) then
                token = parser%peek()
                if (token%text == "=" .or. token%text == "=>") then
                    token = parser%consume()
                    ! Special handling for complex type initializers
                    if (type_spec%type_name == "complex") then
        initializer_index = handle_complex_initializer(parser, arena, type_spec%type_name)
                    else
                        initializer_index = parse_comparison(parser, arena)
                    end if
                end if
            end if

            ! Create declaration node
            if (attr_info%has_global_dimensions) then
                if (type_spec%has_kind) then
                    decl_index = push_declaration( &
                                 arena, &
                                 type_spec%type_name, &
                                 var_name, &
                                 kind_value=type_spec%kind_value, &
                                 dimension_indices=attr_info%global_dimension_indices, &
                                 initializer_index=initializer_index, &
                                 is_allocatable=attr_info%is_allocatable, &
                                 is_pointer=attr_info%is_pointer, &
                                 is_target=attr_info%is_target, &
                                 intent_value=attr_info%intent, &
                                 is_optional=attr_info%is_optional, &
                                 is_parameter=attr_info%is_parameter &
                                 )
                else
                    decl_index = push_declaration( &
                                 arena, &
                                 type_spec%type_name, &
                                 var_name, &
                                 dimension_indices=attr_info%global_dimension_indices, &
                                 initializer_index=initializer_index, &
                                 is_allocatable=attr_info%is_allocatable, &
                                 is_pointer=attr_info%is_pointer, &
                                 is_target=attr_info%is_target, &
                                 intent_value=attr_info%intent, &
                                 is_optional=attr_info%is_optional, &
                                 is_parameter=attr_info%is_parameter &
                                 )
                end if
            else if (has_local_dimensions) then
                if (type_spec%has_kind) then
                    decl_index = push_declaration( &
                                 arena, &
                                 type_spec%type_name, &
                                 var_name, &
                                 kind_value=type_spec%kind_value, &
                                 dimension_indices=local_dimension_indices, &
                                 initializer_index=initializer_index, &
                                 is_allocatable=attr_info%is_allocatable, &
                                 is_pointer=attr_info%is_pointer, &
                                 is_target=attr_info%is_target, &
                                 intent_value=attr_info%intent, &
                                 is_optional=attr_info%is_optional, &
                                 is_parameter=attr_info%is_parameter &
                                 )
                else
                    decl_index = push_declaration( &
                                 arena, &
                                 type_spec%type_name, &
                                 var_name, &
                                 dimension_indices=local_dimension_indices, &
                                 initializer_index=initializer_index, &
                                 is_allocatable=attr_info%is_allocatable, &
                                 is_pointer=attr_info%is_pointer, &
                                 is_target=attr_info%is_target, &
                                 intent_value=attr_info%intent, &
                                 is_optional=attr_info%is_optional, &
                                 is_parameter=attr_info%is_parameter &
                                 )
                end if
            else
                if (type_spec%has_kind) then
                    decl_index = push_declaration( &
                                 arena, &
                                 type_spec%type_name, &
                                 var_name, &
                                 kind_value=type_spec%kind_value, &
                                 initializer_index=initializer_index, &
                                 is_allocatable=attr_info%is_allocatable, &
                                 is_pointer=attr_info%is_pointer, &
                                 is_target=attr_info%is_target, &
                                 intent_value=attr_info%intent, &
                                 is_optional=attr_info%is_optional, &
                                 is_parameter=attr_info%is_parameter &
                                 )
                else
                    decl_index = push_declaration( &
                                 arena, &
                                 type_spec%type_name, &
                                 var_name, &
                                 initializer_index=initializer_index, &
                                 is_allocatable=attr_info%is_allocatable, &
                                 is_pointer=attr_info%is_pointer, &
                                 is_target=attr_info%is_target, &
                                 intent_value=attr_info%intent, &
                                 is_optional=attr_info%is_optional, &
                                 is_parameter=attr_info%is_parameter &
                                 )
                end if
            end if

            if (decl_index > 0) then
                if (attr_info%has_global_dimensions) then
                    call register_type_annotation(decl_index, type_spec%type_name, &
                                 [adjustl(trim(var_name))], has_kind=type_spec%has_kind, &
                                                  kind_value=type_spec%kind_value, &
                                                  is_parameter=attr_info%is_parameter, &
                                                is_allocatable=attr_info%is_allocatable, &
                                                  is_pointer=attr_info%is_pointer, &
                                     dimension_indices=attr_info%global_dimension_indices)
                else if (has_local_dimensions) then
                    call register_type_annotation(decl_index, type_spec%type_name, &
                                 [adjustl(trim(var_name))], has_kind=type_spec%has_kind, &
                                                  kind_value=type_spec%kind_value, &
                                                  is_parameter=attr_info%is_parameter, &
                                                is_allocatable=attr_info%is_allocatable, &
                                                  is_pointer=attr_info%is_pointer, &
                                                dimension_indices=local_dimension_indices)
                else
                    call register_type_annotation(decl_index, type_spec%type_name, &
                                 [adjustl(trim(var_name))], has_kind=type_spec%has_kind, &
                                                  kind_value=type_spec%kind_value, &
                                                  is_parameter=attr_info%is_parameter, &
                                                is_allocatable=attr_info%is_allocatable, &
                                                  is_pointer=attr_info%is_pointer)
                end if
            end if

        end block
    end function parse_declaration

    ! Result-based declaration parser with structured error handling
    function parse_declaration_with_result(parser, arena) result(parse_res)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parse_result_t) :: parse_res

        integer :: decl_index

        decl_index = parse_declaration(parser, arena)

        if (decl_index > 0) then
            parse_res = success_parse_result(decl_index)
        else
            parse_res = error_parse_result("Failed to parse declaration", ERROR_PARSER)
        end if
    end function parse_declaration_with_result

    ! Parse array dimensions (e.g., (:), (10), (1:n))
    subroutine parse_array_dimensions(parser, arena, dimension_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: dimension_indices(:)

        integer, parameter :: max_dims = 10
        integer :: temp_indices(max_dims)
        integer :: dim_count, range_index
        type(token_t) :: token

        dim_count = 0

        ! Parse dimension list until closing parenthesis
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%text == ")") then
                token = parser%consume()
                exit
            end if

            ! Parse dimension specification
            range_index = parse_range(parser, arena)
            if (range_index > 0 .and. dim_count < max_dims) then
                dim_count = dim_count + 1
                temp_indices(dim_count) = range_index
            end if

            ! Check for comma
            token = parser%peek()
            if (token%text == ",") then
                token = parser%consume()
            else if (token%text /= ")") then
                exit
            end if
        end do

        ! Allocate exact size needed
        if (dim_count > 0) then
            allocate (dimension_indices(dim_count))
            dimension_indices = temp_indices(1:dim_count)
        else
            allocate (dimension_indices(0))
        end if
    end subroutine parse_array_dimensions

    ! Parse multi-variable declaration (e.g., real :: x, y, z = 1.0)
    function parse_multi_declaration(parser, arena) result(decl_indices)
        use, intrinsic :: iso_fortran_env, only: error_unit
        use ast_factory, only: push_multi_declaration, push_declaration
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable :: decl_indices(:)

        type(token_t) :: token, next_token
        type(type_specifier_t) :: type_spec
        type(declaration_attributes_t) :: attr_info
        character(len=64), allocatable :: var_names(:)
        integer, allocatable :: per_var_dims(:, :)  ! Store dimensions per variable
        logical, allocatable :: has_dims(:)  ! Track which vars have dimensions
        integer, allocatable :: init_indices(:)
        integer :: var_count, decl_index, i
        logical :: has_any_initializer

        ! Parse type specifier
        type_spec = parse_type_specifier(parser, arena)
        if (.not. allocated(type_spec%type_name)) then
            allocate (decl_indices(0))
            return
        end if

        ! Parse declaration attributes
        call parse_declaration_attributes(parser, arena, attr_info)

        ! Check for :: separator
        token = parser%peek()
        if (token%text == "::") then
            token = parser%consume()
        end if

        ! Collect all variable names and their dimensions
        allocate (var_names(10))  ! Start with reasonable size
        allocate (per_var_dims(10, 10))  ! Max 10 vars, max 10 dims each
        allocate (has_dims(10))
        allocate (init_indices(10))
        var_count = 0
        has_any_initializer = .false.
        per_var_dims = 0
        has_dims = .false.
        init_indices = 0

        do while (.not. parser%is_at_end())
            ! Get variable name
            token = parser%consume()
            if (token%kind /= TK_IDENTIFIER) exit

            var_count = var_count + 1
            if (var_count > size(var_names)) then
                ! Extend arrays if needed
                block
                    character(len=64), allocatable :: temp_names(:)
                    integer, allocatable :: temp_dims(:, :)
                    logical, allocatable :: temp_has(:)
                    integer, allocatable :: temp_init(:)
                    integer :: old_size, new_size
                    old_size = size(var_names)
                    new_size = old_size * 2
                    allocate (temp_names(new_size))
                    allocate (temp_dims(new_size, 10))
                    allocate (temp_has(new_size))
                    allocate (temp_init(new_size))
                    temp_names = ''
                    temp_dims = 0
                    temp_has = .false.
                    temp_init = 0
                    temp_names(1:old_size) = var_names(1:old_size)
                    temp_dims(1:old_size, :) = per_var_dims(1:old_size, :)
                    temp_has(1:old_size) = has_dims(1:old_size)
                    temp_init(1:old_size) = init_indices(1:old_size)
                    deallocate (var_names, per_var_dims, has_dims, init_indices)
                    call move_alloc(temp_names, var_names)
                    call move_alloc(temp_dims, per_var_dims)
                    call move_alloc(temp_has, has_dims)
                    call move_alloc(temp_init, init_indices)
                end block
            end if
            var_names(var_count) = token%text
            init_indices(var_count) = 0
            has_dims(var_count) = .false.
            per_var_dims(var_count, :) = 0

            ! Check for array dimensions for this variable
            if (.not. parser%is_at_end()) then
                next_token = parser%peek()
                if (next_token%text == "(") then
                    ! This variable has dimensions
                    token = parser%consume()  ! consume '('
                    block
                        integer, allocatable :: local_dims(:)
                        integer :: j
                        call parse_array_dimensions(parser, arena, local_dims)
                        if (allocated(local_dims) .and. size(local_dims) > 0) then
                            has_dims(var_count) = .true.
                            do j = 1, min(size(local_dims), 10)
                                per_var_dims(var_count, j) = local_dims(j)
                            end do
                        end if
                    end block
                end if
            end if

            ! Check for initializer for this variable
            if (.not. parser%is_at_end()) then
                next_token = parser%peek()
                if (next_token%text == "=" .or. next_token%text == "=>") then
                    next_token = parser%consume()
                    if (type_spec%type_name == "complex") then
  init_indices(var_count) = handle_complex_initializer(parser, arena, type_spec%type_name)
                    else
                        init_indices(var_count) = parse_comparison(parser, arena)
                    end if
                    if (init_indices(var_count) > 0) has_any_initializer = .true.
                end if
            end if

            ! Check for comma or end of variables
            if (.not. parser%is_at_end()) then
                next_token = parser%peek()
                if (next_token%text == ",") then
                    next_token = parser%consume()
                    cycle
                end if
            end if
            exit
        end do

        if (var_count == 0) then
            allocate (decl_indices(0))
            return
        end if

        ! Check if we have per-variable dimensions
        block
            logical :: needs_separate_decls
            integer :: num_with_dims

            num_with_dims = 0
            do i = 1, var_count
                if (has_dims(i)) num_with_dims = num_with_dims + 1
            end do

            ! If we have per-variable dimensions, create separate declarations
            needs_separate_decls = (num_with_dims > 0) .or. has_any_initializer

            if (needs_separate_decls) then
                ! Create separate declaration for each variable
                allocate (decl_indices(var_count))
                do i = 1, var_count
                    if (has_dims(i)) then
                        ! Variable with dimensions
                        block
                            integer, allocatable :: var_dims(:)
                            integer :: j, dim_count

                            ! Count dimensions for this variable
                            dim_count = 0
                            do j = 1, 10
                                if (per_var_dims(i, j) > 0) then
                                    dim_count = dim_count + 1
                                else
                                    exit
                                end if
                            end do

                            if (dim_count > 0) then
                                allocate (var_dims(dim_count))
                                var_dims = per_var_dims(i, 1:dim_count)

                                decl_indices(i) = push_declaration( &
                                                  arena, &
                                                  type_spec%type_name, &
                                                  var_names(i), &
                                                  dimension_indices=var_dims, &
                                                  initializer_index=init_indices(i), &
                                                is_allocatable=attr_info%is_allocatable, &
                                                  is_pointer=attr_info%is_pointer, &
                                                  is_target=attr_info%is_target, &
                                                  intent_value=attr_info%intent, &
                                                  is_optional=attr_info%is_optional, &
                                                  is_parameter=attr_info%is_parameter &
                                                  )
                                if (decl_indices(i) > 0) then
                     call register_type_annotation(decl_indices(i), type_spec%type_name, &
                             [adjustl(trim(var_names(i)))], has_kind=type_spec%has_kind, &
                                                        kind_value=type_spec%kind_value, &
                                                    is_parameter=attr_info%is_parameter, &
                                                is_allocatable=attr_info%is_allocatable, &
                              is_pointer=attr_info%is_pointer, dimension_indices=var_dims)
                                end if
                            end if
                        end block
                    else if (attr_info%has_global_dimensions) then
                        ! Variable without per-var dims but with global dims
                        decl_indices(i) = push_declaration( &
                                          arena, &
                                          type_spec%type_name, &
                                          var_names(i), &
                                   dimension_indices=attr_info%global_dimension_indices, &
                                          initializer_index=init_indices(i), &
                                          is_allocatable=attr_info%is_allocatable, &
                                          is_pointer=attr_info%is_pointer, &
                                          is_target=attr_info%is_target, &
                                          intent_value=attr_info%intent, &
                                          is_optional=attr_info%is_optional, &
                                          is_parameter=attr_info%is_parameter &
                                          )
                        if (decl_indices(i) > 0) then
                     call register_type_annotation(decl_indices(i), type_spec%type_name, &
                             [adjustl(trim(var_names(i)))], has_kind=type_spec%has_kind, &
                                                        kind_value=type_spec%kind_value, &
                                                    is_parameter=attr_info%is_parameter, &
                                                is_allocatable=attr_info%is_allocatable, &
                                                        is_pointer=attr_info%is_pointer, &
                                     dimension_indices=attr_info%global_dimension_indices)
                        end if
                    else
                        ! Variable without dimensions
                        decl_indices(i) = push_declaration( &
                                          arena, &
                                          type_spec%type_name, &
                                          var_names(i), &
                                          initializer_index=init_indices(i), &
                                          is_allocatable=attr_info%is_allocatable, &
                                          is_pointer=attr_info%is_pointer, &
                                          is_target=attr_info%is_target, &
                                          intent_value=attr_info%intent, &
                                          is_optional=attr_info%is_optional, &
                                          is_parameter=attr_info%is_parameter &
                                          )
                        if (decl_indices(i) > 0) then
                     call register_type_annotation(decl_indices(i), type_spec%type_name, &
                             [adjustl(trim(var_names(i)))], has_kind=type_spec%has_kind, &
                                                        kind_value=type_spec%kind_value, &
                                                    is_parameter=attr_info%is_parameter, &
                                                is_allocatable=attr_info%is_allocatable, &
                                                          is_pointer=attr_info%is_pointer)
                        end if
                    end if
                end do

            else
                ! Use original multi-declaration approach when no per-var dims
                if (type_spec%has_kind) then
                    if (attr_info%has_global_dimensions) then
                        decl_index = push_multi_declaration( &
                                     arena, &
                                     type_spec%type_name, &
                                     var_names(1:var_count), &
                                     kind_value=type_spec%kind_value, &
                                   dimension_indices=attr_info%global_dimension_indices, &
                                     is_allocatable=attr_info%is_allocatable, &
                                     is_pointer=attr_info%is_pointer, &
                                     is_parameter=attr_info%is_parameter &
                                     )
                    else
                        decl_index = push_multi_declaration( &
                                     arena, &
                                     type_spec%type_name, &
                                     var_names(1:var_count), &
                                     kind_value=type_spec%kind_value, &
                                     is_allocatable=attr_info%is_allocatable, &
                                     is_pointer=attr_info%is_pointer, &
                                     is_parameter=attr_info%is_parameter &
                                     )
                    end if
                else
                    if (attr_info%has_global_dimensions) then
                        decl_index = push_multi_declaration( &
                                     arena, &
                                     type_spec%type_name, &
                                     var_names(1:var_count), &
                                   dimension_indices=attr_info%global_dimension_indices, &
                                     is_allocatable=attr_info%is_allocatable, &
                                     is_pointer=attr_info%is_pointer, &
                                     is_parameter=attr_info%is_parameter &
                                     )
                    else
                        decl_index = push_multi_declaration( &
                                     arena, &
                                     type_spec%type_name, &
                                     var_names(1:var_count), &
                                     is_allocatable=attr_info%is_allocatable, &
                                     is_pointer=attr_info%is_pointer, &
                                     is_parameter=attr_info%is_parameter &
                                     )
                    end if
                end if

                if (decl_index > 0) then
                    allocate (decl_indices(1))
                    decl_indices(1) = decl_index
                    if (attr_info%has_global_dimensions) then
                        call register_type_annotation(decl_index, type_spec%type_name, &
                                    var_names(1:var_count), has_kind=type_spec%has_kind, &
                                                      kind_value=type_spec%kind_value, &
                                                    is_parameter=attr_info%is_parameter, &
                                                is_allocatable=attr_info%is_allocatable, &
                                                      is_pointer=attr_info%is_pointer, &
                                     dimension_indices=attr_info%global_dimension_indices)
                    else
                        call register_type_annotation(decl_index, type_spec%type_name, &
                                    var_names(1:var_count), has_kind=type_spec%has_kind, &
                                                      kind_value=type_spec%kind_value, &
                                                    is_parameter=attr_info%is_parameter, &
                                                is_allocatable=attr_info%is_allocatable, &
                                                      is_pointer=attr_info%is_pointer)
                    end if
                else
                    allocate (decl_indices(0))
                end if
            end if
        end block
    end function parse_multi_declaration

    ! Parse derived type definition with robust error handling
    function parse_derived_type_def(parser, arena) result(type_index)
        use ast_factory, only: push_derived_type
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: type_index

        type(token_t) :: token
        character(len=100) :: type_name
        character(len=:), allocatable :: header_attributes
        logical :: has_header_attrs
        integer :: comp_index
        integer, parameter :: max_components = 100
        integer :: component_indices(max_components)
        integer :: component_count
        logical :: invalid_type_spec

        type_index = 0
        component_count = 0
        token = parser%peek()
        ! Consume 'type'
        token = parser%consume()

        call skip_type_definition_attributes(parser, invalid_type_spec, header_attributes)
        if (invalid_type_spec) then
            return
        end if

        has_header_attrs = .false.
        if (allocated(header_attributes)) then
            if (len_trim(header_attributes) > 0) then
                has_header_attrs = .true.
            else
                deallocate (header_attributes)
            end if
        end if

        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER) then
            return
        end if
        token = parser%consume()
        type_name = trim(token%text)

        ! Skip any semicolons or newlines
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ";") then
                token = parser%consume()
            else if (token%kind == TK_NEWLINE .or. token%kind == TK_WHITESPACE .or. &
                     token%kind == TK_COMMENT) then
                token = parser%consume()
            else
                exit
            end if
        end do

        ! Parse components
        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for end type
            if ((token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) .and. token%text == "end") then
                token = parser%consume()
                token = parser%peek()
                if ((token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) .and. token%text == "type") then
                    token = parser%consume()
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER) then
                        token = parser%consume()
                    end if
                end if
                exit
            end if

            ! Parse component
            comp_index = parse_derived_type_component(parser, arena)
            if (comp_index > 0 .and. component_count < max_components) then
                component_count = component_count + 1
                component_indices(component_count) = comp_index
                ! Skip any trailing newlines after parsing a component
                do while (.not. parser%is_at_end())
                    token = parser%peek()
                    if (token%kind == TK_NEWLINE) then
                        token = parser%consume()
                    else if (token%kind == TK_WHITESPACE .or. &
                             token%kind == TK_COMMENT) then
                        token = parser%consume()
                    else
                        exit
                    end if
                end do
            else if (comp_index == 0) then
                ! If we couldn't parse a component, skip to next line or token
                token = parser%peek()
            if (.not. ((token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) .and. &
                           token%text == "end")) then
                    if (token%kind == TK_NEWLINE) then
                        token = parser%consume()
                    else if (token%kind == TK_WHITESPACE .or. &
                             token%kind == TK_COMMENT) then
                        token = parser%consume()
                    else
                        ! Skip unknown token to avoid infinite loop
                        token = parser%consume()
                    end if
                end if
            end if
        end do

        ! Create derived type node
        if (component_count > 0) then
            if (has_header_attrs) then
                type_index = push_derived_type(arena, type_name, &
                                               component_indices(1:component_count), &
                                               attribute_clause=header_attributes)
            else
                type_index = push_derived_type(arena, type_name, &
                                               component_indices(1:component_count))
            end if
        else
            if (has_header_attrs) then
                type_index = push_derived_type(arena, type_name, &
                                               [integer ::], &
                                                attribute_clause=header_attributes)
            else
                type_index = push_derived_type(arena, type_name, &
                                               [integer ::])
            end if
        end if
    end function parse_derived_type_def

    ! Parse derived type component with robust error handling and loop prevention
    function parse_derived_type_component(parser, arena) result(comp_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: comp_index

        type(token_t) :: token

        comp_index = 0

        ! Skip any leading newlines
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE) then
                token = parser%consume()
            else if (token%kind == TK_WHITESPACE .or. &
                     token%kind == TK_COMMENT) then
                token = parser%consume()
            else
                exit
            end if
        end do

        token = parser%peek()

        ! Handle end of type definition
        if ((token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) .and. token%text == "end") then
            return
        end if

        ! Check for type declaration keywords
        if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
            select case (trim(adjustl(token%text)))
            case ("integer", "real", "complex", "logical", "character", "type", "double")
                comp_index = parse_declaration(parser, arena)
            case default
                ! Not a component declaration, return 0
                comp_index = 0
            end select
        else
            ! Not a component declaration
            comp_index = 0
        end if
    end function parse_derived_type_component

    ! Helper function to detect and convert complex literals
    ! When we have a complex type declaration with initializer like (1.0, 2.0),
    ! we need to parse it as a complex literal, not just take the first value
    function handle_complex_initializer(parser, arena, type_name) result(complex_index)
        use ast_factory, only: push_complex_literal
        use parser_expressions_module, only: parse_comparison
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: type_name
        integer :: complex_index

        type(token_t) :: token
        integer :: real_index, imag_index

        complex_index = 0

        ! Only handle if type is complex
        if (type_name /= "complex") then
            ! Not a complex type, parse normally
            complex_index = parse_comparison(parser, arena)
            return
        end if

        ! Check for opening parenthesis
        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            ! Not a parenthesized expression, parse normally
            complex_index = parse_comparison(parser, arena)
            return
        end if

        ! Consume opening parenthesis
        token = parser%consume()

        ! Parse real part
        real_index = parse_comparison(parser, arena)

        ! Check for comma
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            ! This looks like a complex literal
            token = parser%consume()  ! consume comma

            ! Parse imaginary part
            imag_index = parse_comparison(parser, arena)

            ! Check for closing parenthesis
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()  ! consume closing paren

                ! Create complex literal node
                complex_index = push_complex_literal(arena, real_index, imag_index, &
                                                     token%line, token%column)
            else
                ! Malformed, return what we have
                complex_index = real_index
            end if
        else
            ! Not a complex literal, just a parenthesized expression
            ! Check for closing parenthesis
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
            end if
            complex_index = real_index
        end if

    end function handle_complex_initializer

end module parser_declarations
