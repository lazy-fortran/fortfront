module parser_procedure_signatures_module
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD, TK_OPERATOR, &
        TK_NEWLINE, TK_WHITESPACE, TK_STRING
    use parser_state_module, only: parser_state_t
    use parser_parameter_handling_module, only: parse_typed_parameters, &
        merge_parameter_attributes
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t, append_prefix_token
    use parser_procedure_shared_module, only: consume_optional_return_type, &
        consume_optional_kind_spec, &
        keyword_can_be_function_name
    use ast_arena_modern, only: ast_arena_t
    implicit none
    private

    public :: parse_function_prefix_keywords
    public :: parse_function_signature
    public :: parse_function_result_clause
    public :: parse_parameter_list
    public :: merge_parameter_attributes_if_needed
    public :: ensure_recursive_prefix
    public :: parse_subroutine_header
    public :: parse_bind_c_clause

contains

    subroutine parse_function_prefix_keywords(parser, prefix_buffer, prefix_list, &
            prefix_keywords, has_recursive_keyword, &
            return_type_from_prefix)
        type(parser_state_t), intent(inout) :: parser
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), intent(in), optional :: prefix_list(:)
        character(len=16), allocatable, intent(out) :: prefix_keywords(:)
        logical, intent(out) :: has_recursive_keyword
        character(len=:), allocatable, intent(out), optional :: return_type_from_prefix

        character(len=16), allocatable :: pending_prefixes(:)
        character(len=:), allocatable :: local_return_type

        has_recursive_keyword = .false.
        allocate (character(len=16) :: prefix_keywords(0))
        local_return_type = ""
        if (present(return_type_from_prefix)) return_type_from_prefix = ""

        if (present(return_type_from_prefix)) then
            block
                character(len=:), allocatable :: buffered_return_type
                call prefix_buffer%consume_return_type(buffered_return_type)
                if (len_trim(buffered_return_type) > 0) then
                    local_return_type = buffered_return_type
                end if
            end block
        end if
        call initialise_function_prefix_sources(prefix_buffer, prefix_list, &
            pending_prefixes)
        call append_pending_prefixes(pending_prefixes, prefix_keywords, &
            has_recursive_keyword, local_return_type)
        if (present(return_type_from_prefix)) then
            return_type_from_prefix = local_return_type
        end if
        call consume_function_prefix_tokens(parser, prefix_keywords, &
            has_recursive_keyword)
    end subroutine parse_function_prefix_keywords

    subroutine initialise_function_prefix_sources(prefix_buffer, prefix_list, &
            pending_prefixes)
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), intent(in), optional :: prefix_list(:)
        character(len=16), allocatable, intent(out) :: pending_prefixes(:)

        if (present(prefix_list)) then
            if (size(prefix_list) > 0) then
                allocate (character(len=16) :: pending_prefixes(size(prefix_list)))
                pending_prefixes = prefix_list
            else
                allocate (character(len=16) :: pending_prefixes(0))
            end if
            call prefix_buffer%clear()
        else
            call prefix_buffer%consume(pending_prefixes)
            if (.not. allocated(pending_prefixes)) then
                allocate (character(len=16) :: pending_prefixes(0))
            end if
        end if
    end subroutine initialise_function_prefix_sources

    subroutine append_pending_prefixes(pending_prefixes, prefix_keywords, &
            has_recursive_keyword, return_type_from_prefix)
        use string_utils_mod, only: to_lower
        character(len=16), intent(in) :: pending_prefixes(:)
        character(len=16), allocatable, intent(inout) :: prefix_keywords(:)
        logical, intent(inout) :: has_recursive_keyword
        character(len=:), allocatable, intent(inout) :: return_type_from_prefix
        integer :: i, n
        character(len=:), allocatable :: lowered, next_lower
        logical :: return_type_set
        n = size(pending_prefixes)
        return_type_set = .false.
        return_type_set = len_trim(return_type_from_prefix) > 0
        i = 1
        do while (i <= n)
            lowered = to_lower(trim(pending_prefixes(i)))
            select case (trim(lowered))
            case ("integer", "real", "logical", "character", "complex", "procedure")
                call set_return_type_from_token(pending_prefixes(i)); i = i + 1; cycle
            case ("double precision", "double complex")
                call set_return_type_from_token(pending_prefixes(i)); i = i + 1; cycle
            case ("double")
                if (i < n) then
                    next_lower = to_lower(trim(pending_prefixes(i + 1)))
                else
                    next_lower = ""
                end if
                if (trim(next_lower) == "precision" .or. trim(next_lower) == &
                    "complex") then
                    call set_return_type_from_token( &
                        trim(pending_prefixes(i))//" "// &
                        trim(pending_prefixes(i + 1)))
                    i = i + 2; cycle
                else
                    call set_return_type_from_token(pending_prefixes(i))
                    i = i + 1; cycle
                end if
            case ("recursive")
                has_recursive_keyword = .true.
                call append_prefix_token(prefix_keywords, pending_prefixes(i))
            case default
                if (has_type_with_parentheses(lowered)) then
                    call set_return_type_from_token(pending_prefixes(i))
                    i = i + 1
                    cycle
                else
                    call append_prefix_token(prefix_keywords, pending_prefixes(i))
                end if
            end select
            i = i + 1
        end do
    contains
        subroutine set_return_type_from_token(token_value)
            character(len=*), intent(in) :: token_value
            if (return_type_set) return
            return_type_from_prefix = trim(token_value)
            return_type_set = .true.
        end subroutine set_return_type_from_token

        logical function has_type_with_parentheses(text) result(has_type)
            character(len=*), intent(in) :: text

            has_type = starts_with(text, "integer(") .or. &
                starts_with(text, "integer (") .or. &
                starts_with(text, "real(") .or. &
                starts_with(text, "real (") .or. &
                starts_with(text, "logical(") .or. &
                starts_with(text, "logical (") .or. &
                starts_with(text, "character(") .or. &
                starts_with(text, "character (") .or. &
                starts_with(text, "complex(") .or. &
                starts_with(text, "complex (") .or. &
                starts_with(text, "procedure(") .or. &
                starts_with(text, "procedure (") .or. &
                starts_with(text, "type(") .or. &
                starts_with(text, "type (") .or. &
                starts_with(text, "class(") .or. &
                starts_with(text, "class (")
        end function has_type_with_parentheses

        logical function starts_with(text, prefix) result(matches)
            character(len=*), intent(in) :: text, prefix
            integer :: prefix_len

            prefix_len = len(prefix)
            if (len_trim(text) < prefix_len) then
                matches = .false.
            else
                matches = text(1:prefix_len) == prefix
            end if
        end function starts_with
    end subroutine append_pending_prefixes

    subroutine consume_function_prefix_tokens(parser, prefix_keywords, &
            has_recursive_keyword)
        type(parser_state_t), intent(inout) :: parser
        character(len=16), allocatable, intent(inout) :: prefix_keywords(:)
        logical, intent(inout) :: has_recursive_keyword

        type(token_t) :: token
        character(len=:), allocatable :: lowered_text

        do
            token = parser%peek()
            if (.not. (token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER)) then
                exit
            end if

            lowered_text = to_lower(token%text)
            select case (trim(lowered_text))
            case ("recursive")
                has_recursive_keyword = .true.
                call append_prefix_token(prefix_keywords, "recursive")
                token = parser%consume()
            case ("pure")
                call append_prefix_token(prefix_keywords, "pure")
                token = parser%consume()
            case ("elemental")
                call append_prefix_token(prefix_keywords, "elemental")
                token = parser%consume()
            case default
                exit
            end select
        end do
    end subroutine consume_function_prefix_tokens

    subroutine parse_function_signature(parser, return_type_str, function_name, &
            line, column, is_valid, &
            return_type_from_prefix)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: return_type_str, function_name
        integer, intent(out) :: line, column
        logical, intent(out) :: is_valid
        character(len=*), intent(in), optional :: return_type_from_prefix

        type(token_t) :: token

        return_type_str = ""
        is_valid = .true.

        ! First try to get return type from prefix (e.g., "integer" before "function")
        if (present(return_type_from_prefix)) then
            if (len_trim(return_type_from_prefix) > 0) then
                return_type_str = trim(return_type_from_prefix)
                call consume_optional_kind_spec(parser, return_type_str)
            end if
        end if

        ! If not in prefix, try to consume from token stream
        if (len_trim(return_type_str) == 0) then
            call consume_optional_return_type(parser, return_type_str)
        end if

        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. to_lower(token%text) == "function") then
            line = token%line
            column = token%column
            token = parser%consume()
        else
            is_valid = .false.
            return
        end if

        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            function_name = token%text
            token = parser%consume()
            call consume_optional_inline_instantiations(parser, function_name)
        else if (token%kind == TK_KEYWORD .and. &
                keyword_can_be_function_name(parser, token)) then
            function_name = token%text
            token = parser%consume()
            call consume_optional_inline_instantiations(parser, function_name)
        else
            function_name = "unnamed_function"
        end if
    end subroutine parse_function_signature

    subroutine parse_function_result_clause(parser, result_variable_name)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: result_variable_name

        type(token_t) :: token

        result_variable_name = ""
        token = parser%peek()
        if ((token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) .and. &
            to_lower(token%text) == "result") then
            token = parser%consume()
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
                    result_variable_name = token%text
                    token = parser%consume()
                end if
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ")") then
                    token = parser%consume()
                end if
            end if
        end if
    end subroutine parse_function_result_clause

    subroutine parse_parameter_list(parser, arena, param_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: param_indices(:)

        type(token_t) :: token

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            token = parser%consume()
            call parse_typed_parameters(parser, arena, param_indices)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
            end if
        else
            allocate (param_indices(0))
        end if
    end subroutine parse_parameter_list

    subroutine merge_parameter_attributes_if_needed(arena, param_indices, &
            body_indices)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: param_indices(:)
        integer, allocatable, intent(in) :: body_indices(:)

        if (.not. allocated(param_indices)) return
        if (.not. allocated(body_indices)) return
        if (size(param_indices) == 0) return
        if (size(body_indices) == 0) return

        call merge_parameter_attributes(arena, param_indices, body_indices)
    end subroutine merge_parameter_attributes_if_needed

    subroutine ensure_recursive_prefix(has_recursive_keyword, &
            infer_recursive_from_body, &
            prefix_keywords)
        logical, intent(inout) :: has_recursive_keyword
        logical, intent(in) :: infer_recursive_from_body
        character(len=16), allocatable, intent(inout) :: prefix_keywords(:)

        if (has_recursive_keyword) return
        if (.not. infer_recursive_from_body) return

        has_recursive_keyword = .true.
        call append_prefix_token(prefix_keywords, "recursive")
    end subroutine ensure_recursive_prefix

    subroutine parse_subroutine_header(parser, subroutine_name, line, column)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: subroutine_name
        integer, intent(out) :: line, column

        type(token_t) :: token

        token = parser%consume()
        line = token%line
        column = token%column

        token = parser%peek()
        if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
            subroutine_name = token%text
            token = parser%consume()
            call consume_optional_inline_instantiations(parser, subroutine_name)
        else
            subroutine_name = "unnamed_subroutine"
        end if
    end subroutine parse_subroutine_header

    subroutine consume_optional_inline_instantiations(parser, procedure_name)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(inout) :: procedure_name
        type(token_t) :: token
        type(token_t) :: start_token
        character(len=:), allocatable :: inst_text
        integer :: nesting

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (.not. (token%kind == TK_OPERATOR .and. token%text == "{")) then
                exit
            end if

            start_token = token
            if (allocated(inst_text)) deallocate (inst_text)
            nesting = 0

            do while (.not. parser%is_at_end())
                token = parser%consume()
                if (token%kind == TK_OPERATOR) then
                    if (token%text == "{") nesting = nesting + 1
                    if (token%text == "}") nesting = nesting - 1
                end if

                if (.not. allocated(inst_text)) then
                    inst_text = token%text
                else
                    inst_text = inst_text // token%text
                end if

                if (nesting == 0) exit
            end do

            if (nesting /= 0) then
                call parser%error_at_token( &
                    "Unbalanced inline instantiation braces", start_token, &
                    suggestion="Add matching }")
                exit
            end if

            if (allocated(inst_text)) then
                procedure_name = procedure_name // inst_text
                deallocate (inst_text)
            end if
        end do
    end subroutine consume_optional_inline_instantiations

    subroutine parse_bind_c_clause(parser, bind_c_clause)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: bind_c_clause

        type(token_t) :: token
        character(len=:), allocatable :: lowered_text
        character(len=:), allocatable :: clause_text

        bind_c_clause = ""
        token = parser%peek()

        if (token%kind /= TK_IDENTIFIER .and. token%kind /= TK_KEYWORD) return
        lowered_text = to_lower(token%text)
        if (trim(lowered_text) /= "bind") return

        clause_text = "bind"
        token = parser%consume()

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            clause_text = trim(clause_text) // "("
            token = parser%consume()

            token = parser%peek()
            if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
                clause_text = trim(clause_text) // trim(token%text)
                token = parser%consume()

                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    clause_text = trim(clause_text) // ","
                    token = parser%consume()

                    token = parser%peek()
                    if (token%kind == TK_WHITESPACE) then
                        clause_text = trim(clause_text) // " "
                        token = parser%consume()
                    end if

                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
                        clause_text = trim(clause_text) // trim(token%text)
                        token = parser%consume()

                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "=") then
                            clause_text = trim(clause_text) // "="
                            token = parser%consume()

                            token = parser%peek()
                            if (token%kind == TK_STRING) then
                                clause_text = trim(clause_text) // trim(token%text)
                                token = parser%consume()
                            end if
                        end if
                    end if
                end if
            end if

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                clause_text = trim(clause_text) // ")"
                token = parser%consume()
            end if
        end if

        bind_c_clause = clause_text
    end subroutine parse_bind_c_clause

end module parser_procedure_signatures_module
