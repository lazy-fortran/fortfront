module parser_procedure_signatures_module
    use string_utils_mod, only: to_lower
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD, TK_OPERATOR
    use parser_state_module, only: parser_state_t
    use parser_parameter_handling_module, only: parse_typed_parameters, &
                                                merge_parameter_attributes
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
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

contains

    subroutine parse_function_prefix_keywords(parser, prefix_buffer, prefix_list, &
                                              prefix_keywords, has_recursive_keyword)
        type(parser_state_t), intent(inout) :: parser
        type(parser_prefix_buffer_t), intent(inout) :: prefix_buffer
        character(len=16), intent(in), optional :: prefix_list(:)
        character(len=16), allocatable, intent(out) :: prefix_keywords(:)
        logical, intent(out) :: has_recursive_keyword

        character(len=16), allocatable :: pending_prefixes(:)

        has_recursive_keyword = .false.
        allocate (character(len=16) :: prefix_keywords(0))

        call initialise_function_prefix_sources(prefix_buffer, prefix_list, &
                                                pending_prefixes)
        call append_pending_prefixes(pending_prefixes, prefix_keywords, &
                                     has_recursive_keyword)
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
                                       has_recursive_keyword)
        character(len=16), intent(in) :: pending_prefixes(:)
        character(len=16), allocatable, intent(inout) :: prefix_keywords(:)
        logical, intent(inout) :: has_recursive_keyword
        integer :: i

        do i = 1, size(pending_prefixes)
            call append_prefix_keyword(prefix_keywords, pending_prefixes(i))
            if (trim(pending_prefixes(i)) == "recursive") then
                has_recursive_keyword = .true.
            end if
        end do
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
                call append_prefix_keyword(prefix_keywords, "recursive")
                token = parser%consume()
            case ("pure")
                call append_prefix_keyword(prefix_keywords, "pure")
                token = parser%consume()
            case ("elemental")
                call append_prefix_keyword(prefix_keywords, "elemental")
                token = parser%consume()
            case default
                exit
            end select
        end do
    end subroutine consume_function_prefix_tokens

    subroutine parse_function_signature(parser, return_type_str, function_name, &
                                        line, column, is_valid)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: return_type_str, function_name
        integer, intent(out) :: line, column
        logical, intent(out) :: is_valid

        type(token_t) :: token

        return_type_str = ""
        is_valid = .true.

        token = parser%peek()
        if (token%kind == TK_KEYWORD) then
            select case (trim(to_lower(token%text)))
            case ("real", "integer", "logical", "character")
                return_type_str = token%text
                token = parser%consume()
            end select
        end if

        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "function") then
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
        else if (token%kind == TK_KEYWORD .and. &
                 keyword_can_be_function_name(parser, token)) then
            function_name = token%text
            token = parser%consume()
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
        if (token%kind == TK_IDENTIFIER .and. token%text == "result") then
            token = parser%consume()
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "(") then
                token = parser%consume()
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
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
        call append_prefix_keyword(prefix_keywords, "recursive")
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
        if (token%kind == TK_IDENTIFIER) then
            subroutine_name = token%text
            token = parser%consume()
        else
            subroutine_name = "unnamed_subroutine"
        end if
    end subroutine parse_subroutine_header

    subroutine append_prefix_keyword(prefixes, value)
        character(len=16), allocatable, intent(inout) :: prefixes(:)
        character(len=*), intent(in) :: value
        integer :: n, i
        character(len=16), allocatable :: temp(:)
        logical :: already_present

        already_present = .false.
        if (allocated(prefixes)) then
            do i = 1, size(prefixes)
                if (trim(prefixes(i)) == trim(value)) then
                    already_present = .true.
                    exit
                end if
            end do
        else
            allocate (character(len=16) :: prefixes(0))
        end if

        if (already_present) return

        n = size(prefixes)
        allocate (character(len=16) :: temp(n + 1))
        if (n > 0) temp(1:n) = prefixes
        temp(n + 1) = trim(value)
        call move_alloc(temp, prefixes)
    end subroutine append_prefix_keyword

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

end module parser_procedure_signatures_module
