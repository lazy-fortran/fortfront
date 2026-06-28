module parser_declarations_core_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD, TK_NEWLINE, to_lower
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use parser_declarations_construction_module, only: add_single_declaration, &
        emit_multi_declaration, &
        handle_complex_initializer
    use parser_declarations_type_spec_support_module, only: type_specifier_t
    use parser_declarations_type_spec_module, only: parse_type_specifier
    use parser_result_types, only: parse_result_t, success_parse_result, &
        error_parse_result
    use error_handling, only: ERROR_PARSER
    use parser_expressions_module, only: parse_comparison
    use parser_declaration_attributes_module, only: parse_declaration_attributes, &
        parse_array_dimensions
    use declaration_attribute_utils, only: declaration_attribute_info_t
    use parser_utilities, only: peek_next_nontrivial_token
    use parser_keyword_disambiguation_module, only: looks_like_implicit_statement
    implicit none
    private

    public :: parse_declaration
    public :: parse_declaration_with_result
    public :: parse_array_dimensions
    public :: skip_declaration_separator

contains

    function parse_declaration(parser, arena) result(decl_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: decl_index

        type(type_specifier_t) :: type_spec
        type(declaration_attribute_info_t) :: attr_info
        type(token_t) :: identifier_token
        integer :: initializer_index
        integer, allocatable :: local_dimension_indices(:)
        logical :: has_local_dimensions
        character(len=:), allocatable :: var_name
        logical :: handled_multi

        decl_index = 0
        type_spec = parse_type_specifier(parser, arena)
        if (.not. allocated(type_spec%type_name)) then
            return
        end if

        call parse_declaration_attributes(parser, arena, attr_info)
        call skip_declaration_separator(parser)

        identifier_token = parser%consume()
        if (identifier_token%kind /= TK_IDENTIFIER) then
            if (.not. try_promote_keyword_identifier(parser, identifier_token)) then
                return
            end if
        end if

        handled_multi = handle_multi_variable_declaration( &
            parser, arena, type_spec, attr_info, identifier_token, &
            decl_index)
        if (handled_multi) then
            return
        end if

        var_name = identifier_token%text
        call parse_variable_dimensions(parser, arena, local_dimension_indices, &
            has_local_dimensions)
        initializer_index = parse_variable_initializer(parser, arena, type_spec)

        if (has_local_dimensions .and. allocated(local_dimension_indices)) then
            decl_index = add_single_declaration( &
                arena, type_spec, attr_info, var_name, initializer_index, &
                .true., local_dimension_indices)
        else
            decl_index = add_single_declaration( &
                arena, type_spec, attr_info, var_name, initializer_index, &
                .false.)
        end if
    end function parse_declaration

    subroutine skip_declaration_separator(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        token = parser%peek()
        if (token%text == "::") then
            token = parser%consume()
        end if

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE) then
                token = parser%consume()
            else
                exit
            end if
        end do
    end subroutine skip_declaration_separator

    logical function handle_multi_variable_declaration(parser, arena, type_spec, &
            attr_info, first_token, &
            decl_index) result(is_multi)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(type_specifier_t), intent(in) :: type_spec
        type(declaration_attribute_info_t), intent(in) :: attr_info
        type(token_t), intent(in) :: first_token
        integer, intent(out) :: decl_index
        character(len=64), allocatable :: var_names(:)
        integer :: var_count
        type(token_t) :: token

        decl_index = 0
        is_multi = .false.

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%text /= ",") then
            return
        end if

        allocate (var_names(10))
        var_names = ""
        var_count = 1
        var_names(1) = trim(first_token%text)

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%text /= ",") then
                exit
            end if

            token = parser%consume()
            if (parser%is_at_end()) then
                exit
            end if

            token = parser%consume()
            if (token%kind /= TK_IDENTIFIER) then
                if (.not. try_promote_keyword_identifier(parser, token)) then
                    exit
                end if
            end if

            var_count = var_count + 1
            if (var_count > size(var_names)) then
                call grow_var_name_buffer(var_names)
            end if
            var_names(var_count) = trim(token%text)
        end do

        decl_index = emit_multi_declaration( &
            arena, type_spec, attr_info, var_names(1:var_count))
        if (decl_index > 0) then
            is_multi = .true.
        end if

        block
            character(len=64), allocatable :: temp(:)
            call move_alloc(var_names, temp)
        end block
    end function handle_multi_variable_declaration

    subroutine grow_var_name_buffer(var_names)
        character(len=64), allocatable, intent(inout) :: var_names(:)
        character(len=64), allocatable :: temp(:)
        integer :: old_size

        old_size = size(var_names)
        allocate (temp(old_size * 2))
        temp = ""
        temp(1:old_size) = var_names
        call move_alloc(temp, var_names)
    end subroutine grow_var_name_buffer


    subroutine parse_variable_dimensions(parser, arena, dimension_indices, &
            has_dimensions)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: dimension_indices(:)
        logical, intent(out) :: has_dimensions
        type(token_t) :: token

        has_dimensions = .false.
        if (allocated(dimension_indices)) then
            block
                integer, allocatable :: temp(:)
                call move_alloc(dimension_indices, temp)
            end block
        end if

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%text /= "(") then
            return
        end if

        token = parser%consume()
        call parse_array_dimensions(parser, arena, dimension_indices)
        if (allocated(dimension_indices)) then
            has_dimensions = size(dimension_indices) > 0
        end if
    end subroutine parse_variable_dimensions

    integer function parse_variable_initializer(parser, arena, type_spec) &
            result(initializer_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(type_specifier_t), intent(in) :: type_spec
        type(token_t) :: token

        initializer_index = 0

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%text == "=" .or. token%text == "=>") then
            token = parser%consume()
            if (type_spec%base_keyword == "complex") then
                initializer_index = handle_complex_initializer( &
                    parser, arena, type_spec%base_keyword)
            else
                initializer_index = parse_comparison(parser, arena)
            end if
        end if
    end function parse_variable_initializer


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

    logical function try_promote_keyword_identifier(parser, token) result(promoted)
        type(parser_state_t), intent(inout) :: parser
        type(token_t), intent(inout) :: token
        type(token_t) :: next_token
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: next_lower
        integer :: index
        type(parser_state_t) :: parser_copy

        promoted = .false.
        if (token%kind /= TK_KEYWORD) then
            return
        end if

        lowered = to_lower(trim(token%text))

        select case (lowered)
        case ("end")
            next_token = peek_next_nontrivial_token(parser)
            if (next_token%kind == TK_KEYWORD) then
                next_lower = to_lower(trim(next_token%text))
                select case (next_lower)
                case ("type", "module", "subroutine", "function", "program", &
                        "interface", "procedure", "select", "if", "do", &
                        "forall", "where", "associate", "block", "team", &
                        "critical", "blockdata")
                    return
                end select
            end if
        case ("double")
            next_token = parser%get_token_at_index(parser%current_token + 1)
            next_lower = to_lower(trim(next_token%text))
            if (trim(next_lower) == "precision") return
        case ("in", "out", "inout", "data")
            ! Contextual keywords can act as identifiers within declarations
        case ("stop", "call", "cycle", "exit", "return", "continue", "goto", &
                "go", "entry", "select", "contains", "else", "dimension", &
                "common", "program", "module", "if", "format", &
                "read", "write", "print", "open", "close", "inquire", &
                "backspace", "rewind", "endfile")
            ! Allow executable keywords in identifier positions
        case ("implicit")
            parser_copy = parser
            if (parser_copy%current_token > 1) then
                parser_copy%current_token = parser_copy%current_token - 1
            end if
            if (looks_like_implicit_statement(parser_copy)) then
                return
            end if
        case default
            return
        end select

        if (.not. associated(parser%tokens)) then
            return
        end if

        index = parser%current_token - 1
        if (index < 1 .or. index > size(parser%tokens)) then
            return
        end if

        parser%tokens(index)%kind = TK_IDENTIFIER
        token%kind = TK_IDENTIFIER
        promoted = .true.
    end function try_promote_keyword_identifier



end module parser_declarations_core_module
