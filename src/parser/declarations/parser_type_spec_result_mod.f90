module parser_type_spec_result_mod
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_OPERATOR, TK_EOF
    use parser_state_module, only: parser_state_t, create_parser_state
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_identifier
    use parser_expressions_module, only: parse_comparison
    use parser_type_spec_tokens_mod, only: append_token, append_int
    use parser_type_spec_tokens_mod, only: trim_token_sequence, strip_outer_parentheses
    use parser_type_spec_tokens_mod, only: is_trivia_token
    implicit none
    private

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
        integer :: kind_selector_index = 0
        integer :: line = 0
        integer :: column = 0
        logical :: has_character_length = .false.
        character(len=:), allocatable :: character_length_expr
    end type type_specifier_t

    public :: type_specifier_t
    public :: clear_derived_type_storage
    public :: initialize_type_specifier
    public :: split_derived_type_name_and_params
    public :: set_derived_type_name_info
    public :: process_derived_type_parameters
    public :: analyze_derived_type_tokens
    public :: parse_single_parameter

contains

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
        type_spec%kind_selector_index = 0
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

    pure function compact_name_text(name_tokens) result(name_text)
        type(token_t), intent(in) :: name_tokens(:)
        character(len=:), allocatable :: name_text
        integer :: i

        name_text = ""
        do i = 1, size(name_tokens)
            if (.not. is_trivia_token(name_tokens(i))) then
                name_text = name_text//trim(name_tokens(i)%text)
            end if
        end do
    end function compact_name_text

    pure subroutine split_module_and_base(name_text, module_name, base_name)
        character(len=*), intent(in) :: name_text
        character(len=:), allocatable, intent(out) :: module_name
        character(len=:), allocatable, intent(out) :: base_name
        integer :: last_sep
        integer :: i

        module_name = ""
        base_name = ""
        last_sep = 0

        do i = 1, len(name_text) - 1
            if (name_text(i:i + 1) == "::") last_sep = i
        end do

        if (last_sep > 0) then
            module_name = trim(adjustl(name_text(:last_sep - 1)))
            base_name = trim(adjustl(name_text(last_sep + 2:)))
        else
            base_name = trim(adjustl(name_text))
        end if
    end subroutine split_module_and_base

    subroutine find_identifier_reference(name_tokens, line_ref, column_ref, &
            found_identifier)
        type(token_t), intent(in) :: name_tokens(:)
        integer, intent(inout) :: line_ref
        integer, intent(inout) :: column_ref
        logical, intent(out) :: found_identifier
        integer :: i

        found_identifier = .false.
        do i = size(name_tokens), 1, -1
            if (name_tokens(i)%kind == TK_IDENTIFIER) then
                line_ref = name_tokens(i)%line
                column_ref = name_tokens(i)%column
                found_identifier = .true.
                exit
            end if
        end do
    end subroutine find_identifier_reference

    subroutine set_derived_type_name_info(type_spec, name_tokens, arena)
        type(type_specifier_t), intent(inout) :: type_spec
        type(token_t), allocatable, intent(in) :: name_tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        character(len=:), allocatable :: module_name
        character(len=:), allocatable :: base_name
        character(len=:), allocatable :: name_text
        integer :: line_ref
        integer :: column_ref
        logical :: found_identifier

        line_ref = type_spec%line
        column_ref = type_spec%column
        if (.not. allocated(name_tokens)) then
            type_spec%is_derived_type = .false.
            type_spec%derived_type_name = ""
            type_spec%derived_type_module = ""
            return
        end if

        name_text = compact_name_text(name_tokens)
        if (len(name_text) == 0) then
            type_spec%derived_type_name = ""
            type_spec%derived_type_module = ""
            return
        end if

        call split_module_and_base(name_text, module_name, base_name)
        call find_identifier_reference(name_tokens, line_ref, column_ref, &
            found_identifier)

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

    subroutine parse_single_parameter(current, type_spec, arena, parent_parser)
        type(token_t), allocatable, intent(in) :: current(:)
        type(type_specifier_t), intent(inout) :: type_spec
        type(ast_arena_t), intent(inout) :: arena
        type(parser_state_t), intent(in) :: parent_parser
        type(token_t), allocatable :: cleaned(:)
        type(token_t), allocatable :: parser_tokens(:)
        type(token_t) :: eof_token
        type(parser_state_t) :: param_parser
        integer :: expr_index
        if (.not. allocated(current)) return
        call trim_token_sequence(current, cleaned)
        if (.not. allocated(cleaned)) return

        allocate (parser_tokens(size(cleaned) + 1))
        parser_tokens(1:size(cleaned)) = cleaned
        eof_token%kind = TK_EOF
        eof_token%text = ""
        parser_tokens(size(cleaned) + 1) = eof_token

        param_parser = create_parser_state(parser_tokens)
        if (associated(parent_parser%diagnostic_sink)) then
            param_parser%diagnostic_sink => parent_parser%diagnostic_sink
        end if
        expr_index = parse_comparison(param_parser, arena)
        if (expr_index > 0) then
            call append_int(type_spec%derived_parameter_nodes, expr_index)
        end if

        block
            type(token_t), allocatable :: temp(:)
            if (allocated(parser_tokens)) call move_alloc(parser_tokens, temp)
            if (allocated(cleaned)) call move_alloc(cleaned, temp)
        end block
    end subroutine parse_single_parameter

    subroutine split_parameters(working, type_spec, arena, parent_parser)
        type(token_t), allocatable, intent(in) :: working(:)
        type(type_specifier_t), intent(inout) :: type_spec
        type(ast_arena_t), intent(inout) :: arena
        type(parser_state_t), intent(in) :: parent_parser
        type(token_t), allocatable :: current(:)
        integer :: depth
        integer :: i

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
                        call parse_single_parameter(current, type_spec, arena, &
                            parent_parser)
                        if (allocated(current)) then
                            block
                                type(token_t), allocatable :: temp(:)
                                call move_alloc(current, temp)
                            end block
                        end if
                        cycle
                    end if
                end select
            end if
            call append_token(current, working(i))
        end do
        call parse_single_parameter(current, type_spec, arena, parent_parser)
        if (allocated(current)) then
            block
                type(token_t), allocatable :: temp(:)
                call move_alloc(current, temp)
            end block
        end if
    end subroutine split_parameters

    subroutine process_derived_type_parameters(type_spec, param_tokens, arena, &
            parent_parser)
        type(type_specifier_t), intent(inout) :: type_spec
        type(token_t), allocatable, intent(in) :: param_tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        type(parser_state_t), intent(in) :: parent_parser
        type(token_t), allocatable :: working(:)
        type(token_t), allocatable :: trimmed_working(:)

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

        call split_parameters(working, type_spec, arena, parent_parser)

        if (allocated(type_spec%derived_parameter_nodes)) then
            type_spec%has_derived_type_parameters = &
                (size(type_spec%derived_parameter_nodes) > 0)
        else
            type_spec%has_derived_type_parameters = .false.
        end if
    end subroutine process_derived_type_parameters

    subroutine analyze_derived_type_tokens(type_spec, tokens, arena, parent_parser)
        type(type_specifier_t), intent(inout) :: type_spec
        type(token_t), allocatable, intent(in) :: tokens(:)
        type(ast_arena_t), intent(inout) :: arena
        type(parser_state_t), intent(in) :: parent_parser
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
        call process_derived_type_parameters(type_spec, param_tokens, arena, &
            parent_parser)

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

end module parser_type_spec_result_mod
