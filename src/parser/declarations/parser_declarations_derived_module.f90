module parser_declarations_derived_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD, TK_NEWLINE, &
        TK_WHITESPACE, TK_COMMENT, TK_OPERATOR
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_derived_type, push_type_binding
    use parser_declarations_type_spec_support_module, only: &
        skip_type_definition_attributes
    use parser_type_spec_attributes_mod, only: extract_extends_from_attributes
    use parser_declarations_core_module, only: parse_declaration
    use parser_submodule_placement_module, only: reject_misplaced_submodule
    use string_utils_mod, only: to_lower
    use string_types, only: string_t
    use ast_nodes_data, only: derived_type_node, PARAM_KIND, PARAM_LEN, &
        PARAM_UNKNOWN
    use parser_expressions_module, only: parse_comparison
    implicit none
    private

    public :: parse_derived_type_def
    public :: parse_derived_type_component

contains

    function parse_derived_type_def(parser, arena) result(type_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: type_index

        character(len=100) :: type_name
        character(len=:), allocatable :: header_attributes
        character(len=:), allocatable :: extends_parent
        character(len=:), allocatable :: remaining_attrs
        logical :: has_header_attrs
        logical :: invalid_type_spec
        integer, allocatable :: component_indices(:)
        integer :: component_count
        integer, allocatable :: binding_indices(:)
        integer :: binding_count
        type(string_t), allocatable :: param_names(:)
        type(string_t), allocatable :: tp_names(:)
        integer, allocatable :: tp_classes(:)
        integer, allocatable :: tp_defaults(:)

        type_index = 0

        call parse_type_definition_header(parser, type_name, header_attributes, &
            has_header_attrs, invalid_type_spec, param_names)
        if (invalid_type_spec) then
            return
        end if

        if (has_header_attrs .and. allocated(header_attributes)) then
            call extract_extends_from_attributes(header_attributes, &
                extends_parent, remaining_attrs)
            if (allocated(remaining_attrs)) then
                if (len_trim(remaining_attrs) > 0) then
                    call move_alloc(remaining_attrs, header_attributes)
                else
                    has_header_attrs = .false.
                    block
                        character(len=:), allocatable :: temp
                        call move_alloc(header_attributes, temp)
                        call move_alloc(remaining_attrs, temp)
                    end block
                end if
            end if
        end if

        call collect_derived_type_components(parser, arena, component_indices, &
            component_count, binding_indices, &
            binding_count, tp_names, tp_classes, tp_defaults)
        type_index = finalize_derived_type(arena, type_name, header_attributes, &
            has_header_attrs, component_indices, &
            component_count, binding_indices, &
            binding_count, extends_parent)
        call attach_type_parameters(arena, type_index, param_names, tp_names, &
            tp_classes, tp_defaults)
    end function parse_derived_type_def

    ! Record the derived-type parameter formals on the pushed node: the header
    ! fixes the order, the body supplies the KIND/LEN class and the default.
    subroutine attach_type_parameters(arena, type_index, param_names, tp_names, &
            tp_classes, tp_defaults)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: type_index
        type(string_t), allocatable, intent(in) :: param_names(:)
        type(string_t), allocatable, intent(in) :: tp_names(:)
        integer, allocatable, intent(in) :: tp_classes(:)
        integer, allocatable, intent(in) :: tp_defaults(:)
        type(string_t), allocatable :: ordered_names(:)
        integer, allocatable :: classes(:)
        integer, allocatable :: defaults(:)
        integer :: i, j

        if (type_index <= 0) return
        if (.not. arena%has_node_at(type_index)) return
        if (.not. allocated(param_names)) return
        if (.not. allocated(tp_names)) return

        if (size(param_names) > 0) then
            ordered_names = param_names
        else if (size(tp_names) > 0) then
            ordered_names = tp_names
        else
            return
        end if

        allocate (classes(size(ordered_names)))
        allocate (defaults(size(ordered_names)))
        classes = PARAM_UNKNOWN
        defaults = 0

        do i = 1, size(ordered_names)
            do j = 1, size(tp_names)
                if (to_lower(ordered_names(i)%s) /= to_lower(tp_names(j)%s)) cycle
                classes(i) = tp_classes(j)
                defaults(i) = tp_defaults(j)
                exit
            end do
        end do

        select type (node => arena%entries(type_index)%node)
            type is (derived_type_node)
            node%has_parameters = .true.
            node%param_names = ordered_names
            node%param_classes = classes
            node%param_defaults = defaults
        end select
    end subroutine attach_type_parameters

    subroutine parse_type_definition_header(parser, type_name, &
            header_attributes, &
            has_header_attrs, invalid_type_spec, param_names)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(out) :: type_name
        character(len=:), allocatable, intent(out) :: header_attributes
        logical, intent(out) :: has_header_attrs
        logical, intent(out) :: invalid_type_spec
        type(string_t), allocatable, intent(out) :: param_names(:)
        type(token_t) :: token

        allocate (param_names(0))

        type_name = ""
        has_header_attrs = .false.
        invalid_type_spec = .false.

        token = parser%consume()
        call skip_type_definition_attributes(parser, invalid_type_spec, &
            header_attributes)
        if (invalid_type_spec) then
            return
        end if

        if (allocated(header_attributes)) then
            if (len_trim(header_attributes) > 0) then
                has_header_attrs = .true.
            else
                block
                    character(len=:), allocatable :: temp
                    call move_alloc(header_attributes, temp)
                end block
            end if
        end if

        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER) then
            invalid_type_spec = .true.
            return
        end if

        token = parser%consume()
        type_name = trim(token%text)

        call parse_type_parameter_names(parser, param_names)
        call skip_type_header_trivia(parser)
    end subroutine parse_type_definition_header

    ! F2018 R728: parse the type-param-name-list of a parameterized derived
    ! type header, e.g. `type :: box_t(n, k)`.
    subroutine parse_type_parameter_names(parser, param_names)
        type(parser_state_t), intent(inout) :: parser
        type(string_t), allocatable, intent(inout) :: param_names(:)
        type(token_t) :: token

        call skip_inline_trivia(parser)
        token = parser%peek()
        if (token%kind /= TK_OPERATOR) return
        if (token%text /= "(") return
        token = parser%consume()

        call skip_inline_trivia(parser)
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ")") then
            call parser%error_at_token( &
                "A type parameter list must contain at least one parameter", &
                token)
            token = parser%consume()
            return
        end if

        do while (.not. parser%is_at_end())
            call skip_inline_trivia(parser)
            token = parser%peek()
            if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
                token = parser%consume()
                call append_name(param_names, trim(token%text))
            else if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            else
                exit
            end if

            call skip_inline_trivia(parser)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
            else if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                exit
            else
                exit
            end if
        end do
    end subroutine parse_type_parameter_names

    subroutine skip_inline_trivia(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_WHITESPACE .or. token%kind == TK_COMMENT) then
                token = parser%consume()
            else
                exit
            end if
        end do
    end subroutine skip_inline_trivia

    subroutine append_name(names, name)
        type(string_t), allocatable, intent(inout) :: names(:)
        character(len=*), intent(in) :: name
        type(string_t), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(names)) allocate (names(0))
        n = size(names)
        allocate (grown(n + 1))
        if (n > 0) grown(1:n) = names
        grown(n + 1)%s = name
        call move_alloc(grown, names)
    end subroutine append_name

    subroutine append_int_value(values, value)
        integer, allocatable, intent(inout) :: values(:)
        integer, intent(in) :: value
        integer, allocatable :: grown(:)
        integer :: n

        if (.not. allocated(values)) allocate (values(0))
        n = size(values)
        allocate (grown(n + 1))
        if (n > 0) grown(1:n) = values
        grown(n + 1) = value
        call move_alloc(grown, values)
    end subroutine append_int_value

    subroutine skip_type_header_trivia(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

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
    end subroutine skip_type_header_trivia

    subroutine collect_derived_type_components(parser, arena, component_indices, &
            component_count, binding_indices, &
            binding_count, tp_names, tp_classes, tp_defaults)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: component_indices(:)
        integer, intent(out) :: component_count
        integer, allocatable, intent(out) :: binding_indices(:)
        integer, intent(out) :: binding_count
        type(string_t), allocatable, intent(out) :: tp_names(:)
        integer, allocatable, intent(out) :: tp_classes(:)
        integer, allocatable, intent(out) :: tp_defaults(:)
        integer, allocatable :: indices(:)
        integer, allocatable :: bind_indices(:)
        integer :: capacity
        integer :: bind_capacity
        integer :: comp_index
        integer :: bind_index
        logical :: in_contains

        capacity = 8
        allocate (indices(capacity))
        component_count = 0
        bind_capacity = 4
        allocate (bind_indices(bind_capacity))
        binding_count = 0
        in_contains = .false.
        allocate (tp_names(0))
        allocate (tp_classes(0))
        allocate (tp_defaults(0))

        do while (.not. parser%is_at_end())
            if (end_type_ahead(parser)) then
                call consume_end_type_sequence(parser)
                exit
            end if

            ! A submodule is a program unit and cannot appear in a type.
            if (reject_misplaced_submodule(parser, &
                "a derived-type definition")) then
                call skip_component_trivia(parser)
                cycle
            end if

            if (contains_ahead(parser)) then
                call consume_contains_keyword(parser)
                in_contains = .true.
                call skip_component_trivia(parser)
                cycle
            end if

            if (.not. in_contains) then
                if (type_parameter_decl_ahead(parser)) then
                    call parse_type_parameter_declaration(parser, arena, &
                        tp_names, tp_classes, tp_defaults)
                    call skip_component_trivia(parser)
                    cycle
                end if
            end if

            if (in_contains) then
                bind_index = parse_type_bound_procedure(parser, arena)
                if (bind_index > 0) then
                    binding_count = binding_count + 1
                    if (binding_count > bind_capacity) then
                        call expand_binding_storage(bind_indices, bind_capacity)
                    end if
                    bind_indices(binding_count) = bind_index
                    call skip_component_trivia(parser)
                else
                    call skip_failed_component(parser)
                end if
            else
                comp_index = parse_derived_type_component(parser, arena)
                if (comp_index > 0) then
                    component_count = component_count + 1
                    if (component_count > capacity) then
                        call expand_component_storage(indices, capacity)
                    end if
                    indices(component_count) = comp_index
                    call skip_component_trivia(parser)
                else
                    call skip_failed_component(parser)
                end if
            end if
        end do

        call finalize_component_storage(indices, component_count, &
            component_indices)
        call finalize_binding_storage(bind_indices, binding_count, &
            binding_indices)
    end subroutine collect_derived_type_components

    ! F2018 R731: a type-param-def-stmt is an INTEGER declaration carrying the
    ! KIND or LEN attribute. Detect it before the generic component parser,
    ! which has no representation for those attributes.
    logical function type_parameter_decl_ahead(parser) result(is_param_decl)
        type(parser_state_t), intent(inout) :: parser
        integer :: i
        integer :: depth
        character(len=:), allocatable :: lowered

        is_param_decl = .false.
        depth = 0

        i = parser%current_token
        do while (i <= size(parser%tokens))
            if (parser%tokens(i)%kind == TK_WHITESPACE) then
                i = i + 1
                cycle
            end if
            exit
        end do
        if (i > size(parser%tokens)) return
        lowered = to_lower(trim(parser%tokens(i)%text))
        if (lowered /= "integer") return

        i = i + 1
        do while (i <= size(parser%tokens))
            if (parser%tokens(i)%kind == TK_NEWLINE) exit
            lowered = to_lower(trim(parser%tokens(i)%text))
            if (lowered == "(") then
                depth = depth + 1
            else if (lowered == ")") then
                depth = depth - 1
            else if (lowered == "::") then
                exit
            else if (depth == 0) then
                if (lowered == "kind" .or. lowered == "len") then
                    is_param_decl = .true.
                    exit
                end if
            end if
            i = i + 1
        end do
    end function type_parameter_decl_ahead

    ! Parse `integer, kind :: k = 4` / `integer, len :: n` inside a derived
    ! type body, recording the KIND/LEN classification and default value.
    subroutine parse_type_parameter_declaration(parser, arena, tp_names, &
            tp_classes, tp_defaults)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(string_t), allocatable, intent(inout) :: tp_names(:)
        integer, allocatable, intent(inout) :: tp_classes(:)
        integer, allocatable, intent(inout) :: tp_defaults(:)
        type(token_t) :: token
        character(len=:), allocatable :: lowered
        integer :: classification
        integer :: default_index

        classification = PARAM_UNKNOWN

        call skip_inline_trivia(parser)
        token = parser%consume() ! consume 'integer'

        do while (.not. parser%is_at_end())
            call skip_inline_trivia(parser)
            token = parser%peek()
            if (.not. (token%kind == TK_OPERATOR .and. token%text == ",")) exit
            token = parser%consume()
            call skip_inline_trivia(parser)
            token = parser%peek()
            lowered = to_lower(trim(token%text))
            if (lowered == "kind") then
                classification = PARAM_KIND
                token = parser%consume()
            else if (lowered == "len") then
                classification = PARAM_LEN
                token = parser%consume()
            else
                token = parser%consume()
            end if
        end do

        call skip_inline_trivia(parser)
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "::") then
            token = parser%consume()
        end if

        do while (.not. parser%is_at_end())
            call skip_inline_trivia(parser)
            token = parser%peek()
            if (token%kind /= TK_IDENTIFIER .and. token%kind /= TK_KEYWORD) exit
            token = parser%consume()
            default_index = 0

            call skip_inline_trivia(parser)
            if (.not. parser%is_at_end()) then
                block
                    type(token_t) :: next_token
                    next_token = parser%peek()
                    if (next_token%kind == TK_OPERATOR) then
                        if (next_token%text == "=") then
                            next_token = parser%consume()
                            default_index = parse_comparison(parser, arena)
                        end if
                    end if
                end block
            end if

            call append_name(tp_names, trim(token%text))
            call append_int_value(tp_classes, classification)
            call append_int_value(tp_defaults, max(default_index, 0))

            call skip_inline_trivia(parser)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
            else
                exit
            end if
        end do
    end subroutine parse_type_parameter_declaration

    logical function end_type_ahead(parser) result(is_end)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        character(len=:), allocatable :: lowered

        is_end = .false.
        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
            lowered = to_lower(trim(token%text))
            if (lowered == "end" .or. lowered == "endtype") then
                is_end = .true.
            end if
        end if
    end function end_type_ahead

    subroutine consume_end_type_sequence(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        character(len=:), allocatable :: lowered

        if (parser%is_at_end()) then
            return
        end if

        token = parser%consume()
        lowered = to_lower(trim(token%text))

        ! Check if we consumed endtype (single keyword) - done
        if (lowered == "endtype") then
            ! Optionally consume type name
            if (.not. parser%is_at_end()) then
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
                    token = parser%consume()
                end if
            end if
            return
        end if

        ! Otherwise, we consumed end, now look for type
        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        lowered = to_lower(trim(token%text))
        if (lowered == "type") then
            token = parser%consume()
            if (.not. parser%is_at_end()) then
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
                    token = parser%consume()
                end if
            end if
        end if
    end subroutine consume_end_type_sequence

    subroutine expand_component_storage(indices, capacity)
        integer, allocatable, intent(inout) :: indices(:)
        integer, intent(inout) :: capacity
        integer, allocatable :: temp(:)
        integer :: new_capacity

        new_capacity = capacity * 2
        allocate (temp(new_capacity))
        temp = 0
        temp(1:capacity) = indices(1:capacity)
        call move_alloc(temp, indices)
        capacity = new_capacity
    end subroutine expand_component_storage

    subroutine skip_component_trivia(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE) then
                token = parser%consume()
            else if (token%kind == TK_WHITESPACE .or. token%kind == TK_COMMENT) then
                token = parser%consume()
            else
                exit
            end if
        end do
    end subroutine skip_component_trivia

    subroutine skip_failed_component(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if ((token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) .and. &
            to_lower(trim(token%text)) == "end") then
            return
        end if

        if (token%kind == TK_NEWLINE) then
            token = parser%consume()
        else if (token%kind == TK_WHITESPACE .or. token%kind == TK_COMMENT) then
            token = parser%consume()
        else
            token = parser%consume()
        end if
    end subroutine skip_failed_component

    subroutine finalize_component_storage(indices, component_count, &
            component_indices)
        integer, allocatable, intent(inout) :: indices(:)
        integer, intent(in) :: component_count
        integer, allocatable, intent(out) :: component_indices(:)
        integer, allocatable :: temp(:)

        if (component_count <= 0) then
            allocate (temp(0))
            call move_alloc(temp, component_indices)
        else
            allocate (temp(component_count))
            temp = indices(1:component_count)
            call move_alloc(temp, component_indices)
        end if

        block
            integer, allocatable :: discard(:)
            call move_alloc(indices, discard)
        end block
    end subroutine finalize_component_storage

    logical function contains_ahead(parser) result(is_contains)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token
        character(len=:), allocatable :: keyword

        is_contains = .false.
        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
            keyword = to_lower(trim(token%text))
            if (keyword == "contains") then
                is_contains = .true.
            end if
        end if
    end function contains_ahead

    subroutine consume_contains_keyword(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        if (parser%is_at_end()) return
        token = parser%consume()
    end subroutine consume_contains_keyword

    subroutine expand_binding_storage(indices, capacity)
        integer, allocatable, intent(inout) :: indices(:)
        integer, intent(inout) :: capacity
        integer, allocatable :: temp(:)
        integer :: new_capacity

        new_capacity = capacity * 2
        allocate (temp(new_capacity))
        temp = 0
        temp(1:capacity) = indices(1:capacity)
        call move_alloc(temp, indices)
        capacity = new_capacity
    end subroutine expand_binding_storage

    subroutine finalize_binding_storage(indices, binding_count, &
            binding_indices)
        integer, allocatable, intent(inout) :: indices(:)
        integer, intent(in) :: binding_count
        integer, allocatable, intent(out) :: binding_indices(:)
        integer, allocatable :: temp(:)

        if (binding_count <= 0) then
            allocate (temp(0))
            call move_alloc(temp, binding_indices)
        else
            allocate (temp(binding_count))
            temp = indices(1:binding_count)
            call move_alloc(temp, binding_indices)
        end if

        block
            integer, allocatable :: discard(:)
            call move_alloc(indices, discard)
        end block
    end subroutine finalize_binding_storage

    logical function read_binding_keyword(parser, line, column, is_generic, &
            is_final) result(found_keyword)
        type(parser_state_t), intent(inout) :: parser
        integer, intent(out) :: line
        integer, intent(out) :: column
        logical, intent(inout) :: is_generic
        logical, intent(inout) :: is_final
        type(token_t) :: token
        character(len=:), allocatable :: keyword

        call skip_component_trivia(parser)
        found_keyword = .false.
        line = 0
        column = 0

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER .and. token%kind /= TK_KEYWORD) then
            return
        end if

        keyword = to_lower(trim(token%text))
        if (keyword /= "procedure" .and. keyword /= "generic" .and. &
            keyword /= "final") then
            return
        end if

        token = parser%consume()
        line = token%line
        column = token%column

        is_generic = keyword == "generic"
        is_final = keyword == "final"
        found_keyword = .true.
    end function read_binding_keyword

    logical function consume_binding_prefix(parser, is_deferred, pass_arg, &
            accessibility, pass_name) result(found_prefix)
        type(parser_state_t), intent(inout) :: parser
        logical, intent(inout) :: is_deferred
        logical, intent(inout) :: pass_arg
        character(len=:), allocatable, intent(inout) :: accessibility
        character(len=:), allocatable, intent(inout), optional :: pass_name
        type(token_t) :: token
        character(len=:), allocatable :: attr

        found_prefix = .false.

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR) then
                select case (token%text)
                case ("::")
                    token = parser%consume()
                    found_prefix = .true.
                    exit
                case (",", "(", ")", "=>")
                    token = parser%consume()
                case default
                    token = parser%consume()
                end select
            else if (token%kind == TK_NEWLINE .or. token%kind == TK_WHITESPACE .or. &
                    token%kind == TK_COMMENT) then
                token = parser%consume()
            else if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
                attr = to_lower(trim(token%text))
                if (attr == "deferred") then
                    is_deferred = .true.
                    token = parser%consume()
                else if (attr == "nopass") then
                    pass_arg = .false.
                    token = parser%consume()
                else if (attr == "pass") then
                    ! Consume pass; capture the dummy name in pass(arg).
                    token = parser%consume()
                    token = parser%peek()
                    if (token%kind == TK_OPERATOR .and. token%text == "(") then
                        token = parser%consume()
                        token = parser%peek()
                        if (token%kind == TK_IDENTIFIER .or. &
                            token%kind == TK_KEYWORD) then
                            if (present(pass_name)) &
                                pass_name = to_lower(trim(token%text))
                            token = parser%consume()
                        end if
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == ")") &
                            token = parser%consume()
                    end if
                else if (attr == "public" .or. attr == "private") then
                    accessibility = attr
                    token = parser%consume()
                else
                    ! Not a known attribute - this is likely the binding name
                    ! Don't consume it, just exit with found_prefix = true
                    ! to allow read_binding_name to handle it
                    found_prefix = .true.
                    exit
                end if
            else
                token = parser%consume()
            end if
        end do
    end function consume_binding_prefix

    logical function read_binding_name(parser, binding_name) result(found_name)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: binding_name
        type(token_t) :: token

        call skip_component_trivia(parser)
        found_name = .false.

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER) then
            return
        end if

        token = parser%consume()
        binding_name = trim(token%text)
        found_name = .true.
    end function read_binding_name

    subroutine read_binding_target(parser, implementation)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: implementation
        type(token_t) :: token

        call skip_component_trivia(parser)

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (.not. (token%kind == TK_OPERATOR .and. token%text == "=>")) then
            return
        end if

        token = parser%consume()
        call skip_component_trivia(parser)

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            implementation = trim(token%text)
        end if
    end subroutine read_binding_target

    subroutine read_generic_target_list(parser, generic_list, count)
        use string_types, only: string_t
        type(parser_state_t), intent(inout) :: parser
        type(string_t), allocatable, intent(out) :: generic_list(:)
        integer, intent(out) :: count
        type(token_t) :: token
        type(string_t), allocatable :: temp_list(:)
        type(string_t), allocatable :: old_list(:)
        integer :: capacity

        count = 0
        capacity = 4
        allocate (temp_list(capacity))

        call skip_component_trivia(parser)

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (.not. (token%kind == TK_OPERATOR .and. token%text == "=>")) then
            return
        end if

        token = parser%consume()
        call skip_component_trivia(parser)

        do
            if (parser%is_at_end()) exit

            token = parser%peek()
            if (token%kind == TK_IDENTIFIER) then
                token = parser%consume()
                count = count + 1
                if (count > capacity) then
                    capacity = capacity * 2
                    allocate (old_list(count - 1))
                    old_list = temp_list(1:count - 1)
                    deallocate (temp_list)
                    allocate (temp_list(capacity))
                    temp_list(1:count - 1) = old_list
                    deallocate (old_list)
                end if
                temp_list(count)%s = trim(token%text)
            else
                exit
            end if

            call skip_component_trivia(parser)
            if (parser%is_at_end()) exit

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
                call skip_component_trivia(parser)
            else
                exit
            end if
        end do

        if (count > 0) then
            allocate (generic_list(count))
            generic_list = temp_list(1:count)
        end if
        deallocate (temp_list)
    end subroutine read_generic_target_list

    subroutine read_interface_name(parser, interface_name)
        type(parser_state_t), intent(inout) :: parser
        character(len=:), allocatable, intent(out) :: interface_name
        type(token_t) :: token

        call skip_component_trivia(parser)
        if (parser%is_at_end()) return

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") return

        token = parser%consume()
        call skip_component_trivia(parser)

        if (parser%is_at_end()) return
        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER) return

        token = parser%consume()
        interface_name = trim(token%text)

        call skip_component_trivia(parser)
        if (parser%is_at_end()) return

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ")") then
            token = parser%consume()
        end if
    end subroutine read_interface_name

    function parse_type_bound_procedure(parser, arena) result(binding_index)
        use string_types, only: string_t
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: binding_index
        character(len=:), allocatable :: binding_name
        character(len=:), allocatable :: implementation
        character(len=:), allocatable :: interface_name
        character(len=:), allocatable :: accessibility
        type(string_t), allocatable :: generic_list(:)
        integer :: generic_count
        logical :: is_generic
        logical :: is_final
        logical :: is_deferred
        logical :: pass_arg
        character(len=:), allocatable :: pass_name
        integer :: line
        integer :: column
        binding_index = 0
        is_generic = .false.
        is_final = .false.
        is_deferred = .false.
        pass_arg = .true.
        generic_count = 0

        if (.not. read_binding_keyword(parser, line, column, is_generic, &
            is_final)) return
        call read_interface_name(parser, interface_name)
        if (.not. consume_binding_prefix(parser, is_deferred, pass_arg, &
            accessibility, pass_name)) return
        if (.not. read_binding_name(parser, binding_name)) return
        if (is_generic) then
            call read_generic_target_list(parser, generic_list, generic_count)
        else
            call read_binding_target(parser, implementation)
        end if
        if (is_generic .and. generic_count > 0) then
            if (allocated(accessibility)) then
                if (allocated(interface_name)) then
                    binding_index = push_type_binding( &
                        arena, binding_name, &
                        interface_name=interface_name, &
                        is_generic=is_generic, is_final=is_final, &
                        is_deferred=is_deferred, pass_arg=pass_arg, &
                        pass_name=pass_name, &
                        accessibility=accessibility, &
                        generic_list=generic_list, &
                        line=line, column=column)
                else
                    binding_index = push_type_binding( &
                        arena, binding_name, &
                        is_generic=is_generic, is_final=is_final, &
                        is_deferred=is_deferred, pass_arg=pass_arg, &
                        pass_name=pass_name, &
                        accessibility=accessibility, &
                        generic_list=generic_list, &
                        line=line, column=column)
                end if
            else
                if (allocated(interface_name)) then
                    binding_index = push_type_binding( &
                        arena, binding_name, &
                        interface_name=interface_name, &
                        is_generic=is_generic, is_final=is_final, &
                        is_deferred=is_deferred, pass_arg=pass_arg, &
                        pass_name=pass_name, &
                        generic_list=generic_list, &
                        line=line, column=column)
                else
                    binding_index = push_type_binding( &
                        arena, binding_name, &
                        is_generic=is_generic, is_final=is_final, &
                        is_deferred=is_deferred, pass_arg=pass_arg, &
                        pass_name=pass_name, &
                        generic_list=generic_list, &
                        line=line, column=column)
                end if
            end if
        else if (allocated(accessibility)) then
            if (allocated(implementation)) then
                if (allocated(interface_name)) then
                    binding_index = push_type_binding( &
                        arena, binding_name, &
                        implementation=implementation, &
                        interface_name=interface_name, &
                        is_generic=is_generic, is_final=is_final, &
                        is_deferred=is_deferred, pass_arg=pass_arg, &
                        pass_name=pass_name, &
                        accessibility=accessibility, &
                        line=line, column=column)
                else
                    binding_index = push_type_binding( &
                        arena, binding_name, &
                        implementation=implementation, &
                        is_generic=is_generic, is_final=is_final, &
                        is_deferred=is_deferred, pass_arg=pass_arg, &
                        pass_name=pass_name, &
                        accessibility=accessibility, &
                        line=line, column=column)
                end if
            else
                if (allocated(interface_name)) then
                    binding_index = push_type_binding( &
                        arena, binding_name, &
                        interface_name=interface_name, &
                        is_generic=is_generic, is_final=is_final, &
                        is_deferred=is_deferred, pass_arg=pass_arg, &
                        pass_name=pass_name, &
                        accessibility=accessibility, &
                        line=line, column=column)
                else
                    binding_index = push_type_binding( &
                        arena, binding_name, &
                        is_generic=is_generic, is_final=is_final, &
                        is_deferred=is_deferred, pass_arg=pass_arg, &
                        pass_name=pass_name, &
                        accessibility=accessibility, &
                        line=line, column=column)
                end if
            end if
        else if (allocated(implementation)) then
            if (allocated(interface_name)) then
                binding_index = push_type_binding( &
                    arena, binding_name, &
                    implementation=implementation, &
                    interface_name=interface_name, &
                    is_generic=is_generic, is_final=is_final, &
                    is_deferred=is_deferred, pass_arg=pass_arg, &
                    pass_name=pass_name, &
                    line=line, column=column)
            else
                binding_index = push_type_binding( &
                    arena, binding_name, &
                    implementation=implementation, &
                    is_generic=is_generic, is_final=is_final, &
                    is_deferred=is_deferred, pass_arg=pass_arg, &
                    pass_name=pass_name, &
                    line=line, column=column)
            end if
        else
            if (allocated(interface_name)) then
                binding_index = push_type_binding( &
                    arena, binding_name, &
                    interface_name=interface_name, &
                    is_generic=is_generic, is_final=is_final, &
                    is_deferred=is_deferred, pass_arg=pass_arg, &
                    pass_name=pass_name, &
                    line=line, column=column)
            else
                binding_index = push_type_binding( &
                    arena, binding_name, &
                    is_generic=is_generic, is_final=is_final, &
                    is_deferred=is_deferred, pass_arg=pass_arg, &
                    pass_name=pass_name, &
                    line=line, column=column)
            end if
        end if
    end function parse_type_bound_procedure

    integer function finalize_derived_type(arena, type_name, header_attributes, &
            has_header_attrs, component_indices, &
            component_count, binding_indices, &
            binding_count, extends_parent) &
            result(type_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable, intent(in) :: header_attributes
        logical, intent(in) :: has_header_attrs
        integer, intent(in) :: component_indices(:)
        integer, intent(in) :: component_count
        integer, intent(in) :: binding_indices(:)
        integer, intent(in) :: binding_count
        character(len=:), allocatable, intent(in), optional :: extends_parent
        logical :: has_extends

        has_extends = present(extends_parent)
        if (has_extends) then
            if (.not. allocated(extends_parent)) has_extends = .false.
            if (has_extends) then
                if (len_trim(extends_parent) == 0) has_extends = .false.
            end if
        end if

        if (component_count > 0 .and. binding_count > 0) then
            if (has_header_attrs .and. has_extends) then
                type_index = push_derived_type( &
                    arena, type_name, component_indices, &
                    attribute_clause=header_attributes, &
                    binding_indices=binding_indices, &
                    extends_parent=extends_parent)
            else if (has_header_attrs) then
                type_index = push_derived_type( &
                    arena, type_name, component_indices, &
                    attribute_clause=header_attributes, &
                    binding_indices=binding_indices)
            else if (has_extends) then
                type_index = push_derived_type( &
                    arena, type_name, component_indices, &
                    binding_indices=binding_indices, &
                    extends_parent=extends_parent)
            else
                type_index = push_derived_type(arena, type_name, component_indices, &
                    binding_indices=binding_indices)
            end if
        else if (component_count > 0) then
            if (has_header_attrs .and. has_extends) then
                type_index = push_derived_type( &
                    arena, type_name, component_indices, &
                    attribute_clause=header_attributes, &
                    extends_parent=extends_parent)
            else if (has_header_attrs) then
                type_index = push_derived_type( &
                    arena, type_name, component_indices, &
                    attribute_clause=header_attributes)
            else if (has_extends) then
                type_index = push_derived_type( &
                    arena, type_name, component_indices, &
                    extends_parent=extends_parent)
            else
                type_index = push_derived_type(arena, type_name, component_indices)
            end if
        else if (binding_count > 0) then
            if (has_header_attrs .and. has_extends) then
                type_index = push_derived_type( &
                    arena, type_name, [integer ::], &
                    attribute_clause=header_attributes, &
                    binding_indices=binding_indices, &
                    extends_parent=extends_parent)
            else if (has_header_attrs) then
                type_index = push_derived_type( &
                    arena, type_name, [integer ::], &
                    attribute_clause=header_attributes, &
                    binding_indices=binding_indices)
            else if (has_extends) then
                type_index = push_derived_type( &
                    arena, type_name, [integer ::], &
                    binding_indices=binding_indices, &
                    extends_parent=extends_parent)
            else
                type_index = push_derived_type(arena, type_name, [integer ::], &
                    binding_indices=binding_indices)
            end if
        else
            if (has_header_attrs .and. has_extends) then
                type_index = push_derived_type( &
                    arena, type_name, [integer ::], &
                    attribute_clause=header_attributes, &
                    extends_parent=extends_parent)
            else if (has_header_attrs) then
                type_index = push_derived_type( &
                    arena, type_name, [integer ::], &
                    attribute_clause=header_attributes)
            else if (has_extends) then
                type_index = push_derived_type( &
                    arena, type_name, [integer ::], &
                    extends_parent=extends_parent)
            else
                type_index = push_derived_type(arena, type_name, [integer ::])
            end if
        end if
    end function finalize_derived_type

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
        if ((token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) .and. &
            to_lower(trim(adjustl(token%text))) == "end") then
            return
        end if

        ! Check for type declaration keywords. Fortran keywords are
        ! case-insensitive: matching the raw source spelling dropped every
        ! component written in upper case (issue #2966).
        if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
            select case (to_lower(trim(adjustl(token%text))))
            case ("integer", "real", "complex", "logical", "character", &
                    "type", "class", "double", "procedure")
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

end module parser_declarations_derived_module
