module parser_declarations_core_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER, TK_EOF, &
                          TK_KEYWORD, TK_NEWLINE, TK_WHITESPACE, TK_COMMENT
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_declaration, push_complex_literal, push_derived_type
    use parser_declarations_type_spec_support_module, only: type_specifier_t
    use parser_declarations_type_spec_module, only: parse_type_specifier
    use parser_result_types, only: parse_result_t, success_parse_result, &
                                   error_parse_result
    use error_handling, only: ERROR_PARSER
    use parser_expressions_module, only: parse_comparison
    use parser_type_hooks_module, only: register_type_annotation
    use parser_declaration_attributes_module, only: parse_declaration_attributes, &
                                                    parse_array_dimensions
    use declaration_attribute_utils, only: declaration_attribute_info_t
    implicit none
    private

    public :: parse_declaration
    public :: parse_multi_declaration
    public :: parse_declaration_with_result
    public :: parse_array_dimensions

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
        if (identifier_token%kind /= TK_IDENTIFIER .and. identifier_token%kind /= TK_KEYWORD) then
            return
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
                exit
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

    integer function emit_multi_declaration(arena, type_spec, attr_info, &
                                            var_names) result(decl_index)
        type(ast_arena_t), intent(inout) :: arena
        type(type_specifier_t), intent(in) :: type_spec
        type(declaration_attribute_info_t), intent(in) :: attr_info
        character(len=*), intent(in) :: var_names(:)

        decl_index = 0

        if (type_spec%has_kind) then
            if (attr_info%has_global_dimensions) then
                decl_index = push_declaration( &
                             arena, type_spec%type_name, trim_name_array(var_names), &
                             kind_value=type_spec%kind_value, &
                             dimension_indices=attr_info%global_dimension_indices, &
                             is_external=attr_info%is_external, &
                             is_allocatable=attr_info%is_allocatable, &
                             is_pointer=attr_info%is_pointer, &
                             is_parameter=attr_info%is_parameter)
            else
                decl_index = push_declaration( &
                             arena, type_spec%type_name, trim_name_array(var_names), &
                             kind_value=type_spec%kind_value, &
                             is_allocatable=attr_info%is_allocatable, &
                             is_pointer=attr_info%is_pointer, &
                             is_external=attr_info%is_external, &
                             is_parameter=attr_info%is_parameter)
            end if
        else
            if (attr_info%has_global_dimensions) then
                decl_index = push_declaration( &
                             arena, type_spec%type_name, trim_name_array(var_names), &
                             dimension_indices=attr_info%global_dimension_indices, &
                             is_allocatable=attr_info%is_allocatable, &
                             is_external=attr_info%is_external, &
                             is_pointer=attr_info%is_pointer, &
                             is_parameter=attr_info%is_parameter)
            else
                decl_index = push_declaration( &
                             arena, type_spec%type_name, trim_name_array(var_names), &
                             is_external=attr_info%is_external, &
                             is_allocatable=attr_info%is_allocatable, &
                             is_pointer=attr_info%is_pointer, &
                             is_parameter=attr_info%is_parameter)
            end if
        end if

        if (decl_index > 0) then
            if (attr_info%has_global_dimensions) then
                call register_type_annotation( &
                    decl_index, type_spec%type_name, var_names, &
                    has_kind=type_spec%has_kind, &
                    kind_value=type_spec%kind_value, &
                    is_parameter=attr_info%is_parameter, &
                    is_allocatable=attr_info%is_allocatable, &
                    is_pointer=attr_info%is_pointer, &
                    dimension_indices=attr_info%global_dimension_indices)
            else
                call register_type_annotation( &
                    decl_index, type_spec%type_name, var_names, &
                    has_kind=type_spec%has_kind, &
                    kind_value=type_spec%kind_value, &
                    is_parameter=attr_info%is_parameter, &
                    is_allocatable=attr_info%is_allocatable, &
                    is_pointer=attr_info%is_pointer)
            end if
        end if
    end function emit_multi_declaration

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

    integer function add_single_declaration(arena, type_spec, attr_info, &
                                            var_name, initializer_index, &
                                            has_local_dimensions, &
                                            local_dimension_indices) &
        result(decl_index)
        type(ast_arena_t), intent(inout) :: arena
        type(type_specifier_t), intent(in) :: type_spec
        type(declaration_attribute_info_t), intent(in) :: attr_info
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: initializer_index
        logical, intent(in) :: has_local_dimensions
        integer, intent(in), optional :: local_dimension_indices(:)
        character(len=:), allocatable :: name_buffer

        name_buffer = adjustl(trim(var_name))

        if (attr_info%has_global_dimensions) then
            decl_index = create_dimensional_declaration( &
                         arena, type_spec, attr_info, name_buffer, initializer_index, &
                         attr_info%global_dimension_indices)
            call register_declaration_annotation( &
                decl_index, type_spec, attr_info, name_buffer, &
                attr_info%global_dimension_indices)
            return
        end if

        if (has_local_dimensions .and. present(local_dimension_indices)) then
            decl_index = create_dimensional_declaration( &
                         arena, type_spec, attr_info, name_buffer, initializer_index, &
                         local_dimension_indices)
            call register_declaration_annotation( &
                decl_index, type_spec, attr_info, name_buffer, &
                local_dimension_indices)
        else
            decl_index = create_scalar_declaration( &
                         arena, type_spec, attr_info, name_buffer, initializer_index)
            call register_declaration_annotation( &
                decl_index, type_spec, attr_info, name_buffer)
        end if
    end function add_single_declaration

    integer function create_dimensional_declaration( &
        arena, type_spec, attr_info, name_buffer, initializer_index, &
        dimension_indices) result(decl_index)
        type(ast_arena_t), intent(inout) :: arena
        type(type_specifier_t), intent(in) :: type_spec
        type(declaration_attribute_info_t), intent(in) :: attr_info
        character(len=*), intent(in) :: name_buffer
        integer, intent(in) :: initializer_index
        integer, intent(in) :: dimension_indices(:)

        if (type_spec%has_kind) then
            decl_index = push_declaration( &
                         arena, type_spec%type_name, &
                         build_name_array(name_buffer), &
                         kind_value=type_spec%kind_value, &
                         dimension_indices=dimension_indices, &
                         initializer_index=initializer_index, &
                         is_allocatable=attr_info%is_allocatable, &
                         is_pointer=attr_info%is_pointer, &
                         is_target=attr_info%is_target, &
                         is_external=attr_info%is_external, &
                         intent_value=attr_info%intent, &
                         is_optional=attr_info%is_optional, &
                         is_parameter=attr_info%is_parameter)
        else
            decl_index = push_declaration( &
                         arena, type_spec%type_name, &
                         build_name_array(name_buffer), &
                         dimension_indices=dimension_indices, &
                         initializer_index=initializer_index, &
                         is_allocatable=attr_info%is_allocatable, &
                         is_pointer=attr_info%is_pointer, &
                         is_target=attr_info%is_target, &
                         is_external=attr_info%is_external, &
                         intent_value=attr_info%intent, &
                         is_optional=attr_info%is_optional, &
                         is_parameter=attr_info%is_parameter)
        end if
    end function create_dimensional_declaration

    integer function create_scalar_declaration( &
        arena, type_spec, attr_info, name_buffer, initializer_index) &
        result(decl_index)
        type(ast_arena_t), intent(inout) :: arena
        type(type_specifier_t), intent(in) :: type_spec
        type(declaration_attribute_info_t), intent(in) :: attr_info
        character(len=*), intent(in) :: name_buffer
        integer, intent(in) :: initializer_index

        if (type_spec%has_kind) then
            decl_index = push_declaration( &
                         arena, type_spec%type_name, &
                         build_name_array(name_buffer), &
                         kind_value=type_spec%kind_value, &
                         initializer_index=initializer_index, &
                         is_allocatable=attr_info%is_allocatable, &
                         is_pointer=attr_info%is_pointer, &
                         is_target=attr_info%is_target, &
                         is_external=attr_info%is_external, &
                         intent_value=attr_info%intent, &
                         is_optional=attr_info%is_optional, &
                         is_parameter=attr_info%is_parameter)
        else
            decl_index = push_declaration( &
                         arena, type_spec%type_name, &
                         build_name_array(name_buffer), &
                         initializer_index=initializer_index, &
                         is_allocatable=attr_info%is_allocatable, &
                         is_pointer=attr_info%is_pointer, &
                         is_target=attr_info%is_target, &
                         is_external=attr_info%is_external, &
                         intent_value=attr_info%intent, &
                         is_optional=attr_info%is_optional, &
                         is_parameter=attr_info%is_parameter)
        end if
    end function create_scalar_declaration
    function build_name_array(name) result(names)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: names(:)

        allocate (character(len=len_trim(name)) :: names(1))
        names(1) = trim(name)
    end function build_name_array

    integer function name_maxlen(values) result(len_max)
        character(len=*), intent(in) :: values(:)
        integer :: i

        len_max = 1
        do i = 1, size(values)
            len_max = max(len_max, len_trim(values(i)))
        end do
    end function name_maxlen

    function trim_name_array(var_names) result(names)
        character(len=*), intent(in) :: var_names(:)
        character(len=:), allocatable :: names(:)
        integer :: i

        allocate (character(len=name_maxlen(var_names)) :: names(size(var_names)))
        do i = 1, size(var_names)
            names(i) = trim(var_names(i))
        end do
    end function trim_name_array

    subroutine register_declaration_annotation(decl_index, type_spec, attr_info, &
                                               name_buffer, dimension_indices)
        integer, intent(in) :: decl_index
        type(type_specifier_t), intent(in) :: type_spec
        type(declaration_attribute_info_t), intent(in) :: attr_info
        character(len=*), intent(in) :: name_buffer
        integer, intent(in), optional :: dimension_indices(:)

        if (decl_index <= 0) then
            return
        end if

        if (present(dimension_indices)) then
            call register_type_annotation( &
                decl_index, type_spec%type_name, [name_buffer], &
                has_kind=type_spec%has_kind, &
                kind_value=type_spec%kind_value, &
                is_parameter=attr_info%is_parameter, &
                is_allocatable=attr_info%is_allocatable, &
                is_pointer=attr_info%is_pointer, &
                dimension_indices=dimension_indices)
        else
            call register_type_annotation( &
                decl_index, type_spec%type_name, [name_buffer], &
                has_kind=type_spec%has_kind, &
                kind_value=type_spec%kind_value, &
                is_parameter=attr_info%is_parameter, &
                is_allocatable=attr_info%is_allocatable, &
                is_pointer=attr_info%is_pointer)
        end if
    end subroutine register_declaration_annotation

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

    ! Helper function to detect and convert complex literals
    function handle_complex_initializer(parser, arena, type_name) result(complex_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: type_name
        integer :: complex_index

        type(token_t) :: token
        integer :: real_index
        integer :: imag_index

        complex_index = 0

        if (type_name /= "complex") then
            complex_index = parse_comparison(parser, arena)
            return
        end if

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            complex_index = parse_comparison(parser, arena)
            return
        end if

        token = parser%consume()
        real_index = parse_comparison(parser, arena)

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume()
            imag_index = parse_comparison(parser, arena)

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                complex_index = push_complex_literal(arena, real_index, imag_index, &
                                                     token%line, token%column)
            else
                complex_index = real_index
            end if
        else
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
            end if
            complex_index = real_index
        end if
    end function handle_complex_initializer

    function parse_multi_declaration(parser, arena) result(decl_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable :: decl_indices(:)

        type(type_specifier_t) :: type_spec
        type(declaration_attribute_info_t) :: attr_info
        character(len=64), allocatable :: var_names(:)
        integer, allocatable :: per_var_dims(:, :)
        logical, allocatable :: has_dims(:)
        integer, allocatable :: init_indices(:)
        integer :: var_count
        logical :: has_any_initializer

        type_spec = parse_type_specifier(parser, arena)
        if (.not. allocated(type_spec%type_name)) then
            allocate (decl_indices(0))
            return
        end if

        call parse_declaration_attributes(parser, arena, attr_info)
        call skip_declaration_separator(parser)

        call initialize_multi_state(var_names, per_var_dims, has_dims, &
                                    init_indices)
        call collect_multi_variable_data(parser, arena, type_spec, var_names, &
                                         per_var_dims, has_dims, init_indices, &
                                         var_count, has_any_initializer)
        call finalize_multi_declaration( &
            arena, type_spec, attr_info, var_names, per_var_dims, has_dims, &
            init_indices, var_count, has_any_initializer, decl_indices)
    end function parse_multi_declaration

    subroutine initialize_multi_state(var_names, per_var_dims, has_dims, &
                                      init_indices)
        character(len=64), allocatable, intent(out) :: var_names(:)
        integer, allocatable, intent(out) :: per_var_dims(:, :)
        logical, allocatable, intent(out) :: has_dims(:)
        integer, allocatable, intent(out) :: init_indices(:)
        integer, parameter :: initial_capacity = 4
        integer, parameter :: max_dim_slots = 10

        allocate (var_names(initial_capacity))
        allocate (per_var_dims(initial_capacity, max_dim_slots))
        allocate (has_dims(initial_capacity))
        allocate (init_indices(initial_capacity))
        var_names = ""
        per_var_dims = 0
        has_dims = .false.
        init_indices = 0
    end subroutine initialize_multi_state

    subroutine collect_multi_variable_data(parser, arena, type_spec, var_names, &
                                           per_var_dims, has_dims, &
                                           init_indices, var_count, &
                                           has_any_initializer)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(type_specifier_t), intent(in) :: type_spec
        character(len=64), allocatable, intent(inout) :: var_names(:)
        integer, allocatable, intent(inout) :: per_var_dims(:, :)
        logical, allocatable, intent(inout) :: has_dims(:)
        integer, allocatable, intent(inout) :: init_indices(:)
        integer, intent(out) :: var_count
        logical, intent(out) :: has_any_initializer
        integer :: capacity
        type(token_t) :: token

        var_count = 0
        has_any_initializer = .false.
        capacity = size(var_names)

        do while (.not. parser%is_at_end())
            token = parser%consume()
            if (token%kind /= TK_IDENTIFIER) then
                exit
            end if

            var_count = var_count + 1
            if (var_count > capacity) then
                call expand_multi_state(var_names, per_var_dims, has_dims, &
                                        init_indices, capacity)
            end if

            var_names(var_count) = token%text
            has_dims(var_count) = .false.
            init_indices(var_count) = 0
            per_var_dims(var_count, :) = 0

            call parse_multi_variable_dimensions(parser, arena, per_var_dims, &
                                                 has_dims, var_count)
            call parse_multi_variable_initializer(parser, arena, type_spec, &
                                                  init_indices(var_count), &
                                                  has_any_initializer)
            if (.not. continue_multi_variable(parser)) then
                exit
            end if
        end do
    end subroutine collect_multi_variable_data

    subroutine expand_multi_state(var_names, per_var_dims, has_dims, &
                                  init_indices, capacity)
        character(len=64), allocatable, intent(inout) :: var_names(:)
        integer, allocatable, intent(inout) :: per_var_dims(:, :)
        logical, allocatable, intent(inout) :: has_dims(:)
        integer, allocatable, intent(inout) :: init_indices(:)
        integer, intent(inout) :: capacity
        character(len=64), allocatable :: new_names(:)
        integer, allocatable :: new_dims(:, :)
        logical, allocatable :: new_has(:)
        integer, allocatable :: new_init(:)
        integer :: new_capacity
        integer :: dim_slots

        new_capacity = capacity * 2
        dim_slots = size(per_var_dims, 2)

        allocate (new_names(new_capacity))
        allocate (new_dims(new_capacity, dim_slots))
        allocate (new_has(new_capacity))
        allocate (new_init(new_capacity))

        new_names = ""
        new_dims = 0
        new_has = .false.
        new_init = 0

        new_names(1:capacity) = var_names(1:capacity)
        new_dims(1:capacity, :) = per_var_dims(1:capacity, :)
        new_has(1:capacity) = has_dims(1:capacity)
        new_init(1:capacity) = init_indices(1:capacity)

        call move_alloc(new_names, var_names)
        call move_alloc(new_dims, per_var_dims)
        call move_alloc(new_has, has_dims)
        call move_alloc(new_init, init_indices)

        capacity = new_capacity
    end subroutine expand_multi_state

    subroutine parse_multi_variable_dimensions(parser, arena, per_var_dims, &
                                               has_dims, index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(inout) :: per_var_dims(:, :)
        logical, allocatable, intent(inout) :: has_dims(:)
        integer, intent(in) :: index
        type(token_t) :: token
        integer, allocatable :: local_dims(:)
        integer :: slot_count
        integer :: j

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%text /= "(") then
            return
        end if

        token = parser%consume()
        call parse_array_dimensions(parser, arena, local_dims)
        if (.not. allocated(local_dims)) then
            return
        end if

        if (size(local_dims) == 0) then
            block
                integer, allocatable :: temp(:)
                call move_alloc(local_dims, temp)
            end block
            return
        end if

        has_dims(index) = .true.
        slot_count = min(size(local_dims), size(per_var_dims, 2))
        do j = 1, slot_count
            per_var_dims(index, j) = local_dims(j)
        end do
        do j = slot_count + 1, size(per_var_dims, 2)
            per_var_dims(index, j) = 0
        end do

        block
            integer, allocatable :: temp(:)
            call move_alloc(local_dims, temp)
        end block
    end subroutine parse_multi_variable_dimensions

    subroutine parse_multi_variable_initializer(parser, arena, type_spec, &
                                                init_index, has_any_initializer)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(type_specifier_t), intent(in) :: type_spec
        integer, intent(inout) :: init_index
        logical, intent(inout) :: has_any_initializer
        type(token_t) :: token

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%text == "=" .or. token%text == "=>") then
            token = parser%consume()
            if (type_spec%base_keyword == "complex") then
                init_index = handle_complex_initializer( &
                             parser, arena, type_spec%base_keyword)
            else
                init_index = parse_comparison(parser, arena)
            end if
            if (init_index > 0) then
                has_any_initializer = .true.
            end if
        end if
    end subroutine parse_multi_variable_initializer

    logical function continue_multi_variable(parser) result(should_continue)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        should_continue = .false.
        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%text == ",") then
            token = parser%consume()
            should_continue = .true.
        end if
    end function continue_multi_variable

    subroutine finalize_multi_declaration(arena, type_spec, attr_info, &
                                          var_names, per_var_dims, has_dims, &
                                          init_indices, var_count, &
                                          has_any_initializer, decl_indices)
        type(ast_arena_t), intent(inout) :: arena
        type(type_specifier_t), intent(in) :: type_spec
        type(declaration_attribute_info_t), intent(in) :: attr_info
        character(len=64), allocatable, intent(inout) :: var_names(:)
        integer, allocatable, intent(inout) :: per_var_dims(:, :)
        logical, allocatable, intent(inout) :: has_dims(:)
        integer, allocatable, intent(inout) :: init_indices(:)
        integer, intent(in) :: var_count
        logical, intent(in) :: has_any_initializer
        integer, allocatable, intent(out) :: decl_indices(:)
        integer :: decl_index

        if (var_count <= 0) then
            allocate (decl_indices(0))
            return
        end if

        if (requires_individual_declarations(has_dims, has_any_initializer, &
                                             var_count)) then
            call emit_individual_declarations( &
                arena, type_spec, attr_info, var_names, per_var_dims, has_dims, &
                init_indices, var_count, decl_indices)
        else
            decl_index = emit_multi_declaration( &
                         arena, type_spec, attr_info, var_names(1:var_count))
            if (decl_index > 0) then
                allocate (decl_indices(1))
                decl_indices(1) = decl_index
            else
                allocate (decl_indices(0))
            end if
        end if
    end subroutine finalize_multi_declaration

    subroutine emit_individual_declarations(arena, type_spec, attr_info, &
                                            var_names, per_var_dims, has_dims, &
                                            init_indices, var_count, decl_indices)
        type(ast_arena_t), intent(inout) :: arena
        type(type_specifier_t), intent(in) :: type_spec
        type(declaration_attribute_info_t), intent(in) :: attr_info
        character(len=64), allocatable, intent(inout) :: var_names(:)
        integer, allocatable, intent(inout) :: per_var_dims(:, :)
        logical, allocatable, intent(inout) :: has_dims(:)
        integer, allocatable, intent(inout) :: init_indices(:)
        integer, intent(in) :: var_count
        integer, allocatable, intent(out) :: decl_indices(:)
        integer :: i
        integer, allocatable :: var_dims(:)

        allocate (decl_indices(var_count))

        do i = 1, var_count
            if (has_dims(i)) then
                call extract_variable_dimensions(per_var_dims, i, var_dims)
                decl_indices(i) = add_single_declaration( &
                                  arena, type_spec, attr_info, var_names(i), &
                                  init_indices(i), .true., var_dims)
                if (allocated(var_dims)) then
                    block
                        integer, allocatable :: temp(:)
                        call move_alloc(var_dims, temp)
                    end block
                end if
            else
                decl_indices(i) = add_single_declaration( &
                                  arena, type_spec, attr_info, var_names(i), &
                                  init_indices(i), .false.)
            end if
        end do
    end subroutine emit_individual_declarations

    logical function requires_individual_declarations( &
        has_dims, has_any_initializer, var_count) result(needs_split)
        logical, intent(in) :: has_dims(:)
        logical, intent(in) :: has_any_initializer
        integer, intent(in) :: var_count
        integer :: i

        needs_split = has_any_initializer
        if (needs_split) then
            return
        end if

        do i = 1, var_count
            if (has_dims(i)) then
                needs_split = .true.
                return
            end if
        end do
    end function requires_individual_declarations

    subroutine extract_variable_dimensions(per_var_dims, index, var_dims)
        integer, allocatable, intent(in) :: per_var_dims(:, :)
        integer, intent(in) :: index
        integer, allocatable, intent(out) :: var_dims(:)
        integer :: dim_count
        integer :: j

        dim_count = 0
        do j = 1, size(per_var_dims, 2)
            if (per_var_dims(index, j) > 0) then
                dim_count = dim_count + 1
            else
                exit
            end if
        end do

        if (dim_count == 0) then
            allocate (var_dims(0))
            return
        end if

        allocate (var_dims(dim_count))
        var_dims = per_var_dims(index, 1:dim_count)
    end subroutine extract_variable_dimensions

end module parser_declarations_core_module
