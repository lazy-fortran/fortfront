module parser_declarations_construction_module
    use lexer_core, only: token_t, TK_OPERATOR
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_declaration, push_complex_literal
    use parser_declarations_type_spec_support_module, only: type_specifier_t
    use parser_expressions_module, only: parse_comparison
    use parser_type_hooks_module, only: register_type_annotation
    use declaration_attribute_utils, only: declaration_attribute_info_t
    implicit none
    private

    public :: add_single_declaration
    public :: emit_multi_declaration
    public :: handle_complex_initializer

contains

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

        if (type_spec%has_kind .and. type_spec%has_character_length) then
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
                is_unsigned=attr_info%is_unsigned, &
                intent_value=attr_info%intent, &
                is_optional=attr_info%is_optional, &
                is_parameter=attr_info%is_parameter, &
                is_save=attr_info%is_save, &
                is_volatile=attr_info%is_volatile, &
                is_protected=attr_info%is_protected, &
                is_asynchronous=attr_info%is_asynchronous, &
                accessibility=attr_info%accessibility, &
                is_contiguous=attr_info%is_contiguous, &
                is_value=attr_info%is_value, &
                character_length_expr=type_spec%character_length_expr)
        else if (type_spec%has_kind) then
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
                is_unsigned=attr_info%is_unsigned, &
                intent_value=attr_info%intent, &
                is_optional=attr_info%is_optional, &
                is_parameter=attr_info%is_parameter, &
                is_save=attr_info%is_save, &
                is_volatile=attr_info%is_volatile, &
                is_protected=attr_info%is_protected, &
                is_asynchronous=attr_info%is_asynchronous, &
                accessibility=attr_info%accessibility, &
                is_contiguous=attr_info%is_contiguous, &
                is_value=attr_info%is_value)
        else if (type_spec%has_character_length) then
            decl_index = push_declaration( &
                arena, type_spec%type_name, &
                build_name_array(name_buffer), &
                dimension_indices=dimension_indices, &
                initializer_index=initializer_index, &
                is_allocatable=attr_info%is_allocatable, &
                is_pointer=attr_info%is_pointer, &
                is_target=attr_info%is_target, &
                is_external=attr_info%is_external, &
                is_unsigned=attr_info%is_unsigned, &
                intent_value=attr_info%intent, &
                is_optional=attr_info%is_optional, &
                is_parameter=attr_info%is_parameter, &
                is_save=attr_info%is_save, &
                is_volatile=attr_info%is_volatile, &
                is_protected=attr_info%is_protected, &
                is_asynchronous=attr_info%is_asynchronous, &
                accessibility=attr_info%accessibility, &
                is_contiguous=attr_info%is_contiguous, &
                is_value=attr_info%is_value, &
                character_length_expr=type_spec%character_length_expr)
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
                is_unsigned=attr_info%is_unsigned, &
                intent_value=attr_info%intent, &
                is_optional=attr_info%is_optional, &
                is_parameter=attr_info%is_parameter, &
                is_save=attr_info%is_save, &
                is_volatile=attr_info%is_volatile, &
                is_protected=attr_info%is_protected, &
                is_asynchronous=attr_info%is_asynchronous, &
                accessibility=attr_info%accessibility, &
                is_contiguous=attr_info%is_contiguous, &
                is_value=attr_info%is_value)
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

        if (type_spec%has_kind .and. type_spec%has_character_length) then
            decl_index = push_declaration( &
                arena, type_spec%type_name, &
                build_name_array(name_buffer), &
                kind_value=type_spec%kind_value, &
                initializer_index=initializer_index, &
                is_allocatable=attr_info%is_allocatable, &
                is_pointer=attr_info%is_pointer, &
                is_target=attr_info%is_target, &
                is_external=attr_info%is_external, &
                is_unsigned=attr_info%is_unsigned, &
                intent_value=attr_info%intent, &
                is_optional=attr_info%is_optional, &
                is_parameter=attr_info%is_parameter, &
                is_save=attr_info%is_save, &
                is_volatile=attr_info%is_volatile, &
                is_protected=attr_info%is_protected, &
                is_asynchronous=attr_info%is_asynchronous, &
                accessibility=attr_info%accessibility, &
                is_contiguous=attr_info%is_contiguous, &
                is_value=attr_info%is_value, &
                character_length_expr=type_spec%character_length_expr)
        else if (type_spec%has_kind) then
            decl_index = push_declaration( &
                arena, type_spec%type_name, &
                build_name_array(name_buffer), &
                kind_value=type_spec%kind_value, &
                initializer_index=initializer_index, &
                is_allocatable=attr_info%is_allocatable, &
                is_pointer=attr_info%is_pointer, &
                is_target=attr_info%is_target, &
                is_external=attr_info%is_external, &
                is_unsigned=attr_info%is_unsigned, &
                intent_value=attr_info%intent, &
                is_optional=attr_info%is_optional, &
                is_parameter=attr_info%is_parameter, &
                is_save=attr_info%is_save, &
                is_volatile=attr_info%is_volatile, &
                is_protected=attr_info%is_protected, &
                is_asynchronous=attr_info%is_asynchronous, &
                accessibility=attr_info%accessibility, &
                is_contiguous=attr_info%is_contiguous, &
                is_value=attr_info%is_value)
        else if (type_spec%has_character_length) then
            decl_index = push_declaration( &
                arena, type_spec%type_name, &
                build_name_array(name_buffer), &
                initializer_index=initializer_index, &
                is_allocatable=attr_info%is_allocatable, &
                is_pointer=attr_info%is_pointer, &
                is_target=attr_info%is_target, &
                is_external=attr_info%is_external, &
                is_unsigned=attr_info%is_unsigned, &
                intent_value=attr_info%intent, &
                is_optional=attr_info%is_optional, &
                is_parameter=attr_info%is_parameter, &
                is_save=attr_info%is_save, &
                is_volatile=attr_info%is_volatile, &
                is_protected=attr_info%is_protected, &
                is_asynchronous=attr_info%is_asynchronous, &
                accessibility=attr_info%accessibility, &
                is_contiguous=attr_info%is_contiguous, &
                is_value=attr_info%is_value, &
                character_length_expr=type_spec%character_length_expr)
        else
            decl_index = push_declaration( &
                arena, type_spec%type_name, &
                build_name_array(name_buffer), &
                initializer_index=initializer_index, &
                is_allocatable=attr_info%is_allocatable, &
                is_pointer=attr_info%is_pointer, &
                is_target=attr_info%is_target, &
                is_external=attr_info%is_external, &
                is_unsigned=attr_info%is_unsigned, &
                intent_value=attr_info%intent, &
                is_optional=attr_info%is_optional, &
                is_parameter=attr_info%is_parameter, &
                is_save=attr_info%is_save, &
                is_volatile=attr_info%is_volatile, &
                is_protected=attr_info%is_protected, &
                is_asynchronous=attr_info%is_asynchronous, &
                accessibility=attr_info%accessibility, &
                is_contiguous=attr_info%is_contiguous, &
                is_value=attr_info%is_value)
        end if
    end function create_scalar_declaration

    function build_name_array(name) result(names)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: names(:)

        allocate (character(len=len_trim(name)) :: names(1))
        names = [trim(name)]
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
                is_unsigned=attr_info%is_unsigned, &
                is_parameter=attr_info%is_parameter, &
                is_allocatable=attr_info%is_allocatable, &
                is_pointer=attr_info%is_pointer, &
                dimension_indices=dimension_indices)
        else
            call register_type_annotation( &
                decl_index, type_spec%type_name, [name_buffer], &
                has_kind=type_spec%has_kind, &
                kind_value=type_spec%kind_value, &
                is_unsigned=attr_info%is_unsigned, &
                is_parameter=attr_info%is_parameter, &
                is_allocatable=attr_info%is_allocatable, &
                is_pointer=attr_info%is_pointer)
        end if
    end subroutine register_declaration_annotation

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
                    is_unsigned=attr_info%is_unsigned, &
                    is_allocatable=attr_info%is_allocatable, &
                    is_pointer=attr_info%is_pointer, &
                    is_target=attr_info%is_target, &
                    intent_value=attr_info%intent, &
                    is_optional=attr_info%is_optional, &
                    is_parameter=attr_info%is_parameter, &
                    is_save=attr_info%is_save, &
                    is_volatile=attr_info%is_volatile, &
                    is_protected=attr_info%is_protected, &
                    is_asynchronous=attr_info%is_asynchronous, &
                    accessibility=attr_info%accessibility, &
                    is_contiguous=attr_info%is_contiguous, &
                    is_value=attr_info%is_value)
            else
                decl_index = push_declaration( &
                    arena, type_spec%type_name, trim_name_array(var_names), &
                    kind_value=type_spec%kind_value, &
                    is_allocatable=attr_info%is_allocatable, &
                    is_pointer=attr_info%is_pointer, &
                    is_target=attr_info%is_target, &
                    is_external=attr_info%is_external, &
                    is_unsigned=attr_info%is_unsigned, &
                    intent_value=attr_info%intent, &
                    is_optional=attr_info%is_optional, &
                    is_parameter=attr_info%is_parameter, &
                    is_save=attr_info%is_save, &
                    is_volatile=attr_info%is_volatile, &
                    is_protected=attr_info%is_protected, &
                    is_asynchronous=attr_info%is_asynchronous, &
                    accessibility=attr_info%accessibility, &
                    is_contiguous=attr_info%is_contiguous, &
                    is_value=attr_info%is_value)
            end if
        else
            if (attr_info%has_global_dimensions) then
                decl_index = push_declaration( &
                    arena, type_spec%type_name, trim_name_array(var_names), &
                    dimension_indices=attr_info%global_dimension_indices, &
                    intent_value=attr_info%intent, &
                    is_optional=attr_info%is_optional, &
                    is_allocatable=attr_info%is_allocatable, &
                    is_target=attr_info%is_target, &
                    is_external=attr_info%is_external, &
                    is_unsigned=attr_info%is_unsigned, &
                    is_pointer=attr_info%is_pointer, &
                    is_parameter=attr_info%is_parameter, &
                    is_save=attr_info%is_save, &
                    is_volatile=attr_info%is_volatile, &
                    is_protected=attr_info%is_protected, &
                    is_asynchronous=attr_info%is_asynchronous, &
                    accessibility=attr_info%accessibility, &
                    is_contiguous=attr_info%is_contiguous, &
                    is_value=attr_info%is_value)
            else
                decl_index = push_declaration( &
                    arena, type_spec%type_name, trim_name_array(var_names), &
                    intent_value=attr_info%intent, &
                    is_optional=attr_info%is_optional, &
                    is_external=attr_info%is_external, &
                    is_unsigned=attr_info%is_unsigned, &
                    is_allocatable=attr_info%is_allocatable, &
                    is_pointer=attr_info%is_pointer, &
                    is_target=attr_info%is_target, &
                    is_parameter=attr_info%is_parameter, &
                    is_save=attr_info%is_save, &
                    is_volatile=attr_info%is_volatile, &
                    is_protected=attr_info%is_protected, &
                    is_asynchronous=attr_info%is_asynchronous, &
                    accessibility=attr_info%accessibility, &
                    is_contiguous=attr_info%is_contiguous, &
                    is_value=attr_info%is_value)
            end if
        end if

        if (decl_index > 0) then
            if (attr_info%has_global_dimensions) then
                call register_type_annotation( &
                    decl_index, type_spec%type_name, var_names, &
                    has_kind=type_spec%has_kind, &
                    kind_value=type_spec%kind_value, &
                    is_unsigned=attr_info%is_unsigned, &
                    is_parameter=attr_info%is_parameter, &
                    is_allocatable=attr_info%is_allocatable, &
                    is_pointer=attr_info%is_pointer, &
                    dimension_indices=attr_info%global_dimension_indices)
            else
                call register_type_annotation( &
                    decl_index, type_spec%type_name, var_names, &
                    has_kind=type_spec%has_kind, &
                    kind_value=type_spec%kind_value, &
                    is_unsigned=attr_info%is_unsigned, &
                    is_parameter=attr_info%is_parameter, &
                    is_allocatable=attr_info%is_allocatable, &
                    is_pointer=attr_info%is_pointer)
            end if
        end if
    end function emit_multi_declaration

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

end module parser_declarations_construction_module
