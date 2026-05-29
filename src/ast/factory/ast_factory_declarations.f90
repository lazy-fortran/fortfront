module ast_factory_declarations
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
                              derived_type_node, type_binding_node
    use ast_nodes_data, only: INTENT_NONE, INTENT_IN, INTENT_OUT, INTENT_INOUT
    implicit none
    private

    ! Public declaration node creation functions
    public :: push_declaration, push_parameter_declaration
    public :: push_derived_type, push_type_binding

contains
    integer function maxlen(values) result(len_max)
        character(len=*), intent(in) :: values(:)
        integer :: i
        len_max = 0
        do i = 1, size(values)
            len_max = max(len_max, len_trim(values(i)))
        end do
        if (len_max <= 0) len_max = 1
    end function maxlen

    pure subroutine assign_kind_metadata(has_kind, kind_value, input_kind)
        logical, intent(out) :: has_kind
        integer, intent(out) :: kind_value
        integer, intent(in), optional :: input_kind

        if (present(input_kind)) then
            if (input_kind > 0) then
                has_kind = .true.
                kind_value = input_kind
            else
                has_kind = .false.
                kind_value = 0
            end if
        else
            has_kind = .false.
            kind_value = 0
        end if
    end subroutine assign_kind_metadata

    subroutine assign_dimension_metadata(is_array, target_indices, input_indices)
        logical, intent(out) :: is_array
        integer, allocatable, intent(inout) :: target_indices(:)
        integer, intent(in), optional :: input_indices(:)

        if (allocated(target_indices)) deallocate (target_indices)

        if (present(input_indices)) then
            if (size(input_indices) > 0) then
                is_array = .true.
                allocate (target_indices, source=input_indices)
            else
                is_array = .false.
            end if
        else
            is_array = .false.
        end if
    end subroutine assign_dimension_metadata

    pure logical function resolve_optional_flag(optional_value, default_value) &
        result(flag)
        logical, intent(in), optional :: optional_value
        logical, intent(in) :: default_value

        flag = default_value
        if (present(optional_value)) flag = optional_value
    end function resolve_optional_flag

    subroutine initialize_declaration_details(decl, kind_value, dimension_indices, &
                                              initializer_index, is_allocatable, &
                                              is_pointer, &
                                              is_target, is_external, intent_value, &
                                              is_unsigned, &
                                              is_optional, is_parameter, is_save, &
                                              is_volatile, is_protected, &
                                              is_asynchronous, is_contiguous, &
                                              line, column)
        type(declaration_node), intent(inout) :: decl
        integer, intent(in), optional :: kind_value
        integer, intent(in), optional :: dimension_indices(:)
        integer, intent(in), optional :: initializer_index
        logical, intent(in), optional :: is_allocatable
        logical, intent(in), optional :: is_pointer
        logical, intent(in), optional :: is_target
        logical, intent(in), optional :: is_external
        logical, intent(in), optional :: is_unsigned
        character(len=*), intent(in), optional :: intent_value
        logical, intent(in), optional :: is_optional
        logical, intent(in), optional :: is_parameter
        logical, intent(in), optional :: is_save
        logical, intent(in), optional :: is_volatile
        logical, intent(in), optional :: is_protected
        logical, intent(in), optional :: is_asynchronous
        logical, intent(in), optional :: is_contiguous
        integer, intent(in), optional :: line, column

        call assign_kind_metadata(decl%has_kind, decl%kind_value, kind_value)

        if (present(initializer_index)) then
            if (initializer_index > 0) then
                decl%initializer_index = initializer_index
                decl%has_initializer = .true.
            else
                decl%initializer_index = 0
                decl%has_initializer = .false.
            end if
        else
            decl%initializer_index = 0
            decl%has_initializer = .false.
        end if

        call assign_dimension_metadata(decl%is_array, decl%dimension_indices, &
                                       dimension_indices)

        decl%is_allocatable = resolve_optional_flag(is_allocatable, .false.)
        decl%is_pointer = resolve_optional_flag(is_pointer, .false.)
        decl%is_target = resolve_optional_flag(is_target, .false.)
        decl%is_external = resolve_optional_flag(is_external, .false.)
        decl%is_unsigned = resolve_optional_flag(is_unsigned, .false.)

        if (present(intent_value)) then
            decl%intent = intent_value
            decl%has_intent = .true.
        else
            decl%has_intent = .false.
        end if

        decl%is_optional = resolve_optional_flag(is_optional, .false.)
        decl%is_parameter = resolve_optional_flag(is_parameter, .false.)
        decl%is_save = resolve_optional_flag(is_save, .false.)
        decl%is_volatile = resolve_optional_flag(is_volatile, .false.)
        decl%is_protected = resolve_optional_flag(is_protected, .false.)
        decl%is_asynchronous = resolve_optional_flag(is_asynchronous, .false.)
        decl%is_contiguous = resolve_optional_flag(is_contiguous, .false.)

        if (present(line)) decl%line = line
        if (present(column)) decl%column = column
    end subroutine initialize_declaration_details

    ! Create derived type node and add to stack
    function push_derived_type(arena, name, component_indices, param_indices, &
                               line, column, parent_index, attribute_clause, &
                               extends_parent, binding_indices) result(type_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: component_indices(:)
        integer, intent(in), optional :: param_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        character(len=*), intent(in), optional :: attribute_clause
        character(len=*), intent(in), optional :: extends_parent
        integer, intent(in), optional :: binding_indices(:)
        integer :: type_index
        type(derived_type_node) :: dtype

        ! Create derived type with index-based components
        dtype%name = name

        if (present(extends_parent)) then
            if (len_trim(extends_parent) > 0) then
                dtype%extends_parent = trim(extends_parent)
            end if
        end if

        if (present(attribute_clause)) then
            if (len_trim(attribute_clause) > 0) then
                dtype%attribute_clause = trim(attribute_clause)
                dtype%has_attributes = .true.
            else
                dtype%has_attributes = .false.
            end if
        else
            dtype%has_attributes = .false.
        end if

        if (present(component_indices)) then
            if (size(component_indices) > 0) then
                allocate (dtype%component_indices, source=component_indices)
            end if
        end if

        if (present(param_indices)) then
            if (size(param_indices) > 0) then
                dtype%has_parameters = .true.
                allocate (dtype%param_indices, source=param_indices)
            end if
        end if

        if (present(binding_indices)) then
            if (size(binding_indices) > 0) then
                dtype%has_contains = .true.
                allocate (dtype%binding_indices, source=binding_indices)
            end if
        end if

        if (present(line)) dtype%line = line
        if (present(column)) dtype%column = column

        call arena%push(dtype, "derived_type", parent_index)
        type_index = arena%size

        ! Update parent_index of all components to point to this derived type
        ! This ensures is_type_component() can correctly identify them (fixes #1738)
        if (present(component_indices)) then
            block
                integer :: i, comp_idx
                do i = 1, size(component_indices)
                    comp_idx = component_indices(i)
                    if (comp_idx > 0 .and. comp_idx <= arena%size) then
                        if (allocated(arena%entries(comp_idx)%node)) then
                            arena%entries(comp_idx)%parent_index = type_index
                        end if
                    end if
                end do
            end block
        end if
    end function push_derived_type

    ! Create declaration node and add to stack
    function push_declaration(arena, type_name, names, kind_value, &
                              dimension_indices, initializer_index, &
                              is_allocatable, is_pointer, is_target, &
                              is_external, is_unsigned, intent_value, is_optional, &
                              is_parameter, is_save, is_volatile, is_protected, &
                              is_asynchronous, is_contiguous, line, column, &
                              parent_index, character_length_expr, accessibility) &
        result(decl_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: type_name
        character(len=*), intent(in) :: names(:)
        integer, intent(in), optional :: kind_value
        character(len=*), intent(in), optional :: character_length_expr
        integer, intent(in), optional :: dimension_indices(:)
        integer, intent(in), optional :: initializer_index
        logical, intent(in), optional :: is_allocatable
        logical, intent(in), optional :: is_pointer
        logical, intent(in), optional :: is_target
        logical, intent(in), optional :: is_external
        logical, intent(in), optional :: is_unsigned
        character(len=*), intent(in), optional :: intent_value
        logical, intent(in), optional :: is_optional
        logical, intent(in), optional :: is_parameter
        logical, intent(in), optional :: is_save
        logical, intent(in), optional :: is_volatile
        logical, intent(in), optional :: is_protected
        logical, intent(in), optional :: is_asynchronous
        logical, intent(in), optional :: is_contiguous
        integer, intent(in), optional :: line, column, parent_index
        character(len=*), intent(in), optional :: accessibility
        integer :: decl_index
        type(declaration_node) :: decl

        decl%type_name = type_name
        if (size(names) > 0) decl%var_name = names(1)
        if (present(accessibility)) then
            if (len_trim(accessibility) > 0) decl%accessibility = trim(accessibility)
        end if

        if (size(names) > 1) then
            decl%is_multi_declaration = .true.
            allocate (character(len=maxlen(names)) :: decl%var_names(size(names)))
            decl%var_names = names
        else
            decl%is_multi_declaration = .false.
        end if

        if (present(character_length_expr)) then
            if (len_trim(character_length_expr) > 0) then
                decl%character_length_expr = character_length_expr
                decl%has_character_length = .true.
            end if
        end if

        call initialize_declaration_details(decl, kind_value=kind_value, &
                                            dimension_indices=dimension_indices, &
                                            initializer_index=initializer_index, &
                                            is_allocatable=is_allocatable, &
                                            is_pointer=is_pointer, &
                                            is_target=is_target, &
                                            is_external=is_external, &
                                            is_unsigned=is_unsigned, &
                                            intent_value=intent_value, &
                                            is_optional=is_optional, &
                                            is_parameter=is_parameter, &
                                            is_save=is_save, &
                                            is_volatile=is_volatile, &
                                            is_protected=is_protected, &
                                            is_asynchronous=is_asynchronous, &
                                            is_contiguous=is_contiguous, line=line, &
                                            column=column)

        call arena%push(decl, "declaration", parent_index)
        decl_index = arena%size
    end function push_declaration

    ! Create parameter declaration node and add to stack
    function push_parameter_declaration(arena, name, type_name, kind_value, &
                                        intent_value, is_optional, is_target, &
                                        is_unsigned, dimension_indices, line, column, &
                                        parent_index, character_length_expr) &
        result(param_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name, type_name
        integer, intent(in), optional :: kind_value, intent_value
        logical, intent(in), optional :: is_optional, is_target, is_unsigned
        integer, intent(in), optional :: dimension_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        character(len=*), intent(in), optional :: character_length_expr
        integer :: param_index
        type(parameter_declaration_node) :: param

        param%name = name
        param%type_name = type_name

        if (present(kind_value)) then
            if (kind_value > 0) then
                param%kind_value = kind_value
            else
                param%kind_value = 0
            end if
        else
            param%kind_value = 0
        end if

        if (present(character_length_expr)) then
            if (len_trim(character_length_expr) > 0) then
                param%character_length_expr = character_length_expr
                param%has_character_length = .true.
            end if
        end if

        if (present(intent_value)) then
            param%intent_type = intent_value
        else
            param%intent_type = INTENT_NONE
        end if

        param%is_optional = resolve_optional_flag(is_optional, .false.)
        param%is_target = resolve_optional_flag(is_target, .false.)
        param%is_unsigned = resolve_optional_flag(is_unsigned, .false.)

        call assign_dimension_metadata(param%is_array, param%dimension_indices, &
                                       dimension_indices)

        if (present(line)) param%line = line
        if (present(column)) param%column = column

        call arena%push(param, "parameter_declaration", parent_index)
        param_index = arena%size
    end function push_parameter_declaration

    ! Create type binding node and add to stack
    function push_type_binding(arena, binding_name, implementation, is_generic, &
                               is_final, is_deferred, pass_arg, accessibility, &
                               interface_name, generic_list, line, column, &
                               parent_index, pass_name) result(binding_index)
        use string_types, only: string_t
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: binding_name
        character(len=*), intent(in), optional :: implementation
        logical, intent(in), optional :: is_generic
        logical, intent(in), optional :: is_final
        logical, intent(in), optional :: is_deferred
        logical, intent(in), optional :: pass_arg
        character(len=*), intent(in), optional :: accessibility
        character(len=*), intent(in), optional :: interface_name
        type(string_t), intent(in), optional :: generic_list(:)
        integer, intent(in), optional :: line, column, parent_index
        character(len=*), intent(in), optional :: pass_name
        integer :: binding_index
        type(type_binding_node) :: binding

        binding%binding_name = binding_name
        if (present(pass_name)) then
            if (len_trim(pass_name) > 0) binding%pass_name = trim(pass_name)
        end if

        if (present(implementation)) then
            if (len_trim(implementation) > 0) then
                binding%implementation = trim(implementation)
            end if
        end if

        if (present(generic_list)) then
            if (size(generic_list) > 0) then
                allocate (binding%generic_list(size(generic_list)))
                binding%generic_list = generic_list
            end if
        end if

        if (present(interface_name)) then
            if (len_trim(interface_name) > 0) then
                binding%interface_name = trim(interface_name)
            end if
        end if

        binding%is_generic = resolve_optional_flag(is_generic, .false.)
        binding%is_final = resolve_optional_flag(is_final, .false.)
        binding%is_deferred = resolve_optional_flag(is_deferred, .false.)
        binding%pass_arg = resolve_optional_flag(pass_arg, .true.)

        if (present(accessibility)) then
            if (len_trim(accessibility) > 0) then
                binding%accessibility = trim(accessibility)
            end if
        end if

        if (present(line)) binding%line = line
        if (present(column)) binding%column = column

        call arena%push(binding, "type_binding", parent_index)
        binding_index = arena%size
    end function push_type_binding

end module ast_factory_declarations
