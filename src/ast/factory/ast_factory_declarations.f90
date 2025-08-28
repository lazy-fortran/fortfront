module ast_factory_declarations
    use ast_core
    use ast_nodes_data, only: INTENT_NONE, INTENT_IN, INTENT_OUT, INTENT_INOUT
    implicit none
    private

    ! Public declaration node creation functions
    public :: push_declaration, push_multi_declaration, push_parameter_declaration
    public :: push_derived_type

contains

    ! Create derived type node and add to stack
    function push_derived_type(arena, name, component_indices, param_indices, &
                               line, column, parent_index) result(type_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: component_indices(:)
        integer, intent(in), optional :: param_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: type_index
        type(derived_type_node) :: dtype

        ! Create derived type with index-based components
        dtype%name = name

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

        if (present(line)) dtype%line = line
        if (present(column)) dtype%column = column

        call arena%push(dtype, "derived_type", parent_index)
        type_index = arena%size
    end function push_derived_type

    ! Create declaration node and add to stack
    function push_declaration(arena, type_name, var_name, kind_value, &
       dimension_indices, &
       initializer_index, is_allocatable, is_pointer, is_target, &
       intent_value, is_optional, is_parameter, line, column, &
       parent_index) result(decl_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: type_name, var_name
        integer, intent(in), optional :: kind_value
        integer, intent(in), optional :: dimension_indices(:)
        integer, intent(in), optional :: initializer_index
        logical, intent(in), optional :: is_allocatable
        logical, intent(in), optional :: is_pointer  
        logical, intent(in), optional :: is_target
        character(len=*), intent(in), optional :: intent_value
        logical, intent(in), optional :: is_optional
        logical, intent(in), optional :: is_parameter
        integer, intent(in), optional :: line, column, parent_index
        integer :: decl_index
        type(declaration_node) :: decl

        ! Create declaration with index-based fields
        decl%type_name = type_name
        decl%var_name = var_name

        if (present(kind_value)) then
            decl%kind_value = kind_value
            decl%has_kind = .true.
        else
            decl%kind_value = 0
            decl%has_kind = .false.
        end if

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

        if (present(dimension_indices)) then
            decl%is_array = .true.
            allocate (decl%dimension_indices, source=dimension_indices)
        else
            decl%is_array = .false.
        end if

        if (present(is_allocatable)) then
            decl%is_allocatable = is_allocatable
        else
            decl%is_allocatable = .false.
        end if

        if (present(is_pointer)) then
            decl%is_pointer = is_pointer
        else
            decl%is_pointer = .false.
        end if
        
        if (present(is_target)) then
            decl%is_target = is_target
        else
            decl%is_target = .false.
        end if

        if (present(intent_value)) then
            decl%intent = intent_value
            decl%has_intent = .true.
        else
            decl%has_intent = .false.
        end if

        if (present(is_optional)) then
            decl%is_optional = is_optional
        else
            decl%is_optional = .false.
        end if

        if (present(is_parameter)) then
            decl%is_parameter = is_parameter
        else
            decl%is_parameter = .false.
        end if

        if (present(line)) decl%line = line
        if (present(column)) decl%column = column

        call arena%push(decl, "declaration", parent_index)
        decl_index = arena%size
    end function push_declaration

    ! Create multi-variable declaration node and add to stack
    function push_multi_declaration(arena, type_name, var_names, kind_value, &
           dimension_indices, &
           initializer_index, is_allocatable, is_pointer, is_target, intent_value, &
           is_optional, is_parameter, line, column, parent_index) result(decl_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: type_name
        character(len=*), intent(in) :: var_names(:)
        integer, intent(in), optional :: kind_value
        integer, intent(in), optional :: dimension_indices(:)
        integer, intent(in), optional :: initializer_index
        logical, intent(in), optional :: is_allocatable
        logical, intent(in), optional :: is_pointer
        logical, intent(in), optional :: is_target
        character(len=*), intent(in), optional :: intent_value
        logical, intent(in), optional :: is_optional
        logical, intent(in), optional :: is_parameter
        integer, intent(in), optional :: line, column, parent_index
        integer :: decl_index
        type(declaration_node) :: decl
        integer :: i

        ! Create multi-variable declaration
        decl%type_name = type_name
        decl%is_multi_declaration = .true.
        
        ! Set primary variable name to first variable
        if (size(var_names) > 0) then
            decl%var_name = trim(var_names(1))
        end if
        
        ! Allocate and copy variable names
        allocate(character(len=100) :: decl%var_names(size(var_names)))
        do i = 1, size(var_names)
            decl%var_names(i) = trim(var_names(i))
        end do

        if (present(kind_value)) then
            decl%kind_value = kind_value
            decl%has_kind = .true.
        else
            decl%kind_value = 0
            decl%has_kind = .false.
        end if

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

        if (present(dimension_indices)) then
            decl%is_array = .true.
            allocate (decl%dimension_indices, source=dimension_indices)
        else
            decl%is_array = .false.
        end if

        if (present(is_allocatable)) then
            decl%is_allocatable = is_allocatable
        else
            decl%is_allocatable = .false.
        end if

        if (present(is_pointer)) then
            decl%is_pointer = is_pointer
        else
            decl%is_pointer = .false.
        end if
        
        if (present(is_target)) then
            decl%is_target = is_target
        else
            decl%is_target = .false.
        end if

        if (present(intent_value)) then
            decl%intent = intent_value
            decl%has_intent = .true.
        else
            decl%has_intent = .false.
        end if

        if (present(is_optional)) then
            decl%is_optional = is_optional
        else
            decl%is_optional = .false.
        end if

        if (present(is_parameter)) then
            decl%is_parameter = is_parameter
        else
            decl%is_parameter = .false.
        end if

        if (present(line)) decl%line = line
        if (present(column)) decl%column = column

        call arena%push(decl, "multi_declaration", parent_index)
        decl_index = arena%size
    end function push_multi_declaration

    ! Create parameter declaration node and add to stack
    function push_parameter_declaration(arena, name, type_name, kind_value, &
                      intent_value, is_optional, dimension_indices, line, column, &
                      parent_index) result(param_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name, type_name
        integer, intent(in), optional :: kind_value, intent_value
        logical, intent(in), optional :: is_optional
        integer, intent(in), optional :: dimension_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: param_index
        type(parameter_declaration_node) :: param

        param%name = name
        param%type_name = type_name

        if (present(kind_value) .and. kind_value > 0) then
            param%kind_value = kind_value
        else
            param%kind_value = 0
        end if

        if (present(intent_value)) then
            param%intent_type = intent_value
        else
            param%intent_type = INTENT_NONE
        end if

        if (present(is_optional)) then
            param%is_optional = is_optional
        else
            param%is_optional = .false.
        end if

        ! Handle array dimensions
        if (present(dimension_indices)) then
            if (size(dimension_indices) > 0) then
                param%is_array = .true.
                allocate (param%dimension_indices, source=dimension_indices)
            else
                param%is_array = .false.
            end if
        else
            param%is_array = .false.
        end if

        if (present(line)) param%line = line
        if (present(column)) param%column = column

        call arena%push(param, "parameter_declaration", parent_index)
        param_index = arena%size
    end function push_parameter_declaration

end module ast_factory_declarations