module frontend_compiler_queries
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: string_t
    use ast_nodes_procedure, only: subroutine_call_node, function_def_node, &
        subroutine_def_node
    use ast_nodes_core, only: binary_op_node, literal_node, identifier_node, &
        array_literal_node, program_node, component_access_node, &
        pointer_assignment_node
    use ast_nodes_bounds, only: array_slice_node, array_bounds_node, &
        range_expression_node
    use ast_nodes_transfer, only: nullify_node, return_node, &
        alt_return_spec_node
    use ast_nodes_data, only: declaration_node, derived_type_node, &
        parameter_declaration_node, module_node, block_data_node, &
        submodule_node, multi_unit_container_node, type_binding_node, &
        PARAM_UNKNOWN, PARAM_KIND, PARAM_LEN
    use ast_nodes_misc, only: interface_block_node, import_statement_node, &
        use_statement_node, visibility_statement_node, &
        namelist_statement_node, data_statement_node, &
        statement_function_node
    use ast_nodes_legacy, only: common_block_node, enum_node
    use ast_nodes_conditional, only: select_case_node, case_block_node, &
        case_default_node, case_range_node, &
        select_type_node, type_guard_block_node
    implicit none
    private

    public :: is_subroutine_call_statement
    public :: get_subroutine_call_name
    public :: get_subroutine_call_arg_indices
    public :: is_binary_op
    public :: get_binary_op_info
    public :: is_literal
    public :: get_literal_info
    public :: is_identifier
    public :: get_identifier_name
    public :: get_declaration_initializer
    public :: get_derived_type_components
    public :: get_derived_type_parameters
    public :: get_declaration_type_parameters
    public :: type_parameter_t
    public :: PARAM_UNKNOWN, PARAM_KIND, PARAM_LEN
    public :: get_array_literal_elements
    public :: get_import_list
    public :: get_interface_block_body
    public :: has_bind_c_attribute
    public :: get_bind_c_name
    public :: get_select_case_info
    public :: get_case_block_info
    public :: get_case_default_body
    public :: get_case_range_info
    public :: get_select_type_info
    public :: get_type_guard_info
    public :: get_dummy_allocatable_attribute
    public :: get_alternate_return_label
    public :: get_return_selector
    public :: is_alternate_return_dummy
    public :: get_program_body_info
    public :: get_module_body_info
    public :: get_function_body_info
    public :: get_subroutine_body_info
    public :: get_used_modules
    public :: get_defined_module
    public :: used_module_t
    public :: defined_module_t
    public :: program_unit_query_t, declaration_query_t
    public :: derived_type_query_t, type_binding_query_t
    public :: use_statement_query_t, interface_query_t
    public :: visibility_query_t, namelist_query_t, data_statement_query_t
    public :: common_block_query_t, enum_query_t
    public :: statement_function_query_t, block_data_query_t
    public :: query_program_units, query_program_unit
    public :: query_declarations, query_declaration
    public :: query_derived_type, query_type_binding
    public :: query_use_statement, query_use_statements
    public :: query_interface, query_visibility, query_namelist
    public :: query_data_statement, query_common_block, query_enum
    public :: query_statement_function, query_block_data
    public :: array_slice_query_t, array_bounds_query_t, range_expression_query_t
    public :: component_access_query_t, array_literal_query_t
    public :: pointer_assignment_query_t, nullify_query_t
    public :: query_array_slice, query_array_bounds, query_range_expression, &
        query_component_access, query_array_literal, query_pointer_assignment, &
        query_nullify

    ! Derived-type parameter formal (issue #2952)
    type :: type_parameter_t
        character(len=:), allocatable :: name
        integer :: classification = PARAM_UNKNOWN ! PARAM_KIND / PARAM_LEN
        integer :: default_index = 0 ! Arena index of default value, or 0
    end type type_parameter_t

    type :: array_slice_query_t
        logical :: found = .false.
        integer :: base_node_index = 0
        integer, allocatable :: bounds_node_indices(:)
        logical :: is_character_substring = .false.
    end type array_slice_query_t
    type :: array_bounds_query_t
        logical :: found = .false.
        integer :: lower_bound_node_index = 0, upper_bound_node_index = 0
        integer :: stride_node_index = 0
        logical :: is_assumed_shape = .false., is_deferred_shape = .false.
        logical :: is_assumed_size = .false., is_assumed_rank = .false.
    end type array_bounds_query_t
    type :: range_expression_query_t
        logical :: found = .false.
        integer :: start_node_index = 0, end_node_index = 0
        integer :: stride_node_index = 0
    end type range_expression_query_t
    type :: component_access_query_t
        logical :: found = .false.
        integer :: base_node_index = 0
        character(len=:), allocatable :: component_name
    end type component_access_query_t
    type :: array_literal_query_t
        logical :: found = .false.
        integer, allocatable :: element_node_indices(:)
        character(len=:), allocatable :: element_type, type_spec, syntax_style
    end type array_literal_query_t
    type :: pointer_assignment_query_t
        logical :: found = .false.
        integer :: pointer_node_index = 0, target_node_index = 0
    end type pointer_assignment_query_t
    type :: nullify_query_t
        logical :: found = .false.
        integer, allocatable :: pointer_node_indices(:)
    end type nullify_query_t

    type :: used_module_t
        character(len=:), allocatable :: module_name
        character(len=:), allocatable :: only_list(:)
        character(len=:), allocatable :: rename_list(:)
        logical :: has_only = .false.
        logical :: is_intrinsic = .false.
    end type used_module_t

    type :: defined_module_t
        character(len=:), allocatable :: name
        logical :: is_submodule = .false.
        character(len=:), allocatable :: parent_identifier
    end type defined_module_t

    ! Compiler-facing records contain values and indices copied out of the
    ! arena. They deliberately do not expose the concrete AST node types.
    type :: program_unit_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: parent_node_index = 0
        integer :: line = 0
        integer :: column = 0
        character(len=:), allocatable :: unit_kind
        character(len=:), allocatable :: name
        character(len=:), allocatable :: parent_identifier
        character(len=:), allocatable :: result_name
        character(len=:), allocatable :: return_type
        character(len=:), allocatable :: bind_c_clause
        character(len=:), allocatable :: header_label
        character(len=:), allocatable :: end_label
        logical :: has_contains = .false.
        logical :: is_abstract = .false.
        logical :: is_recursive = .false.
        integer, allocatable :: declaration_indices(:)
        integer, allocatable :: procedure_indices(:)
        integer, allocatable :: parameter_indices(:)
        integer, allocatable :: body_indices(:)
        integer, allocatable :: statement_indices(:)
    end type program_unit_query_t

    type :: declaration_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: line = 0
        integer :: column = 0
        character(len=:), allocatable :: name
        character(len=:), allocatable :: names(:)
        character(len=:), allocatable :: type_name
        character(len=:), allocatable :: character_length_expr
        character(len=:), allocatable :: intent
        character(len=:), allocatable :: accessibility
        character(len=:), allocatable :: bind_name
        integer :: kind_value = 0
        integer :: intent_type = 0
        integer :: initializer_index = 0
        logical :: is_parameter_declaration = .false.
        logical :: has_kind = .false.
        logical :: has_character_length = .false.
        logical :: has_intent = .false.
        logical :: has_initializer = .false.
        logical :: is_optional = .false.
        logical :: is_array = .false.
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_target = .false.
        logical :: is_external = .false.
        logical :: is_parameter = .false.
        logical :: is_save = .false.
        logical :: is_volatile = .false.
        logical :: is_protected = .false.
        logical :: is_asynchronous = .false.
        logical :: is_contiguous = .false.
        logical :: is_value = .false.
        logical :: is_bind_c = .false.
        logical :: is_inferred = .false.
        integer, allocatable :: dimension_indices(:)
    end type declaration_query_t

    type :: derived_type_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: line = 0
        integer :: column = 0
        character(len=:), allocatable :: name
        character(len=:), allocatable :: extends_parent
        character(len=:), allocatable :: attribute_clause
        logical :: has_attributes = .false.
        logical :: has_parameters = .false.
        logical :: has_contains = .false.
        integer, allocatable :: component_indices(:)
        integer, allocatable :: parameter_indices(:)
        integer, allocatable :: binding_indices(:)
    end type derived_type_query_t

    type :: type_binding_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: line = 0
        integer :: column = 0
        character(len=:), allocatable :: binding_name
        character(len=:), allocatable :: implementation
        character(len=:), allocatable :: interface_name
        character(len=:), allocatable :: pass_name
        character(len=:), allocatable :: accessibility
        logical :: is_generic = .false.
        logical :: is_final = .false.
        logical :: is_deferred = .false.
        logical :: pass_arg = .true.
        character(len=:), allocatable :: generic_names(:)
    end type type_binding_query_t

    type :: use_statement_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: line = 0
        integer :: column = 0
        character(len=:), allocatable :: module_name
        character(len=:), allocatable :: url_spec
        character(len=:), allocatable :: only_list(:)
        character(len=:), allocatable :: rename_list(:)
        logical :: has_only = .false.
        logical :: has_double_colon = .false.
        logical :: is_intrinsic = .false.
        logical :: is_non_intrinsic = .false.
    end type use_statement_query_t

    type :: interface_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: line = 0
        integer :: column = 0
        character(len=:), allocatable :: name
        character(len=:), allocatable :: kind
        character(len=:), allocatable :: operator
        logical :: is_abstract = .false.
        integer, allocatable :: procedure_indices(:)
    end type interface_query_t

    type :: visibility_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: line = 0
        integer :: column = 0
        logical :: is_private = .false.
        logical :: has_list = .false.
        logical :: has_double_colon = .false.
        character(len=:), allocatable :: names(:)
    end type visibility_query_t

    type :: namelist_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: line = 0
        integer :: column = 0
        character(len=:), allocatable :: group_name
        character(len=:), allocatable :: variable_names(:)
    end type namelist_query_t

    type :: data_statement_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: line = 0
        integer :: column = 0
        integer, allocatable :: object_indices(:)
        integer, allocatable :: value_indices(:)
    end type data_statement_query_t

    type :: common_block_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: line = 0
        integer :: column = 0
        character(len=:), allocatable :: block_names(:)
        character(len=:), allocatable :: member_names(:)
        integer, allocatable :: member_block(:)
    end type common_block_query_t

    type :: enum_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: line = 0
        integer :: column = 0
        logical :: is_bind_c = .false.
        character(len=:), allocatable :: enumerator_names(:)
        integer, allocatable :: enumerator_values(:)
    end type enum_query_t

    type :: statement_function_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: line = 0
        integer :: column = 0
        character(len=:), allocatable :: name
        character(len=:), allocatable :: argument_names(:)
        integer :: body_expression_index = 0
    end type statement_function_query_t

    type :: block_data_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: line = 0
        integer :: column = 0
        character(len=:), allocatable :: name
        character(len=:), allocatable :: header_label
        character(len=:), allocatable :: end_label
        integer, allocatable :: statement_indices(:)
    end type block_data_query_t

contains

    function query_array_slice(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(array_slice_query_t) :: query

        allocate (query%bounds_node_indices(0))
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (array_slice_node)
            query%found = .true.
            query%base_node_index = max(node%array_index, 0)
            if (node%num_dimensions > 0) then
                query%bounds_node_indices = &
                    node%bounds_indices(1:node%num_dimensions)
            end if
            query%is_character_substring = node%is_character_substring
        end select
    end function query_array_slice
    function query_array_bounds(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(array_bounds_query_t) :: query

        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (array_bounds_node)
            query%found = .true.
            query%lower_bound_node_index = max(node%lower_bound_index, 0)
            query%upper_bound_node_index = max(node%upper_bound_index, 0)
            query%stride_node_index = max(node%stride_index, 0)
            query%is_assumed_shape = node%is_assumed_shape
            query%is_deferred_shape = node%is_deferred_shape
            query%is_assumed_size = node%is_assumed_size
            query%is_assumed_rank = node%is_assumed_rank
        end select
    end function query_array_bounds
    function query_range_expression(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(range_expression_query_t) :: query

        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (range_expression_node)
            query%found = .true.
            query%start_node_index = max(node%start_index, 0)
            query%end_node_index = max(node%end_index, 0)
            query%stride_node_index = max(node%stride_index, 0)
        end select
    end function query_range_expression
    function query_component_access(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(component_access_query_t) :: query

        call set_empty(query%component_name)
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (component_access_node)
            query%found = .true.
            query%base_node_index = max(node%base_expr_index, 0)
            if (allocated(node%component_name)) query%component_name = &
                node%component_name
        end select
    end function query_component_access
    function query_array_literal(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(array_literal_query_t) :: query

        allocate (query%element_node_indices(0))
        call set_empty(query%element_type)
        call set_empty(query%type_spec)
        call set_empty(query%syntax_style)
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (array_literal_node)
            query%found = .true.
            if (allocated(node%element_indices)) then
                query%element_node_indices = node%element_indices
            end if
            if (allocated(node%element_type)) query%element_type = node%element_type
            if (allocated(node%type_spec)) query%type_spec = node%type_spec
            if (allocated(node%syntax_style)) query%syntax_style = node%syntax_style
        end select
    end function query_array_literal

    function query_pointer_assignment(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(pointer_assignment_query_t) :: query

        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (pointer_assignment_node)
            query%found = .true.
            query%pointer_node_index = max(node%pointer_index, 0)
            query%target_node_index = max(node%target_index, 0)
        end select
    end function query_pointer_assignment

    function query_nullify(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(nullify_query_t) :: query

        allocate (query%pointer_node_indices(0))
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (nullify_node)
            query%found = .true.
            if (allocated(node%pointer_indices)) then
                query%pointer_node_indices = node%pointer_indices
            end if
        end select
    end function query_nullify

    logical function is_identifier(arena, node_index) result(is_id)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_id = .false.
        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
            type is (identifier_node)
            is_id = .true.
        end select
    end function is_identifier

    subroutine get_identifier_name(arena, node_index, name, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(name)
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'identifier index does not reference an AST node'
            return
        end if

        select type (node => arena%entries(node_index)%node)
            type is (identifier_node)
            if (allocated(node%name)) name = node%name
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not an identifier'
        end select
    end subroutine get_identifier_name

    logical function is_literal(arena, node_index) result(is_lit)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_lit = .false.
        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
            type is (literal_node)
            is_lit = .true.
        end select
    end function is_literal

    subroutine get_literal_info(arena, node_index, value, literal_type, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: value
        character(len=:), allocatable, intent(out) :: literal_type
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(value)
        call set_empty(literal_type)
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'literal index does not reference an AST node'
            return
        end if

        select type (node => arena%entries(node_index)%node)
            type is (literal_node)
            if (allocated(node%value)) value = node%value
            if (allocated(node%literal_type)) literal_type = node%literal_type
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not a literal'
        end select
    end subroutine get_literal_info

    logical function is_binary_op(arena, node_index) result(is_op)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_op = .false.
        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
            type is (binary_op_node)
            is_op = .true.
        end select
    end function is_binary_op

    subroutine get_binary_op_info(arena, node_index, operator, left_index, &
            right_index, line, column, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: operator
        integer, intent(out) :: left_index
        integer, intent(out) :: right_index
        integer, intent(out) :: line
        integer, intent(out) :: column
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(operator)
        left_index = 0
        right_index = 0
        line = 0
        column = 0
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'binary op index does not reference an AST node'
            return
        end if

        select type (node => arena%entries(node_index)%node)
            type is (binary_op_node)
            if (allocated(node%operator)) operator = node%operator
            left_index = node%left_index
            right_index = node%right_index
            line = node%line
            column = node%column
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not a binary operation'
        end select
    end subroutine get_binary_op_info

    logical function is_subroutine_call_statement(arena, node_index) result(is_call)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_call = .false.
        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
            type is (subroutine_call_node)
            is_call = .true.
        end select
    end function is_subroutine_call_statement

    subroutine get_subroutine_call_name(arena, node_index, name, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(name)
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'subroutine call index does not reference an AST node'
            return
        end if

        select type (node => arena%entries(node_index)%node)
            type is (subroutine_call_node)
            if (.not. allocated(node%name)) then
                error_msg = 'subroutine call node has no callee name'
                return
            end if
            name = node%name
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not an explicit subroutine CALL statement'
        end select
    end subroutine get_subroutine_call_name

    subroutine get_subroutine_call_arg_indices(arena, node_index, arg_indices, &
            error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: arg_indices(:)
        character(len=:), allocatable, intent(out) :: error_msg

        allocate (arg_indices(0))
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'subroutine call index does not reference an AST node'
            return
        end if

        select type (node => arena%entries(node_index)%node)
            type is (subroutine_call_node)
            if (allocated(node%arg_indices)) then
                if (size(node%arg_indices) > 0) arg_indices = node%arg_indices
            end if
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not an explicit subroutine CALL statement'
        end select
    end subroutine get_subroutine_call_arg_indices

    ! Compiler-facing query: return the arena index of a declaration's
    ! initializer expression, or 0 if the declaration has none.
    function get_declaration_initializer(arena, decl_index) result(init_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index
        integer :: init_index

        init_index = 0
        if (.not. arena%has_node_at(decl_index)) return
        select type (node => arena%entries(decl_index)%node)
            type is (declaration_node)
            if (node%has_initializer .and. node%initializer_index > 0) then
                init_index = node%initializer_index
            end if
        end select
    end function get_declaration_initializer

    ! Compiler-facing query: return the derived-type parameter formals of a
    ! type definition, in declaration order, with their KIND/LEN
    ! classification and the arena index of their default value.
    subroutine get_derived_type_parameters(arena, type_index, params)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: type_index
        type(type_parameter_t), allocatable, intent(out) :: params(:)
        integer :: i, n

        if (.not. arena%has_node_at(type_index)) then
            allocate (params(0))
            return
        end if
        select type (node => arena%entries(type_index)%node)
            type is (derived_type_node)
            if (.not. allocated(node%param_names)) then
                allocate (params(0))
                return
            end if
            n = size(node%param_names)
            allocate (params(n))
            do i = 1, n
                params(i)%name = node%param_names(i)%s
                if (allocated(node%param_classes)) then
                    if (size(node%param_classes) >= i) then
                        params(i)%classification = node%param_classes(i)
                    end if
                end if
                if (allocated(node%param_defaults)) then
                    if (size(node%param_defaults) >= i) then
                        params(i)%default_index = node%param_defaults(i)
                    end if
                end if
            end do
        class default
            allocate (params(0))
        end select
    end subroutine get_derived_type_parameters

    ! Compiler-facing query: return the arena indices of the derived-type
    ! parameter actuals on an entity declaration, e.g. type(box_t(3, 8)).
    subroutine get_declaration_type_parameters(arena, decl_index, params)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index
        integer, allocatable, intent(out) :: params(:)

        if (.not. arena%has_node_at(decl_index)) then
            allocate (params(0))
            return
        end if
        select type (node => arena%entries(decl_index)%node)
            type is (declaration_node)
            if (allocated(node%type_param_indices)) then
                params = node%type_param_indices
            else
                allocate (params(0))
            end if
        class default
            allocate (params(0))
        end select
    end subroutine get_declaration_type_parameters

    ! Compiler-facing query: copy the component_indices of a derived
    ! type definition into an allocatable result.
    subroutine get_derived_type_components(arena, type_index, components)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: type_index
        integer, allocatable, intent(out) :: components(:)

        if (.not. arena%has_node_at(type_index)) then
            allocate (components(0))
            return
        end if
        select type (node => arena%entries(type_index)%node)
            type is (derived_type_node)
            if (allocated(node%component_indices)) then
                components = node%component_indices
            else
                allocate (components(0))
            end if
        class default
            allocate (components(0))
        end select
    end subroutine get_derived_type_components

    ! Compiler-facing query: copy the element_indices of an array
    ! literal (Fortran array constructor) into an allocatable result.
    subroutine get_array_literal_elements(arena, node_index, elements)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: elements(:)

        if (.not. arena%has_node_at(node_index)) then
            allocate (elements(0))
            return
        end if
        select type (node => arena%entries(node_index)%node)
            type is (array_literal_node)
            if (allocated(node%element_indices)) then
                elements = node%element_indices
            else
                allocate (elements(0))
            end if
        class default
            allocate (elements(0))
        end select
    end subroutine get_array_literal_elements

    ! Compiler-facing query: copy the import_list of an import_statement_node
    ! into an allocatable character array.
    subroutine get_import_list(arena, node_index, names)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: names(:)
        integer :: i, max_len

        if (.not. arena%has_node_at(node_index)) then
            allocate (character(len=1) :: names(0))
            return
        end if
        select type (node => arena%entries(node_index)%node)
            type is (import_statement_node)
            if (.not. allocated(node%import_list)) then
                allocate (character(len=1) :: names(0))
                return
            end if
            max_len = 1
            do i = 1, size(node%import_list)
                if (allocated(node%import_list(i)%s)) then
                    if (len(node%import_list(i)%s) > max_len) &
                        max_len = len(node%import_list(i)%s)
                end if
            end do
            allocate (character(len=max_len) :: names(size(node%import_list)))
            do i = 1, size(node%import_list)
                if (allocated(node%import_list(i)%s)) then
                    names(i) = node%import_list(i)%s
                else
                    names(i) = ''
                end if
            end do
        class default
            allocate (character(len=1) :: names(0))
        end select
    end subroutine get_import_list

    ! Compiler-facing query: return whether an interface_block_node has
    ! body indices and copy them out.
    subroutine get_interface_block_body(arena, node_index, body_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: body_indices(:)

        if (.not. arena%has_node_at(node_index)) then
            allocate (body_indices(0))
            return
        end if
        select type (node => arena%entries(node_index)%node)
            type is (interface_block_node)
            if (allocated(node%procedure_indices)) then
                body_indices = node%procedure_indices
            else
                allocate (body_indices(0))
            end if
        class default
            allocate (body_indices(0))
        end select
    end subroutine get_interface_block_body

    ! Compiler-facing query: whether a function/subroutine definition has
    ! a bind(c) clause.
    function has_bind_c_attribute(arena, node_index) result(present_flag)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical :: present_flag

        present_flag = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (function_def_node)
            present_flag = allocated(node%bind_c_clause)
            type is (subroutine_def_node)
            present_flag = allocated(node%bind_c_clause)
        end select
    end function has_bind_c_attribute

    ! Compiler-facing query: return the bind-name from a bind(c, name="...")
    ! clause if specified; empty string otherwise.
    function get_bind_c_name(arena, node_index) result(bind_name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: bind_name

        bind_name = ''
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (function_def_node)
            if (allocated(node%bind_c_clause)) bind_name = node%bind_c_clause
            type is (subroutine_def_node)
            if (allocated(node%bind_c_clause)) bind_name = node%bind_c_clause
        end select
    end function get_bind_c_name

    ! Compiler-facing query: return the selector index, the case-arm arena
    ! indices, and the optional default-arm arena index of a select_case_node.
    subroutine get_select_case_info(arena, node_index, selector_index, &
            case_indices, default_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: selector_index
        integer, allocatable, intent(out) :: case_indices(:)
        integer, intent(out) :: default_index

        selector_index = 0
        default_index = 0
        if (.not. arena%has_node_at(node_index)) then
            allocate (case_indices(0))
            return
        end if
        select type (node => arena%entries(node_index)%node)
            type is (select_case_node)
            selector_index = node%selector_index
            default_index = node%default_index
            if (allocated(node%case_indices)) then
                case_indices = node%case_indices
            else
                allocate (case_indices(0))
            end if
        class default
            allocate (case_indices(0))
        end select
    end subroutine get_select_case_info

    ! Compiler-facing query: copy a case_block_node's case-value and body
    ! indices.
    subroutine get_case_block_info(arena, node_index, value_indices, &
            body_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: value_indices(:)
        integer, allocatable, intent(out) :: body_indices(:)

        if (.not. arena%has_node_at(node_index)) then
            allocate (value_indices(0))
            allocate (body_indices(0))
            return
        end if
        select type (node => arena%entries(node_index)%node)
            type is (case_block_node)
            if (allocated(node%value_indices)) then
                value_indices = node%value_indices
            else
                allocate (value_indices(0))
            end if
            if (allocated(node%body_indices)) then
                body_indices = node%body_indices
            else
                allocate (body_indices(0))
            end if
        class default
            allocate (value_indices(0))
            allocate (body_indices(0))
        end select
    end subroutine get_case_block_info

    ! Compiler-facing query: copy a case_default_node's body indices.
    subroutine get_case_default_body(arena, node_index, body_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: body_indices(:)

        if (.not. arena%has_node_at(node_index)) then
            allocate (body_indices(0))
            return
        end if
        select type (node => arena%entries(node_index)%node)
            type is (case_default_node)
            if (allocated(node%body_indices)) then
                body_indices = node%body_indices
            else
                allocate (body_indices(0))
            end if
        class default
            allocate (body_indices(0))
        end select
    end subroutine get_case_default_body

    ! Compiler-facing query: return the inclusive start/end integer values of
    ! a case_range_node.
    subroutine get_case_range_info(arena, node_index, start_value, end_value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: start_value
        integer, intent(out) :: end_value

        start_value = 0
        end_value = 0
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (case_range_node)
            start_value = node%start_value
            end_value = node%end_value
        end select
    end subroutine get_case_range_info

    ! Compiler-facing query: return the selector index, the type-guard arena
    ! indices, and the optional class-default arena index of a
    ! select_type_node.
    subroutine get_select_type_info(arena, node_index, selector_index, &
            guard_indices, default_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: selector_index
        integer, allocatable, intent(out) :: guard_indices(:)
        integer, intent(out) :: default_index

        selector_index = 0
        default_index = 0
        if (.not. arena%has_node_at(node_index)) then
            allocate (guard_indices(0))
            return
        end if
        select type (node => arena%entries(node_index)%node)
            type is (select_type_node)
            selector_index = node%selector_index
            default_index = node%default_index
            if (allocated(node%guard_indices)) then
                guard_indices = node%guard_indices
            else
                allocate (guard_indices(0))
            end if
        class default
            allocate (guard_indices(0))
        end select
    end subroutine get_select_type_info

    ! Compiler-facing query: return the guard kind string, the type-name
    ! identifier arena index, and the body statement indices of a
    ! type_guard_block_node.
    subroutine get_type_guard_info(arena, node_index, guard_type, &
            type_name_index, body_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: guard_type
        integer, intent(out) :: type_name_index
        integer, allocatable, intent(out) :: body_indices(:)

        type_name_index = 0
        guard_type = ''
        if (.not. arena%has_node_at(node_index)) then
            allocate (body_indices(0))
            return
        end if
        select type (node => arena%entries(node_index)%node)
            type is (type_guard_block_node)
            guard_type = trim(node%guard_type)
            type_name_index = node%type_name_index
            if (allocated(node%body_indices)) then
                body_indices = node%body_indices
            else
                allocate (body_indices(0))
            end if
        class default
            allocate (body_indices(0))
        end select
    end subroutine get_type_guard_info

    ! Compiler-facing query: whether a declaration carries the allocatable
    ! attribute.
    function get_dummy_allocatable_attribute(arena, node_index) result(is_alloc)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical :: is_alloc

        is_alloc = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (declaration_node)
            is_alloc = node%is_allocatable
            type is (parameter_declaration_node)
            is_alloc = .false.
        end select
    end function get_dummy_allocatable_attribute

    ! Compiler-facing query: statement label of an alternate return spec
    ! (`*<label>` actual argument).  Returns 0 for any other node kind.
    function get_alternate_return_label(arena, node_index) result(label)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: label

        label = 0
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (alt_return_spec_node)
            label = node%label_value
        end select
    end function get_alternate_return_label

    ! Compiler-facing query: alternate-return selector of a RETURN statement.
    ! A plain RETURN reports has_selector = .false., which stays distinct from
    ! RETURN 0.
    subroutine get_return_selector(arena, node_index, has_selector, &
            selector_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical, intent(out) :: has_selector
        integer, intent(out) :: selector_index

        has_selector = .false.
        selector_index = 0
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (return_node)
            has_selector = node%has_selector
            selector_index = node%selector_index
        end select
    end subroutine get_return_selector

    ! Compiler-facing query: whether a dummy argument is an alternate-return
    ! slot (`*` in the dummy argument list).
    function is_alternate_return_dummy(arena, node_index) result(is_alt)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical :: is_alt

        is_alt = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (parameter_declaration_node)
            is_alt = node%is_alternate_return
        end select
    end function is_alternate_return_dummy

    ! Compiler-facing query: return the name and body statement indices of a
    ! program_node.  Wrong node kind returns zero-length body_indices, an empty
    ! name, and a non-empty error_msg naming the expected node kind.
    subroutine get_program_body_info(arena, node_index, name, body_indices, &
            error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: body_indices(:)
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(name)
        if (allocated(body_indices)) deallocate (body_indices)
        allocate (body_indices(0))
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'program body index does not reference an AST node'
            return
        end if
        select type (node => arena%entries(node_index)%node)
            type is (program_node)
            if (allocated(node%name)) name = node%name
            if (allocated(node%body_indices)) then
                body_indices = node%body_indices
            end if
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not a program; expected program_node'
        end select
    end subroutine get_program_body_info

    ! Compiler-facing query: return the name, declaration indices, and
    ! procedure (contains-section) indices of a module_node.
    subroutine get_module_body_info(arena, node_index, name, &
            declaration_indices, procedure_indices, &
            error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: declaration_indices(:)
        integer, allocatable, intent(out) :: procedure_indices(:)
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(name)
        if (allocated(declaration_indices)) deallocate (declaration_indices)
        if (allocated(procedure_indices)) deallocate (procedure_indices)
        allocate (declaration_indices(0))
        allocate (procedure_indices(0))
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'module body index does not reference an AST node'
            return
        end if
        select type (node => arena%entries(node_index)%node)
            type is (module_node)
            if (allocated(node%name)) name = node%name
            if (allocated(node%declaration_indices)) then
                declaration_indices = node%declaration_indices
            end if
            if (allocated(node%procedure_indices)) then
                procedure_indices = node%procedure_indices
            end if
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not a module; expected module_node'
        end select
    end subroutine get_module_body_info

    ! Compiler-facing query: return the name, parameter indices, body indices,
    ! and result-variable name of a function_def_node.
    subroutine get_function_body_info(arena, node_index, name, param_indices, &
            body_indices, result_name, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: param_indices(:)
        integer, allocatable, intent(out) :: body_indices(:)
        character(len=:), allocatable, intent(out) :: result_name
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(name)
        call set_empty(result_name)
        if (allocated(param_indices)) deallocate (param_indices)
        if (allocated(body_indices)) deallocate (body_indices)
        allocate (param_indices(0))
        allocate (body_indices(0))
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'function body index does not reference an AST node'
            return
        end if
        select type (node => arena%entries(node_index)%node)
            type is (function_def_node)
            if (allocated(node%name)) name = node%name
            if (allocated(node%param_indices)) then
                param_indices = node%param_indices
            end if
            if (allocated(node%body_indices)) then
                body_indices = node%body_indices
            end if
            if (allocated(node%result_variable)) result_name = node%result_variable
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not a function; expected function_def_node'
        end select
    end subroutine get_function_body_info

    ! Compiler-facing query: return the name, parameter indices, and body
    ! indices of a subroutine_def_node.
    subroutine get_subroutine_body_info(arena, node_index, name, param_indices, &
            body_indices, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: param_indices(:)
        integer, allocatable, intent(out) :: body_indices(:)
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(name)
        if (allocated(param_indices)) deallocate (param_indices)
        if (allocated(body_indices)) deallocate (body_indices)
        allocate (param_indices(0))
        allocate (body_indices(0))
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'subroutine body index does not reference an AST node'
            return
        end if
        select type (node => arena%entries(node_index)%node)
            type is (subroutine_def_node)
            if (allocated(node%name)) name = node%name
            if (allocated(node%param_indices)) then
                param_indices = node%param_indices
            end if
            if (allocated(node%body_indices)) then
                body_indices = node%body_indices
            end if
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not a subroutine; expected subroutine_def_node'
        end select
    end subroutine get_subroutine_body_info

    subroutine get_used_modules(arena, modules)
        type(ast_arena_t), intent(in) :: arena
        type(used_module_t), allocatable, intent(out) :: modules(:)
        integer :: i, count, k

        count = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (use_statement_node)
                count = count + 1
            end select
        end do

        allocate (modules(count))
        k = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (use_statement_node)
                k = k + 1
                if (allocated(node%module_name)) then
                    modules(k)%module_name = node%module_name
                else
                    modules(k)%module_name = ''
                end if
                modules(k)%has_only = node%has_only
                modules(k)%is_intrinsic = node%is_intrinsic
                call copy_string_t_array(node%only_list, modules(k)%only_list)
                call copy_string_t_array(node%rename_list, modules(k)%rename_list)
            end select
        end do
    end subroutine get_used_modules

    subroutine get_defined_module(arena, module_info, error_msg)
        type(ast_arena_t), intent(in) :: arena
        type(defined_module_t), intent(out) :: module_info
        character(len=:), allocatable, intent(out) :: error_msg
        integer :: i

        call set_empty(error_msg)
        module_info%name = ''
        module_info%is_submodule = .false.
        module_info%parent_identifier = ''

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (module_node)
                if (allocated(node%name)) then
                    module_info%name = node%name
                else
                    module_info%name = ''
                end if
                module_info%is_submodule = .false.
                call set_empty(error_msg)
                return
                type is (submodule_node)
                if (allocated(node%name)) then
                    module_info%name = node%name
                else
                    module_info%name = ''
                end if
                module_info%is_submodule = .true.
                if (allocated(node%parent_identifier)) then
                    module_info%parent_identifier = node%parent_identifier
                end if
                call set_empty(error_msg)
                return
            end select
        end do

        error_msg = 'no module or submodule definition found in arena'
    end subroutine get_defined_module

    recursive function query_program_units(arena, root_index) result(units)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in), optional :: root_index
        type(program_unit_query_t), allocatable :: units(:)

        integer, allocatable :: indices(:)
        integer :: i, count, k
        logical :: selected_root

        allocate (indices(0))
        selected_root = .false.

        if (present(root_index)) then
            if (.not. arena%has_node_at(root_index)) then
                allocate (units(0))
                return
            end if
            selected_root = .true.
            select type (root => arena%entries(root_index)%node)
                type is (multi_unit_container_node)
                if (allocated(root%body_indices)) then
                    count = count_program_units(arena, root%body_indices)
                    if (allocated(indices)) deallocate (indices)
                    allocate (indices(count))
                    k = 0
                    do i = 1, size(root%body_indices)
                        if (.not. is_program_unit_node(arena, &
                            root%body_indices(i))) cycle
                        k = k + 1
                        indices(k) = root%body_indices(i)
                    end do
                end if
            class default
                if (is_program_unit_node(arena, root_index)) then
                    if (allocated(indices)) deallocate (indices)
                    allocate (indices(1))
                    indices(1) = root_index
                end if
            end select
        end if

        if (.not. selected_root) then
            do i = 1, arena%size
                if (.not. arena%has_node_at(i)) cycle
                if (arena%entries(i)%parent_index /= 0) cycle
                select type (root => arena%entries(i)%node)
                    type is (multi_unit_container_node)
                    units = query_program_units(arena, i)
                    return
                end select
            end do
            count = 0
            do i = 1, arena%size
                if (.not. arena%has_node_at(i)) cycle
                if (arena%entries(i)%parent_index /= 0) cycle
                if (is_program_unit_node(arena, i)) count = count + 1
            end do
            if (allocated(indices)) deallocate (indices)
            allocate (indices(count))
            k = 0
            do i = 1, arena%size
                if (.not. arena%has_node_at(i)) cycle
                if (arena%entries(i)%parent_index /= 0) cycle
                if (.not. is_program_unit_node(arena, i)) cycle
                k = k + 1
                indices(k) = i
            end do
        end if

        allocate (units(size(indices)))
        do i = 1, size(indices)
            units(i) = query_program_unit(arena, indices(i))
        end do
    end function query_program_units

    function query_program_unit(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(program_unit_query_t) :: query

        call initialize_program_unit_query(query)
        if (.not. arena%has_node_at(node_index)) return

        query%node_index = node_index
        query%parent_node_index = arena%entries(node_index)%parent_index
        query%line = arena%get_node_line(node_index)
        query%column = arena%get_node_column(node_index)

        select type (node => arena%entries(node_index)%node)
            type is (program_node)
            query%found = .true.
            query%unit_kind = 'program'
            if (allocated(node%name)) query%name = node%name
            call copy_integer_array(node%body_indices, query%body_indices)
            type is (module_node)
            query%found = .true.
            query%unit_kind = 'module'
            if (allocated(node%name)) query%name = node%name
            query%has_contains = node%has_contains
            call copy_integer_array(node%declaration_indices, &
                query%declaration_indices)
            call copy_integer_array(node%procedure_indices, query%procedure_indices)
            type is (submodule_node)
            query%found = .true.
            query%unit_kind = 'submodule'
            if (allocated(node%name)) query%name = node%name
            if (allocated(node%parent_identifier)) then
                query%parent_identifier = node%parent_identifier
            end if
            query%has_contains = node%has_contains
            call copy_integer_array(node%declaration_indices, &
                query%declaration_indices)
            call copy_integer_array(node%procedure_indices, query%procedure_indices)
            type is (block_data_node)
            query%found = .true.
            query%unit_kind = 'block_data'
            if (allocated(node%name)) query%name = node%name
            if (allocated(node%header_label)) query%header_label = node%header_label
            if (allocated(node%end_label)) query%end_label = node%end_label
            call copy_integer_array(node%statement_indices, &
                query%statement_indices)
            type is (function_def_node)
            query%found = .true.
            query%unit_kind = 'function'
            if (allocated(node%name)) query%name = node%name
            if (allocated(node%result_variable)) query%result_name = &
                node%result_variable
            if (allocated(node%return_type)) query%return_type = node%return_type
            if (allocated(node%bind_c_clause)) query%bind_c_clause = &
                node%bind_c_clause
            query%is_recursive = node%is_recursive
            call copy_integer_array(node%param_indices, query%parameter_indices)
            call copy_integer_array(node%body_indices, query%body_indices)
            type is (subroutine_def_node)
            query%found = .true.
            query%unit_kind = 'subroutine'
            if (allocated(node%name)) query%name = node%name
            if (allocated(node%bind_c_clause)) query%bind_c_clause = &
                node%bind_c_clause
            query%is_recursive = node%is_recursive
            call copy_integer_array(node%param_indices, query%parameter_indices)
            call copy_integer_array(node%body_indices, query%body_indices)
            type is (interface_block_node)
            query%found = .true.
            query%unit_kind = 'interface'
            if (allocated(node%name)) query%name = node%name
            query%is_abstract = node%is_abstract
            call copy_integer_array(node%procedure_indices, &
                query%procedure_indices)
            type is (multi_unit_container_node)
            query%found = .true.
            query%unit_kind = 'multi_unit_container'
            call copy_integer_array(node%body_indices, query%body_indices)
        end select
    end function query_program_unit

    function query_declarations(arena, parent_index) result(queries)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: parent_index
        type(declaration_query_t), allocatable :: queries(:)

        integer, allocatable :: children(:)
        integer :: i, count, k

        children = arena%get_children(parent_index)
        if (size(children) > 0) then
            count = 0
            do i = 1, size(children)
                if (is_declaration_query_node(arena, children(i))) count = count + 1
            end do

            allocate (queries(count))
            k = 0
            do i = 1, size(children)
                if (.not. is_declaration_query_node(arena, children(i))) cycle
                k = k + 1
                queries(k) = query_declaration(arena, children(i))
            end do
            return
        end if

        count = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (arena%entries(i)%parent_index /= parent_index) cycle
            if (is_declaration_query_node(arena, i)) count = count + 1
        end do
        allocate (queries(count))
        k = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (arena%entries(i)%parent_index /= parent_index) cycle
            if (.not. is_declaration_query_node(arena, i)) cycle
            k = k + 1
            queries(k) = query_declaration(arena, i)
        end do
    end function query_declarations

    function query_declaration(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(declaration_query_t) :: query

        call initialize_declaration_query(query)
        if (.not. arena%has_node_at(node_index)) return

        query%node_index = node_index
        query%line = arena%get_node_line(node_index)
        query%column = arena%get_node_column(node_index)
        select type (node => arena%entries(node_index)%node)
            type is (declaration_node)
            query%found = .true.
            if (allocated(node%var_name)) query%name = node%var_name
            if (allocated(node%var_names)) then
                query%names = node%var_names
            else
                call copy_single_name(query%name, query%names)
            end if
            if (allocated(node%type_name)) query%type_name = node%type_name
            if (allocated(node%character_length_expr)) then
                query%character_length_expr = node%character_length_expr
            end if
            if (allocated(node%intent)) query%intent = node%intent
            if (allocated(node%accessibility)) query%accessibility = &
                node%accessibility
            if (allocated(node%bind_name)) query%bind_name = node%bind_name
            query%kind_value = node%kind_value
            query%has_kind = node%has_kind
            query%has_character_length = node%has_character_length
            query%has_intent = node%has_intent
            query%initializer_index = node%initializer_index
            query%has_initializer = node%has_initializer
            query%is_optional = node%is_optional
            query%is_array = node%is_array
            query%is_allocatable = node%is_allocatable
            query%is_pointer = node%is_pointer
            query%is_target = node%is_target
            query%is_external = node%is_external
            query%is_parameter = node%is_parameter
            query%is_save = node%is_save
            query%is_volatile = node%is_volatile
            query%is_protected = node%is_protected
            query%is_asynchronous = node%is_asynchronous
            query%is_contiguous = node%is_contiguous
            query%is_value = node%is_value
            query%is_bind_c = node%is_bind_c
            query%is_inferred = node%is_inferred
            call copy_integer_array(node%dimension_indices, &
                query%dimension_indices)
            type is (parameter_declaration_node)
            query%found = .true.
            query%is_parameter_declaration = .true.
            if (allocated(node%name)) query%name = node%name
            call copy_single_name(query%name, query%names)
            if (allocated(node%type_name)) query%type_name = node%type_name
            if (allocated(node%character_length_expr)) then
                query%character_length_expr = node%character_length_expr
            end if
            query%kind_value = node%kind_value
            query%intent_type = node%intent_type
            query%has_kind = node%has_kind
            query%has_character_length = node%has_character_length
            query%is_optional = node%is_optional
            query%is_array = node%is_array
            query%is_target = node%is_target
            query%is_value = .false.
            call copy_integer_array(node%dimension_indices, &
                query%dimension_indices)
        end select
    end function query_declaration

    function query_derived_type(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(derived_type_query_t) :: query

        call initialize_derived_type_query(query)
        if (.not. arena%has_node_at(node_index)) return
        query%node_index = node_index
        query%line = arena%get_node_line(node_index)
        query%column = arena%get_node_column(node_index)
        select type (node => arena%entries(node_index)%node)
            type is (derived_type_node)
            query%found = .true.
            if (allocated(node%name)) query%name = node%name
            if (allocated(node%extends_parent)) query%extends_parent = &
                node%extends_parent
            if (allocated(node%attribute_clause)) query%attribute_clause = &
                node%attribute_clause
            query%has_attributes = node%has_attributes
            query%has_parameters = node%has_parameters
            query%has_contains = node%has_contains
            call copy_integer_array(node%component_indices, query%component_indices)
            call copy_integer_array(node%param_indices, query%parameter_indices)
            call copy_integer_array(node%binding_indices, query%binding_indices)
        end select
    end function query_derived_type

    function query_type_binding(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(type_binding_query_t) :: query

        call initialize_type_binding_query(query)
        if (.not. arena%has_node_at(node_index)) return
        query%node_index = node_index
        query%line = arena%get_node_line(node_index)
        query%column = arena%get_node_column(node_index)
        select type (node => arena%entries(node_index)%node)
            type is (type_binding_node)
            query%found = .true.
            if (allocated(node%binding_name)) query%binding_name = &
                node%binding_name
            if (allocated(node%implementation)) query%implementation = &
                node%implementation
            if (allocated(node%interface_name)) query%interface_name = &
                node%interface_name
            if (allocated(node%pass_name)) query%pass_name = node%pass_name
            if (allocated(node%accessibility)) query%accessibility = &
                node%accessibility
            query%is_generic = node%is_generic
            query%is_final = node%is_final
            query%is_deferred = node%is_deferred
            query%pass_arg = node%pass_arg
            call copy_string_t_array(node%generic_list, query%generic_names)
        end select
    end function query_type_binding

    function query_use_statement(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(use_statement_query_t) :: query

        call initialize_use_statement_query(query)
        if (.not. arena%has_node_at(node_index)) return
        query%node_index = node_index
        query%line = arena%get_node_line(node_index)
        query%column = arena%get_node_column(node_index)
        select type (node => arena%entries(node_index)%node)
            type is (use_statement_node)
            query%found = .true.
            if (allocated(node%module_name)) query%module_name = node%module_name
            if (allocated(node%url_spec)) query%url_spec = node%url_spec
            query%has_only = node%has_only
            query%has_double_colon = node%has_double_colon
            query%is_intrinsic = node%is_intrinsic
            query%is_non_intrinsic = node%is_non_intrinsic
            call copy_string_t_array(node%only_list, query%only_list)
            call copy_string_t_array(node%rename_list, query%rename_list)
        end select
    end function query_use_statement

    function query_use_statements(arena) result(queries)
        type(ast_arena_t), intent(in) :: arena
        type(use_statement_query_t), allocatable :: queries(:)

        integer :: i, count, k

        count = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (use_statement_node)
                count = count + 1
            end select
        end do
        allocate (queries(count))
        k = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (use_statement_node)
                k = k + 1
                queries(k) = query_use_statement(arena, i)
            end select
        end do
    end function query_use_statements

    function query_interface(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(interface_query_t) :: query

        call initialize_interface_query(query)
        if (.not. arena%has_node_at(node_index)) return
        query%node_index = node_index
        query%line = arena%get_node_line(node_index)
        query%column = arena%get_node_column(node_index)
        select type (node => arena%entries(node_index)%node)
            type is (interface_block_node)
            query%found = .true.
            if (allocated(node%name)) query%name = node%name
            if (allocated(node%kind)) query%kind = node%kind
            if (allocated(node%operator)) query%operator = node%operator
            query%is_abstract = node%is_abstract
            call copy_integer_array(node%procedure_indices, &
                query%procedure_indices)
        end select
    end function query_interface

    function query_visibility(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(visibility_query_t) :: query

        call initialize_visibility_query(query)
        if (.not. arena%has_node_at(node_index)) return
        query%node_index = node_index
        query%line = arena%get_node_line(node_index)
        query%column = arena%get_node_column(node_index)
        select type (node => arena%entries(node_index)%node)
            type is (visibility_statement_node)
            query%found = .true.
            query%is_private = node%is_private
            query%has_list = node%has_list
            query%has_double_colon = node%has_double_colon
            call copy_string_t_array(node%names, query%names)
        end select
    end function query_visibility

    function query_namelist(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(namelist_query_t) :: query

        call initialize_namelist_query(query)
        if (.not. arena%has_node_at(node_index)) return
        query%node_index = node_index
        query%line = arena%get_node_line(node_index)
        query%column = arena%get_node_column(node_index)
        select type (node => arena%entries(node_index)%node)
            type is (namelist_statement_node)
            query%found = .true.
            if (allocated(node%group_name)) query%group_name = node%group_name
            call copy_string_t_array(node%variable_names, query%variable_names)
        end select
    end function query_namelist

    function query_data_statement(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(data_statement_query_t) :: query

        call initialize_data_statement_query(query)
        if (.not. arena%has_node_at(node_index)) return
        query%node_index = node_index
        query%line = arena%get_node_line(node_index)
        query%column = arena%get_node_column(node_index)
        select type (node => arena%entries(node_index)%node)
            type is (data_statement_node)
            query%found = .true.
            call copy_integer_array(node%object_indices, query%object_indices)
            call copy_integer_array(node%value_indices, query%value_indices)
        end select
    end function query_data_statement

    function query_common_block(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(common_block_query_t) :: query

        call initialize_common_block_query(query)
        if (.not. arena%has_node_at(node_index)) return
        query%node_index = node_index
        query%line = arena%get_node_line(node_index)
        query%column = arena%get_node_column(node_index)
        select type (node => arena%entries(node_index)%node)
            type is (common_block_node)
            query%found = .true.
            call copy_string_t_array(node%block_names, query%block_names)
            call copy_string_t_array(node%member_names, query%member_names)
            call copy_integer_array(node%member_block, query%member_block)
        end select
    end function query_common_block

    function query_enum(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(enum_query_t) :: query

        call initialize_enum_query(query)
        if (.not. arena%has_node_at(node_index)) return
        query%node_index = node_index
        query%line = arena%get_node_line(node_index)
        query%column = arena%get_node_column(node_index)
        select type (node => arena%entries(node_index)%node)
            type is (enum_node)
            query%found = .true.
            query%is_bind_c = node%is_bind_c
            call copy_string_t_array(node%enumerator_names, &
                query%enumerator_names)
            call copy_integer_array(node%enumerator_values, &
                query%enumerator_values)
        end select
    end function query_enum

    function query_statement_function(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(statement_function_query_t) :: query

        call initialize_statement_function_query(query)
        if (.not. arena%has_node_at(node_index)) return
        query%node_index = node_index
        query%line = arena%get_node_line(node_index)
        query%column = arena%get_node_column(node_index)
        select type (node => arena%entries(node_index)%node)
            type is (statement_function_node)
            query%found = .true.
            if (allocated(node%name)) query%name = node%name
            if (allocated(node%arg_names)) query%argument_names = node%arg_names
            query%body_expression_index = node%body_expr_index
        end select
    end function query_statement_function

    function query_block_data(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(block_data_query_t) :: query

        call initialize_block_data_query(query)
        if (.not. arena%has_node_at(node_index)) return
        query%node_index = node_index
        query%line = arena%get_node_line(node_index)
        query%column = arena%get_node_column(node_index)
        select type (node => arena%entries(node_index)%node)
            type is (block_data_node)
            query%found = .true.
            if (allocated(node%name)) query%name = node%name
            if (allocated(node%header_label)) query%header_label = node%header_label
            if (allocated(node%end_label)) query%end_label = node%end_label
            call copy_integer_array(node%statement_indices, query%statement_indices)
        end select
    end function query_block_data

    logical function is_program_unit_node(arena, node_index) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        found = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (program_node)
            found = .true.
            type is (module_node)
            found = .true.
            type is (submodule_node)
            found = .true.
            type is (block_data_node)
            found = .true.
            type is (function_def_node)
            found = .true.
            type is (subroutine_def_node)
            found = .true.
            type is (interface_block_node)
            found = .true.
            type is (multi_unit_container_node)
            found = .true.
        end select
    end function is_program_unit_node

    logical function is_declaration_query_node(arena, node_index) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        found = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (declaration_node)
            found = .true.
            type is (parameter_declaration_node)
            found = .true.
        end select
    end function is_declaration_query_node

    integer function count_program_units(arena, indices) result(count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: indices(:)
        integer :: i

        count = 0
        do i = 1, size(indices)
            if (is_program_unit_node(arena, indices(i))) count = count + 1
        end do
    end function count_program_units

    subroutine initialize_program_unit_query(query)
        type(program_unit_query_t), intent(out) :: query

        call set_empty(query%unit_kind)
        call set_empty(query%name)
        call set_empty(query%parent_identifier)
        call set_empty(query%result_name)
        call set_empty(query%return_type)
        call set_empty(query%bind_c_clause)
        call set_empty(query%header_label)
        call set_empty(query%end_label)
        allocate (query%declaration_indices(0))
        allocate (query%procedure_indices(0))
        allocate (query%parameter_indices(0))
        allocate (query%body_indices(0))
        allocate (query%statement_indices(0))
    end subroutine initialize_program_unit_query

    subroutine initialize_declaration_query(query)
        type(declaration_query_t), intent(out) :: query

        call set_empty(query%name)
        call set_empty(query%type_name)
        call set_empty(query%character_length_expr)
        call set_empty(query%intent)
        call set_empty(query%accessibility)
        call set_empty(query%bind_name)
        allocate (character(len=1) :: query%names(0))
        allocate (query%dimension_indices(0))
    end subroutine initialize_declaration_query

    subroutine initialize_derived_type_query(query)
        type(derived_type_query_t), intent(out) :: query

        call set_empty(query%name)
        call set_empty(query%extends_parent)
        call set_empty(query%attribute_clause)
        allocate (query%component_indices(0))
        allocate (query%parameter_indices(0))
        allocate (query%binding_indices(0))
    end subroutine initialize_derived_type_query

    subroutine initialize_type_binding_query(query)
        type(type_binding_query_t), intent(out) :: query

        call set_empty(query%binding_name)
        call set_empty(query%implementation)
        call set_empty(query%interface_name)
        call set_empty(query%pass_name)
        call set_empty(query%accessibility)
        allocate (character(len=1) :: query%generic_names(0))
    end subroutine initialize_type_binding_query

    subroutine initialize_use_statement_query(query)
        type(use_statement_query_t), intent(out) :: query

        call set_empty(query%module_name)
        call set_empty(query%url_spec)
        allocate (character(len=1) :: query%only_list(0))
        allocate (character(len=1) :: query%rename_list(0))
    end subroutine initialize_use_statement_query

    subroutine initialize_interface_query(query)
        type(interface_query_t), intent(out) :: query

        call set_empty(query%name)
        call set_empty(query%kind)
        call set_empty(query%operator)
        allocate (query%procedure_indices(0))
    end subroutine initialize_interface_query

    subroutine initialize_visibility_query(query)
        type(visibility_query_t), intent(out) :: query

        allocate (character(len=1) :: query%names(0))
    end subroutine initialize_visibility_query

    subroutine initialize_namelist_query(query)
        type(namelist_query_t), intent(out) :: query

        call set_empty(query%group_name)
        allocate (character(len=1) :: query%variable_names(0))
    end subroutine initialize_namelist_query

    subroutine initialize_data_statement_query(query)
        type(data_statement_query_t), intent(out) :: query

        allocate (query%object_indices(0))
        allocate (query%value_indices(0))
    end subroutine initialize_data_statement_query

    subroutine initialize_common_block_query(query)
        type(common_block_query_t), intent(out) :: query

        allocate (character(len=1) :: query%block_names(0))
        allocate (character(len=1) :: query%member_names(0))
        allocate (query%member_block(0))
    end subroutine initialize_common_block_query

    subroutine initialize_enum_query(query)
        type(enum_query_t), intent(out) :: query

        allocate (character(len=1) :: query%enumerator_names(0))
        allocate (query%enumerator_values(0))
    end subroutine initialize_enum_query

    subroutine initialize_statement_function_query(query)
        type(statement_function_query_t), intent(out) :: query

        call set_empty(query%name)
        allocate (character(len=1) :: query%argument_names(0))
    end subroutine initialize_statement_function_query

    subroutine initialize_block_data_query(query)
        type(block_data_query_t), intent(out) :: query

        call set_empty(query%name)
        call set_empty(query%header_label)
        call set_empty(query%end_label)
        allocate (query%statement_indices(0))
    end subroutine initialize_block_data_query

    subroutine copy_integer_array(src, dst)
        integer, allocatable, intent(in) :: src(:)
        integer, allocatable, intent(out) :: dst(:)

        if (allocated(src)) then
            dst = src
        else
            allocate (dst(0))
        end if
    end subroutine copy_integer_array

    subroutine copy_single_name(name, names)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: names(:)

        if (len_trim(name) == 0) then
            allocate (character(len=1) :: names(0))
        else
            allocate (character(len=len(name)) :: names(1))
            names(1) = name
        end if
    end subroutine copy_single_name

    subroutine copy_string_t_array(src, dst)
        type(string_t), allocatable, intent(in) :: src(:)
        character(len=:), allocatable, intent(out) :: dst(:)
        integer :: i, max_len

        if (.not. allocated(src)) then
            allocate (character(len=1) :: dst(0))
            return
        end if
        if (size(src) == 0) then
            allocate (character(len=1) :: dst(0))
            return
        end if
        max_len = 1
        do i = 1, size(src)
            if (allocated(src(i)%s)) then
                if (len(src(i)%s) > max_len) max_len = len(src(i)%s)
            end if
        end do
        allocate (character(len=max_len) :: dst(size(src)))
        do i = 1, size(src)
            if (allocated(src(i)%s)) then
                dst(i) = src(i)%s
            else
                dst(i) = ''
            end if
        end do
    end subroutine copy_string_t_array

    subroutine set_empty(value)
        character(len=:), allocatable, intent(out) :: value

        allocate (character(len=0) :: value)
    end subroutine set_empty

end module frontend_compiler_queries
