module frontend_compiler_queries
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: string_t
    use ast_nodes_procedure, only: subroutine_call_node, function_def_node, &
        subroutine_def_node
    use ast_nodes_core, only: binary_op_node, literal_node, identifier_node, &
        array_literal_node, program_node, component_access_node, &
        call_or_subscript_node, pointer_assignment_node, assignment_node
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
        statement_function_node, allocate_statement_node, &
        deallocate_statement_node
    use ast_nodes_legacy, only: common_block_node, enum_node
    use ast_nodes_conditional, only: select_case_node, case_block_node, &
        case_default_node, case_range_node, &
        select_type_node, type_guard_block_node
    use frontend_compiler_resolution, only: declaration_binding_t, &
        resolve_identifier_binding, resolve_name_at_node, find_enclosing_scope, &
        BINDING_FUNCTION, BINDING_SUBROUTINE
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
    public :: get_construct_name
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
    public :: call_argument_query_t, call_arguments_query_t
    public :: query_call_arguments
    public :: STORAGE_LOCAL, STORAGE_OWNED, STORAGE_BORROWED, STORAGE_POINTER
    public :: STORAGE_MODULE, STORAGE_SAVE, STORAGE_COMMON
    public :: OWNERSHIP_EVENT_ALLOCATE, OWNERSHIP_EVENT_DEALLOCATE
    public :: OWNERSHIP_EVENT_POINTER_ASSIGN, OWNERSHIP_EVENT_MOVE_ALLOC
    public :: OWNERSHIP_EVENT_NULLIFY
    public :: ACCESS_READ, ACCESS_WRITE, ACCESS_READ_WRITE
    public :: storage_query_t, ownership_event_query_t, component_path_query_t
    public :: binding_resolution_query_t, global_reference_query_t
    public :: query_storage, query_ownership_events, query_component_path
    public :: query_type_binding_resolution, query_active_global_references

    integer, parameter :: STORAGE_LOCAL = 1
    integer, parameter :: STORAGE_OWNED = 2
    integer, parameter :: STORAGE_BORROWED = 3
    integer, parameter :: STORAGE_POINTER = 4
    integer, parameter :: STORAGE_MODULE = 5
    integer, parameter :: STORAGE_SAVE = 6
    integer, parameter :: STORAGE_COMMON = 7

    integer, parameter :: OWNERSHIP_EVENT_ALLOCATE = 1
    integer, parameter :: OWNERSHIP_EVENT_DEALLOCATE = 2
    integer, parameter :: OWNERSHIP_EVENT_POINTER_ASSIGN = 3
    integer, parameter :: OWNERSHIP_EVENT_MOVE_ALLOC = 4
    integer, parameter :: OWNERSHIP_EVENT_NULLIFY = 5

    integer, parameter :: ACCESS_READ = 1
    integer, parameter :: ACCESS_WRITE = 2
    integer, parameter :: ACCESS_READ_WRITE = 3

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

    ! Resolved actual-to-formal call facts.  The result is ordered by the
    ! callee's formal parameter list, so an omitted optional dummy is present
    ! as a record with is_supplied=.false. rather than being erased.
    type :: call_argument_query_t
        integer :: actual_node_index = 0
        integer :: actual_value_node_index = 0
        integer :: formal_node_index = 0
        character(len=:), allocatable :: formal_name
        logical :: is_supplied = .false.
        logical :: is_keyword = .false.
        logical :: is_optional = .false.
    end type call_argument_query_t

    type :: call_arguments_query_t
        logical :: found = .false.
        integer :: call_node_index = 0
        integer :: procedure_node_index = 0
        character(len=:), allocatable :: procedure_name
        character(len=:), allocatable :: procedure_kind
        type(call_argument_query_t), allocatable :: arguments(:)
    end type call_arguments_query_t

    ! Normalized storage facts for compiler consumers.  The existing
    ! declaration query mirrors source attributes; this record additionally
    ! gives ownership-sensitive consumers one stable classification.
    type :: storage_query_t
        logical :: found = .false.
        integer :: node_index = 0
        character(len=:), allocatable :: name
        character(len=:), allocatable :: type_name
        integer :: storage_class = STORAGE_LOCAL
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_target = .false.
        logical :: is_contiguous = .false.
        logical :: is_polymorphic = .false.
        logical :: is_unlimited_polymorphic = .false.
        logical :: is_module_state = .false.
        logical :: is_save_state = .false.
        logical :: is_common_state = .false.
    end type storage_query_t

    type :: ownership_event_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: event_kind = 0
        integer, allocatable :: object_indices(:)
        integer :: source_index = 0
        integer :: target_index = 0
        ! ALLOCATE SOURCE=/MOLD= expression indices, when present.
        integer :: source_expr_index = 0
        integer :: mold_expr_index = 0
    end type ownership_event_query_t

    type :: component_path_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: base_node_index = 0
        character(len=:), allocatable :: component_names(:)
        integer, allocatable :: component_node_indices(:)
    end type component_path_query_t

    type :: binding_resolution_query_t
        logical :: found = .false.
        character(len=:), allocatable :: requested_name
        character(len=:), allocatable :: binding_name
        character(len=:), allocatable :: implementation
        character(len=:), allocatable :: interface_name
        character(len=:), allocatable :: pass_name
        integer :: declaring_type_index = 0
        integer :: resolved_type_index = 0
        integer :: binding_node_index = 0
        logical :: is_inherited = .false.
        logical :: is_generic = .false.
        logical :: is_deferred = .false.
        logical :: is_abstract_type = .false.
        logical :: pass_arg = .true.
        character(len=:), allocatable :: generic_names(:)
        integer, allocatable :: dispatch_target_type_indices(:)
        character(len=:), allocatable :: dispatch_target_implementations(:)
    end type binding_resolution_query_t

    type :: global_reference_query_t
        logical :: found = .false.
        integer :: reference_node_index = 0
        integer :: declaration_node_index = 0
        integer :: owner_scope_index = 0
        integer :: access_kind = ACCESS_READ
        character(len=:), allocatable :: name
        character(len=:), allocatable :: module_name
        logical :: is_module_state = .false.
        logical :: is_save_state = .false.
        logical :: is_common_state = .false.
    end type global_reference_query_t

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

    function query_call_arguments(arena, call_node_index) result(query)
        !! Resolve a same-arena procedure call into formal-ordered bindings.
        !!
        !! `actual_node_index` preserves the original AST argument (including
        !! a keyword assignment wrapper); `actual_value_node_index` points at
        !! the expression passed to the dummy.  A zero actual index means the
        !! formal was omitted.  The query deliberately returns no result for
        !! array accesses, unresolved procedures, generic interfaces, or an
        !! invalid/ambiguous argument list.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        type(call_arguments_query_t) :: query

        type(declaration_binding_t) :: binding
        type(program_unit_query_t) :: procedure
        integer, allocatable :: actual_indices(:)
        integer, allocatable :: formal_actual(:)
        integer, allocatable :: formal_value(:)
        logical, allocatable :: formal_keyword(:)
        character(len=:), allocatable :: call_name
        character(len=:), allocatable :: keyword
        character(len=:), allocatable :: error_msg
        logical :: is_call, is_keyword, saw_keyword
        integer :: i, j, formal, next_formal, n_formal, actual_value
        type(declaration_query_t) :: formal_query

        call initialize_call_arguments_query(query)
        call get_call_parts(arena, call_node_index, call_name, actual_indices, &
            is_call)
        if (.not. is_call) return
        if (len_trim(call_name) == 0) return

        call resolve_name_at_node(arena, call_node_index, call_name, binding, &
            error_msg)
        if (.not. binding%found) return
        if (binding%binding_kind /= BINDING_FUNCTION .and. &
            binding%binding_kind /= BINDING_SUBROUTINE) return
        if (binding%node_index <= 0) return

        procedure = query_program_unit(arena, binding%node_index)
        if (.not. procedure%found) return
        if (procedure%unit_kind /= 'function' .and. &
            procedure%unit_kind /= 'subroutine') return
        n_formal = size(procedure%parameter_indices)
        if (size(actual_indices) > n_formal) return

        allocate (formal_actual(n_formal))
        allocate (formal_value(n_formal))
        allocate (formal_keyword(n_formal))
        formal_actual = 0
        formal_value = 0
        formal_keyword = .false.
        next_formal = 1
        saw_keyword = .false.
        do j = 1, size(actual_indices)
            call get_call_actual_info(arena, actual_indices(j), keyword, &
                actual_value, is_keyword)
            if (is_keyword) then
                saw_keyword = .true.
                formal = find_formal_name(arena, procedure%parameter_indices, &
                    keyword)
                if (formal <= 0) return
            else
                if (saw_keyword) return
                do while (next_formal <= n_formal)
                    if (formal_actual(next_formal) == 0) exit
                    next_formal = next_formal + 1
                end do
                formal = next_formal
            end if
            if (formal <= 0 .or. formal > n_formal) return
            if (formal_actual(formal) /= 0) return
            formal_actual(formal) = actual_indices(j)
            formal_value(formal) = actual_value
            formal_keyword(formal) = is_keyword
        end do

        if (allocated(query%arguments)) deallocate (query%arguments)
        allocate (query%arguments(n_formal))
        do i = 1, n_formal
            formal_query = query_declaration(arena, &
                procedure%parameter_indices(i))
            if (.not. formal_query%found) return
            query%arguments(i)%formal_node_index = &
                procedure%parameter_indices(i)
            query%arguments(i)%formal_name = formal_query%name
            query%arguments(i)%is_optional = formal_query%is_optional
            query%arguments(i)%actual_node_index = formal_actual(i)
            query%arguments(i)%is_supplied = formal_actual(i) > 0
            query%arguments(i)%is_keyword = formal_keyword(i)
            if (.not. query%arguments(i)%is_supplied .and. &
                .not. query%arguments(i)%is_optional) return
            if (query%arguments(i)%is_supplied) then
                query%arguments(i)%actual_value_node_index = formal_value(i)
            end if
        end do

        query%found = .true.
        query%call_node_index = call_node_index
        query%procedure_node_index = binding%node_index
        query%procedure_name = procedure%name
        query%procedure_kind = procedure%unit_kind
    end function query_call_arguments

    subroutine initialize_call_arguments_query(query)
        type(call_arguments_query_t), intent(out) :: query

        call set_empty(query%procedure_name)
        call set_empty(query%procedure_kind)
        allocate (query%arguments(0))
    end subroutine initialize_call_arguments_query

    subroutine get_call_parts(arena, call_node_index, name, actual_indices, &
            is_call)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        character(len=:), allocatable, intent(out) :: name
        integer, allocatable, intent(out) :: actual_indices(:)
        logical, intent(out) :: is_call

        call set_empty(name)
        allocate (actual_indices(0))
        is_call = .false.
        if (.not. arena%has_node_at(call_node_index)) return

        select type (node => arena%entries(call_node_index)%node)
            type is (subroutine_call_node)
            is_call = .true.
            if (allocated(node%name)) name = node%name
            if (allocated(node%arg_indices)) then
                actual_indices = node%arg_indices
            end if
            type is (call_or_subscript_node)
            if (node%is_array_access) return
            is_call = .true.
            if (allocated(node%name)) name = node%name
            if (allocated(node%arg_indices)) then
                actual_indices = node%arg_indices
            end if
        end select
    end subroutine get_call_parts

    subroutine get_call_actual_info(arena, actual_index, keyword, value_index, &
            is_keyword)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: actual_index
        character(len=:), allocatable, intent(out) :: keyword
        integer, intent(out) :: value_index
        logical, intent(out) :: is_keyword

        call set_empty(keyword)
        value_index = actual_index
        is_keyword = .false.
        if (.not. arena%has_node_at(actual_index)) return

        select type (actual => arena%entries(actual_index)%node)
            type is (assignment_node)
            if (actual%target_index <= 0) return
            if (.not. arena%has_node_at(actual%target_index)) return
            if (actual%value_index <= 0) return
            select type (target => arena%entries(actual%target_index)%node)
                type is (identifier_node)
                if (.not. allocated(target%name)) return
                keyword = target%name
                value_index = actual%value_index
                is_keyword = .true.
            end select
        end select
    end subroutine get_call_actual_info

    integer function find_formal_name(arena, formal_indices, name) result(index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: formal_indices(:)
        character(len=*), intent(in) :: name
        type(declaration_query_t) :: formal_query
        integer :: i

        index = 0
        do i = 1, size(formal_indices)
            formal_query = query_declaration(arena, formal_indices(i))
            if (.not. formal_query%found) cycle
            if (same_name(formal_query%name, name)) then
                index = i
                return
            end if
        end do
    end function find_formal_name

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
    ! Compiler-facing query: the construct name of a named construct
    ! ("check: if (...) then ... end if check"), or an empty string when the
    ! construct is unnamed or the node is not a construct. EXIT and CYCLE name
    ! the construct they target, so a consumer resolves them by matching their
    ! label against this over the enclosing constructs.
    function get_construct_name(arena, node_index) result(name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: name

        name = ""
        if (.not. arena%has_node_at(node_index)) return
        if (.not. allocated(arena%entries(node_index)%node%construct_name)) return
        name = trim(arena%entries(node_index)%node%construct_name)
    end function get_construct_name

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

    recursive function query_storage(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(storage_query_t) :: query
        type(declaration_query_t) :: declaration
        type(component_access_query_t) :: component
        integer :: i
        logical :: common_state

        call set_empty(query%name)
        call set_empty(query%type_name)
        if (.not. arena%has_node_at(node_index)) return
        declaration = query_declaration(arena, node_index)
        if (.not. declaration%found) then
            component = query_component_access(arena, node_index)
            if (component%found) call query_component_storage(arena, node_index, &
                component, query)
            return
        end if

        query%found = .true.
        query%node_index = node_index
        query%name = declaration%name
        query%type_name = declaration%type_name
        query%is_allocatable = declaration%is_allocatable
        query%is_pointer = declaration%is_pointer
        query%is_target = declaration%is_target
        query%is_contiguous = declaration%is_contiguous
        query%is_polymorphic = is_polymorphic_type_spec(query%type_name)
        query%is_unlimited_polymorphic = &
            is_unlimited_polymorphic_type_spec(query%type_name)
        query%is_module_state = declaration_owned_by_module(arena, node_index)
        query%is_save_state = declaration%is_save
        common_state = .false.
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (common_member_name(arena, i, declaration%name)) then
                common_state = .true.
                exit
            end if
        end do
        query%is_common_state = common_state

        if (query%is_common_state) then
            query%storage_class = STORAGE_COMMON
        else if (query%is_save_state) then
            query%storage_class = STORAGE_SAVE
        else if (query%is_module_state) then
            query%storage_class = STORAGE_MODULE
        else if (query%is_pointer) then
            query%storage_class = STORAGE_POINTER
        else if (query%is_allocatable) then
            if (len_trim(declaration%intent) > 0) then
                query%storage_class = STORAGE_BORROWED
            else
                query%storage_class = STORAGE_OWNED
            end if
        else if (len_trim(declaration%intent) > 0 .or. query%is_target) then
            query%storage_class = STORAGE_BORROWED
        else
            query%storage_class = STORAGE_LOCAL
        end if
    end function query_storage

    subroutine query_component_storage(arena, node_index, component, query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(component_access_query_t), intent(in) :: component
        type(storage_query_t), intent(out) :: query
        type(declaration_binding_t) :: binding
        type(declaration_query_t) :: base_declaration, component_declaration
        type(storage_query_t) :: base_storage
        type(component_access_query_t) :: base_component
        logical :: base_is_array_element
        type(derived_type_query_t) :: derived
        character(len=:), allocatable :: base_type, derived_name, error_msg
        character(len=:), allocatable :: base_name
        integer :: derived_index, i, component_index, scope_index
        integer :: fallback_index

        query%node_index = node_index
        base_declaration = query_declaration(arena, component%base_node_index)
        base_component = query_component_access(arena, component%base_node_index)
        base_is_array_element = is_array_element_node(arena, &
            component%base_node_index)
        if (base_declaration%found) then
            base_type = base_declaration%type_name
        else if (base_component%found) then
            base_storage = query_storage(arena, component%base_node_index)
            if (.not. base_storage%found) return
            base_type = base_storage%type_name
        else if (base_is_array_element) then
            call resolve_array_element_declaration(arena, &
                component%base_node_index, base_declaration)
            if (.not. base_declaration%found) return
            base_type = base_declaration%type_name
        else
            call resolve_identifier_binding(arena, component%base_node_index, &
                binding, error_msg)
            if (binding%found) then
                base_declaration = query_declaration(arena, &
                    binding%declaration_node_index)
            else
                call identifier_name_at(arena, component%base_node_index, base_name)
                scope_index = find_enclosing_scope(arena, node_index)
                fallback_index = 0
                do i = 1, arena%size
                    if (.not. arena%has_node_at(i)) cycle
                    base_declaration = query_declaration(arena, i)
                    if (.not. base_declaration%found) cycle
                    if (.not. same_name(base_declaration%name, base_name)) cycle
                    if (fallback_index == 0) fallback_index = i
                    if (scope_index > 0 .and. node_is_in_scope(arena, i, &
                        scope_index)) then
                        fallback_index = i
                        exit
                    end if
                end do
                if (fallback_index == 0) return
                base_declaration = query_declaration(arena, fallback_index)
            end if
            if (.not. base_declaration%found) return
            base_type = base_declaration%type_name
        end if

        derived_name = derived_type_name_from_spec(base_type)
        derived_index = find_derived_type_by_name(arena, derived_name)
        if (derived_index <= 0) return
        derived = query_derived_type(arena, derived_index)
        do i = 1, size(derived%component_indices)
            component_index = derived%component_indices(i)
            component_declaration = query_declaration(arena, component_index)
            if (.not. component_declaration%found) cycle
            if (.not. same_name(component_declaration%name, &
                component%component_name)) cycle

            query%found = .true.
            query%name = component_declaration%name
            query%type_name = component_declaration%type_name
            query%is_allocatable = component_declaration%is_allocatable
            query%is_pointer = component_declaration%is_pointer
            query%is_target = component_declaration%is_target
            query%is_contiguous = component_declaration%is_contiguous
            query%is_polymorphic = is_polymorphic_type_spec(query%type_name)
            query%is_unlimited_polymorphic = &
                is_unlimited_polymorphic_type_spec(query%type_name)
            if (query%is_pointer) then
                query%storage_class = STORAGE_POINTER
            else if (query%is_allocatable) then
                query%storage_class = STORAGE_OWNED
            else
                query%storage_class = STORAGE_LOCAL
            end if
            return
        end do
    end subroutine query_component_storage

    logical function is_array_element_node(arena, node_index) result(is_element)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_element = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (call_or_subscript_node)
            if (.not. allocated(node%arg_indices)) return
            is_element = size(node%arg_indices) > 0
        class default
        end select
    end function is_array_element_node

    subroutine resolve_array_element_declaration(arena, node_index, declaration)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(declaration_query_t), intent(out) :: declaration
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg, array_name
        integer :: i, fallback_index, scope_index

        declaration = query_declaration(arena, node_index)
        if (declaration%found) return
        call resolve_identifier_binding(arena, node_index, binding, error_msg)
        if (binding%found) then
            declaration = query_declaration(arena, binding%declaration_node_index)
            if (declaration%found .and. declaration%is_array) return
        end if

        call array_element_name_at(arena, node_index, array_name)
        if (len_trim(array_name) == 0) return
        scope_index = find_enclosing_scope(arena, node_index)
        fallback_index = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            declaration = query_declaration(arena, i)
            if (.not. declaration%found .or. .not. declaration%is_array) cycle
            if (.not. same_name(declaration%name, array_name)) cycle
            if (fallback_index == 0) fallback_index = i
            if (scope_index > 0 .and. node_is_in_scope(arena, i, &
                scope_index)) then
                fallback_index = i
                exit
            end if
        end do
        if (fallback_index > 0) then
            declaration = query_declaration(arena, fallback_index)
        else
            declaration%found = .false.
        end if
    end subroutine resolve_array_element_declaration

    subroutine array_element_name_at(arena, node_index, name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name

        call set_empty(name)
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (call_or_subscript_node)
            if (allocated(node%name)) name = node%name
        class default
        end select
    end subroutine array_element_name_at

    function derived_type_name_from_spec(type_name) result(name)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: name
        character(len=:), allocatable :: normalized
        integer :: open_pos, close_pos

        normalized = remove_type_spec_spaces(trim(type_name))
        open_pos = index(normalized, '(')
        close_pos = len(normalized)
        if (open_pos > 0 .and. close_pos > open_pos .and. &
            normalized(close_pos:close_pos) == ')') then
            allocate (character(len=close_pos - open_pos - 1) :: name)
            if (len(name) > 0) name = normalized(open_pos + 1:close_pos - 1)
        else
            allocate (character(len=len(normalized)) :: name)
            name = normalized
        end if
    end function derived_type_name_from_spec

    subroutine identifier_name_at(arena, node_index, name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name

        call set_empty(name)
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (identifier_node)
            if (allocated(node%name)) name = node%name
        class default
        end select
    end subroutine identifier_name_at

    function query_ownership_events(arena, scope_index) result(events)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        type(ownership_event_query_t), allocatable :: events(:)
        integer :: i, count

        allocate (events(0))
        if (.not. arena%has_node_at(scope_index)) return
        count = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (.not. node_is_in_scope(arena, i, scope_index)) cycle
            if (is_ownership_event(arena, i)) count = count + 1
        end do
        if (count == 0) return
        deallocate (events)
        allocate (events(count))
        count = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (.not. node_is_in_scope(arena, i, scope_index)) cycle
            if (.not. is_ownership_event(arena, i)) cycle
            count = count + 1
            events(count) = ownership_event(arena, i)
        end do
    end function query_ownership_events

    function query_component_path(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(component_path_query_t) :: query
        integer, allocatable :: indices(:)
        character(len=:), allocatable :: names(:)

        allocate (character(len=1) :: query%component_names(0))
        allocate (query%component_node_indices(0))
        if (.not. arena%has_node_at(node_index)) return
        call collect_component_path(arena, node_index, names, indices, &
            query%base_node_index)
        if (size(indices) == 0) return
        query%found = .true.
        query%node_index = node_index
        query%component_names = names
        query%component_node_indices = indices
    end function query_component_path

    function query_type_binding_resolution(arena, derived_type_index, &
            binding_name) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: derived_type_index
        character(len=*), intent(in) :: binding_name
        type(binding_resolution_query_t) :: query
        integer :: i, j, target_type
        type(binding_resolution_query_t) :: target
        type(binding_resolution_query_t) :: concrete

        call initialize_binding_resolution(query, binding_name)
        if (.not. arena%has_node_at(derived_type_index)) return
        call resolve_binding_base(arena, derived_type_index, binding_name, query)
        if (.not. query%found) return
        query%resolved_type_index = derived_type_index

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (.not. is_derived_type_at(arena, i)) cycle
            if (.not. type_extends(arena, i, derived_type_index)) cycle
            target_type = i
            call initialize_binding_resolution(target, binding_name)
            call resolve_binding_base(arena, target_type, binding_name, target)
            if (.not. target%found) cycle
            if (len_trim(target%implementation) == 0 .and. target%is_generic) then
                do j = 1, size(target%generic_names)
                    call initialize_binding_resolution(concrete, &
                        target%generic_names(j))
                    call resolve_binding_base(arena, target_type, &
                        target%generic_names(j), concrete)
                    if (concrete%found .and. &
                        len_trim(concrete%implementation) > 0) then
                        target%implementation = concrete%implementation
                        exit
                    end if
                end do
            end if
            if (len_trim(target%implementation) == 0) cycle
            call append_dispatch_target(query, target_type, target%implementation)
        end do
    end function query_type_binding_resolution

    function query_active_global_references(arena, scope_index) result(refs)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        type(global_reference_query_t), allocatable :: refs(:)
        type(declaration_binding_t) :: binding
        integer :: i, count
        character(len=:), allocatable :: error_msg

        allocate (refs(0))
        if (.not. arena%has_node_at(scope_index)) return
        count = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (.not. node_is_in_scope(arena, i, scope_index)) cycle
            if (.not. is_identifier_at(arena, i)) cycle
            call resolve_identifier_binding(arena, i, binding, error_msg)
            if (.not. binding%found) cycle
            if (.not. global_declaration(arena, binding%declaration_node_index, &
                binding%name)) cycle
            count = count + 1
        end do
        if (count == 0) return
        deallocate (refs)
        allocate (refs(count))
        count = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (.not. node_is_in_scope(arena, i, scope_index)) cycle
            if (.not. is_identifier_at(arena, i)) cycle
            call resolve_identifier_binding(arena, i, binding, error_msg)
            if (.not. binding%found) cycle
            if (.not. global_declaration(arena, binding%declaration_node_index, &
                binding%name)) cycle
            count = count + 1
            refs(count) = make_global_reference(arena, i, binding)
        end do
    end function query_active_global_references

    subroutine initialize_binding_resolution(query, requested_name)
        type(binding_resolution_query_t), intent(out) :: query
        character(len=*), intent(in) :: requested_name

        query%requested_name = trim(requested_name)
        call set_empty(query%binding_name)
        call set_empty(query%implementation)
        call set_empty(query%interface_name)
        call set_empty(query%pass_name)
        allocate (character(len=1) :: query%generic_names(0))
        allocate (query%dispatch_target_type_indices(0))
        allocate (character(len=1) :: query%dispatch_target_implementations(0))
    end subroutine initialize_binding_resolution

    recursive subroutine resolve_binding_base(arena, type_index, name, query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: type_index
        character(len=*), intent(in) :: name
        type(binding_resolution_query_t), intent(inout) :: query
        type(derived_type_query_t) :: type_query
        type(type_binding_query_t) :: binding
        type(binding_resolution_query_t) :: parent_query
        integer :: i, parent_index

        type_query = query_derived_type(arena, type_index)
        if (.not. type_query%found) return
        query%is_abstract_type = contains_word(type_query%attribute_clause, &
            'abstract')
        do i = 1, size(type_query%binding_indices)
            binding = query_type_binding(arena, type_query%binding_indices(i))
            if (.not. binding%found) cycle
            if (.not. binding_matches(binding, name)) cycle
            query%found = .true.
            query%declaring_type_index = type_index
            query%binding_node_index = binding%node_index
            query%binding_name = binding%binding_name
            query%implementation = binding%implementation
            query%interface_name = binding%interface_name
            query%pass_name = binding%pass_name
            query%is_generic = binding%is_generic
            query%is_deferred = binding%is_deferred
            query%pass_arg = binding%pass_arg
            query%generic_names = binding%generic_names
            return
        end do

        parent_index = find_derived_type_by_name(arena, type_query%extends_parent)
        if (parent_index <= 0) return
        call initialize_binding_resolution(parent_query, name)
        call resolve_binding_base(arena, parent_index, name, parent_query)
        if (.not. parent_query%found) return
        query = parent_query
        query%is_inherited = .true.
        query%is_abstract_type = contains_word(type_query%attribute_clause, &
            'abstract')
    end subroutine resolve_binding_base

    subroutine append_dispatch_target(query, type_index, implementation)
        type(binding_resolution_query_t), intent(inout) :: query
        integer, intent(in) :: type_index
        character(len=*), intent(in) :: implementation
        integer, allocatable :: int_tmp(:)
        character(len=:), allocatable :: char_tmp(:)
        integer :: n, width

        n = size(query%dispatch_target_type_indices)
        allocate (int_tmp(n + 1))
        if (n > 0) int_tmp(:n) = query%dispatch_target_type_indices
        int_tmp(n + 1) = type_index
        call move_alloc(int_tmp, query%dispatch_target_type_indices)
        width = max(1, len_trim(implementation))
        allocate (character(len=width) :: char_tmp(n + 1))
        if (n > 0) char_tmp(:n) = query%dispatch_target_implementations
        char_tmp(n + 1) = trim(implementation)
        call move_alloc(char_tmp, query%dispatch_target_implementations)
    end subroutine append_dispatch_target

    logical function binding_matches(binding, name)
        type(type_binding_query_t), intent(in) :: binding
        character(len=*), intent(in) :: name
        integer :: i

        binding_matches = same_name(binding%binding_name, name)
        if (binding_matches .or. .not. binding%is_generic) return
        do i = 1, size(binding%generic_names)
            if (same_name(binding%generic_names(i), name)) then
                binding_matches = .true.
                return
            end if
        end do
    end function binding_matches

    logical function type_extends(arena, candidate_index, base_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: candidate_index, base_index
        type(derived_type_query_t) :: candidate, base
        integer :: parent_index, guard

        type_extends = .false.
        if (candidate_index == base_index) return
        base = query_derived_type(arena, base_index)
        if (.not. base%found) return
        candidate = query_derived_type(arena, candidate_index)
        guard = 0
        do while (candidate%found .and. len_trim(candidate%extends_parent) > 0)
            parent_index = find_derived_type_by_name(arena, candidate%extends_parent)
            if (parent_index <= 0) return
            if (parent_index == base_index) then
                type_extends = .true.
                return
            end if
            candidate = query_derived_type(arena, parent_index)
            guard = guard + 1
            if (guard > arena%size) return
        end do
    end function type_extends

    integer function find_derived_type_by_name(arena, name) result(index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name
        type(derived_type_query_t) :: query
        integer :: i

        index = 0
        if (len_trim(name) == 0) return
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            query = query_derived_type(arena, i)
            if (query%found .and. same_name(query%name, name)) then
                index = i
                return
            end if
        end do
    end function find_derived_type_by_name

    logical function is_derived_type_at(arena, index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(derived_type_query_t) :: query

        query = query_derived_type(arena, index)
        is_derived_type_at = query%found
    end function is_derived_type_at

    recursive subroutine collect_component_path(arena, node_index, names, indices, base)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: names(:)
        integer, allocatable, intent(out) :: indices(:)
        integer, intent(out) :: base
        type(component_access_query_t) :: component
        character(len=:), allocatable :: prefix_names(:)
        integer, allocatable :: prefix_indices(:)
        integer :: i, width

        base = 0
        component = query_component_access(arena, node_index)
        if (.not. component%found) then
            allocate (character(len=1) :: names(0))
            allocate (indices(0))
            base = node_index
            return
        end if
        call collect_component_path(arena, component%base_node_index, &
            prefix_names, prefix_indices, base)
        width = max(1, len_trim(component%component_name))
        if (size(prefix_names) > 0) width = max(width, len(prefix_names))
        allocate (character(len=width) :: names(size(prefix_names) + 1))
        do i = 1, size(prefix_names)
            names(i) = prefix_names(i)
        end do
        names(size(prefix_names) + 1) = trim(component%component_name)
        allocate (indices(size(prefix_indices) + 1))
        if (size(prefix_indices) > 0) indices(:size(prefix_indices)) = prefix_indices
        indices(size(prefix_indices) + 1) = node_index
    end subroutine collect_component_path

    logical function is_ownership_event(arena, index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index

        is_ownership_event = .false.
        select type (node => arena%entries(index)%node)
            type is (allocate_statement_node)
            is_ownership_event = .true.
            type is (deallocate_statement_node)
            is_ownership_event = .true.
            type is (pointer_assignment_node)
            is_ownership_event = .true.
            type is (nullify_node)
            is_ownership_event = .true.
            type is (subroutine_call_node)
            is_ownership_event = allocated(node%name) .and. &
                same_name(node%name, 'move_alloc')
        class default
        end select
    end function is_ownership_event

    function ownership_event(arena, index) result(event)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(ownership_event_query_t) :: event

        allocate (event%object_indices(0))
        event%found = .true.
        event%node_index = index
        select type (node => arena%entries(index)%node)
            type is (allocate_statement_node)
            event%event_kind = OWNERSHIP_EVENT_ALLOCATE
            call copy_integer_array(node%var_indices, event%object_indices)
            event%source_expr_index = node%source_expr_index
            event%mold_expr_index = node%mold_expr_index
            type is (deallocate_statement_node)
            event%event_kind = OWNERSHIP_EVENT_DEALLOCATE
            call copy_integer_array(node%var_indices, event%object_indices)
            type is (pointer_assignment_node)
            event%event_kind = OWNERSHIP_EVENT_POINTER_ASSIGN
            event%source_index = node%target_index
            event%target_index = node%pointer_index
            type is (nullify_node)
            event%event_kind = OWNERSHIP_EVENT_NULLIFY
            call copy_integer_array(node%pointer_indices, event%object_indices)
            type is (subroutine_call_node)
            event%event_kind = OWNERSHIP_EVENT_MOVE_ALLOC
            if (allocated(node%arg_indices)) then
                event%object_indices = node%arg_indices
                if (size(node%arg_indices) >= 2) then
                    event%source_index = node%arg_indices(1)
                    event%target_index = node%arg_indices(2)
                end if
            end if
        class default
        end select
    end function ownership_event

    logical function node_is_in_scope(arena, node_index, scope_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, scope_index
        integer :: current, guard

        current = node_index
        guard = 0
        do while (current > 0 .and. arena%has_node_at(current))
            if (current == scope_index) then
                node_is_in_scope = .true.
                return
            end if
            current = arena%entries(current)%parent_index
            guard = guard + 1
            if (guard > arena%size) exit
        end do
        node_is_in_scope = .false.
    end function node_is_in_scope

    integer function enclosing_module(arena, node_index) result(module_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: current, guard

        module_index = 0
        current = node_index
        guard = 0
        do while (current > 0 .and. arena%has_node_at(current))
            select type (node => arena%entries(current)%node)
                type is (module_node)
                module_index = current
                return
            class default
            end select
            current = arena%entries(current)%parent_index
            guard = guard + 1
            if (guard > arena%size) return
        end do
    end function enclosing_module

    logical function declaration_owned_by_module(arena, node_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: current, guard

        declaration_owned_by_module = .false.
        current = arena%entries(node_index)%parent_index
        guard = 0
        do while (current > 0 .and. arena%has_node_at(current))
            select type (node => arena%entries(current)%node)
                type is (module_node)
                declaration_owned_by_module = .true.
                return
                type is (subroutine_def_node)
                return
                type is (function_def_node)
                return
                type is (derived_type_node)
                return
                type is (program_node)
                return
            class default
            end select
            current = arena%entries(current)%parent_index
            guard = guard + 1
            if (guard > arena%size) return
        end do
    end function declaration_owned_by_module

    logical function common_member_name(arena, node_index, name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: name
        type(common_block_query_t) :: query
        integer :: i

        common_member_name = .false.
        query = query_common_block(arena, node_index)
        if (.not. query%found) return
        do i = 1, size(query%member_names)
            if (same_name(query%member_names(i), name)) then
                common_member_name = .true.
                return
            end if
        end do
    end function common_member_name

    logical function global_declaration(arena, declaration_index, name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: declaration_index
        character(len=*), intent(in) :: name
        type(storage_query_t) :: storage

        storage = query_storage_without_common_scan(arena, declaration_index)
        global_declaration = storage%found .and. (storage%is_module_state .or. &
            storage%is_save_state .or. storage%is_common_state)
        if (.not. global_declaration) then
            global_declaration = common_name_in_arena(arena, name)
        end if
    end function global_declaration

    function query_storage_without_common_scan(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(storage_query_t) :: query
        type(declaration_query_t) :: declaration
        call set_empty(query%name)
        call set_empty(query%type_name)
        declaration = query_declaration(arena, node_index)
        if (.not. declaration%found) return
        query%found = .true.
        query%node_index = node_index
        query%name = declaration%name
        query%type_name = declaration%type_name
        query%is_allocatable = declaration%is_allocatable
        query%is_pointer = declaration%is_pointer
        query%is_target = declaration%is_target
        query%is_contiguous = declaration%is_contiguous
        query%is_polymorphic = is_polymorphic_type_spec(query%type_name)
        query%is_unlimited_polymorphic = &
            is_unlimited_polymorphic_type_spec(query%type_name)
        query%is_save_state = declaration%is_save
        query%is_module_state = declaration_owned_by_module(arena, node_index)
        if (query%is_save_state) then
            query%storage_class = STORAGE_SAVE
        else if (query%is_module_state) then
            query%storage_class = STORAGE_MODULE
        else if (query%is_pointer) then
            query%storage_class = STORAGE_POINTER
        else if (query%is_allocatable) then
            query%storage_class = STORAGE_OWNED
        else
            query%storage_class = STORAGE_LOCAL
        end if
    end function query_storage_without_common_scan

    logical function is_polymorphic_type_spec(type_name)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: normalized

        normalized = remove_type_spec_spaces(lower_text(trim(type_name)))
        is_polymorphic_type_spec = .false.
        if (len(normalized) < 7) return
        if (normalized(1:6) /= 'class(') return
        is_polymorphic_type_spec = normalized(len(normalized):) == ')'
    end function is_polymorphic_type_spec

    logical function is_unlimited_polymorphic_type_spec(type_name)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: normalized

        normalized = remove_type_spec_spaces(lower_text(trim(type_name)))
        is_unlimited_polymorphic_type_spec = .false.
        if (len(normalized) /= 8) return
        is_unlimited_polymorphic_type_spec = normalized == 'class(*)'
    end function is_unlimited_polymorphic_type_spec

    function remove_type_spec_spaces(value) result(result)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: result
        integer :: i, count

        count = 0
        do i = 1, len(value)
            if (value(i:i) /= ' ' .and. value(i:i) /= achar(9)) count = count + 1
        end do
        allocate (character(len=count) :: result)
        count = 0
        do i = 1, len(value)
            if (value(i:i) == ' ' .or. value(i:i) == achar(9)) cycle
            count = count + 1
            result(count:count) = value(i:i)
        end do
    end function remove_type_spec_spaces

    logical function common_name_in_arena(arena, name)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name
        integer :: i

        common_name_in_arena = .false.
        do i = 1, arena%size
            if (common_member_name(arena, i, name)) then
                common_name_in_arena = .true.
                return
            end if
        end do
    end function common_name_in_arena

    function make_global_reference(arena, reference_index, binding) result(ref)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: reference_index
        type(declaration_binding_t), intent(in) :: binding
        type(global_reference_query_t) :: ref
        type(storage_query_t) :: storage

        call set_empty(ref%name)
        call set_empty(ref%module_name)
        ref%found = .true.
        ref%reference_node_index = reference_index
        ref%declaration_node_index = binding%declaration_node_index
        ref%owner_scope_index = enclosing_module(arena, &
            binding%declaration_node_index)
        ref%name = binding%name
        storage = query_storage(arena, binding%declaration_node_index)
        ref%is_module_state = storage%is_module_state
        ref%is_save_state = storage%is_save_state
        ref%is_common_state = storage%is_common_state .or. &
            common_name_in_arena(arena, binding%name)
        if (ref%owner_scope_index > 0) then
            select type (module => arena%entries(ref%owner_scope_index)%node)
                type is (module_node)
                if (allocated(module%name)) ref%module_name = module%name
            class default
            end select
        end if
        ref%access_kind = reference_access_kind(arena, reference_index)
    end function make_global_reference

    logical function is_identifier_at(arena, index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index

        is_identifier_at = .false.
        select type (node => arena%entries(index)%node)
            type is (identifier_node)
            is_identifier_at = allocated(node%name)
        class default
        end select
    end function is_identifier_at

    integer function reference_access_kind(arena, reference_index) result(kind)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: reference_index
        integer :: parent

        kind = ACCESS_READ
        parent = arena%entries(reference_index)%parent_index
        if (.not. arena%has_node_at(parent)) return
        select type (node => arena%entries(parent)%node)
            type is (assignment_node)
            if (node%target_index == reference_index) then
                kind = ACCESS_WRITE
            else if (node%value_index == reference_index) then
                kind = ACCESS_READ
            end if
            type is (pointer_assignment_node)
            if (node%pointer_index == reference_index) kind = ACCESS_WRITE
        class default
        end select
    end function reference_access_kind

    logical function same_name(left, right)
        character(len=*), intent(in) :: left, right
        same_name = lower_text(trim(left)) == lower_text(trim(right))
    end function same_name

    logical function contains_word(text, word)
        character(len=*), intent(in) :: text, word
        contains_word = index(lower_text(text), lower_text(word)) > 0
    end function contains_word

    function lower_text(value) result(result)
        character(len=*), intent(in) :: value
        character(len=len(value)) :: result
        integer :: i, code

        result = value
        do i = 1, len(value)
            code = iachar(result(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) then
                result(i:i) = achar(code + iachar('a') - iachar('A'))
            end if
        end do
    end function lower_text

end module frontend_compiler_queries
