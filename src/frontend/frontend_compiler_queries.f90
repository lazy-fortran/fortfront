module frontend_compiler_queries
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: string_t
    use ast_nodes_procedure, only: subroutine_call_node, function_def_node, &
        subroutine_def_node
    use ast_nodes_core, only: binary_op_node, literal_node, identifier_node, &
        array_literal_node, program_node, component_access_node, &
        call_or_subscript_node, pointer_assignment_node, assignment_node
    use ast_nodes_associate, only: associate_node
    use ast_nodes_bounds, only: array_slice_node, array_bounds_node, &
        range_expression_node
    use ast_nodes_transfer, only: nullify_node, return_node, &
        alt_return_spec_node
    use ast_nodes_data, only: declaration_node, derived_type_node, &
        parameter_declaration_node, module_node, block_data_node, &
        submodule_node, multi_unit_container_node, type_binding_node, &
        PARAM_UNKNOWN, PARAM_KIND, PARAM_LEN
    use ast_nodes_misc, only: interface_block_node, module_procedure_node, &
        import_statement_node, &
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
        get_scope_statement_indices, &
        find_host_scope, resolve_name_in_scope, BINDING_FUNCTION, &
        BINDING_SUBROUTINE, BINDING_GENERIC_INTERFACE, BINDING_DECLARATION, &
        BINDING_ASSOCIATE_NAME
    use frontend_compiler_type_queries, only: resolved_type_query_t, &
        query_resolved_type
    use type_system_unified, only: TDERIVED
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
    public :: procedure_target_query_t, query_procedure_target
    public :: procedure_call_target_query_t, query_procedure_call_target
    public :: call_argument_query_t, call_arguments_query_t
    public :: query_call_arguments
    public :: generic_argument_query_t, generic_candidate_query_t, &
        generic_call_query_t, query_generic_call
    public :: STORAGE_LOCAL, STORAGE_OWNED, STORAGE_BORROWED, STORAGE_POINTER
    public :: STORAGE_MODULE, STORAGE_SAVE, STORAGE_COMMON
    public :: OWNERSHIP_EVENT_ALLOCATE, OWNERSHIP_EVENT_DEALLOCATE
    public :: OWNERSHIP_EVENT_POINTER_ASSIGN, OWNERSHIP_EVENT_MOVE_ALLOC
    public :: OWNERSHIP_EVENT_NULLIFY, OWNERSHIP_EVENT_ASSIGNMENT
    public :: OWNERSHIP_ASSIGNMENT_NONE, OWNERSHIP_ASSIGNMENT_WHOLE_ALLOCATABLE
    public :: OWNERSHIP_REALLOCATION_NONE, OWNERSHIP_REALLOCATION_POTENTIAL
    public :: ACCESS_READ, ACCESS_WRITE, ACCESS_READ_WRITE
    public :: POLYMORPHIC_SOURCE_UNKNOWN, POLYMORPHIC_SOURCE_CONCRETE
    public :: POLYMORPHIC_SOURCE_POLYMORPHIC
    public :: storage_query_t, ownership_event_query_t, component_path_query_t
    public :: polymorphic_allocation_query_t
    public :: associate_selector_query_t
    public :: binding_resolution_query_t, global_reference_query_t
    public :: query_storage, query_ownership_events, query_component_path
    public :: query_polymorphic_allocation
    public :: query_associate_selector, query_associate_selectors
    public :: query_type_binding_resolution, query_active_global_references
    public :: binding_hierarchy_entry_t, binding_hierarchy_query_t
    public :: query_type_binding_hierarchy
    public :: type_bound_call_query_t, query_type_bound_call

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
    integer, parameter :: OWNERSHIP_EVENT_ASSIGNMENT = 6

    integer, parameter :: OWNERSHIP_ASSIGNMENT_NONE = 0
    integer, parameter :: OWNERSHIP_ASSIGNMENT_WHOLE_ALLOCATABLE = 1
    integer, parameter :: OWNERSHIP_REALLOCATION_NONE = 0
    integer, parameter :: OWNERSHIP_REALLOCATION_POTENTIAL = 1

    integer, parameter :: ACCESS_READ = 1
    integer, parameter :: ACCESS_WRITE = 2
    integer, parameter :: ACCESS_READ_WRITE = 3

    integer, parameter :: POLYMORPHIC_SOURCE_UNKNOWN = 0
    integer, parameter :: POLYMORPHIC_SOURCE_CONCRETE = 1
    integer, parameter :: POLYMORPHIC_SOURCE_POLYMORPHIC = 2

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

    type :: procedure_target_query_t
        !! Facts for one procedure-pointer assignment.
        !!
        !! FOUND means that the assignment has a directly resolved procedure
        !! pointer on its left-hand side.  The target may still be unresolved
        !! or NULL; no flow-sensitive target state is inferred here.
        logical :: found = .false.
        integer :: assignment_node_index = 0
        integer :: pointer_node_index = 0
        integer :: pointer_declaration_index = 0
        integer :: target_node_index = 0
        integer :: target_declaration_index = 0
        integer :: target_procedure_index = 0
        integer :: binding_node_index = 0
        integer :: binding_kind = 0
        integer :: scope_node_index = 0
        character(len=:), allocatable :: pointer_name
        character(len=:), allocatable :: procedure_name
        character(len=:), allocatable :: binding_name
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_null = .false.
    end type procedure_target_query_t

    type :: procedure_call_target_query_t
        !! One bounded, resolved call through a procedure pointer.
        !!
        !! FOUND means that CALL_NODE_INDEX names a procedure pointer and its
        !! lexical scope contains exactly one unconditional direct pointer
        !! assignment before the call.  That assignment must resolve to an
        !! internal or external procedure.  A pointer call with no such
        !! proof leaves FOUND false and sets IS_UNRESOLVED; this includes
        !! branches, reassignment, NULL(), generic calls, and other
        !! flow-sensitive cases.
        logical :: found = .false.
        integer :: call_node_index = 0
        ! Call nodes carry the callee name directly; for this bounded fact
        ! POINTER_NODE_INDEX identifies that same pointer-call occurrence.
        integer :: pointer_node_index = 0
        integer :: pointer_declaration_index = 0
        integer :: assignment_node_index = 0
        integer :: target_node_index = 0
        integer :: target_declaration_index = 0
        integer :: target_procedure_index = 0
        integer :: target_binding_node_index = 0
        integer :: target_binding_kind = 0
        integer :: scope_node_index = 0
        character(len=:), allocatable :: pointer_name
        character(len=:), allocatable :: procedure_name
        character(len=:), allocatable :: target_binding_name
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
    end type procedure_call_target_query_t
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

    ! Exact generic-candidate facts for compiler consumers.  The query does
    ! not apply implicit conversions or dynamic dispatch: a candidate is an
    ! exact match only when every supplied actual has the same semantic
    ! category, kind, and rank as its formal (and derived types have the same
    ! identity).  This keeps a backend from guessing when a generic is
    ! ambiguous or requires a rule it does not implement.
    type :: generic_argument_query_t
        logical :: found = .false.
        integer :: formal_node_index = 0
        character(len=:), allocatable :: name
        logical :: is_optional = .false.
        integer :: type_kind = 0
        integer :: kind_value = 0
        integer :: rank = -1
        character(len=:), allocatable :: derived_type_name
    end type generic_argument_query_t

    type :: generic_candidate_query_t
        logical :: found = .false.
        integer :: procedure_node_index = 0
        character(len=:), allocatable :: procedure_name
        character(len=:), allocatable :: procedure_kind
        logical :: is_match = .false.
        logical :: has_unknown_types = .false.
        type(generic_argument_query_t), allocatable :: arguments(:)
    end type generic_candidate_query_t

    type :: generic_call_query_t
        ! FOUND means that CALL or function-reference syntax resolved to a
        ! same-arena named generic and its concrete candidates were listed.
        logical :: found = .false.
        logical :: is_generic = .false.
        logical :: is_ambiguous = .false.
        logical :: has_exact_match = .false.
        integer :: call_node_index = 0
        integer :: interface_node_index = 0
        integer :: selected_procedure_node_index = 0
        character(len=:), allocatable :: generic_name
        type(generic_candidate_query_t), allocatable :: candidates(:)
    end type generic_call_query_t

    ! Normalized storage facts for compiler consumers.  The existing
    ! declaration query mirrors source attributes; this record additionally
    ! gives ownership-sensitive consumers one stable classification.
    type :: storage_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: declaration_index = 0
        integer :: rank = -1
        character(len=:), allocatable :: name
        character(len=:), allocatable :: type_name
        integer :: storage_class = STORAGE_LOCAL
        logical :: is_component = .false.
        logical :: is_array_element = .false.
        logical :: is_array_section = .false.
        logical :: is_derived = .false.
        logical :: is_concrete_derived = .false.
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

    type :: component_path_query_t
        logical :: found = .false.
        integer :: node_index = 0
        integer :: base_node_index = 0
        integer :: base_rank = -1
        integer :: rank = -1
        integer :: storage_class = STORAGE_LOCAL
        integer :: base_storage_class = STORAGE_LOCAL
        logical :: is_array_element = .false.
        logical :: is_array_section = .false.
        logical :: is_derived = .false.
        logical :: is_concrete_derived = .false.
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_polymorphic = .false.
        logical :: is_unlimited_polymorphic = .false.
        character(len=:), allocatable :: component_names(:)
        integer, allocatable :: component_node_indices(:)
        integer, allocatable :: component_declaration_indices(:)
    end type component_path_query_t

    ! Bounded facts for one ASSOCIATE selector. FOUND means that the
    ! association record exists, not that the selector has a statically
    ! usable storage identity. Expressions, unresolved names, pointer
    ! targets, polymorphic dynamic types, and ambiguous body accesses retain
    ! explicit boundary flags instead of being guessed.
    type :: associate_selector_query_t
        logical :: found = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_ambiguous = .false.
        logical :: is_selector_designator = .false.
        logical :: is_storage_resolved = .false.
        logical :: is_alias = .false.
        logical :: is_alias_boundary = .false.
        logical :: is_pointer = .false.
        logical :: is_allocatable = .false.
        logical :: is_polymorphic = .false.
        logical :: is_unlimited_polymorphic = .false.
        logical :: is_dynamic_type_known = .false.
        logical :: is_read_only = .false.
        logical :: is_writeable = .false.
        logical :: has_read_reference = .false.
        logical :: has_write_reference = .false.
        logical :: has_ambiguous_access = .false.
        integer :: associate_node_index = 0
        integer :: association_index = 0
        integer :: selector_node_index = 0
        integer :: selector_declaration_index = 0
        integer :: storage_identity_node_index = 0
        integer :: base_node_index = 0
        integer :: selector_access_kind = ACCESS_READ
        integer :: association_access_kind = 0
        integer :: declared_type_kind = 0
        integer :: declared_kind_value = 0
        integer :: declared_rank = -1
        integer :: dynamic_type_index = 0
        character(len=:), allocatable :: associate_name
        character(len=:), allocatable :: selector_declared_type
        character(len=:), allocatable :: selector_dynamic_type
        type(storage_query_t) :: selector_storage
        type(component_path_query_t) :: selector_path
    end type associate_selector_query_t

    ! One deliberately bounded fact for a polymorphic ALLOCATE target.  FOUND
    ! means that the allocation target is a directly resolved polymorphic
    ! allocatable.  IS_BOUNDED is true only for one direct SOURCE= data
    ! designator with a concrete declared derived type and one acquisition in
    ! the enclosing scope.  The remaining flags make the refusal boundary
    ! observable without asking a backend to guess from source text.
    type :: polymorphic_allocation_query_t
        logical :: found = .false.
        logical :: is_bounded = .false.
        integer :: allocation_node_index = 0
        integer :: owner_node_index = 0
        integer :: owner_declaration_index = 0
        integer :: source_declaration_index = 0
        integer :: source_expr_index = 0
        integer :: source_classification = POLYMORPHIC_SOURCE_UNKNOWN
        character(len=:), allocatable :: owner_declared_type
        character(len=:), allocatable :: source_resolved_type
        type(component_path_query_t) :: owner_path
        type(component_path_query_t) :: source_path
        logical :: is_source_concrete = .false.
        logical :: is_source_polymorphic = .false.
        logical :: is_source_unknown = .true.
        logical :: is_factory_source = .false.
        logical :: is_repeated_acquisition = .false.
        logical :: is_alias = .false.
    end type polymorphic_allocation_query_t

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
        ! ALLOCATE shape expressions, when explicit bounds are present.
        integer, allocatable :: shape_expr_indices(:)
        integer :: rank = -1
        ! Paths retain the existing component-path representation, including
        ! the base node for a plain identifier or array reference.
        type(component_path_query_t) :: owner_path
        type(component_path_query_t) :: source_path
        type(component_path_query_t) :: destination_path
        ! Assignment-specific names make the ownership direction explicit.
        type(component_path_query_t) :: lhs_owner_path
        type(component_path_query_t) :: rhs_owner_path
        type(polymorphic_allocation_query_t) :: polymorphic_allocation
        integer :: lhs_rank = -1
        integer :: rhs_rank = -1
        integer :: assignment_kind = OWNERSHIP_ASSIGNMENT_NONE
        integer :: reallocation_kind = OWNERSHIP_REALLOCATION_NONE
        logical :: is_potential_automatic_reallocation = .false.
        logical :: is_explicit_ownership_transfer = .false.
    end type ownership_event_query_t

    type :: binding_resolution_query_t
        logical :: found = .false.
        character(len=:), allocatable :: requested_name
        character(len=:), allocatable :: binding_name
        character(len=:), allocatable :: implementation
        integer :: implementation_node_index = 0
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
        integer, allocatable :: dispatch_target_implementation_node_indices(:)
        character(len=:), allocatable :: dispatch_target_pass_names(:)
        integer, allocatable :: dispatch_target_pass_positions(:)
        character(len=:), allocatable :: dispatch_target_passed_object_types(:)
        ! These arrays are parallel to the existing target arrays. A false
        ! flag means the implementation was found but its passed-object
        ! declaration was not available to this arena query.
        logical, allocatable :: dispatch_target_signature_resolved(:)
    end type binding_resolution_query_t

    type :: binding_hierarchy_entry_t
        !! Effective binding metadata for one type in an EXTENDS chain.
        logical :: found = .false.
        integer :: type_index = 0
        integer :: parent_type_index = 0
        integer :: binding_node_index = 0
        integer :: declaring_type_index = 0
        character(len=:), allocatable :: type_name
        character(len=:), allocatable :: parent_type_name
        character(len=:), allocatable :: declaring_type_name
        character(len=:), allocatable :: binding_name
        character(len=:), allocatable :: implementation
        integer :: implementation_node_index = 0
        character(len=:), allocatable :: implementation_pass_name
        integer :: implementation_pass_position = 0
        character(len=:), allocatable :: implementation_passed_object_type
        logical :: implementation_signature_resolved = .false.
        character(len=:), allocatable :: interface_name
        character(len=:), allocatable :: pass_name
        logical :: is_local = .false.
        logical :: is_inherited = .false.
        logical :: is_generic = .false.
        logical :: is_deferred = .false.
        logical :: is_abstract_type = .false.
        logical :: is_ambiguous = .false.
        logical :: is_resolved = .false.
        logical :: pass_arg = .true.
    end type binding_hierarchy_entry_t

    type :: binding_hierarchy_query_t
        !! Static binding information for one declared derived type.
        !!
        !! The hierarchy is ordered from the queried type toward its root
        !! parent.  It contains no descendant or runtime dispatch targets.
        logical :: found = .false.
        character(len=:), allocatable :: requested_name
        integer :: declared_type_index = 0
        character(len=:), allocatable :: declared_type_name
        integer :: declaring_type_index = 0
        character(len=:), allocatable :: declaring_type_name
        integer :: binding_node_index = 0
        character(len=:), allocatable :: binding_name
        character(len=:), allocatable :: implementation
        integer :: implementation_node_index = 0
        character(len=:), allocatable :: implementation_pass_name
        integer :: implementation_pass_position = 0
        character(len=:), allocatable :: implementation_passed_object_type
        logical :: implementation_signature_resolved = .false.
        character(len=:), allocatable :: interface_name
        character(len=:), allocatable :: pass_name
        logical :: is_inherited = .false.
        logical :: is_generic = .false.
        logical :: is_deferred = .false.
        logical :: is_abstract_type = .false.
        logical :: is_ambiguous = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: pass_arg = .true.
        integer, allocatable :: parent_type_indices(:)
        character(len=:), allocatable :: parent_type_names(:)
        type(binding_hierarchy_entry_t), allocatable :: hierarchy(:)
    end type binding_hierarchy_query_t

    type :: type_bound_call_query_t
        !! Static facts for one type-bound call site.
        !!
        !! FOUND means that the call receiver has a declared derived type and
        !! its binding was found.  IS_RESOLVED is narrower: generic,
        !! ambiguous, deferred, and implementation-free bindings are never
        !! given an implementation guess.  Dispatch targets are the concrete
        !! descendant facts supplied by query_type_binding_resolution; no
        !! runtime object flow or AD policy is inferred here.
        logical :: found = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_ambiguous = .false.
        logical :: is_generic = .false.
        logical :: is_deferred = .false.
        logical :: is_inherited = .false.
        logical :: is_abstract_type = .false.
        logical :: pass_arg = .true.
        integer :: call_node_index = 0
        integer :: receiver_node_index = 0
        integer :: receiver_declaration_index = 0
        integer :: declared_type_index = 0
        integer :: declaring_type_index = 0
        integer :: resolved_type_index = 0
        integer :: binding_node_index = 0
        character(len=:), allocatable :: receiver_name
        character(len=:), allocatable :: declared_type_name
        character(len=:), allocatable :: binding_name
        character(len=:), allocatable :: implementation
        character(len=:), allocatable :: interface_name
        character(len=:), allocatable :: pass_name
        ! For expression-form calls, preserve the receiver's component path
        ! instead of making consumers recover it from a flattened name.
        type(component_path_query_t) :: receiver_path
        integer, allocatable :: dispatch_target_type_indices(:)
        character(len=:), allocatable :: dispatch_target_implementations(:)
        integer, allocatable :: dispatch_target_implementation_node_indices(:)
        character(len=:), allocatable :: dispatch_target_pass_names(:)
        integer, allocatable :: dispatch_target_pass_positions(:)
        character(len=:), allocatable :: dispatch_target_passed_object_types(:)
        ! Parallel to the existing target type and implementation arrays.
        logical, allocatable :: dispatch_target_signature_resolved(:)
    end type type_bound_call_query_t

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

    function query_procedure_target(arena, node_index) result(query)
        !! Resolve one direct procedure-pointer assignment without inferring
        !! flow-sensitive callback state.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(procedure_target_query_t) :: query
        type(pointer_assignment_query_t) :: assignment
        type(declaration_binding_t) :: pointer_binding, target_binding
        type(declaration_query_t) :: pointer_declaration, target_declaration
        character(len=:), allocatable :: error_msg

        call initialize_procedure_target_query(query)
        assignment = query_pointer_assignment(arena, node_index)
        if (.not. assignment%found) return

        query%assignment_node_index = node_index
        query%pointer_node_index = assignment%pointer_node_index
        query%target_node_index = assignment%target_node_index
        query%scope_node_index = find_enclosing_scope(arena, node_index)

        call resolve_identifier_binding(arena, query%pointer_node_index, &
            pointer_binding, error_msg)
        if (.not. pointer_binding%found) return
        query%pointer_declaration_index = pointer_binding%declaration_node_index
        query%pointer_name = pointer_binding%name
        pointer_declaration = query_declaration(arena, &
            query%pointer_declaration_index)
        if (.not. is_procedure_pointer_declaration(pointer_declaration)) return
        query%found = .true.

        call procedure_target_name_at(arena, query%target_node_index, &
            query%procedure_name)
        if (is_null_procedure_target(arena, query%target_node_index)) then
            query%is_null = .true.
            return
        end if

        if (.not. is_identifier_at(arena, query%target_node_index)) then
            query%is_unresolved = .true.
            return
        end if

        call resolve_identifier_binding(arena, query%target_node_index, &
            target_binding, error_msg)
        if (.not. target_binding%found) then
            query%is_unresolved = .true.
            return
        end if

        query%binding_node_index = target_binding%node_index
        query%binding_kind = target_binding%binding_kind
        query%target_declaration_index = target_binding%declaration_node_index
        query%binding_name = target_binding%name

        if (target_binding%binding_kind == BINDING_FUNCTION .or. &
            target_binding%binding_kind == BINDING_SUBROUTINE) then
            query%target_procedure_index = target_binding%node_index
            query%is_resolved = .true.
        else if (target_binding%binding_kind == BINDING_DECLARATION) then
            target_declaration = query_declaration(arena, &
                target_binding%declaration_node_index)
            query%is_resolved = target_declaration%found .and. &
                target_declaration%is_external
        end if
        query%is_unresolved = .not. query%is_resolved
    end function query_procedure_target

    function query_procedure_call_target(arena, node_index) result(query)
        !! Resolve one direct call through a procedure pointer.
        !!
        !! The proof is deliberately bounded: the pointer must have exactly
        !! one direct assignment in the call's lexical scope, that assignment
        !! must precede the call, and no other same-scope pointer assignment or
        !! NULLIFY may touch the pointer.  No branch or general dataflow
        !! analysis is attempted.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(procedure_call_target_query_t) :: query
        type(declaration_binding_t) :: pointer_binding
        type(declaration_query_t) :: pointer_declaration
        type(procedure_target_query_t) :: target
        character(len=:), allocatable :: call_name
        character(len=:), allocatable :: error_msg
        integer, allocatable :: scope_indices(:)
        integer :: call_scope_statement, assignment_count
        integer :: assignment_index
        logical :: is_call, has_non_direct_mutation

        call initialize_procedure_call_target_query(query)
        if (.not. arena%has_node_at(node_index)) return

        call get_call_parts(arena, node_index, call_name, scope_indices, is_call)
        if (.not. is_call) return
        if (len_trim(call_name) == 0) return

        query%call_node_index = node_index
        query%scope_node_index = find_enclosing_scope(arena, node_index)
        if (query%scope_node_index <= 0) return

        call resolve_identifier_binding(arena, node_index, pointer_binding, &
            error_msg)
        if (.not. pointer_binding%found) return
        pointer_declaration = query_declaration(arena, &
            pointer_binding%declaration_node_index)
        if (pointer_binding%binding_kind /= BINDING_DECLARATION .or. &
            .not. is_procedure_pointer_declaration(pointer_declaration)) return

        query%pointer_node_index = node_index
        query%pointer_declaration_index = &
            pointer_binding%declaration_node_index
        query%pointer_name = pointer_binding%name
        query%is_unresolved = .true.

        call get_scope_statement_indices(arena, query%scope_node_index, &
            scope_indices)
        call direct_scope_statement_for_node(arena, node_index, &
            query%scope_node_index, call_scope_statement)
        if (call_scope_statement <= 0) return

        assignment_count = 0
        assignment_index = 0
        has_non_direct_mutation = .false.
        call find_pointer_mutations(arena, query%scope_node_index, &
            query%pointer_declaration_index, query%pointer_name, &
            scope_indices, assignment_count, assignment_index, &
            has_non_direct_mutation)
        if (assignment_count /= 1 .or. has_non_direct_mutation) return
        if (.not. index_precedes(scope_indices, assignment_index, &
            call_scope_statement)) return

        target = query_procedure_target(arena, assignment_index)
        if (.not. target%found .or. .not. target%is_resolved) return

        query%assignment_node_index = target%assignment_node_index
        query%target_node_index = target%target_node_index
        query%target_declaration_index = target%target_declaration_index
        query%target_procedure_index = target%target_procedure_index
        query%target_binding_node_index = target%binding_node_index
        query%target_binding_kind = target%binding_kind
        query%procedure_name = target%procedure_name
        query%target_binding_name = target%binding_name
        query%found = .true.
        query%is_resolved = .true.
        query%is_unresolved = .false.
    end function query_procedure_call_target

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

    function query_generic_call(arena, call_node_index) result(query)
        !! Enumerate and exactly match a same-arena named generic call.
        !!
        !! The result is deliberately conservative.  It records every
        !! concrete interface candidate, including its semantic formal
        !! category, kind, rank, and derived-type identity.  `is_match` is
        !! true only for a complete exact signature match; conversions,
        !! extension-type compatibility, elemental expansion, and procedure
        !! pointer dispatch remain outside this contract.  A unique exact
        !! match is exposed through selected_procedure_node_index.  Zero or
        !! multiple exact matches are reported without selecting a procedure.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        type(generic_call_query_t) :: query

        type(declaration_binding_t) :: binding
        integer, allocatable :: actual_indices(:)
        integer, allocatable :: candidate_indices(:)
        character(len=:), allocatable :: call_name
        character(len=:), allocatable :: error_msg
        logical :: is_call
        integer :: i, match_count

        call initialize_generic_call_query(query)
        call get_call_parts(arena, call_node_index, call_name, actual_indices, &
            is_call)
        if (.not. is_call) return
        if (len_trim(call_name) == 0) return

        call resolve_name_at_node(arena, call_node_index, call_name, binding, &
            error_msg)
        if (.not. binding%found) return
        if (binding%binding_kind /= BINDING_GENERIC_INTERFACE) return
        if (.not. arena%has_node_at(binding%node_index)) return

        query%is_generic = .true.
        query%call_node_index = call_node_index
        query%interface_node_index = binding%node_index
        query%generic_name = trim(call_name)
        call collect_generic_candidate_indices(arena, binding%node_index, &
            candidate_indices)
        if (size(candidate_indices) == 0) return

        if (allocated(query%candidates)) deallocate (query%candidates)
        allocate (query%candidates(size(candidate_indices)))
        match_count = 0
        do i = 1, size(candidate_indices)
            call fill_generic_candidate(arena, actual_indices, &
                candidate_indices(i), query%candidates(i))
            if (query%candidates(i)%is_match) match_count = match_count + 1
        end do

        query%found = .true.
        query%has_exact_match = match_count > 0
        query%is_ambiguous = match_count > 1
        if (match_count == 1) then
            do i = 1, size(query%candidates)
                if (query%candidates(i)%is_match) then
                    query%selected_procedure_node_index = &
                        query%candidates(i)%procedure_node_index
                    exit
                end if
            end do
        end if
    end function query_generic_call

    subroutine initialize_generic_call_query(query)
        type(generic_call_query_t), intent(out) :: query

        call set_empty(query%generic_name)
        allocate (query%candidates(0))
    end subroutine initialize_generic_call_query

    subroutine collect_generic_candidate_indices(arena, interface_index, indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: interface_index
        integer, allocatable, intent(out) :: indices(:)
        integer :: i, j, procedure_index, resolved_index

        allocate (indices(0))
        if (.not. arena%has_node_at(interface_index)) return
        select type (interface => arena%entries(interface_index)%node)
            type is (interface_block_node)
            if (.not. allocated(interface%procedure_indices)) return
            do i = 1, size(interface%procedure_indices)
                procedure_index = interface%procedure_indices(i)
                if (.not. arena%has_node_at(procedure_index)) cycle
                select type (procedure => arena%entries(procedure_index)%node)
                    type is (function_def_node)
                    call append_candidate_index(indices, procedure_index)
                    type is (subroutine_def_node)
                    call append_candidate_index(indices, procedure_index)
                    type is (module_procedure_node)
                    if (.not. allocated(procedure%procedure_names)) cycle
                    do j = 1, size(procedure%procedure_names)
                        call resolve_generic_procedure_name(arena, &
                            interface_index, procedure%procedure_names(j)%s, &
                            resolved_index)
                        if (resolved_index > 0) then
                            call append_candidate_index(indices, resolved_index)
                        end if
                    end do
                end select
            end do
        end select
    end subroutine collect_generic_candidate_indices

    subroutine resolve_generic_procedure_name(arena, interface_index, name, &
            procedure_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: interface_index
        character(len=*), intent(in) :: name
        integer, intent(out) :: procedure_index
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        integer :: scope_index, host_index

        procedure_index = 0
        if (len_trim(name) == 0) return
        scope_index = find_enclosing_scope(arena, interface_index)
        if (scope_index == interface_index) then
            host_index = find_host_scope(arena, scope_index)
            scope_index = host_index
        end if
        if (scope_index <= 0) return
        call resolve_name_in_scope(arena, scope_index, trim(name), binding, &
            error_msg)
        if (.not. binding%found) return
        if (binding%binding_kind /= BINDING_FUNCTION .and. &
            binding%binding_kind /= BINDING_SUBROUTINE) return
        procedure_index = binding%node_index
    end subroutine resolve_generic_procedure_name

    subroutine append_candidate_index(indices, value)
        integer, allocatable, intent(inout) :: indices(:)
        integer, intent(in) :: value
        integer, allocatable :: expanded(:)
        integer :: i

        if (value <= 0) return
        do i = 1, size(indices)
            if (indices(i) == value) return
        end do
        allocate (expanded(size(indices) + 1))
        if (size(indices) > 0) expanded(:size(indices)) = indices
        expanded(size(expanded)) = value
        call move_alloc(expanded, indices)
    end subroutine append_candidate_index

    subroutine fill_generic_candidate(arena, actual_indices, procedure_index, &
            candidate)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: actual_indices(:)
        integer, intent(in) :: procedure_index
        type(generic_candidate_query_t), intent(out) :: candidate
        type(program_unit_query_t) :: procedure
        type(declaration_query_t) :: declaration
        type(resolved_type_query_t) :: resolved
        integer, allocatable :: actual_for_formal(:)
        integer, allocatable :: value_for_formal(:)
        logical :: valid_mapping, supplied
        integer :: i

        call initialize_generic_candidate(candidate)
        procedure = query_program_unit(arena, procedure_index)
        if (.not. procedure%found) return
        if (procedure%unit_kind /= 'function' .and. &
            procedure%unit_kind /= 'subroutine') return
        candidate%found = .true.
        candidate%procedure_node_index = procedure_index
        candidate%procedure_name = procedure%name
        candidate%procedure_kind = procedure%unit_kind
        if (allocated(candidate%arguments)) deallocate (candidate%arguments)
        allocate (candidate%arguments(size(procedure%parameter_indices)))

        do i = 1, size(procedure%parameter_indices)
            call initialize_generic_argument(candidate%arguments(i))
            candidate%arguments(i)%formal_node_index = &
                procedure%parameter_indices(i)
            declaration = query_declaration(arena, &
                procedure%parameter_indices(i))
            if (.not. declaration%found) cycle
            candidate%arguments(i)%name = declaration%name
            candidate%arguments(i)%is_optional = declaration%is_optional
            resolved = query_resolved_type(arena, &
                procedure%parameter_indices(i))
            if (.not. resolved%found) then
                candidate%has_unknown_types = .true.
                cycle
            end if
            candidate%arguments(i)%found = .true.
            candidate%arguments(i)%type_kind = resolved%type_kind
            candidate%arguments(i)%kind_value = resolved%kind_value
            candidate%arguments(i)%rank = resolved%rank
            candidate%arguments(i)%derived_type_name = &
                resolved%derived_type_name
        end do

        call map_generic_actuals(arena, actual_indices, &
            procedure%parameter_indices, actual_for_formal, value_for_formal, &
            valid_mapping)
        if (.not. valid_mapping) return
        candidate%is_match = .true.
        do i = 1, size(procedure%parameter_indices)
            supplied = actual_for_formal(i) > 0
            if (.not. supplied) then
                if (.not. candidate%arguments(i)%is_optional) then
                    candidate%is_match = .false.
                    return
                end if
                cycle
            end if
            if (.not. candidate%arguments(i)%found) then
                candidate%is_match = .false.
                return
            end if
            resolved = query_resolved_type(arena, value_for_formal(i))
            if (.not. resolved%found) then
                candidate%has_unknown_types = .true.
                candidate%is_match = .false.
                return
            end if
            if (.not. generic_types_match(candidate%arguments(i), resolved)) then
                candidate%is_match = .false.
                return
            end if
        end do
    end subroutine fill_generic_candidate

    subroutine initialize_generic_candidate(candidate)
        type(generic_candidate_query_t), intent(out) :: candidate

        call set_empty(candidate%procedure_name)
        call set_empty(candidate%procedure_kind)
        allocate (candidate%arguments(0))
    end subroutine initialize_generic_candidate

    subroutine initialize_generic_argument(argument)
        type(generic_argument_query_t), intent(out) :: argument

        call set_empty(argument%name)
        call set_empty(argument%derived_type_name)
    end subroutine initialize_generic_argument

    subroutine map_generic_actuals(arena, actual_indices, formal_indices, &
            actual_for_formal, value_for_formal, valid)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: actual_indices(:), formal_indices(:)
        integer, allocatable, intent(out) :: actual_for_formal(:)
        integer, allocatable, intent(out) :: value_for_formal(:)
        logical, intent(out) :: valid
        character(len=:), allocatable :: keyword
        logical :: is_keyword
        integer :: i, formal, next_formal, value_index
        type(declaration_query_t) :: formal_query

        allocate (actual_for_formal(size(formal_indices)))
        allocate (value_for_formal(size(formal_indices)))
        actual_for_formal = 0
        value_for_formal = 0
        valid = size(actual_indices) <= size(formal_indices)
        if (.not. valid) return
        next_formal = 1
        do i = 1, size(actual_indices)
            call get_call_actual_info(arena, actual_indices(i), keyword, &
                value_index, is_keyword)
            if (is_keyword) then
                formal = find_formal_name(arena, formal_indices, keyword)
            else
                formal = next_formal
                do while (next_formal <= size(formal_indices))
                    if (actual_for_formal(next_formal) == 0) exit
                    next_formal = next_formal + 1
                end do
            end if
            if (formal <= 0 .or. formal > size(formal_indices)) then
                valid = .false.
                return
            end if
            if (actual_for_formal(formal) /= 0) then
                valid = .false.
                return
            end if
            if (.not. is_keyword) then
                formal_query = query_declaration(arena, formal_indices(formal))
                if (.not. formal_query%found) then
                    valid = .false.
                    return
                end if
            end if
            actual_for_formal(formal) = actual_indices(i)
            value_for_formal(formal) = value_index
            if (.not. is_keyword) next_formal = formal + 1
        end do
    end subroutine map_generic_actuals

    logical function generic_types_match(formal, actual) result(matches)
        type(generic_argument_query_t), intent(in) :: formal
        type(resolved_type_query_t), intent(in) :: actual

        matches = .false.
        if (formal%type_kind /= actual%type_kind) return
        if (formal%kind_value /= actual%kind_value) return
        if (formal%rank /= actual%rank) return
        if (len_trim(formal%derived_type_name) > 0 .or. &
            len_trim(actual%derived_type_name) > 0) then
            if (.not. same_name(formal%derived_type_name, &
                actual%derived_type_name)) return
        end if
        matches = .true.
    end function generic_types_match

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

    subroutine initialize_procedure_target_query(query)
        type(procedure_target_query_t), intent(out) :: query

        call set_empty(query%pointer_name)
        call set_empty(query%procedure_name)
        call set_empty(query%binding_name)
    end subroutine initialize_procedure_target_query

    subroutine initialize_procedure_call_target_query(query)
        type(procedure_call_target_query_t), intent(out) :: query

        call set_empty(query%pointer_name)
        call set_empty(query%procedure_name)
        call set_empty(query%target_binding_name)
    end subroutine initialize_procedure_call_target_query

    subroutine direct_scope_statement_for_node(arena, node_index, scope_index, &
            statement_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, scope_index
        integer, intent(out) :: statement_index
        integer :: current, parent

        statement_index = 0
        current = node_index
        do while (current > 0)
            if (.not. arena%has_node_at(current)) return
            if (current == scope_index) return
            parent = arena%entries(current)%parent_index
            if (parent == scope_index) then
                statement_index = current
                return
            end if
            current = parent
        end do
    end subroutine direct_scope_statement_for_node

    subroutine find_pointer_mutations(arena, scope_index, declaration_index, &
            pointer_name, scope_indices, mutation_count, assignment_index, &
            has_non_direct_mutation)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index, declaration_index
        character(len=*), intent(in) :: pointer_name
        integer, intent(in) :: scope_indices(:)
        integer, intent(out) :: mutation_count, assignment_index
        logical, intent(out) :: has_non_direct_mutation
        type(pointer_assignment_query_t) :: assignment
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: mutation_name
        integer :: i, j
        logical :: matches

        mutation_count = 0
        assignment_index = 0
        has_non_direct_mutation = .false.
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (.not. index_in_list(scope_indices, i)) then
                if (find_enclosing_scope(arena, i) /= scope_index) cycle
            end if

            matches = .false.
            select type (node => arena%entries(i)%node)
                type is (pointer_assignment_node)
                assignment = query_pointer_assignment(arena, i)
                if (.not. assignment%found) cycle
                call resolve_identifier_binding(arena, &
                    assignment%pointer_node_index, binding, error_msg)
                if (binding%found .and. binding%declaration_node_index == &
                    declaration_index .and. same_name(binding%name, &
                    pointer_name)) then
                    matches = .true.
                end if
                if (matches) then
                    mutation_count = mutation_count + 1
                    if (assignment_index == 0) assignment_index = i
                    if (.not. index_in_list(scope_indices, i)) then
                        has_non_direct_mutation = .true.
                    end if
                end if
                type is (nullify_node)
                if (.not. allocated(node%pointer_indices)) cycle
                do j = 1, size(node%pointer_indices)
                    call procedure_target_name_at(arena, node%pointer_indices(j), &
                        mutation_name)
                    call resolve_name_in_scope(arena, scope_index, mutation_name, &
                        binding, error_msg)
                    if (.not. binding%found) cycle
                    if (binding%declaration_node_index /= declaration_index) cycle
                    if (.not. same_name(binding%name, pointer_name)) cycle
                    matches = .true.
                    exit
                end do
                if (matches) then
                    mutation_count = mutation_count + 1
                    if (.not. index_in_list(scope_indices, i)) then
                        has_non_direct_mutation = .true.
                    end if
                end if
            class default
            end select
        end do
    end subroutine find_pointer_mutations

    logical function index_in_list(indices, value) result(found)
        integer, intent(in) :: indices(:), value
        integer :: i

        found = .false.
        do i = 1, size(indices)
            if (indices(i) == value) then
                found = .true.
                return
            end if
        end do
    end function index_in_list

    logical function index_precedes(indices, first, second) result(precedes)
        integer, intent(in) :: indices(:), first, second
        integer :: i, first_position, second_position

        first_position = 0
        second_position = 0
        do i = 1, size(indices)
            if (indices(i) == first) first_position = i
            if (indices(i) == second) second_position = i
        end do
        precedes = first_position > 0 .and. second_position > first_position
    end function index_precedes

    logical function is_procedure_pointer_declaration(query) result(is_pointer)
        type(declaration_query_t), intent(in) :: query
        character(len=:), allocatable :: normalized

        is_pointer = .false.
        if (.not. query%found .or. .not. query%is_pointer) return
        normalized = remove_type_spec_spaces(lower_text(query%type_name))
        is_pointer = index(normalized, 'procedure') == 1
    end function is_procedure_pointer_declaration

    subroutine procedure_target_name_at(arena, node_index, name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name

        call set_empty(name)
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (identifier_node)
            if (allocated(node%name)) name = node%name
            type is (call_or_subscript_node)
            if (allocated(node%name)) name = node%name
        class default
        end select
    end subroutine procedure_target_name_at

    logical function is_null_procedure_target(arena, node_index) result(is_null)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: name

        is_null = .false.
        call procedure_target_name_at(arena, node_index, name)
        is_null = lower_text(trim(name)) == 'null'
    end function is_null_procedure_target

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

    recursive function query_storage(arena, node_index, &
            allow_associate_selector) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical, intent(in), optional :: allow_associate_selector
        type(storage_query_t) :: query
        type(declaration_query_t) :: declaration
        type(component_access_query_t) :: component
        integer :: i
        logical :: common_state, allow_selector

        allow_selector = .false.
        if (present(allow_associate_selector)) then
            allow_selector = allow_associate_selector
        end if

        call set_empty(query%name)
        call set_empty(query%type_name)
        if (.not. arena%has_node_at(node_index)) return
        declaration = query_declaration(arena, node_index)
        if (.not. declaration%found) then
            component = query_component_access(arena, node_index)
            if (component%found) call query_component_storage(arena, node_index, &
                component, query, allow_selector)
            return
        end if

        query%found = .true.
        query%node_index = node_index
        query%declaration_index = node_index
        query%name = declaration%name
        query%type_name = declaration%type_name
        query%rank = declaration_rank(declaration)
        query%is_allocatable = declaration%is_allocatable
        query%is_pointer = declaration%is_pointer
        query%is_target = declaration%is_target
        query%is_contiguous = declaration%is_contiguous
        query%is_polymorphic = is_polymorphic_type_spec(query%type_name)
        query%is_unlimited_polymorphic = &
            is_unlimited_polymorphic_type_spec(query%type_name)
        call set_derived_storage_facts(arena, node_index, query)
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

    integer function declaration_rank(declaration) result(rank)
        type(declaration_query_t), intent(in) :: declaration

        rank = 0
        if (.not. declaration%found) then
            rank = -1
            return
        end if
        if (declaration%is_array) then
            if (allocated(declaration%dimension_indices)) then
                rank = size(declaration%dimension_indices)
            else
                rank = -1
            end if
        end if
    end function declaration_rank

    subroutine set_derived_storage_facts(arena, node_index, query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(storage_query_t), intent(inout) :: query
        type(resolved_type_query_t) :: resolved

        resolved = query_resolved_type(arena, node_index)
        if (resolved%found) then
            query%is_derived = resolved%type_kind == TDERIVED
        else
            query%is_derived = is_derived_type_spec(query%type_name)
        end if
        query%is_concrete_derived = query%is_derived .and. &
            .not. query%is_polymorphic
    end subroutine set_derived_storage_facts

    logical function is_derived_type_spec(type_name)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: normalized

        normalized = remove_type_spec_spaces(lower_text(trim(type_name)))
        is_derived_type_spec = .false.
        if (len(normalized) < 6) return
        if (normalized(1:5) == 'type(' .and. &
            normalized(len(normalized):len(normalized)) == ')') then
            is_derived_type_spec = .true.
            return
        end if
        if (len(normalized) >= 6) then
            if (normalized(1:6) == 'class(' .and. &
                normalized(len(normalized):len(normalized)) == ')') then
                is_derived_type_spec = .true.
            end if
        end if
    end function is_derived_type_spec

    subroutine query_component_storage(arena, node_index, component, query, &
            allow_associate_selector)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(component_access_query_t), intent(in) :: component
        type(storage_query_t), intent(out) :: query
        logical, intent(in) :: allow_associate_selector
        type(declaration_binding_t) :: binding
        type(declaration_query_t) :: base_declaration, component_declaration
        type(storage_query_t) :: base_storage
        type(component_access_query_t) :: base_component
        logical :: base_is_array_element, base_is_array_section
        logical :: base_is_array_designator
        character(len=:), allocatable :: base_type, derived_name, error_msg
        character(len=:), allocatable :: base_name
        integer :: derived_index, component_index, i, scope_index
        integer :: fallback_index

        query%node_index = node_index
        if (.not. allow_associate_selector .and. &
                is_associate_selector_node(arena, node_index)) return
        base_declaration = query_declaration(arena, component%base_node_index)
        base_component = query_component_access(arena, component%base_node_index)
        base_is_array_element = is_array_element_node(arena, &
            component%base_node_index)
        base_is_array_section = is_array_section_node(arena, &
            component%base_node_index)
        base_is_array_designator = base_is_array_element .or. &
            base_is_array_section
        if (base_declaration%found) then
            base_type = base_declaration%type_name
            base_storage = query_storage(arena, base_declaration%node_index, &
                allow_associate_selector)
        else if (base_component%found) then
            base_storage = query_storage(arena, component%base_node_index, &
                allow_associate_selector)
            if (.not. base_storage%found) return
            base_type = base_storage%type_name
            base_is_array_element = base_storage%is_array_element
            base_is_array_section = base_storage%is_array_section
        else if (base_is_array_designator) then
            call resolve_array_element_declaration(arena, &
                component%base_node_index, base_declaration)
            if (.not. base_declaration%found) return
            base_type = base_declaration%type_name
            base_storage = query_storage(arena, base_declaration%node_index, &
                allow_associate_selector)
        else
            call resolve_identifier_binding(arena, component%base_node_index, &
                binding, error_msg)
            if (binding%found) then
                if (binding%binding_kind == BINDING_ASSOCIATE_NAME) return
                base_declaration = query_declaration(arena, &
                    binding%declaration_node_index)
            end if
            if (.not. base_declaration%found) then
                ! Semantic callers normally resolve the identifier above. A
                ! parse-only arena may not have lexical bindings yet; retain
                ! the old scope-bounded declaration fallback so nested
                ! component paths remain queryable without source-text guesses.
                call identifier_name_at(arena, component%base_node_index, &
                    base_name)
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
            base_storage = query_storage(arena, base_declaration%node_index, &
                allow_associate_selector)
        end if
        if (.not. base_storage%found) return

        derived_name = derived_type_name_from_spec(base_type)
        derived_index = find_derived_type_by_name(arena, derived_name)
        if (derived_index <= 0) return
        component_index = find_component_declaration_in_hierarchy(arena, &
            derived_index, component%component_name)
        if (component_index <= 0) return
        component_declaration = query_declaration(arena, component_index)
        if (.not. component_declaration%found) return

        query%found = .true.
        query%name = component_declaration%name
        query%type_name = component_declaration%type_name
        query%declaration_index = component_index
        query%rank = component_rank(arena, component_declaration) + &
            max(0, designator_rank(arena, component%base_node_index))
        query%is_component = .true.
        query%is_array_element = base_is_array_element
        query%is_array_section = base_is_array_section
        query%is_allocatable = component_declaration%is_allocatable
        query%is_pointer = component_declaration%is_pointer
        query%is_target = component_declaration%is_target
        query%is_contiguous = component_declaration%is_contiguous
        query%is_polymorphic = is_polymorphic_type_spec(query%type_name)
        query%is_unlimited_polymorphic = &
            is_unlimited_polymorphic_type_spec(query%type_name)
        call set_derived_storage_facts(arena, component_index, query)
        query%is_module_state = base_storage%is_module_state
        query%is_save_state = base_storage%is_save_state
        query%is_common_state = base_storage%is_common_state
        if (query%is_pointer) then
            query%storage_class = STORAGE_POINTER
        else if (query%is_allocatable) then
            query%storage_class = STORAGE_OWNED
        else
            query%storage_class = STORAGE_LOCAL
        end if
        if (query%is_common_state) then
            query%storage_class = STORAGE_COMMON
        else if (query%is_save_state) then
            query%storage_class = STORAGE_SAVE
        else if (query%is_module_state) then
            query%storage_class = STORAGE_MODULE
        end if
    end subroutine query_component_storage

    integer function component_rank(arena, declaration) result(rank)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_query_t), intent(in) :: declaration

        rank = declaration_rank(declaration)
        if (rank < 0) return
        if (declaration%node_index > 0) then
            if (.not. arena%has_node_at(declaration%node_index)) rank = -1
        end if
    end function component_rank

    logical function is_array_element_node(arena, node_index) result(is_element)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_element = .false.
        if (.not. is_array_designator_node(arena, node_index)) return
        if (is_array_section_node(arena, node_index)) return
        is_element = .true.
    end function is_array_element_node

    logical function is_array_designator_node(arena, node_index) result(is_designator)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_designator = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (call_or_subscript_node)
            if (.not. allocated(node%arg_indices)) return
            is_designator = size(node%arg_indices) > 0
            type is (array_slice_node)
            is_designator = node%num_dimensions > 0
        class default
        end select
    end function is_array_designator_node

    logical function is_array_section_node(arena, node_index) result(is_section)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: i

        is_section = .false.
        if (.not. is_array_designator_node(arena, node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (call_or_subscript_node)
            do i = 1, size(node%arg_indices)
                if (subscript_retains_dimension(arena, node%arg_indices(i))) then
                    is_section = .true.
                    return
                end if
            end do
            type is (array_slice_node)
            is_section = .true.
        class default
        end select
    end function is_array_section_node

    subroutine resolve_array_element_declaration(arena, node_index, declaration)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(declaration_query_t), intent(out) :: declaration
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: array_name
        integer :: i, fallback_index, scope_index

        declaration = query_declaration(arena, node_index)
        if (declaration%found) return
        select type (node => arena%entries(node_index)%node)
            type is (array_slice_node)
            if (node%array_index <= 0) return
            declaration = query_declaration(arena, node%array_index)
            if (.not. declaration%found) then
                call resolve_identifier_binding(arena, node%array_index, &
                    binding, error_msg)
                if (binding%found) then
                    if (binding%binding_kind == BINDING_ASSOCIATE_NAME) return
                    declaration = query_declaration(arena, &
                        binding%declaration_node_index)
                end if
            end if
            if (declaration%found .and. .not. declaration%is_array) then
                declaration%found = .false.
            end if
            return
        class default
        end select
        call array_element_name_at(arena, node_index, array_name)
        if (len_trim(array_name) == 0) return
        call resolve_name_at_node(arena, node_index, array_name, binding, &
            error_msg)
        if (.not. binding%found) then
            scope_index = find_enclosing_scope(arena, node_index)
            if (scope_index > 0) then
                call resolve_name_in_scope(arena, scope_index, array_name, &
                    binding, error_msg)
            end if
        end if
        if (binding%found) then
            if (binding%binding_kind == BINDING_ASSOCIATE_NAME) return
            declaration = query_declaration(arena, binding%declaration_node_index)
            if (.not. declaration%found) return
            if (.not. declaration%is_array) declaration%found = .false.
            return
        end if

        ! Parse-only callers do not populate the resolver's lexical scope
        ! bindings.  Match the already parsed declaration nodes in that
        ! scope, retaining the old query behavior without inspecting source
        ! text.  Semantic callers take the resolver path above, including its
        ! explicit rejection of ASSOCIATE aliases.
        scope_index = find_enclosing_scope(arena, node_index)
        fallback_index = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            declaration = query_declaration(arena, i)
            if (.not. declaration%found) cycle
            if (.not. declaration%is_array) cycle
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

    recursive integer function designator_rank(arena, node_index) result(rank)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(declaration_query_t) :: declaration
        type(declaration_binding_t) :: binding
        type(component_access_query_t) :: component
        type(storage_query_t) :: base_storage
        type(declaration_query_t) :: component_declaration
        type(resolved_type_query_t) :: resolved
        character(len=:), allocatable :: name, error_message
        character(len=:), allocatable :: base_type, derived_name
        integer :: base_rank, retained_dimensions, i, scope_index, fallback_index
        integer :: derived_index, component_index

        rank = -1
        if (.not. arena%has_node_at(node_index)) return
        resolved = query_resolved_type(arena, node_index)
        rank = resolved%rank

        select type (node => arena%entries(node_index)%node)
            type is (identifier_node)
            declaration = query_declaration(arena, node_index)
            if (.not. declaration%found) then
                call resolve_identifier_binding(arena, node_index, binding, &
                    error_message)
                if (binding%found .and. binding%binding_kind /= &
                    BINDING_ASSOCIATE_NAME) then
                    declaration = query_declaration(arena, &
                        binding%declaration_node_index)
                end if
            end if
            if (.not. declaration%found) then
                call identifier_name_at(arena, node_index, name)
                scope_index = find_enclosing_scope(arena, node_index)
                fallback_index = 0
                do i = 1, arena%size
                    if (.not. arena%has_node_at(i)) cycle
                    declaration = query_declaration(arena, i)
                    if (.not. declaration%found) cycle
                    if (.not. same_name(declaration%name, name)) cycle
                    if (fallback_index == 0) fallback_index = i
                    if (scope_index > 0 .and. node_is_in_scope(arena, i, &
                        scope_index)) then
                        fallback_index = i
                        exit
                    end if
                end do
                if (fallback_index > 0) declaration = query_declaration(arena, &
                    fallback_index)
            end if
            if (declaration%found) rank = declaration_rank(declaration)
            return
            type is (call_or_subscript_node)
            if (.not. allocated(node%arg_indices)) return
            if (size(node%arg_indices) == 0) return
            call resolve_array_element_declaration(arena, node_index, declaration)
            if (.not. declaration%found) return
            base_rank = declaration_rank(declaration)
            if (base_rank < 0) return
            retained_dimensions = 0
            do i = 1, size(node%arg_indices)
                if (subscript_retains_dimension(arena, node%arg_indices(i))) &
                    retained_dimensions = retained_dimensions + 1
            end do
            rank = max(0, base_rank - size(node%arg_indices) + &
                retained_dimensions)
            type is (array_slice_node)
            call resolve_array_element_declaration(arena, node_index, declaration)
            if (.not. declaration%found) return
            base_rank = declaration_rank(declaration)
            if (base_rank < 0) return
            retained_dimensions = 0
            do i = 1, node%num_dimensions
                if (subscript_retains_dimension(arena, node%bounds_indices(i))) &
                    retained_dimensions = retained_dimensions + 1
            end do
            rank = max(0, base_rank - node%num_dimensions + &
                retained_dimensions)
            type is (component_access_node)
            base_rank = designator_rank(arena, node%base_expr_index)
            if (base_rank < 0) return
            component = query_component_access(arena, node_index)
            base_storage = query_designator_storage(arena, &
                component%base_node_index)
            if (base_storage%found) then
                base_type = base_storage%type_name
                derived_name = derived_type_name_from_spec(base_type)
                derived_index = find_derived_type_by_name(arena, derived_name)
                if (derived_index > 0) then
                    component_index = find_component_declaration_in_hierarchy(&
                        arena, derived_index, component%component_name)
                    if (component_index > 0) then
                        component_declaration = query_declaration(arena, &
                            component_index)
                    end if
                    if (component_declaration%found) then
                        rank = component_rank(arena, component_declaration) + &
                            max(0, base_rank)
                        return
                    end if
                end if
            end if
            if (resolved%rank >= 0) then
                rank = resolved%rank + base_rank
            end if
        end select
    end function designator_rank

    logical function subscript_retains_dimension(arena, node_index) result(retains)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        retains = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (range_expression_node)
            retains = .true.
            type is (array_bounds_node)
            retains = .true.
        end select
    end function subscript_retains_dimension

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

    function query_component_path(arena, node_index, &
            allow_associate_selector) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical, intent(in), optional :: allow_associate_selector
        type(component_path_query_t) :: query
        integer, allocatable :: indices(:)
        character(len=:), allocatable :: names(:)
        type(storage_query_t) :: storage, base_storage, segment_storage
        integer :: i
        logical :: allow_selector

        allow_selector = .false.
        if (present(allow_associate_selector)) then
            allow_selector = allow_associate_selector
        end if

        allocate (character(len=1) :: query%component_names(0))
        allocate (query%component_node_indices(0))
        allocate (query%component_declaration_indices(0))
        if (.not. arena%has_node_at(node_index)) return
        call collect_component_path(arena, node_index, names, indices, &
            query%base_node_index)
        if (size(indices) == 0) return
        storage = query_storage(arena, node_index, allow_selector)
        if (.not. storage%found) return
        if (.not. storage%is_component) return
        query%found = .true.
        query%node_index = node_index
        query%component_names = names
        query%component_node_indices = indices
        deallocate (query%component_declaration_indices)
        allocate (query%component_declaration_indices(size(indices)))
        do i = 1, size(indices)
            segment_storage = query_storage(arena, indices(i), allow_selector)
            if (.not. segment_storage%found) then
                query%found = .false.
                deallocate (query%component_declaration_indices)
                allocate (query%component_declaration_indices(0))
                return
            end if
            query%component_declaration_indices(i) = &
                segment_storage%declaration_index
        end do
        query%base_rank = designator_rank(arena, query%base_node_index)
        query%rank = storage%rank
        query%storage_class = storage%storage_class
        query%is_array_element = storage%is_array_element
        query%is_array_section = storage%is_array_section
        query%is_derived = storage%is_derived
        query%is_concrete_derived = storage%is_concrete_derived
        query%is_allocatable = storage%is_allocatable
        query%is_pointer = storage%is_pointer
        query%is_polymorphic = storage%is_polymorphic
        query%is_unlimited_polymorphic = storage%is_unlimited_polymorphic
        base_storage = query_designator_storage(arena, query%base_node_index, &
            allow_selector)
        if (base_storage%found) then
            query%base_storage_class = base_storage%storage_class
        end if
    end function query_component_path

    function query_associate_selector(arena, node_index, association_index) &
            result(query)
        !! Return bounded facts for one ASSOCIATE association.
        !!
        !! NODE_INDEX may be an ASSOCIATE node with an optional association
        !! ordinal, or the selector expression itself.  The latter form is
        !! useful to a transformer walking expression nodes.  A selector
        !! expression is evaluated in the enclosing scope.  Storage facts
        !! are enabled only for this query, so the existing conservative
        !! query_storage behavior for associate-name aliases is unchanged.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(in), optional :: association_index
        type(associate_selector_query_t) :: query
        integer :: associate_index, selected_index

        call initialize_associate_selector_query(query)
        if (.not. arena%has_node_at(node_index)) return

        associate_index = 0
        selected_index = 0
        select type (node => arena%entries(node_index)%node)
            type is (associate_node)
            associate_index = node_index
            selected_index = 1
            if (present(association_index)) selected_index = association_index
        class default
            call find_associate_selector(arena, node_index, associate_index, &
                selected_index)
        end select
        if (associate_index <= 0 .or. selected_index <= 0) return
        call populate_associate_selector_query(arena, associate_index, &
            selected_index, query)
    end function query_associate_selector

    function query_associate_selectors(arena, associate_node_index) result(queries)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: associate_node_index
        type(associate_selector_query_t), allocatable :: queries(:)
        integer :: i, count

        allocate (queries(0))
        if (.not. arena%has_node_at(associate_node_index)) return
        select type (node => arena%entries(associate_node_index)%node)
            type is (associate_node)
            if (.not. allocated(node%associations)) return
            count = size(node%associations)
            if (count <= 0) return
            deallocate (queries)
            allocate (queries(count))
            do i = 1, count
                call initialize_associate_selector_query(queries(i))
                call populate_associate_selector_query(arena, &
                    associate_node_index, i, queries(i))
            end do
        class default
        end select
    end function query_associate_selectors

    subroutine initialize_associate_selector_query(query)
        type(associate_selector_query_t), intent(out) :: query

        call set_empty(query%associate_name)
        call set_empty(query%selector_declared_type)
        call set_empty(query%selector_dynamic_type)
        call set_empty(query%selector_storage%name)
        call set_empty(query%selector_storage%type_name)
        call initialize_component_path_query(query%selector_path)
    end subroutine initialize_associate_selector_query

    subroutine find_associate_selector(arena, node_index, associate_index, &
            association_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(out) :: associate_index, association_index
        integer :: current, i, guard

        associate_index = 0
        association_index = 0
        current = node_index
        guard = 0
        do while (current > 0 .and. arena%has_node_at(current))
            select type (node => arena%entries(current)%node)
                type is (associate_node)
                if (allocated(node%associations)) then
                    do i = 1, size(node%associations)
                        if (node%associations(i)%expr_index <= 0) cycle
                        if (node_is_in_scope(arena, node_index, &
                                node%associations(i)%expr_index)) then
                            associate_index = current
                            association_index = i
                            return
                        end if
                    end do
                end if
            class default
            end select
            current = arena%entries(current)%parent_index
            guard = guard + 1
            if (guard > arena%size) exit
        end do
    end subroutine find_associate_selector

    subroutine populate_associate_selector_query(arena, associate_index, &
            association_index, query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: associate_index, association_index
        type(associate_selector_query_t), intent(out) :: query
        type(resolved_type_query_t) :: resolved
        type(associate_node) :: node
        integer :: selector_index, designator_rank_value

        call initialize_associate_selector_query(query)
        query%associate_node_index = associate_index
        query%association_index = association_index
        if (.not. arena%has_node_at(associate_index)) return
        select type (arena_node => arena%entries(associate_index)%node)
            type is (associate_node)
            node = arena_node
        class default
            return
        end select
        if (.not. allocated(node%associations)) return
        if (association_index > size(node%associations)) return

        query%found = .true.
        if (allocated(node%associations(association_index)%name)) then
            query%associate_name = node%associations(association_index)%name
        end if
        selector_index = node%associations(association_index)%expr_index
        query%selector_node_index = selector_index
        if (selector_index <= 0 .or. .not. arena%has_node_at(selector_index)) then
            query%is_unresolved = .true.
            query%is_ambiguous = .true.
            query%is_alias_boundary = .true.
            return
        end if

        resolved = query_resolved_type(arena, selector_index)
        if (resolved%found) then
            query%declared_type_kind = resolved%type_kind
            query%declared_kind_value = resolved%kind_value
            query%declared_rank = resolved%rank
            if (len_trim(resolved%derived_type_name) > 0) then
                query%selector_declared_type = resolved%derived_type_name
            end if
        end if

        query%selector_path = query_component_path(arena, selector_index, .true.)
        query%selector_storage = query_designator_storage(arena, selector_index, &
            .true.)
        if (query%selector_storage%found) then
            query%is_storage_resolved = .true.
            query%selector_storage%node_index = selector_index
            query%selector_declaration_index = &
                query%selector_storage%declaration_index
            query%storage_identity_node_index = &
                query%selector_storage%declaration_index
            query%selector_declared_type = query%selector_storage%type_name
            query%is_pointer = query%selector_storage%is_pointer
            query%is_allocatable = query%selector_storage%is_allocatable
            query%is_polymorphic = query%selector_storage%is_polymorphic
            query%is_unlimited_polymorphic = &
                query%selector_storage%is_unlimited_polymorphic
        end if

        query%is_selector_designator = query%selector_path%found .or. &
            is_selector_array_designator(arena, selector_index, &
                query%selector_storage%found)
        if (query%selector_path%found) then
            query%base_node_index = query%selector_path%base_node_index
        else if (query%is_selector_designator) then
            query%base_node_index = selector_index
        end if
        if (query%is_selector_designator .and. query%selector_storage%found) then
            designator_rank_value = designator_rank(arena, selector_index)
            if (designator_rank_value >= 0) then
                query%selector_storage%rank = designator_rank_value
                query%declared_rank = designator_rank_value
            end if
            if (is_array_designator_node(arena, selector_index)) then
                query%selector_storage%is_array_element = &
                    is_array_element_node(arena, selector_index)
                query%selector_storage%is_array_section = &
                    is_array_section_node(arena, selector_index)
            end if
        end if

        query%is_alias = query%is_selector_designator .and. &
            query%is_storage_resolved
        query%is_ambiguous = query%is_pointer .or. query%is_polymorphic
        query%is_unresolved = .not. resolved%found .or. &
            (query%is_selector_designator .and. .not. query%is_storage_resolved)
        query%is_resolved = resolved%found .and. &
            (.not. query%is_selector_designator .or. query%is_storage_resolved)

        if (query%is_resolved .and. .not. query%is_pointer .and. &
                .not. query%is_polymorphic) then
            query%is_dynamic_type_known = .true.
            query%selector_dynamic_type = query%selector_declared_type
            if (len_trim(resolved%derived_type_name) > 0) then
                query%selector_dynamic_type = resolved%derived_type_name
            end if
            if (len_trim(query%selector_dynamic_type) > 0) then
                query%dynamic_type_index = find_derived_type_by_name(arena, &
                    derived_type_name_from_spec(query%selector_dynamic_type))
            end if
        end if

        query%is_alias_boundary = query%is_alias .and. &
            (query%is_pointer .or. query%is_polymorphic .or. &
             query%is_unresolved .or. query%is_ambiguous)
        query%is_read_only = .not. query%is_selector_designator .or. &
            query%is_pointer .or. query%is_polymorphic .or. query%is_ambiguous
        query%is_writeable = query%is_selector_designator .and. &
            query%is_storage_resolved .and. .not. query%is_read_only
        call collect_associate_access(arena, query)
    end subroutine populate_associate_selector_query

    logical function is_selector_array_designator(arena, node_index, found) &
            result(is_designator)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical, intent(in) :: found

        is_designator = found .and. is_array_designator_node(arena, node_index)
    end function is_selector_array_designator

    subroutine collect_associate_access(arena, query)
        type(ast_arena_t), intent(in) :: arena
        type(associate_selector_query_t), intent(inout) :: query
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_message
        integer :: i, access_kind
        logical :: ambiguous

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (.not. is_associate_reference_node(arena, i)) cycle
            if (query%selector_node_index > 0) then
                if (node_is_in_scope(arena, i, query%selector_node_index)) cycle
            end if
            call resolve_identifier_binding(arena, i, binding, error_message)
            if (.not. binding%found) cycle
            if (binding%binding_kind /= BINDING_ASSOCIATE_NAME) cycle
            if (binding%declaration_node_index /= query%associate_node_index) cycle
            if (binding%declaration_entity_index /= query%association_index) cycle
            call associate_reference_access(arena, i, query%associate_node_index, &
                access_kind, ambiguous)
            if (ambiguous) then
                query%has_ambiguous_access = .true.
                query%is_ambiguous = .true.
            end if
            select case (access_kind)
            case (ACCESS_WRITE)
                query%has_write_reference = .true.
            case (ACCESS_READ_WRITE)
                query%has_read_reference = .true.
                query%has_write_reference = .true.
            case default
                query%has_read_reference = .true.
            end select
        end do
        if (query%has_read_reference .and. query%has_write_reference) then
            query%association_access_kind = ACCESS_READ_WRITE
        else if (query%has_write_reference) then
            query%association_access_kind = ACCESS_WRITE
        else if (query%has_read_reference) then
            query%association_access_kind = ACCESS_READ
        end if
        if (query%has_ambiguous_access) then
            query%association_access_kind = ACCESS_READ_WRITE
            query%is_alias_boundary = .true.
        end if
        if (query%is_ambiguous) query%is_writeable = .false.
    end subroutine collect_associate_access

    logical function is_associate_reference_node(arena, node_index) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        found = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (identifier_node)
            found = .true.
            type is (call_or_subscript_node)
            found = .true.
        class default
        end select
    end function is_associate_reference_node

    subroutine associate_reference_access(arena, reference_index, associate_index, &
            access_kind, ambiguous)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: reference_index, associate_index
        integer, intent(out) :: access_kind
        logical, intent(out) :: ambiguous
        integer :: current

        access_kind = ACCESS_READ
        ambiguous = .false.
        current = arena%entries(reference_index)%parent_index
        do while (current > 0 .and. arena%has_node_at(current))
            if (current == associate_index) return
            select type (node => arena%entries(current)%node)
                type is (assignment_node)
                if (node_is_in_scope(arena, reference_index, node%target_index)) then
                    access_kind = ACCESS_WRITE
                else if (node_is_in_scope(arena, reference_index, node%value_index)) then
                    access_kind = ACCESS_READ
                end if
                return
                type is (pointer_assignment_node)
                if (node_is_in_scope(arena, reference_index, node%pointer_index)) then
                    access_kind = ACCESS_WRITE
                else if (node_is_in_scope(arena, reference_index, node%target_index)) then
                    access_kind = ACCESS_READ
                end if
                ambiguous = .true.
                return
                type is (subroutine_call_node)
                access_kind = ACCESS_READ_WRITE
                ambiguous = .true.
                return
                type is (call_or_subscript_node)
                if (current /= reference_index) then
                    access_kind = ACCESS_READ_WRITE
                    ambiguous = .true.
                    return
                end if
            class default
            end select
            current = arena%entries(current)%parent_index
        end do
    end subroutine associate_reference_access

    subroutine set_associate_access_kind(query)
        type(associate_selector_query_t), intent(inout) :: query

        if (query%has_read_reference .and. query%has_write_reference) then
            query%association_access_kind = ACCESS_READ_WRITE
        else if (query%has_write_reference) then
            query%association_access_kind = ACCESS_WRITE
        else if (query%has_read_reference) then
            query%association_access_kind = ACCESS_READ
        else
            query%association_access_kind = 0
        end if
    end subroutine set_associate_access_kind

    function query_polymorphic_allocation(arena, allocation_node_index) result(query)
        !! Return the bounded SOURCE= fact for one polymorphic allocation.
        !!
        !! FOUND identifies a directly resolved polymorphic allocatable target;
        !! IS_BOUNDED additionally requires one scalar concrete data-designator
        !! source and one acquisition in the enclosing scope.  Calls, dynamic
        !! polymorphic sources, aliases, and repeated acquisitions remain
        !! observable as unbounded facts rather than being guessed as concrete.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: allocation_node_index
        type(polymorphic_allocation_query_t) :: query
        type(storage_query_t) :: owner_storage, source_storage
        type(allocate_statement_node) :: allocation
        logical :: owner_alias, source_alias, has_type_spec
        integer :: owner_index, scope_index

        call initialize_polymorphic_allocation_query(query)
        if (.not. arena%has_node_at(allocation_node_index)) return
        select type (node => arena%entries(allocation_node_index)%node)
            type is (allocate_statement_node)
            allocation = node
        class default
            return
        end select

        query%allocation_node_index = allocation_node_index
        query%source_expr_index = allocation%source_expr_index
        has_type_spec = .false.
        if (allocated(allocation%type_spec)) then
            has_type_spec = len_trim(allocation%type_spec) > 0
        end if
        if (.not. allocated(allocation%var_indices)) return
        if (size(allocation%var_indices) /= 1) return
        owner_index = allocation%var_indices(1)
        query%owner_node_index = owner_index
        query%owner_path = ownership_path(arena, owner_index)
        owner_alias = is_associate_selector_node(arena, owner_index)
        if (owner_alias) query%is_alias = .true.

        owner_storage = query_polymorphic_owner_storage(arena, owner_index)
        if (.not. owner_storage%found) return
        if (.not. owner_storage%is_allocatable) return
        if (.not. owner_storage%is_polymorphic) return
        if (owner_storage%is_pointer .or. owner_storage%is_target) then
            query%is_alias = .true.
        end if

        query%found = .true.
        query%owner_declaration_index = owner_storage%declaration_index
        query%owner_declared_type = owner_storage%type_name
        if (allocation%source_expr_index <= 0) return

        query%source_path = ownership_path(arena, allocation%source_expr_index)
        source_alias = is_associate_selector_node(arena, &
            allocation%source_expr_index)
        if (source_alias) query%is_alias = .true.
        source_storage = query_designator_storage(arena, &
            allocation%source_expr_index)
        if (.not. source_storage%found) then
            query%is_factory_source = is_factory_source_expression(arena, &
                allocation%source_expr_index)
            if (.not. query%is_factory_source) then
                if (is_identifier_at(arena, allocation%source_expr_index)) then
                    query%is_alias = .true.
                end if
            end if
            return
        end if

        query%source_declaration_index = source_storage%declaration_index
        if (source_storage%is_polymorphic) then
            query%source_classification = POLYMORPHIC_SOURCE_POLYMORPHIC
            query%is_source_concrete = .false.
            query%is_source_polymorphic = .true.
            query%is_source_unknown = .false.
        else if (source_storage%is_concrete_derived .and. &
                source_storage%rank == 0) then
            query%source_classification = POLYMORPHIC_SOURCE_CONCRETE
            query%source_resolved_type = derived_type_name_from_spec( &
                source_storage%type_name)
            query%is_source_concrete = len_trim(query%source_resolved_type) > 0
            query%is_source_unknown = .not. query%is_source_concrete
        end if
        if (source_storage%is_pointer .or. source_storage%is_target .or. &
            source_storage%is_allocatable) query%is_alias = .true.

        scope_index = find_enclosing_scope(arena, allocation_node_index)
        if (scope_index > 0) then
            query%is_repeated_acquisition = has_repeated_polymorphic_acquisition( &
                arena, allocation_node_index, owner_index, scope_index)
        end if
        query%is_bounded = query%is_source_concrete .and. &
            .not. query%is_source_polymorphic .and. .not. query%is_alias .and. &
            .not. query%is_factory_source .and. &
            .not. query%is_repeated_acquisition .and. &
            owner_storage%rank == 0 .and. source_storage%rank == 0 .and. &
            allocation%mold_expr_index == 0 .and. .not. has_type_spec
    end function query_polymorphic_allocation

    subroutine initialize_polymorphic_allocation_query(query)
        type(polymorphic_allocation_query_t), intent(out) :: query

        call set_empty(query%owner_declared_type)
        call set_empty(query%source_resolved_type)
        call initialize_component_path_query(query%owner_path)
        call initialize_component_path_query(query%source_path)
    end subroutine initialize_polymorphic_allocation_query

    logical function is_factory_source_expression(arena, node_index) result(is_factory)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_factory = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (call_or_subscript_node)
            if (.not. is_array_designator_node(arena, node_index)) then
                is_factory = .true.
            end if
        class default
        end select
    end function is_factory_source_expression

    logical function has_repeated_polymorphic_acquisition(arena, &
            allocation_node_index, owner_index, scope_index) result(repeated)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: allocation_node_index, owner_index, scope_index
        integer :: i, candidate_owner

        repeated = .false.
        do i = 1, arena%size
            if (i == allocation_node_index) cycle
            if (.not. arena%has_node_at(i)) cycle
            if (.not. node_is_in_scope(arena, i, scope_index)) cycle
            select type (node => arena%entries(i)%node)
                type is (allocate_statement_node)
                if (.not. allocated(node%var_indices)) cycle
                if (size(node%var_indices) /= 1) cycle
                candidate_owner = node%var_indices(1)
                if (same_allocation_owner(arena, owner_index, candidate_owner)) then
                    repeated = .true.
                    return
                end if
            class default
            end select
        end do
    end function has_repeated_polymorphic_acquisition

    logical function same_allocation_owner(arena, left_index, right_index) &
            result(same)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: left_index, right_index
        type(component_path_query_t) :: left_path, right_path
        type(storage_query_t) :: left_base, right_base, left_storage, right_storage
        integer :: i

        same = .false.
        left_path = query_component_path(arena, left_index)
        right_path = query_component_path(arena, right_index)
        if (left_path%found .neqv. right_path%found) return
        if (left_path%found) then
            if (size(left_path%component_names) /= &
                size(right_path%component_names)) return
            do i = 1, size(left_path%component_names)
                if (.not. same_name(left_path%component_names(i), &
                    right_path%component_names(i))) return
            end do
            left_base = query_designator_storage(arena, left_path%base_node_index)
            right_base = query_designator_storage(arena, right_path%base_node_index)
            if (.not. left_base%found .or. .not. right_base%found) return
            if (left_base%declaration_index /= right_base%declaration_index) return
            if (size(left_path%component_declaration_indices) /= &
                size(right_path%component_declaration_indices)) return
            do i = 1, size(left_path%component_declaration_indices)
                if (left_path%component_declaration_indices(i) /= &
                    right_path%component_declaration_indices(i)) return
            end do
            same = .true.
            return
        end if

        left_storage = query_designator_storage(arena, left_index)
        right_storage = query_designator_storage(arena, right_index)
        if (.not. left_storage%found .or. .not. right_storage%found) return
        same = left_storage%declaration_index == right_storage%declaration_index
    end function same_allocation_owner

    function query_designator_storage(arena, node_index, &
            allow_associate_selector) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical, intent(in), optional :: allow_associate_selector
        type(storage_query_t) :: query
        type(declaration_query_t) :: declaration
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg, name
        integer :: i, fallback_index, scope_index
        logical :: allow_selector

        allow_selector = .false.
        if (present(allow_associate_selector)) then
            allow_selector = allow_associate_selector
        end if
        query = query_storage(arena, node_index, allow_selector)
        if (query%found) return
        call identifier_name_at(arena, node_index, name)
        if (len_trim(name) > 0) then
            scope_index = find_enclosing_scope(arena, node_index)
            fallback_index = 0
            do i = 1, arena%size
                if (.not. arena%has_node_at(i)) cycle
                declaration = query_declaration(arena, i)
                if (.not. declaration%found) cycle
                if (.not. same_name(declaration%name, name)) cycle
                if (scope_index > 0 .and. node_is_in_scope(arena, i, &
                    scope_index)) then
                    fallback_index = i
                else if (fallback_index == 0) then
                    fallback_index = i
                end if
            end do
            if (fallback_index > 0) then
                query = query_storage(arena, fallback_index, allow_selector)
                if (query%found) return
            end if
        end if
        call resolve_identifier_binding(arena, node_index, binding, error_msg)
        if (binding%found) then
            if (binding%binding_kind == BINDING_ASSOCIATE_NAME) return
            query = query_storage(arena, binding%declaration_node_index, &
                allow_selector)
            if (query%found) return
        end if
        if (.not. is_array_designator_node(arena, node_index)) return
        call resolve_array_element_declaration(arena, node_index, declaration)
        if (.not. declaration%found) return
        query = query_storage(arena, declaration%node_index, allow_selector)
    end function query_designator_storage

    function query_polymorphic_owner_storage(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(storage_query_t) :: query, candidate
        type(declaration_query_t) :: declaration
        character(len=:), allocatable :: name
        integer :: i

        query = query_designator_storage(arena, node_index)
        if (query%found) then
            if (query%is_allocatable .and. query%is_polymorphic) return
        end if
        call identifier_name_at(arena, node_index, name)
        if (len_trim(name) == 0) return
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            declaration = query_declaration(arena, i)
            if (.not. declaration%found) cycle
            if (.not. same_name(declaration%name, name)) cycle
            candidate = query_storage(arena, i)
            if (.not. candidate%found) cycle
            if (.not. candidate%is_allocatable) cycle
            if (.not. candidate%is_polymorphic) cycle
            query = candidate
        end do
    end function query_polymorphic_owner_storage

    function query_type_bound_call(arena, call_node_index) result(query)
        !! Resolve one type-bound call into receiver and binding facts.
        !!
        !! The receiver must have a declared derived type.  The query then
        !! delegates binding and descendant enumeration to
        !! query_type_binding_resolution, so it does not repeat EXTENDS
        !! traversal.  Generic, ambiguous, deferred, and unresolved cases
        !! remain visible in the result without selecting a procedure.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        type(type_bound_call_query_t) :: query
        type(binding_resolution_query_t) :: resolution
        character(len=:), allocatable :: receiver_type_name
        character(len=:), allocatable :: binding_name
        logical :: is_call

        call initialize_type_bound_call_query(query)
        if (.not. arena%has_node_at(call_node_index)) return

        call get_type_bound_call_parts(arena, call_node_index, &
            query%receiver_node_index, query%receiver_name, binding_name, &
            is_call)
        if (.not. is_call) return
        if (len_trim(binding_name) == 0) return
        query%call_node_index = call_node_index
        query%receiver_path = query_component_path(arena, &
            query%receiver_node_index)

        call resolve_type_bound_receiver(arena, call_node_index, &
            query%receiver_node_index, query%receiver_name, &
            query%receiver_declaration_index, receiver_type_name)
        query%declared_type_name = receiver_type_name
        query%binding_name = trim(binding_name)
        if (len_trim(receiver_type_name) == 0) then
            query%is_unresolved = .true.
            return
        end if

        query%declared_type_index = find_derived_type_by_name(arena, &
            receiver_type_name)
        if (query%declared_type_index <= 0) then
            query%is_unresolved = .true.
            return
        end if

        resolution = query_type_binding_resolution(arena, &
            query%declared_type_index, binding_name)
        if (.not. resolution%found) then
            query%is_unresolved = .true.
            return
        end if

        query%found = .true.
        query%declaring_type_index = resolution%declaring_type_index
        query%resolved_type_index = resolution%resolved_type_index
        query%binding_node_index = resolution%binding_node_index
        query%implementation = resolution%implementation
        query%interface_name = resolution%interface_name
        query%pass_name = resolution%pass_name
        query%is_inherited = resolution%is_inherited
        query%is_generic = resolution%is_generic
        query%is_deferred = resolution%is_deferred
        query%is_abstract_type = resolution%is_abstract_type
        query%pass_arg = resolution%pass_arg
        query%is_ambiguous = query%is_generic .and. &
            size(resolution%generic_names) > 1

        ! query_type_binding_resolution deliberately exposes descendant
        ! implementations for ordinary and deferred bindings.  Generic
        ! dispatch still needs argument matching, so this call-site query
        ! refuses those targets rather than forwarding a guessed candidate.
        if (.not. query%is_generic) then
            query%dispatch_target_type_indices = &
                resolution%dispatch_target_type_indices
            query%dispatch_target_implementations = &
                resolution%dispatch_target_implementations
            query%dispatch_target_implementation_node_indices = &
                resolution%dispatch_target_implementation_node_indices
            query%dispatch_target_pass_names = &
                resolution%dispatch_target_pass_names
            query%dispatch_target_pass_positions = &
                resolution%dispatch_target_pass_positions
            query%dispatch_target_passed_object_types = &
                resolution%dispatch_target_passed_object_types
            query%dispatch_target_signature_resolved = &
                resolution%dispatch_target_signature_resolved
        end if
        query%is_resolved = .not. query%is_generic .and. &
            .not. query%is_ambiguous .and. .not. query%is_deferred .and. &
            len_trim(query%implementation) > 0
        if (.not. query%is_resolved .and. .not. query%is_generic .and. &
            .not. query%is_deferred) query%is_unresolved = .true.
    end function query_type_bound_call

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
        query%implementation_node_index = find_procedure_definition(arena, &
            query%implementation)

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
                        target%implementation_node_index = &
                            concrete%implementation_node_index
                        exit
                    end if
                end do
            end if
            if (len_trim(target%implementation) == 0) cycle
            call append_dispatch_target(arena, query, target_type, target)
        end do
    end function query_type_binding_resolution

    function query_type_binding_hierarchy(arena, derived_type_index, &
            binding_name) result(query)
        !! Report one binding through the queried type's parent chain.
        !!
        !! This is deliberately a fixed, local query.  It walks only
        !! ``EXTENDS`` parents from ``derived_type_index`` and never scans
        !! descendants or invents a runtime dispatch target.  A generic with
        !! more than one possible procedure is therefore marked ambiguous
        !! and has no implementation guess.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: derived_type_index
        character(len=*), intent(in) :: binding_name
        type(binding_hierarchy_query_t) :: query
        type(derived_type_query_t) :: derived
        type(derived_type_query_t) :: parent
        type(type_binding_query_t) :: binding
        type(binding_hierarchy_entry_t), allocatable :: entries(:)
        integer, allocatable :: chain(:)
        integer :: current_index, parent_index, i, n, matches, width

        call initialize_binding_hierarchy(query, binding_name)
        if (.not. arena%has_node_at(derived_type_index)) return
        derived = query_derived_type(arena, derived_type_index)
        if (.not. derived%found) return

        query%declared_type_index = derived_type_index
        query%declared_type_name = derived%name
        allocate (chain(0))
        current_index = derived_type_index
        do
            call append_type_index(chain, current_index)
            derived = query_derived_type(arena, current_index)
            if (.not. derived%found) exit
            if (len_trim(derived%extends_parent) == 0) exit
            parent_index = find_derived_type_by_name(arena, derived%extends_parent)
            if (parent_index <= 0) then
                exit
            end if
            current_index = parent_index
            if (size(chain) > arena%size) then
                exit
            end if
        end do

        n = size(chain)
        allocate (entries(n))
        do i = n, 1, -1
            call initialize_binding_hierarchy_entry(entries(i))
            derived = query_derived_type(arena, chain(i))
            entries(i)%type_index = chain(i)
            if (derived%found) then
                entries(i)%type_name = derived%name
                entries(i)%is_abstract_type = contains_word( &
                    derived%attribute_clause, 'abstract')
            end if
            if (i < n) then
                entries(i)%parent_type_index = chain(i + 1)
                parent = query_derived_type(arena, chain(i + 1))
                if (parent%found) entries(i)%parent_type_name = parent%name
            end if

            call find_local_hierarchy_binding(arena, chain(i), binding_name, &
                binding, matches)
            if (matches > 0) then
                call fill_local_hierarchy_entry(arena, entries(i), binding, &
                    matches)
            else if (i < n) then
                if (entries(i + 1)%found) then
                    call inherit_hierarchy_entry(entries(i), entries(i + 1))
                end if
            end if
        end do

        deallocate (query%hierarchy)
        allocate (query%hierarchy(n))
        do i = 1, n
            query%hierarchy(i) = entries(i)
        end do
        width = 1
        do i = 2, n
            width = max(width, len_trim(entries(i)%type_name))
        end do
        deallocate (query%parent_type_names, query%parent_type_indices)
        allocate (character(len=width) :: query%parent_type_names(max(0, n - 1)))
        allocate (query%parent_type_indices(max(0, n - 1)))
        do i = 2, n
            query%parent_type_indices(i - 1) = chain(i)
            query%parent_type_names(i - 1) = entries(i)%type_name
        end do

        call copy_hierarchy_summary(query, entries(1))
        if (.not. query%found) query%is_unresolved = .true.
    end function query_type_binding_hierarchy

    subroutine initialize_binding_hierarchy(query, requested_name)
        type(binding_hierarchy_query_t), intent(out) :: query
        character(len=*), intent(in) :: requested_name

        query%requested_name = trim(requested_name)
        call set_empty(query%declared_type_name)
        call set_empty(query%declaring_type_name)
        call set_empty(query%binding_name)
        call set_empty(query%implementation)
        query%implementation_node_index = 0
        call set_empty(query%interface_name)
        call set_empty(query%pass_name)
        allocate (query%parent_type_indices(0))
        allocate (character(len=1) :: query%parent_type_names(0))
        allocate (query%hierarchy(0))
    end subroutine initialize_binding_hierarchy

    subroutine initialize_binding_hierarchy_entry(entry)
        type(binding_hierarchy_entry_t), intent(out) :: entry

        call set_empty(entry%type_name)
        call set_empty(entry%parent_type_name)
        call set_empty(entry%declaring_type_name)
        call set_empty(entry%binding_name)
        call set_empty(entry%implementation)
        entry%implementation_node_index = 0
        call set_empty(entry%implementation_pass_name)
        entry%implementation_pass_position = 0
        call set_empty(entry%implementation_passed_object_type)
        entry%implementation_signature_resolved = .false.
        call set_empty(entry%interface_name)
        call set_empty(entry%pass_name)
    end subroutine initialize_binding_hierarchy_entry

    subroutine append_type_index(indices, value)
        integer, allocatable, intent(inout) :: indices(:)
        integer, intent(in) :: value
        integer, allocatable :: grown(:)
        integer :: n

        n = size(indices)
        allocate (grown(n + 1))
        if (n > 0) grown(:n) = indices
        grown(n + 1) = value
        call move_alloc(grown, indices)
    end subroutine append_type_index

    subroutine find_local_hierarchy_binding(arena, type_index, binding_name, &
            binding, matches)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: type_index
        character(len=*), intent(in) :: binding_name
        type(type_binding_query_t), intent(out) :: binding
        integer, intent(out) :: matches
        type(derived_type_query_t) :: derived
        type(type_binding_query_t) :: candidate
        integer :: i

        binding = query_type_binding(arena, 0)
        matches = 0
        derived = query_derived_type(arena, type_index)
        if (.not. derived%found) return
        do i = 1, size(derived%binding_indices)
            candidate = query_type_binding(arena, derived%binding_indices(i))
            if (.not. candidate%found) cycle
            if (.not. binding_matches(candidate, binding_name)) cycle
            matches = matches + 1
            if (matches == 1) binding = candidate
        end do
    end subroutine find_local_hierarchy_binding

    subroutine fill_local_hierarchy_entry(arena, entry, binding, matches)
        type(ast_arena_t), intent(in) :: arena
        type(binding_hierarchy_entry_t), intent(inout) :: entry
        type(type_binding_query_t), intent(in) :: binding
        integer, intent(in) :: matches
        logical :: has_implementation

        entry%found = .true.
        entry%binding_node_index = binding%node_index
        entry%declaring_type_index = entry%type_index
        entry%declaring_type_name = entry%type_name
        entry%binding_name = binding%binding_name
        entry%interface_name = binding%interface_name
        entry%pass_name = binding%pass_name
        entry%is_local = .true.
        entry%is_inherited = .false.
        entry%is_generic = binding%is_generic
        entry%is_deferred = binding%is_deferred
        entry%pass_arg = binding%pass_arg
        entry%is_ambiguous = matches > 1
        if (binding%is_generic) then
            if (size(binding%generic_names) > 1) entry%is_ambiguous = .true.
        end if
        call set_empty(entry%implementation)

        if (.not. entry%is_ambiguous) then
            if (binding%is_generic) then
                if (size(binding%generic_names) == 1) then
                    entry%implementation = binding%generic_names(1)
                end if
            else if (allocated(binding%implementation)) then
                if (len_trim(binding%implementation) > 0) then
                    entry%implementation = binding%implementation
                end if
            else
                entry%implementation = binding%binding_name
            end if
        end if

        has_implementation = len_trim(entry%implementation) > 0
        if (entry%is_deferred) then
            call set_empty(entry%implementation)
            has_implementation = .false.
        end if
        entry%is_resolved = has_implementation .and. .not. entry%is_ambiguous
        call fill_implementation_facts(arena, entry)
    end subroutine fill_local_hierarchy_entry

    subroutine fill_implementation_facts(arena, entry)
        type(ast_arena_t), intent(in) :: arena
        type(binding_hierarchy_entry_t), intent(inout) :: entry

        call resolve_dispatch_signature(arena, entry%implementation, &
            entry%pass_arg, entry%pass_name, entry%implementation_pass_name, &
            entry%implementation_pass_position, &
            entry%implementation_passed_object_type, &
            entry%implementation_signature_resolved, &
            entry%implementation_node_index)
    end subroutine fill_implementation_facts

    subroutine inherit_hierarchy_entry(entry, parent)
        type(binding_hierarchy_entry_t), intent(inout) :: entry
        type(binding_hierarchy_entry_t), intent(in) :: parent

        entry%found = parent%found
        entry%binding_node_index = parent%binding_node_index
        entry%declaring_type_index = parent%declaring_type_index
        entry%declaring_type_name = parent%declaring_type_name
        entry%binding_name = parent%binding_name
        entry%implementation = parent%implementation
        entry%implementation_node_index = parent%implementation_node_index
        entry%implementation_pass_name = parent%implementation_pass_name
        entry%implementation_pass_position = parent%implementation_pass_position
        entry%implementation_passed_object_type = &
            parent%implementation_passed_object_type
        entry%implementation_signature_resolved = &
            parent%implementation_signature_resolved
        entry%interface_name = parent%interface_name
        entry%pass_name = parent%pass_name
        entry%is_local = .false.
        entry%is_inherited = parent%found
        entry%is_generic = parent%is_generic
        entry%is_deferred = parent%is_deferred
        entry%is_ambiguous = parent%is_ambiguous
        entry%is_resolved = parent%is_resolved
        entry%pass_arg = parent%pass_arg
    end subroutine inherit_hierarchy_entry

    subroutine copy_hierarchy_summary(query, entry)
        type(binding_hierarchy_query_t), intent(inout) :: query
        type(binding_hierarchy_entry_t), intent(in) :: entry

        query%found = entry%found
        query%declaring_type_index = entry%declaring_type_index
        query%declaring_type_name = entry%declaring_type_name
        query%binding_node_index = entry%binding_node_index
        query%binding_name = entry%binding_name
        query%implementation = entry%implementation
        query%implementation_node_index = entry%implementation_node_index
        query%implementation_pass_name = entry%implementation_pass_name
        query%implementation_pass_position = entry%implementation_pass_position
        query%implementation_passed_object_type = &
            entry%implementation_passed_object_type
        query%implementation_signature_resolved = &
            entry%implementation_signature_resolved
        query%interface_name = entry%interface_name
        query%pass_name = entry%pass_name
        query%is_inherited = entry%is_inherited
        query%is_generic = entry%is_generic
        query%is_deferred = entry%is_deferred
        query%is_abstract_type = entry%is_abstract_type
        query%is_ambiguous = entry%is_ambiguous
        query%is_resolved = entry%is_resolved
        query%pass_arg = entry%pass_arg
    end subroutine copy_hierarchy_summary

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

    subroutine initialize_type_bound_call_query(query)
        type(type_bound_call_query_t), intent(out) :: query

        call set_empty(query%receiver_name)
        call set_empty(query%declared_type_name)
        call set_empty(query%binding_name)
        call set_empty(query%implementation)
        call set_empty(query%interface_name)
        call set_empty(query%pass_name)
        call initialize_component_path_query(query%receiver_path)
        allocate (query%dispatch_target_type_indices(0))
        allocate (character(len=1) :: query%dispatch_target_implementations(0))
        allocate (query%dispatch_target_implementation_node_indices(0))
        allocate (character(len=1) :: query%dispatch_target_pass_names(0))
        allocate (query%dispatch_target_pass_positions(0))
        allocate (character(len=1) :: query%dispatch_target_passed_object_types(0))
        allocate (query%dispatch_target_signature_resolved(0))
    end subroutine initialize_type_bound_call_query

    subroutine initialize_component_path_query(query)
        type(component_path_query_t), intent(out) :: query

        allocate (character(len=1) :: query%component_names(0))
        allocate (query%component_node_indices(0))
        allocate (query%component_declaration_indices(0))
    end subroutine initialize_component_path_query

    subroutine get_type_bound_call_parts(arena, call_node_index, &
            receiver_node_index, receiver_name, binding_name, is_call)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        integer, intent(out) :: receiver_node_index
        character(len=:), allocatable, intent(out) :: receiver_name
        character(len=:), allocatable, intent(out) :: binding_name
        logical, intent(out) :: is_call
        integer :: separator
        character(len=:), allocatable :: designator

        receiver_node_index = 0
        call set_empty(receiver_name)
        call set_empty(binding_name)
        is_call = .false.
        if (.not. arena%has_node_at(call_node_index)) return

        select type (node => arena%entries(call_node_index)%node)
            type is (subroutine_call_node)
            if (.not. allocated(node%name)) return
            designator = trim(node%name)
            separator = index(designator, '%', back=.true.)
            if (separator <= 1) return
            receiver_name = trim(designator(:separator - 1))
            binding_name = trim(designator(separator + 1:))
            if (index(receiver_name, '(') > 0) return
            if (index(receiver_name, '[') > 0) return
            if (index(binding_name, '%') > 0) return
            if (len_trim(receiver_name) == 0) return
            if (len_trim(binding_name) == 0) return
            is_call = .true.
            type is (call_or_subscript_node)
            if (node%is_array_access) return
            if (node%base_expr_index <= 0) return
            if (.not. arena%has_node_at(node%base_expr_index)) return
            select type (base => arena%entries(node%base_expr_index)%node)
                type is (component_access_node)
                if (.not. allocated(base%component_name)) return
                receiver_node_index = base%base_expr_index
                if (receiver_node_index <= 0) return
                binding_name = trim(base%component_name)
                call receiver_designator_name(arena, receiver_node_index, &
                    receiver_name)
                if (len_trim(binding_name) == 0) return
                is_call = .true.
            class default
                return
            end select
        end select
    end subroutine get_type_bound_call_parts

    recursive subroutine receiver_designator_name(arena, node_index, name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        character(len=:), allocatable :: base_name

        call set_empty(name)
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (identifier_node)
            if (allocated(node%name)) name = trim(node%name)
            type is (component_access_node)
            if (.not. allocated(node%component_name)) return
            call receiver_designator_name(arena, node%base_expr_index, base_name)
            if (len_trim(base_name) > 0) then
                name = trim(base_name)//'%'//trim(node%component_name)
            end if
            type is (call_or_subscript_node)
            if (allocated(node%name)) name = trim(node%name)
        end select
    end subroutine receiver_designator_name

    subroutine resolve_type_bound_receiver(arena, call_node_index, &
            receiver_node_index, receiver_name, declaration_index, type_name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        integer, intent(in) :: receiver_node_index
        character(len=*), intent(in) :: receiver_name
        integer, intent(out) :: declaration_index
        character(len=:), allocatable, intent(out) :: type_name
        type(declaration_binding_t) :: binding
        type(declaration_query_t) :: declaration
        type(resolved_type_query_t) :: resolved
        character(len=:), allocatable :: error_msg

        declaration_index = 0
        call set_empty(type_name)

        if (receiver_node_index > 0) then
            resolved = query_resolved_type(arena, receiver_node_index)
            if (resolved%found) then
                if (len_trim(resolved%derived_type_name) > 0) then
                    type_name = trim(resolved%derived_type_name)
                end if
            end if
            if (len_trim(type_name) == 0 .and. len_trim(receiver_name) > 0) then
                call resolve_receiver_designator(arena, call_node_index, &
                    receiver_name, binding, type_name)
                if (binding%found) declaration_index = &
                    binding%declaration_node_index
            end if
        end if

        if (receiver_node_index > 0) then
            select type (receiver => arena%entries(receiver_node_index)%node)
                type is (identifier_node)
                call resolve_identifier_binding(arena, receiver_node_index, &
                    binding, error_msg)
                if (binding%found) declaration_index = &
                    binding%declaration_node_index
            class default
            end select
        else
            if (len_trim(receiver_name) > 0) then
                call resolve_receiver_designator(arena, call_node_index, &
                    receiver_name, binding, type_name)
                if (binding%found) declaration_index = &
                    binding%declaration_node_index
            end if
        end if

        if (declaration_index <= 0) return
        declaration = query_declaration(arena, declaration_index)
        if (.not. declaration%found) return
        if (len_trim(declaration%type_name) > 0) then
            if (len_trim(type_name) == 0) then
                type_name = declared_type_name(declaration%type_name)
            end if
        end if
    end subroutine resolve_type_bound_receiver

    subroutine resolve_receiver_designator(arena, call_node_index, name, &
            binding, type_name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        character(len=*), intent(in) :: name
        type(declaration_binding_t), intent(out) :: binding
        character(len=:), allocatable, intent(out) :: type_name
        character(len=:), allocatable :: base_name, remaining
        character(len=:), allocatable :: component_name, component_type
        character(len=:), allocatable :: error_msg
        type(declaration_query_t) :: declaration, component_declaration
        type(derived_type_query_t) :: derived
        integer :: separator, start, derived_index, component_index

        binding%found = .false.
        binding%declaration_node_index = 0
        call set_empty(type_name)
        separator = index(trim(name), '%')
        if (separator <= 0) then
            call resolve_name_at_node(arena, call_node_index, trim(name), &
                binding, error_msg)
            return
        end if

        base_name = trim(name(:separator - 1))
        call resolve_name_at_node(arena, call_node_index, base_name, binding, &
            error_msg)
        if (.not. binding%found) return
        declaration = query_declaration(arena, binding%declaration_node_index)
        if (.not. declaration%found) return
        type_name = declared_type_name(declaration%type_name)
        remaining = trim(name(separator + 1:))
        start = 1
        do
            separator = index(remaining(start:), '%')
            if (separator <= 0) then
                component_name = trim(remaining(start:))
            else
                component_name = trim(remaining(start:start + separator - 2))
            end if
            if (len_trim(component_name) == 0) then
                call set_empty(type_name)
                return
            end if
            derived_index = find_derived_type_by_name(arena, type_name)
            if (derived_index <= 0) then
                call set_empty(type_name)
                return
            end if
            derived = query_derived_type(arena, derived_index)
            component_index = find_component_declaration(arena, derived, &
                component_name)
            if (component_index <= 0) then
                call set_empty(type_name)
                return
            end if
            component_declaration = query_declaration(arena, component_index)
            if (.not. component_declaration%found) then
                call set_empty(type_name)
                return
            end if
            component_type = declared_type_name(component_declaration%type_name)
            type_name = component_type
            if (separator <= 0) exit
            start = start + separator
            if (start > len(remaining)) then
                call set_empty(type_name)
                return
            end if
        end do
    end subroutine resolve_receiver_designator

    integer function find_component_declaration(arena, derived, name) result(index)
        type(ast_arena_t), intent(in) :: arena
        type(derived_type_query_t), intent(in) :: derived
        character(len=*), intent(in) :: name
        type(declaration_query_t) :: declaration
        integer :: i, candidate

        index = 0
        do i = 1, size(derived%component_indices)
            candidate = derived%component_indices(i)
            declaration = query_declaration(arena, candidate)
            if (.not. declaration%found) cycle
            if (same_name(declaration%name, name)) then
                index = candidate
                return
            end if
        end do
    end function find_component_declaration

    integer function find_component_declaration_in_hierarchy(arena, &
            derived_index, name) result(index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: derived_index
        character(len=*), intent(in) :: name
        type(derived_type_query_t) :: derived
        integer :: current_index, parent_index, guard

        index = 0
        current_index = derived_index
        guard = 0
        do while (current_index > 0)
            derived = query_derived_type(arena, current_index)
            if (.not. derived%found) return
            index = find_component_declaration(arena, derived, name)
            if (index > 0) return
            if (len_trim(derived%extends_parent) == 0) return
            parent_index = find_derived_type_by_name(arena, &
                derived%extends_parent)
            if (parent_index <= 0) return
            current_index = parent_index
            guard = guard + 1
            if (guard > arena%size) return
        end do
    end function find_component_declaration_in_hierarchy

    function declared_type_name(source) result(name)
        character(len=*), intent(in) :: source
        character(len=:), allocatable :: name
        character(len=:), allocatable :: lowered
        integer :: left, right, prefix_length

        name = trim(source)
        lowered = lower_text(name)
        left = index(lowered, 'class(')
        prefix_length = len('class(')
        if (left /= 1) then
            left = index(lowered, 'type(')
            prefix_length = len('type(')
        end if
        if (left == 1) then
            right = index(name, ')')
            if (right > prefix_length) then
                name = trim(name(prefix_length + 1:right - 1))
            end if
        end if
    end function declared_type_name

    subroutine initialize_binding_resolution(query, requested_name)
        type(binding_resolution_query_t), intent(out) :: query
        character(len=*), intent(in) :: requested_name

        query%requested_name = trim(requested_name)
        call set_empty(query%binding_name)
        call set_empty(query%implementation)
        query%implementation_node_index = 0
        call set_empty(query%interface_name)
        call set_empty(query%pass_name)
        allocate (character(len=1) :: query%generic_names(0))
        allocate (query%dispatch_target_type_indices(0))
        allocate (character(len=1) :: query%dispatch_target_implementations(0))
        allocate (query%dispatch_target_implementation_node_indices(0))
        allocate (character(len=1) :: query%dispatch_target_pass_names(0))
        allocate (query%dispatch_target_pass_positions(0))
        allocate (character(len=1) :: query%dispatch_target_passed_object_types(0))
        allocate (query%dispatch_target_signature_resolved(0))
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

    subroutine append_dispatch_target(arena, query, type_index, target)
        type(ast_arena_t), intent(in) :: arena
        type(binding_resolution_query_t), intent(inout) :: query
        integer, intent(in) :: type_index
        type(binding_resolution_query_t), intent(in) :: target
        integer, allocatable :: int_tmp(:)
        logical, allocatable :: logical_tmp(:)
        character(len=:), allocatable :: char_tmp(:)
        character(len=:), allocatable :: pass_name, passed_object_type
        integer :: n, width, pass_position, procedure_index
        logical :: signature_resolved

        call resolve_dispatch_signature(arena, target%implementation, &
            target%pass_arg, target%pass_name, pass_name, pass_position, &
            passed_object_type, signature_resolved, procedure_index)

        n = size(query%dispatch_target_type_indices)
        allocate (int_tmp(n + 1))
        if (n > 0) int_tmp(:n) = query%dispatch_target_type_indices
        int_tmp(n + 1) = type_index
        call move_alloc(int_tmp, query%dispatch_target_type_indices)
        width = max(1, len_trim(target%implementation))
        allocate (character(len=width) :: char_tmp(n + 1))
        if (n > 0) char_tmp(:n) = query%dispatch_target_implementations
        char_tmp(n + 1) = trim(target%implementation)
        call move_alloc(char_tmp, query%dispatch_target_implementations)

        allocate (int_tmp(n + 1))
        if (n > 0) int_tmp(:n) = &
            query%dispatch_target_implementation_node_indices
        int_tmp(n + 1) = procedure_index
        call move_alloc(int_tmp, &
            query%dispatch_target_implementation_node_indices)

        call append_dispatch_character(query%dispatch_target_pass_names, &
            pass_name)
        call append_dispatch_integer(query%dispatch_target_pass_positions, &
            pass_position)
        call append_dispatch_character( &
            query%dispatch_target_passed_object_types, passed_object_type)
        allocate (logical_tmp(n + 1))
        if (n > 0) logical_tmp(:n) = query%dispatch_target_signature_resolved
        logical_tmp(n + 1) = signature_resolved
        call move_alloc(logical_tmp, query%dispatch_target_signature_resolved)
    end subroutine append_dispatch_target

    subroutine resolve_dispatch_signature(arena, implementation, pass_arg, &
            binding_pass_name, pass_name, pass_position, passed_object_type, &
            signature_resolved, procedure_index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: implementation
        logical, intent(in) :: pass_arg
        character(len=*), intent(in) :: binding_pass_name
        character(len=:), allocatable, intent(out) :: pass_name
        integer, intent(out) :: pass_position
        character(len=:), allocatable, intent(out) :: passed_object_type
        logical, intent(out) :: signature_resolved
        integer, intent(out) :: procedure_index
        integer :: i, pass_index
        type(declaration_query_t) :: formal

        call set_empty(pass_name)
        pass_position = 0
        call set_empty(passed_object_type)
        signature_resolved = .false.
        procedure_index = find_procedure_definition(arena, implementation)
        if (.not. pass_arg) then
            signature_resolved = .true.
            return
        end if

        if (procedure_index <= 0) return
        pass_index = 0
        select type (procedure => arena%entries(procedure_index)%node)
            type is (function_def_node)
            if (.not. allocated(procedure%param_indices)) return
            do i = 1, size(procedure%param_indices)
                formal = query_declaration(arena, procedure%param_indices(i))
                if (.not. formal%found) cycle
                if (len_trim(binding_pass_name) > 0) then
                    if (.not. same_name(formal%name, binding_pass_name)) cycle
                else if (i /= 1) then
                    cycle
                end if
                pass_index = i
                exit
            end do
            type is (subroutine_def_node)
            if (.not. allocated(procedure%param_indices)) return
            do i = 1, size(procedure%param_indices)
                formal = query_declaration(arena, procedure%param_indices(i))
                if (.not. formal%found) cycle
                if (len_trim(binding_pass_name) > 0) then
                    if (.not. same_name(formal%name, binding_pass_name)) cycle
                else if (i /= 1) then
                    cycle
                end if
                pass_index = i
                exit
            end do
        class default
            return
        end select

        if (pass_index <= 0) return
        select type (procedure => arena%entries(procedure_index)%node)
            type is (function_def_node)
            formal = query_declaration(arena, procedure%param_indices(pass_index))
            type is (subroutine_def_node)
            formal = query_declaration(arena, procedure%param_indices(pass_index))
        end select
        if (.not. formal%found) return
        pass_name = formal%name
        pass_position = pass_index
        passed_object_type = formal%type_name
        signature_resolved = len_trim(passed_object_type) > 0
    end subroutine resolve_dispatch_signature

    integer function find_procedure_definition(arena, name) result(index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name
        integer :: i

        index = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (function_def_node)
                if (allocated(node%name) .and. same_name(node%name, name)) then
                    index = i
                    return
                end if
                type is (subroutine_def_node)
                if (allocated(node%name) .and. same_name(node%name, name)) then
                    index = i
                    return
                end if
            class default
            end select
        end do
    end function find_procedure_definition

    subroutine append_dispatch_character(values, value)
        character(len=:), allocatable, intent(inout) :: values(:)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: grown(:)
        integer :: n, width

        n = size(values)
        width = max(1, len_trim(value))
        if (n > 0) width = max(width, len(values))
        allocate (character(len=width) :: grown(n + 1))
        if (n > 0) grown(:n) = values
        grown(n + 1) = trim(value)
        call move_alloc(grown, values)
    end subroutine append_dispatch_character

    subroutine append_dispatch_integer(values, value)
        integer, allocatable, intent(inout) :: values(:)
        integer, intent(in) :: value
        integer, allocatable :: grown(:)
        integer :: n

        n = size(values)
        allocate (grown(n + 1))
        if (n > 0) grown(:n) = values
        grown(n + 1) = value
        call move_alloc(grown, values)
    end subroutine append_dispatch_integer

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

    logical function is_associate_selector_node(arena, node_index) result(is_selector)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: current, i, guard

        is_selector = .false.
        current = node_index
        guard = 0
        do while (current > 0 .and. arena%has_node_at(current))
            select type (node => arena%entries(current)%node)
                type is (associate_node)
                if (.not. allocated(node%associations)) exit
                do i = 1, size(node%associations)
                    if (node%associations(i)%expr_index <= 0) cycle
                    if (node_is_in_scope(arena, node_index, &
                        node%associations(i)%expr_index)) then
                        is_selector = .true.
                        return
                    end if
                end do
            class default
            end select
            current = arena%entries(current)%parent_index
            guard = guard + 1
            if (guard > arena%size) exit
        end do
    end function is_associate_selector_node

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
            type is (assignment_node)
            is_ownership_event = is_potential_reallocation_assignment(arena, index)
        class default
        end select
    end function is_ownership_event

    logical function is_potential_reallocation_assignment(arena, index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(storage_query_t) :: storage
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg

        is_potential_reallocation_assignment = .false.
        select type (node => arena%entries(index)%node)
            type is (assignment_node)
            if (node%is_keyword_argument) return
            storage = query_storage(arena, node%target_index)
            if (.not. storage%found) then
                call resolve_identifier_binding(arena, node%target_index, binding, &
                    error_msg)
                if (binding%found) then
                    storage = query_storage(arena, binding%declaration_node_index)
                end if
            end if
            is_potential_reallocation_assignment = storage%found .and. &
                storage%is_allocatable
        class default
        end select
    end function is_potential_reallocation_assignment

    function ownership_event(arena, index) result(event)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(ownership_event_query_t) :: event

        allocate (event%object_indices(0))
        allocate (event%shape_expr_indices(0))
        call initialize_component_path_query(event%owner_path)
        call initialize_component_path_query(event%source_path)
        call initialize_component_path_query(event%destination_path)
        call initialize_component_path_query(event%lhs_owner_path)
        call initialize_component_path_query(event%rhs_owner_path)
        call initialize_polymorphic_allocation_query(event%polymorphic_allocation)
        event%found = .true.
        event%node_index = index
        select type (node => arena%entries(index)%node)
            type is (allocate_statement_node)
            event%event_kind = OWNERSHIP_EVENT_ALLOCATE
            call copy_integer_array(node%var_indices, event%object_indices)
            call copy_integer_array(node%shape_indices, event%shape_expr_indices)
            event%source_expr_index = node%source_expr_index
            event%mold_expr_index = node%mold_expr_index
            if (allocated(node%var_indices)) then
                if (size(node%var_indices) > 0) event%owner_path = &
                    ownership_path(arena, node%var_indices(1))
            end if
            if (allocated(node%var_indices)) then
                if (size(node%var_indices) > 0) event%rank = &
                    expression_rank(arena, node%var_indices(1))
            end if
            if (node%source_expr_index > 0) then
                event%source_path = ownership_path(arena, &
                    node%source_expr_index)
            end if
            event%polymorphic_allocation = query_polymorphic_allocation(arena, index)
            type is (deallocate_statement_node)
            event%event_kind = OWNERSHIP_EVENT_DEALLOCATE
            call copy_integer_array(node%var_indices, event%object_indices)
            if (allocated(node%var_indices)) then
                if (size(node%var_indices) > 0) event%owner_path = &
                    ownership_path(arena, node%var_indices(1))
            end if
            type is (pointer_assignment_node)
            event%event_kind = OWNERSHIP_EVENT_POINTER_ASSIGN
            event%source_index = node%target_index
            event%target_index = node%pointer_index
            type is (nullify_node)
            event%event_kind = OWNERSHIP_EVENT_NULLIFY
            call copy_integer_array(node%pointer_indices, event%object_indices)
            type is (subroutine_call_node)
            event%event_kind = OWNERSHIP_EVENT_MOVE_ALLOC
            event%is_explicit_ownership_transfer = .true.
            if (allocated(node%arg_indices)) then
                event%object_indices = node%arg_indices
                if (size(node%arg_indices) >= 2) then
                    event%source_index = node%arg_indices(1)
                    event%target_index = node%arg_indices(2)
                    event%source_path = ownership_path(arena, &
                        node%arg_indices(1))
                    event%destination_path = ownership_path(arena, &
                        node%arg_indices(2))
                    event%rhs_owner_path = event%source_path
                    event%lhs_owner_path = event%destination_path
                    event%rhs_rank = expression_rank(arena, node%arg_indices(1))
                    event%lhs_rank = expression_rank(arena, node%arg_indices(2))
                end if
            end if
            type is (assignment_node)
            event%event_kind = OWNERSHIP_EVENT_ASSIGNMENT
            event%assignment_kind = OWNERSHIP_ASSIGNMENT_WHOLE_ALLOCATABLE
            event%reallocation_kind = OWNERSHIP_REALLOCATION_POTENTIAL
            event%is_potential_automatic_reallocation = .true.
            event%source_path = ownership_path(arena, node%value_index)
            event%destination_path = ownership_path(arena, node%target_index)
            event%owner_path = event%destination_path
            event%rhs_owner_path = event%source_path
            event%lhs_owner_path = event%destination_path
            event%rhs_rank = expression_rank(arena, node%value_index)
            event%lhs_rank = expression_rank(arena, node%target_index)
        class default
        end select
    end function ownership_event

    function ownership_path(arena, node_index) result(path)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(component_path_query_t) :: path

        path = query_component_path(arena, node_index)
        if (.not. arena%has_node_at(node_index)) return
        path%node_index = node_index
        if (path%base_node_index == 0) path%base_node_index = node_index
    end function ownership_path

    integer function expression_rank(arena, node_index) result(rank)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(resolved_type_query_t) :: resolved

        rank = -1
        resolved = query_resolved_type(arena, node_index)
        if (resolved%found) rank = resolved%rank
    end function expression_rank

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
        query%declaration_index = node_index
        query%name = declaration%name
        query%type_name = declaration%type_name
        query%rank = declaration_rank(declaration)
        query%is_allocatable = declaration%is_allocatable
        query%is_pointer = declaration%is_pointer
        query%is_target = declaration%is_target
        query%is_contiguous = declaration%is_contiguous
        query%is_polymorphic = is_polymorphic_type_spec(query%type_name)
        query%is_unlimited_polymorphic = &
            is_unlimited_polymorphic_type_spec(query%type_name)
        call set_derived_storage_facts(arena, node_index, query)
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
