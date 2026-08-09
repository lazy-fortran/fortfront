module frontend_compiler_queries
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: string_t
    use ast_nodes_procedure, only: subroutine_call_node, function_def_node, &
        subroutine_def_node
    use ast_nodes_core, only: binary_op_node, literal_node, identifier_node, &
        array_literal_node, program_node, component_access_node, &
        call_or_subscript_node, pointer_assignment_node, assignment_node
    use ast_nodes_control, only: if_node, do_loop_node, do_while_node, &
        forall_node, where_node, where_stmt_node
    use ast_nodes_associate, only: associate_node
    use ast_nodes_bounds, only: array_slice_node, array_bounds_node, &
        range_expression_node
    use ast_nodes_transfer, only: nullify_node, return_node, &
        alt_return_spec_node
    use ast_nodes_data, only: declaration_node, derived_type_node, &
        parameter_declaration_node, module_node, block_data_node, &
        submodule_node, multi_unit_container_node, type_binding_node, &
        PARAM_UNKNOWN, PARAM_KIND, PARAM_LEN, INTENT_IN, INTENT_OUT, &
        INTENT_INOUT
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
        BINDING_ASSOCIATE_NAME, ASSOCIATION_DIRECT
    use frontend_compiler_type_queries, only: resolved_type_query_t, &
        query_resolved_type
    use semantic_procedure_signature, only: type_category
    use generic_spec_names, only: normalize_generic_operator
    use string_utils_mod, only: to_lower
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
    public :: procedure_pointer_state_query_t, query_procedure_pointer_state
    public :: procedure_target_query_t, query_procedure_target
    public :: procedure_callback_target_query_t, procedure_callback_flow_query_t
    public :: query_procedure_callback_flow, query_procedure_pointer_callback_flow
    public :: procedure_call_target_query_t, query_procedure_call_target
    public :: procedure_reassignment_call_query_t
    public :: query_procedure_reassignment_call, &
        query_procedure_reassignment_call_into
    public :: procedure_dummy_query_t, procedure_signature_query_t
    public :: query_procedure_signature
    public :: procedure_actual_argument_query_t
    public :: query_procedure_actual_argument
    public :: call_argument_query_t, call_arguments_query_t
    public :: query_call_arguments
    public :: generic_argument_query_t, generic_candidate_query_t, &
        generic_call_query_t, query_generic_call
    public :: defined_operator_operand_query_t, &
        defined_operator_candidate_query_t, defined_operator_query_t, &
        query_defined_operator, query_defined_operator_into
    public :: STORAGE_LOCAL, STORAGE_OWNED, STORAGE_BORROWED, STORAGE_POINTER
    public :: STORAGE_MODULE, STORAGE_SAVE, STORAGE_COMMON
    public :: OWNERSHIP_EVENT_ALLOCATE, OWNERSHIP_EVENT_DEALLOCATE
    public :: OWNERSHIP_EVENT_POINTER_ASSIGN, OWNERSHIP_EVENT_MOVE_ALLOC
    public :: OWNERSHIP_EVENT_NULLIFY, OWNERSHIP_EVENT_ASSIGNMENT
    public :: OWNERSHIP_ASSIGNMENT_NONE, OWNERSHIP_ASSIGNMENT_WHOLE_ALLOCATABLE
    public :: OWNERSHIP_ASSIGNMENT_DEEP_DERIVED
    public :: OWNERSHIP_REALLOCATION_NONE, OWNERSHIP_REALLOCATION_POTENTIAL
    public :: OWNERSHIP_STATE_UNKNOWN, OWNERSHIP_STATE_UNALLOCATED
    public :: OWNERSHIP_STATE_ALLOCATED, OWNERSHIP_STATE_SAME_AS_SOURCE
    public :: ACCESS_READ, ACCESS_WRITE, ACCESS_READ_WRITE
    public :: POLYMORPHIC_SOURCE_UNKNOWN, POLYMORPHIC_SOURCE_CONCRETE
    public :: POLYMORPHIC_SOURCE_POLYMORPHIC
    public :: storage_query_t, ownership_event_query_t, component_path_query_t
    public :: polymorphic_allocation_query_t, polymorphic_assignment_query_t
    public :: associate_selector_query_t
    public :: binding_resolution_query_t, global_reference_query_t
    public :: query_storage, query_ownership_events, query_component_path
    public :: query_polymorphic_allocation, query_polymorphic_assignment, &
        query_polymorphic_assignment_into
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
    integer, parameter :: OWNERSHIP_ASSIGNMENT_DEEP_DERIVED = 2
    integer, parameter :: OWNERSHIP_REALLOCATION_NONE = 0
    integer, parameter :: OWNERSHIP_REALLOCATION_POTENTIAL = 1

    integer, parameter :: OWNERSHIP_STATE_UNKNOWN = 0
    integer, parameter :: OWNERSHIP_STATE_UNALLOCATED = 1
    integer, parameter :: OWNERSHIP_STATE_ALLOCATED = 2
    integer, parameter :: OWNERSHIP_STATE_SAME_AS_SOURCE = 3

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

    type :: procedure_dummy_query_t
        !! Bounded facts for one ordered dummy of a resolved procedure.
        !! A *_known flag is required before a consumer uses the value; the
        !! frontend does not fill in defaults or infer missing declarations.
        integer :: node_index = 0
        character(len=:), allocatable :: name
        character(len=:), allocatable :: type_category
        character(len=:), allocatable :: intent
        integer :: type_kind = 0
        integer :: kind_value = 0
        integer :: rank = -1
        logical :: type_known = .false.
        logical :: category_known = .false.
        logical :: kind_known = .false.
        logical :: rank_known = .false.
        logical :: has_intent = .false.
        logical :: is_optional = .false.
        logical :: is_value = .false.
    end type procedure_dummy_query_t

    type :: procedure_signature_query_t
        !! Signature facts for one directly resolved internal procedure.
        !! External, generic, ambiguous, and unresolved targets leave FOUND
        !! false rather than receiving a guessed interface.
        logical :: found = .false.
        logical :: is_function = .false.
        integer :: procedure_node_index = 0
        character(len=:), allocatable :: procedure_name
        character(len=:), allocatable :: result_category
        integer :: result_type_kind = 0
        integer :: result_kind_value = 0
        integer :: result_rank = -1
        logical :: result_type_known = .false.
        logical :: result_category_known = .false.
        logical :: result_kind_known = .false.
        logical :: result_rank_known = .false.
        integer :: dummy_count = 0
        type(procedure_dummy_query_t), allocatable :: dummies(:)
    end type procedure_signature_query_t

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
        type(procedure_signature_query_t) :: signature
    end type procedure_target_query_t

    type :: procedure_call_target_query_t
        !! One bounded, resolved call through a procedure pointer.
        !!
        !! FOUND means that CALL_NODE_INDEX names a procedure pointer and its
        !! lexical scope contains exactly one unconditional direct pointer
        !! assignment before the call.  That assignment must resolve to an
        !! internal or external procedure.  A pointer call with no such
        !! proof leaves FOUND false and sets IS_UNRESOLVED; this includes
        !! branches, NULL(), generic calls, and other flow-sensitive cases.
        !! HAS_REASSIGNMENT is a narrower refusal fact for two or more direct
        !! same-scope pointer assignments; NULLIFY does not set it.
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
        logical :: has_reassignment = .false.
        type(procedure_signature_query_t) :: signature
    end type procedure_call_target_query_t

    type :: procedure_reassignment_call_query_t
        !! A deliberately narrow two-target procedure-pointer proof.
        !!
        !! FOUND means that CALL_NODE_INDEX is the only direct call through a
        !! local procedure pointer in its scope, preceded by exactly two
        !! direct same-scope assignments.  Both assignments resolve to fixed
        !! same-arena scalar REAL(8) functions with the same one-argument
        !! interface; the second target is the active call target.  All other
        !! flow remains a refusal, including a third assignment, branches,
        !! loops, NULL/NULLIFY, aliases, globals, and unresolved targets.
        logical :: found = .false.
        logical :: is_unresolved = .true.
        logical :: is_refused = .false.
        logical :: has_reassignment = .false.
        logical :: has_branch = .false.
        logical :: has_loop = .false.
        logical :: has_null_assignment = .false.
        logical :: has_nullify = .false.
        logical :: has_alias = .false.
        logical :: has_global_mutable_state = .false.
        logical :: has_unresolved_target = .false.
        logical :: has_multiple_calls = .false.
        integer :: call_node_index = 0
        integer :: pointer_node_index = 0
        integer :: pointer_declaration_index = 0
        integer :: scope_node_index = 0
        integer :: assignment_count = 0
        character(len=:), allocatable :: pointer_name
        type(procedure_target_query_t) :: first_target
        type(procedure_target_query_t) :: second_target
    end type procedure_reassignment_call_query_t

    type :: procedure_callback_target_query_t
        !! One ordered target in a branch-merged callback proof.
        integer :: branch_assignment_node_index = 0
        integer :: target_procedure_index = 0
        integer :: target_declaration_index = 0
        character(len=:), allocatable :: procedure_name
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_generic = .false.
        logical :: is_ambiguous = .false.
        logical :: is_signature_compatible = .false.
        type(procedure_signature_query_t) :: signature
    end type procedure_callback_target_query_t

    type :: procedure_callback_flow_query_t
        !! A deliberately narrow IF/ELSE callback target-set proof.
        logical :: found = .false.
        logical :: is_unresolved = .false.
        logical :: is_refused = .false.
        logical :: has_loop = .false.
        logical :: has_nested_branch = .false.
        logical :: has_missing_branch = .false.
        logical :: has_reassignment = .false.
        logical :: has_null_assignment = .false.
        logical :: has_nullify = .false.
        logical :: has_missing_assignment = .false.
        logical :: has_generic_target = .false.
        logical :: has_ambiguous_target = .false.
        logical :: has_incompatible_signature = .false.
        logical :: has_branch_call = .false.
        integer :: pointer_node_index = 0
        integer :: pointer_declaration_index = 0
        integer :: call_node_index = 0
        integer :: call_pointer_node_index = 0
        integer :: if_node_index = 0
        integer :: then_entry_node_index = 0
        integer :: then_exit_node_index = 0
        integer :: else_entry_node_index = 0
        integer :: else_exit_node_index = 0
        integer :: merge_boundary_node_index = 0
        integer :: scope_node_index = 0
        character(len=:), allocatable :: pointer_name
        type(procedure_callback_target_query_t), allocatable :: targets(:)
    end type procedure_callback_flow_query_t
    type :: nullify_query_t
        logical :: found = .false.
        integer, allocatable :: pointer_node_indices(:)
    end type nullify_query_t

    type :: procedure_pointer_state_query_t
        !! One bounded ASSOCIATED/NULLIFY fact for a procedure pointer.
        !!
        !! FOUND identifies the source operation.  STATE_KNOWN is set only
        !! for a direct NULLIFY of one procedure pointer, or for a unary
        !! ASSOCIATED test whose state is fixed by one direct assignment and
        !! at most one direct NULLIFY before the test.  Branch-local,
        !! reassigned, aliased, indirect, ambiguous, and otherwise
        !! flow-sensitive callbacks remain explicit refusal facts.
        logical :: found = .false.
        logical :: is_associated_test = .false.
        logical :: is_nullify = .false.
        logical :: state_known = .false.
        logical :: is_associated = .false.
        logical :: is_refused = .false.
        logical :: is_unresolved = .false.
        logical :: has_invalid_arity = .false.
        logical :: has_second_argument = .false.
        logical :: has_multiple_pointers = .false.
        logical :: has_non_identifier_pointer = .false.
        logical :: has_alias = .false.
        logical :: has_non_procedure_pointer = .false.
        logical :: has_reassignment = .false.
        logical :: has_null_assignment = .false.
        logical :: has_nullify = .false.
        logical :: has_flow_sensitive_state = .false.
        logical :: has_control_flow_boundary = .false.
        logical :: has_global_mutable_state = .false.
        logical :: has_unresolved_target = .false.
        integer :: observation_node_index = 0
        integer :: pointer_node_index = 0
        integer :: pointer_declaration_index = 0
        integer :: assignment_node_index = 0
        integer :: nullify_node_index = 0
        integer :: scope_node_index = 0
        character(len=:), allocatable :: pointer_name
    end type procedure_pointer_state_query_t

    ! Resolved actual-to-formal call facts.  The result is ordered by the
    ! callee's formal parameter list, so an omitted optional dummy is present
    ! as a record with is_supplied=.false. rather than being erased.
    type :: call_argument_query_t
        integer :: actual_node_index = 0
        integer :: actual_value_node_index = 0
        integer :: formal_node_index = 0
        character(len=:), allocatable :: formal_name
        character(len=:), allocatable :: formal_intent
        character(len=:), allocatable :: formal_type_category
        integer :: formal_type_kind = 0
        integer :: formal_kind_value = 0
        integer :: formal_rank = -1
        logical :: formal_intent_known = .false.
        logical :: formal_type_known = .false.
        logical :: formal_kind_known = .false.
        logical :: formal_rank_known = .false.
        logical :: formal_is_value = .false.
        logical :: formal_is_pointer = .false.
        logical :: formal_is_allocatable = .false.
        logical :: formal_is_target = .false.
        integer :: actual_type_kind = 0
        integer :: actual_kind_value = 0
        integer :: actual_rank = -1
        character(len=:), allocatable :: actual_derived_type_name
        logical :: actual_type_known = .false.
        logical :: actual_kind_known = .false.
        logical :: actual_rank_known = .false.
        logical :: actual_is_pointer = .false.
        logical :: actual_is_allocatable = .false.
        logical :: actual_is_target = .false.
        logical :: type_compatibility_known = .false.
        logical :: has_type_mismatch = .false.
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
        logical :: has_global_mutable_state = .false.
        logical :: has_unresolved_alias = .false.
        logical :: has_procedure_callback = .false.
        logical :: has_unknown_argument_types = .false.
        logical :: has_type_mismatch = .false.
        logical :: is_refused = .false.
        type(call_argument_query_t), allocatable :: arguments(:)
    end type call_arguments_query_t

    type :: procedure_actual_argument_query_t
        !! One bounded procedure actual-to-formal mapping.
        !!
        !! FOUND means that CALL_NODE_INDEX resolved to a same-arena call and
        !! FORMAL_NAME identifies a procedure dummy in that call.  A direct
        !! contained function or subroutine actual, or a procedure pointer
        !! with exactly one unconditional direct assignment before the call,
        !! is the only target for which IS_RESOLVED is set and SIGNATURE is
        !! populated.  Procedure dummies, external/contextual names, generic
        !! names, and non-identifiers remain refusal-only facts.  Pointer
        !! targets retain explicit branch, NULL, reassignment, and unresolved
        !! target flags; no flow-sensitive state is guessed.
        logical :: found = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_refused = .false.
        logical :: has_reassignment = .false.
        logical :: has_contextual_target = .false.
        logical :: has_ambiguous_target = .false.
        logical :: has_branch_target = .false.
        logical :: has_null_target = .false.
        logical :: has_unresolved_target = .false.
        integer :: call_node_index = 0
        integer :: formal_node_index = 0
        integer :: actual_node_index = 0
        integer :: actual_value_node_index = 0
        integer :: target_assignment_node_index = 0
        integer :: target_node_index = 0
        integer :: target_procedure_index = 0
        integer :: target_declaration_index = 0
        integer :: target_binding_node_index = 0
        character(len=:), allocatable :: formal_name
        character(len=:), allocatable :: actual_name
        character(len=:), allocatable :: procedure_name
        character(len=:), allocatable :: procedure_kind
        type(procedure_signature_query_t) :: signature
    end type procedure_actual_argument_query_t

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

    ! Exact facts for one actual/formal operand pair of a defined operator.
    ! The actual and formal type metadata are intentionally parallel: a
    ! consumer can distinguish an exact match from a known mismatch without
    ! reimplementing generic resolution or guessing an implicit conversion.
    type :: defined_operator_operand_query_t
        logical :: found = .false.
        integer :: actual_node_index = 0
        integer :: formal_node_index = 0
        logical :: actual_type_known = .false.
        logical :: formal_type_known = .false.
        integer :: actual_type_kind = 0
        integer :: actual_kind_value = 0
        integer :: actual_rank = -1
        integer :: formal_type_kind = 0
        integer :: formal_kind_value = 0
        integer :: formal_rank = -1
        character(len=:), allocatable :: actual_derived_type_name
        character(len=:), allocatable :: formal_derived_type_name
        logical :: actual_is_pointer = .false.
        logical :: actual_is_target = .false.
        logical :: actual_is_allocatable = .false.
        logical :: actual_is_polymorphic = .false.
        logical :: actual_has_global_mutable_state = .false.
        logical :: formal_is_pointer = .false.
        logical :: formal_is_target = .false.
        logical :: formal_is_allocatable = .false.
        logical :: formal_is_polymorphic = .false.
        logical :: is_exact = .false.
        logical :: has_conversion = .false.
        logical :: has_unknown_type = .false.
    end type defined_operator_operand_query_t

    ! One concrete function candidate from an operator generic interface.
    ! `is_match` is true only when arity, all operand type/kind/rank facts,
    ! storage boundaries, and the candidate procedure body are exact and
    ! statically safe for a backend consumer.
    type :: defined_operator_candidate_query_t
        logical :: found = .false.
        integer :: interface_node_index = 0
        integer :: procedure_node_index = 0
        character(len=:), allocatable :: procedure_name
        character(len=:), allocatable :: procedure_kind
        logical :: is_match = .false.
        logical :: has_conversion = .false.
        logical :: has_unknown_types = .false.
        logical :: has_pointer_operand = .false.
        logical :: has_global_mutable_state = .false.
        logical :: has_invalid_arity = .false.
        type(defined_operator_operand_query_t), allocatable :: operands(:)
    end type defined_operator_candidate_query_t

    ! Same-arena exact resolution of a user-defined unary or binary operator.
    ! `found` means that the operator expression and at least one visible
    ! operator interface were identified. `is_resolved` is stronger: exactly
    ! one candidate has an exact operand signature and no explicit refusal
    ! boundary. No implicit conversion, dynamic polymorphism, pointer/TARGET
    ! alias, or mutable global state is guessed by this query.
    type :: defined_operator_query_t
        logical :: found = .false.
        logical :: is_defined_operator = .false.
        logical :: is_unary = .false.
        logical :: is_binary = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_refused = .false.
        logical :: is_ambiguous = .false.
        logical :: has_exact_match = .false.
        logical :: has_conversion = .false.
        logical :: has_unknown_types = .false.
        logical :: has_pointer_operand = .false.
        logical :: has_global_mutable_state = .false.
        logical :: has_invalid_arity = .false.
        integer :: operator_node_index = 0
        integer :: interface_node_index = 0
        integer :: selected_procedure_node_index = 0
        character(len=:), allocatable :: operator
        character(len=:), allocatable :: refusal_reason
        integer, allocatable :: interface_node_indices(:)
        type(defined_operator_candidate_query_t), allocatable :: candidates(:)
    end type defined_operator_query_t

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
        logical :: is_abstract_type = .false.
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
        logical :: is_abstract_type = .false.
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_polymorphic = .false.
        logical :: is_unlimited_polymorphic = .false.
        character(len=:), allocatable :: component_names(:)
        integer, allocatable :: component_node_indices(:)
        integer, allocatable :: component_declaration_indices(:)
    end type component_path_query_t

    ! Bounded facts for one intrinsic assignment whose destination is a
    ! polymorphic allocatable derived object. FOUND identifies the exact
    ! assignment shape and storage operands. IS_REPLAYABLE is stronger: a
    ! concrete source type is statically compatible with the declared
    ! polymorphic destination, and no mutable global state, alias, or control
    ! flow boundary requires a runtime guess. The query intentionally does
    ! not claim a dynamic type for a polymorphic source.
    type :: polymorphic_assignment_query_t
        logical :: found = .false.
        logical :: is_replayable = .false.
        logical :: is_refused = .false.
        logical :: is_source_concrete = .false.
        logical :: is_source_polymorphic = .false.
        logical :: is_destination_polymorphic = .false.
        logical :: is_dynamic_type_known = .false.
        logical :: has_owned_components = .false.
        logical :: has_global_mutable_state = .false.
        logical :: has_unresolved_alias = .false.
        logical :: has_control_flow_boundary = .false.
        logical :: has_type_mismatch = .false.
        integer :: assignment_node_index = 0
        integer :: source_node_index = 0
        integer :: destination_node_index = 0
        integer :: source_declaration_index = 0
        integer :: destination_declaration_index = 0
        character(len=:), allocatable :: source_declared_type
        character(len=:), allocatable :: destination_declared_type
        character(len=:), allocatable :: dynamic_type
        type(component_path_query_t) :: source_path
        type(component_path_query_t) :: destination_path
    end type polymorphic_assignment_query_t

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
        ! One-based source order among ownership events in the queried scope.
        integer :: sequence_index = 0
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
        type(polymorphic_assignment_query_t) :: polymorphic_assignment
        integer :: lhs_rank = -1
        integer :: rhs_rank = -1
        integer :: assignment_kind = OWNERSHIP_ASSIGNMENT_NONE
        integer :: reallocation_kind = OWNERSHIP_REALLOCATION_NONE
        ! Deterministic allocation-state effects. UNKNOWN means that the
        ! source alone does not prove a runtime state.
        integer :: owner_state_before = OWNERSHIP_STATE_UNKNOWN
        integer :: owner_state_after = OWNERSHIP_STATE_UNKNOWN
        integer :: source_state_after = OWNERSHIP_STATE_UNKNOWN
        integer :: destination_state_after = OWNERSHIP_STATE_UNKNOWN
        ! Direct whole-allocatable storage identity.  Dynamic type facts are
        ! flow-sensitive only within a bounded source-order query; a false
        ! known flag means that the frontend refuses to guess the runtime
        ! type.
        integer :: source_declaration_index = 0
        integer :: destination_declaration_index = 0
        integer :: source_storage_class = STORAGE_LOCAL
        integer :: destination_storage_class = STORAGE_LOCAL
        logical :: source_storage_resolved = .false.
        logical :: destination_storage_resolved = .false.
        logical :: source_is_polymorphic = .false.
        logical :: destination_is_polymorphic = .false.
        logical :: is_source_dynamic_type_known = .false.
        logical :: is_destination_dynamic_type_known = .false.
        character(len=:), allocatable :: source_dynamic_type
        character(len=:), allocatable :: destination_dynamic_type
        logical :: is_deep_assignment = .false.
        logical :: has_owned_components = .false.
        logical :: has_global_mutable_state = .false.
        logical :: has_unresolved_alias = .false.
        logical :: is_refused = .false.
        logical :: is_potential_automatic_reallocation = .false.
        logical :: has_implicit_destination_deallocation = .false.
        logical :: has_potential_implicit_reallocation = .false.
        logical :: is_explicit_ownership_transfer = .false.
        logical :: has_dynamic_type_boundary = .false.
    end type ownership_event_query_t

    type :: ownership_dynamic_flow_t
        integer :: declaration_index = 0
        logical :: is_known = .false.
        character(len=:), allocatable :: dynamic_type
    end type ownership_dynamic_flow_t

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
        ! These arrays identify where the effective binding was declared.
        ! They are useful when a concrete leaf inherits an implementation
        ! through an abstract intermediate type.
        integer, allocatable :: dispatch_target_declaring_type_indices(:)
        logical, allocatable :: dispatch_target_is_inherited(:)
        ! Parallel to the target provenance arrays.  This is the number of
        ! EXTENDS links from the concrete target to the effective declaring
        ! type; zero denotes a local implementation.
        integer, allocatable :: dispatch_target_inheritance_depth(:)
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
        ! instead of making consumers recover it from a flattened name.  An
        ! explicit CALL may have no receiver AST node; in that case the path
        ! can still carry semantically resolved component names and
        ! declaration identities, while unavailable AST/shape facts remain
        ! unset.
        type(component_path_query_t) :: receiver_path
        integer, allocatable :: dispatch_target_type_indices(:)
        character(len=:), allocatable :: dispatch_target_implementations(:)
        integer, allocatable :: dispatch_target_implementation_node_indices(:)
        character(len=:), allocatable :: dispatch_target_pass_names(:)
        integer, allocatable :: dispatch_target_pass_positions(:)
        character(len=:), allocatable :: dispatch_target_passed_object_types(:)
        ! Parallel to the existing target type and implementation arrays.
        logical, allocatable :: dispatch_target_signature_resolved(:)
        ! Parallel provenance facts for the effective binding declaration.
        integer, allocatable :: dispatch_target_declaring_type_indices(:)
        logical, allocatable :: dispatch_target_is_inherited(:)
        integer, allocatable :: dispatch_target_inheritance_depth(:)
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
        if (query%is_resolved) then
            call fill_procedure_signature(arena, query%target_procedure_index, &
                query%signature)
        end if
    end function query_procedure_target

    function query_procedure_signature(arena, procedure_index) result(signature)
        !! Return the ordered signature facts for one same-arena procedure.
        !!
        !! The result remains FOUND false for a missing or non-procedure node.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: procedure_index
        type(procedure_signature_query_t) :: signature

        call fill_procedure_signature(arena, procedure_index, signature)
    end function query_procedure_signature

    subroutine fill_procedure_signature(arena, procedure_index, signature)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: procedure_index
        type(procedure_signature_query_t), intent(out) :: signature
        type(program_unit_query_t) :: procedure
        type(declaration_query_t) :: formal, body_declaration, result_decl
        type(resolved_type_query_t) :: resolved
        integer :: i, body_index, result_index
        character(len=:), allocatable :: result_name
        logical :: has_body_declaration

        call initialize_procedure_signature_query(signature)
        if (.not. arena%has_node_at(procedure_index)) return

        procedure = query_program_unit(arena, procedure_index)
        if (.not. procedure%found) return
        if (trim(procedure%unit_kind) /= 'function' .and. &
            trim(procedure%unit_kind) /= 'subroutine') return

        signature%found = .true.
        signature%is_function = trim(procedure%unit_kind) == 'function'
        signature%procedure_node_index = procedure_index
        signature%procedure_name = procedure%name
        if (allocated(procedure%parameter_indices)) then
            signature%dummy_count = size(procedure%parameter_indices)
        end if
        deallocate (signature%dummies)
        allocate (signature%dummies(signature%dummy_count))

        do i = 1, signature%dummy_count
            formal = query_declaration(arena, procedure%parameter_indices(i))
            body_index = find_named_declaration(arena, procedure%body_indices, &
                formal%name)
            has_body_declaration = body_index > 0
            if (has_body_declaration) then
                body_declaration = query_declaration(arena, body_index)
            else
                call initialize_declaration_query(body_declaration)
            end if
            call fill_procedure_dummy(arena, formal, body_declaration, &
                has_body_declaration, signature%dummies(i))
        end do

        if (.not. signature%is_function) return
        result_name = procedure%result_name
        if (len_trim(result_name) == 0) result_name = procedure%name
        result_index = find_named_declaration(arena, procedure%body_indices, &
            result_name)
        if (result_index > 0) then
            result_decl = query_declaration(arena, result_index)
        else
            call initialize_declaration_query(result_decl)
        end if
        resolved = query_resolved_type(arena, result_index)
        if (.not. resolved%found) then
            resolved = query_resolved_type(arena, procedure_index)
        end if
        call fill_procedure_result(result_decl, procedure%return_type, resolved, &
            signature)
    end subroutine fill_procedure_signature

    subroutine fill_procedure_dummy(arena, formal, body_declaration, has_body, &
            dummy)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_query_t), intent(in) :: formal, body_declaration
        logical, intent(in) :: has_body
        type(procedure_dummy_query_t), intent(out) :: dummy
        type(declaration_query_t) :: selected
        type(resolved_type_query_t) :: resolved, body_resolved

        call initialize_procedure_dummy_query(dummy)
        dummy%node_index = formal%node_index
        dummy%name = formal%name
        selected = formal
        if (len_trim(selected%type_name) == 0) then
            if (has_body) selected%type_name = body_declaration%type_name
        end if
        if (.not. selected%has_kind) then
            if (has_body) then
                if (body_declaration%has_kind) then
                    selected%has_kind = .true.
                    selected%kind_value = body_declaration%kind_value
                end if
            end if
        end if
        if (.not. selected%is_array) then
            if (has_body) selected%is_array = body_declaration%is_array
        end if
        if (has_body) then
            if (body_declaration%is_optional) selected%is_optional = .true.
            if (body_declaration%is_value) selected%is_value = .true.
        end if

        if (len_trim(selected%type_name) > 0) then
            dummy%type_category = type_category(selected%type_name)
            dummy%category_known = len_trim(dummy%type_category) > 0
        end if
        resolved = query_resolved_type(arena, formal%node_index)
        if (has_body) then
            body_resolved = query_resolved_type(arena, body_declaration%node_index)
            if (body_resolved%found) resolved = body_resolved
        end if
        if (resolved%found) then
            dummy%type_known = .true.
            dummy%type_kind = resolved%type_kind
            dummy%kind_value = resolved%kind_value
            dummy%kind_known = resolved%kind_value > 0
            dummy%rank = resolved%rank
            dummy%rank_known = resolved%rank >= 0
        else
            if (selected%has_kind) then
                dummy%kind_value = selected%kind_value
                dummy%kind_known = selected%kind_value > 0
            end if
            call fill_syntactic_rank(selected, dummy%rank, dummy%rank_known)
        end if

        if (has_body) then
            if (body_declaration%has_intent) then
                dummy%intent = body_declaration%intent
                dummy%has_intent = len_trim(dummy%intent) > 0
            end if
        end if
        if (.not. dummy%has_intent) then
            call intent_text_from_query(formal, dummy%intent, dummy%has_intent)
        end if
        dummy%is_optional = selected%is_optional
        dummy%is_value = selected%is_value
    end subroutine fill_procedure_dummy

    subroutine fill_procedure_result(declaration, header_type, resolved, signature)
        type(declaration_query_t), intent(in) :: declaration
        character(len=*), intent(in) :: header_type
        type(resolved_type_query_t), intent(in) :: resolved
        type(procedure_signature_query_t), intent(inout) :: signature
        character(len=:), allocatable :: type_name

        type_name = declaration%type_name
        if (len_trim(type_name) == 0) type_name = header_type
        if (len_trim(type_name) > 0) then
            signature%result_category = type_category(type_name)
            signature%result_category_known = &
                len_trim(signature%result_category) > 0
        end if
        if (resolved%found) then
            signature%result_type_known = .true.
            signature%result_type_kind = resolved%type_kind
            signature%result_kind_value = resolved%kind_value
            signature%result_kind_known = resolved%kind_value > 0
            signature%result_rank = resolved%rank
            signature%result_rank_known = resolved%rank >= 0
        else
            if (declaration%has_kind) then
                signature%result_kind_value = declaration%kind_value
                signature%result_kind_known = declaration%kind_value > 0
            end if
            call fill_syntactic_rank(declaration, signature%result_rank, &
                signature%result_rank_known)
        end if
    end subroutine fill_procedure_result

    subroutine fill_syntactic_rank(declaration, rank, known)
        type(declaration_query_t), intent(in) :: declaration
        integer, intent(out) :: rank
        logical, intent(out) :: known

        rank = -1
        known = .false.
        if (.not. declaration%is_array) then
            rank = 0
            known = .true.
        else if (allocated(declaration%dimension_indices)) then
            rank = size(declaration%dimension_indices)
            known = rank > 0
        end if
    end subroutine fill_syntactic_rank

    subroutine intent_text_from_query(declaration, intent, known)
        type(declaration_query_t), intent(in) :: declaration
        character(len=:), allocatable, intent(out) :: intent
        logical, intent(out) :: known

        intent = ''
        known = .false.
        if (declaration%has_intent) then
            intent = to_lower(trim(declaration%intent))
            known = len_trim(intent) > 0
            return
        end if
        select case (declaration%intent_type)
        case (INTENT_IN)
            intent = 'in'
        case (INTENT_OUT)
            intent = 'out'
        case (INTENT_INOUT)
            intent = 'inout'
        case default
            return
        end select
        known = .true.
    end subroutine intent_text_from_query

    integer function find_named_declaration(arena, indices, name) result(index)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: indices(:)
        character(len=*), intent(in) :: name
        type(declaration_query_t) :: declaration
        integer :: i, j
        character(len=:), allocatable :: wanted

        index = 0
        wanted = to_lower(trim(name))
        if (len_trim(wanted) == 0) return
        if (.not. allocated(indices)) return
        do i = 1, size(indices)
            declaration = query_declaration(arena, indices(i))
            if (.not. declaration%found) cycle
            if (to_lower(trim(declaration%name)) == wanted) then
                index = indices(i)
                return
            end if
            if (.not. allocated(declaration%names)) cycle
            do j = 1, size(declaration%names)
                if (to_lower(trim(declaration%names(j))) == wanted) then
                    index = indices(i)
                    return
                end if
            end do
        end do
    end function find_named_declaration

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
        logical :: is_call, has_non_direct_mutation, has_reassignment

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
        has_reassignment = .false.
        call find_pointer_mutations(arena, query%scope_node_index, &
            query%pointer_declaration_index, query%pointer_name, &
            scope_indices, assignment_count, assignment_index, &
            has_non_direct_mutation, has_reassignment)
        query%has_reassignment = has_reassignment
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
        query%signature = target%signature
        query%found = .true.
        query%is_resolved = .true.
        query%is_unresolved = .false.
    end function query_procedure_call_target

    function query_procedure_reassignment_call(arena, node_index) result(query)
        !! Return the bounded two-target procedure-pointer proof.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(procedure_reassignment_call_query_t) :: query

        call query_procedure_reassignment_call_into(arena, node_index, query)
    end function query_procedure_reassignment_call

    subroutine query_procedure_reassignment_call_into(arena, node_index, query)
        !! Prove exactly two direct assignments followed by one direct call.
        !!
        !! This is intentionally separate from QUERY_PROCEDURE_CALL_TARGET:
        !! its one-assignment contract and reassignment refusal remain stable
        !! for existing consumers.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(procedure_reassignment_call_query_t), intent(out) :: query
        type(declaration_binding_t) :: pointer_binding
        type(declaration_query_t) :: pointer_declaration
        character(len=:), allocatable :: call_name, error_msg
        integer, allocatable :: scope_indices(:), assignments(:), ignored(:)
        logical :: is_call, non_direct, has_nullify
        integer :: call_statement, call_count
        type(global_reference_query_t), allocatable :: globals(:)

        call initialize_procedure_reassignment_call_query(query)
        if (.not. arena%has_node_at(node_index)) return
        call get_call_parts(arena, node_index, call_name, ignored, is_call)
        if (.not. is_call .or. len_trim(call_name) == 0) return

        query%call_node_index = node_index
        query%pointer_node_index = node_index
        query%scope_node_index = find_enclosing_scope(arena, node_index)
        if (query%scope_node_index <= 0) return
        call resolve_identifier_binding(arena, node_index, pointer_binding, &
            error_msg)
        if (.not. pointer_binding%found) return
        pointer_declaration = query_declaration(arena, &
            pointer_binding%declaration_node_index)
        if (pointer_binding%binding_kind /= BINDING_DECLARATION .or. &
            .not. is_procedure_pointer_declaration(pointer_declaration)) return
        query%pointer_declaration_index = pointer_binding%declaration_node_index
        query%pointer_name = pointer_binding%name

        call get_scope_statement_indices(arena, query%scope_node_index, &
            scope_indices)
        call direct_scope_statement_for_node(arena, node_index, &
            query%scope_node_index, call_statement)
        if (call_statement <= 0) return
        call collect_reassignment_mutations(arena, query%scope_node_index, &
            query%pointer_declaration_index, query%pointer_name, &
            scope_indices, assignments, non_direct, has_nullify)
        query%assignment_count = size(assignments)
        query%has_reassignment = query%assignment_count > 1
        query%has_nullify = has_nullify
        call scan_reassignment_flow(arena, query%scope_node_index, &
            query%pointer_name, query%has_branch, query%has_loop, call_count)
        query%has_multiple_calls = call_count > 1

        if (size(assignments) == 2) then
            query%first_target = query_procedure_target(arena, assignments(1))
            query%second_target = query_procedure_target(arena, assignments(2))
            if (query%first_target%is_null .or. query%second_target%is_null) then
                query%has_null_assignment = .true.
            end if
            if (.not. query%first_target%is_resolved .or. &
                .not. query%second_target%is_resolved) then
                query%has_unresolved_target = .true.
                query%has_alias = .not. query%has_null_assignment
            end if
            if (query%first_target%target_procedure_index > 0) then
                globals = query_active_global_references(arena, &
                    query%first_target%target_procedure_index)
                if (size(globals) > 0) query%has_global_mutable_state = .true.
            end if
            if (query%second_target%target_procedure_index > 0) then
                globals = query_active_global_references(arena, &
                    query%second_target%target_procedure_index)
                if (size(globals) > 0) query%has_global_mutable_state = .true.
            end if
        else if (size(assignments) > 0) then
            query%has_unresolved_target = .true.
        end if

        if (size(assignments) == 2) then
            if (.not. non_direct .and. .not. has_nullify .and. &
                .not. query%has_branch .and. .not. query%has_loop .and. &
                .not. query%has_multiple_calls .and. &
                .not. query%has_global_mutable_state) then
                if (index_precedes(scope_indices, assignments(1), call_statement) &
                    .and. index_precedes(scope_indices, assignments(2), &
                        call_statement)) then
                    if (reassignment_target_is_supported(query%first_target) &
                        .and. reassignment_target_is_supported( &
                            query%second_target)) then
                        query%found = .true.
                        query%is_unresolved = .false.
                        query%is_refused = .false.
                    end if
                end if
            end if
        end if
        if (.not. query%found) query%is_refused = query%assignment_count > 0
    end subroutine query_procedure_reassignment_call_into

    function query_procedure_callback_flow(arena, node_index) result(query)
        !! Prove one IF/ELSE procedure-pointer callback target set.
        !!
        !! NODE_INDEX must be the direct call after the IF.  The proof has
        !! exactly two arms, one direct assignment in each arm, two resolved
        !! internal targets with matching signatures, and no other mutation.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(procedure_callback_flow_query_t) :: query
        type(declaration_binding_t) :: binding
        type(declaration_query_t) :: declaration
        type(procedure_target_query_t) :: then_target, else_target
        type(if_node) :: branch
        character(len=:), allocatable :: call_name, error_msg
        integer, allocatable :: scope_indices(:)
        integer :: call_statement, if_statement, i
        logical :: is_call

        call initialize_procedure_callback_flow_query(query)
        if_statement = 0
        if (.not. arena%has_node_at(node_index)) return
        call get_call_parts(arena, node_index, call_name, scope_indices, is_call)
        if (.not. is_call .or. len_trim(call_name) == 0) return
        call resolve_identifier_binding(arena, node_index, binding, error_msg)
        if (.not. binding%found) return
        declaration = query_declaration(arena, binding%declaration_node_index)
        if (binding%binding_kind /= BINDING_DECLARATION .or. &
            .not. is_procedure_pointer_declaration(declaration)) return

        query%call_node_index = node_index
        query%call_pointer_node_index = node_index
        query%pointer_node_index = node_index
        query%pointer_declaration_index = binding%declaration_node_index
        query%pointer_name = binding%name
        query%scope_node_index = find_enclosing_scope(arena, node_index)
        query%is_unresolved = .true.
        if (query%scope_node_index <= 0) return
        call get_scope_statement_indices(arena, query%scope_node_index, scope_indices)
        call direct_scope_statement_for_node(arena, node_index, &
            query%scope_node_index, call_statement)
        if (call_statement <= 0) return
        if (call_statement /= node_index) then
            query%has_branch_call = .true.
            query%if_node_index = call_statement
            query%is_refused = .true.
            return
        end if
        call scan_scope_loops(arena, query%scope_node_index, query)

        do i = 1, size(scope_indices)
            if (scope_indices(i) == call_statement) exit
            if (.not. arena%has_node_at(scope_indices(i))) cycle
            select type (candidate => arena%entries(scope_indices(i))%node)
                type is (if_node)
                if (if_statement /= 0) cycle
                if_statement = scope_indices(i)
                branch = candidate
            class default
            end select
        end do
        if (if_statement <= 0) return
        if (.not. index_precedes(scope_indices, if_statement, call_statement)) return
        query%if_node_index = if_statement
        query%merge_boundary_node_index = call_statement

        if (.not. allocated(branch%then_body_indices) .or. &
            .not. allocated(branch%else_body_indices) .or. &
            size(branch%then_body_indices) == 0 .or. &
            size(branch%else_body_indices) == 0 .or. &
            allocated(branch%elseif_blocks)) then
            query%has_missing_branch = .true.
            query%is_refused = .true.
            return
        end if

        query%then_entry_node_index = branch%then_body_indices(1)
        query%then_exit_node_index = branch%then_body_indices(size(branch%then_body_indices))
        query%else_entry_node_index = branch%else_body_indices(1)
        query%else_exit_node_index = branch%else_body_indices(size(branch%else_body_indices))

        call inspect_callback_arm(arena, branch%then_body_indices, &
            query%pointer_declaration_index, query%pointer_name, then_target, query)
        call inspect_callback_arm(arena, branch%else_body_indices, &
            query%pointer_declaration_index, query%pointer_name, else_target, query)
        if (query%has_loop .or. query%has_nested_branch .or. &
            query%has_reassignment .or. query%has_null_assignment .or. &
            query%has_nullify .or. query%has_missing_assignment .or. &
            query%has_branch_call) then
            query%is_refused = .true.
            return
        end if

        if (.not. then_target%is_resolved .or. .not. else_target%is_resolved) then
            query%is_refused = .true.
            return
        end if
        if (.not. signatures_compatible(then_target%signature, &
            else_target%signature)) then
            query%has_incompatible_signature = .true.
            query%is_refused = .true.
            return
        end if

        call append_callback_target(query%targets, then_target)
        call append_callback_target(query%targets, else_target)
        query%found = .true.
        query%is_unresolved = .false.
        query%is_refused = .false.
    end function query_procedure_callback_flow

    function query_procedure_pointer_callback_flow(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(procedure_callback_flow_query_t) :: query

        query = query_procedure_callback_flow(arena, node_index)
    end function query_procedure_pointer_callback_flow

    subroutine scan_scope_loops(arena, scope_index, query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        type(procedure_callback_flow_query_t), intent(inout) :: query
        integer :: i

        do i = 1, arena%size
            if (.not. node_is_descendant_of(arena, i, [scope_index])) cycle
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (do_loop_node)
                query%has_loop = .true.
                type is (do_while_node)
                query%has_loop = .true.
            class default
            end select
        end do
    end subroutine scan_scope_loops

    subroutine inspect_callback_arm(arena, body_indices, declaration_index, &
            pointer_name, target, query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:), declaration_index
        character(len=*), intent(in) :: pointer_name
        type(procedure_target_query_t), intent(out) :: target
        type(procedure_callback_flow_query_t), intent(inout) :: query
        type(pointer_assignment_query_t) :: assignment
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg, name
        integer :: i, count

        call initialize_procedure_target_query(target)
        count = 0
        do i = 1, arena%size
            if (.not. node_is_descendant_of(arena, i, body_indices)) cycle
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (if_node)
                query%has_nested_branch = .true.
                type is (do_loop_node)
                query%has_loop = .true.
                type is (do_while_node)
                query%has_loop = .true.
                type is (nullify_node)
                if (nullify_touches_pointer(arena, i, declaration_index, &
                    pointer_name)) query%has_nullify = .true.
                type is (pointer_assignment_node)
                assignment = query_pointer_assignment(arena, i)
                if (.not. assignment%found) cycle
                call resolve_identifier_binding(arena, assignment%pointer_node_index, &
                    binding, error_msg)
                if (.not. binding%found .or. binding%declaration_node_index /= &
                    declaration_index .or. .not. same_name(binding%name, pointer_name)) cycle
                count = count + 1
                if (count == 1) target = query_procedure_target(arena, i)
                type is (assignment_node)
                call procedure_target_name_at(arena, node%target_index, name)
                call resolve_name_in_scope(arena, query%scope_node_index, name, &
                    binding, error_msg)
                if (binding%found .and. binding%declaration_node_index == &
                    declaration_index) query%has_reassignment = .true.
            class default
            end select
        end do
        if (count == 0) query%has_missing_assignment = .true.
        if (count > 1) query%has_reassignment = .true.
        if (target%found) then
            if (target%is_null) query%has_null_assignment = .true.
            if (target%is_unresolved) query%is_unresolved = .true.
            if (target%binding_kind == BINDING_GENERIC_INTERFACE) then
                query%has_generic_target = .true.
                query%has_ambiguous_target = .true.
            end if
        end if
        call detect_callback_branch_call(arena, body_indices, pointer_name, query)
    end subroutine inspect_callback_arm

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

    function query_procedure_pointer_state(arena, node_index) result(query)
        !! Return one bounded procedure-pointer ASSOCIATED/NULLIFY fact.
        !!
        !! ASSOCIATED is intentionally limited to its unary form.  The
        !! associated state is proved only from one direct same-scope pointer
        !! assignment, with at most one direct NULLIFY before the observation,
        !! and no nested mutation.  This is a source-order fact, not a general
        !! data-flow analysis.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(procedure_pointer_state_query_t) :: query
        type(declaration_binding_t) :: pointer_binding
        type(declaration_binding_t) :: operation_binding
        type(declaration_query_t) :: pointer_declaration
        type(procedure_target_query_t) :: target
        type(storage_query_t) :: pointer_storage
        character(len=:), allocatable :: error_msg, name
        integer, allocatable :: scope_indices(:)
        integer :: observation_statement
        integer :: assignment_count, assignment_index
        integer :: nullify_count, nullify_index
        logical :: is_associated_node

        call initialize_procedure_pointer_state_query(query)
        if (.not. arena%has_node_at(node_index)) return

        is_associated_node = .false.
        select type (node => arena%entries(node_index)%node)
            type is (call_or_subscript_node)
            if (node%is_array_access) return
            if (.not. allocated(node%name)) return
            if (lower_text(trim(node%name)) /= 'associated') return
            if (.not. node%is_intrinsic) return
            call resolve_identifier_binding(arena, node_index, operation_binding, &
                error_msg)
            if (operation_binding%found) return
            query%found = .true.
            query%is_associated_test = .true.
            query%observation_node_index = node_index
            is_associated_node = .true.
            if (.not. allocated(node%arg_indices)) then
                query%has_invalid_arity = .true.
                query%is_refused = .true.
                query%is_unresolved = .true.
                return
            end if
            if (size(node%arg_indices) /= 1) then
                query%has_invalid_arity = .true.
                query%has_second_argument = size(node%arg_indices) > 1
                query%is_refused = .true.
                query%is_unresolved = .true.
                return
            end if
            query%pointer_node_index = node%arg_indices(1)
            type is (nullify_node)
            query%found = .true.
            query%is_nullify = .true.
            query%observation_node_index = node_index
            if (.not. allocated(node%pointer_indices)) then
                query%has_multiple_pointers = .true.
                query%is_refused = .true.
                query%is_unresolved = .true.
                return
            end if
            if (size(node%pointer_indices) /= 1) then
                query%has_multiple_pointers = .true.
                query%is_refused = .true.
                query%is_unresolved = .true.
                return
            end if
            query%pointer_node_index = node%pointer_indices(1)
        class default
            return
        end select

        if (.not. arena%has_node_at(query%pointer_node_index)) then
            query%has_non_identifier_pointer = .true.
            query%is_refused = .true.
            query%is_unresolved = .true.
            return
        end if
        if (.not. is_identifier_at(arena, query%pointer_node_index)) then
            query%has_non_identifier_pointer = .true.
            query%is_refused = .true.
            query%is_unresolved = .true.
            return
        end if

        query%scope_node_index = find_enclosing_scope(arena, node_index)
        if (query%scope_node_index <= 0) then
            query%is_refused = .true.
            query%is_unresolved = .true.
            return
        end if
        if (is_associated_node) then
            call resolve_identifier_binding(arena, query%pointer_node_index, &
                pointer_binding, error_msg)
        else
            call procedure_target_name_at(arena, query%pointer_node_index, name)
            if (len_trim(name) == 0) then
                query%is_refused = .true.
                query%is_unresolved = .true.
                return
            end if
            call resolve_name_in_scope(arena, query%scope_node_index, name, &
                pointer_binding, error_msg)
        end if
        if (.not. pointer_binding%found) then
            query%is_refused = .true.
            query%is_unresolved = .true.
            return
        end if
        if (pointer_binding%binding_kind /= BINDING_DECLARATION) then
            query%has_alias = pointer_binding%binding_kind == BINDING_ASSOCIATE_NAME
            query%has_flow_sensitive_state = .true.
            query%is_refused = .true.
            query%is_unresolved = .true.
            return
        end if
        query%pointer_declaration_index = pointer_binding%declaration_node_index
        query%pointer_name = pointer_binding%name
        pointer_declaration = query_declaration(arena, &
            query%pointer_declaration_index)
        if (.not. is_procedure_pointer_declaration(pointer_declaration)) then
            query%has_non_procedure_pointer = .true.
            query%is_refused = .true.
            query%is_unresolved = .true.
            return
        end if
        pointer_storage = query_storage(arena, query%pointer_declaration_index)
        if (pointer_storage%is_module_state .or. pointer_storage%is_save_state .or. &
            pointer_storage%is_common_state) then
            query%has_global_mutable_state = .true.
            query%is_refused = .true.
            query%is_unresolved = .true.
            return
        end if
        if (pointer_binding%association /= ASSOCIATION_DIRECT .or. &
            is_procedure_pointer_dummy(arena, query%scope_node_index, &
            pointer_binding%declaration_node_index)) then
            query%has_alias = .true.
            query%is_refused = .true.
            query%is_unresolved = .true.
            return
        end if

        if (.not. is_associated_node) then
            query%state_known = .true.
            query%is_associated = .false.
            query%is_unresolved = .false.
            query%is_refused = .false.
            return
        end if

        call direct_scope_statement_for_node(arena, node_index, &
            query%scope_node_index, observation_statement)
        if (observation_statement <= 0) then
            query%is_refused = .true.
            query%is_unresolved = .true.
            return
        end if
        if (is_pointer_state_control_statement(arena, observation_statement)) then
            if (.not. is_pointer_state_condition_observation(arena, node_index, &
                observation_statement)) then
                query%has_control_flow_boundary = .true.
                query%is_refused = .true.
                query%is_unresolved = .true.
                return
            end if
        end if

        call get_scope_statement_indices(arena, query%scope_node_index, &
            scope_indices)
        call scan_pointer_state_mutations_before(arena, query%scope_node_index, &
            query%pointer_declaration_index, query%pointer_name, scope_indices, &
            observation_statement, assignment_count, assignment_index, &
            nullify_count, nullify_index, query%has_flow_sensitive_state)
        query%has_reassignment = assignment_count > 1
        query%has_nullify = nullify_count > 0
        if (assignment_count /= 1) then
            query%has_flow_sensitive_state = .true.
        end if
        if (nullify_count > 1) then
            query%has_flow_sensitive_state = .true.
        end if
        if (assignment_count /= 1 .or. nullify_count > 1 .or. &
            query%has_flow_sensitive_state) then
            query%is_refused = .true.
            query%is_unresolved = .true.
            return
        end if

        target = query_procedure_target(arena, assignment_index)
        if (.not. target%found) then
            query%has_unresolved_target = .true.
            query%is_refused = .true.
            query%is_unresolved = .true.
            return
        end if
        query%assignment_node_index = assignment_index
        if (target%is_null) then
            query%has_null_assignment = .true.
        else if (.not. target%is_resolved) then
            query%has_unresolved_target = .true.
            query%is_refused = .true.
            query%is_unresolved = .true.
            return
        end if

        if (nullify_count == 1) then
            if (index_precedes(scope_indices, assignment_index, nullify_index)) then
                query%nullify_node_index = nullify_index
                query%state_known = .true.
                query%is_associated = .false.
            else
                query%state_known = .true.
                query%is_associated = .not. target%is_null
            end if
        else
            query%state_known = .true.
            query%is_associated = .not. target%is_null
        end if
        query%is_unresolved = .false.
        query%is_refused = .false.
    end function query_procedure_pointer_state

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
        type(procedure_signature_query_t) :: signature
        type(procedure_dummy_query_t) :: dummy
        type(resolved_type_query_t) :: actual_type
        type(storage_query_t) :: actual_storage
        type(global_reference_query_t), allocatable :: global_refs(:)
        type(declaration_binding_t) :: actual_binding
        integer, allocatable :: seen_declarations(:)
        integer :: actual_declaration_index

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
            call initialize_call_argument_query(query%arguments(i))
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

        ! Attach the semantic facts a differentiation backend needs at the
        ! call boundary.  The mapping remains a useful fact even when the
        ! call is refused for AD; the refusal flags prevent a consumer from
        ! silently treating aliases, callbacks, or global state as pure.
        call fill_procedure_signature(arena, binding%node_index, signature)
        allocate (seen_declarations(n_formal), source=0)
        do i = 1, n_formal
            if (i <= signature%dummy_count) then
                dummy = signature%dummies(i)
                query%arguments(i)%formal_intent = dummy%intent
                query%arguments(i)%formal_type_category = dummy%type_category
                query%arguments(i)%formal_type_kind = dummy%type_kind
                query%arguments(i)%formal_kind_value = dummy%kind_value
                query%arguments(i)%formal_rank = dummy%rank
                query%arguments(i)%formal_intent_known = dummy%has_intent
                query%arguments(i)%formal_type_known = dummy%type_known
                query%arguments(i)%formal_kind_known = dummy%kind_known
                query%arguments(i)%formal_rank_known = dummy%rank_known
                query%arguments(i)%formal_is_value = dummy%is_value
            end if

            formal_query = query_declaration(arena, &
                query%arguments(i)%formal_node_index)
            if (formal_query%found) then
                query%arguments(i)%formal_is_pointer = formal_query%is_pointer
                query%arguments(i)%formal_is_allocatable = &
                    formal_query%is_allocatable
                query%arguments(i)%formal_is_target = formal_query%is_target
            end if
            if (.not. query%arguments(i)%is_supplied) cycle

            actual_type = query_resolved_type(arena, &
                query%arguments(i)%actual_value_node_index)
            query%arguments(i)%actual_type_known = actual_type%found
            query%arguments(i)%actual_type_kind = actual_type%type_kind
            query%arguments(i)%actual_kind_value = actual_type%kind_value
            query%arguments(i)%actual_rank = actual_type%rank
            query%arguments(i)%actual_kind_known = actual_type%kind_value > 0
            query%arguments(i)%actual_rank_known = actual_type%rank >= 0
            query%arguments(i)%actual_derived_type_name = &
                actual_type%derived_type_name

            actual_storage = query_storage(arena, &
                query%arguments(i)%actual_value_node_index)
            actual_declaration_index = 0
            call resolve_identifier_binding(arena, &
                query%arguments(i)%actual_value_node_index, actual_binding, &
                error_msg)
            if (actual_binding%found .and. &
                    actual_binding%declaration_node_index > 0) then
                actual_declaration_index = actual_binding%declaration_node_index
                formal_query = query_declaration(arena, actual_declaration_index)
                if (formal_query%found) then
                    query%arguments(i)%actual_is_pointer = formal_query%is_pointer
                    query%arguments(i)%actual_is_allocatable = &
                        formal_query%is_allocatable
                    query%arguments(i)%actual_is_target = formal_query%is_target
                end if
            end if
            if (actual_storage%found) then
                query%arguments(i)%actual_is_pointer = actual_storage%is_pointer
                query%arguments(i)%actual_is_allocatable = &
                    actual_storage%is_allocatable
                query%arguments(i)%actual_is_target = actual_storage%is_target
                if (actual_storage%declaration_index > 0) &
                    actual_declaration_index = actual_storage%declaration_index
                if (actual_storage%is_pointer .or. actual_storage%is_target .or. &
                        actual_storage%is_allocatable) then
                    query%has_unresolved_alias = .true.
                end if
            end if
            if (actual_declaration_index > 0) then
                do j = 1, i - 1
                    if (seen_declarations(j) == actual_declaration_index) then
                        query%has_unresolved_alias = .true.
                    end if
                end do
                seen_declarations(i) = actual_declaration_index
            end if
            formal_query = query_declaration(arena, &
                query%arguments(i)%formal_node_index)
            if (query%arguments(i)%formal_is_pointer .or. &
                    query%arguments(i)%formal_is_target .or. &
                    query%arguments(i)%formal_is_allocatable) then
                query%has_unresolved_alias = .true.
            end if

            if (is_procedure_dummy_declaration(formal_query)) then
                query%has_procedure_callback = .true.
            else if (query%arguments(i)%formal_type_known .and. &
                    query%arguments(i)%actual_type_known) then
                query%arguments(i)%type_compatibility_known = .true.
                query%arguments(i)%has_type_mismatch = &
                    .not. call_argument_types_match(query%arguments(i), &
                    actual_type)
                if (query%arguments(i)%has_type_mismatch) then
                    query%has_type_mismatch = .true.
                end if
            else
                query%has_unknown_argument_types = .true.
            end if
        end do

        global_refs = query_active_global_references(arena, binding%node_index)
        query%has_global_mutable_state = .false.
        do j = 1, size(global_refs)
            formal_query = query_declaration(arena, &
                global_refs(j)%declaration_node_index)
            if (formal_query%found .and. .not. formal_query%is_parameter) then
                query%has_global_mutable_state = .true.
                exit
            end if
        end do
        query%is_refused = query%has_global_mutable_state .or. &
            query%has_unresolved_alias .or. query%has_procedure_callback .or. &
            query%has_unknown_argument_types .or. query%has_type_mismatch

        query%found = .true.
        query%call_node_index = call_node_index
        query%procedure_node_index = binding%node_index
        query%procedure_name = procedure%name
        query%procedure_kind = procedure%unit_kind
    end function query_call_arguments

    function query_procedure_actual_argument(arena, call_node_index, &
            formal_name) result(query)
        !! Join one procedure actual with a named procedure dummy.
        !!
        !! The call mapping is obtained from QUERY_CALL_ARGUMENTS.  A target
        !! is exposed when the mapped actual is either a directly resolved
        !! same-arena function or subroutine, or a procedure pointer with one
        !! unconditional direct assignment before this call.  Procedure
        !! dummies and other contextual targets remain refusal-only facts.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        character(len=*), intent(in) :: formal_name
        type(procedure_actual_argument_query_t) :: query
        type(call_arguments_query_t) :: call_query
        type(call_argument_query_t) :: argument
        type(declaration_query_t) :: formal_declaration, actual_declaration
        type(declaration_binding_t) :: actual_binding
        type(procedure_target_query_t) :: pointer_target
        character(len=:), allocatable :: actual_error
        integer :: i, scope_index, mutation_count, assignment_index
        integer :: call_statement
        integer, allocatable :: scope_indices(:)
        logical :: has_non_direct_mutation, has_reassignment

        call initialize_procedure_actual_argument_query(query)
        if (.not. arena%has_node_at(call_node_index)) return
        if (len_trim(formal_name) == 0) return

        call_query = query_call_arguments(arena, call_node_index)
        if (.not. call_query%found) return
        do i = 1, size(call_query%arguments)
            if (.not. same_name(call_query%arguments(i)%formal_name, &
                    formal_name)) cycle
            argument = call_query%arguments(i)
            formal_declaration = query_declaration(arena, &
                argument%formal_node_index)
            if (.not. is_procedure_dummy_declaration(formal_declaration)) return

            query%found = .true.
            query%call_node_index = call_node_index
            query%formal_node_index = argument%formal_node_index
            query%formal_name = argument%formal_name
            query%actual_node_index = argument%actual_node_index
            query%actual_value_node_index = argument%actual_value_node_index
            if (.not. argument%is_supplied) then
                query%is_unresolved = .true.
                query%is_refused = .true.
                return
            end if

            call identifier_name_at(arena, argument%actual_value_node_index, &
                query%actual_name)
            if (.not. is_identifier_at(arena, argument%actual_value_node_index)) then
                query%is_unresolved = .true.
                query%is_refused = .true.
                return
            end if

            call resolve_identifier_binding(arena, &
                argument%actual_value_node_index, actual_binding, actual_error)
            if (.not. actual_binding%found) then
                query%is_unresolved = .true.
                query%is_refused = .true.
                return
            end if

            select case (actual_binding%binding_kind)
            case (BINDING_FUNCTION, BINDING_SUBROUTINE)
                if (actual_binding%node_index <= 0) then
                    query%is_unresolved = .true.
                    query%is_refused = .true.
                    return
                end if
                query%target_procedure_index = actual_binding%node_index
                query%target_binding_node_index = actual_binding%node_index
                query%procedure_name = actual_binding%name
                if (actual_binding%binding_kind == BINDING_FUNCTION) then
                    query%procedure_kind = 'function'
                else
                    query%procedure_kind = 'subroutine'
                end if
                call fill_procedure_signature(arena, actual_binding%node_index, &
                    query%signature)
                if (.not. query%signature%found) then
                    query%is_unresolved = .true.
                    query%is_refused = .true.
                    return
                end if
                query%is_resolved = .true.
                return
            case (BINDING_GENERIC_INTERFACE)
                query%has_ambiguous_target = .true.
                query%is_unresolved = .true.
                query%is_refused = .true.
                return
            case (BINDING_DECLARATION, BINDING_ASSOCIATE_NAME)
                actual_declaration = query_declaration(arena, &
                    actual_binding%declaration_node_index)
                query%has_contextual_target = .true.
                if (is_procedure_pointer_declaration(actual_declaration)) then
                    scope_index = find_enclosing_scope(arena, &
                        argument%actual_value_node_index)
                    if (scope_index > 0) then
                        call get_scope_statement_indices(arena, scope_index, &
                            scope_indices)
                        call find_pointer_mutations(arena, scope_index, &
                            actual_binding%declaration_node_index, query%actual_name, &
                            scope_indices, mutation_count, assignment_index, &
                            has_non_direct_mutation, has_reassignment)
                        query%has_reassignment = has_reassignment
                        query%has_branch_target = has_non_direct_mutation
                        call direct_scope_statement_for_node(arena, &
                            call_node_index, scope_index, call_statement)
                        if (mutation_count /= 1 .or. has_non_direct_mutation .or. &
                                call_statement <= 0 .or. &
                                .not. index_precedes(scope_indices, &
                                assignment_index, call_statement)) then
                            query%has_ambiguous_target = .true.
                            query%has_unresolved_target = .true.
                            query%is_unresolved = .true.
                            query%is_refused = .true.
                            return
                        end if

                        pointer_target = query_procedure_target(arena, &
                            assignment_index)
                        if (.not. pointer_target%found) then
                            query%has_unresolved_target = .true.
                            query%is_unresolved = .true.
                            query%is_refused = .true.
                            return
                        end if
                        query%target_assignment_node_index = &
                            pointer_target%assignment_node_index
                        query%target_node_index = pointer_target%target_node_index
                        if (pointer_target%is_null) then
                            query%has_null_target = .true.
                            query%has_unresolved_target = .true.
                            query%is_unresolved = .true.
                            query%is_refused = .true.
                            return
                        end if
                        if (.not. pointer_target%is_resolved .or. &
                                .not. pointer_target%signature%found) then
                            query%has_unresolved_target = .true.
                            query%is_unresolved = .true.
                            query%is_refused = .true.
                            return
                        end if
                        query%target_procedure_index = &
                            pointer_target%target_procedure_index
                        query%target_declaration_index = &
                            pointer_target%target_declaration_index
                        query%target_binding_node_index = &
                            pointer_target%binding_node_index
                        query%procedure_name = pointer_target%procedure_name
                        query%signature = pointer_target%signature
                        query%is_resolved = .true.
                        query%is_unresolved = .false.
                        query%is_refused = .false.
                        return
                    end if
                end if
                query%is_unresolved = .true.
                query%is_refused = .true.
                return
            case default
                query%is_unresolved = .true.
                query%is_refused = .true.
                return
            end select
        end do
    end function query_procedure_actual_argument

    subroutine initialize_call_argument_query(query)
        type(call_argument_query_t), intent(out) :: query

        call set_empty(query%formal_name)
        call set_empty(query%formal_intent)
        call set_empty(query%formal_type_category)
        call set_empty(query%actual_derived_type_name)
    end subroutine initialize_call_argument_query

    subroutine initialize_procedure_actual_argument_query(query)
        type(procedure_actual_argument_query_t), intent(out) :: query

        call set_empty(query%formal_name)
        call set_empty(query%actual_name)
        call set_empty(query%procedure_name)
        call set_empty(query%procedure_kind)
        call initialize_procedure_signature_query(query%signature)
    end subroutine initialize_procedure_actual_argument_query

    logical function is_procedure_dummy_declaration(query) result(is_procedure)
        type(declaration_query_t), intent(in) :: query
        character(len=:), allocatable :: normalized

        is_procedure = .false.
        if (.not. query%found) return
        normalized = remove_type_spec_spaces(lower_text(query%type_name))
        is_procedure = index(normalized, 'procedure') == 1
    end function is_procedure_dummy_declaration

    logical function call_argument_types_match(argument, actual) result(matches)
        type(call_argument_query_t), intent(in) :: argument
        type(resolved_type_query_t), intent(in) :: actual
        logical :: formal_is_derived, actual_is_derived
        character(len=:), allocatable :: formal_derived_name

        matches = argument%formal_type_known .and. actual%found
        if (.not. matches) return
        matches = argument%formal_type_kind == actual%type_kind .and. &
            argument%formal_rank == actual%rank
        if (.not. matches) return
        formal_is_derived = index(lower_text(argument%formal_type_category), &
            'type:') == 1 .or. index(lower_text(argument%formal_type_category), &
            'class:') == 1
        actual_is_derived = len_trim(actual%derived_type_name) > 0
        if (formal_is_derived .or. actual_is_derived) then
            formal_derived_name = argument%formal_type_category
            if (index(formal_derived_name, ':') > 0) then
                formal_derived_name = formal_derived_name(index( &
                    formal_derived_name, ':') + 1:)
            end if
            matches = formal_is_derived .and. actual_is_derived .and. &
                same_name(formal_derived_name, actual%derived_type_name)
        end if
        if (argument%formal_kind_known .and. argument%formal_kind_value > 0) then
            matches = matches .and. argument%formal_kind_value == actual%kind_value
        end if
    end function call_argument_types_match

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

    function query_defined_operator(arena, operator_node_index) result(query)
        !! Compatibility function for callers that can safely receive a
        !! derived result with nested allocatable components.  NVHPC 26.5
        !! callers must use query_defined_operator_into instead.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: operator_node_index
        type(defined_operator_query_t) :: query

        call query_defined_operator_into(arena, operator_node_index, query)
    end function query_defined_operator

    subroutine query_defined_operator_into(arena, operator_node_index, query)
        !! Resolve one same-arena user-defined unary or binary operator.
        !!
        !! The query walks visible INTERFACE OPERATOR(...) blocks, expands
        !! their concrete procedures, and compares the actual operands with
        !! the formal operands by semantic type, kind, rank, and derived-type
        !! identity.  It never applies an implicit conversion or guesses a
        !! dynamic/pointer target.  A unique exact candidate is selected only
        !! when its operands and procedure body are outside the explicit
        !! pointer/TARGET, polymorphic, and mutable-global boundaries.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: operator_node_index
        type(defined_operator_query_t), intent(out) :: query

        character(len=:), allocatable :: operator_symbol
        integer :: left_index, right_index, line, column
        character(len=:), allocatable :: error_msg
        integer, allocatable :: actual_indices(:), interface_indices(:)
        integer, allocatable :: procedure_indices(:)
        type(defined_operator_candidate_query_t) :: candidate
        integer :: i, j, match_count

        call initialize_defined_operator_query(query)
        if (.not. arena%has_node_at(operator_node_index)) return
        call get_binary_op_info(arena, operator_node_index, operator_symbol, &
            left_index, right_index, line, column, error_msg)
        if (len_trim(error_msg) > 0 .or. len_trim(operator_symbol) == 0) return

        query%operator_node_index = operator_node_index
        query%operator = normalize_generic_operator(operator_symbol)
        if (left_index > 0 .and. right_index > 0) then
            query%is_binary = .true.
            allocate (actual_indices(2))
            actual_indices = [left_index, right_index]
        else if (left_index > 0 .or. right_index > 0) then
            query%is_unary = .true.
            allocate (actual_indices(1))
            if (right_index > 0) then
                actual_indices(1) = right_index
            else
                actual_indices(1) = left_index
            end if
        else
            return
        end if

        call collect_visible_operator_interfaces(arena, operator_node_index, &
            query%operator, interface_indices)
        if (size(interface_indices) == 0) return

        query%found = .true.
        query%is_defined_operator = .true.
        query%interface_node_indices = interface_indices
        if (size(interface_indices) == 1) then
            query%interface_node_index = interface_indices(1)
        else
            query%is_ambiguous = .true.
        end if

        do i = 1, size(interface_indices)
            call collect_generic_candidate_indices(arena, interface_indices(i), &
                procedure_indices)
            do j = 1, size(procedure_indices)
                call fill_defined_operator_candidate(arena, actual_indices, &
                    interface_indices(i), procedure_indices(j), candidate)
                call append_defined_operator_candidate(query%candidates, &
                    candidate)
            end do
        end do

        match_count = 0
        do i = 1, size(query%candidates)
            if (query%candidates(i)%has_conversion) query%has_conversion = .true.
            if (query%candidates(i)%has_unknown_types) &
                query%has_unknown_types = .true.
            if (query%candidates(i)%has_pointer_operand) &
                query%has_pointer_operand = .true.
            if (query%candidates(i)%has_global_mutable_state) &
                query%has_global_mutable_state = .true.
            if (query%candidates(i)%has_invalid_arity) &
                query%has_invalid_arity = .true.
            if (query%candidates(i)%is_match) match_count = match_count + 1
        end do

        query%has_exact_match = match_count > 0
        if (match_count > 1) query%is_ambiguous = .true.
        if (match_count == 1 .and. .not. query%is_ambiguous) then
            do i = 1, size(query%candidates)
                if (.not. query%candidates(i)%is_match) cycle
                query%selected_procedure_node_index = &
                    query%candidates(i)%procedure_node_index
                query%is_resolved = .true.
                exit
            end do
        end if

        if (.not. query%is_resolved) then
            query%is_unresolved = .true.
            query%is_refused = .true.
            call set_defined_operator_refusal_reason(query)
        end if
    end subroutine query_defined_operator_into

    subroutine fill_defined_operator_candidate(arena, actual_indices, &
            interface_index, procedure_index, candidate)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: actual_indices(:)
        integer, intent(in) :: interface_index, procedure_index
        type(defined_operator_candidate_query_t), intent(out) :: candidate
        type(program_unit_query_t) :: procedure
        type(declaration_query_t) :: declaration
        type(storage_query_t) :: formal_storage
        type(global_reference_query_t), allocatable :: global_refs(:)
        integer :: actual_count, formal_count, operand_count, i

        call initialize_defined_operator_candidate(candidate)
        procedure = query_program_unit(arena, procedure_index)
        if (.not. procedure%found) return
        if (procedure%unit_kind /= 'function' .and. &
            procedure%unit_kind /= 'subroutine') return

        candidate%found = .true.
        candidate%interface_node_index = interface_index
        candidate%procedure_node_index = procedure_index
        candidate%procedure_name = procedure%name
        candidate%procedure_kind = procedure%unit_kind
        actual_count = size(actual_indices)
        formal_count = size(procedure%parameter_indices)
        operand_count = max(actual_count, formal_count)
        if (allocated(candidate%operands)) deallocate (candidate%operands)
        allocate (candidate%operands(operand_count))
        candidate%is_match = actual_count == formal_count
        candidate%has_invalid_arity = .not. candidate%is_match

        do i = 1, operand_count
            call initialize_defined_operator_operand(candidate%operands(i))
            if (i <= actual_count) candidate%operands(i)%actual_node_index = &
                actual_indices(i)
            if (i <= formal_count) candidate%operands(i)%formal_node_index = &
                procedure%parameter_indices(i)
            if (i > actual_count .or. i > formal_count) cycle
            call fill_defined_operator_operand(arena, &
                candidate%operands(i)%actual_node_index, &
                candidate%operands(i)%formal_node_index, candidate%operands(i))
            candidate%has_conversion = candidate%has_conversion .or. &
                candidate%operands(i)%has_conversion
            candidate%has_unknown_types = candidate%has_unknown_types .or. &
                candidate%operands(i)%has_unknown_type
            candidate%has_pointer_operand = candidate%has_pointer_operand .or. &
                candidate%operands(i)%actual_is_pointer .or. &
                candidate%operands(i)%actual_is_target .or. &
                candidate%operands(i)%formal_is_pointer .or. &
                candidate%operands(i)%formal_is_target
            candidate%has_global_mutable_state = &
                candidate%has_global_mutable_state .or. &
                candidate%operands(i)%actual_has_global_mutable_state
            if (.not. candidate%operands(i)%is_exact) candidate%is_match = .false.
        end do

        global_refs = query_active_global_references(arena, procedure_index)
        candidate%has_global_mutable_state = candidate%has_global_mutable_state .or. &
            size(global_refs) > 0
        if (candidate%has_pointer_operand .or. &
                candidate%has_global_mutable_state) candidate%is_match = .false.

        do i = 1, formal_count
            declaration = query_declaration(arena, procedure%parameter_indices(i))
            formal_storage = query_designator_storage(arena, &
                procedure%parameter_indices(i))
            if (.not. declaration%found .or. .not. formal_storage%found) cycle
            if (declaration%is_pointer .or. declaration%is_target) then
                candidate%has_pointer_operand = .true.
                candidate%is_match = .false.
            end if
        end do
    end subroutine fill_defined_operator_candidate

    subroutine fill_defined_operator_operand(arena, actual_index, formal_index, &
            operand)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: actual_index, formal_index
        type(defined_operator_operand_query_t), intent(inout) :: operand
        type(resolved_type_query_t) :: actual_type, formal_type
        type(storage_query_t) :: actual_storage, formal_storage

        actual_type = query_resolved_type(arena, actual_index)
        formal_type = query_resolved_type(arena, formal_index)
        actual_storage = query_designator_storage(arena, actual_index)
        formal_storage = query_designator_storage(arena, formal_index)
        operand%found = actual_index > 0 .and. formal_index > 0
        operand%actual_type_known = actual_type%found .and. &
            .not. actual_storage%is_polymorphic
        operand%formal_type_known = formal_type%found .and. &
            .not. formal_storage%is_polymorphic
        operand%actual_type_kind = actual_type%type_kind
        operand%actual_kind_value = actual_type%kind_value
        operand%actual_rank = actual_type%rank
        operand%formal_type_kind = formal_type%type_kind
        operand%formal_kind_value = formal_type%kind_value
        operand%formal_rank = formal_type%rank
        operand%actual_derived_type_name = actual_type%derived_type_name
        operand%formal_derived_type_name = formal_type%derived_type_name
        if (actual_storage%found) then
            operand%actual_is_pointer = actual_storage%is_pointer
            operand%actual_is_target = actual_storage%is_target
            operand%actual_is_allocatable = actual_storage%is_allocatable
            operand%actual_is_polymorphic = actual_storage%is_polymorphic
            operand%actual_has_global_mutable_state = &
                storage_has_global_state(actual_storage)
        end if
        if (formal_storage%found) then
            operand%formal_is_pointer = formal_storage%is_pointer
            operand%formal_is_target = formal_storage%is_target
            operand%formal_is_allocatable = formal_storage%is_allocatable
            operand%formal_is_polymorphic = formal_storage%is_polymorphic
        end if

        operand%has_unknown_type = .not. operand%actual_type_known .or. &
            .not. operand%formal_type_known
        if (operand%has_unknown_type) return
        if (.not. defined_operator_types_match(operand)) then
            operand%has_conversion = .true.
            return
        end if
        operand%is_exact = .not. operand%actual_is_pointer .and. &
            .not. operand%actual_is_target .and. &
            .not. operand%formal_is_pointer .and. &
            .not. operand%formal_is_target .and. &
            .not. operand%actual_is_polymorphic .and. &
            .not. operand%formal_is_polymorphic .and. &
            .not. operand%actual_has_global_mutable_state
    end subroutine fill_defined_operator_operand

    logical function defined_operator_types_match(operand) result(matches)
        type(defined_operator_operand_query_t), intent(in) :: operand

        matches = operand%actual_type_kind == operand%formal_type_kind .and. &
            operand%actual_kind_value == operand%formal_kind_value .and. &
            operand%actual_rank == operand%formal_rank
        if (.not. matches) return
        if (len_trim(operand%actual_derived_type_name) > 0 .or. &
                len_trim(operand%formal_derived_type_name) > 0) then
            matches = same_name(operand%actual_derived_type_name, &
                operand%formal_derived_type_name)
        end if
    end function defined_operator_types_match

    subroutine collect_visible_operator_interfaces(arena, operator_node_index, &
            operator_symbol, interface_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: operator_node_index
        character(len=*), intent(in) :: operator_symbol
        integer, allocatable, intent(out) :: interface_indices(:)
        integer :: i

        allocate (interface_indices(0))
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (interface_block_node)
                if (.not. allocated(node%kind) .or. &
                        .not. same_name(node%kind, 'operator')) cycle
                if (.not. allocated(node%operator)) cycle
                if (normalize_generic_operator(node%operator) /= &
                        normalize_generic_operator(operator_symbol)) cycle
                if (.not. operator_interface_visible(arena, i, &
                        operator_node_index)) cycle
                call append_candidate_index(interface_indices, i)
            end select
        end do
    end subroutine collect_visible_operator_interfaces

    logical function operator_interface_visible(arena, interface_index, &
            operator_node_index) result(is_visible)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: interface_index, operator_node_index
        integer :: call_scope, interface_scope, module_index, current_scope
        character(len=:), allocatable :: module_name

        is_visible = .false.
        call_scope = find_enclosing_scope(arena, operator_node_index)
        interface_scope = find_enclosing_scope(arena, interface_index)
        if (call_scope <= 0) return
        if (node_is_in_scope(arena, interface_index, call_scope)) then
            is_visible = .true.
            return
        end if
        if (interface_scope > 0 .and. node_is_in_scope(arena, call_scope, &
                interface_scope)) then
            is_visible = .true.
            return
        end if

        module_index = interface_scope
        if (module_index <= 0) module_index = enclosing_module(arena, &
            interface_index)
        if (module_index <= 0) return
        select type (module => arena%entries(module_index)%node)
            type is (module_node)
            if (.not. allocated(module%name)) return
            module_name = module%name
            if (.not. module_operator_is_public(arena, module_index, &
                    interface_index)) return
            current_scope = call_scope
            do while (current_scope > 0)
                if (scope_uses_operator(arena, current_scope, module_name, &
                        interface_index)) then
                    is_visible = .true.
                    return
                end if
                current_scope = find_host_scope(arena, current_scope)
            end do
        class default
        end select
    end function operator_interface_visible

    logical function scope_uses_operator(arena, scope_index, module_name, &
            interface_index) result(is_used)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index, interface_index
        character(len=*), intent(in) :: module_name
        type(interface_query_t) :: iface
        integer, allocatable :: statement_indices(:)
        integer :: i, j

        is_used = .false.
        iface = query_interface(arena, interface_index)
        call get_scope_statement_indices(arena, scope_index, statement_indices)
        do i = 1, size(statement_indices)
            if (.not. arena%has_node_at(statement_indices(i))) cycle
            select type (use => arena%entries(statement_indices(i))%node)
                type is (use_statement_node)
                if (use%is_intrinsic .or. .not. allocated(use%module_name)) cycle
                if (.not. same_name(use%module_name, module_name)) cycle
                if (.not. use%has_only) then
                    is_used = .true.
                    return
                end if
                if (.not. allocated(use%only_list)) cycle
                do j = 1, size(use%only_list)
                    if (operator_spec_matches(use%only_list(j)%s, &
                            iface%operator)) then
                        is_used = .true.
                        return
                    end if
                    if (j < size(use%only_list)) then
                        if (operator_spec_matches(use%only_list(j + 1)%s, &
                                iface%operator)) then
                            is_used = .true.
                            return
                        end if
                    end if
                end do
            end select
        end do
    end function scope_uses_operator

    logical function operator_spec_matches(text, operator_symbol) result(matches)
        character(len=*), intent(in) :: text, operator_symbol
        character(len=:), allocatable :: lowered, wanted
        integer :: open_pos, close_pos

        matches = .false.
        lowered = to_lower(trim(text))
        if (len_trim(lowered) < len('operator()')) return
        if (index(lowered, 'operator(') /= 1) return
        open_pos = index(lowered, '(')
        close_pos = len_trim(lowered)
        if (close_pos <= open_pos .or. lowered(close_pos:close_pos) /= ')') return
        wanted = normalize_generic_operator(lowered(open_pos + 1:close_pos - 1))
        matches = wanted == normalize_generic_operator(operator_symbol)
    end function operator_spec_matches

    logical function module_operator_is_public(arena, module_index, &
            interface_index) result(is_public)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: module_index, interface_index
        integer, allocatable :: statement_indices(:)
        type(interface_query_t) :: iface
        logical :: default_public, named, named_public
        integer :: i, j

        is_public = .true.
        iface = query_interface(arena, interface_index)
        call get_scope_statement_indices(arena, module_index, statement_indices)
        default_public = .true.
        named = .false.
        named_public = .false.
        do i = 1, size(statement_indices)
            if (.not. arena%has_node_at(statement_indices(i))) cycle
            select type (visibility => arena%entries(statement_indices(i))%node)
                type is (visibility_statement_node)
                if (.not. visibility%has_list) then
                    default_public = .not. visibility%is_private
                    cycle
                end if
                if (.not. allocated(visibility%names)) cycle
                do j = 1, size(visibility%names)
                    if (.not. operator_spec_matches(visibility%names(j)%s, &
                            iface%operator)) cycle
                    named = .true.
                    named_public = .not. visibility%is_private
                end do
            end select
        end do
        if (named) then
            is_public = named_public
        else
            is_public = default_public
        end if
    end function module_operator_is_public

    subroutine append_defined_operator_candidate(values, value)
        type(defined_operator_candidate_query_t), allocatable, intent(inout) :: values(:)
        type(defined_operator_candidate_query_t), intent(in) :: value
        type(defined_operator_candidate_query_t), allocatable :: grown(:)
        integer :: n

        n = size(values)
        allocate (grown(n + 1))
        if (n > 0) grown(:n) = values
        grown(n + 1) = value
        call move_alloc(grown, values)
    end subroutine append_defined_operator_candidate

    subroutine initialize_defined_operator_query(query)
        type(defined_operator_query_t), intent(out) :: query

        call set_empty(query%operator)
        call set_empty(query%refusal_reason)
        if (allocated(query%interface_node_indices)) deallocate (query%interface_node_indices)
        if (allocated(query%candidates)) deallocate (query%candidates)
        allocate (query%interface_node_indices(0))
        allocate (query%candidates(0))
    end subroutine initialize_defined_operator_query

    subroutine initialize_defined_operator_candidate(candidate)
        type(defined_operator_candidate_query_t), intent(out) :: candidate

        call set_empty(candidate%procedure_name)
        call set_empty(candidate%procedure_kind)
        if (allocated(candidate%operands)) deallocate (candidate%operands)
        allocate (candidate%operands(0))
    end subroutine initialize_defined_operator_candidate

    subroutine initialize_defined_operator_operand(operand)
        type(defined_operator_operand_query_t), intent(out) :: operand

        if (allocated(operand%actual_derived_type_name)) then
            deallocate (operand%actual_derived_type_name)
        end if
        if (allocated(operand%formal_derived_type_name)) then
            deallocate (operand%formal_derived_type_name)
        end if
        call set_empty(operand%actual_derived_type_name)
        call set_empty(operand%formal_derived_type_name)
    end subroutine initialize_defined_operator_operand

    subroutine set_defined_operator_refusal_reason(query)
        type(defined_operator_query_t), intent(inout) :: query

        if (query%is_ambiguous) then
            query%refusal_reason = 'ambiguous defined operator candidates'
        else if (query%has_pointer_operand) then
            query%refusal_reason = 'pointer or TARGET operator operand'
        else if (query%has_global_mutable_state) then
            query%refusal_reason = 'mutable global state in operator'
        else if (query%has_conversion) then
            query%refusal_reason = 'defined operator requires conversion'
        else if (query%has_unknown_types) then
            query%refusal_reason = 'defined operator operand type is unknown'
        else if (query%has_invalid_arity) then
            query%refusal_reason = 'defined operator candidate has invalid arity'
        else
            query%refusal_reason = 'no exact defined operator candidate'
        end if
    end subroutine set_defined_operator_refusal_reason

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
        call initialize_procedure_signature_query(query%signature)
    end subroutine initialize_procedure_target_query

    subroutine initialize_procedure_call_target_query(query)
        type(procedure_call_target_query_t), intent(out) :: query

        call set_empty(query%pointer_name)
        call set_empty(query%procedure_name)
        call set_empty(query%target_binding_name)
        call initialize_procedure_signature_query(query%signature)
    end subroutine initialize_procedure_call_target_query

    subroutine initialize_procedure_reassignment_call_query(query)
        type(procedure_reassignment_call_query_t), intent(out) :: query

        call set_empty(query%pointer_name)
        call initialize_procedure_target_query(query%first_target)
        call initialize_procedure_target_query(query%second_target)
    end subroutine initialize_procedure_reassignment_call_query

    subroutine initialize_procedure_callback_flow_query(query)
        type(procedure_callback_flow_query_t), intent(out) :: query

        call set_empty(query%pointer_name)
        allocate (query%targets(0))
    end subroutine initialize_procedure_callback_flow_query

    subroutine initialize_procedure_pointer_state_query(query)
        type(procedure_pointer_state_query_t), intent(out) :: query

        call set_empty(query%pointer_name)
    end subroutine initialize_procedure_pointer_state_query

    logical function node_is_descendant_of(arena, node_index, roots) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, roots(:)
        integer :: current, parent, i

        found = .false.
        current = node_index
        do while (current > 0 .and. arena%has_node_at(current))
            do i = 1, size(roots)
                if (current == roots(i)) then
                    found = .true.
                    return
                end if
            end do
            parent = arena%entries(current)%parent_index
            if (parent == current) exit
            current = parent
        end do
    end function node_is_descendant_of

    logical function node_is_direct_child(arena, node_index, roots) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, roots(:)
        integer :: i

        found = .false.
        if (.not. arena%has_node_at(node_index)) return
        do i = 1, size(roots)
            if (arena%entries(node_index)%parent_index == roots(i)) then
                found = .true.
                return
            end if
        end do
    end function node_is_direct_child

    logical function nullify_touches_pointer(arena, node_index, declaration_index, &
            pointer_name) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, declaration_index
        character(len=*), intent(in) :: pointer_name
        type(nullify_query_t) :: nullify
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: name, error_msg
        integer :: i

        found = .false.
        nullify = query_nullify(arena, node_index)
        if (.not. nullify%found .or. .not. allocated(nullify%pointer_node_indices)) return
        do i = 1, size(nullify%pointer_node_indices)
            call procedure_target_name_at(arena, nullify%pointer_node_indices(i), name)
            call resolve_name_in_scope(arena, find_enclosing_scope(arena, node_index), &
                name, binding, error_msg)
            if (binding%found .and. binding%declaration_node_index == declaration_index &
                .and. same_name(binding%name, pointer_name)) then
                found = .true.
                return
            end if
        end do
    end function nullify_touches_pointer

    logical function is_pointer_state_control_statement(arena, node_index) &
            result(is_control)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_control = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (if_node)
            is_control = .true.
            type is (select_case_node)
            is_control = .true.
            type is (select_type_node)
            is_control = .true.
            type is (do_loop_node)
            is_control = .true.
            type is (do_while_node)
            is_control = .true.
            type is (forall_node)
            is_control = .true.
            type is (where_node)
            is_control = .true.
            type is (where_stmt_node)
            is_control = .true.
        class default
        end select
    end function is_pointer_state_control_statement

    logical function is_pointer_state_condition_observation(arena, node_index, &
            control_index) result(is_condition)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, control_index
        integer :: i

        is_condition = .false.
        if (.not. arena%has_node_at(control_index)) return
        select type (control => arena%entries(control_index)%node)
            type is (if_node)
            if (control%condition_index > 0) then
                is_condition = node_is_descendant_of(arena, node_index, &
                    [control%condition_index])
            end if
            if (is_condition) return
            if (.not. allocated(control%elseif_blocks)) return
            do i = 1, size(control%elseif_blocks)
                if (control%elseif_blocks(i)%condition_index <= 0) cycle
                is_condition = node_is_descendant_of(arena, node_index, &
                    [control%elseif_blocks(i)%condition_index])
                if (is_condition) return
            end do
        class default
        end select
    end function is_pointer_state_condition_observation

    subroutine scan_pointer_state_mutations_before(arena, scope_index, &
            declaration_index, pointer_name, scope_indices, observation_statement, &
            assignment_count, assignment_index, nullify_count, nullify_index, &
            has_non_direct_mutation)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index, declaration_index
        character(len=*), intent(in) :: pointer_name
        integer, intent(in) :: scope_indices(:), observation_statement
        integer, intent(out) :: assignment_count, assignment_index
        integer, intent(out) :: nullify_count, nullify_index
        logical, intent(out) :: has_non_direct_mutation
        type(pointer_assignment_query_t) :: assignment
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg, mutation_name
        integer :: i, j, statement_index
        logical :: matches

        assignment_count = 0
        assignment_index = 0
        nullify_count = 0
        nullify_index = 0
        has_non_direct_mutation = .false.
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            call direct_scope_statement_for_node(arena, i, scope_index, &
                statement_index)
            if (statement_index <= 0) cycle
            if (.not. index_precedes(scope_indices, statement_index, &
                observation_statement)) cycle

            select type (node => arena%entries(i)%node)
                type is (pointer_assignment_node)
                assignment = query_pointer_assignment(arena, i)
                if (.not. assignment%found) cycle
                call resolve_identifier_binding(arena, &
                    assignment%pointer_node_index, binding, error_msg)
                matches = .false.
                if (binding%found) then
                    if (binding%declaration_node_index == declaration_index) then
                        matches = same_name(binding%name, pointer_name)
                    end if
                end if
                if (.not. matches) cycle
                assignment_count = assignment_count + 1
                if (assignment_index == 0) assignment_index = i
                if (.not. index_in_list(scope_indices, i)) then
                    has_non_direct_mutation = .true.
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
                    nullify_count = nullify_count + 1
                    nullify_index = i
                    if (.not. index_in_list(scope_indices, i)) then
                        has_non_direct_mutation = .true.
                    end if
                    exit
                end do
            class default
            end select
        end do
    end subroutine scan_pointer_state_mutations_before

    subroutine detect_callback_branch_call(arena, body_indices, pointer_name, query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: pointer_name
        type(procedure_callback_flow_query_t), intent(inout) :: query
        character(len=:), allocatable :: name
        integer, allocatable :: ignored(:)
        logical :: is_call
        integer :: i

        do i = 1, arena%size
            if (.not. node_is_descendant_of(arena, i, body_indices)) cycle
            call get_call_parts(arena, i, name, ignored, is_call)
            if (is_call .and. same_name(name, pointer_name)) then
                query%has_branch_call = .true.
                return
            end if
        end do
    end subroutine detect_callback_branch_call

    logical function signatures_compatible(first, second) result(compatible)
        type(procedure_signature_query_t), intent(in) :: first, second
        integer :: i

        compatible = first%found .and. second%found .and. &
            first%is_function .eqv. second%is_function .and. &
            first%dummy_count == second%dummy_count
        if (.not. compatible) return
        do i = 1, first%dummy_count
            if (first%dummies(i)%type_known .neqv. second%dummies(i)%type_known) return
            if (first%dummies(i)%type_known) then
                if (first%dummies(i)%type_category /= second%dummies(i)%type_category) return
                if (first%dummies(i)%type_kind /= second%dummies(i)%type_kind) return
            end if
            if (first%dummies(i)%rank_known .neqv. second%dummies(i)%rank_known) return
            if (first%dummies(i)%rank_known .and. first%dummies(i)%rank /= second%dummies(i)%rank) return
            if (first%dummies(i)%is_optional .neqv. second%dummies(i)%is_optional) return
            if (first%dummies(i)%is_value .neqv. second%dummies(i)%is_value) return
        end do
        if (first%is_function) then
            if (first%result_category_known .neqv. second%result_category_known) return
            if (first%result_category_known .and. first%result_category /= second%result_category) return
            if (first%result_type_kind /= second%result_type_kind) return
            if (first%result_rank_known .neqv. second%result_rank_known) return
            if (first%result_rank_known .and. first%result_rank /= second%result_rank) return
        end if
    end function signatures_compatible

    subroutine append_callback_target(targets, source)
        type(procedure_callback_target_query_t), allocatable, intent(inout) :: targets(:)
        type(procedure_target_query_t), intent(in) :: source
        type(procedure_callback_target_query_t), allocatable :: extended(:)
        integer :: n

        n = size(targets)
        allocate (extended(n + 1))
        if (n > 0) extended(:n) = targets
        extended(n + 1)%branch_assignment_node_index = source%assignment_node_index
        extended(n + 1)%target_procedure_index = source%target_procedure_index
        extended(n + 1)%target_declaration_index = source%target_declaration_index
        extended(n + 1)%procedure_name = source%procedure_name
        extended(n + 1)%is_resolved = source%is_resolved
        extended(n + 1)%is_unresolved = source%is_unresolved
        extended(n + 1)%is_signature_compatible = .true.
        extended(n + 1)%signature = source%signature
        call move_alloc(extended, targets)
    end subroutine append_callback_target

    subroutine initialize_procedure_signature_query(query)
        type(procedure_signature_query_t), intent(out) :: query

        call set_empty(query%procedure_name)
        call set_empty(query%result_category)
        allocate (query%dummies(0))
    end subroutine initialize_procedure_signature_query

    subroutine initialize_procedure_dummy_query(query)
        type(procedure_dummy_query_t), intent(out) :: query

        call set_empty(query%name)
        call set_empty(query%type_category)
        call set_empty(query%intent)
    end subroutine initialize_procedure_dummy_query

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
            has_non_direct_mutation, has_reassignment)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index, declaration_index
        character(len=*), intent(in) :: pointer_name
        integer, intent(in) :: scope_indices(:)
        integer, intent(out) :: mutation_count, assignment_index
        logical, intent(out) :: has_non_direct_mutation, has_reassignment
        type(pointer_assignment_query_t) :: assignment
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: mutation_name
        integer :: i, j, pointer_assignment_count
        logical :: matches

        mutation_count = 0
        assignment_index = 0
        has_non_direct_mutation = .false.
        pointer_assignment_count = 0
        has_reassignment = .false.
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
                    pointer_assignment_count = pointer_assignment_count + 1
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
        has_reassignment = pointer_assignment_count > 1
    end subroutine find_pointer_mutations

    subroutine collect_reassignment_mutations(arena, scope_index, &
            declaration_index, pointer_name, scope_indices, assignments, &
            has_non_direct_mutation, has_nullify)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index, declaration_index
        character(len=*), intent(in) :: pointer_name
        integer, intent(in) :: scope_indices(:)
        integer, allocatable, intent(out) :: assignments(:)
        logical, intent(out) :: has_non_direct_mutation, has_nullify
        type(pointer_assignment_query_t) :: assignment
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg, mutation_name
        integer :: i, j
        logical :: matches

        allocate (assignments(0))
        has_non_direct_mutation = .false.
        has_nullify = .false.
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (find_enclosing_scope(arena, i) /= scope_index) cycle
            select type (node => arena%entries(i)%node)
                type is (pointer_assignment_node)
                assignment = query_pointer_assignment(arena, i)
                if (.not. assignment%found) cycle
                call resolve_identifier_binding(arena, assignment%pointer_node_index, &
                    binding, error_msg)
                matches = .false.
                if (binding%found) then
                    if (binding%declaration_node_index == declaration_index) then
                        matches = same_name(binding%name, pointer_name)
                    end if
                end if
                if (.not. matches) cycle
                if (index_in_list(scope_indices, i)) then
                    call append_reassignment_index(assignments, i)
                else
                    has_non_direct_mutation = .true.
                end if
                type is (nullify_node)
                if (.not. allocated(node%pointer_indices)) cycle
                do j = 1, size(node%pointer_indices)
                    call procedure_target_name_at(arena, node%pointer_indices(j), &
                        mutation_name)
                    call resolve_name_in_scope(arena, scope_index, mutation_name, &
                        binding, error_msg)
                    if (binding%found) then
                        if (binding%declaration_node_index == declaration_index) then
                            if (same_name(binding%name, pointer_name)) then
                                has_nullify = .true.
                                exit
                            end if
                        end if
                    end if
                end do
            class default
            end select
        end do
    end subroutine collect_reassignment_mutations

    subroutine scan_reassignment_flow(arena, scope_index, pointer_name, &
            has_branch, has_loop, call_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        character(len=*), intent(in) :: pointer_name
        logical, intent(out) :: has_branch, has_loop
        integer, intent(out) :: call_count
        character(len=:), allocatable :: name
        integer, allocatable :: ignored(:)
        logical :: is_call
        integer :: i

        has_branch = .false.
        has_loop = .false.
        call_count = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (find_enclosing_scope(arena, i) /= scope_index) cycle
            select type (node => arena%entries(i)%node)
                type is (if_node)
                has_branch = .true.
                type is (select_case_node)
                has_branch = .true.
                type is (select_type_node)
                has_branch = .true.
                type is (where_node)
                has_branch = .true.
                type is (where_stmt_node)
                has_branch = .true.
                type is (forall_node)
                has_branch = .true.
                type is (do_loop_node)
                has_loop = .true.
                type is (do_while_node)
                has_loop = .true.
                class default
            end select
            call get_call_parts(arena, i, name, ignored, is_call)
            if (is_call .and. same_name(name, pointer_name)) call_count = call_count + 1
        end do
    end subroutine scan_reassignment_flow

    logical function reassignment_target_is_supported(target) result(supported)
        type(procedure_target_query_t), intent(in) :: target
        type(procedure_dummy_query_t) :: dummy

        supported = .false.
        if (.not. target%found) return
        if (.not. target%is_resolved) return
        if (target%binding_kind /= BINDING_FUNCTION) return
        if (.not. target%signature%found) return
        if (.not. target%signature%is_function) return
        if (.not. target%signature%result_category_known) return
        if (.not. same_name(target%signature%result_category, 'real')) return
        if (.not. target%signature%result_kind_known) return
        if (target%signature%result_kind_value /= 8) return
        if (.not. target%signature%result_rank_known) return
        if (target%signature%result_rank /= 0) return
        if (target%signature%dummy_count /= 1) return
        if (.not. allocated(target%signature%dummies)) return
        if (size(target%signature%dummies) /= 1) return
        dummy = target%signature%dummies(1)
        if (.not. dummy%type_known) return
        if (.not. dummy%category_known) return
        if (.not. same_name(dummy%type_category, 'real')) return
        if (.not. dummy%kind_known) return
        if (dummy%kind_value /= 8) return
        if (.not. dummy%rank_known) return
        if (dummy%rank /= 0) return
        if (.not. dummy%has_intent) return
        if (.not. same_name(dummy%intent, 'in')) return
        if (dummy%is_optional .or. dummy%is_value) return
        supported = .true.
    end function reassignment_target_is_supported

    subroutine append_reassignment_index(values, value)
        integer, allocatable, intent(inout) :: values(:)
        integer, intent(in) :: value
        integer, allocatable :: grown(:)
        integer :: n

        n = size(values)
        allocate (grown(n + 1))
        if (n > 0) grown(:n) = values
        grown(n + 1) = value
        call move_alloc(grown, values)
    end subroutine append_reassignment_index

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

    logical function is_procedure_pointer_dummy(arena, scope_index, &
            declaration_index) result(is_dummy)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index, declaration_index
        type(declaration_query_t) :: declaration
        character(len=:), allocatable :: parameter_name
        integer :: i

        is_dummy = .false.
        if (.not. arena%has_node_at(scope_index)) return
        declaration = query_declaration(arena, declaration_index)
        if (.not. declaration%found) return
        select type (scope => arena%entries(scope_index)%node)
            type is (function_def_node)
            if (allocated(scope%param_indices)) then
                do i = 1, size(scope%param_indices)
                    if (scope%param_indices(i) == declaration_index) then
                        is_dummy = .true.
                        return
                    end if
                    call procedure_target_name_at(arena, &
                        scope%param_indices(i), parameter_name)
                    if (same_name(parameter_name, declaration%name)) then
                        is_dummy = .true.
                        return
                    end if
                end do
            end if
            type is (subroutine_def_node)
            if (allocated(scope%param_indices)) then
                do i = 1, size(scope%param_indices)
                    if (scope%param_indices(i) == declaration_index) then
                        is_dummy = .true.
                        return
                    end if
                    call procedure_target_name_at(arena, &
                        scope%param_indices(i), parameter_name)
                    if (same_name(parameter_name, declaration%name)) then
                        is_dummy = .true.
                        return
                    end if
                end do
            end if
        class default
        end select
    end function is_procedure_pointer_dummy

    subroutine procedure_target_name_at(arena, node_index, name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name

        call set_empty(name)
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (identifier_node)
            if (allocated(node%name)) name = node%name
            type is (parameter_declaration_node)
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
        query%is_abstract_type = query%is_derived .and. &
            derived_storage_type_is_abstract(arena, node_index)
    end subroutine set_derived_storage_facts

    logical function derived_storage_type_is_abstract(arena, node_index) &
            result(is_abstract)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(declaration_query_t) :: declaration
        type(derived_type_query_t) :: derived
        integer :: derived_index

        is_abstract = .false.
        declaration = query_declaration(arena, node_index)
        if (.not. declaration%found) return
        derived_index = find_derived_type_by_name(arena, &
            derived_type_name_from_spec(declaration%type_name))
        if (derived_index <= 0) return
        derived = query_derived_type(arena, derived_index)
        if (derived%found) is_abstract = contains_word(derived%attribute_clause, &
            'abstract')
    end function derived_storage_type_is_abstract

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
            events(count)%sequence_index = count
        end do
        call annotate_ownership_identity(arena, events)
    end function query_ownership_events

    subroutine annotate_ownership_identity(arena, events)
        type(ast_arena_t), intent(in) :: arena
        type(ownership_event_query_t), intent(inout) :: events(:)
        type(ownership_dynamic_flow_t), allocatable :: flow(:)
        integer :: i, flow_count

        allocate (flow(0))
        flow_count = 0
        do i = 1, size(events)
            call annotate_ownership_event(arena, events, i, flow, flow_count)
        end do
    end subroutine annotate_ownership_identity

    subroutine annotate_ownership_event(arena, events, event_index, flow, &
            flow_count)
        type(ast_arena_t), intent(in) :: arena
        type(ownership_event_query_t), intent(inout) :: events(:)
        integer, intent(in) :: event_index
        type(ownership_dynamic_flow_t), allocatable, intent(inout) :: flow(:)
        integer, intent(inout) :: flow_count
        type(ownership_event_query_t) :: event
        logical :: source_known, destination_known
        character(len=:), allocatable :: source_type, destination_type

        event = events(event_index)
        if (event%event_kind == OWNERSHIP_EVENT_ASSIGNMENT .and. &
                event%polymorphic_assignment%found) then
            if (event%polymorphic_assignment%is_replayable) then
                event%has_dynamic_type_boundary = .false.
                call set_event_dynamic_type(event%source_dynamic_type, &
                    event%polymorphic_assignment%dynamic_type, .true.)
                call set_event_dynamic_type(event%destination_dynamic_type, &
                    event%polymorphic_assignment%dynamic_type, .true.)
                event%is_source_dynamic_type_known = .true.
                event%is_destination_dynamic_type_known = .true.
            else
                event%has_dynamic_type_boundary = .true.
                event%is_destination_dynamic_type_known = .false.
                call set_event_dynamic_type(event%destination_dynamic_type, '', &
                    .false.)
            end if
            events(event_index) = event
            return
        end if
        if (event%event_kind == OWNERSHIP_EVENT_ASSIGNMENT .and. &
                event%is_deep_assignment) then
            event%has_dynamic_type_boundary = .true.
            event%is_source_dynamic_type_known = .false.
            event%is_destination_dynamic_type_known = .false.
            events(event_index) = event
            return
        end if
        if (event%event_kind == OWNERSHIP_EVENT_ALLOCATE .and. &
                event%polymorphic_allocation%found) then
            event%has_dynamic_type_boundary = .true.
            event%is_source_dynamic_type_known = .false.
            event%is_destination_dynamic_type_known = .false.
            events(event_index) = event
            return
        end if
        if (.not. ownership_identity_supported(event)) then
            event%has_dynamic_type_boundary = .true.
            event%is_source_dynamic_type_known = .false.
            event%is_destination_dynamic_type_known = .false.
            events(event_index) = event
            return
        end if
        source_known = event%is_source_dynamic_type_known
        destination_known = event%is_destination_dynamic_type_known
        call set_empty(source_type)
        if (allocated(event%source_dynamic_type)) source_type = &
            event%source_dynamic_type
        call set_empty(destination_type)
        if (allocated(event%destination_dynamic_type)) destination_type = &
            event%destination_dynamic_type
        if (.not. allocated(source_type)) source_known = .false.
        if (source_known) then
            if (len_trim(source_type) == 0) source_known = .false.
        end if
        if (.not. allocated(destination_type)) destination_known = .false.
        if (destination_known) then
            if (len_trim(destination_type) == 0) destination_known = .false.
        end if
        if (ownership_event_has_control_boundary(arena, event%node_index)) then
            event%has_dynamic_type_boundary = .true.
            flow_count = 0
            deallocate (flow)
            allocate (flow(0))
        end if

        if (event%source_declaration_index > 0) then
            call get_flow_type(flow, flow_count, event%source_declaration_index, &
                source_known, source_type)
        end if
        select case (event%event_kind)
        case (OWNERSHIP_EVENT_ALLOCATE)
            if (.not. destination_known .and. source_known .and. &
                .not. event%has_dynamic_type_boundary) then
                destination_known = .true.
                destination_type = source_type
            end if
            call set_event_dynamic_type(event%source_dynamic_type, source_type, &
                source_known)
            call set_event_dynamic_type(event%destination_dynamic_type, &
                destination_type, destination_known)
            event%is_source_dynamic_type_known = source_known
            event%is_destination_dynamic_type_known = destination_known
            if (event%destination_declaration_index > 0 .and. &
                .not. event%has_dynamic_type_boundary) then
                call set_flow_type(flow, flow_count, &
                    event%destination_declaration_index, destination_known, &
                    destination_type)
            end if
        case (OWNERSHIP_EVENT_DEALLOCATE)
            call set_event_dynamic_type(event%destination_dynamic_type, '', .false.)
            event%is_destination_dynamic_type_known = .false.
            if (event%destination_declaration_index > 0) then
                call set_flow_type(flow, flow_count, &
                    event%destination_declaration_index, .false., '')
            end if
        case (OWNERSHIP_EVENT_MOVE_ALLOC)
            if (.not. source_known) event%has_dynamic_type_boundary = &
                event%source_storage_resolved .and. &
                event%source_storage_class /= STORAGE_POINTER .and. &
                event%source_storage_class /= STORAGE_MODULE .and. &
                event%source_storage_class /= STORAGE_SAVE .and. &
                event%source_storage_class /= STORAGE_COMMON
            if (event%is_refused) event%has_dynamic_type_boundary = .true.
            if (event%destination_storage_resolved .and. &
                .not. event%is_refused .and. &
                .not. event%has_dynamic_type_boundary) then
                destination_known = source_known
                destination_type = source_type
            else if (.not. destination_known) then
                destination_known = .false.
                call set_empty(destination_type)
            end if
            call set_event_dynamic_type(event%source_dynamic_type, source_type, &
                source_known)
            call set_event_dynamic_type(event%destination_dynamic_type, &
                destination_type, destination_known)
            event%is_source_dynamic_type_known = source_known
            event%is_destination_dynamic_type_known = destination_known
            if (.not. event%is_refused) then
                if (event%source_declaration_index > 0) then
                    call set_flow_type(flow, flow_count, &
                        event%source_declaration_index, .false., '')
                end if
                if (event%destination_declaration_index > 0) then
                    call set_flow_type(flow, flow_count, &
                        event%destination_declaration_index, destination_known, &
                        destination_type)
                end if
            end if
        case (OWNERSHIP_EVENT_ASSIGNMENT)
            if (event%destination_storage_resolved .and. &
                event%destination_declaration_index > 0 .and. &
                .not. event%is_refused .and. &
                .not. event%has_dynamic_type_boundary) then
                if (event%destination_storage_class /= STORAGE_POINTER .and. &
                    event%destination_storage_class /= STORAGE_MODULE .and. &
                    event%destination_storage_class /= STORAGE_SAVE .and. &
                    event%destination_storage_class /= STORAGE_COMMON) then
                    if (event%destination_is_polymorphic) then
                        destination_known = source_known
                        destination_type = source_type
                    end if
                    call set_flow_type(flow, flow_count, &
                        event%destination_declaration_index, destination_known, &
                        destination_type)
                end if
            end if
            call set_event_dynamic_type(event%source_dynamic_type, source_type, &
                source_known)
            call set_event_dynamic_type(event%destination_dynamic_type, &
                destination_type, destination_known)
            event%is_source_dynamic_type_known = source_known
            event%is_destination_dynamic_type_known = destination_known
        end select
        events(event_index) = event
    end subroutine annotate_ownership_event

    logical function ownership_identity_supported(event) result(supported)
        type(ownership_event_query_t), intent(in) :: event

        supported = .false.
        if (event%is_refused) return
        if (.not. event%destination_storage_resolved) return
        if (event%destination_path%is_array_element .or. &
                event%destination_path%is_array_section) return
        select case (event%event_kind)
        case (OWNERSHIP_EVENT_ALLOCATE, OWNERSHIP_EVENT_DEALLOCATE)
            supported = .true.
        case (OWNERSHIP_EVENT_MOVE_ALLOC)
            supported = event%source_storage_resolved .and. &
                .not. event%source_path%is_array_element .and. &
                .not. event%source_path%is_array_section
        case (OWNERSHIP_EVENT_ASSIGNMENT)
            supported = event%assignment_kind == &
                OWNERSHIP_ASSIGNMENT_WHOLE_ALLOCATABLE
        end select
    end function ownership_identity_supported

    logical function ownership_event_has_control_boundary(arena, node_index) &
            result(has_boundary)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: current, guard

        has_boundary = .false.
        current = node_index
        guard = 0
        do while (current > 0 .and. arena%has_node_at(current))
            select type (node => arena%entries(current)%node)
                type is (if_node)
                has_boundary = .true.
                return
                type is (do_loop_node)
                has_boundary = .true.
                return
                type is (do_while_node)
                has_boundary = .true.
                return
                type is (select_case_node)
                has_boundary = .true.
                return
                type is (select_type_node)
                has_boundary = .true.
                return
                type is (type_guard_block_node)
                has_boundary = .true.
                return
            class default
            end select
            current = arena%entries(current)%parent_index
            guard = guard + 1
            if (guard > arena%size) exit
        end do
    end function ownership_event_has_control_boundary

    subroutine get_flow_type(flow, flow_count, declaration_index, is_known, type_name)
        type(ownership_dynamic_flow_t), intent(in) :: flow(:)
        integer, intent(in) :: flow_count, declaration_index
        logical, intent(out) :: is_known
        character(len=:), allocatable, intent(out) :: type_name
        integer :: i

        do i = 1, flow_count
            if (flow(i)%declaration_index /= declaration_index) cycle
            is_known = flow(i)%is_known
            call set_empty(type_name)
            if (is_known .and. allocated(flow(i)%dynamic_type)) then
                type_name = flow(i)%dynamic_type
            end if
            return
        end do
    end subroutine get_flow_type

    subroutine set_flow_type(flow, flow_count, declaration_index, is_known, &
            type_name)
        type(ownership_dynamic_flow_t), allocatable, intent(inout) :: flow(:)
        integer, intent(inout) :: flow_count
        integer, intent(in) :: declaration_index
        logical, intent(in) :: is_known
        character(len=*), intent(in) :: type_name
        type(ownership_dynamic_flow_t), allocatable :: grown(:)
        integer :: i

        if (declaration_index <= 0) return
        do i = 1, flow_count
            if (flow(i)%declaration_index /= declaration_index) cycle
            flow(i)%is_known = is_known
            call set_empty(flow(i)%dynamic_type)
            if (is_known) flow(i)%dynamic_type = type_name
            return
        end do
        allocate (grown(flow_count + 1))
        if (flow_count > 0) grown(1:flow_count) = flow
        grown(flow_count + 1)%declaration_index = declaration_index
        grown(flow_count + 1)%is_known = is_known
        call set_empty(grown(flow_count + 1)%dynamic_type)
        if (is_known) grown(flow_count + 1)%dynamic_type = type_name
        call move_alloc(grown, flow)
        flow_count = flow_count + 1
    end subroutine set_flow_type

    subroutine set_event_dynamic_type(value, type_name, is_known)
        character(len=:), allocatable, intent(inout) :: value
        character(len=*), intent(in) :: type_name
        logical, intent(in) :: is_known

        if (allocated(value)) deallocate (value)
        if (is_known) then
            allocate (character(len=len(type_name)) :: value)
            value = type_name
        else
            allocate (character(len=0) :: value)
        end if
    end subroutine set_event_dynamic_type

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
        query%is_abstract_type = storage%is_abstract_type
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

    function query_polymorphic_assignment(arena, assignment_node_index) &
            result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: assignment_node_index
        type(polymorphic_assignment_query_t) :: query

        ! Keep the function form for source compatibility. Consumers that
        ! cross compiler/runtime boundaries should use the out-argument form.
        call query_polymorphic_assignment_into(arena, assignment_node_index, query)
    end function query_polymorphic_assignment

    subroutine query_polymorphic_assignment_into(arena, assignment_node_index, query)
        !! Return one bounded polymorphic allocatable-assignment fact.
        !!
        !! The source and destination are resolved through the existing
        !! storage/component queries.  A concrete source may be assigned to
        !! its declared polymorphic base (or CLASS(*)); in that case the
        !! source type is the dynamic type that intrinsic assignment acquires
        !! and copies.  Polymorphic sources, aliases, global state, control
        !! flow, and incompatible declared types remain explicit boundaries.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: assignment_node_index
        type(polymorphic_assignment_query_t), intent(out) :: query
        type(storage_query_t) :: source_storage, destination_storage
        type(assignment_node) :: assignment
        character(len=:), allocatable :: source_type, destination_type
        integer :: source_type_index, destination_type_index
        logical :: compatible

        call initialize_polymorphic_assignment_query(query)
        if (.not. arena%has_node_at(assignment_node_index)) return
        select type (node => arena%entries(assignment_node_index)%node)
            type is (assignment_node)
            assignment = node
        class default
            return
        end select
        if (assignment%is_keyword_argument) return

        query%assignment_node_index = assignment_node_index
        query%source_node_index = assignment%value_index
        query%destination_node_index = assignment%target_index
        query%source_path = ownership_path(arena, assignment%value_index)
        query%destination_path = ownership_path(arena, assignment%target_index)
        source_storage = assignment_operand_storage_query(arena, &
            assignment%value_index)
        destination_storage = assignment_operand_storage_query(arena, &
            assignment%target_index)
        if (.not. source_storage%found .or. .not. destination_storage%found) return
        if (.not. destination_storage%is_allocatable .or. &
                .not. destination_storage%is_polymorphic .or. &
                .not. destination_storage%is_derived) return
        if (.not. source_storage%is_derived) return

        query%found = .true.
        query%source_declaration_index = source_storage%declaration_index
        query%destination_declaration_index = destination_storage%declaration_index
        query%source_declared_type = source_storage%type_name
        query%destination_declared_type = destination_storage%type_name
        query%is_source_polymorphic = source_storage%is_polymorphic
        query%is_source_concrete = source_storage%is_concrete_derived
        query%is_destination_polymorphic = destination_storage%is_polymorphic
        query%has_global_mutable_state = storage_has_global_state(source_storage) .or. &
            storage_has_global_state(destination_storage)
        query%has_unresolved_alias = polymorphic_assignment_has_alias(arena, &
            assignment%value_index, query%source_path, source_storage) .or. &
            polymorphic_assignment_has_alias(arena, assignment%target_index, &
            query%destination_path, destination_storage)
        query%has_control_flow_boundary = ownership_event_has_control_boundary( &
            arena, assignment_node_index)

        source_type = derived_type_name_from_spec(source_storage%type_name)
        destination_type = derived_type_name_from_spec(destination_storage%type_name)
        compatible = .false.
        if (destination_storage%is_unlimited_polymorphic) then
            compatible = .true.
        else if (len_trim(source_type) > 0 .and. len_trim(destination_type) > 0) then
            source_type_index = find_derived_type_by_name(arena, source_type)
            destination_type_index = find_derived_type_by_name(arena, destination_type)
            if (same_name(source_type, destination_type)) then
                compatible = .true.
            else if (source_type_index > 0 .and. destination_type_index > 0) then
                compatible = type_extends(arena, source_type_index, &
                    destination_type_index)
            end if
        end if
        query%has_type_mismatch = .not. compatible

        if (query%is_source_concrete .and. len_trim(source_type) > 0) then
            source_type_index = find_derived_type_by_name(arena, source_type)
            query%has_owned_components = derived_type_has_owned_components(arena, &
                source_type_index, 0)
            if (compatible) then
                query%dynamic_type = source_type
            end if
        end if
        query%is_dynamic_type_known = query%is_source_concrete .and. compatible .and. &
            .not. query%has_global_mutable_state .and. &
            .not. query%has_unresolved_alias .and. &
            .not. query%has_control_flow_boundary
        query%is_refused = query%is_source_polymorphic .or. &
            query%has_type_mismatch .or. query%has_global_mutable_state .or. &
            query%has_unresolved_alias .or. query%has_control_flow_boundary
        query%is_replayable = query%is_dynamic_type_known .and. &
            .not. query%is_refused
    end subroutine query_polymorphic_assignment_into

    subroutine initialize_polymorphic_assignment_query(query)
        type(polymorphic_assignment_query_t), intent(out) :: query

        call set_empty(query%source_declared_type)
        call set_empty(query%destination_declared_type)
        call set_empty(query%dynamic_type)
        call initialize_component_path_query(query%source_path)
        call initialize_component_path_query(query%destination_path)
    end subroutine initialize_polymorphic_assignment_query

    function assignment_operand_storage_query(arena, node_index) result(storage)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(storage_query_t) :: storage

        storage = query_designator_storage(arena, node_index)
    end function assignment_operand_storage_query

    logical function polymorphic_assignment_has_alias(arena, node_index, path, &
            storage) result(has_alias)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(component_path_query_t), intent(in) :: path
        type(storage_query_t), intent(in) :: storage
        type(storage_query_t) :: base_storage

        has_alias = is_associate_selector_node(arena, node_index) .or. &
            storage%is_pointer .or. storage%is_target
        if (.not. path%found) return
        if (path%base_node_index <= 0) return
        if (path%base_node_index == node_index) return
        base_storage = query_designator_storage(arena, path%base_node_index)
        if (.not. base_storage%found) then
            has_alias = .true.
            return
        end if
        has_alias = has_alias .or. base_storage%is_pointer .or. &
            base_storage%is_target .or. base_storage%is_polymorphic
    end function polymorphic_assignment_has_alias

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
        if (query%receiver_node_index > 0) then
            query%receiver_path = query_component_path(arena, &
                query%receiver_node_index)
        else
            call resolve_source_receiver_path(arena, call_node_index, &
                query%receiver_name, query%receiver_path)
        end if

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
            query%dispatch_target_declaring_type_indices = &
                resolution%dispatch_target_declaring_type_indices
            query%dispatch_target_is_inherited = &
                resolution%dispatch_target_is_inherited
            query%dispatch_target_inheritance_depth = &
                resolution%dispatch_target_inheritance_depth
        end if
        query%is_resolved = .not. query%is_generic .and. &
            .not. query%is_ambiguous .and. .not. query%is_deferred .and. &
            len_trim(query%implementation) > 0
        if (.not. query%is_resolved .and. .not. query%is_generic .and. &
            .not. query%is_deferred) query%is_unresolved = .true.
    end function query_type_bound_call

    subroutine resolve_source_receiver_path(arena, call_node_index, name, path)
        !! Resolve component identity for an explicit CALL receiver whose
        !! parser representation is source-only (for example
        !! ``call outer%inner%apply()``).  No AST component nodes are
        !! manufactured: component_node_indices stay zero, and rank/section
        !! facts that depend on the missing designator AST stay at their
        !! initialized unknown values.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        character(len=*), intent(in) :: name
        type(component_path_query_t), intent(inout) :: path
        type(declaration_binding_t) :: binding
        type(declaration_query_t) :: base_declaration, component_declaration
        type(storage_query_t) :: base_storage, terminal_storage
        character(len=:), allocatable :: base_name, remaining, component_name
        character(len=:), allocatable :: type_name, error_msg
        integer :: separator, start, derived_index, component_index

        if (len_trim(name) == 0) return
        path%found = .false.
        separator = top_level_percent(name)
        if (separator <= 0) return
        base_name = trim(name(:separator - 1))
        remaining = trim(name(separator + 1:))
        if (len_trim(base_name) == 0 .or. len_trim(remaining) == 0) return

        call resolve_name_at_node(arena, call_node_index, &
            receiver_object_name(base_name), binding, error_msg)
        if (.not. binding%found) return
        if (binding%binding_kind == BINDING_ASSOCIATE_NAME) return
        base_declaration = query_declaration(arena, &
            binding%declaration_node_index)
        if (.not. base_declaration%found) return
        base_storage = query_storage(arena, base_declaration%node_index)
        if (.not. base_storage%found) return

        type_name = declared_type_name(base_declaration%type_name)
        start = 1
        do
            separator = top_level_percent(remaining(start:))
            if (separator <= 0) then
                component_name = trim(remaining(start:))
            else
                if (separator == 1) return
                component_name = trim(remaining(start:start + separator - 2))
            end if
            component_name = receiver_object_name(component_name)
            if (len_trim(component_name) == 0) return

            derived_index = find_derived_type_by_name(arena, type_name)
            if (derived_index <= 0) return
            component_index = find_component_declaration_in_hierarchy(arena, &
                derived_index, component_name)
            if (component_index <= 0) return
            component_declaration = query_declaration(arena, component_index)
            if (.not. component_declaration%found) return
            terminal_storage = query_storage(arena, component_index)
            if (.not. terminal_storage%found) return

            call append_dispatch_character(path%component_names, &
                component_declaration%name)
            call append_dispatch_integer(path%component_node_indices, 0)
            call append_dispatch_integer(path%component_declaration_indices, &
                component_index)
            type_name = declared_type_name(component_declaration%type_name)
            if (separator <= 0) exit
            start = start + separator
            if (start > len(remaining)) return
        end do

        path%storage_class = terminal_storage%storage_class
        path%base_storage_class = base_storage%storage_class
        path%is_derived = terminal_storage%is_derived
        path%is_concrete_derived = terminal_storage%is_concrete_derived
        path%is_abstract_type = terminal_storage%is_abstract_type
        path%is_allocatable = terminal_storage%is_allocatable
        path%is_pointer = terminal_storage%is_pointer
        path%is_polymorphic = terminal_storage%is_polymorphic
        path%is_unlimited_polymorphic = terminal_storage%is_unlimited_polymorphic
        path%is_array_element = .false.
        path%is_array_section = .false.
        path%base_rank = -1
        path%rank = -1
        path%found = size(path%component_declaration_indices) > 0
    end subroutine resolve_source_receiver_path

    integer function top_level_percent(text) result(position)
        character(len=*), intent(in) :: text
        integer :: i, depth
        character :: ch

        position = 0
        depth = 0
        do i = 1, len_trim(text)
            ch = text(i:i)
            select case (ch)
            case ('(', '[')
                depth = depth + 1
            case (')', ']')
                if (depth > 0) depth = depth - 1
            case ('%')
                if (depth == 0) then
                    position = i
                    return
                end if
            end select
        end do
    end function top_level_percent

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
            ! An ABSTRACT descendant may provide a concrete implementation,
            ! but it is not an instantiable runtime dispatch arm. Consumers
            ! use this array as the set of possible concrete dynamic types.
            if (target%is_abstract_type) cycle
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
        allocate (query%dispatch_target_declaring_type_indices(0))
        allocate (query%dispatch_target_is_inherited(0))
        allocate (query%dispatch_target_inheritance_depth(0))
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
            call resolve_name_at_node(arena, call_node_index, &
                receiver_object_name(name), binding, error_msg)
            return
        end if

        base_name = receiver_object_name(name(:separator - 1))
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
            component_name = receiver_object_name(component_name)
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

    function receiver_object_name(designator) result(name)
        character(len=*), intent(in) :: designator
        character(len=:), allocatable :: name
        integer :: separator

        ! Explicit CALL nodes retain the receiver as source text.  Strip only
        ! its subscript so name resolution can recover the declared object;
        ! the public receiver_name remains the original indexed designator.
        name = trim(designator)
        separator = index(name, '(')
        if (separator <= 0) separator = index(name, '[')
        if (separator > 1) name = trim(name(:separator - 1))
    end function receiver_object_name

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
        allocate (query%dispatch_target_declaring_type_indices(0))
        allocate (query%dispatch_target_is_inherited(0))
        allocate (query%dispatch_target_inheritance_depth(0))
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

        allocate (int_tmp(n + 1))
        if (n > 0) int_tmp(:n) = &
            query%dispatch_target_declaring_type_indices
        int_tmp(n + 1) = target%declaring_type_index
        call move_alloc(int_tmp, query%dispatch_target_declaring_type_indices)

        allocate (logical_tmp(n + 1))
        if (n > 0) logical_tmp(:n) = query%dispatch_target_is_inherited
        logical_tmp(n + 1) = target%is_inherited
        call move_alloc(logical_tmp, query%dispatch_target_is_inherited)

        allocate (int_tmp(n + 1))
        if (n > 0) int_tmp(:n) = query%dispatch_target_inheritance_depth
        int_tmp(n + 1) = inheritance_distance(arena, type_index, &
            target%declaring_type_index)
        call move_alloc(int_tmp, query%dispatch_target_inheritance_depth)
    end subroutine append_dispatch_target

    integer function inheritance_distance(arena, derived_type_index, &
            declaring_type_index) result(distance)
        !! Return the source-backed EXTENDS depth to an effective binding.
        !!
        !! A negative result means that the declaring type is not reachable
        !! through the queried type's parent chain.  Dispatch targets are
        !! appended only after normal binding resolution, so this should not
        !! occur for a resolved target; retaining the sentinel keeps the
        !! provenance explicit if the arena is incomplete.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: derived_type_index
        integer, intent(in) :: declaring_type_index
        type(derived_type_query_t) :: derived
        integer :: current_index, parent_index

        distance = -1
        if (.not. arena%has_node_at(derived_type_index)) return
        if (.not. arena%has_node_at(declaring_type_index)) return

        current_index = derived_type_index
        distance = 0
        do
            if (current_index == declaring_type_index) return
            if (distance >= arena%size) then
                distance = -1
                return
            end if
            derived = query_derived_type(arena, current_index)
            if (.not. derived%found) then
                distance = -1
                return
            end if
            parent_index = find_derived_type_by_name(arena, &
                derived%extends_parent)
            if (parent_index <= 0) then
                distance = -1
                return
            end if
            current_index = parent_index
            distance = distance + 1
        end do
    end function inheritance_distance

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
            is_ownership_event = is_potential_reallocation_assignment(arena, index) .or. &
                is_deep_assignment_event(arena, index)
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

    logical function is_deep_assignment_event(arena, index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical :: has_owned_components, has_global_state, has_alias

        call classify_deep_assignment(arena, index, is_deep_assignment_event, &
            has_owned_components, has_global_state, has_alias)
    end function is_deep_assignment_event

    subroutine classify_deep_assignment(arena, index, is_deep, &
            has_owned_components, has_global_state, has_alias)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        logical, intent(out) :: is_deep, has_owned_components
        logical, intent(out) :: has_global_state, has_alias
        type(storage_query_t) :: lhs_storage, rhs_storage
        character(len=:), allocatable :: lhs_type, rhs_type

        is_deep = .false.
        has_owned_components = .false.
        has_global_state = .false.
        has_alias = .false.
        if (.not. arena%has_node_at(index)) return
        select type (node => arena%entries(index)%node)
            type is (assignment_node)
            if (node%is_keyword_argument) return
            call assignment_operand_storage(arena, node%target_index, lhs_storage)
            call assignment_operand_storage(arena, node%value_index, rhs_storage)
            if (.not. lhs_storage%found .or. .not. rhs_storage%found) return
            has_global_state = storage_has_global_state(lhs_storage) .or. &
                storage_has_global_state(rhs_storage)
            has_alias = assignment_operand_has_unsafe_alias(arena, &
                node%target_index, lhs_storage) .or. &
                assignment_operand_has_unsafe_alias(arena, node%value_index, &
                rhs_storage)
            if (lhs_storage%is_array_element .or. lhs_storage%is_array_section .or. &
                rhs_storage%is_array_element .or. rhs_storage%is_array_section) return
            if (.not. lhs_storage%is_concrete_derived .or. &
                .not. rhs_storage%is_concrete_derived) return
            lhs_type = derived_type_name_from_spec(lhs_storage%type_name)
            rhs_type = derived_type_name_from_spec(rhs_storage%type_name)
            if (len_trim(lhs_type) == 0 .or. .not. same_name(lhs_type, rhs_type)) return
            has_owned_components = derived_type_has_owned_components(arena, &
                find_derived_type_by_name(arena, lhs_type), 0)
            is_deep = has_owned_components
        class default
        end select
    end subroutine classify_deep_assignment

    subroutine assignment_operand_storage(arena, node_index, storage)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(storage_query_t), intent(out) :: storage
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg

        storage = query_storage(arena, node_index)
        if (storage%found) return
        call resolve_identifier_binding(arena, node_index, binding, error_msg)
        if (binding%found) storage = query_storage(arena, &
            binding%declaration_node_index)
    end subroutine assignment_operand_storage

    logical function storage_has_global_state(storage)
        type(storage_query_t), intent(in) :: storage

        storage_has_global_state = storage%found .and. &
            (storage%is_module_state .or. storage%is_save_state .or. &
            storage%is_common_state)
    end function storage_has_global_state

    logical function assignment_operand_has_unsafe_alias(arena, node_index, storage)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(storage_query_t), intent(in) :: storage

        assignment_operand_has_unsafe_alias = is_associate_selector_node(arena, &
            node_index) .or. storage%is_pointer .or. storage%is_target
    end function assignment_operand_has_unsafe_alias

    recursive logical function derived_type_has_owned_components(arena, &
            type_index, depth) result(has_owned)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: type_index, depth
        type(derived_type_query_t) :: derived
        type(declaration_query_t) :: component
        integer :: i, nested_index, parent_index
        character(len=:), allocatable :: component_type

        has_owned = .false.
        if (depth > arena%size) return
        derived = query_derived_type(arena, type_index)
        if (.not. derived%found) return
        do i = 1, size(derived%component_indices)
            component = query_declaration(arena, derived%component_indices(i))
            if (.not. component%found) cycle
            if (component%is_allocatable) then
                has_owned = .true.
                return
            end if
            if (component%is_pointer .or. &
                .not. is_derived_type_spec(component%type_name)) cycle
            component_type = derived_type_name_from_spec(component%type_name)
            nested_index = find_derived_type_by_name(arena, component_type)
            if (nested_index > 0) then
                if (derived_type_has_owned_components(arena, nested_index, &
                    depth + 1)) then
                    has_owned = .true.
                    return
                end if
            end if
        end do
        if (len_trim(derived%extends_parent) == 0) return
        parent_index = find_derived_type_by_name(arena, derived%extends_parent)
        if (parent_index > 0) has_owned = &
            derived_type_has_owned_components(arena, parent_index, depth + 1)
    end function derived_type_has_owned_components

    function ownership_event(arena, index) result(event)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: index
        type(ownership_event_query_t) :: event
        logical :: is_deep, has_owned_components, has_global_state, has_alias

        allocate (event%object_indices(0))
        allocate (event%shape_expr_indices(0))
        call initialize_component_path_query(event%owner_path)
        call initialize_component_path_query(event%source_path)
        call initialize_component_path_query(event%destination_path)
        call initialize_component_path_query(event%lhs_owner_path)
        call initialize_component_path_query(event%rhs_owner_path)
        call initialize_polymorphic_allocation_query(event%polymorphic_allocation)
        call initialize_polymorphic_assignment_query(event%polymorphic_assignment)
        call set_empty(event%source_dynamic_type)
        call set_empty(event%destination_dynamic_type)
        event%found = .true.
        event%node_index = index
        select type (node => arena%entries(index)%node)
            type is (allocate_statement_node)
            event%event_kind = OWNERSHIP_EVENT_ALLOCATE
            event%owner_state_before = OWNERSHIP_STATE_UNALLOCATED
            event%owner_state_after = OWNERSHIP_STATE_ALLOCATED
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
            event%owner_state_before = OWNERSHIP_STATE_ALLOCATED
            event%owner_state_after = OWNERSHIP_STATE_UNALLOCATED
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
                    event%source_state_after = OWNERSHIP_STATE_UNALLOCATED
                    event%destination_state_after = OWNERSHIP_STATE_SAME_AS_SOURCE
                    event%has_implicit_destination_deallocation = .true.
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
            call classify_deep_assignment(arena, index, is_deep, &
                has_owned_components, has_global_state, has_alias)
            event%event_kind = OWNERSHIP_EVENT_ASSIGNMENT
            call query_polymorphic_assignment_into(arena, index, &
                event%polymorphic_assignment)
            event%is_deep_assignment = is_deep
            event%has_owned_components = has_owned_components
            event%has_global_mutable_state = has_global_state
            event%has_unresolved_alias = has_alias
            event%is_refused = has_global_state .or. has_alias
            if (event%polymorphic_assignment%found) then
                event%has_global_mutable_state = event%has_global_mutable_state .or. &
                    event%polymorphic_assignment%has_global_mutable_state
                event%has_unresolved_alias = event%has_unresolved_alias .or. &
                    event%polymorphic_assignment%has_unresolved_alias
                event%is_refused = event%is_refused .or. &
                    event%polymorphic_assignment%is_refused
            end if
            if (is_potential_reallocation_assignment(arena, index)) then
                event%assignment_kind = OWNERSHIP_ASSIGNMENT_WHOLE_ALLOCATABLE
                event%reallocation_kind = OWNERSHIP_REALLOCATION_POTENTIAL
                event%is_potential_automatic_reallocation = .true.
                event%owner_state_after = OWNERSHIP_STATE_ALLOCATED
                event%has_potential_implicit_reallocation = .true.
            else if (is_deep) then
                event%assignment_kind = OWNERSHIP_ASSIGNMENT_DEEP_DERIVED
            end if
            event%source_path = ownership_path(arena, node%value_index)
            event%destination_path = ownership_path(arena, node%target_index)
            event%owner_path = event%destination_path
            event%rhs_owner_path = event%source_path
            event%lhs_owner_path = event%destination_path
            event%rhs_rank = expression_rank(arena, node%value_index)
            event%lhs_rank = expression_rank(arena, node%target_index)
        class default
        end select
        call populate_ownership_event_storage(arena, event)
    end function ownership_event

    subroutine populate_ownership_event_storage(arena, event)
        type(ast_arena_t), intent(in) :: arena
        type(ownership_event_query_t), intent(inout) :: event
        integer :: source_index, destination_index

        source_index = event%source_index
        destination_index = event%target_index
        if (source_index <= 0) source_index = event%source_path%node_index
        if (destination_index <= 0) destination_index = &
            event%destination_path%node_index
        if (event%event_kind == OWNERSHIP_EVENT_ALLOCATE .or. &
            event%event_kind == OWNERSHIP_EVENT_DEALLOCATE) then
            destination_index = event%owner_path%node_index
        end if
        if (event%source_expr_index > 0 .and. source_index <= 0) then
            source_index = event%source_expr_index
        end if

        if (source_index > 0) call populate_ownership_operand(arena, event, &
            source_index, event%source_path, .true.)
        if (destination_index > 0) call populate_ownership_operand(arena, event, &
            destination_index, event%destination_path, .false.)
        event%is_refused = event%is_refused .or. &
            event%has_global_mutable_state .or. event%has_unresolved_alias
    end subroutine populate_ownership_event_storage

    subroutine populate_ownership_operand(arena, event, node_index, path, &
            is_source)
        type(ast_arena_t), intent(in) :: arena
        type(ownership_event_query_t), intent(inout) :: event
        integer, intent(in) :: node_index
        type(component_path_query_t), intent(in) :: path
        logical, intent(in) :: is_source
        type(storage_query_t) :: storage
        type(declaration_query_t) :: declaration
        logical :: unsafe_alias
        character(len=:), allocatable :: dynamic_type

        storage = query_designator_storage(arena, node_index)
        unsafe_alias = is_associate_selector_node(arena, node_index)
            unsafe_alias = unsafe_alias .or. path%is_array_element .or. &
                path%is_array_section
        if (storage%found) then
            unsafe_alias = unsafe_alias .or. storage%is_pointer .or. &
                storage%is_target
            if (storage%is_component) event%has_dynamic_type_boundary = .true.
            if (storage_has_global_state(storage)) then
                event%has_global_mutable_state = .true.
            end if
            dynamic_type = ''
            declaration = query_declaration(arena, storage%declaration_index)
            if (storage%is_polymorphic .or. (declaration%found .and. &
                is_polymorphic_type_spec(declaration%type_name))) then
                dynamic_type = ''
            else if (storage%is_concrete_derived .or. &
                    (declaration%found .and. &
                    is_derived_type_spec(declaration%type_name))) then
                if (declaration%found) then
                    dynamic_type = derived_type_name_from_spec(declaration%type_name)
                else
                    dynamic_type = derived_type_name_from_spec(storage%type_name)
                end if
            end if
            if (is_source) then
                event%source_declaration_index = storage%declaration_index
                event%source_storage_class = storage%storage_class
                event%source_storage_resolved = .true.
                event%source_is_polymorphic = storage%is_polymorphic
                call set_event_dynamic_type(event%source_dynamic_type, &
                    dynamic_type, len_trim(dynamic_type) > 0)
                event%is_source_dynamic_type_known = len_trim(dynamic_type) > 0
                event%has_unresolved_alias = event%has_unresolved_alias .or. &
                    unsafe_alias
            else
                event%destination_declaration_index = &
                    storage%declaration_index
                event%destination_storage_class = storage%storage_class
                event%destination_storage_resolved = .true.
                event%destination_is_polymorphic = storage%is_polymorphic
                call set_event_dynamic_type(event%destination_dynamic_type, &
                    dynamic_type, len_trim(dynamic_type) > 0)
                event%is_destination_dynamic_type_known = len_trim(dynamic_type) > 0
                event%has_unresolved_alias = event%has_unresolved_alias .or. &
                    unsafe_alias
            end if
        else if (event%event_kind == OWNERSHIP_EVENT_MOVE_ALLOC) then
            event%has_unresolved_alias = .true.
        end if
    end subroutine populate_ownership_operand

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
