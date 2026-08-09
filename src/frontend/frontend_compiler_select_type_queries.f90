module frontend_compiler_select_type_queries
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: assignment_node, identifier_node, &
        call_or_subscript_node
    use ast_nodes_conditional, only: if_node, select_case_node, &
        select_rank_node, select_type_node, type_guard_block_node
    use ast_nodes_array, only: where_node
    use ast_nodes_loops, only: do_loop_node, do_while_node, forall_node
    use ast_nodes_procedure, only: subroutine_call_node
    use frontend_compiler_control_queries, only: control_statement_query_t, &
        select_type_arm_query_t, query_control_statement, CONTROL_SELECT_TYPE
    use frontend_compiler_queries, only: type_bound_call_query_t, &
        query_type_bound_call, binding_hierarchy_query_t, &
        query_type_binding_hierarchy, procedure_signature_query_t, &
        query_procedure_signature, derived_type_query_t, query_derived_type, &
        storage_query_t, component_path_query_t, component_access_query_t, &
        query_component_access, query_component_path, query_declaration, &
        query_storage, get_identifier_name, declaration_query_t, &
        query_type_binding, type_binding_query_t, STORAGE_BORROWED, &
        STORAGE_OWNED, STORAGE_LOCAL, query_active_global_references, &
        global_reference_query_t, query_type_binding_hierarchy
    use frontend_compiler_resolution, only: declaration_binding_t, &
        resolve_name_at_node, BINDING_ASSOCIATE_NAME, BINDING_FUNCTION, &
        BINDING_SUBROUTINE
    use frontend_compiler_type_queries, only: resolved_type_query_t, &
        query_resolved_type
    use string_utils_mod, only: to_lower
    implicit none
    private

    integer, parameter, public :: SELECT_TYPE_MATCH_UNKNOWN = 0
    integer, parameter, public :: SELECT_TYPE_MATCH_EXACT = 1
    integer, parameter, public :: SELECT_TYPE_MATCH_EXTENSION = 2
    integer, parameter, public :: SELECT_TYPE_MATCH_DEFAULT = 3

    type, public :: select_type_branch_query_t
        !! Source-backed type narrowing facts for one SELECT TYPE arm.
        !!
        !! EXACT means TYPE IS and matches only the named dynamic type.
        !! EXTENSION means CLASS IS and matches the named type or one of its
        !! extensions.  These are predicates, not runtime type selections;
        !! unresolved, ambiguous, intrinsic, and out-of-hierarchy guards are
        !! refused rather than assigned a guessed type.
        logical :: found = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_refused = .false.
        logical :: is_type_is = .false.
        logical :: is_class_is = .false.
        logical :: is_class_default = .false.
        logical :: is_exact_dynamic_type = .false.
        logical :: is_extension_dynamic_type = .false.
        logical :: is_guard_type_abstract = .false.
        logical :: is_declared_type_relation_known = .false.
        logical :: is_guard_same_as_declared = .false.
        logical :: is_guard_extension_of_declared = .false.
        logical :: is_guard_base_of_declared = .false.
        logical :: is_out_of_hierarchy = .false.
        integer :: match_kind = SELECT_TYPE_MATCH_UNKNOWN
        integer :: select_type_node_index = 0
        integer :: arm_node_index = 0
        integer :: arm_ordinal = 0
        integer :: selector_node_index = 0
        integer :: selector_declaration_index = 0
        integer :: guard_type_node_index = 0
        integer :: concrete_type_index = 0
        integer :: declared_type_index = 0
        character(len=:), allocatable :: selector_name
        character(len=:), allocatable :: guard_type_name
        character(len=:), allocatable :: declared_type_name
        character(len=:), allocatable :: refusal_reason
    end type select_type_branch_query_t

    type, public :: select_type_owned_array_query_t
        !! Bounded dynamic-type identity for one local owned polymorphic array.
        !!
        !! A resolved result proves that a direct CLASS IS arm narrows one
        !! local allocatable array from its declared polymorphic type to the
        !! concrete guard type.  The query deliberately does not follow an
        !! associate or pointer alias, mutable global state, or a branch
        !! nested in another control-flow construct.
        logical :: found = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_refused = .false.
        logical :: is_class_is = .false.
        logical :: is_owned_array = .false.
        logical :: is_declared_type_abstract = .false.
        logical :: is_dynamic_type_concrete = .false.
        logical :: has_global_mutable_state = .false.
        logical :: has_unresolved_alias = .false.
        logical :: has_control_flow_boundary = .false.
        integer :: select_type_node_index = 0
        integer :: arm_node_index = 0
        integer :: selector_node_index = 0
        integer :: selector_expression_node_index = 0
        integer :: selector_declaration_index = 0
        integer :: selector_rank = -1
        integer :: selector_storage_class = 0
        integer :: declared_type_index = 0
        integer :: dynamic_type_index = 0
        character(len=:), allocatable :: selector_name
        character(len=:), allocatable :: selector_associate_name
        character(len=:), allocatable :: declared_type_name
        character(len=:), allocatable :: dynamic_type_name
        character(len=:), allocatable :: refusal_reason
        type(storage_query_t) :: selector_storage
    end type select_type_owned_array_query_t

    type, public :: select_type_owned_array_binding_query_t
        !! Bounded binding identity through one owned polymorphic array.
        !!
        !! The owned-array proof supplies the declared and narrowed dynamic
        !! types.  The two hierarchy records retain both sides of the
        !! abstract/deferred binding boundary: the declared binding may be
        !! deferred, while the concrete guard type may expose one inherited
        !! or local implementation.  No runtime dispatch or array-element
        !! inspection is performed.
        logical :: found = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_refused = .false.
        logical :: is_owned_array = .false.
        logical :: is_declared_binding_deferred = .false.
        logical :: is_inherited = .false.
        logical :: is_deferred_binding = .false.
        logical :: is_generic_binding = .false.
        logical :: is_ambiguous_binding = .false.
        logical :: is_implementation_concrete = .false.
        logical :: has_global_mutable_state = .false.
        logical :: has_unresolved_alias = .false.
        logical :: has_control_flow_boundary = .false.
        integer :: select_type_node_index = 0
        integer :: arm_node_index = 0
        integer :: selector_declaration_index = 0
        integer :: declared_type_index = 0
        integer :: dynamic_type_index = 0
        integer :: declaring_type_index = 0
        integer :: binding_node_index = 0
        integer :: implementation_node_index = 0
        character(len=:), allocatable :: selector_name
        character(len=:), allocatable :: declared_type_name
        character(len=:), allocatable :: dynamic_type_name
        character(len=:), allocatable :: binding_name
        character(len=:), allocatable :: declaring_type_name
        character(len=:), allocatable :: implementation
        character(len=:), allocatable :: refusal_reason
        type(select_type_owned_array_query_t) :: owned_array
        type(binding_hierarchy_query_t) :: declared_binding
        type(binding_hierarchy_query_t) :: dynamic_binding
    end type select_type_owned_array_binding_query_t

    type, public :: select_type_component_query_t
        !! Bounded component/storage facts for one component path in a
        !! resolved SELECT TYPE arm.  This is the bridge for a branch
        !! associate such as ``select type (typed => value); type is(child_t);
        !! typed%payload``: the ordinary component query intentionally does
        !! not invent storage for the associate name.
        logical :: found = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_refused = .false.
        logical :: is_selector_associate = .false.
        logical :: is_direct_selector = .false.
        integer :: select_type_node_index = 0
        integer :: arm_node_index = 0
        integer :: arm_ordinal = 0
        integer :: component_node_index = 0
        integer :: selector_node_index = 0
        integer :: selector_expression_node_index = 0
        integer :: selector_declaration_index = 0
        integer :: concrete_type_index = 0
        integer :: terminal_declaration_index = 0
        character(len=:), allocatable :: selector_name
        character(len=:), allocatable :: selector_associate_name
        character(len=:), allocatable :: guard_type_name
        character(len=:), allocatable :: refusal_reason
        type(component_path_query_t) :: selector_path
        type(component_path_query_t) :: component_path
        type(storage_query_t) :: terminal_storage
    end type select_type_component_query_t

    type, public :: select_type_component_binding_query_t
        !! Effective binding facts for a derived component in a narrowed arm.
        !!
        !! The component path is resolved first, then its declared concrete
        !! type is used for the local EXTENDS hierarchy query.  This reports
        !! inherited implementations without treating a polymorphic,
        !! pointer, or allocatable component as having a static target.
        logical :: found = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_refused = .false.
        logical :: is_inherited = .false.
        logical :: is_deferred = .false.
        logical :: is_generic = .false.
        logical :: is_ambiguous = .false.
        logical :: is_abstract_type = .false.
        logical :: is_pointer_boundary = .false.
        logical :: is_allocatable_boundary = .false.
        logical :: is_polymorphic_boundary = .false.
        integer :: select_type_node_index = 0
        integer :: arm_node_index = 0
        integer :: component_node_index = 0
        integer :: component_type_index = 0
        integer :: declaring_type_index = 0
        integer :: binding_node_index = 0
        integer :: implementation_node_index = 0
        character(len=:), allocatable :: component_type_name
        character(len=:), allocatable :: binding_name
        character(len=:), allocatable :: declaring_type_name
        character(len=:), allocatable :: implementation
        character(len=:), allocatable :: refusal_reason
        type(select_type_component_query_t) :: component
        type(binding_hierarchy_query_t) :: hierarchy
    end type select_type_component_binding_query_t

    type, public :: select_type_component_dispatch_query_t
        !! Direct non-generic binding facts through a narrowed component.
        !!
        !! This is the call-site counterpart to
        !! ``query_select_type_component_binding``.  It is deliberately
        !! limited to one explicit CALL that is the sole direct statement of
        !! a concrete SELECT TYPE arm.  The receiver path normally ends in a
        !! scalar, concrete, non-pointer, non-allocatable component.  The
        !! bounded array extension also admits exactly one rank-one component
        !! section with literal lower/upper bounds and unit stride.  The
        !! section facts retain the source bounds; all other array forms stay
        !! explicit refusals without guessing through an ownership or alias
        !! edge.
        logical :: found = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_refused = .false.
        logical :: is_type_is = .false.
        logical :: is_class_is = .false.
        logical :: is_class_default = .false.
        logical :: is_inherited = .false.
        logical :: is_deferred_binding = .false.
        logical :: is_generic_binding = .false.
        logical :: is_ambiguous_target = .false.
        logical :: is_nested = .false.
        logical :: is_array_receiver = .false.
        logical :: is_array_section_receiver = .false.
        logical :: is_contiguous_array_section = .false.
        logical :: is_literal_array_section = .false.
        logical :: is_pointer_boundary = .false.
        logical :: is_allocatable_boundary = .false.
        logical :: is_polymorphic_boundary = .false.
        logical :: is_ownership_changing = .false.
        logical :: has_global_mutable_state = .false.
        logical :: has_unresolved_alias = .false.
        logical :: is_selector_resolved = .false.
        logical :: is_binding_resolved = .false.
        logical :: is_signature_resolved = .false.
        logical :: is_incompatible_pass = .false.
        logical :: pass_arg = .true.
        logical :: is_nopass = .false.
        integer :: select_type_node_index = 0
        integer :: arm_node_index = 0
        integer :: arm_ordinal = 0
        integer :: selector_node_index = 0
        integer :: selector_declaration_index = 0
        integer :: concrete_type_index = 0
        integer :: component_type_index = 0
        integer :: declaring_type_index = 0
        integer :: binding_node_index = 0
        integer :: implementation_node_index = 0
        integer :: receiver_node_index = 0
        integer :: call_node_index = 0
        integer :: array_section_rank = 0
        integer :: array_section_lower_bound = 0
        integer :: array_section_upper_bound = 0
        integer :: array_section_stride = 0
        integer :: arm_source_line = 0
        integer :: arm_source_column = 0
        integer :: call_source_line = 0
        integer :: call_source_column = 0
        integer :: implementation_pass_position = 0
        character(len=:), allocatable :: selector_name
        character(len=:), allocatable :: receiver_name
        character(len=:), allocatable :: component_type_name
        character(len=:), allocatable :: guard_type_name
        character(len=:), allocatable :: binding_name
        character(len=:), allocatable :: declaring_type_name
        character(len=:), allocatable :: implementation
        character(len=:), allocatable :: pass_name
        character(len=:), allocatable :: implementation_pass_name
        character(len=:), allocatable :: implementation_passed_object_type
        character(len=:), allocatable :: refusal_reason
        type(component_path_query_t) :: receiver_path
        type(binding_hierarchy_query_t) :: hierarchy
        type(procedure_signature_query_t) :: signature
    end type select_type_component_dispatch_query_t

    type, public :: select_type_dispatch_query_t
        !! Facts for one direct type-bound invocation in one concrete SELECT
        !! TYPE arm. The invocation may be a CALL statement or the sole
        !! function reference on the right hand side of an assignment.
        !!
        !! FOUND means that the supplied arm and call form a direct structural
        !! match. IS_RESOLVED is narrower and requires a concrete implementation,
        !! compatible PASS metadata, and an ordered same-arena signature. The
        !! query never guesses through CLASS DEFAULT, generic or deferred
        !! bindings, dynamic receivers, nested constructs, or ownership edges.
        logical :: found = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_refused = .false.
        logical :: is_type_is = .false.
        logical :: is_class_is = .false.
        logical :: is_class_default = .false.
        logical :: is_function_reference = .false.
        logical :: is_abstract_guard = .false.
        logical :: is_deferred_binding = .false.
        logical :: is_generic_binding = .false.
        logical :: is_ambiguous_target = .false.
        logical :: is_inherited = .false.
        logical :: is_incompatible_pass = .false.
        logical :: is_nested = .false.
        logical :: is_dynamic_receiver = .false.
        logical :: is_array_receiver = .false.
        logical :: is_ownership_changing = .false.
        logical :: is_selector_resolved = .false.
        logical :: is_binding_resolved = .false.
        logical :: is_signature_resolved = .false.
        logical :: dispatch_boundary_known = .false.
        logical :: pass_arg = .true.
        logical :: is_nopass = .false.
        integer :: select_type_node_index = 0
        integer :: arm_node_index = 0
        integer :: arm_ordinal = 0
        integer :: selector_node_index = 0
        integer :: selector_declaration_index = 0
        integer :: guard_type_node_index = 0
        integer :: concrete_type_index = 0
        integer :: declared_type_index = 0
        integer :: resolved_type_index = 0
        integer :: declaring_type_index = 0
        integer :: binding_node_index = 0
        integer :: implementation_node_index = 0
        integer :: receiver_node_index = 0
        integer :: call_node_index = 0
        integer :: arm_source_line = 0
        integer :: arm_source_column = 0
        integer :: arm_entry_node_index = 0
        integer :: arm_exit_node_index = 0
        integer :: call_source_line = 0
        integer :: call_source_column = 0
        integer :: binding_pass_position = 0
        integer :: implementation_pass_position = 0
        character(len=:), allocatable :: selector_name
        character(len=:), allocatable :: guard_kind
        character(len=:), allocatable :: guard_type_name
        character(len=:), allocatable :: declared_type_name
        character(len=:), allocatable :: concrete_type_name
        character(len=:), allocatable :: declaring_type_name
        character(len=:), allocatable :: receiver_name
        character(len=:), allocatable :: binding_name
        character(len=:), allocatable :: implementation
        character(len=:), allocatable :: pass_name
        character(len=:), allocatable :: implementation_pass_name
        character(len=:), allocatable :: implementation_passed_object_type
        character(len=:), allocatable :: refusal_reason
        type(procedure_signature_query_t) :: signature
    end type select_type_dispatch_query_t

    type, public :: select_type_generic_candidate_query_t
        !! One exact type-bound generic specific considered at a call site.
        !! For the owned-array generic query, the PASS fields below are the
        !! specific binding's metadata, not the enclosing generic's defaults.
        logical :: found = .false.
        logical :: is_match = .false.
        logical :: has_unknown_types = .false.
        logical :: has_global_mutable_state = .false.
        logical :: pass_metadata_resolved = .false.
        logical :: pass_arg = .true.
        logical :: is_nopass = .false.
        integer :: procedure_node_index = 0
        integer :: implementation_node_index = 0
        integer :: pass_position = 0
        character(len=:), allocatable :: procedure_name
        character(len=:), allocatable :: implementation
        character(len=:), allocatable :: pass_name
        type(procedure_signature_query_t) :: signature
    end type select_type_generic_candidate_query_t

    type, public :: type_bound_generic_dispatch_query_t
        !! Exact generic dispatch for one statically typed scalar receiver.
        !!
        !! The selected specific is source-resolved only when exactly one
        !! candidate matches the supplied actuals.  PASS metadata is taken
        !! from that specific's effective inherited binding, not copied from
        !! the enclosing generic interface.
        logical :: found = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_refused = .false.
        logical :: is_generic_binding = .false.
        logical :: is_ambiguous = .false.
        logical :: is_deferred_binding = .false.
        logical :: has_unresolved_alias = .false.
        logical :: has_global_mutable_state = .false.
        logical :: has_dynamic_receiver = .false.
        logical :: has_pointer_boundary = .false.
        logical :: has_allocatable_boundary = .false.
        logical :: has_array_receiver = .false.
        logical :: has_unsupported_ownership = .false.
        logical :: pass_arg = .true.
        logical :: is_nopass = .false.
        integer :: call_node_index = 0
        integer :: receiver_node_index = 0
        integer :: receiver_declaration_index = 0
        integer :: declared_type_index = 0
        integer :: binding_node_index = 0
        integer :: selected_candidate_index = 0
        integer :: selected_procedure_node_index = 0
        integer :: pass_position = 0
        integer :: selected_pass_position = 0
        character(len=:), allocatable :: receiver_name
        character(len=:), allocatable :: declared_type_name
        character(len=:), allocatable :: generic_name
        character(len=:), allocatable :: pass_name
        character(len=:), allocatable :: selected_pass_name
        character(len=:), allocatable :: refusal_reason
        type(storage_query_t) :: receiver_storage
        type(select_type_generic_candidate_query_t), allocatable :: candidates(:)
        type(procedure_signature_query_t) :: signature
    end type type_bound_generic_dispatch_query_t

    type, public :: select_type_generic_dispatch_query_t
        !! Exact type-bound generic resolution for one narrowed SELECT TYPE arm.
        !!
        !! A generic binding is admitted only when one same-arena specific
        !! matches every supplied actual by exact type, kind, and rank.  The
        !! query never selects through ambiguity, deferred or unresolved
        !! specifics, pointer/allocatable selectors, or dynamic receivers.
        logical :: found = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_refused = .false.
        logical :: is_generic_binding = .false.
        logical :: is_ambiguous = .false.
        logical :: is_deferred_binding = .false.
        logical :: is_pointer_boundary = .false.
        logical :: is_allocatable_boundary = .false.
        logical :: is_dynamic_receiver = .false.
        logical :: is_array_receiver = .false.
        integer :: select_type_node_index = 0
        integer :: arm_node_index = 0
        integer :: call_node_index = 0
        integer :: concrete_type_index = 0
        integer :: selected_candidate_index = 0
        integer :: selected_procedure_node_index = 0
        integer :: binding_node_index = 0
        character(len=:), allocatable :: selector_name
        character(len=:), allocatable :: concrete_type_name
        character(len=:), allocatable :: generic_name
        character(len=:), allocatable :: refusal_reason
        type(select_type_generic_candidate_query_t), allocatable :: candidates(:)
        type(procedure_signature_query_t) :: signature
    end type select_type_generic_dispatch_query_t

    type, public :: select_type_component_generic_dispatch_query_t
        !! Exact type-bound generic resolution through one narrowed component.
        !!
        !! This is the downstream contract after component-path narrowing:
        !! ``SELECT TYPE (typed => object); TYPE IS (container_t); CALL
        !! typed%leaf%choose(value)``.  The terminal component must have one
        !! statically known concrete derived type.  Candidates retain their
        !! implementation and ordered signature facts; no dynamic component,
        !! pointer, allocatable, deferred, or ambiguous target is guessed.
        logical :: found = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_refused = .false.
        logical :: is_generic_binding = .false.
        logical :: is_ambiguous = .false.
        logical :: is_deferred_binding = .false.
        logical :: is_pointer_boundary = .false.
        logical :: is_allocatable_boundary = .false.
        logical :: is_polymorphic_boundary = .false.
        logical :: is_dynamic_receiver = .false.
        logical :: is_array_receiver = .false.
        integer :: select_type_node_index = 0
        integer :: arm_node_index = 0
        integer :: call_node_index = 0
        integer :: component_type_index = 0
        integer :: selected_candidate_index = 0
        integer :: selected_procedure_node_index = 0
        integer :: binding_node_index = 0
        character(len=:), allocatable :: selector_name
        character(len=:), allocatable :: receiver_name
        character(len=:), allocatable :: component_type_name
        character(len=:), allocatable :: generic_name
        character(len=:), allocatable :: refusal_reason
        type(component_path_query_t) :: receiver_path
        type(select_type_generic_candidate_query_t), allocatable :: candidates(:)
        type(procedure_signature_query_t) :: signature
    end type select_type_component_generic_dispatch_query_t

    type, public :: select_type_owned_array_generic_dispatch_query_t
        !! Exact generic/PASS resolution through one owned polymorphic array.
        !!
        !! The receiver remains a source designator such as ``values(i)``;
        !! its storage identity is mapped back to the direct SELECT TYPE
        !! selector without manufacturing an AST receiver.  A unique exact
        !! specific is required before any implementation or signature fact
        !! is exposed.
        logical :: found = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_refused = .false.
        logical :: is_owned_array = .false.
        logical :: is_generic_binding = .false.
        logical :: is_ambiguous = .false.
        logical :: is_deferred_binding = .false.
        logical :: is_array_element_receiver = .false.
        logical :: is_array_section_receiver = .false.
        logical :: pass_arg = .true.
        logical :: is_nopass = .false.
        ! PASS metadata on the generic binding itself is retained above;
        ! selected_pass_* identifies the effective selected specific.
        logical :: selected_pass_metadata_resolved = .false.
        logical :: selected_pass_arg = .true.
        logical :: selected_is_nopass = .false.
        logical :: has_global_mutable_state = .false.
        logical :: has_unresolved_alias = .false.
        logical :: has_control_flow_boundary = .false.
        integer :: select_type_node_index = 0
        integer :: arm_node_index = 0
        integer :: call_node_index = 0
        integer :: selector_declaration_index = 0
        integer :: receiver_node_index = 0
        integer :: receiver_declaration_index = 0
        integer :: declared_type_index = 0
        integer :: dynamic_type_index = 0
        integer :: binding_node_index = 0
        integer :: selected_candidate_index = 0
        integer :: selected_procedure_node_index = 0
        integer :: pass_position = 0
        integer :: selected_pass_position = 0
        character(len=:), allocatable :: selector_name
        character(len=:), allocatable :: receiver_name
        character(len=:), allocatable :: declared_type_name
        character(len=:), allocatable :: dynamic_type_name
        character(len=:), allocatable :: generic_name
        character(len=:), allocatable :: pass_name
        character(len=:), allocatable :: selected_pass_name
        character(len=:), allocatable :: refusal_reason
        type(storage_query_t) :: receiver_storage
        type(select_type_owned_array_query_t) :: owned_array
        type(select_type_generic_candidate_query_t), allocatable :: candidates(:)
        type(procedure_signature_query_t) :: signature
    end type select_type_owned_array_generic_dispatch_query_t

    type, public :: select_type_owned_array_dispatch_query_t
        !! Direct non-generic binding dispatch through one owned polymorphic
        !! array element.  The receiver storage remains the selector's array
        !! owner; IS_ARRAY_ELEMENT_RECEIVER identifies the scalar designator
        !! used for the call.
        logical :: found = .false.
        logical :: is_resolved = .false.
        logical :: is_unresolved = .false.
        logical :: is_refused = .false.
        logical :: is_owned_array = .false.
        logical :: is_inherited = .false.
        logical :: is_generic_binding = .false.
        logical :: is_deferred_binding = .false.
        logical :: is_ambiguous_target = .false.
        logical :: is_array_element_receiver = .false.
        logical :: is_array_section_receiver = .false.
        logical :: pass_arg = .true.
        logical :: is_nopass = .false.
        logical :: pass_metadata_resolved = .false.
        logical :: has_global_mutable_state = .false.
        logical :: has_unresolved_alias = .false.
        logical :: has_control_flow_boundary = .false.
        integer :: select_type_node_index = 0
        integer :: arm_node_index = 0
        integer :: call_node_index = 0
        integer :: selector_declaration_index = 0
        integer :: receiver_node_index = 0
        integer :: receiver_declaration_index = 0
        integer :: declared_type_index = 0
        integer :: dynamic_type_index = 0
        integer :: declaring_type_index = 0
        integer :: binding_node_index = 0
        integer :: implementation_node_index = 0
        integer :: pass_position = 0
        integer :: implementation_pass_position = 0
        character(len=:), allocatable :: selector_name
        character(len=:), allocatable :: receiver_name
        character(len=:), allocatable :: declared_type_name
        character(len=:), allocatable :: dynamic_type_name
        character(len=:), allocatable :: binding_name
        character(len=:), allocatable :: declaring_type_name
        character(len=:), allocatable :: implementation
        character(len=:), allocatable :: pass_name
        character(len=:), allocatable :: implementation_pass_name
        character(len=:), allocatable :: implementation_passed_object_type
        character(len=:), allocatable :: refusal_reason
        type(storage_query_t) :: receiver_storage
        type(select_type_owned_array_query_t) :: owned_array
        type(procedure_signature_query_t) :: signature
    end type select_type_owned_array_dispatch_query_t

    public :: query_select_type_branch, query_select_type_owned_array, &
        query_select_type_owned_array_binding, &
        query_select_type_component_path, &
        query_select_type_component_binding, query_select_type_dispatch, &
        query_select_type_component_dispatch, &
        query_select_type_generic_dispatch, &
        query_type_bound_generic_dispatch, &
        query_select_type_component_generic_dispatch, &
        query_select_type_owned_array_generic_dispatch, &
        query_select_type_owned_array_dispatch

contains

    function query_select_type_owned_array(arena, arm_node_index) result(query)
        !! Return one bounded CLASS IS-to-owned-array dynamic identity fact.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arm_node_index
        type(select_type_owned_array_query_t) :: query
        type(select_type_branch_query_t) :: branch
        type(control_statement_query_t) :: control
        type(select_type_arm_query_t) :: arm
        integer :: i

        call initialize_owned_array_query(query, arm_node_index)
        branch = query_select_type_branch(arena, arm_node_index)
        query%found = branch%found
        query%select_type_node_index = branch%select_type_node_index
        query%arm_node_index = branch%arm_node_index
        query%selector_node_index = branch%selector_node_index
        query%selector_declaration_index = branch%selector_declaration_index
        query%declared_type_index = branch%declared_type_index
        query%dynamic_type_index = branch%concrete_type_index
        query%selector_name = branch%selector_name
        query%declared_type_name = branch%declared_type_name
        query%dynamic_type_name = branch%guard_type_name
        query%is_class_is = branch%is_class_is
        if (.not. branch%found) then
            call refuse_owned_array(query, 'SELECT TYPE arm identity is absent')
            return
        end if

        if (.not. arena%has_node_at(branch%select_type_node_index)) then
            call refuse_owned_array(query, 'SELECT TYPE construct identity is absent')
            return
        end if
        control = query_control_statement(arena, branch%select_type_node_index)
        do i = 1, size(control%type_arms)
            if (control%type_arms(i)%arm_node_index /= arm_node_index) cycle
            arm = control%type_arms(i)
            exit
        end do
        if (arm%arm_node_index /= arm_node_index) then
            call refuse_owned_array(query, 'SELECT TYPE arm storage facts are absent')
            return
        end if

        query%selector_expression_node_index = arm%selector_expression_node_index
        query%selector_associate_name = arm%selector_associate_name
        query%selector_storage = arm%selector_storage
        query%selector_rank = arm%selector_storage%rank
        query%selector_storage_class = arm%selector_storage%storage_class
        query%is_declared_type_abstract = arm%selector_storage%is_abstract_type

        if (.not. branch%is_class_is) then
            call refuse_owned_array(query, 'owned-array identity requires CLASS IS')
            return
        end if
        if (arm%is_selector_associate) then
            query%has_unresolved_alias = .true.
            call refuse_owned_array(query, &
                'SELECT TYPE associate selector is an alias boundary')
            return
        end if
        if (arm%selector_storage%is_pointer .or. &
            arm%selector_storage%is_target .or. &
            arm%selector_storage%storage_class == STORAGE_BORROWED) then
            query%has_unresolved_alias = .true.
            call refuse_owned_array(query, &
                'pointer, TARGET, or borrowed selector is an alias boundary')
            return
        end if
        if (arm%selector_storage%is_module_state .or. &
            arm%selector_storage%is_save_state .or. &
            arm%selector_storage%is_common_state) then
            query%has_global_mutable_state = .true.
            call refuse_owned_array(query, &
                'mutable global selector storage is outside the bounded query')
            return
        end if
        if (owned_array_has_control_flow_boundary(arena, arm_node_index) .or. &
            owned_array_body_has_control_flow(arena, arm)) then
            query%has_control_flow_boundary = .true.
            call refuse_owned_array(query, &
                'SELECT TYPE arm is nested in a control-flow construct')
            return
        end if
        if (.not. arm%selector_storage%found .or. &
            .not. arm%selector_storage%is_allocatable) then
            call refuse_owned_array(query, 'selector is not an allocatable array')
            return
        end if
        if (arm%selector_storage%storage_class /= STORAGE_OWNED .or. &
            arm%selector_storage%rank <= 0) then
            call refuse_owned_array(query, 'selector is not a local owned array')
            return
        end if
        if (.not. arm%selector_storage%is_polymorphic .or. &
            arm%selector_storage%is_unlimited_polymorphic) then
            call refuse_owned_array(query, &
                'selector is not a bounded CLASS(base) array')
            return
        end if
        if (.not. query%is_declared_type_abstract) then
            call refuse_owned_array(query, &
                'selector declared type is not abstract')
            return
        end if
        if (.not. branch%is_resolved .or. branch%is_refused .or. &
            branch%concrete_type_index <= 0) then
            call refuse_owned_array(query, &
                'CLASS IS guard does not provide a concrete dynamic identity')
            return
        end if
        if (branch%is_guard_type_abstract) then
            call refuse_owned_array(query, &
                'abstract CLASS IS guard is not a concrete dynamic identity')
            return
        end if

        query%is_owned_array = .true.
        query%is_dynamic_type_concrete = .true.
        query%is_resolved = .true.
    end function query_select_type_owned_array

    function query_select_type_owned_array_binding(arena, arm_node_index, &
            binding_name) result(query)
        !! Resolve one binding from an owned polymorphic CLASS IS array.
        !!
        !! This is a storage-to-binding query, not a runtime dispatch query.
        !! It reports the declared abstract binding separately from the
        !! effective binding on the concrete CLASS IS guard.  A concrete,
        !! non-generic implementation is required for IS_RESOLVED.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arm_node_index
        character(len=*), intent(in) :: binding_name
        type(select_type_owned_array_binding_query_t) :: query
        type(binding_hierarchy_query_t) :: declared_binding
        type(binding_hierarchy_query_t) :: dynamic_binding

        call initialize_owned_array_binding_query(query, arm_node_index, &
            binding_name)
        query%owned_array = query_select_type_owned_array(arena, arm_node_index)
        query%found = query%owned_array%found
        query%is_owned_array = query%owned_array%is_owned_array
        query%has_global_mutable_state = &
            query%owned_array%has_global_mutable_state
        query%has_unresolved_alias = query%owned_array%has_unresolved_alias
        query%has_control_flow_boundary = &
            query%owned_array%has_control_flow_boundary
        query%select_type_node_index = &
            query%owned_array%select_type_node_index
        query%arm_node_index = query%owned_array%arm_node_index
        query%selector_declaration_index = &
            query%owned_array%selector_declaration_index
        query%declared_type_index = query%owned_array%declared_type_index
        query%dynamic_type_index = query%owned_array%dynamic_type_index
        query%selector_name = query%owned_array%selector_name
        query%declared_type_name = query%owned_array%declared_type_name
        query%dynamic_type_name = query%owned_array%dynamic_type_name

        if (.not. query%owned_array%is_resolved) then
            call refuse_owned_array_binding(query, &
                query%owned_array%refusal_reason)
            return
        end if
        if (len_trim(binding_name) == 0) then
            call refuse_owned_array_binding(query, &
                'owned-array binding name is unresolved')
            return
        end if

        declared_binding = query_type_binding_hierarchy(arena, &
            query%declared_type_index, binding_name)
        query%declared_binding = declared_binding
        if (.not. declared_binding%found) then
            call refuse_owned_array_binding(query, &
                'declared owned-array binding is unresolved')
            return
        end if
        query%is_declared_binding_deferred = declared_binding%is_deferred
        if (declared_binding%is_generic .or. declared_binding%is_ambiguous) then
            call refuse_owned_array_binding(query, &
                'generic or ambiguous declared binding is not selected')
            return
        end if

        dynamic_binding = query_type_binding_hierarchy(arena, &
            query%dynamic_type_index, binding_name)
        query%dynamic_binding = dynamic_binding
        if (.not. dynamic_binding%found) then
            call refuse_owned_array_binding(query, &
                'CLASS IS dynamic binding is unresolved')
            return
        end if

        query%is_inherited = dynamic_binding%is_inherited
        query%is_deferred_binding = dynamic_binding%is_deferred
        query%is_generic_binding = dynamic_binding%is_generic
        query%is_ambiguous_binding = dynamic_binding%is_ambiguous
        query%declaring_type_index = dynamic_binding%declaring_type_index
        query%binding_node_index = dynamic_binding%binding_node_index
        query%implementation_node_index = &
            dynamic_binding%implementation_node_index
        query%binding_name = dynamic_binding%binding_name
        query%declaring_type_name = dynamic_binding%declaring_type_name
        query%implementation = dynamic_binding%implementation

        if (query%is_generic_binding .or. query%is_ambiguous_binding) then
            call refuse_owned_array_binding(query, &
                'generic or ambiguous owned-array binding is not selected')
            return
        end if
        if (query%is_deferred_binding) then
            call refuse_owned_array_binding(query, &
                'deferred CLASS IS binding has no implementation target')
            return
        end if
        if (.not. dynamic_binding%is_resolved .or. &
            query%implementation_node_index <= 0 .or. &
            len_trim(query%implementation) == 0) then
            call refuse_owned_array_binding(query, &
                'CLASS IS binding implementation is unresolved')
            return
        end if

        query%is_implementation_concrete = .true.
        query%is_resolved = .true.
    end function query_select_type_owned_array_binding

    function query_select_type_owned_array_generic_dispatch(arena, &
            arm_node_index, call_node_index) result(query)
        !! Resolve one exact generic call on an owned-array element.
        !!
        !! The existing narrowed generic query deliberately rejects
        !! allocatable and array-valued receivers.  This bounded variant
        !! composes the owned-array CLASS IS proof with the same exact
        !! specific matcher, and maps ``values(i)`` back to ``values`` for
        !! storage identity without inventing an AST node.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arm_node_index
        integer, intent(in) :: call_node_index
        type(select_type_owned_array_generic_dispatch_query_t) :: query
        type(type_bound_call_query_t) :: call_facts
        type(binding_hierarchy_query_t) :: hierarchy
        type(type_binding_query_t) :: binding
        type(control_statement_query_t) :: control
        type(select_type_arm_query_t) :: arm
        integer :: i, arm_position
        logical :: arm_found, direct_call

        call initialize_owned_array_generic_query(query, arm_node_index, &
            call_node_index)
        query%owned_array = query_select_type_owned_array(arena, arm_node_index)
        call copy_owned_array_generic_identity(query)
        if (.not. query%owned_array%is_resolved) then
            call refuse_owned_array_generic(query, &
                query%owned_array%refusal_reason)
            return
        end if
        if (.not. arena%has_node_at(call_node_index)) then
            call refuse_owned_array_generic(query, 'type-bound generic call is absent')
            return
        end if

        control = query_control_statement(arena, &
            query%owned_array%select_type_node_index)
        arm_found = .false.
        arm_position = 0
        if (allocated(control%type_arms)) then
            do i = 1, size(control%type_arms)
                if (control%type_arms(i)%arm_node_index /= arm_node_index) cycle
                arm = control%type_arms(i)
                arm_found = .true.
                arm_position = i
                exit
            end do
        end if
        if (.not. arm_found .or. arm_position <= 0) then
            call refuse_owned_array_generic(query, &
                'owned-array CLASS IS arm facts are absent')
            return
        end if
        direct_call = allocated(arm%body_node_indices) .and. &
            size(arm%body_node_indices) == 1 .and. &
            arm%body_node_indices(1) == call_node_index .and. &
            arena%entries(call_node_index)%parent_index == arm_node_index
        if (.not. direct_call) then
            call refuse_owned_array_generic(query, &
                'owned-array generic call is not a direct arm statement')
            return
        end if

        call_facts = query_type_bound_call(arena, call_node_index)
        if (.not. call_facts%found .or. len_trim(call_facts%binding_name) == 0) then
            call refuse_owned_array_generic(query, &
                'owned-array generic receiver or binding is unresolved')
            return
        end if
        query%receiver_node_index = call_facts%receiver_node_index
        query%receiver_name = call_facts%receiver_name
        query%receiver_declaration_index = &
            query%owned_array%selector_declaration_index
        query%receiver_storage = query%owned_array%selector_storage
        if (owned_array_receiver_is_section(query%receiver_name)) then
            query%is_array_section_receiver = .true.
            call refuse_owned_array_generic(query, &
                'owned-array generic receiver is an array section')
            return
        end if
        if (.not. owned_array_element_receiver(query%receiver_name, &
            query%selector_name)) then
            call refuse_owned_array_generic(query, &
                'owned-array generic receiver is not a single array element')
            return
        end if
        query%is_array_element_receiver = .true.

        hierarchy = query_type_binding_hierarchy(arena, &
            query%dynamic_type_index, call_facts%binding_name)
        if (.not. hierarchy%found) then
            call refuse_owned_array_generic(query, &
                'owned-array generic binding hierarchy is unresolved')
            return
        end if
        binding = query_type_binding(arena, hierarchy%binding_node_index)
        if (.not. binding%found) then
            call refuse_owned_array_generic(query, &
                'owned-array generic binding declaration is unresolved')
            return
        end if
        query%binding_node_index = hierarchy%binding_node_index
        query%generic_name = call_facts%binding_name
        query%is_generic_binding = binding%is_generic
        query%is_deferred_binding = binding%is_deferred .or. hierarchy%is_deferred
        query%pass_arg = binding%pass_arg
        query%is_nopass = .not. binding%pass_arg
        query%pass_name = binding%pass_name
        if (.not. query%pass_arg) then
            query%pass_position = 0
        else if (len_trim(query%pass_name) == 0) then
            query%pass_position = 1
        end if
        if (query%is_deferred_binding) then
            call refuse_owned_array_generic(query, &
                'deferred owned-array generic binding has no callable target')
            return
        end if
        if (.not. query%is_generic_binding) then
            call refuse_owned_array_generic(query, &
                'owned-array binding is not a generic interface')
            return
        end if
        call resolve_owned_array_generic_candidates(arena, call_node_index, &
            binding, query)
        if (query%selected_candidate_index > 0 .and. &
            len_trim(query%pass_name) > 0) then
            query%pass_position = find_signature_dummy( &
                query%candidates(query%selected_candidate_index)%signature, &
                query%pass_name)
        end if
    end function query_select_type_owned_array_generic_dispatch

    function query_select_type_owned_array_dispatch(arena, arm_node_index, &
            call_node_index) result(query)
        !! Resolve one direct non-generic binding call on an owned-array element.
        !!
        !! This composes the owned-array CLASS IS proof with the effective
        !! binding hierarchy.  It deliberately does not perform generic
        !! argument matching: callers needing that fact use the generic
        !! dispatch query above.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arm_node_index
        integer, intent(in) :: call_node_index
        type(select_type_owned_array_dispatch_query_t) :: query
        type(type_bound_call_query_t) :: call_facts
        type(binding_hierarchy_query_t) :: hierarchy
        type(control_statement_query_t) :: control
        type(select_type_arm_query_t) :: arm
        integer :: i
        logical :: arm_found, direct_call

        call initialize_owned_array_dispatch_query(query, arm_node_index, &
            call_node_index)
        query%owned_array = query_select_type_owned_array(arena, arm_node_index)
        call copy_owned_array_dispatch_identity(query)
        if (.not. query%owned_array%is_resolved) then
            call refuse_owned_array_dispatch(query, &
                query%owned_array%refusal_reason)
            return
        end if
        if (.not. arena%has_node_at(call_node_index)) then
            call refuse_owned_array_dispatch(query, &
                'owned-array type-bound call is absent')
            return
        end if

        control = query_control_statement(arena, &
            query%owned_array%select_type_node_index)
        arm_found = .false.
        if (allocated(control%type_arms)) then
            do i = 1, size(control%type_arms)
                if (control%type_arms(i)%arm_node_index /= arm_node_index) cycle
                arm = control%type_arms(i)
                arm_found = .true.
                exit
            end do
        end if
        if (.not. arm_found) then
            call refuse_owned_array_dispatch(query, &
                'owned-array CLASS IS arm facts are absent')
            return
        end if
        direct_call = allocated(arm%body_node_indices) .and. &
            size(arm%body_node_indices) == 1 .and. &
            arm%body_node_indices(1) == call_node_index .and. &
            arena%entries(call_node_index)%parent_index == arm_node_index
        if (.not. direct_call) then
            call refuse_owned_array_dispatch(query, &
                'owned-array type-bound call is not a direct arm statement')
            return
        end if

        call_facts = query_type_bound_call(arena, call_node_index)
        if (.not. call_facts%found .or. len_trim(call_facts%binding_name) == 0) then
            call refuse_owned_array_dispatch(query, &
                'owned-array receiver or binding is unresolved')
            return
        end if
        query%receiver_node_index = call_facts%receiver_node_index
        query%receiver_name = call_facts%receiver_name
        query%receiver_declaration_index = query%selector_declaration_index
        query%receiver_storage = query%owned_array%selector_storage
        query%binding_name = call_facts%binding_name
        if (owned_array_receiver_is_section(query%receiver_name)) then
            query%is_array_section_receiver = .true.
            call refuse_owned_array_dispatch(query, &
                'owned-array receiver is an array section')
            return
        end if
        if (.not. owned_array_element_receiver(query%receiver_name, &
            query%selector_name)) then
            call refuse_owned_array_dispatch(query, &
                'owned-array receiver is not a single array element')
            return
        end if
        query%is_array_element_receiver = .true.

        hierarchy = query_type_binding_hierarchy(arena, query%dynamic_type_index, &
            query%binding_name)
        if (.not. hierarchy%found) then
            call refuse_owned_array_dispatch(query, &
                'owned-array binding hierarchy is unresolved')
            return
        end if
        query%declaring_type_index = hierarchy%declaring_type_index
        query%binding_node_index = hierarchy%binding_node_index
        query%implementation_node_index = hierarchy%implementation_node_index
        query%declaring_type_name = hierarchy%declaring_type_name
        query%implementation = hierarchy%implementation
        query%pass_name = hierarchy%pass_name
        query%implementation_pass_name = hierarchy%implementation_pass_name
        query%implementation_pass_position = &
            hierarchy%implementation_pass_position
        query%implementation_passed_object_type = &
            hierarchy%implementation_passed_object_type
        query%is_inherited = hierarchy%is_inherited
        query%is_generic_binding = hierarchy%is_generic
        query%is_deferred_binding = hierarchy%is_deferred
        query%is_ambiguous_target = hierarchy%is_ambiguous
        query%pass_arg = hierarchy%pass_arg
        query%is_nopass = .not. query%pass_arg
        if (query%is_generic_binding) then
            call refuse_owned_array_dispatch(query, &
                'owned-array binding is a generic interface')
            return
        end if
        if (query%is_deferred_binding) then
            call refuse_owned_array_dispatch(query, &
                'owned-array binding is deferred')
            return
        end if
        if (query%is_ambiguous_target) then
            call refuse_owned_array_dispatch(query, &
                'owned-array binding target is ambiguous')
            return
        end if
        if (.not. hierarchy%is_resolved .or. &
            query%implementation_node_index <= 0 .or. &
            len_trim(query%implementation) == 0) then
            call refuse_owned_array_dispatch(query, &
                'owned-array binding implementation is unresolved')
            return
        end if

        query%signature = query_procedure_signature(arena, &
            query%implementation_node_index)
        if (.not. query%signature%found) then
            call refuse_owned_array_dispatch(query, &
                'owned-array implementation signature is unresolved')
            return
        end if
        query%pass_metadata_resolved = hierarchy%implementation_signature_resolved
        if (.not. query%pass_arg) then
            query%pass_position = 0
        else if (len_trim(query%pass_name) == 0) then
            query%pass_position = 1
        else
            query%pass_position = find_signature_dummy(query%signature, &
                query%pass_name)
        end if
        if (query%pass_arg .and. query%pass_position <= 0) then
            call refuse_owned_array_dispatch(query, &
                'owned-array binding PASS dummy is unresolved')
            return
        end if
        if (.not. query%pass_metadata_resolved) then
            call refuse_owned_array_dispatch(query, &
                'owned-array binding PASS metadata is unresolved')
            return
        end if
        query%found = .true.
        query%is_resolved = .true.
    end function query_select_type_owned_array_dispatch

    function query_select_type_branch(arena, arm_node_index) result(query)
        !! Return the semantic type predicate represented by one SELECT TYPE arm.
        !! The result is found only when ARM_NODE_INDEX is an arm in a SELECT
        !! TYPE construct; a found result can still be refused when its source
        !! identity is incomplete or outside the selector hierarchy.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arm_node_index
        type(select_type_branch_query_t) :: query
        type(control_statement_query_t) :: control
        integer :: j
        integer :: select_index

        call initialize_branch_query(query, arm_node_index)
        if (.not. arena%has_node_at(arm_node_index)) then
            call refuse_branch(query, 'SELECT TYPE arm node is absent')
            return
        end if

        select_index = enclosing_select_type(arena, arm_node_index)
        if (select_index <= 0) then
            call refuse_branch(query, &
                'node is not a SELECT TYPE arm of an enclosing construct')
            return
        end if
        control = query_control_statement(arena, select_index)
        if (.not. control%found .or. &
            control%statement_kind /= CONTROL_SELECT_TYPE) then
            call refuse_branch(query, 'SELECT TYPE control facts are absent')
            return
        end if
        do j = 1, size(control%type_arms)
            if (control%type_arms(j)%arm_node_index /= arm_node_index) cycle
            query%found = control%type_arms(j)%found
            query%select_type_node_index = select_index
            call copy_branch_arm_facts(query, control%type_arms(j))
            call classify_branch(query, arena, control%type_arms(j))
            return
        end do

        call refuse_branch(query, &
            'node is not a SELECT TYPE arm of an enclosing construct')
    end function query_select_type_branch

    function query_select_type_component_path(arena, arm_node_index, &
            component_node_index) result(query)
        !! Return storage facts for a component path under one SELECT TYPE arm.
        !!
        !! A direct selector uses the ordinary component-path query.  A branch
        !! associate is resolved only when the guard has a concrete derived
        !! type and every component segment is a scalar, non-polymorphic,
        !! non-pointer, non-allocatable component.  This keeps the result
        !! useful to a transformer without pretending that alias ownership or
        !! dynamic intermediate components are static.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arm_node_index
        integer, intent(in) :: component_node_index
        type(select_type_component_query_t) :: query
        type(control_statement_query_t) :: control
        type(select_type_arm_query_t) :: arm
        type(component_access_query_t) :: access
        type(component_path_query_t) :: direct_path
        type(declaration_query_t) :: terminal
        character(len=:), allocatable :: root_name
        character(len=:), allocatable :: names(:)
        integer, allocatable :: nodes(:)
        integer :: select_index, arm_position, current_type, i

        call initialize_component_query(query, arm_node_index, &
            component_node_index)
        if (.not. arena%has_node_at(component_node_index)) then
            call refuse_component(query, 'component node is absent')
            return
        end if
        select_index = enclosing_select_type(arena, arm_node_index)
        if (select_index <= 0) then
            call refuse_component(query, &
                'arm is not contained in a SELECT TYPE construct')
            return
        end if
        query%select_type_node_index = select_index
        control = query_control_statement(arena, select_index)
        arm_position = find_select_type_arm(control, arm_node_index)
        if (arm_position <= 0) then
            call refuse_component(query, 'node is not a SELECT TYPE arm')
            return
        end if
        arm = control%type_arms(arm_position)
        query%arm_ordinal = arm%arm_ordinal
        query%selector_node_index = arm%selector_node_index
        query%selector_expression_node_index = &
            arm%selector_expression_node_index
        query%selector_declaration_index = arm%selector_declaration_index
        query%selector_name = arm%selector_name
        query%selector_associate_name = arm%selector_associate_name
        query%is_selector_associate = arm%is_selector_associate
        query%is_direct_selector = .not. arm%is_selector_associate
        query%concrete_type_index = arm%concrete_type_index
        query%guard_type_name = arm%concrete_type_name
        query%selector_path = query_component_path(arena, &
            arm%selector_expression_node_index, .true.)

        if (.not. arm%is_selector_resolved .or. arm%is_invalid .or. &
            arm%is_unresolved) then
            call refuse_component(query, 'SELECT TYPE selector or guard is unresolved')
            return
        end if
        if (arm%is_class_default) then
            call refuse_component(query, &
                'CLASS DEFAULT has no statically narrowed component type')
            return
        end if
        if (.not. arm%is_concrete_type_resolved) then
            call refuse_component(query, &
                'SELECT TYPE guard has no concrete derived identity')
            return
        end if
        if (.not. node_is_directly_in_arm(arena, arm_node_index, &
            component_node_index)) then
            call refuse_component(query, &
                'component is outside the direct SELECT TYPE arm body')
            return
        end if

        access = query_component_access(arena, component_node_index)
        if (.not. access%found) then
            call refuse_component(query, 'node is not a component access')
            return
        end if
        call collect_component_segments(arena, component_node_index, names, nodes, &
            root_name)
        if (size(names) == 0) then
            call refuse_component(query, 'component path is empty')
            return
        end if

        if (.not. arm%is_selector_associate) then
            if (.not. same_name(root_name, arm%selector_name)) then
                call refuse_component(query, &
                    'component path root is not the SELECT TYPE selector')
                return
            end if
            direct_path = query_component_path(arena, component_node_index)
            if (.not. direct_path%found) then
                call refuse_component(query, &
                    'direct selector component storage is unresolved')
                return
            end if
            if (size(direct_path%component_declaration_indices) == 0) then
                call refuse_component(query, &
                    'direct selector component declaration is absent')
                return
            end if
            query%component_path = direct_path
            query%terminal_declaration_index = &
                direct_path%component_declaration_indices(size( &
                direct_path%component_declaration_indices))
            query%terminal_storage = query_storage_for_component(arena, &
                component_node_index, query%terminal_declaration_index)
            if (.not. query%terminal_storage%found) then
                call refuse_component(query, &
                    'direct selector terminal storage is unresolved')
                return
            end if
            query%found = .true.
            query%is_resolved = .true.
            return
        end if

        if (.not. same_name(root_name, arm%selector_associate_name)) then
            call refuse_component(query, &
                'component path root is not the SELECT TYPE associate')
            return
        end if

        current_type = arm%concrete_type_index
        do i = 1, size(names)
            query%terminal_declaration_index = find_component_in_hierarchy(&
                arena, current_type, names(i))
            if (query%terminal_declaration_index <= 0) then
                call refuse_component(query, &
                    'component is absent from the narrowed type hierarchy')
                return
            end if
            terminal = query_declaration(arena, query%terminal_declaration_index)
            if (.not. terminal%found) then
                call refuse_component(query, 'component declaration is unresolved')
                return
            end if
            call append_component_path(query%component_path, names(i), nodes(i), &
                query%terminal_declaration_index)
            query%terminal_storage = query_storage_for_component(arena, nodes(i), &
                query%terminal_declaration_index)
            if (query%terminal_storage%is_pointer .or. &
                query%terminal_storage%is_allocatable .or. &
                query%terminal_storage%is_polymorphic) then
                call refuse_component(query, &
                    'pointer, allocatable, or polymorphic component is a storage boundary')
                return
            end if
            if (i < size(names)) then
                current_type = find_derived_type_by_name_local(arena, &
                    declared_type_name(terminal%type_name))
                if (current_type <= 0) then
                    call refuse_component(query, &
                        'intermediate component type is not a resolved derived type')
                    return
                end if
            end if
        end do

        call finalize_component_path(query, arm, component_node_index)
    end function query_select_type_component_path

    function query_select_type_component_binding(arena, arm_node_index, &
            component_node_index, binding_name) result(query)
        !! Resolve one effective binding on a narrowed component path.
        !!
        !! This composes the existing component-path facts with the local
        !! binding hierarchy of the terminal component type.  It deliberately
        !! does not perform generic argument matching or runtime dispatch.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arm_node_index
        integer, intent(in) :: component_node_index
        character(len=*), intent(in) :: binding_name
        type(select_type_component_binding_query_t) :: query
        type(derived_type_query_t) :: component_type
        character(len=:), allocatable :: type_name

        call initialize_component_binding_query(query, arm_node_index, &
            component_node_index, binding_name)
        query%component = query_select_type_component_path(arena, &
            arm_node_index, component_node_index)
        query%select_type_node_index = query%component%select_type_node_index
        query%arm_node_index = arm_node_index
        query%component_node_index = component_node_index
        query%is_pointer_boundary = query%component%terminal_storage%is_pointer
        query%is_allocatable_boundary = &
            query%component%terminal_storage%is_allocatable
        query%is_polymorphic_boundary = &
            query%component%terminal_storage%is_polymorphic

        if (query%component%is_refused .or. &
            .not. query%component%is_resolved) then
            call refuse_component_binding(query, &
                component_refusal(query%component))
            return
        end if
        if (len_trim(binding_name) == 0) then
            call refuse_component_binding(query, &
                'component binding name is unresolved')
            return
        end if
        if (query%is_pointer_boundary) then
            call refuse_component_binding(query, &
                'pointer component is a dynamic storage boundary')
            return
        end if
        if (query%is_allocatable_boundary) then
            call refuse_component_binding(query, &
                'allocatable component is an ownership boundary')
            return
        end if
        if (query%is_polymorphic_boundary) then
            call refuse_component_binding(query, &
                'polymorphic component has no static binding target')
            return
        end if
        if (.not. query%component%terminal_storage%is_derived .or. &
            .not. query%component%terminal_storage%is_concrete_derived) then
            call refuse_component_binding(query, &
                'component terminal type is not a concrete derived type')
            return
        end if

        type_name = declared_type_name(query%component%terminal_storage%type_name)
        query%component_type_name = type_name
        query%component_type_index = find_derived_type_by_name_local(arena, &
            type_name)
        if (query%component_type_index <= 0) then
            call refuse_component_binding(query, &
                'component terminal derived type is unresolved')
            return
        end if

        component_type = query_derived_type(arena, query%component_type_index)
        if (.not. component_type%found) then
            call refuse_component_binding(query, &
                'component terminal derived declaration is unresolved')
            return
        end if
        query%is_abstract_type = contains_word(component_type%attribute_clause, &
            'abstract')
        if (query%is_abstract_type) then
            call refuse_component_binding(query, &
                'abstract component type has no concrete binding storage')
            return
        end if
        query%hierarchy = query_type_binding_hierarchy(arena, &
            query%component_type_index, binding_name)
        if (.not. query%hierarchy%found) then
            call refuse_component_binding(query, &
                'component binding is unresolved')
            return
        end if

        query%is_inherited = query%hierarchy%is_inherited
        query%is_deferred = query%hierarchy%is_deferred
        query%is_generic = query%hierarchy%is_generic
        query%is_ambiguous = query%hierarchy%is_ambiguous
        query%declaring_type_index = query%hierarchy%declaring_type_index
        query%binding_node_index = query%hierarchy%binding_node_index
        query%implementation_node_index = &
            query%hierarchy%implementation_node_index
        query%declaring_type_name = query%hierarchy%declaring_type_name
        query%binding_name = query%hierarchy%binding_name
        query%implementation = query%hierarchy%implementation

        if (query%is_generic .or. query%is_ambiguous) then
            call refuse_component_binding(query, &
                'generic or ambiguous component binding is not selected')
            return
        end if
        if (query%is_deferred) then
            call refuse_component_binding(query, &
                'deferred component binding has no implementation')
            return
        end if
        if (.not. query%hierarchy%is_resolved .or. &
            query%implementation_node_index <= 0 .or. &
            len_trim(query%implementation) == 0) then
            call refuse_component_binding(query, &
                'component binding implementation is unresolved')
            return
        end if

        query%found = .true.
        query%is_resolved = .true.
    end function query_select_type_component_binding

    function query_select_type_component_dispatch(arena, arm_node_index, &
            call_node_index) result(query)
        !! Resolve one direct non-generic CALL through a narrowed component.
        !!
        !! The receiver is source-backed (for example ``typed%leaf``) because
        !! explicit CALL syntax does not always retain a receiver AST node.
        !! This query reports a target only after the narrowed component path,
        !! static hierarchy, PASS metadata, and implementation signature all
        !! resolve.  Generic, nested, aliased, global, pointer, allocatable,
        !! polymorphic, array, and ownership-changing cases remain refusals.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arm_node_index
        integer, intent(in) :: call_node_index
        type(select_type_component_dispatch_query_t) :: query
        type(control_statement_query_t) :: control
        type(select_type_arm_query_t) :: arm
        type(declaration_binding_t) :: selector_binding
        type(binding_hierarchy_query_t) :: hierarchy
        character(len=:), allocatable :: receiver_name, binding_name, reason
        character(len=:), allocatable :: passed_type
        integer :: select_index, arm_position, passed_type_index
        logical :: arm_found, direct_call, is_call, compatible
        character(len=:), allocatable :: selector_error

        call initialize_component_dispatch_query(query, arm_node_index, &
            call_node_index)
        if (.not. arena%has_node_at(arm_node_index)) then
            call refuse_component_dispatch(query, 'SELECT TYPE arm node is absent')
            return
        end if
        if (.not. arena%has_node_at(call_node_index)) then
            call refuse_component_dispatch(query, &
                'type-bound component call node is absent')
            return
        end if

        select_index = enclosing_select_type(arena, arm_node_index)
        if (select_index <= 0) then
            call refuse_component_dispatch(query, &
                'arm is not contained in a SELECT TYPE construct')
            return
        end if
        query%select_type_node_index = select_index
        control = query_control_statement(arena, select_index)
        arm_found = .false.
        arm_position = find_select_type_arm(control, arm_node_index)
        if (arm_position > 0) then
            arm = control%type_arms(arm_position)
            arm_found = .true.
        end if
        if (.not. arm_found) then
            call refuse_component_dispatch(query, &
                'node is not a SELECT TYPE arm')
            return
        end if

        query%arm_ordinal = arm%arm_ordinal
        query%selector_node_index = arm%selector_node_index
        query%selector_declaration_index = arm%selector_declaration_index
        query%concrete_type_index = arm%concrete_type_index
        query%guard_type_name = arm%concrete_type_name
        query%selector_name = arm%selector_name
        query%is_selector_resolved = arm%is_selector_resolved
        query%is_type_is = arm%is_type_is
        query%is_class_is = arm%is_class_is
        query%is_class_default = arm%is_class_default
        query%arm_source_line = arm%source_line
        query%arm_source_column = arm%source_column

        if (arm%is_class_default) then
            call refuse_component_dispatch(query, &
                'CLASS DEFAULT has no narrowed component type')
            return
        end if
        if (arm%is_selector_associate) then
            call resolve_name_at_node(arena, arm%selector_expression_node_index, &
                arm%selector_name, selector_binding, selector_error)
            if (selector_binding%binding_kind == BINDING_ASSOCIATE_NAME) then
                query%has_unresolved_alias = .true.
                call refuse_component_dispatch(query, &
                    'SELECT TYPE selector is an unresolved alias boundary')
                return
            end if
        end if
        if (arm%is_unresolved .or. arm%is_invalid .or. &
            .not. arm%is_selector_resolved .or. &
            .not. arm%is_concrete_type_resolved) then
            call refuse_component_dispatch(query, &
                'SELECT TYPE selector or guard is unresolved')
            return
        end if

        direct_call = allocated(arm%body_node_indices) .and. &
            size(arm%body_node_indices) == 1 .and. &
            arm%body_node_indices(1) == call_node_index .and. &
            arena%entries(call_node_index)%parent_index == arm_node_index
        if (.not. direct_call) then
            query%is_nested = .true.
            call refuse_component_dispatch(query, &
                'component call is not the single direct arm statement')
            return
        end if
        query%call_source_line = arena%entries(call_node_index)%node%line
        query%call_source_column = arena%entries(call_node_index)%node%column

        if (.not. is_explicit_call(arena, call_node_index)) then
            call refuse_component_dispatch(query, &
                'component dispatch requires an explicit CALL statement')
            return
        end if
        call component_call_parts(arena, call_node_index, receiver_name, &
            binding_name, is_call)
        if (.not. is_call .or. index(trim(receiver_name), '%') <= 0 .or. &
            len_trim(binding_name) == 0) then
            call refuse_component_dispatch(query, &
                'component receiver or binding identity is unresolved')
            return
        end if
        query%receiver_name = receiver_name
        query%binding_name = binding_name

        if (arm%selector_storage%is_module_state .or. &
            arm%selector_storage%is_save_state .or. &
            arm%selector_storage%is_common_state) then
            query%has_global_mutable_state = .true.
            query%is_ownership_changing = .true.
            call refuse_component_dispatch(query, &
                'mutable global selector storage is outside the bounded query')
            return
        end if
        if (arm%selector_storage%is_pointer .or. &
            arm%selector_storage%is_target) then
            query%has_unresolved_alias = .true.
            call refuse_component_dispatch(query, &
                'pointer or TARGET selector is an alias boundary')
            return
        end if
        if (arm%selector_storage%is_allocatable .or. &
            arm%selector_storage%is_component) then
            query%is_ownership_changing = .true.
            call refuse_component_dispatch(query, &
                'selector storage has an ownership-changing component edge')
            return
        end if
        if (arm%selector_storage%rank > 0 .or. &
            arm%selector_storage%is_array_element .or. &
            arm%selector_storage%is_array_section) then
            query%is_array_receiver = .true.
            call refuse_component_dispatch(query, &
                'narrowed component receiver is array-valued')
            return
        end if

        if (receiver_has_subscript(receiver_name)) then
            query%is_array_receiver = .true.
            call resolve_narrowed_component_section_receiver(arena, arm, &
                receiver_name, query%receiver_path, query%component_type_index, &
                query%component_type_name, query, reason)
        else
            call resolve_narrowed_component_receiver(arena, arm, receiver_name, &
                query%receiver_path, query%component_type_index, &
                query%component_type_name, query%is_pointer_boundary, &
                query%is_allocatable_boundary, query%is_polymorphic_boundary, &
                query%is_array_receiver, reason)
        end if
        if (query%is_pointer_boundary) query%has_unresolved_alias = .true.
        if (query%is_pointer_boundary .or. query%is_allocatable_boundary .or. &
            query%is_polymorphic_boundary .or. &
            (query%is_array_receiver .and. &
            .not. query%is_array_section_receiver)) then
            if (query%is_pointer_boundary .or. query%is_allocatable_boundary .or. &
                query%is_polymorphic_boundary) query%is_ownership_changing = &
                query%is_allocatable_boundary .or. query%is_polymorphic_boundary
            call refuse_component_dispatch(query, reason)
            return
        end if
        if (query%is_refused) return
        if (.not. query%receiver_path%found .or. &
            query%component_type_index <= 0) then
            call refuse_component_dispatch(query, reason)
            return
        end if
        call inspect_component_dispatch_path(arena, query%receiver_path, query)
        if (query%has_global_mutable_state .or. query%has_unresolved_alias .or. &
            query%is_ownership_changing) then
            return
        end if

        query%found = .true.
        hierarchy = query_type_binding_hierarchy(arena, &
            query%component_type_index, binding_name)
        query%hierarchy = hierarchy
        if (.not. hierarchy%found) then
            call refuse_component_dispatch(query, &
                'component binding hierarchy is unresolved')
            return
        end if
        query%declaring_type_index = hierarchy%declaring_type_index
        query%binding_node_index = hierarchy%binding_node_index
        query%implementation_node_index = hierarchy%implementation_node_index
        query%declaring_type_name = hierarchy%declaring_type_name
        query%implementation = hierarchy%implementation
        query%pass_name = hierarchy%pass_name
        query%implementation_pass_name = hierarchy%implementation_pass_name
        query%implementation_pass_position = &
            hierarchy%implementation_pass_position
        query%implementation_passed_object_type = &
            hierarchy%implementation_passed_object_type
        query%is_inherited = hierarchy%is_inherited
        query%is_deferred_binding = hierarchy%is_deferred
        query%is_generic_binding = hierarchy%is_generic
        query%is_ambiguous_target = hierarchy%is_ambiguous
        query%pass_arg = hierarchy%pass_arg
        query%is_nopass = .not. hierarchy%pass_arg

        if (query%is_generic_binding .or. query%is_ambiguous_target) then
            call refuse_component_dispatch(query, &
                'generic or ambiguous component binding is not selected')
            return
        end if
        if (query%is_deferred_binding) then
            call refuse_component_dispatch(query, &
                'deferred component binding has no implementation')
            return
        end if
        if (.not. hierarchy%is_resolved .or. &
            query%implementation_node_index <= 0 .or. &
            len_trim(query%implementation) == 0) then
            call refuse_component_dispatch(query, &
                'component binding implementation is unresolved')
            return
        end if

        query%signature = query_procedure_signature(arena, &
            query%implementation_node_index)
        query%is_signature_resolved = query%signature%found
        if (.not. query%is_signature_resolved) then
            call refuse_component_dispatch(query, &
                'component implementation signature is unresolved')
            return
        end if
        if (query%pass_arg) then
            if (.not. hierarchy%implementation_signature_resolved .or. &
                query%implementation_pass_position <= 0) then
                query%is_incompatible_pass = .true.
                call refuse_component_dispatch(query, &
                    'component implementation PASS metadata is unresolved')
                return
            end if
            passed_type = normalized_pass_type( &
                query%implementation_passed_object_type)
            passed_type_index = find_derived_type_by_name_local(arena, &
                passed_type)
            compatible = same_name(passed_type, query%component_type_name)
            if (.not. compatible .and. passed_type_index > 0) then
                compatible = type_extends(arena, query%component_type_index, &
                    passed_type_index)
            end if
            if (len_trim(passed_type) == 0 .or. .not. compatible .or. &
                query%implementation_pass_position > &
                query%signature%dummy_count) then
                query%is_incompatible_pass = .true.
                call refuse_component_dispatch(query, &
                    'component implementation PASS type is incompatible')
                return
            end if
        end if

        query%is_binding_resolved = .true.
        query%is_resolved = .true.
    end function query_select_type_component_dispatch

    subroutine inspect_component_dispatch_path(arena, path, query)
        type(ast_arena_t), intent(in) :: arena
        type(component_path_query_t), intent(in) :: path
        type(select_type_component_dispatch_query_t), intent(inout) :: query
        type(storage_query_t) :: storage
        integer :: i, declaration_index

        if (.not. allocated(path%component_declaration_indices)) return
        do i = 1, size(path%component_declaration_indices)
            declaration_index = path%component_declaration_indices(i)
            storage = query_storage(arena, declaration_index)
            if (.not. storage%found) then
                call refuse_component_dispatch(query, &
                    'component path storage is unresolved')
                return
            end if
            if (storage%is_module_state .or. storage%is_save_state .or. &
                storage%is_common_state) then
                query%has_global_mutable_state = .true.
                call refuse_component_dispatch(query, &
                    'mutable global component storage is outside the bounded query')
                return
            end if
            if (storage%is_pointer .or. storage%is_target) then
                query%has_unresolved_alias = .true.
                call refuse_component_dispatch(query, &
                    'pointer or TARGET component is an alias boundary')
                return
            end if
            if (storage%is_allocatable .or. storage%is_polymorphic .or. &
                storage%is_unlimited_polymorphic) then
                query%is_ownership_changing = storage%is_allocatable .or. &
                    storage%is_polymorphic .or. storage%is_unlimited_polymorphic
                call refuse_component_dispatch(query, &
                    'component path crosses a dynamic ownership boundary')
                return
            end if
        end do
    end subroutine inspect_component_dispatch_path

    subroutine initialize_component_query(query, arm_node_index, component_node_index)
        type(select_type_component_query_t), intent(out) :: query
        integer, intent(in) :: arm_node_index, component_node_index

        query%arm_node_index = arm_node_index
        query%component_node_index = component_node_index
        call set_empty(query%selector_name)
        call set_empty(query%selector_associate_name)
        call set_empty(query%guard_type_name)
        call set_empty(query%refusal_reason)
        call initialize_component_path(query%selector_path)
        call initialize_component_path(query%component_path)
        call initialize_storage(query%terminal_storage)
    end subroutine initialize_component_query

    subroutine initialize_component_binding_query(query, arm_node_index, &
            component_node_index, binding_name)
        type(select_type_component_binding_query_t), intent(out) :: query
        integer, intent(in) :: arm_node_index, component_node_index
        character(len=*), intent(in) :: binding_name

        query%arm_node_index = arm_node_index
        query%component_node_index = component_node_index
        call set_empty(query%component_type_name)
        call set_empty(query%binding_name)
        query%binding_name = trim(binding_name)
        call set_empty(query%declaring_type_name)
        call set_empty(query%implementation)
        call set_empty(query%refusal_reason)
    end subroutine initialize_component_binding_query

    subroutine initialize_component_dispatch_query(query, arm_node_index, &
            call_node_index)
        type(select_type_component_dispatch_query_t), intent(out) :: query
        integer, intent(in) :: arm_node_index, call_node_index

        query%arm_node_index = arm_node_index
        query%call_node_index = call_node_index
        call set_empty(query%selector_name)
        call set_empty(query%receiver_name)
        call set_empty(query%component_type_name)
        call set_empty(query%guard_type_name)
        call set_empty(query%binding_name)
        call set_empty(query%declaring_type_name)
        call set_empty(query%implementation)
        call set_empty(query%pass_name)
        call set_empty(query%implementation_pass_name)
        call set_empty(query%implementation_passed_object_type)
        call set_empty(query%refusal_reason)
        call initialize_component_path(query%receiver_path)
    end subroutine initialize_component_dispatch_query

    subroutine refuse_component_dispatch(query, reason)
        type(select_type_component_dispatch_query_t), intent(inout) :: query
        character(len=*), intent(in) :: reason

        query%is_refused = .true.
        query%is_unresolved = .true.
        if (len_trim(query%refusal_reason) == 0) then
            query%refusal_reason = trim(reason)
        end if
    end subroutine refuse_component_dispatch

    subroutine initialize_component_path(path)
        type(component_path_query_t), intent(out) :: path

        allocate (character(len=0) :: path%component_names(0))
        allocate (path%component_node_indices(0))
        allocate (path%component_declaration_indices(0))
    end subroutine initialize_component_path

    subroutine initialize_storage(storage)
        type(storage_query_t), intent(out) :: storage

        call set_empty(storage%name)
        call set_empty(storage%type_name)
    end subroutine initialize_storage

    subroutine refuse_component(query, reason)
        type(select_type_component_query_t), intent(inout) :: query
        character(len=*), intent(in) :: reason

        query%is_refused = .true.
        query%is_unresolved = .true.
        if (len_trim(query%refusal_reason) == 0) then
            query%refusal_reason = trim(reason)
        end if
    end subroutine refuse_component

    subroutine refuse_component_binding(query, reason)
        type(select_type_component_binding_query_t), intent(inout) :: query
        character(len=*), intent(in) :: reason

        query%is_refused = .true.
        query%is_unresolved = .true.
        if (len_trim(query%refusal_reason) == 0) then
            query%refusal_reason = trim(reason)
        end if
    end subroutine refuse_component_binding

    function component_refusal(component) result(reason)
        type(select_type_component_query_t), intent(in) :: component
        character(len=:), allocatable :: reason

        if (allocated(component%refusal_reason) .and. &
            len_trim(component%refusal_reason) > 0) then
            reason = component%refusal_reason
        else
            reason = 'SELECT TYPE component path is unresolved'
        end if
    end function component_refusal

    integer function find_select_type_arm(control, arm_node_index) result(position)
        type(control_statement_query_t), intent(in) :: control
        integer, intent(in) :: arm_node_index
        integer :: i

        position = 0
        if (.not. allocated(control%type_arms)) return
        do i = 1, size(control%type_arms)
            if (control%type_arms(i)%arm_node_index == arm_node_index) then
                position = i
                return
            end if
        end do
    end function find_select_type_arm

    logical function node_is_directly_in_arm(arena, arm_node_index, node_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arm_node_index, node_index
        integer :: current, guard

        node_is_directly_in_arm = .false.
        current = node_index
        guard = 0
        do while (current > 0)
            if (.not. arena%has_node_at(current)) return
            if (current == arm_node_index) then
                node_is_directly_in_arm = .true.
                return
            end if
            select type (node => arena%entries(current)%node)
                type is (type_guard_block_node)
                if (current /= arm_node_index) return
            class default
            end select
            current = arena%entries(current)%parent_index
            guard = guard + 1
            if (guard > arena%size) return
        end do
    end function node_is_directly_in_arm

    recursive subroutine collect_component_segments(arena, node_index, names, &
            nodes, root_name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: names(:)
        integer, allocatable, intent(out) :: nodes(:)
        character(len=:), allocatable, intent(out) :: root_name
        type(component_access_query_t) :: access
        character(len=:), allocatable :: prefix_names(:), identifier, error
        integer, allocatable :: prefix_nodes(:)
        integer :: width, i, count

        access = query_component_access(arena, node_index)
        if (.not. access%found) then
            call get_identifier_name(arena, node_index, identifier, error)
            root_name = trim(identifier)
            allocate (character(len=0) :: names(0))
            allocate (nodes(0))
            return
        end if
        call collect_component_segments(arena, access%base_node_index, &
            prefix_names, prefix_nodes, root_name)
        count = size(prefix_names) + 1
        width = max(1, len_trim(access%component_name))
        if (size(prefix_names) > 0) width = max(width, len(prefix_names))
        allocate (character(len=width) :: names(count))
        allocate (nodes(count))
        do i = 1, size(prefix_names)
            names(i) = prefix_names(i)
            nodes(i) = prefix_nodes(i)
        end do
        names(count) = trim(access%component_name)
        nodes(count) = node_index
    end subroutine collect_component_segments

    integer function find_component_in_hierarchy(arena, type_index, name) &
            result(component_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: type_index
        character(len=*), intent(in) :: name
        type(derived_type_query_t) :: derived
        type(declaration_query_t) :: declaration
        integer :: current, parent, i, guard

        component_index = 0
        current = type_index
        guard = 0
        do while (current > 0)
            derived = query_derived_type(arena, current)
            if (.not. derived%found) return
            if (allocated(derived%component_indices)) then
                do i = 1, size(derived%component_indices)
                    declaration = query_declaration(arena, &
                        derived%component_indices(i))
                    if (declaration%found .and. &
                        same_name(declaration%name, name)) then
                        component_index = declaration%node_index
                        return
                    end if
                end do
            end if
            if (len_trim(derived%extends_parent) == 0) return
            parent = find_derived_type_by_name_local(arena, derived%extends_parent)
            if (parent <= 0) return
            current = parent
            guard = guard + 1
            if (guard > arena%size) return
        end do
    end function find_component_in_hierarchy

    integer function find_derived_type_by_name_local(arena, name) result(index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name
        type(derived_type_query_t) :: derived
        integer :: i

        index = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            derived = query_derived_type(arena, i)
            if (derived%found .and. same_name(derived%name, name)) then
                index = i
                return
            end if
        end do
    end function find_derived_type_by_name_local

    function declared_type_name(type_spec) result(name)
        character(len=*), intent(in) :: type_spec
        character(len=:), allocatable :: name
        character(len=:), allocatable :: lowered
        integer :: left, right, prefix

        name = trim(type_spec)
        lowered = to_lower(name)
        left = index(lowered, 'type(')
        prefix = len('type(')
        if (left /= 1) then
            left = index(lowered, 'class(')
            prefix = len('class(')
        end if
        if (left == 1) then
            right = index(name, ')')
            if (right > prefix) name = trim(name(prefix + 1:right - 1))
        end if
    end function declared_type_name

    subroutine append_component_path(path, name, node_index, declaration_index)
        type(component_path_query_t), intent(inout) :: path
        character(len=*), intent(in) :: name
        integer, intent(in) :: node_index, declaration_index
        character(len=:), allocatable :: names(:)
        integer, allocatable :: nodes(:), declarations(:)
        integer :: old_size, width, i

        old_size = size(path%component_names)
        width = max(1, len_trim(name))
        if (old_size > 0) width = max(width, len(path%component_names))
        allocate (character(len=width) :: names(old_size + 1))
        allocate (nodes(old_size + 1), declarations(old_size + 1))
        do i = 1, old_size
            names(i) = path%component_names(i)
            nodes(i) = path%component_node_indices(i)
            declarations(i) = path%component_declaration_indices(i)
        end do
        names(old_size + 1) = trim(name)
        nodes(old_size + 1) = node_index
        declarations(old_size + 1) = declaration_index
        call move_alloc(names, path%component_names)
        call move_alloc(nodes, path%component_node_indices)
        call move_alloc(declarations, path%component_declaration_indices)
    end subroutine append_component_path

    function query_storage_for_component(arena, node_index, declaration_index) &
            result(storage)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index, declaration_index
        type(storage_query_t) :: storage

        storage = query_storage(arena, declaration_index)
        if (storage%found) then
            storage%node_index = node_index
            storage%is_component = .true.
        end if
    end function query_storage_for_component

    subroutine finalize_component_path(query, arm, component_node_index)
        type(select_type_component_query_t), intent(inout) :: query
        type(select_type_arm_query_t), intent(in) :: arm
        integer, intent(in) :: component_node_index

        if (.not. query%terminal_storage%found) then
            call refuse_component(query, 'terminal component storage is unresolved')
            return
        end if
        query%component_path%found = .true.
        query%component_path%node_index = component_node_index
        query%component_path%base_node_index = &
            arm%selector_expression_node_index
        query%component_path%base_rank = 0
        query%component_path%rank = query%terminal_storage%rank
        query%component_path%storage_class = query%terminal_storage%storage_class
        query%component_path%base_storage_class = &
            arm%selector_storage%storage_class
        query%component_path%is_array_element = &
            query%terminal_storage%is_array_element
        query%component_path%is_array_section = &
            query%terminal_storage%is_array_section
        query%component_path%is_derived = query%terminal_storage%is_derived
        query%component_path%is_concrete_derived = &
            query%terminal_storage%is_concrete_derived
        query%component_path%is_abstract_type = &
            query%terminal_storage%is_abstract_type
        query%component_path%is_allocatable = &
            query%terminal_storage%is_allocatable
        query%component_path%is_pointer = query%terminal_storage%is_pointer
        query%component_path%is_polymorphic = &
            query%terminal_storage%is_polymorphic
        query%component_path%is_unlimited_polymorphic = &
            query%terminal_storage%is_unlimited_polymorphic
        query%found = .true.
        query%is_resolved = .true.
    end subroutine finalize_component_path

    subroutine initialize_branch_query(query, arm_node_index)
        type(select_type_branch_query_t), intent(out) :: query
        integer, intent(in) :: arm_node_index

        query%arm_node_index = arm_node_index
        call set_empty(query%selector_name)
        call set_empty(query%guard_type_name)
        call set_empty(query%declared_type_name)
        call set_empty(query%refusal_reason)
    end subroutine initialize_branch_query

    subroutine copy_branch_arm_facts(query, arm)
        type(select_type_branch_query_t), intent(inout) :: query
        type(select_type_arm_query_t), intent(in) :: arm

        query%arm_ordinal = arm%arm_ordinal
        query%selector_node_index = arm%selector_node_index
        query%selector_declaration_index = arm%selector_declaration_index
        query%guard_type_node_index = arm%type_name_node_index
        query%concrete_type_index = arm%concrete_type_index
        query%declared_type_index = arm%declared_type_index
        query%selector_name = arm%selector_name
        query%guard_type_name = arm%concrete_type_name
        query%declared_type_name = arm%declared_type_name
        query%is_type_is = arm%is_type_is
        query%is_class_is = arm%is_class_is
        query%is_class_default = arm%is_class_default
        query%is_out_of_hierarchy = arm%is_out_of_hierarchy
        query%is_unresolved = arm%is_unresolved
        query%is_refused = arm%is_invalid .or. arm%is_unresolved
        if (query%is_refused) query%refusal_reason = arm%refusal_reason
    end subroutine copy_branch_arm_facts

    subroutine classify_branch(query, arena, arm)
        type(select_type_branch_query_t), intent(inout) :: query
        type(ast_arena_t), intent(in) :: arena
        type(select_type_arm_query_t), intent(in) :: arm
        type(derived_type_query_t) :: guard_type

        if (query%is_class_default) then
            query%match_kind = SELECT_TYPE_MATCH_DEFAULT
            query%is_resolved = query%found .and. arm%is_selector_resolved .and. &
                .not. query%is_refused
            return
        end if
        if (.not. arm%is_concrete_type_resolved) then
            call refuse_branch(query, 'SELECT TYPE guard type identity is unresolved')
            return
        end if

        if (query%is_type_is) then
            query%match_kind = SELECT_TYPE_MATCH_EXACT
            query%is_exact_dynamic_type = .true.
        else if (query%is_class_is) then
            query%match_kind = SELECT_TYPE_MATCH_EXTENSION
            query%is_extension_dynamic_type = .true.
        else
            call refuse_branch(query, 'SELECT TYPE guard kind is unresolved')
            return
        end if

        guard_type = query_derived_type(arena, query%concrete_type_index)
        if (guard_type%found) then
            query%is_guard_type_abstract = contains_word( &
                guard_type%attribute_clause, 'abstract')
        end if
        query%is_declared_type_relation_known = &
            arm%is_declared_type_resolved .and. query%declared_type_index > 0
        if (query%is_declared_type_relation_known) then
            if (same_name(query%guard_type_name, query%declared_type_name)) then
                query%is_guard_same_as_declared = .true.
            else if (type_extends(arena, query%concrete_type_index, &
                    query%declared_type_index)) then
                query%is_guard_extension_of_declared = .true.
            else if (type_extends(arena, query%declared_type_index, &
                    query%concrete_type_index)) then
                query%is_guard_base_of_declared = .true.
            end if
        end if

        query%is_resolved = query%found .and. arm%is_selector_resolved .and. &
            .not. query%is_refused
        if (.not. query%is_resolved .and. len_trim(query%refusal_reason) == 0) then
            call refuse_branch(query, 'SELECT TYPE branch facts are unresolved')
        end if
    end subroutine classify_branch

    logical function contains_word(text, word)
        character(len=*), intent(in) :: text, word

        contains_word = index(to_lower(trim(text)), to_lower(trim(word))) > 0
    end function contains_word

    logical function type_extends(arena, candidate_index, base_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: candidate_index, base_index
        type(derived_type_query_t) :: candidate, parent
        integer :: parent_index, steps

        type_extends = .false.
        candidate = query_derived_type(arena, candidate_index)
        steps = 0
        do while (candidate%found .and. len_trim(candidate%extends_parent) > 0)
            steps = steps + 1
            if (steps > arena%size) return
            parent_index = 0
            do parent_index = 1, arena%size
                parent = query_derived_type(arena, parent_index)
                if (parent%found .and. same_name(parent%name, &
                    candidate%extends_parent)) exit
            end do
            if (parent_index > arena%size) return
            if (parent_index == base_index) then
                type_extends = .true.
                return
            end if
            candidate = parent
        end do
    end function type_extends

    logical function same_name(left, right)
        character(len=*), intent(in) :: left, right

        same_name = to_lower(trim(left)) == to_lower(trim(right))
    end function same_name

    subroutine refuse_branch(query, reason)
        type(select_type_branch_query_t), intent(inout) :: query
        character(len=*), intent(in) :: reason

        query%is_refused = .true.
        query%is_unresolved = .true.
        if (len_trim(query%refusal_reason) == 0) query%refusal_reason = trim(reason)
    end subroutine refuse_branch

    function query_select_type_dispatch(arena, arm_node_index, call_node_index) &
            result(query)
        !! Compose SELECT TYPE arm, concrete binding, and implementation facts.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arm_node_index
        integer, intent(in) :: call_node_index
        type(select_type_dispatch_query_t) :: query
        type(select_type_arm_query_t) :: arm
        type(control_statement_query_t) :: select_query
        type(type_bound_call_query_t) :: call_facts
        type(binding_hierarchy_query_t) :: hierarchy
        type(derived_type_query_t) :: concrete
        integer :: select_index
        logical :: arm_found, direct_call

        call initialize_query(query, arm_node_index, call_node_index)
        if (.not. arena%has_node_at(arm_node_index)) then
            call refuse_unresolved(query, 'SELECT TYPE arm node is absent')
            return
        end if
        if (.not. arena%has_node_at(call_node_index)) then
            call refuse_unresolved(query, 'type-bound call node is absent')
            return
        end if

        select_index = enclosing_select_type(arena, arm_node_index)
        if (select_index <= 0) then
            call refuse_unresolved(query, &
                'arm is not contained in a SELECT TYPE construct')
            return
        end if
        query%select_type_node_index = select_index
        select_query = query_control_statement(arena, select_index)
        if (.not. select_query%found .or. &
            select_query%statement_kind /= CONTROL_SELECT_TYPE) then
            call refuse_unresolved(query, 'SELECT TYPE control facts are absent')
            return
        end if

        arm_found = .false.
        if (allocated(select_query%type_arms)) then
            do select_index = 1, size(select_query%type_arms)
                if (select_query%type_arms(select_index)%arm_node_index == &
                    arm_node_index) then
                    arm = select_query%type_arms(select_index)
                    arm_found = .true.
                    exit
                end if
            end do
        end if
        if (.not. arm_found) then
            call refuse_unresolved(query, &
                'arm is not a SELECT TYPE guard of its enclosing construct')
            return
        end if
        call copy_arm_facts(query, arm)

        if (arm%is_class_default) then
            query%is_class_default = .true.
            call refuse(query, 'CLASS DEFAULT arm is outside concrete dispatch')
        else if (arm%is_type_is) then
            query%is_type_is = .true.
        else if (arm%is_class_is) then
            query%is_class_is = .true.
        else
            call refuse_unresolved(query, 'SELECT TYPE guard kind is unresolved')
        end if
        if (.not. arm%is_concrete_type_resolved) then
            call refuse_unresolved(query, &
                'SELECT TYPE guard concrete type is unresolved')
        end if

        if (arm%is_concrete_type_resolved) then
            concrete = query_derived_type(arena, arm%concrete_type_index)
            if (concrete%found) then
                if (index(to_lower(concrete%attribute_clause), 'abstract') > 0) then
                    query%is_abstract_guard = .true.
                    call refuse_unresolved(query, &
                        'abstract SELECT TYPE guard has unresolved runtime type')
                end if
            end if
        end if

        direct_call = direct_call_in_arm(arena, arm, call_node_index, query)
        if (.not. direct_call) then
            call refuse(query, 'call is not the single direct arm statement')
        end if
        if (.not. is_explicit_call(arena, call_node_index) .and. &
            .not. query%is_function_reference) then
            query%is_dynamic_receiver = .true.
            call refuse(query, &
                'invocation is not an explicit CALL or direct function reference')
        end if

        call_facts = query_type_bound_call(arena, call_node_index)
        query%receiver_node_index = call_facts%receiver_node_index
        query%receiver_name = call_facts%receiver_name
        query%binding_name = call_facts%binding_name
        if (call_facts%receiver_path%found) then
            query%is_dynamic_receiver = .true.
            call refuse(query, 'receiver is a component or dynamic expression')
        end if
        if (len_trim(query%selector_name) == 0) then
            call refuse_unresolved(query, 'SELECT TYPE selector identity is absent')
        else if (.not. same_name(query%receiver_name, query%selector_name)) then
            query%is_dynamic_receiver = .true.
            call refuse(query, 'call receiver is not the SELECT TYPE selector')
        end if
        if (len_trim(query%binding_name) == 0) then
            call refuse_unresolved(query, 'type-bound binding identity is absent')
        end if

        call classify_receiver_storage(query, arm)
        if (arm%is_unresolved .or. arm%is_invalid .or. &
            .not. arm%is_selector_resolved) then
            call refuse_unresolved(query, 'SELECT TYPE selector identity is unresolved')
        end if
        query%found = direct_call .and. arm%found
        if (.not. query%found) then
            query%is_unresolved = .true.
            query%is_refused = .true.
            return
        end if
        if (arm%is_unresolved .or. arm%is_invalid) return
        if (query%is_ownership_changing) return
        if (query%is_class_default .or. &
            .not. arm%is_concrete_type_resolved .or. &
            len_trim(query%binding_name) == 0) return

        hierarchy = query_type_binding_hierarchy(arena, &
            arm%concrete_type_index, query%binding_name)
        if (.not. hierarchy%found) then
            call refuse_unresolved(query, 'concrete type-bound target is unresolved')
            return
        end if
        call copy_hierarchy_facts(query, hierarchy, arm)
        if (hierarchy%is_deferred) then
            query%is_deferred_binding = .true.
            call refuse(query, 'type-bound binding is deferred')
        end if
        if (hierarchy%is_generic) then
            query%is_generic_binding = .true.
            call refuse(query, 'type-bound binding is generic')
        end if
        if (hierarchy%is_ambiguous) then
            query%is_ambiguous_target = .true.
            call refuse_unresolved(query, 'type-bound target is ambiguous')
        end if
        ! An abstract CLASS IS guard names a hierarchy, not one runtime
        ! implementation.  Retain binding/deferred metadata above, but do
        ! not expose the guard type's static binding as a callable target.
        if (query%is_abstract_guard) then
            call set_empty(query%implementation)
            query%implementation_node_index = 0
            call set_empty(query%implementation_pass_name)
            query%implementation_pass_position = 0
            call set_empty(query%implementation_passed_object_type)
            return
        end if
        if (.not. hierarchy%is_resolved .or. &
            len_trim(hierarchy%implementation) == 0 .or. &
            hierarchy%implementation_node_index <= 0) then
            call refuse_unresolved(query, &
                'concrete type-bound implementation is unresolved')
            return
        end if

        query%signature = query_procedure_signature(arena, &
            hierarchy%implementation_node_index)
        query%is_signature_resolved = query%signature%found
        if (.not. query%is_signature_resolved) then
            call refuse_unresolved(query, &
                'implementation procedure signature is unresolved')
        end if
        call check_pass_compatibility(arena, query, hierarchy)
        if (.not. query%is_signature_resolved) return
        if (query%is_incompatible_pass) return
        if (query%is_refused .or. query%is_unresolved) return

        query%is_binding_resolved = .true.
        query%is_resolved = .true.
    end function query_select_type_dispatch

    function query_select_type_generic_dispatch(arena, arm_node_index, &
            call_node_index) result(query)
        !! Resolve one type-bound generic call after SELECT TYPE narrowing.
        !!
        !! This is intentionally a call-site query rather than an extension
        !! of query_type_binding_hierarchy: the latter reports the generic
        !! interface, while this query must inspect the actual arguments and
        !! expose the one exact specific implementation and signature.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arm_node_index
        integer, intent(in) :: call_node_index
        type(select_type_generic_dispatch_query_t) :: query
        type(select_type_dispatch_query_t) :: dispatch
        type(type_bound_call_query_t) :: call_facts
        type(binding_hierarchy_query_t) :: hierarchy
        type(type_binding_query_t) :: binding
        type(storage_query_t) :: selector_storage

        call initialize_generic_dispatch_query(query, arm_node_index, &
            call_node_index)
        dispatch = query_select_type_dispatch(arena, arm_node_index, &
            call_node_index)
        query%select_type_node_index = dispatch%select_type_node_index
        query%concrete_type_index = dispatch%concrete_type_index
        query%selector_name = dispatch%selector_name
        query%concrete_type_name = dispatch%concrete_type_name
        query%is_dynamic_receiver = dispatch%is_dynamic_receiver
        query%is_array_receiver = dispatch%is_array_receiver

        selector_storage = query_storage(arena, dispatch%selector_declaration_index)
        if (selector_storage%is_pointer) then
            query%is_pointer_boundary = .true.
            call refuse_generic_dispatch(query, &
                'pointer SELECT TYPE selector is a dynamic storage boundary')
            return
        end if
        if (selector_storage%is_allocatable) then
            query%is_allocatable_boundary = .true.
            call refuse_generic_dispatch(query, &
                'allocatable SELECT TYPE selector is an ownership boundary')
            return
        end if
        if (.not. dispatch%found) then
            call refuse_generic_dispatch(query, &
                'SELECT TYPE call is not a direct resolved arm call')
            return
        end if
        if (dispatch%is_class_default) then
            call refuse_generic_dispatch(query, &
                'CLASS DEFAULT arm has no concrete generic target')
            return
        end if
        if (dispatch%is_abstract_guard) then
            call refuse_generic_dispatch(query, &
                'abstract SELECT TYPE guard has no concrete generic target')
            return
        end if
        if (dispatch%is_dynamic_receiver .or. dispatch%is_array_receiver) then
            call refuse_generic_dispatch(query, &
                'generic receiver is dynamic or array-valued')
            return
        end if

        call_facts = query_type_bound_call(arena, call_node_index)
        if (len_trim(dispatch%binding_name) == 0 .and. &
            .not. call_facts%found) then
            call refuse_generic_dispatch(query, &
                'type-bound generic receiver or binding is unresolved')
            return
        end if
        if (len_trim(dispatch%binding_name) > 0) then
            query%generic_name = dispatch%binding_name
        else
            query%generic_name = call_facts%binding_name
        end if
        hierarchy = query_type_binding_hierarchy(arena, &
            dispatch%concrete_type_index, query%generic_name)
        if (.not. hierarchy%found) then
            call refuse_generic_dispatch(query, &
                'narrowed type-bound generic hierarchy is unresolved')
            return
        end if
        query%binding_node_index = hierarchy%binding_node_index
        binding = query_type_binding(arena, hierarchy%binding_node_index)
        if (.not. binding%found) then
            call refuse_generic_dispatch(query, &
                'type-bound generic interface declaration is unresolved')
            return
        end if
        query%is_generic_binding = binding%is_generic
        if (binding%is_deferred .or. hierarchy%is_deferred) then
            query%is_deferred_binding = .true.
            call refuse_generic_dispatch(query, &
                'deferred type-bound generic has no callable implementation')
            return
        end if
        if (.not. query%is_generic_binding) then
            call refuse_generic_dispatch(query, &
                'narrowed binding is not a generic interface')
            return
        end if

        if (dispatch%is_ownership_changing) then
            call refuse_generic_dispatch(query, &
                'SELECT TYPE selector has an ownership-changing storage edge')
            return
        end if

        call resolve_generic_candidates(arena, call_node_index, binding, query)
    end function query_select_type_generic_dispatch

    function query_type_bound_generic_dispatch(arena, call_node_index) &
            result(query)
        !! Resolve one type-bound generic call on a static scalar receiver.
        !!
        !! This is the ordinary-receiver counterpart to the narrowed
        !! SELECT TYPE generic query.  A CLASS, pointer, allocatable, target,
        !! component, array, global, or otherwise owned receiver remains an
        !! explicit boundary because its runtime target is not one static
        !! generic-specific fact.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        type(type_bound_generic_dispatch_query_t) :: query
        type(type_bound_call_query_t) :: call_facts
        type(type_binding_query_t) :: binding
        type(binding_hierarchy_query_t) :: hierarchy
        integer :: receiver_storage_node

        call initialize_type_bound_generic_dispatch_query(query, &
            call_node_index)
        if (.not. arena%has_node_at(call_node_index)) return

        call_facts = query_type_bound_call(arena, call_node_index)
        if (.not. call_facts%found .or. .not. call_facts%is_generic) return

        query%found = .true.
        query%receiver_node_index = call_facts%receiver_node_index
        query%receiver_declaration_index = &
            call_facts%receiver_declaration_index
        query%receiver_name = call_facts%receiver_name
        query%declared_type_index = call_facts%declared_type_index
        query%declared_type_name = call_facts%declared_type_name
        query%generic_name = call_facts%binding_name
        query%binding_node_index = call_facts%binding_node_index

        receiver_storage_node = query%receiver_node_index
        if (receiver_storage_node <= 0) then
            receiver_storage_node = query%receiver_declaration_index
        end if
        if (receiver_storage_node <= 0) then
            call refuse_type_bound_generic(query, &
                'type-bound generic receiver storage is unresolved')
            return
        end if
        query%receiver_storage = query_storage(arena, receiver_storage_node)
        if (.not. query%receiver_storage%found) then
            call refuse_type_bound_generic(query, &
                'type-bound generic receiver storage is unresolved')
            return
        end if

        if (call_facts%receiver_path%found .or. &
            query%receiver_storage%is_component .or. &
            query%receiver_storage%is_target) then
            query%has_unresolved_alias = .true.
        end if
        if (query%receiver_storage%is_pointer) then
            query%has_pointer_boundary = .true.
            query%has_unresolved_alias = .true.
        end if
        if (query%receiver_storage%is_allocatable) then
            query%has_allocatable_boundary = .true.
        end if
        query%has_dynamic_receiver = query%receiver_storage%is_polymorphic .or. &
            query%receiver_storage%is_unlimited_polymorphic
        query%has_array_receiver = query%receiver_storage%rank > 0 .or. &
            query%receiver_storage%is_array_element .or. &
            query%receiver_storage%is_array_section
        query%has_global_mutable_state = &
            query%receiver_storage%is_module_state .or. &
            query%receiver_storage%is_save_state .or. &
            query%receiver_storage%is_common_state
        query%has_unsupported_ownership = &
            query%receiver_storage%is_component .or. &
            query%receiver_storage%is_allocatable .or. &
            query%receiver_storage%is_pointer

        if (query%has_unresolved_alias .or. query%has_dynamic_receiver .or. &
            query%has_pointer_boundary .or. &
            query%has_allocatable_boundary .or. query%has_array_receiver .or. &
            query%has_global_mutable_state .or. &
            query%has_unsupported_ownership) then
            call refuse_type_bound_generic(query, &
                'type-bound generic receiver has an unsupported boundary')
            return
        end if

        hierarchy = query_type_binding_hierarchy(arena, &
            query%declared_type_index, query%generic_name)
        if (.not. hierarchy%found) then
            call refuse_type_bound_generic(query, &
                'type-bound generic hierarchy is unresolved')
            return
        end if
        query%binding_node_index = hierarchy%binding_node_index
        binding = query_type_binding(arena, hierarchy%binding_node_index)
        if (.not. binding%found) then
            call refuse_type_bound_generic(query, &
                'type-bound generic declaration is unresolved')
            return
        end if
        query%is_generic_binding = binding%is_generic
        query%is_deferred_binding = binding%is_deferred .or. hierarchy%is_deferred
        query%pass_arg = binding%pass_arg
        query%is_nopass = .not. query%pass_arg
        query%pass_name = binding%pass_name
        if (.not. query%pass_arg) then
            query%pass_position = 0
        else if (len_trim(query%pass_name) == 0) then
            query%pass_position = 1
        end if
        if (query%is_deferred_binding) then
            call refuse_type_bound_generic(query, &
                'deferred type-bound generic has no callable target')
            return
        end if
        if (.not. query%is_generic_binding) then
            call refuse_type_bound_generic(query, &
                'type-bound binding is not a generic interface')
            return
        end if

        if (generic_call_has_alias_boundary(arena, call_node_index)) then
            query%has_unresolved_alias = .true.
            call refuse_type_bound_generic(query, &
                'type-bound generic actual has an alias boundary')
            return
        end if

        call resolve_type_bound_generic_candidates(arena, call_node_index, &
            query%declared_type_index, binding, query)
    end function query_type_bound_generic_dispatch

    subroutine resolve_type_bound_generic_candidates(arena, call_node_index, &
            declared_type_index, binding, query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index, declared_type_index
        type(type_binding_query_t), intent(in) :: binding
        type(type_bound_generic_dispatch_query_t), intent(inout) :: query
        type(declaration_binding_t) :: candidate_binding
        character(len=:), allocatable :: error_msg
        integer, allocatable :: actual_indices(:)
        integer :: i, match_count, selected
        logical :: pass_metadata_found

        if (.not. allocated(binding%generic_names) .or. &
            size(binding%generic_names) == 0) then
            call refuse_type_bound_generic(query, &
                'type-bound generic has no concrete specific names')
            return
        end if

        call generic_call_actuals(arena, call_node_index, actual_indices)
        deallocate (query%candidates)
        allocate (query%candidates(size(binding%generic_names)))
        match_count = 0
        selected = 0
        do i = 1, size(binding%generic_names)
            query%candidates(i)%procedure_name = binding%generic_names(i)
            call resolve_name_at_node(arena, call_node_index, &
                binding%generic_names(i), candidate_binding, error_msg)
            if (.not. candidate_binding%found .or. &
                (candidate_binding%binding_kind /= BINDING_FUNCTION .and. &
                candidate_binding%binding_kind /= BINDING_SUBROUTINE)) then
                query%candidates(i)%has_unknown_types = .true.
                cycle
            end if
            query%candidates(i)%procedure_node_index = candidate_binding%node_index
            query%candidates(i)%implementation_node_index = candidate_binding%node_index
            query%candidates(i)%implementation = binding%generic_names(i)
            query%candidates(i)%signature = query_procedure_signature(arena, &
                candidate_binding%node_index)
            query%candidates(i)%found = query%candidates(i)%signature%found
            if (.not. query%candidates(i)%found) then
                query%candidates(i)%has_unknown_types = .true.
                cycle
            end if

            call resolve_specific_pass_metadata(arena, declared_type_index, &
                binding%generic_names(i), query%candidates(i)%pass_arg, &
                query%candidates(i)%pass_name, pass_metadata_found)
            if (.not. pass_metadata_found) then
                query%candidates(i)%has_unknown_types = .true.
                cycle
            end if
            query%candidates(i)%is_nopass = .not. query%candidates(i)%pass_arg
            query%candidates(i)%pass_metadata_resolved = .true.
            if (.not. query%candidates(i)%pass_arg) then
                query%candidates(i)%pass_position = 0
            else if (len_trim(query%candidates(i)%pass_name) == 0) then
                query%candidates(i)%pass_position = 1
            else
                query%candidates(i)%pass_position = find_signature_dummy( &
                    query%candidates(i)%signature, &
                    query%candidates(i)%pass_name)
            end if
            if (query%candidates(i)%pass_arg .and. &
                query%candidates(i)%pass_position <= 0) then
                query%candidates(i)%has_unknown_types = .true.
                cycle
            end if

            call match_generic_candidate(arena, actual_indices, &
                query%candidates(i), query%candidates(i)%pass_arg, &
                query%candidates(i)%pass_name)
            if (.not. query%candidates(i)%is_match) cycle
            query%candidates(i)%has_global_mutable_state = &
                procedure_has_global_mutable_state(arena, &
                candidate_binding%node_index)
            match_count = match_count + 1
            selected = i
        end do

        if (match_count > 1) then
            query%is_ambiguous = .true.
            call refuse_type_bound_generic(query, &
                'more than one type-bound generic specific matches exactly')
        else if (match_count == 0) then
            call refuse_type_bound_generic(query, &
                'no type-bound generic specific matches exactly')
        else if (query%candidates(selected)%has_global_mutable_state) then
            query%has_global_mutable_state = .true.
            call refuse_type_bound_generic(query, &
                'selected type-bound generic has active global state')
        else
            query%selected_candidate_index = selected
            query%selected_procedure_node_index = &
                query%candidates(selected)%procedure_node_index
            query%signature = query%candidates(selected)%signature
            query%selected_pass_position = query%candidates(selected)%pass_position
            query%selected_pass_name = query%candidates(selected)%pass_name
            query%is_resolved = .true.
        end if
    end subroutine resolve_type_bound_generic_candidates

    subroutine resolve_specific_pass_metadata(arena, derived_type_index, &
            specific_name, pass_arg, pass_name, found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: derived_type_index
        character(len=*), intent(in) :: specific_name
        logical, intent(out) :: pass_arg, found
        character(len=:), allocatable, intent(out) :: pass_name
        type(derived_type_query_t) :: derived
        type(type_binding_query_t) :: binding
        integer :: current_index, parent_index, i, guard

        pass_arg = .true.
        found = .false.
        call set_empty(pass_name)
        current_index = derived_type_index
        guard = 0
        do while (current_index > 0 .and. arena%has_node_at(current_index))
            derived = query_derived_type(arena, current_index)
            if (.not. derived%found) return
            do i = 1, size(derived%binding_indices)
                binding = query_type_binding(arena, derived%binding_indices(i))
                if (.not. binding%found .or. binding%is_generic) cycle
                if (.not. same_name(binding%binding_name, specific_name)) cycle
                if (binding%is_deferred) return
                pass_arg = binding%pass_arg
                pass_name = binding%pass_name
                found = .true.
                return
            end do
            parent_index = find_derived_type_by_name_local(arena, &
                derived%extends_parent)
            if (parent_index <= 0) return
            current_index = parent_index
            guard = guard + 1
            if (guard > arena%size) return
        end do
    end subroutine resolve_specific_pass_metadata

    logical function generic_call_has_alias_boundary(arena, call_node_index) &
            result(has_alias)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        integer, allocatable :: actual_indices(:)
        type(storage_query_t) :: storage
        character(len=:), allocatable :: keyword
        integer :: i, actual_value
        logical :: is_keyword

        has_alias = .false.
        call generic_call_actuals(arena, call_node_index, actual_indices)
        do i = 1, size(actual_indices)
            call generic_actual_info(arena, actual_indices(i), keyword, &
                actual_value, is_keyword)
            if (actual_value <= 0) cycle
            storage = query_storage(arena, actual_value)
            if (.not. storage%found) cycle
            if (storage%is_pointer .or. storage%is_target .or. &
                storage%is_allocatable .or. storage%is_component) then
                has_alias = .true.
                return
            end if
        end do
    end function generic_call_has_alias_boundary

    logical function procedure_has_global_mutable_state(arena, procedure_index) &
            result(has_global)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: procedure_index
        type(global_reference_query_t), allocatable :: references(:)
        type(declaration_query_t) :: declaration
        integer :: i

        has_global = .false.
        references = query_active_global_references(arena, procedure_index)
        do i = 1, size(references)
            declaration = query_declaration(arena, &
                references(i)%declaration_node_index)
            if (declaration%found .and. .not. declaration%is_parameter) then
                has_global = .true.
                return
            end if
        end do
    end function procedure_has_global_mutable_state

    subroutine resolve_generic_candidates(arena, call_node_index, binding, query)
        !! Fill the exact candidate set shared by selector and component calls.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        type(type_binding_query_t), intent(in) :: binding
        type(select_type_generic_dispatch_query_t), intent(inout) :: query
        type(declaration_binding_t) :: candidate_binding
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: specific_pass_name
        integer, allocatable :: actual_indices(:)
        integer :: i, match_count, selected, specific_pass_position
        logical :: specific_pass_arg, specific_pass_found

        if (.not. allocated(binding%generic_names) .or. &
            size(binding%generic_names) == 0) then
            call refuse_generic_dispatch(query, &
                'type-bound generic has no concrete specific names')
            return
        end if

        call generic_call_actuals(arena, call_node_index, actual_indices)
        deallocate (query%candidates)
        allocate (query%candidates(size(binding%generic_names)))
        match_count = 0
        selected = 0
        do i = 1, size(binding%generic_names)
            query%candidates(i)%procedure_name = binding%generic_names(i)
            call resolve_name_at_node(arena, call_node_index, &
                binding%generic_names(i), candidate_binding, error_msg)
            if (.not. candidate_binding%found .or. &
                (candidate_binding%binding_kind /= BINDING_FUNCTION .and. &
                candidate_binding%binding_kind /= BINDING_SUBROUTINE)) then
                query%candidates(i)%has_unknown_types = .true.
                cycle
            end if
            query%candidates(i)%procedure_node_index = candidate_binding%node_index
            query%candidates(i)%implementation_node_index = candidate_binding%node_index
            query%candidates(i)%implementation = binding%generic_names(i)
            query%candidates(i)%signature = query_procedure_signature(arena, &
                candidate_binding%node_index)
            query%candidates(i)%found = query%candidates(i)%signature%found
            if (.not. query%candidates(i)%found) then
                query%candidates(i)%has_unknown_types = .true.
                cycle
            end if
            call resolve_specific_pass_metadata(arena, query%concrete_type_index, &
                binding%generic_names(i), specific_pass_arg, specific_pass_name, &
                specific_pass_found)
            if (specific_pass_found) then
                query%candidates(i)%pass_metadata_resolved = .true.
                query%candidates(i)%pass_arg = specific_pass_arg
                query%candidates(i)%is_nopass = .not. specific_pass_arg
                query%candidates(i)%pass_name = specific_pass_name
            else
                query%candidates(i)%pass_metadata_resolved = .false.
                query%candidates(i)%pass_arg = binding%pass_arg
                query%candidates(i)%is_nopass = .not. binding%pass_arg
                query%candidates(i)%pass_name = binding%pass_name
            end if
            if (.not. query%candidates(i)%pass_arg) then
                specific_pass_position = 0
            else if (len_trim(query%candidates(i)%pass_name) == 0) then
                specific_pass_position = 1
            else
                specific_pass_position = find_signature_dummy( &
                    query%candidates(i)%signature, query%candidates(i)%pass_name)
            end if
            query%candidates(i)%pass_position = specific_pass_position
            if (query%candidates(i)%pass_arg .and. specific_pass_position <= 0) then
                query%candidates(i)%has_unknown_types = .true.
                cycle
            end if
            call match_generic_candidate(arena, actual_indices, &
                query%candidates(i), query%candidates(i)%pass_arg, &
                query%candidates(i)%pass_name)
            if (query%candidates(i)%is_match) then
                match_count = match_count + 1
                selected = i
            end if
        end do

        query%found = .true.
        if (match_count > 1) then
            query%is_ambiguous = .true.
            call refuse_generic_dispatch(query, &
                'more than one type-bound generic specific matches exactly')
        else if (match_count == 0) then
            call refuse_generic_dispatch(query, &
                'no type-bound generic specific matches exactly')
        else
            query%selected_candidate_index = selected
            query%selected_procedure_node_index = &
                query%candidates(selected)%procedure_node_index
            query%signature = query%candidates(selected)%signature
            query%is_resolved = .true.
        end if
    end subroutine resolve_generic_candidates

    function query_select_type_component_generic_dispatch(arena, &
            arm_node_index, call_node_index) result(query)
        !! Resolve one type-bound generic call through a narrowed component.
        !!
        !! Unlike query_select_type_generic_dispatch, the receiver is a
        !! component designator rooted in the narrowed SELECT TYPE selector.
        !! The component path is source-backed for explicit CALL syntax, so no
        !! synthetic AST receiver is created.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arm_node_index
        integer, intent(in) :: call_node_index
        type(select_type_component_generic_dispatch_query_t) :: query
        type(control_statement_query_t) :: control
        type(select_type_arm_query_t) :: arm
        type(select_type_dispatch_query_t) :: direct_query
        type(binding_hierarchy_query_t) :: hierarchy
        type(type_binding_query_t) :: binding
        type(select_type_generic_dispatch_query_t) :: generic_query
        character(len=:), allocatable :: receiver_name, binding_name, reason
        logical :: is_call
        integer :: select_index, arm_position

        call initialize_component_generic_dispatch_query(query, &
            arm_node_index, call_node_index)
        if (.not. arena%has_node_at(arm_node_index)) then
            call refuse_component_generic_dispatch(query, &
                'SELECT TYPE arm node is absent')
            return
        end if
        if (.not. arena%has_node_at(call_node_index)) then
            call refuse_component_generic_dispatch(query, &
                'type-bound generic call node is absent')
            return
        end if

        select_index = enclosing_select_type(arena, arm_node_index)
        if (select_index <= 0) then
            call refuse_component_generic_dispatch(query, &
                'arm is not contained in a SELECT TYPE construct')
            return
        end if
        query%select_type_node_index = select_index
        control = query_control_statement(arena, select_index)
        arm_position = find_select_type_arm(control, arm_node_index)
        if (arm_position <= 0) then
            call refuse_component_generic_dispatch(query, &
                'node is not a SELECT TYPE arm')
            return
        end if
        arm = control%type_arms(arm_position)
        if (arm%is_selector_associate) then
            query%selector_name = arm%selector_associate_name
        else
            query%selector_name = arm%selector_name
        end if
        if (arm%is_class_default) then
            call refuse_component_generic_dispatch(query, &
                'CLASS DEFAULT arm has no narrowed component type')
            return
        end if
        if (arm%is_unresolved .or. arm%is_invalid .or. &
            .not. arm%is_selector_resolved .or. &
            .not. arm%is_concrete_type_resolved) then
            call refuse_component_generic_dispatch(query, &
                'SELECT TYPE selector or guard is unresolved')
            return
        end if

        call initialize_query(direct_query, arm_node_index, call_node_index)
        if (.not. direct_call_in_arm(arena, arm, call_node_index, direct_query)) then
            call refuse_component_generic_dispatch(query, &
                'generic component call is not the single direct arm statement')
            return
        end if
        if (.not. is_explicit_call(arena, call_node_index)) then
            query%is_dynamic_receiver = .true.
            call refuse_component_generic_dispatch(query, &
                'generic component call is not an explicit CALL statement')
            return
        end if

        call component_call_parts(arena, call_node_index, receiver_name, &
            binding_name, is_call)
        if (.not. is_call) then
            call refuse_component_generic_dispatch(query, &
                'call receiver is not a component designator')
            return
        end if
        query%receiver_name = receiver_name
        query%generic_name = binding_name

        call resolve_narrowed_component_receiver(arena, arm, receiver_name, &
            query%receiver_path, query%component_type_index, &
            query%component_type_name, query%is_pointer_boundary, &
            query%is_allocatable_boundary, query%is_polymorphic_boundary, &
            query%is_array_receiver, reason)
        if (query%is_pointer_boundary .or. query%is_allocatable_boundary .or. &
            query%is_polymorphic_boundary .or. query%is_array_receiver) then
            call refuse_component_generic_dispatch(query, reason)
            return
        end if
        if (.not. query%receiver_path%found .or. &
            query%component_type_index <= 0) then
            call refuse_component_generic_dispatch(query, reason)
            return
        end if

        hierarchy = query_type_binding_hierarchy(arena, &
            query%component_type_index, binding_name)
        if (.not. hierarchy%found) then
            call refuse_component_generic_dispatch(query, &
                'narrowed component generic hierarchy is unresolved')
            return
        end if
        query%binding_node_index = hierarchy%binding_node_index
        binding = query_type_binding(arena, hierarchy%binding_node_index)
        if (.not. binding%found) then
            call refuse_component_generic_dispatch(query, &
                'component generic interface declaration is unresolved')
            return
        end if
        if (binding%is_deferred .or. hierarchy%is_deferred) then
            query%is_deferred_binding = .true.
            call refuse_component_generic_dispatch(query, &
                'deferred component generic has no callable implementation')
            return
        end if
        if (.not. binding%is_generic) then
            call refuse_component_generic_dispatch(query, &
                'narrowed component binding is not a generic interface')
            return
        end if
        query%is_generic_binding = .true.

        call initialize_generic_dispatch_query(generic_query, arm_node_index, &
            call_node_index)
        generic_query%select_type_node_index = select_index
        generic_query%concrete_type_index = query%component_type_index
        generic_query%generic_name = binding_name
        generic_query%is_generic_binding = .true.
        generic_query%is_pointer_boundary = query%is_pointer_boundary
        generic_query%is_allocatable_boundary = query%is_allocatable_boundary
        call resolve_generic_candidates(arena, call_node_index, binding, &
            generic_query)

        query%found = generic_query%found
        query%is_resolved = generic_query%is_resolved
        query%is_unresolved = generic_query%is_unresolved
        query%is_refused = generic_query%is_refused
        query%is_ambiguous = generic_query%is_ambiguous
        query%selected_candidate_index = generic_query%selected_candidate_index
        query%selected_procedure_node_index = &
            generic_query%selected_procedure_node_index
        query%candidates = generic_query%candidates
        query%signature = generic_query%signature
        if (allocated(generic_query%refusal_reason)) then
            query%refusal_reason = generic_query%refusal_reason
        end if
    end function query_select_type_component_generic_dispatch

    subroutine component_call_parts(arena, call_node_index, receiver_name, &
            binding_name, is_call)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        character(len=:), allocatable, intent(out) :: receiver_name
        character(len=:), allocatable, intent(out) :: binding_name
        logical, intent(out) :: is_call
        character(len=:), allocatable :: designator
        integer :: separator

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
            if (index(receiver_name, '%') <= 0 .or. &
                len_trim(binding_name) == 0) then
                call set_empty(receiver_name)
                call set_empty(binding_name)
                return
            end if
            is_call = .true.
        class default
        end select
    end subroutine component_call_parts

    logical function receiver_has_subscript(receiver) result(has_subscript)
        character(len=*), intent(in) :: receiver

        has_subscript = index(trim(receiver), '(') > 0
    end function receiver_has_subscript

    subroutine resolve_narrowed_component_section_receiver(arena, arm, &
            receiver_name, path, component_type_index, component_type_name, &
            query, refusal_reason)
        type(ast_arena_t), intent(in) :: arena
        type(select_type_arm_query_t), intent(in) :: arm
        character(len=*), intent(in) :: receiver_name
        type(component_path_query_t), intent(out) :: path
        integer, intent(out) :: component_type_index
        character(len=:), allocatable, intent(out) :: component_type_name
        type(select_type_component_dispatch_query_t), intent(inout) :: query
        character(len=:), allocatable, intent(out) :: refusal_reason
        type(declaration_query_t) :: declaration
        type(storage_query_t) :: storage
        character(len=:), allocatable :: designator, prefix, root_name
        character(len=:), allocatable :: remaining, segment, type_name
        integer :: open_paren, close_paren, separator, start, next_separator
        integer :: current_type, component_index
        logical :: last_segment

        call initialize_component_path(path)
        component_type_index = 0
        call set_empty(component_type_name)
        call set_empty(refusal_reason)
        query%is_array_section_receiver = .true.
        query%is_array_receiver = .true.
        path%base_node_index = arm%selector_expression_node_index
        path%base_storage_class = arm%selector_storage%storage_class
        path%base_rank = arm%selector_storage%rank

        designator = trim(receiver_name)
        open_paren = index(designator, '(')
        close_paren = index(designator, ')', back=.true.)
        if (open_paren <= 0 .or. close_paren <= open_paren .or. &
            close_paren /= len_trim(designator)) then
            refusal_reason = 'component array section designator is malformed'
            return
        end if
        if (index(designator(open_paren + 1:close_paren - 1), '(') > 0 .or. &
            index(designator(open_paren + 1:close_paren - 1), ')') > 0) then
            refusal_reason = 'component array section bounds are unresolved'
            return
        end if
        call parse_component_array_section_bounds( &
            designator(open_paren + 1:close_paren - 1), query, &
            refusal_reason)
        if (len_trim(refusal_reason) > 0) return

        prefix = trim(designator(:open_paren - 1))
        if (index(prefix, '(') > 0 .or. index(prefix, ')') > 0) then
            refusal_reason = 'component array section has more than one subscript'
            return
        end if
        separator = index(prefix, '%')
        if (separator <= 1) then
            refusal_reason = 'component array section path is absent'
            return
        end if
        root_name = trim(prefix(:separator - 1))
        if (arm%is_selector_associate) then
            if (.not. same_name(root_name, arm%selector_associate_name)) then
                refusal_reason = 'component receiver is not the SELECT TYPE associate'
                return
            end if
        else if (.not. same_name(root_name, arm%selector_name)) then
            refusal_reason = 'component receiver is not the SELECT TYPE selector'
            return
        end if

        remaining = trim(prefix(separator + 1:))
        current_type = arm%concrete_type_index
        start = 1
        do
            next_separator = index(remaining(start:), '%')
            last_segment = next_separator <= 0
            if (last_segment) then
                segment = trim(remaining(start:))
            else
                if (next_separator <= 1) then
                    refusal_reason = 'component receiver path contains an empty segment'
                    return
                end if
                segment = trim(remaining(start:start + next_separator - 2))
            end if
            if (len_trim(segment) == 0) then
                refusal_reason = 'component receiver path contains an empty segment'
                return
            end if

            component_index = find_component_in_hierarchy(arena, current_type, &
                segment)
            if (component_index <= 0) then
                refusal_reason = 'component receiver is absent from the narrowed type hierarchy'
                return
            end if
            declaration = query_declaration(arena, component_index)
            if (.not. declaration%found) then
                refusal_reason = 'component receiver declaration is unresolved'
                return
            end if
            storage = query_storage(arena, component_index)
            if (.not. storage%found) then
                refusal_reason = 'component receiver storage is unresolved'
                return
            end if
            call append_component_path(path, segment, 0, component_index)
            if (storage%is_pointer .or. storage%is_target) then
                query%is_pointer_boundary = .true.
                query%has_unresolved_alias = .true.
                refusal_reason = 'pointer or TARGET component is an alias boundary'
                return
            end if
            if (storage%is_allocatable) then
                query%is_allocatable_boundary = .true.
                query%is_ownership_changing = .true.
                refusal_reason = 'allocatable component is an ownership boundary'
                return
            end if
            if (storage%is_polymorphic .or. storage%is_unlimited_polymorphic) then
                query%is_polymorphic_boundary = .true.
                query%is_ownership_changing = .true.
                refusal_reason = 'polymorphic component has no static binding target'
                return
            end if

            type_name = declared_type_name(declaration%type_name)
            if (last_segment) then
                if (storage%rank /= 1 .or. storage%is_array_element .or. &
                    storage%is_array_section) then
                    refusal_reason = 'component array section requires a rank-one array component'
                    return
                end if
                if (.not. storage%is_derived .or. &
                    .not. storage%is_concrete_derived) then
                    refusal_reason = 'component array section element type is not concrete'
                    return
                end if
                component_type_name = type_name
                component_type_index = find_derived_type_by_name_local(arena, &
                    type_name)
                if (component_type_index <= 0) then
                    refusal_reason = 'component array section element type is unresolved'
                    return
                end if
                exit
            end if

            if (storage%rank > 0 .or. storage%is_array_element .or. &
                storage%is_array_section) then
                refusal_reason = 'intermediate component array is not supported'
                return
            end if
            if (.not. storage%is_derived .or. &
                .not. storage%is_concrete_derived) then
                refusal_reason = 'intermediate component type is not concrete'
                return
            end if
            current_type = find_derived_type_by_name_local(arena, type_name)
            if (current_type <= 0) then
                refusal_reason = 'intermediate component type is unresolved'
                return
            end if
            start = start + next_separator
            if (start > len(remaining)) then
                refusal_reason = 'component receiver path is incomplete'
                return
            end if
        end do

        path%found = .true.
        path%storage_class = storage%storage_class
        path%rank = 1
        path%is_array_section = .true.
        path%is_derived = storage%is_derived
        path%is_concrete_derived = storage%is_concrete_derived
        path%is_abstract_type = storage%is_abstract_type
        path%is_allocatable = storage%is_allocatable
        path%is_pointer = storage%is_pointer
        path%is_polymorphic = storage%is_polymorphic
        path%is_unlimited_polymorphic = storage%is_unlimited_polymorphic
    end subroutine resolve_narrowed_component_section_receiver

    subroutine parse_component_array_section_bounds(text, query, reason)
        character(len=*), intent(in) :: text
        type(select_type_component_dispatch_query_t), intent(inout) :: query
        character(len=:), allocatable, intent(out) :: reason
        character(len=:), allocatable :: lower_text, upper_text, stride_text
        integer :: first_colon, second_colon, colon_count
        integer :: lower, upper, stride
        logical :: lower_ok, upper_ok, stride_ok

        call set_empty(reason)
        query%array_section_rank = 1 + count_text_character(text, ',')
        query%array_section_lower_bound = 0
        query%array_section_upper_bound = 0
        query%array_section_stride = 1
        query%is_literal_array_section = .false.
        query%is_contiguous_array_section = .false.
        if (query%array_section_rank /= 1) then
            reason = 'only rank-one component array sections are supported'
            return
        end if

        colon_count = count_text_character(text, ':')
        if (colon_count < 1) then
            reason = 'component array receiver is an array element, not a section'
            return
        end if
        if (colon_count > 2) then
            reason = 'component array section has an unresolved shape'
            return
        end if
        first_colon = index(text, ':')
        second_colon = 0
        if (colon_count == 2) then
            second_colon = index(text(first_colon + 1:), ':') + first_colon
        end if
        if (second_colon > 0) then
            lower_text = trim(text(:first_colon - 1))
            upper_text = trim(text(first_colon + 1:second_colon - 1))
            stride_text = trim(text(second_colon + 1:))
            if (len_trim(stride_text) == 0) then
                reason = 'component array section stride is unresolved'
                return
            end if
        else
            lower_text = trim(text(:first_colon - 1))
            upper_text = trim(text(first_colon + 1:))
            stride_text = ''
        end if

        lower_ok = parse_integer_literal_text(lower_text, lower)
        upper_ok = parse_integer_literal_text(upper_text, upper)
        stride_ok = .true.
        stride = 1
        if (len_trim(stride_text) > 0) then
            stride_ok = parse_integer_literal_text(stride_text, stride)
        end if
        if (.not. lower_ok .or. .not. upper_ok .or. .not. stride_ok) then
            reason = 'component array section bounds must be integer literals'
            return
        end if

        query%array_section_lower_bound = lower
        query%array_section_upper_bound = upper
        query%array_section_stride = stride
        query%is_literal_array_section = .true.
        if (stride /= 1) then
            reason = 'component array section must have unit stride'
            return
        end if
        query%is_contiguous_array_section = .true.
    end subroutine parse_component_array_section_bounds

    integer function count_text_character(text, wanted) result(count)
        character(len=*), intent(in) :: text
        character(len=1), intent(in) :: wanted
        integer :: i

        count = 0
        do i = 1, len(text)
            if (text(i:i) == wanted) count = count + 1
        end do
    end function count_text_character

    logical function parse_integer_literal_text(text, value) result(is_literal)
        character(len=*), intent(in) :: text
        integer, intent(out) :: value
        character(len=:), allocatable :: normalized, digits
        integer :: first_digit, ios

        value = 0
        normalized = trim(adjustl(text))
        is_literal = .false.
        if (len_trim(normalized) == 0) return
        first_digit = 1
        if (normalized(1:1) == '+' .or. normalized(1:1) == '-') then
            first_digit = 2
        end if
        if (first_digit > len(normalized)) return
        digits = normalized(first_digit:)
        if (verify(digits, '0123456789') /= 0) return
        read (normalized, *, iostat=ios) value
        is_literal = ios == 0
    end function parse_integer_literal_text

    subroutine resolve_narrowed_component_receiver(arena, arm, receiver_name, &
            path, component_type_index, component_type_name, is_pointer, &
            is_allocatable, is_polymorphic, is_array, refusal_reason)
        type(ast_arena_t), intent(in) :: arena
        type(select_type_arm_query_t), intent(in) :: arm
        character(len=*), intent(in) :: receiver_name
        type(component_path_query_t), intent(out) :: path
        integer, intent(out) :: component_type_index
        character(len=:), allocatable, intent(out) :: component_type_name
        logical, intent(out) :: is_pointer, is_allocatable, is_polymorphic, is_array
        character(len=:), allocatable, intent(out) :: refusal_reason
        type(declaration_query_t) :: declaration
        type(storage_query_t) :: storage
        character(len=:), allocatable :: root_name, remaining, segment, type_name
        integer :: separator, start, next_separator, current_type, component_index
        logical :: last_segment

        call initialize_component_path(path)
        component_type_index = 0
        call set_empty(component_type_name)
        call set_empty(refusal_reason)
        is_pointer = .false.
        is_allocatable = .false.
        is_polymorphic = .false.
        is_array = .false.
        path%base_node_index = arm%selector_expression_node_index
        path%base_storage_class = arm%selector_storage%storage_class
        path%base_rank = arm%selector_storage%rank

        if (arm%selector_storage%is_pointer) then
            is_pointer = .true.
            refusal_reason = 'SELECT TYPE selector is a pointer storage boundary'
            return
        end if
        if (arm%selector_storage%is_allocatable) then
            is_allocatable = .true.
            refusal_reason = 'SELECT TYPE selector is an allocatable boundary'
            return
        end if
        if (arm%selector_storage%rank > 0 .or. &
            arm%selector_storage%is_array_element .or. &
            arm%selector_storage%is_array_section) then
            is_array = .true.
            refusal_reason = 'narrowed component receiver is array-valued'
            return
        end if

        separator = index(trim(receiver_name), '%')
        if (separator <= 1) then
            refusal_reason = 'component receiver path is absent'
            return
        end if
        root_name = trim(receiver_name(:separator - 1))
        if (arm%is_selector_associate) then
            if (.not. same_name(root_name, arm%selector_associate_name)) then
                refusal_reason = 'component receiver is not the SELECT TYPE associate'
                return
            end if
        else if (.not. same_name(root_name, arm%selector_name)) then
            refusal_reason = 'component receiver is not the SELECT TYPE selector'
            return
        end if

        remaining = trim(receiver_name(separator + 1:))
        current_type = arm%concrete_type_index
        start = 1
        do
            next_separator = index(remaining(start:), '%')
            last_segment = next_separator <= 0
            if (last_segment) then
                segment = trim(remaining(start:))
            else
                if (next_separator <= 1) then
                    refusal_reason = 'component receiver path contains an empty segment'
                    return
                end if
                segment = trim(remaining(start:start + next_separator - 2))
            end if
            if (len_trim(segment) == 0) then
                refusal_reason = 'component receiver path contains an empty segment'
                return
            end if

            component_index = find_component_in_hierarchy(arena, current_type, &
                segment)
            if (component_index <= 0) then
                refusal_reason = 'component receiver is absent from the narrowed type hierarchy'
                return
            end if
            declaration = query_declaration(arena, component_index)
            if (.not. declaration%found) then
                refusal_reason = 'component receiver declaration is unresolved'
                return
            end if
            storage = query_storage(arena, component_index)
            if (.not. storage%found) then
                refusal_reason = 'component receiver storage is unresolved'
                return
            end if
            call append_component_path(path, segment, 0, component_index)
            if (storage%is_pointer) then
                is_pointer = .true.
                path%is_pointer = .true.
                refusal_reason = 'pointer component is a dynamic storage boundary'
                return
            end if
            if (storage%is_allocatable) then
                is_allocatable = .true.
                path%is_allocatable = .true.
                refusal_reason = 'allocatable component is an ownership boundary'
                return
            end if
            if (storage%is_polymorphic .or. storage%is_unlimited_polymorphic) then
                is_polymorphic = .true.
                path%is_polymorphic = .true.
                refusal_reason = 'polymorphic component has no static binding target'
                return
            end if
            if (storage%rank > 0 .or. storage%is_array_element .or. &
                storage%is_array_section) then
                is_array = .true.
                refusal_reason = 'component receiver is array-valued'
                return
            end if
            if (.not. storage%is_derived .or. &
                .not. storage%is_concrete_derived) then
                refusal_reason = 'component receiver type is not a concrete derived type'
                return
            end if

            type_name = declared_type_name(declaration%type_name)
            if (last_segment) then
                component_type_name = type_name
                component_type_index = find_derived_type_by_name_local(arena, &
                    type_name)
                if (component_type_index <= 0) then
                    refusal_reason = 'component receiver type is unresolved'
                    return
                end if
                exit
            end if
            current_type = find_derived_type_by_name_local(arena, type_name)
            if (current_type <= 0) then
                refusal_reason = 'intermediate component type is unresolved'
                return
            end if
            start = start + next_separator
            if (start > len(remaining)) then
                refusal_reason = 'component receiver path is incomplete'
                return
            end if
        end do

        path%found = .true.
        path%storage_class = storage%storage_class
        path%rank = storage%rank
        path%is_derived = storage%is_derived
        path%is_concrete_derived = storage%is_concrete_derived
        path%is_abstract_type = storage%is_abstract_type
        path%is_allocatable = storage%is_allocatable
        path%is_pointer = storage%is_pointer
        path%is_polymorphic = storage%is_polymorphic
        path%is_unlimited_polymorphic = storage%is_unlimited_polymorphic
    end subroutine resolve_narrowed_component_receiver

    subroutine initialize_component_generic_dispatch_query(query, &
            arm_node_index, call_node_index)
        type(select_type_component_generic_dispatch_query_t), intent(out) :: query
        integer, intent(in) :: arm_node_index, call_node_index

        query%arm_node_index = arm_node_index
        query%call_node_index = call_node_index
        call set_empty(query%selector_name)
        call set_empty(query%receiver_name)
        call set_empty(query%component_type_name)
        call set_empty(query%generic_name)
        call set_empty(query%refusal_reason)
        call initialize_component_path(query%receiver_path)
        allocate (query%candidates(0))
    end subroutine initialize_component_generic_dispatch_query

    subroutine refuse_component_generic_dispatch(query, reason)
        type(select_type_component_generic_dispatch_query_t), intent(inout) :: query
        character(len=*), intent(in) :: reason

        query%is_refused = .true.
        query%is_unresolved = .true.
        if (len_trim(query%refusal_reason) == 0) then
            query%refusal_reason = trim(reason)
        end if
    end subroutine refuse_component_generic_dispatch

    subroutine initialize_generic_dispatch_query(query, arm_node_index, &
            call_node_index)
        type(select_type_generic_dispatch_query_t), intent(out) :: query
        integer, intent(in) :: arm_node_index, call_node_index

        query%arm_node_index = arm_node_index
        query%call_node_index = call_node_index
        call set_empty(query%selector_name)
        call set_empty(query%concrete_type_name)
        call set_empty(query%generic_name)
        call set_empty(query%refusal_reason)
        allocate (query%candidates(0))
    end subroutine initialize_generic_dispatch_query

    subroutine initialize_type_bound_generic_dispatch_query(query, &
            call_node_index)
        type(type_bound_generic_dispatch_query_t), intent(out) :: query
        integer, intent(in) :: call_node_index

        query%call_node_index = call_node_index
        call set_empty(query%receiver_name)
        call set_empty(query%declared_type_name)
        call set_empty(query%generic_name)
        call set_empty(query%pass_name)
        call set_empty(query%selected_pass_name)
        call set_empty(query%refusal_reason)
        call initialize_storage(query%receiver_storage)
        allocate (query%candidates(0))
    end subroutine initialize_type_bound_generic_dispatch_query

    subroutine refuse_type_bound_generic(query, reason)
        type(type_bound_generic_dispatch_query_t), intent(inout) :: query
        character(len=*), intent(in) :: reason

        query%is_refused = .true.
        query%is_unresolved = .true.
        if (len_trim(query%refusal_reason) == 0) then
            query%refusal_reason = trim(reason)
        end if
    end subroutine refuse_type_bound_generic

    subroutine initialize_owned_array_generic_query(query, arm_node_index, &
            call_node_index)
        type(select_type_owned_array_generic_dispatch_query_t), intent(out) :: query
        integer, intent(in) :: arm_node_index, call_node_index

        query%arm_node_index = arm_node_index
        query%call_node_index = call_node_index
        call set_empty(query%selector_name)
        call set_empty(query%receiver_name)
        call set_empty(query%declared_type_name)
        call set_empty(query%dynamic_type_name)
        call set_empty(query%generic_name)
        call set_empty(query%pass_name)
        call set_empty(query%selected_pass_name)
        call set_empty(query%refusal_reason)
        call initialize_storage(query%receiver_storage)
        allocate (query%candidates(0))
    end subroutine initialize_owned_array_generic_query

    subroutine initialize_owned_array_dispatch_query(query, arm_node_index, &
            call_node_index)
        type(select_type_owned_array_dispatch_query_t), intent(out) :: query
        integer, intent(in) :: arm_node_index, call_node_index

        query%arm_node_index = arm_node_index
        query%call_node_index = call_node_index
        call set_empty(query%selector_name)
        call set_empty(query%receiver_name)
        call set_empty(query%declared_type_name)
        call set_empty(query%dynamic_type_name)
        call set_empty(query%binding_name)
        call set_empty(query%declaring_type_name)
        call set_empty(query%implementation)
        call set_empty(query%pass_name)
        call set_empty(query%implementation_pass_name)
        call set_empty(query%implementation_passed_object_type)
        call set_empty(query%refusal_reason)
        call initialize_storage(query%receiver_storage)
    end subroutine initialize_owned_array_dispatch_query

    subroutine copy_owned_array_dispatch_identity(query)
        type(select_type_owned_array_dispatch_query_t), intent(inout) :: query

        query%found = query%owned_array%found
        query%is_owned_array = query%owned_array%is_owned_array
        query%has_global_mutable_state = &
            query%owned_array%has_global_mutable_state
        query%has_unresolved_alias = query%owned_array%has_unresolved_alias
        query%has_control_flow_boundary = &
            query%owned_array%has_control_flow_boundary
        query%select_type_node_index = &
            query%owned_array%select_type_node_index
        query%selector_declaration_index = &
            query%owned_array%selector_declaration_index
        query%declared_type_index = query%owned_array%declared_type_index
        query%dynamic_type_index = query%owned_array%dynamic_type_index
        query%selector_name = query%owned_array%selector_name
        query%declared_type_name = query%owned_array%declared_type_name
        query%dynamic_type_name = query%owned_array%dynamic_type_name
    end subroutine copy_owned_array_dispatch_identity

    subroutine refuse_owned_array_dispatch(query, reason)
        type(select_type_owned_array_dispatch_query_t), intent(inout) :: query
        character(len=*), intent(in) :: reason

        query%is_refused = .true.
        query%is_unresolved = .true.
        if (len_trim(query%refusal_reason) == 0) then
            query%refusal_reason = trim(reason)
        end if
    end subroutine refuse_owned_array_dispatch

    subroutine copy_owned_array_generic_identity(query)
        type(select_type_owned_array_generic_dispatch_query_t), intent(inout) :: query

        query%found = query%owned_array%found
        query%is_owned_array = query%owned_array%is_owned_array
        query%has_global_mutable_state = &
            query%owned_array%has_global_mutable_state
        query%has_unresolved_alias = query%owned_array%has_unresolved_alias
        query%has_control_flow_boundary = &
            query%owned_array%has_control_flow_boundary
        query%select_type_node_index = &
            query%owned_array%select_type_node_index
        query%selector_declaration_index = &
            query%owned_array%selector_declaration_index
        query%declared_type_index = query%owned_array%declared_type_index
        query%dynamic_type_index = query%owned_array%dynamic_type_index
        query%selector_name = query%owned_array%selector_name
        query%declared_type_name = query%owned_array%declared_type_name
        query%dynamic_type_name = query%owned_array%dynamic_type_name
    end subroutine copy_owned_array_generic_identity

    subroutine resolve_owned_array_generic_candidates(arena, call_node_index, &
            binding, query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        type(type_binding_query_t), intent(in) :: binding
        type(select_type_owned_array_generic_dispatch_query_t), intent(inout) :: query
        type(declaration_binding_t) :: candidate_binding
        character(len=:), allocatable :: error_msg
        integer, allocatable :: actual_indices(:)
        type(binding_hierarchy_query_t) :: specific_hierarchy
        integer :: i, match_count, selected

        if (.not. allocated(binding%generic_names) .or. &
            size(binding%generic_names) == 0) then
            call refuse_owned_array_generic(query, &
                'owned-array generic has no concrete specific names')
            return
        end if

        call generic_call_actuals(arena, call_node_index, actual_indices)
        deallocate (query%candidates)
        allocate (query%candidates(size(binding%generic_names)))
        match_count = 0
        selected = 0
        do i = 1, size(binding%generic_names)
            query%candidates(i)%procedure_name = binding%generic_names(i)
            call resolve_name_at_node(arena, call_node_index, &
                binding%generic_names(i), candidate_binding, error_msg)
            if (.not. candidate_binding%found .or. &
                (candidate_binding%binding_kind /= BINDING_FUNCTION .and. &
                candidate_binding%binding_kind /= BINDING_SUBROUTINE)) then
                query%candidates(i)%has_unknown_types = .true.
                cycle
            end if
            query%candidates(i)%procedure_node_index = candidate_binding%node_index
            query%candidates(i)%implementation_node_index = candidate_binding%node_index
            query%candidates(i)%implementation = binding%generic_names(i)
            query%candidates(i)%signature = query_procedure_signature(arena, &
                candidate_binding%node_index)
            query%candidates(i)%found = query%candidates(i)%signature%found
            if (.not. query%candidates(i)%found) then
                query%candidates(i)%has_unknown_types = .true.
                cycle
            end if
            specific_hierarchy = query_type_binding_hierarchy(arena, &
                query%dynamic_type_index, binding%generic_names(i))
            if (.not. specific_hierarchy%found) then
                query%candidates(i)%has_unknown_types = .true.
                cycle
            end if
            query%candidates(i)%pass_metadata_resolved = .true.
            query%candidates(i)%pass_arg = specific_hierarchy%pass_arg
            query%candidates(i)%is_nopass = .not. specific_hierarchy%pass_arg
            query%candidates(i)%pass_name = specific_hierarchy%pass_name
            if (.not. query%candidates(i)%pass_arg) then
                query%candidates(i)%pass_position = 0
            else if (len_trim(query%candidates(i)%pass_name) == 0) then
                query%candidates(i)%pass_position = 1
            else
                query%candidates(i)%pass_position = find_signature_dummy( &
                    query%candidates(i)%signature, &
                    query%candidates(i)%pass_name)
            end if
            if (query%candidates(i)%pass_arg .and. &
                query%candidates(i)%pass_position <= 0) then
                query%candidates(i)%has_unknown_types = .true.
                cycle
            end if
            call match_generic_candidate(arena, actual_indices, &
                query%candidates(i), query%candidates(i)%pass_arg, &
                query%candidates(i)%pass_name)
            if (query%candidates(i)%is_match) then
                match_count = match_count + 1
                selected = i
            end if
        end do

        query%found = .true.
        if (match_count > 1) then
            query%is_ambiguous = .true.
            call refuse_owned_array_generic(query, &
                'more than one owned-array generic specific matches exactly')
        else if (match_count == 0) then
            call refuse_owned_array_generic(query, &
                'no owned-array generic specific matches exactly')
        else
            query%selected_candidate_index = selected
            query%selected_procedure_node_index = &
                query%candidates(selected)%procedure_node_index
            query%signature = query%candidates(selected)%signature
            query%selected_pass_metadata_resolved = &
                query%candidates(selected)%pass_metadata_resolved
            query%selected_pass_arg = query%candidates(selected)%pass_arg
            query%selected_is_nopass = query%candidates(selected)%is_nopass
            query%selected_pass_position = &
                query%candidates(selected)%pass_position
            query%selected_pass_name = query%candidates(selected)%pass_name
            query%is_resolved = .true.
        end if
    end subroutine resolve_owned_array_generic_candidates

    logical function owned_array_element_receiver(receiver, selector) result(is_element)
        character(len=*), intent(in) :: receiver, selector
        character(len=:), allocatable :: prefix
        integer :: open_paren

        is_element = .false.
        if (len_trim(receiver) <= len_trim(selector)) return
        prefix = trim(receiver)
        open_paren = index(prefix, '(')
        if (open_paren /= len_trim(selector) + 1) return
        if (.not. same_name(prefix(:open_paren - 1), selector)) return
        is_element = len_trim(prefix(open_paren + 1:)) > 1 .and. &
            prefix(len_trim(prefix):len_trim(prefix)) == ')'
    end function owned_array_element_receiver

    logical function owned_array_receiver_is_section(receiver) result(is_section)
        character(len=*), intent(in) :: receiver
        integer :: open_paren, close_paren

        is_section = .false.
        open_paren = index(trim(receiver), '(')
        close_paren = index(trim(receiver), ')', back=.true.)
        if (open_paren <= 0 .or. close_paren <= open_paren) return
        is_section = index(receiver(open_paren + 1:close_paren - 1), ':') > 0
    end function owned_array_receiver_is_section

    subroutine refuse_owned_array_generic(query, reason)
        type(select_type_owned_array_generic_dispatch_query_t), intent(inout) :: query
        character(len=*), intent(in) :: reason

        query%is_refused = .true.
        query%is_unresolved = .true.
        if (len_trim(query%refusal_reason) == 0) then
            query%refusal_reason = trim(reason)
        end if
    end subroutine refuse_owned_array_generic

    subroutine refuse_generic_dispatch(query, reason)
        type(select_type_generic_dispatch_query_t), intent(inout) :: query
        character(len=*), intent(in) :: reason

        query%is_refused = .true.
        query%is_unresolved = .true.
        if (len_trim(query%refusal_reason) == 0) then
            query%refusal_reason = trim(reason)
        end if
    end subroutine refuse_generic_dispatch

    subroutine generic_call_actuals(arena, call_node_index, actual_indices)
        !! Preserve actuals for both CALL statements and function references.
        !! Matching remains exact and is performed by the caller; this helper
        !! only exposes the source-owned argument list.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        integer, allocatable, intent(out) :: actual_indices(:)

        allocate (actual_indices(0))
        if (.not. arena%has_node_at(call_node_index)) return
        select type (node => arena%entries(call_node_index)%node)
            type is (subroutine_call_node)
            if (allocated(node%arg_indices)) actual_indices = node%arg_indices
            type is (call_or_subscript_node)
            if (allocated(node%arg_indices)) actual_indices = node%arg_indices
        class default
        end select
    end subroutine generic_call_actuals

    subroutine match_generic_candidate(arena, actual_indices, candidate, &
            pass_arg, pass_name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: actual_indices(:)
        type(select_type_generic_candidate_query_t), intent(inout) :: candidate
        logical, intent(in) :: pass_arg
        character(len=*), intent(in) :: pass_name
        integer, allocatable :: value_for_dummy(:)
        logical, allocatable :: supplied(:)
        type(resolved_type_query_t) :: actual_type
        type(declaration_query_t) :: formal_declaration
        character(len=:), allocatable :: keyword, formal_type
        integer :: pass_position, next_dummy, dummy, i, actual_value
        logical :: is_keyword, valid

        if (.not. candidate%signature%found) return
        allocate (value_for_dummy(candidate%signature%dummy_count))
        allocate (supplied(candidate%signature%dummy_count))
        value_for_dummy = 0
        supplied = .false.
        pass_position = 0
        if (pass_arg) then
            if (len_trim(pass_name) == 0) then
                ! Fortran's default PASS object is the first dummy.
                pass_position = 1
            else
                pass_position = find_signature_dummy(candidate%signature, &
                    pass_name)
            end if
            if (pass_position <= 0) return
        end if

        next_dummy = 1
        valid = .true.
        do i = 1, size(actual_indices)
            call generic_actual_info(arena, actual_indices(i), keyword, &
                actual_value, is_keyword)
            if (actual_value <= 0) then
                valid = .false.
                exit
            end if
            if (is_keyword) then
                dummy = find_signature_dummy(candidate%signature, keyword)
                if (dummy == pass_position) dummy = 0
            else
                do while (next_dummy <= candidate%signature%dummy_count .and. &
                        (next_dummy == pass_position .or. supplied(next_dummy)))
                    next_dummy = next_dummy + 1
                end do
                dummy = next_dummy
                next_dummy = next_dummy + 1
            end if
            if (dummy <= 0 .or. dummy > candidate%signature%dummy_count .or. &
                supplied(dummy)) then
                valid = .false.
                exit
            end if
            value_for_dummy(dummy) = actual_value
            supplied(dummy) = .true.
        end do
        if (.not. valid) return

        do i = 1, candidate%signature%dummy_count
            if (i == pass_position) cycle
            if (.not. supplied(i)) then
                if (.not. candidate%signature%dummies(i)%is_optional) then
                    return
                end if
                cycle
            end if
            if (.not. candidate%signature%dummies(i)%type_known .or. &
                .not. candidate%signature%dummies(i)%kind_known .or. &
                .not. candidate%signature%dummies(i)%rank_known) then
                candidate%has_unknown_types = .true.
                return
            end if
            actual_type = query_resolved_type(arena, value_for_dummy(i))
            if (.not. actual_type%found) then
                candidate%has_unknown_types = .true.
                return
            end if
            if (candidate%signature%dummies(i)%type_kind /= &
                actual_type%type_kind .or. &
                candidate%signature%dummies(i)%kind_value /= &
                actual_type%kind_value .or. &
                candidate%signature%dummies(i)%rank /= actual_type%rank) return
            formal_declaration = query_declaration(arena, &
                candidate%signature%dummies(i)%node_index)
            formal_type = ''
            if (formal_declaration%found) then
                if (is_derived_type_spec(formal_declaration%type_name)) then
                    formal_type = declared_type_name(formal_declaration%type_name)
                end if
            end if
            if (len_trim(formal_type) > 0 .or. &
                len_trim(actual_type%derived_type_name) > 0) then
                if (.not. same_name(formal_type, actual_type%derived_type_name)) &
                    return
            end if
        end do
        candidate%is_match = .true.
    end subroutine match_generic_candidate

    logical function is_derived_type_spec(type_spec) result(is_derived)
        character(len=*), intent(in) :: type_spec
        character(len=:), allocatable :: lowered

        lowered = to_lower(trim(type_spec))
        is_derived = index(lowered, 'type(') == 1 .or. &
            index(lowered, 'class(') == 1
    end function is_derived_type_spec

    integer function find_signature_dummy(signature, name) result(position)
        type(procedure_signature_query_t), intent(in) :: signature
        character(len=*), intent(in) :: name
        integer :: i

        position = 0
        do i = 1, signature%dummy_count
            if (same_name(signature%dummies(i)%name, name)) then
                position = i
                return
            end if
        end do
    end function find_signature_dummy

    subroutine generic_actual_info(arena, actual_index, keyword, value_index, &
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
            if (actual%target_index <= 0 .or. actual%value_index <= 0) return
            if (.not. arena%has_node_at(actual%target_index)) return
            select type (target => arena%entries(actual%target_index)%node)
                type is (identifier_node)
                if (.not. allocated(target%name)) return
                keyword = target%name
                value_index = actual%value_index
                is_keyword = .true.
            class default
            end select
        class default
        end select
    end subroutine generic_actual_info

    subroutine initialize_query(query, arm_node_index, call_node_index)
        type(select_type_dispatch_query_t), intent(out) :: query
        integer, intent(in) :: arm_node_index, call_node_index

        query%arm_node_index = arm_node_index
        query%call_node_index = call_node_index
        call set_empty(query%selector_name)
        call set_empty(query%guard_kind)
        call set_empty(query%guard_type_name)
        call set_empty(query%declared_type_name)
        call set_empty(query%concrete_type_name)
        call set_empty(query%declaring_type_name)
        call set_empty(query%receiver_name)
        call set_empty(query%binding_name)
        call set_empty(query%implementation)
        call set_empty(query%pass_name)
        call set_empty(query%implementation_pass_name)
        call set_empty(query%implementation_passed_object_type)
        call set_empty(query%refusal_reason)
    end subroutine initialize_query

    subroutine copy_arm_facts(query, arm)
        type(select_type_dispatch_query_t), intent(inout) :: query
        type(select_type_arm_query_t), intent(in) :: arm

        query%arm_ordinal = arm%arm_ordinal
        query%selector_node_index = arm%selector_node_index
        query%selector_declaration_index = arm%selector_declaration_index
        query%guard_type_node_index = arm%type_name_node_index
        query%concrete_type_index = arm%concrete_type_index
        query%declared_type_index = arm%declared_type_index
        query%arm_source_line = arm%source_line
        query%arm_source_column = arm%source_column
        query%arm_entry_node_index = arm%body_entry_node_index
        query%arm_exit_node_index = arm%body_exit_node_index
        query%dispatch_boundary_known = arm%dispatch_boundary_known
        query%selector_name = arm%selector_name
        query%guard_type_name = arm%concrete_type_name
        query%declared_type_name = arm%declared_type_name
        query%concrete_type_name = arm%concrete_type_name
        query%is_selector_resolved = arm%is_selector_resolved
        if (arm%is_type_is) query%guard_kind = 'type_is'
        if (arm%is_class_is) query%guard_kind = 'class_is'
        if (arm%is_class_default) query%guard_kind = 'class_default'
    end subroutine copy_arm_facts

    subroutine copy_hierarchy_facts(query, hierarchy, arm)
        type(select_type_dispatch_query_t), intent(inout) :: query
        type(binding_hierarchy_query_t), intent(in) :: hierarchy
        type(select_type_arm_query_t), intent(in) :: arm

        query%resolved_type_index = arm%concrete_type_index
        query%declaring_type_index = hierarchy%declaring_type_index
        query%binding_node_index = hierarchy%binding_node_index
        query%implementation_node_index = hierarchy%implementation_node_index
        query%declaring_type_name = hierarchy%declaring_type_name
        query%binding_name = hierarchy%binding_name
        query%implementation = hierarchy%implementation
        query%pass_name = hierarchy%pass_name
        query%is_inherited = hierarchy%is_inherited
        query%implementation_pass_name = hierarchy%implementation_pass_name
        query%implementation_pass_position = &
            hierarchy%implementation_pass_position
        query%implementation_passed_object_type = &
            hierarchy%implementation_passed_object_type
        query%binding_pass_position = hierarchy%implementation_pass_position
        query%pass_arg = hierarchy%pass_arg
        query%is_nopass = .not. hierarchy%pass_arg
    end subroutine copy_hierarchy_facts

    subroutine classify_receiver_storage(query, arm)
        type(select_type_dispatch_query_t), intent(inout) :: query
        type(select_type_arm_query_t), intent(in) :: arm
        type(storage_query_t) :: storage

        storage = arm%selector_storage
        if (.not. storage%found) return
        if (storage%rank > 0 .or. storage%is_array_element .or. &
            storage%is_array_section) then
            query%is_array_receiver = .true.
            call refuse(query, 'SELECT TYPE selector is array-valued')
        end if
        if (storage%is_pointer .or. storage%is_allocatable .or. &
            storage%is_component .or. storage%is_module_state .or. &
            storage%is_save_state .or. storage%is_common_state) then
            query%is_ownership_changing = .true.
            call refuse(query, &
                'SELECT TYPE selector has an ownership-changing storage edge')
        end if
    end subroutine classify_receiver_storage

    subroutine check_pass_compatibility(arena, query, hierarchy)
        type(ast_arena_t), intent(in) :: arena
        type(select_type_dispatch_query_t), intent(inout) :: query
        type(binding_hierarchy_query_t), intent(in) :: hierarchy
        character(len=:), allocatable :: passed_type, concrete_name
        integer :: passed_type_index
        logical :: passed_type_compatible

        if (.not. hierarchy%pass_arg) return
        if (.not. hierarchy%implementation_signature_resolved .or. &
            hierarchy%implementation_pass_position <= 0) then
            query%is_incompatible_pass = .true.
            call refuse(query, 'implementation PASS argument is incompatible')
            return
        end if
        passed_type = normalized_pass_type( &
            hierarchy%implementation_passed_object_type)
        concrete_name = to_lower(trim(query%concrete_type_name))
        passed_type_index = find_derived_type_by_name_local(arena, passed_type)
        passed_type_compatible = same_name(passed_type, concrete_name)
        if (.not. passed_type_compatible .and. passed_type_index > 0 .and. &
            query%concrete_type_index > 0) then
            passed_type_compatible = type_extends(arena, &
                query%concrete_type_index, passed_type_index)
        end if
        if (len_trim(passed_type) == 0 .or. .not. passed_type_compatible) then
            query%is_incompatible_pass = .true.
            call refuse(query, &
                'implementation PASS type is outside guard hierarchy')
        end if
        if (query%implementation_pass_position > query%signature%dummy_count) then
            query%is_incompatible_pass = .true.
            call refuse(query, 'implementation PASS position is outside signature')
        end if
    end subroutine check_pass_compatibility

    function normalized_pass_type(type_spec) result(normalized)
        character(len=*), intent(in) :: type_spec
        character(len=:), allocatable :: normalized
        integer :: left, right

        normalized = to_lower(trim(type_spec))
        left = index(normalized, '(')
        right = index(normalized, ')', back=.true.)
        if (left > 0 .and. right > left) then
            normalized = trim(normalized(left + 1:right - 1))
        end if
    end function normalized_pass_type

    logical function direct_call_in_arm(arena, arm, call_node_index, query) &
            result(is_direct)
        type(ast_arena_t), intent(in) :: arena
        type(select_type_arm_query_t), intent(in) :: arm
        integer, intent(in) :: call_node_index
        type(select_type_dispatch_query_t), intent(inout) :: query
        integer :: body_index
        integer :: parent_index

        is_direct = .false.
        if (.not. allocated(arm%body_node_indices)) return
        if (size(arm%body_node_indices) /= 1) then
            query%is_nested = .true.
            return
        end if
        body_index = arm%body_node_indices(1)
        if (body_index == call_node_index) then
            parent_index = arena%entries(call_node_index)%parent_index
            if (parent_index /= arm%arm_node_index) then
                query%is_nested = .true.
                return
            end if
        else
            ! A function reference is accepted only when it is the complete
            ! RHS of the arm's sole assignment.  In particular, do not infer
            ! through arithmetic, nested calls, or an array subscript.
            if (.not. arena%has_node_at(body_index)) return
            select type (body => arena%entries(body_index)%node)
                type is (assignment_node)
                if (body%value_index /= call_node_index) return
                if (.not. arena%has_node_at(call_node_index)) return
                select type (call => arena%entries(call_node_index)%node)
                    type is (call_or_subscript_node)
                    if (call%is_array_access) return
                    parent_index = arena%entries(call_node_index)%parent_index
                    if (parent_index /= body_index) then
                        query%is_nested = .true.
                        return
                    end if
                    query%is_function_reference = .true.
                class default
                    return
                end select
                parent_index = arena%entries(body_index)%parent_index
                if (parent_index /= arm%arm_node_index) then
                    query%is_nested = .true.
                    return
                end if
            class default
                return
            end select
        end if
        query%call_source_line = arena%entries(call_node_index)%node%line
        query%call_source_column = arena%entries(call_node_index)%node%column
        is_direct = .true.
    end function direct_call_in_arm

    logical function is_explicit_call(arena, node_index) result(is_call)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_call = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (subroutine_call_node)
            is_call = .true.
        end select
    end function is_explicit_call

    integer function enclosing_select_type(arena, node_index) result(select_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: current, guard

        select_index = 0
        current = node_index
        guard = 0
        do while (current > 0)
            if (.not. arena%has_node_at(current)) exit
            select type (node => arena%entries(current)%node)
                type is (select_type_node)
                select_index = current
                return
            class default
            end select
            current = arena%entries(current)%parent_index
            guard = guard + 1
            if (guard > arena%size) exit
        end do
    end function enclosing_select_type

    subroutine refuse(query, reason)
        type(select_type_dispatch_query_t), intent(inout) :: query
        character(len=*), intent(in) :: reason

        query%is_refused = .true.
        if (len_trim(query%refusal_reason) == 0) query%refusal_reason = trim(reason)
    end subroutine refuse

    subroutine refuse_unresolved(query, reason)
        type(select_type_dispatch_query_t), intent(inout) :: query
        character(len=*), intent(in) :: reason

        query%is_unresolved = .true.
        call refuse(query, reason)
    end subroutine refuse_unresolved

    subroutine initialize_owned_array_query(query, arm_node_index)
        type(select_type_owned_array_query_t), intent(out) :: query
        integer, intent(in) :: arm_node_index

        query%arm_node_index = arm_node_index
        call set_empty(query%selector_name)
        call set_empty(query%selector_associate_name)
        call set_empty(query%declared_type_name)
        call set_empty(query%dynamic_type_name)
        call set_empty(query%refusal_reason)
        call initialize_storage(query%selector_storage)
    end subroutine initialize_owned_array_query

    subroutine initialize_owned_array_binding_query(query, arm_node_index, &
            binding_name)
        type(select_type_owned_array_binding_query_t), intent(out) :: query
        integer, intent(in) :: arm_node_index
        character(len=*), intent(in) :: binding_name

        query%arm_node_index = arm_node_index
        call set_empty(query%selector_name)
        call set_empty(query%declared_type_name)
        call set_empty(query%dynamic_type_name)
        call set_empty(query%binding_name)
        query%binding_name = trim(binding_name)
        call set_empty(query%declaring_type_name)
        call set_empty(query%implementation)
        call set_empty(query%refusal_reason)
    end subroutine initialize_owned_array_binding_query

    subroutine refuse_owned_array(query, reason)
        type(select_type_owned_array_query_t), intent(inout) :: query
        character(len=*), intent(in) :: reason

        query%is_refused = .true.
        query%is_unresolved = .true.
        if (len_trim(query%refusal_reason) == 0) then
            query%refusal_reason = trim(reason)
        end if
    end subroutine refuse_owned_array

    subroutine refuse_owned_array_binding(query, reason)
        type(select_type_owned_array_binding_query_t), intent(inout) :: query
        character(len=*), intent(in) :: reason

        query%is_refused = .true.
        query%is_unresolved = .true.
        if (len_trim(query%refusal_reason) == 0) then
            query%refusal_reason = trim(reason)
        end if
    end subroutine refuse_owned_array_binding

    logical function owned_array_has_control_flow_boundary(arena, arm_index) &
            result(has_boundary)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arm_index
        integer :: current, guard

        has_boundary = .false.
        if (.not. arena%has_node_at(arm_index)) return
        current = arena%entries(arm_index)%parent_index
        guard = 0
        do while (current > 0)
            if (.not. arena%has_node_at(current)) exit
            if (owned_array_is_control_flow_node(arena, current)) then
                has_boundary = .true.
                return
            end if
            current = arena%entries(current)%parent_index
            guard = guard + 1
            if (guard > arena%size) exit
        end do
    end function owned_array_has_control_flow_boundary

    logical function owned_array_body_has_control_flow(arena, arm) &
            result(has_boundary)
        type(ast_arena_t), intent(in) :: arena
        type(select_type_arm_query_t), intent(in) :: arm
        integer :: i

        has_boundary = .false.
        if (.not. allocated(arm%body_node_indices)) return
        do i = 1, size(arm%body_node_indices)
            if (.not. arena%has_node_at(arm%body_node_indices(i))) cycle
            if (owned_array_is_control_flow_node(arena, &
                arm%body_node_indices(i))) then
                has_boundary = .true.
                return
            end if
        end do
    end function owned_array_body_has_control_flow

    logical function owned_array_is_control_flow_node(arena, node_index) &
            result(is_control_flow)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_control_flow = .false.
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (if_node)
            is_control_flow = .true.
            type is (do_loop_node)
            is_control_flow = .true.
            type is (do_while_node)
            is_control_flow = .true.
            type is (forall_node)
            is_control_flow = .true.
            type is (where_node)
            is_control_flow = .true.
            type is (select_case_node)
            is_control_flow = .true.
            type is (select_rank_node)
            is_control_flow = .true.
        class default
        end select
    end function owned_array_is_control_flow_node

    subroutine set_empty(value)
        character(len=:), allocatable, intent(out) :: value

        allocate (character(len=0) :: value)
    end subroutine set_empty

end module frontend_compiler_select_type_queries
