module frontend_compiler_select_type_queries
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: assignment_node, identifier_node
    use ast_nodes_conditional, only: select_type_node, type_guard_block_node
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
        query_type_binding, type_binding_query_t
    use frontend_compiler_resolution, only: declaration_binding_t, &
        resolve_name_at_node, BINDING_FUNCTION, BINDING_SUBROUTINE
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

    type, public :: select_type_dispatch_query_t
        !! Facts for one direct type-bound CALL in one concrete SELECT TYPE arm.
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
        logical :: found = .false.
        logical :: is_match = .false.
        logical :: has_unknown_types = .false.
        integer :: procedure_node_index = 0
        integer :: implementation_node_index = 0
        character(len=:), allocatable :: procedure_name
        character(len=:), allocatable :: implementation
        type(procedure_signature_query_t) :: signature
    end type select_type_generic_candidate_query_t

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

    public :: query_select_type_branch, query_select_type_component_path, &
        query_select_type_component_binding, query_select_type_dispatch, &
        query_select_type_generic_dispatch, &
        query_select_type_component_generic_dispatch

contains

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
                    call refuse(query, 'abstract SELECT TYPE guard is not concrete')
                end if
            end if
        end if

        direct_call = direct_call_in_arm(arena, arm, call_node_index, query)
        if (.not. direct_call) then
            call refuse(query, 'call is not the single direct arm statement')
        end if
        if (.not. is_explicit_call(arena, call_node_index)) then
            query%is_dynamic_receiver = .true.
            call refuse(query, 'call is not an explicit direct CALL statement')
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
        call check_pass_compatibility(query, hierarchy)
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

    subroutine resolve_generic_candidates(arena, call_node_index, binding, query)
        !! Fill the exact candidate set shared by selector and component calls.
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        type(type_binding_query_t), intent(in) :: binding
        type(select_type_generic_dispatch_query_t), intent(inout) :: query
        type(declaration_binding_t) :: candidate_binding
        character(len=:), allocatable :: error_msg
        integer, allocatable :: actual_indices(:)
        integer :: i, match_count, selected

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
            call match_generic_candidate(arena, actual_indices, &
                query%candidates(i), binding%pass_arg, binding%pass_name)
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
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: call_node_index
        integer, allocatable, intent(out) :: actual_indices(:)

        allocate (actual_indices(0))
        if (.not. arena%has_node_at(call_node_index)) return
        select type (node => arena%entries(call_node_index)%node)
            type is (subroutine_call_node)
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

    subroutine check_pass_compatibility(query, hierarchy)
        type(select_type_dispatch_query_t), intent(inout) :: query
        type(binding_hierarchy_query_t), intent(in) :: hierarchy
        character(len=:), allocatable :: passed_type, concrete_name

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
        if (len_trim(passed_type) == 0 .or. &
            .not. same_name(passed_type, concrete_name)) then
            query%is_incompatible_pass = .true.
            call refuse(query, 'implementation PASS type does not match guard')
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
        integer :: parent_index

        is_direct = .false.
        if (.not. allocated(arm%body_node_indices)) return
        if (size(arm%body_node_indices) /= 1) then
            query%is_nested = .true.
            return
        end if
        if (arm%body_node_indices(1) /= call_node_index) then
            query%is_nested = .true.
            return
        end if
        parent_index = arena%entries(call_node_index)%parent_index
        if (parent_index /= arm%arm_node_index) then
            query%is_nested = .true.
            return
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

    subroutine set_empty(value)
        character(len=:), allocatable, intent(out) :: value

        allocate (character(len=0) :: value)
    end subroutine set_empty

end module frontend_compiler_select_type_queries
