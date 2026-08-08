module frontend_compiler_select_type_queries
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_conditional, only: select_type_node, type_guard_block_node
    use ast_nodes_procedure, only: subroutine_call_node
    use frontend_compiler_control_queries, only: control_statement_query_t, &
        select_type_arm_query_t, query_control_statement, CONTROL_SELECT_TYPE
    use frontend_compiler_queries, only: type_bound_call_query_t, &
        query_type_bound_call, binding_hierarchy_query_t, &
        query_type_binding_hierarchy, procedure_signature_query_t, &
        query_procedure_signature, derived_type_query_t, query_derived_type, &
        storage_query_t
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

    public :: query_select_type_branch, query_select_type_dispatch

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
