module frontend_compiler_control_queries
    use ast_arena_modern, only: ast_arena_t
    use frontend_compiler_queries, only: storage_query_t, component_path_query_t, &
        query_storage, query_component_path
    use frontend_compiler_resolution, only: declaration_binding_t, &
        resolve_identifier_binding
    use ast_nodes_associate, only: associate_node, block_construct_node
    use ast_nodes_conditional, only: select_type_node, type_guard_block_node, &
        select_rank_node, rank_block_node
    use ast_nodes_array, only: where_node, where_stmt_node
    implicit none
    private

    integer, parameter, public :: CONTROL_ASSOCIATE = 1
    integer, parameter, public :: CONTROL_BLOCK = 2
    integer, parameter, public :: CONTROL_SELECT_TYPE = 3
    integer, parameter, public :: CONTROL_TYPE_GUARD = 4
    integer, parameter, public :: CONTROL_SELECT_RANK = 5
    integer, parameter, public :: CONTROL_RANK_BLOCK = 6
    integer, parameter, public :: CONTROL_WHERE = 7
    integer, parameter, public :: CONTROL_WHERE_STATEMENT = 8

    integer, parameter, public :: SELECT_RANK_DISPATCH_EXPLICIT = 1
    integer, parameter, public :: SELECT_RANK_DISPATCH_ASSUMED_SIZE = 2
    integer, parameter, public :: SELECT_RANK_DISPATCH_DEFAULT = 3

    type, public :: association_query_t
        character(len=:), allocatable :: name
        integer :: expression_node_index = 0
    end type association_query_t

    type, public :: elsewhere_clause_query_t
        logical :: has_mask = .false.
        integer :: mask_node_index = 0
        integer, allocatable :: body_node_indices(:)
    end type elsewhere_clause_query_t

    type, public :: select_rank_arm_query_t
        !! Facts for one SELECT RANK dispatch arm.
        !!
        !! The record describes parser/source and name-resolution facts only.
        !! It does not infer a derivative for the selected rank.  POINTER,
        !! polymorphic, and unresolved selectors retain explicit boundaries.
        logical :: found = .false.
        logical :: has_rank = .false.
        logical :: is_default = .false.
        logical :: is_assumed_size = .false.
        logical :: has_selector = .false.
        logical :: is_storage_resolved = .false.
        logical :: is_component_path_available = .false.
        logical :: is_pointer_selector = .false.
        logical :: is_polymorphic_selector = .false.
        logical :: is_dynamic_type_known = .false.
        logical :: is_dynamic_ownership_unresolved = .false.
        logical :: is_unresolved_selector = .false.
        logical :: is_unsupported_selector = .false.
        logical :: is_refusal_boundary = .false.
        logical :: source_boundary_known = .false.
        logical :: dispatch_boundary_known = .false.
        integer :: arm_node_index = 0
        integer :: selector_node_index = 0
        integer :: selector_declaration_index = 0
        integer :: selector_storage_identity_node_index = 0
        integer :: selected_rank = -1
        integer :: dispatch_kind = 0
        integer :: source_line = 0
        integer :: source_column = 0
        integer :: body_entry_node_index = 0
        integer :: body_exit_node_index = 0
        character(len=:), allocatable :: selector_name
        character(len=:), allocatable :: refusal_reason
        integer, allocatable :: body_node_indices(:)
        type(storage_query_t) :: selector_storage
        type(component_path_query_t) :: selector_path
    end type select_rank_arm_query_t

    type, public :: control_statement_query_t
        logical :: found = .false.
        integer :: statement_kind = 0
        integer :: line = 0
        integer :: column = 0
        type(association_query_t), allocatable :: associations(:)
        type(select_rank_arm_query_t), allocatable :: rank_arms(:)
        integer, allocatable :: body_node_indices(:)
        logical :: has_selector = .false.
        integer :: selector_node_index = 0
        integer, allocatable :: child_node_indices(:)
        logical :: has_default = .false.
        integer :: default_node_index = 0
        character(len=:), allocatable :: guard_type
        logical :: has_type_name = .false.
        integer :: type_name_node_index = 0
        logical :: has_rank = .false.
        integer :: rank_value = 0
        logical :: is_default = .false.
        logical :: is_assumed_size = .false.
        logical :: has_mask = .false.
        integer :: mask_node_index = 0
        type(elsewhere_clause_query_t), allocatable :: elsewhere_clauses(:)
        logical :: has_assignment = .false.
        integer :: assignment_node_index = 0
        logical :: is_single_line = .false.
    end type control_statement_query_t

    public :: query_control_statement

contains

    function query_control_statement(arena, node_index) result(query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(control_statement_query_t) :: query

        call initialize_control_query(query)
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (associate_node)
            call fill_associate_query(node, query)
            type is (block_construct_node)
            call fill_block_query(node, query)
            type is (select_type_node)
            call fill_select_type_query(node, query)
            type is (type_guard_block_node)
            call fill_type_guard_query(node, query)
            type is (select_rank_node)
            call fill_select_rank_query(arena, node, query)
            type is (rank_block_node)
            call fill_rank_block_query(node, query)
            type is (where_node)
            call fill_where_query(node, query)
            type is (where_stmt_node)
            call fill_where_statement_query(node, query)
        end select
        if (.not. query%found) return
        query%line = arena%entries(node_index)%node%line
        query%column = arena%entries(node_index)%node%column
    end function query_control_statement

    subroutine initialize_control_query(query)
        type(control_statement_query_t), intent(out) :: query

        query%guard_type = ''
        allocate (query%associations(0))
        allocate (query%rank_arms(0))
        allocate (query%body_node_indices(0))
        allocate (query%child_node_indices(0))
        allocate (query%elsewhere_clauses(0))
    end subroutine initialize_control_query

    subroutine fill_associate_query(node, query)
        type(associate_node), intent(in) :: node
        type(control_statement_query_t), intent(inout) :: query
        integer :: i

        query%found = .true.
        query%statement_kind = CONTROL_ASSOCIATE
        if (allocated(node%body_indices)) query%body_node_indices = node%body_indices
        if (.not. allocated(node%associations)) return
        deallocate (query%associations)
        allocate (query%associations(size(node%associations)))
        do i = 1, size(node%associations)
            query%associations(i)%name = ''
            if (allocated(node%associations(i)%name)) then
                query%associations(i)%name = node%associations(i)%name
            end if
            query%associations(i)%expression_node_index = &
                node%associations(i)%expr_index
        end do
    end subroutine fill_associate_query

    subroutine fill_block_query(node, query)
        type(block_construct_node), intent(in) :: node
        type(control_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%statement_kind = CONTROL_BLOCK
        if (allocated(node%body_indices)) query%body_node_indices = node%body_indices
    end subroutine fill_block_query

    subroutine fill_select_type_query(node, query)
        type(select_type_node), intent(in) :: node
        type(control_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%statement_kind = CONTROL_SELECT_TYPE
        call set_present_index(node%selector_index, query%selector_node_index, &
            query%has_selector)
        if (allocated(node%guard_indices)) then
            query%child_node_indices = node%guard_indices
        end if
        call set_present_index(node%default_index, query%default_node_index, &
            query%has_default)
    end subroutine fill_select_type_query

    subroutine fill_type_guard_query(node, query)
        type(type_guard_block_node), intent(in) :: node
        type(control_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%statement_kind = CONTROL_TYPE_GUARD
        query%guard_type = trim(node%guard_type)
        query%is_default = query%guard_type == 'class_default'
        call set_present_index(node%type_name_index, &
            query%type_name_node_index, query%has_type_name)
        if (allocated(node%body_indices)) query%body_node_indices = node%body_indices
    end subroutine fill_type_guard_query

    subroutine fill_select_rank_query(arena, node, query)
        type(ast_arena_t), intent(in) :: arena
        type(select_rank_node), intent(in) :: node
        type(control_statement_query_t), intent(inout) :: query
        integer :: i, arm_count

        query%found = .true.
        query%statement_kind = CONTROL_SELECT_RANK
        call set_present_index(node%selector_index, query%selector_node_index, &
            query%has_selector)
        if (allocated(node%rank_indices)) then
            query%child_node_indices = node%rank_indices
        end if
        call set_present_index(node%default_index, query%default_node_index, &
            query%has_default)

        arm_count = 0
        if (allocated(node%rank_indices)) arm_count = size(node%rank_indices)
        if (node%default_index > 0) arm_count = arm_count + 1
        deallocate (query%rank_arms)
        allocate (query%rank_arms(arm_count))
        do i = 1, arm_count
            if (allocated(node%rank_indices)) then
                if (i <= size(node%rank_indices)) then
                    call fill_select_rank_arm_query(arena, node%rank_indices(i), &
                        node%selector_index, query%rank_arms(i))
                    cycle
                end if
            end if
            if (node%default_index > 0) then
                call fill_select_rank_arm_query(arena, node%default_index, &
                    node%selector_index, query%rank_arms(i))
            else
                call fill_select_rank_arm_query(arena, node%rank_indices(i), &
                    node%selector_index, query%rank_arms(i))
            end if
        end do
    end subroutine fill_select_rank_query

    subroutine fill_select_rank_arm_query(arena, arm_index, selector_index, query)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arm_index, selector_index
        type(select_rank_arm_query_t), intent(out) :: query
        type(rank_block_node) :: arm
        type(declaration_binding_t) :: binding
        type(storage_query_t) :: storage
        character(len=:), allocatable :: error_message
        integer :: declaration_index

        call initialize_select_rank_arm_query(query)
        query%arm_node_index = arm_index
        query%selector_node_index = selector_index
        query%has_selector = .false.
        if (selector_index > 0) query%has_selector = arena%has_node_at(selector_index)
        if (.not. arena%has_node_at(arm_index)) then
            query%is_refusal_boundary = .true.
            query%is_unresolved_selector = .true.
            query%refusal_reason = 'rank arm node is absent'
            return
        end if
        select type (node => arena%entries(arm_index)%node)
            type is (rank_block_node)
            arm = node
        class default
            query%is_refusal_boundary = .true.
            query%refusal_reason = 'node is not a rank arm'
            return
        end select

        query%found = .true.
        query%source_line = arena%entries(arm_index)%node%line
        query%source_column = arena%entries(arm_index)%node%column
        query%source_boundary_known = query%source_line > 0
        query%dispatch_boundary_known = query%has_selector
        if (allocated(arm%body_indices)) then
            query%body_node_indices = arm%body_indices
            if (size(arm%body_indices) > 0) then
                query%body_entry_node_index = arm%body_indices(1)
                query%body_exit_node_index = arm%body_indices(size(arm%body_indices))
            end if
        end if
        if (arm%rank_value == -1) then
            query%is_default = .true.
            query%dispatch_kind = SELECT_RANK_DISPATCH_DEFAULT
        else if (arm%rank_value == -2) then
            query%is_assumed_size = .true.
            query%dispatch_kind = SELECT_RANK_DISPATCH_ASSUMED_SIZE
        else
            query%has_rank = .true.
            query%selected_rank = arm%rank_value
            query%dispatch_kind = SELECT_RANK_DISPATCH_EXPLICIT
        end if

        call set_empty(query%selector_name)
        call set_empty(query%refusal_reason)
        if (.not. query%has_selector) then
            query%is_refusal_boundary = .true.
            query%is_unresolved_selector = .true.
            query%refusal_reason = 'selector identity is absent'
            return
        end if

        query%selector_path = query_component_path(arena, selector_index, .true.)
        query%is_component_path_available = query%selector_path%found
        call resolve_identifier_binding(arena, selector_index, binding, error_message)
        declaration_index = binding%declaration_node_index
        if (declaration_index <= 0 .and. query%selector_path%found) then
            if (allocated(query%selector_path%component_declaration_indices)) then
                if (size(query%selector_path%component_declaration_indices) > 0) then
                    declaration_index = &
                        query%selector_path%component_declaration_indices(&
                        size(query%selector_path%component_declaration_indices))
                end if
            end if
        end if
        if (declaration_index > 0) then
            storage = query_storage(arena, declaration_index)
            query%selector_storage = storage
            query%is_storage_resolved = storage%found
            query%selector_declaration_index = declaration_index
            query%selector_storage_identity_node_index = storage%node_index
            if (allocated(storage%name)) query%selector_name = storage%name
            query%is_pointer_selector = storage%is_pointer
            query%is_polymorphic_selector = storage%is_polymorphic .or. &
                storage%is_unlimited_polymorphic
            query%is_dynamic_type_known = storage%found .and. &
                .not. query%is_pointer_selector .and. &
                .not. query%is_polymorphic_selector
        else
            query%is_unresolved_selector = .true.
            query%is_unsupported_selector = .true.
        end if
        query%is_dynamic_ownership_unresolved = query%is_pointer_selector .or. &
            query%is_unresolved_selector
        query%is_refusal_boundary = query%is_dynamic_ownership_unresolved
        query%is_unsupported_selector = query%is_unresolved_selector .and. &
            .not. query%is_pointer_selector
        if (query%is_pointer_selector) then
            query%refusal_reason = 'pointer selector ownership is unresolved'
        else if (query%is_unresolved_selector) then
            query%refusal_reason = 'selector storage identity is unresolved'
        else if (query%is_polymorphic_selector) then
            query%refusal_reason = 'selector dynamic type is unresolved'
        end if
    end subroutine fill_select_rank_arm_query

    subroutine initialize_select_rank_arm_query(query)
        type(select_rank_arm_query_t), intent(out) :: query

        call set_empty(query%selector_name)
        call set_empty(query%refusal_reason)
        allocate (query%body_node_indices(0))
        call initialize_storage_query(query%selector_storage)
        call initialize_component_path_query(query%selector_path)
    end subroutine initialize_select_rank_arm_query

    subroutine initialize_storage_query(query)
        type(storage_query_t), intent(out) :: query

        call set_empty(query%name)
        call set_empty(query%type_name)
    end subroutine initialize_storage_query

    subroutine initialize_component_path_query(query)
        type(component_path_query_t), intent(out) :: query

        allocate (character(len=0) :: query%component_names(0))
        allocate (query%component_node_indices(0))
        allocate (query%component_declaration_indices(0))
    end subroutine initialize_component_path_query

    subroutine set_empty(value)
        character(len=:), allocatable, intent(out) :: value

        allocate (character(len=0) :: value)
    end subroutine set_empty

    subroutine fill_rank_block_query(node, query)
        type(rank_block_node), intent(in) :: node
        type(control_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%statement_kind = CONTROL_RANK_BLOCK
        query%is_default = node%rank_value == -1
        query%is_assumed_size = node%rank_value == -2
        query%has_rank = .not. query%is_default .and. &
            .not. query%is_assumed_size
        if (query%has_rank) query%rank_value = node%rank_value
        if (allocated(node%body_indices)) query%body_node_indices = node%body_indices
    end subroutine fill_rank_block_query

    subroutine fill_where_query(node, query)
        type(where_node), intent(in) :: node
        type(control_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%is_single_line = node%is_single_line
        if (node%is_single_line) then
            query%statement_kind = CONTROL_WHERE_STATEMENT
        else
            query%statement_kind = CONTROL_WHERE
        end if
        call set_present_index(node%mask_expr_index, query%mask_node_index, &
            query%has_mask)
        if (allocated(node%where_body_indices)) then
            query%body_node_indices = node%where_body_indices
        end if
        if (node%is_single_line) call set_single_assignment(query)
        call copy_elsewhere_clauses(node, query)
    end subroutine fill_where_query

    subroutine fill_where_statement_query(node, query)
        type(where_stmt_node), intent(in) :: node
        type(control_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%statement_kind = CONTROL_WHERE_STATEMENT
        query%is_single_line = .true.
        call set_present_index(node%mask_expr_index, query%mask_node_index, &
            query%has_mask)
        call set_present_index(node%assignment_index, &
            query%assignment_node_index, query%has_assignment)
    end subroutine fill_where_statement_query

    subroutine set_single_assignment(query)
        type(control_statement_query_t), intent(inout) :: query

        if (size(query%body_node_indices) /= 1) return
        query%assignment_node_index = query%body_node_indices(1)
        query%has_assignment = query%assignment_node_index > 0
    end subroutine set_single_assignment

    subroutine copy_elsewhere_clauses(node, query)
        type(where_node), intent(in) :: node
        type(control_statement_query_t), intent(inout) :: query
        integer :: i

        if (.not. allocated(node%elsewhere_clauses)) return
        deallocate (query%elsewhere_clauses)
        allocate (query%elsewhere_clauses(size(node%elsewhere_clauses)))
        do i = 1, size(node%elsewhere_clauses)
            call initialize_elsewhere(query%elsewhere_clauses(i))
            call set_present_index(node%elsewhere_clauses(i)%mask_index, &
                query%elsewhere_clauses(i)%mask_node_index, &
                query%elsewhere_clauses(i)%has_mask)
            if (allocated(node%elsewhere_clauses(i)%body_indices)) then
                query%elsewhere_clauses(i)%body_node_indices = &
                    node%elsewhere_clauses(i)%body_indices
            end if
        end do
    end subroutine copy_elsewhere_clauses

    subroutine initialize_elsewhere(clause)
        type(elsewhere_clause_query_t), intent(out) :: clause

        allocate (clause%body_node_indices(0))
    end subroutine initialize_elsewhere

    subroutine set_present_index(source, target, present_flag)
        integer, intent(in) :: source
        integer, intent(inout) :: target
        logical, intent(inout) :: present_flag

        if (source <= 0) return
        target = source
        present_flag = .true.
    end subroutine set_present_index

end module frontend_compiler_control_queries
