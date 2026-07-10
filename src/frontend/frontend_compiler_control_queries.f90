module frontend_compiler_control_queries
    use ast_arena_modern, only: ast_arena_t
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

    type, public :: association_query_t
        character(len=:), allocatable :: name
        integer :: expression_node_index = 0
    end type association_query_t

    type, public :: elsewhere_clause_query_t
        logical :: has_mask = .false.
        integer :: mask_node_index = 0
        integer, allocatable :: body_node_indices(:)
    end type elsewhere_clause_query_t

    type, public :: control_statement_query_t
        logical :: found = .false.
        integer :: statement_kind = 0
        integer :: line = 0
        integer :: column = 0
        type(association_query_t), allocatable :: associations(:)
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
            call fill_select_rank_query(node, query)
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

    subroutine fill_select_rank_query(node, query)
        type(select_rank_node), intent(in) :: node
        type(control_statement_query_t), intent(inout) :: query

        query%found = .true.
        query%statement_kind = CONTROL_SELECT_RANK
        call set_present_index(node%selector_index, query%selector_node_index, &
            query%has_selector)
        if (allocated(node%rank_indices)) then
            query%child_node_indices = node%rank_indices
        end if
        call set_present_index(node%default_index, query%default_node_index, &
            query%has_default)
    end subroutine fill_select_rank_query

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
