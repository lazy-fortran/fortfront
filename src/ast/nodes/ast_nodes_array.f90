module ast_nodes_array
    use uid_generator, only: generate_uid
    use ast_base, only: ast_node, &
                        ast_node_wrapper, ast_visitor_base_t
    implicit none
    private

    ! Public types
    public :: where_node, where_stmt_node, elsewhere_clause_t
    ! Constructor migrated from ast_core
    public :: create_where

    ! Type for ELSEWHERE clause information
    type :: elsewhere_clause_t
        integer :: mask_index = 0 ! 0 for final ELSEWHERE without mask
        integer, allocatable :: body_indices(:)
    end type elsewhere_clause_t

    ! Enhanced WHERE construct node
    type, extends(ast_node) :: where_node
        ! Main WHERE clause
        integer :: mask_expr_index = 0
        integer, allocatable :: where_body_indices(:)

        ! ELSEWHERE clauses (includes all ELSEWHERE, even final one without mask)
        type(elsewhere_clause_t), allocatable :: elsewhere_clauses(:)

        ! Optimization hints
        logical :: mask_is_simple = .false. ! True if mask is simple comparison
        logical :: can_vectorize = .false. ! True if all assignments vectorizable
    contains
        procedure :: accept => where_accept
        procedure :: assign => where_assign
        generic :: assignment(=) => assign
    end type where_node

    ! Single-line WHERE statement node
    type, extends(ast_node) :: where_stmt_node
        integer :: mask_expr_index = 0
        integer :: assignment_index = 0
    contains
        procedure :: accept => where_stmt_accept
        procedure :: assign => where_stmt_assign
        generic :: assignment(=) => assign
    end type where_stmt_node

contains

    ! Where construct implementations
    subroutine where_accept(this, visitor)
        class(where_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine where_accept

    subroutine where_assign(lhs, rhs)
        class(where_node), intent(inout) :: lhs
        class(where_node), intent(in) :: rhs
        integer :: i

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        lhs%mask_expr_index = rhs%mask_expr_index
        if (allocated(rhs%where_body_indices)) then
            if (allocated(lhs%where_body_indices)) deallocate (lhs%where_body_indices)
            allocate (lhs%where_body_indices(size(rhs%where_body_indices)))
            lhs%where_body_indices = rhs%where_body_indices
        end if
        if (allocated(rhs%elsewhere_clauses)) then
            if (allocated(lhs%elsewhere_clauses)) deallocate (lhs%elsewhere_clauses)
            allocate (lhs%elsewhere_clauses(size(rhs%elsewhere_clauses)))
            do i = 1, size(rhs%elsewhere_clauses)
                lhs%elsewhere_clauses(i)%mask_index = &
                    rhs%elsewhere_clauses(i)%mask_index
                if (allocated(rhs%elsewhere_clauses(i)%body_indices)) then
                    allocate (lhs%elsewhere_clauses(i)%body_indices( &
                              size(rhs%elsewhere_clauses(i)%body_indices)))
                    lhs%elsewhere_clauses(i)%body_indices = &
                        rhs%elsewhere_clauses(i)%body_indices
                end if
            end do
        end if
        lhs%mask_is_simple = rhs%mask_is_simple
        lhs%can_vectorize = rhs%can_vectorize
    end subroutine where_assign

    ! WHERE statement implementations
    subroutine where_stmt_accept(this, visitor)
        class(where_stmt_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine where_stmt_accept

    subroutine where_stmt_assign(lhs, rhs)
        class(where_stmt_node), intent(inout) :: lhs
        class(where_stmt_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        lhs%mask_expr_index = rhs%mask_expr_index
        lhs%assignment_index = rhs%assignment_index
    end subroutine where_stmt_assign

    ! Constructor
    function create_where(mask_expr_index, where_body_indices, &
                          elsewhere_body_indices, line, column) result(node)
        use uid_generator, only: generate_uid
        integer, intent(in) :: mask_expr_index
        integer, intent(in), optional :: where_body_indices(:), &
                                         elsewhere_body_indices(:)
        integer, intent(in), optional :: line, column
        type(where_node) :: node
        integer :: n

        node%uid = generate_uid()
        node%mask_expr_index = mask_expr_index
        node%mask_is_simple = .false.
        node%can_vectorize = .false.

        if (present(where_body_indices)) then
            if (size(where_body_indices) > 0) then
                allocate (node%where_body_indices(size(where_body_indices)))
                node%where_body_indices = where_body_indices
            end if
        end if

        if (present(elsewhere_body_indices)) then
            n = size(elsewhere_body_indices)
            if (n > 0) then
                allocate (node%elsewhere_clauses(1))
                node%elsewhere_clauses(1)%mask_index = 0
                allocate (node%elsewhere_clauses(1)%body_indices(n))
                node%elsewhere_clauses(1)%body_indices = elsewhere_body_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_where

end module ast_nodes_array
