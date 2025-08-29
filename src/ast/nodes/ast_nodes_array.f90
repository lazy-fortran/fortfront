module ast_nodes_array
    use json_module
    use uid_generator, only: generate_uid
    use ast_base, only: ast_node, visit_interface, to_json_interface, &
                         ast_node_wrapper, ast_visitor_base_t
    implicit none
    private

    ! Public types
    public :: where_node, where_stmt_node, elsewhere_clause_t

    ! Type for ELSEWHERE clause information
    type :: elsewhere_clause_t
        integer :: mask_index = 0  ! 0 for final ELSEWHERE without mask
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
        logical :: mask_is_simple = .false.  ! True if mask is simple comparison
        logical :: can_vectorize = .false.   ! True if all assignments vectorizable
    contains
        procedure :: accept => where_accept
        procedure :: to_json => where_to_json
        procedure :: assign => where_assign
        generic :: assignment(=) => assign
    end type where_node
    
    ! Single-line WHERE statement node
    type, extends(ast_node) :: where_stmt_node
        integer :: mask_expr_index = 0
        integer :: assignment_index = 0
    contains
        procedure :: accept => where_stmt_accept
        procedure :: to_json => where_stmt_to_json
        procedure :: assign => where_stmt_assign
        generic :: assignment(=) => assign
    end type where_stmt_node

contains

    ! Where construct implementations
    subroutine where_accept(this, visitor)
        class(where_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine where_accept

    subroutine where_to_json(this, json, parent)
        class(where_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'where')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'mask_expr_index', this%mask_expr_index)
        if (allocated(this%where_body_indices)) then
            call json%add(obj, 'where_body_indices', this%where_body_indices)
        end if
        if (allocated(this%elsewhere_clauses)) then
            call json%add(obj, 'num_elsewhere_clauses', size(this%elsewhere_clauses))
        else
            call json%add(obj, 'num_elsewhere_clauses', 0)
        end if
        call json%add(obj, 'mask_is_simple', this%mask_is_simple)
        call json%add(obj, 'can_vectorize', this%can_vectorize)
        call json%add(parent, obj)
    end subroutine where_to_json

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
            if (allocated(lhs%where_body_indices)) deallocate(lhs%where_body_indices)
            allocate(lhs%where_body_indices(size(rhs%where_body_indices)))
            lhs%where_body_indices = rhs%where_body_indices
        end if
        if (allocated(rhs%elsewhere_clauses)) then
            if (allocated(lhs%elsewhere_clauses)) deallocate(lhs%elsewhere_clauses)
            allocate(lhs%elsewhere_clauses(size(rhs%elsewhere_clauses)))
            do i = 1, size(rhs%elsewhere_clauses)
                lhs%elsewhere_clauses(i)%mask_index = &
                    rhs%elsewhere_clauses(i)%mask_index
                if (allocated(rhs%elsewhere_clauses(i)%body_indices)) then
                    allocate(lhs%elsewhere_clauses(i)%body_indices( &
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

    subroutine where_stmt_to_json(this, json, parent)
        class(where_stmt_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'where_stmt')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'mask_expr_index', this%mask_expr_index)
        call json%add(obj, 'assignment_index', this%assignment_index)
        call json%add(parent, obj)
    end subroutine where_stmt_to_json

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

end module ast_nodes_array