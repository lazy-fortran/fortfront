module ast_nodes_associate
    use uid_generator, only: generate_uid
    use ast_base, only: ast_node, &
                        ast_node_wrapper, ast_visitor_base_t
    implicit none
    private

    ! Public types
    public :: association_t, associate_node, block_construct_node

    ! Public factory functions
    public :: create_associate, create_block_construct

    ! Association type for ASSOCIATE construct
    type :: association_t
        character(len=:), allocatable :: name ! Associate name
        integer :: expr_index = 0 ! Expression index in arena
    end type association_t

    ! ASSOCIATE construct node
    type, extends(ast_node) :: associate_node
        type(association_t), allocatable :: associations(:) ! List of associations
        integer, allocatable :: body_indices(:) ! Body statement indices
    contains
        procedure :: accept => associate_accept
        procedure :: assign => associate_assign
        generic :: assignment(=) => assign
    end type associate_node

    ! BLOCK construct node (F2008 scoped block)
    type, extends(ast_node) :: block_construct_node
        integer, allocatable :: body_indices(:) ! Body statement indices
    contains
        procedure :: accept => block_construct_accept
        procedure :: assign => block_construct_assign
        generic :: assignment(=) => assign
    end type block_construct_node

contains

    ! ASSOCIATE node implementations
    subroutine associate_accept(this, visitor)
        class(associate_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine associate_accept

    subroutine associate_assign(lhs, rhs)
        class(associate_node), intent(inout) :: lhs
        class(associate_node), intent(in) :: rhs
        integer :: i

        ! Copy base fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type

        ! Copy associations
        if (allocated(rhs%associations)) then
            allocate (lhs%associations(size(rhs%associations)))
            do i = 1, size(rhs%associations)
                lhs%associations(i)%name = rhs%associations(i)%name
                lhs%associations(i)%expr_index = rhs%associations(i)%expr_index
            end do
        end if

        ! Copy body indices
        if (allocated(rhs%body_indices)) then
            lhs%body_indices = rhs%body_indices
        end if
    end subroutine associate_assign

    ! Factory function for ASSOCIATE node
    function create_associate(associations, body_indices, line, column) result(node)
        type(association_t), intent(in) :: associations(:)
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(associate_node) :: node

        node%uid = generate_uid()
        if (size(associations) > 0) then
            node%associations = associations
        end if

        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                node%body_indices = body_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_associate

    ! BLOCK construct node implementations
    subroutine block_construct_accept(this, visitor)
        class(block_construct_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine block_construct_accept

    subroutine block_construct_assign(lhs, rhs)
        class(block_construct_node), intent(inout) :: lhs
        class(block_construct_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type

        if (allocated(rhs%body_indices)) then
            lhs%body_indices = rhs%body_indices
        end if
    end subroutine block_construct_assign

    function create_block_construct(body_indices, line, column) result(node)
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(block_construct_node) :: node

        node%uid = generate_uid()

        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                node%body_indices = body_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_block_construct

end module ast_nodes_associate
