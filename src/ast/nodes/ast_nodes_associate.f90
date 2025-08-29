module ast_nodes_associate
    use json_module
    use uid_generator, only: generate_uid
    use ast_base, only: ast_node, visit_interface, to_json_interface, &
                         ast_node_wrapper, ast_visitor_base_t
    implicit none
    private

    ! Public types
    public :: association_t, associate_node

    ! Public factory functions
    public :: create_associate

    ! Association type for ASSOCIATE construct
    type :: association_t
        character(len=:), allocatable :: name     ! Associate name
        integer :: expr_index = 0                 ! Expression index in arena
    end type association_t

    ! ASSOCIATE construct node
    type, extends(ast_node) :: associate_node
        type(association_t), allocatable :: associations(:)  ! List of associations
        integer, allocatable :: body_indices(:)              ! Body statement indices
    contains
        procedure :: accept => associate_accept
        procedure :: to_json => associate_to_json
        procedure :: assign => associate_assign
        generic :: assignment(=) => assign
    end type associate_node

contains

    ! ASSOCIATE node implementations
    subroutine associate_accept(this, visitor)
        class(associate_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine associate_accept

    subroutine associate_to_json(this, json, parent)
        class(associate_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, assoc_array, assoc_obj, body_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'associate')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        ! Add associations array
        if (allocated(this%associations)) then
            call json%create_array(assoc_array, 'associations')
            do i = 1, size(this%associations)
                call json%create_object(assoc_obj, '')
                call json%add(assoc_obj, 'name', this%associations(i)%name)
                call json%add(assoc_obj, 'expr_index', this%associations(i)%expr_index)
                call json%add(assoc_array, assoc_obj)
            end do
            call json%add(obj, assoc_array)
        end if

        ! Add body indices
        if (allocated(this%body_indices)) then
            call json%create_array(body_array, 'body_indices')
            do i = 1, size(this%body_indices)
                call json%add(body_array, '', this%body_indices(i))
            end do
            call json%add(obj, body_array)
        end if

        call json%add(parent, obj)
    end subroutine associate_to_json

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
            allocate(lhs%associations(size(rhs%associations)))
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

end module ast_nodes_associate