module ast_nodes_legacy
    ! AST nodes for legacy / FORTRAN constructs that previously survived only
    ! as comment/text/error nodes: COMMON blocks and ENUM definitions.
    use ast_base, only: ast_node, ast_visitor_base_t
    use string_types, only: string_t
    implicit none
    private

    ! COMMON statement node (F77 5.5.2). A single COMMON statement may declare
    ! several blocks (named or blank). Members are stored flattened with a
    ! parallel index identifying the owning block, preserving source order.
    type, extends(ast_node), public :: common_block_node
        type(string_t), allocatable :: block_names(:) ! "" for blank common
        type(string_t), allocatable :: member_names(:) ! flattened members
        integer, allocatable :: member_block(:) ! 1-based block index
    contains
        procedure :: accept => common_block_accept
        procedure :: assign => common_block_assign
        generic :: assignment(=) => assign
    end type common_block_node

    ! ENUM definition node (F2003 4.6). Carries the enumerator names and their
    ! resolved integer values, plus whether bind(c) was specified.
    type, extends(ast_node), public :: enum_node
        logical :: is_bind_c = .false.
        type(string_t), allocatable :: enumerator_names(:)
        integer, allocatable :: enumerator_values(:)
    contains
        procedure :: accept => enum_accept
        procedure :: assign => enum_assign
        generic :: assignment(=) => assign
    end type enum_node

contains

    subroutine common_block_accept(this, visitor)
        class(common_block_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine common_block_accept

    subroutine common_block_assign(lhs, rhs)
        class(common_block_node), intent(inout) :: lhs
        class(common_block_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        if (allocated(rhs%block_names)) then
            lhs%block_names = rhs%block_names
        else if (allocated(lhs%block_names)) then
            deallocate (lhs%block_names)
        end if
        if (allocated(rhs%member_names)) then
            lhs%member_names = rhs%member_names
        else if (allocated(lhs%member_names)) then
            deallocate (lhs%member_names)
        end if
        if (allocated(rhs%member_block)) then
            lhs%member_block = rhs%member_block
        else if (allocated(lhs%member_block)) then
            deallocate (lhs%member_block)
        end if
    end subroutine common_block_assign

    subroutine enum_accept(this, visitor)
        class(enum_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine enum_accept

    subroutine enum_assign(lhs, rhs)
        class(enum_node), intent(inout) :: lhs
        class(enum_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        lhs%is_bind_c = rhs%is_bind_c
        if (allocated(rhs%enumerator_names)) then
            lhs%enumerator_names = rhs%enumerator_names
        else if (allocated(lhs%enumerator_names)) then
            deallocate (lhs%enumerator_names)
        end if
        if (allocated(rhs%enumerator_values)) then
            lhs%enumerator_values = rhs%enumerator_values
        else if (allocated(lhs%enumerator_values)) then
            deallocate (lhs%enumerator_values)
        end if
    end subroutine enum_assign

end module ast_nodes_legacy
