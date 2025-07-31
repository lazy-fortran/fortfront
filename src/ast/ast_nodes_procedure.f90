module ast_nodes_procedure
    use json_module
    use ast_base, only: ast_node, visit_interface, to_json_interface
    implicit none
    private

    ! Public factory functions
    public :: create_function_def, create_subroutine_def

    ! Procedure-related AST nodes

    ! Function definition node
    type, extends(ast_node), public :: function_def_node
        character(len=:), allocatable :: name
        integer, allocatable :: param_indices(:)
        character(len=:), allocatable :: return_type
        integer, allocatable :: body_indices(:)
    contains
        procedure :: accept => function_def_accept
        procedure :: to_json => function_def_to_json
        procedure :: assign => function_def_assign
        generic :: assignment(=) => assign
    end type function_def_node

    ! Subroutine definition node
    type, extends(ast_node), public :: subroutine_def_node
        character(len=:), allocatable :: name
        integer, allocatable :: param_indices(:)
        integer, allocatable :: body_indices(:)
    contains
        procedure :: accept => subroutine_def_accept
        procedure :: to_json => subroutine_def_to_json
        procedure :: assign => subroutine_def_assign
        generic :: assignment(=) => assign
    end type subroutine_def_node

    ! Subroutine call node (represents explicit CALL statements)
    type, extends(ast_node), public :: subroutine_call_node
        character(len=:), allocatable :: name
        integer, allocatable :: arg_indices(:)
    contains
        procedure :: accept => subroutine_call_accept
        procedure :: to_json => subroutine_call_to_json
        procedure :: assign => subroutine_call_assign
        generic :: assignment(=) => assign
    end type subroutine_call_node

contains

    ! Stub implementations for function_def_node
    subroutine function_def_accept(this, visitor)
        class(function_def_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine function_def_accept

    subroutine function_def_to_json(this, json, parent)
        class(function_def_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine function_def_to_json

    subroutine function_def_assign(lhs, rhs)
        class(function_def_node), intent(inout) :: lhs
        class(function_def_node), intent(in) :: rhs
        lhs%name = rhs%name
        lhs%return_type = rhs%return_type
        if (allocated(rhs%param_indices)) lhs%param_indices = rhs%param_indices
        if (allocated(rhs%body_indices)) lhs%body_indices = rhs%body_indices
    end subroutine function_def_assign

    ! Stub implementations for subroutine_def_node
    subroutine subroutine_def_accept(this, visitor)
        class(subroutine_def_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine subroutine_def_accept

    subroutine subroutine_def_to_json(this, json, parent)
        class(subroutine_def_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine subroutine_def_to_json

    subroutine subroutine_def_assign(lhs, rhs)
        class(subroutine_def_node), intent(inout) :: lhs
        class(subroutine_def_node), intent(in) :: rhs
        lhs%name = rhs%name
        if (allocated(rhs%param_indices)) lhs%param_indices = rhs%param_indices
        if (allocated(rhs%body_indices)) lhs%body_indices = rhs%body_indices
    end subroutine subroutine_def_assign

    ! Stub implementations for subroutine_call_node
    subroutine subroutine_call_accept(this, visitor)
        class(subroutine_call_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine subroutine_call_accept

    subroutine subroutine_call_to_json(this, json, parent)
        class(subroutine_call_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine subroutine_call_to_json

    subroutine subroutine_call_assign(lhs, rhs)
        class(subroutine_call_node), intent(inout) :: lhs
        class(subroutine_call_node), intent(in) :: rhs
        lhs%name = rhs%name
        if (allocated(rhs%arg_indices)) lhs%arg_indices = rhs%arg_indices
    end subroutine subroutine_call_assign

    ! Factory functions
    function create_function_def(name, param_indices, return_type, body_indices, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:)
        character(len=*), intent(in) :: return_type
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(function_def_node) :: node

        node%name = name
        if (present(param_indices)) then
            if (size(param_indices) > 0) then
                node%param_indices = param_indices
            end if
        end if
        node%return_type = return_type
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                node%body_indices = body_indices
            end if
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_function_def

    function create_subroutine_def(name, param_indices, body_indices, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:)
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(subroutine_def_node) :: node

        node%name = name
        if (present(param_indices)) then
            if (size(param_indices) > 0) then
                node%param_indices = param_indices
            end if
        end if
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                node%body_indices = body_indices
            end if
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_subroutine_def

end module ast_nodes_procedure