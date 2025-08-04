module ast_nodes_procedure
    use json_module
    use ast_base, only: ast_node, visit_interface, to_json_interface
    implicit none
    private

    ! Public factory functions
    public :: create_function_def, create_subroutine_def
    
    ! Public interface helpers
    public :: is_procedure_node, get_procedure_name, get_procedure_params, get_procedure_body
    public :: procedure_has_return_type, get_procedure_return_type

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
    function create_function_def(name, param_indices, return_type, &
                                body_indices, line, column) result(node)
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

    function create_subroutine_def(name, param_indices, body_indices, &
                                  line, column) result(node)
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

    ! Helper functions for unified procedure interface

    ! Check if a node is a procedure definition (function or subroutine)
    function is_procedure_node(node) result(is_proc)
        class(ast_node), intent(in) :: node
        logical :: is_proc
        
        is_proc = .false.
        select type (n => node)
        type is (function_def_node)
            is_proc = .true.
        type is (subroutine_def_node)
            is_proc = .true.
        end select
    end function is_procedure_node

    ! Get procedure name (works for both functions and subroutines)
    ! Memory ownership: Caller owns the returned string and is responsible for deallocation
    function get_procedure_name(node) result(name)
        class(ast_node), intent(in) :: node
        character(len=:), allocatable :: name
        
        select type (n => node)
        type is (function_def_node)
            if (allocated(n%name)) then
                name = n%name
            else
                name = ""
            end if
        type is (subroutine_def_node)
            if (allocated(n%name)) then
                name = n%name
            else
                name = ""
            end if
        class default
            name = ""
        end select
    end function get_procedure_name

    ! Get procedure parameters (works for both functions and subroutines)
    ! Memory ownership: Caller owns the returned array and is responsible for deallocation
    function get_procedure_params(node) result(param_indices)
        class(ast_node), intent(in) :: node
        integer, allocatable :: param_indices(:)
        
        select type (n => node)
        type is (function_def_node)
            if (allocated(n%param_indices)) then
                param_indices = n%param_indices
            else
                allocate(param_indices(0))
            end if
        type is (subroutine_def_node)
            if (allocated(n%param_indices)) then
                param_indices = n%param_indices
            else
                allocate(param_indices(0))
            end if
        class default
            allocate(param_indices(0))
        end select
    end function get_procedure_params

    ! Get procedure body (works for both functions and subroutines)
    ! Memory ownership: Caller owns the returned array and is responsible for deallocation
    function get_procedure_body(node) result(body_indices)
        class(ast_node), intent(in) :: node
        integer, allocatable :: body_indices(:)
        
        select type (n => node)
        type is (function_def_node)
            if (allocated(n%body_indices)) then
                body_indices = n%body_indices
            else
                allocate(body_indices(0))
            end if
        type is (subroutine_def_node)
            if (allocated(n%body_indices)) then
                body_indices = n%body_indices
            else
                allocate(body_indices(0))
            end if
        class default
            allocate(body_indices(0))
        end select
    end function get_procedure_body

    ! Check if procedure has a return type (only functions do)
    function procedure_has_return_type(node) result(has_return)
        class(ast_node), intent(in) :: node
        logical :: has_return
        
        has_return = .false.
        select type (n => node)
        type is (function_def_node)
            has_return = .true.
        end select
    end function procedure_has_return_type

    ! Get procedure return type (only for functions)
    ! Memory ownership: Caller owns the returned string and is responsible for deallocation
    function get_procedure_return_type(node) result(return_type)
        class(ast_node), intent(in) :: node
        character(len=:), allocatable :: return_type
        
        select type (n => node)
        type is (function_def_node)
            if (allocated(n%return_type)) then
                return_type = n%return_type
            else
                return_type = ""
            end if
        class default
            return_type = ""
        end select
    end function get_procedure_return_type

end module ast_nodes_procedure