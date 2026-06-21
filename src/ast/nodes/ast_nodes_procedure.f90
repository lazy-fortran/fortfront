module ast_nodes_procedure
    use uid_generator, only: generate_uid
    use ast_base, only: ast_node, &
                        ast_visitor_base_t, copy_ast_node_base
    implicit none
    private

    ! Public factory functions
    public :: create_function_def, create_subroutine_def

    ! Public interface helpers
    public :: is_procedure_node, get_procedure_name, get_procedure_params, &
              get_procedure_body
    public :: procedure_has_return_type, get_procedure_return_type

    ! Procedure-related AST nodes

    ! Function definition node
    type, extends(ast_node), public :: function_def_node
        character(len=:), allocatable :: name
        integer, allocatable :: param_indices(:)
        character(len=:), allocatable :: return_type
        logical :: has_return_type_in_header = .false.
        character(len=:), allocatable :: result_variable
        character(len=16), allocatable :: prefix_keywords(:)
        character(len=:), allocatable :: param_intents(:)
        integer, allocatable :: body_indices(:)
        logical :: is_recursive = .false.
        character(len=:), allocatable :: bind_c_clause
    contains
        procedure :: accept => function_def_accept
        procedure :: assign => function_def_assign
        generic :: assignment(=) => assign
    end type function_def_node

    ! Subroutine definition node
    type, extends(ast_node), public :: subroutine_def_node
        character(len=:), allocatable :: name
        integer, allocatable :: param_indices(:)
        character(len=16), allocatable :: prefix_keywords(:)
        character(len=:), allocatable :: param_intents(:)
        integer, allocatable :: body_indices(:)
        logical :: is_recursive = .false.
        character(len=:), allocatable :: bind_c_clause
    contains
        procedure :: accept => subroutine_def_accept
        procedure :: assign => subroutine_def_assign
        generic :: assignment(=) => assign
    end type subroutine_def_node

    ! Subroutine call node (represents explicit CALL statements)
    type, extends(ast_node), public :: subroutine_call_node
        character(len=:), allocatable :: name
        integer, allocatable :: arg_indices(:)
    contains
        procedure :: accept => subroutine_call_accept
        procedure :: assign => subroutine_call_assign
        generic :: assignment(=) => assign
    end type subroutine_call_node

contains

    ! Implementation for function_def_node
    subroutine function_def_accept(this, visitor)
        class(function_def_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Implementation is in ast_visitor.f90
    end subroutine function_def_accept

    subroutine function_def_assign(lhs, rhs)
        class(function_def_node), intent(inout) :: lhs
        class(function_def_node), intent(in) :: rhs
        call copy_ast_node_base(lhs, rhs)
        if (allocated(rhs%name)) then
            lhs%name = rhs%name
        else if (allocated(lhs%name)) then
            deallocate (lhs%name)
        end if
        if (allocated(rhs%return_type)) then
            lhs%return_type = rhs%return_type
        else if (allocated(lhs%return_type)) then
            deallocate (lhs%return_type)
        end if
        lhs%has_return_type_in_header = rhs%has_return_type_in_header
        lhs%is_recursive = rhs%is_recursive
        if (allocated(rhs%result_variable)) then
            lhs%result_variable = rhs%result_variable
        else if (allocated(lhs%result_variable)) then
            deallocate (lhs%result_variable)
        end if
        if (allocated(rhs%prefix_keywords)) then
            lhs%prefix_keywords = rhs%prefix_keywords
        else if (allocated(lhs%prefix_keywords)) then
            deallocate (lhs%prefix_keywords)
        end if
        if (allocated(rhs%param_intents)) then
            lhs%param_intents = rhs%param_intents
        else if (allocated(lhs%param_intents)) then
            deallocate (lhs%param_intents)
        end if
        if (allocated(rhs%param_indices)) then
            lhs%param_indices = rhs%param_indices
        else if (allocated(lhs%param_indices)) then
            deallocate (lhs%param_indices)
        end if
        if (allocated(rhs%body_indices)) then
            lhs%body_indices = rhs%body_indices
        else if (allocated(lhs%body_indices)) then
            deallocate (lhs%body_indices)
        end if
        if (allocated(rhs%bind_c_clause)) then
            lhs%bind_c_clause = rhs%bind_c_clause
        else if (allocated(lhs%bind_c_clause)) then
            deallocate (lhs%bind_c_clause)
        end if
    end subroutine function_def_assign

    ! Implementation for subroutine_def_node
    subroutine subroutine_def_accept(this, visitor)
        class(subroutine_def_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Implementation is in ast_visitor.f90
    end subroutine subroutine_def_accept

    subroutine subroutine_def_assign(lhs, rhs)
        class(subroutine_def_node), intent(inout) :: lhs
        class(subroutine_def_node), intent(in) :: rhs
        call copy_ast_node_base(lhs, rhs)
        if (allocated(rhs%name)) then
            lhs%name = rhs%name
        else if (allocated(lhs%name)) then
            deallocate (lhs%name)
        end if
        if (allocated(rhs%param_indices)) then
            lhs%param_indices = rhs%param_indices
        else if (allocated(lhs%param_indices)) then
            deallocate (lhs%param_indices)
        end if
        if (allocated(rhs%prefix_keywords)) then
            lhs%prefix_keywords = rhs%prefix_keywords
        else if (allocated(lhs%prefix_keywords)) then
            deallocate (lhs%prefix_keywords)
        end if
        if (allocated(rhs%param_intents)) then
            lhs%param_intents = rhs%param_intents
        else if (allocated(lhs%param_intents)) then
            deallocate (lhs%param_intents)
        end if
        if (allocated(rhs%body_indices)) then
            lhs%body_indices = rhs%body_indices
        else if (allocated(lhs%body_indices)) then
            deallocate (lhs%body_indices)
        end if
        lhs%is_recursive = rhs%is_recursive
        if (allocated(rhs%bind_c_clause)) then
            lhs%bind_c_clause = rhs%bind_c_clause
        else if (allocated(lhs%bind_c_clause)) then
            deallocate (lhs%bind_c_clause)
        end if
    end subroutine subroutine_def_assign

    ! Implementation for subroutine_call_node
    subroutine subroutine_call_accept(this, visitor)
        class(subroutine_call_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Implementation is in ast_visitor.f90
    end subroutine subroutine_call_accept

    subroutine subroutine_call_assign(lhs, rhs)
        class(subroutine_call_node), intent(inout) :: lhs
        class(subroutine_call_node), intent(in) :: rhs
        call copy_ast_node_base(lhs, rhs)
        if (allocated(rhs%name)) then
            lhs%name = rhs%name
        else if (allocated(lhs%name)) then
            deallocate (lhs%name)
        end if
        if (allocated(rhs%arg_indices)) then
            lhs%arg_indices = rhs%arg_indices
        else if (allocated(lhs%arg_indices)) then
            deallocate (lhs%arg_indices)
        end if
    end subroutine subroutine_call_assign

    ! Factory functions
    function create_function_def(name, param_indices, return_type, &
                                 body_indices, line, column, result_variable, &
                                 prefix_keywords) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:)
        character(len=*), intent(in) :: return_type
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column
        character(len=*), intent(in), optional :: result_variable
        character(len=16), intent(in), optional :: prefix_keywords(:)
        type(function_def_node) :: node

        node%uid = generate_uid()
        node%name = name
        if (present(param_indices)) then
            if (size(param_indices) > 0) then
                node%param_indices = param_indices
            end if
        end if
        node%return_type = return_type
        node%has_return_type_in_header = len_trim(return_type) > 0
        if (present(result_variable)) then
            node%result_variable = result_variable
        end if
        if (present(prefix_keywords)) then
            if (size(prefix_keywords) > 0) then
                node%prefix_keywords = prefix_keywords
            end if
        end if
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                node%body_indices = body_indices
            end if
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_function_def

    function create_subroutine_def(name, param_indices, body_indices, &
                                   line, column, prefix_keywords, is_recursive) &
        result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:)
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column
        character(len=16), intent(in), optional :: prefix_keywords(:)
        logical, intent(in), optional :: is_recursive
        type(subroutine_def_node) :: node

        node%uid = generate_uid()
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
        if (present(prefix_keywords)) then
            if (size(prefix_keywords) > 0) then
                node%prefix_keywords = prefix_keywords
            end if
        end if
        if (present(is_recursive)) then
            node%is_recursive = is_recursive
        end if
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
    ! Memory ownership: caller owns the returned string and releases it
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
    ! Memory ownership: caller releases the returned array
    function get_procedure_params(node) result(param_indices)
        class(ast_node), intent(in) :: node
        integer, allocatable :: param_indices(:)

        select type (n => node)
        type is (function_def_node)
            if (allocated(n%param_indices)) then
                param_indices = n%param_indices
            else
                allocate (param_indices(0))
            end if
        type is (subroutine_def_node)
            if (allocated(n%param_indices)) then
                param_indices = n%param_indices
            else
                allocate (param_indices(0))
            end if
        class default
            allocate (param_indices(0))
        end select
    end function get_procedure_params

    ! Get procedure body (works for both functions and subroutines)
    ! Memory ownership: caller releases the returned array
    function get_procedure_body(node) result(body_indices)
        class(ast_node), intent(in) :: node
        integer, allocatable :: body_indices(:)

        select type (n => node)
        type is (function_def_node)
            if (allocated(n%body_indices)) then
                body_indices = n%body_indices
            else
                allocate (body_indices(0))
            end if
        type is (subroutine_def_node)
            if (allocated(n%body_indices)) then
                body_indices = n%body_indices
            else
                allocate (body_indices(0))
            end if
        class default
            allocate (body_indices(0))
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
    ! Memory ownership: caller releases the returned string
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
