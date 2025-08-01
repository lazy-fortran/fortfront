module ast_nodes_misc
    use json_module
    use ast_base, only: ast_node, visit_interface, to_json_interface, string_t
    implicit none
    private

    ! Miscellaneous AST nodes

    ! Comment node
    type, extends(ast_node), public :: comment_node
        character(len=:), allocatable :: text
    contains
        procedure :: accept => comment_accept
        procedure :: to_json => comment_to_json
        procedure :: assign => comment_assign
        generic :: assignment(=) => assign
    end type comment_node

    ! Complex literal node
    type, extends(ast_node), public :: complex_literal_node
        integer :: real_index = 0     ! Index to real part expression in arena
        integer :: imag_index = 0     ! Index to imaginary part expression in arena
    contains
        procedure :: accept => complex_literal_accept
        procedure :: to_json => complex_literal_to_json
        procedure :: assign => complex_literal_assign
        generic :: assignment(=) => assign
    end type complex_literal_node

    ! Allocate statement node
    type, extends(ast_node), public :: allocate_statement_node
        integer, allocatable :: var_indices(:)         ! Variables to allocate
        integer, allocatable :: shape_indices(:)       ! Shape expressions for each variable
        integer :: stat_var_index = 0                  ! Optional stat variable index
        integer :: errmsg_var_index = 0                ! Optional errmsg variable index
        integer :: source_expr_index = 0               ! Optional source expression index
        integer :: mold_expr_index = 0                 ! Optional mold expression index
    contains
        procedure :: accept => allocate_statement_accept
        procedure :: to_json => allocate_statement_to_json
        procedure :: assign => allocate_statement_assign
        generic :: assignment(=) => assign
    end type allocate_statement_node

    ! Deallocate statement node
    type, extends(ast_node), public :: deallocate_statement_node
        integer, allocatable :: var_indices(:)         ! Variables to deallocate
        integer :: stat_var_index = 0                  ! Optional stat variable index
        integer :: errmsg_var_index = 0                ! Optional errmsg variable index
    contains
        procedure :: accept => deallocate_statement_accept
        procedure :: to_json => deallocate_statement_to_json
        procedure :: assign => deallocate_statement_assign
        generic :: assignment(=) => assign
    end type deallocate_statement_node

    ! Use statement node
    type, extends(ast_node), public :: use_statement_node
        character(len=:), allocatable :: module_name
        type(string_t), allocatable :: only_list(:)       ! Optional only clause items
        type(string_t), allocatable :: rename_list(:)     ! Optional rename mappings (new_name => old_name)
        logical :: has_only = .false.                     ! Whether the only clause is present
    contains
        procedure :: accept => use_statement_accept
        procedure :: to_json => use_statement_to_json
        procedure :: assign => use_statement_assign
        generic :: assignment(=) => assign
    end type use_statement_node

    ! Include statement node
    type, extends(ast_node), public :: include_statement_node
        character(len=:), allocatable :: filename
    contains
        procedure :: accept => include_statement_accept
        procedure :: to_json => include_statement_to_json
        procedure :: assign => include_statement_assign
        generic :: assignment(=) => assign
    end type include_statement_node

    ! Contains node
    type, extends(ast_node), public :: contains_node
    contains
        procedure :: accept => contains_accept
        procedure :: to_json => contains_to_json
        procedure :: assign => contains_assign
        generic :: assignment(=) => assign
    end type contains_node

    ! Interface block node
    type, extends(ast_node), public :: interface_block_node
        character(len=:), allocatable :: name         ! Interface name (optional)
        character(len=:), allocatable :: kind         ! "interface", "generic", "operator", "assignment"
        character(len=:), allocatable :: operator     ! Operator symbol (for operator interfaces)
        integer, allocatable :: procedure_indices(:)  ! Procedure declaration arena indices
    contains
        procedure :: accept => interface_block_accept
        procedure :: to_json => interface_block_to_json
        procedure :: assign => interface_block_assign
        generic :: assignment(=) => assign
    end type interface_block_node

contains

    ! Complex literal implementations
    subroutine complex_literal_accept(this, visitor)
        class(complex_literal_node), intent(in) :: this
        class(*), intent(inout) :: visitor
    end subroutine complex_literal_accept

    subroutine complex_literal_to_json(this, json, parent)
        class(complex_literal_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'complex_literal')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'real_index', this%real_index)
        call json%add(obj, 'imag_index', this%imag_index)
        call json%add(parent, obj)
    end subroutine complex_literal_to_json

    subroutine complex_literal_assign(lhs, rhs)
        class(complex_literal_node), intent(inout) :: lhs
        class(complex_literal_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        ! Copy specific components
        lhs%real_index = rhs%real_index
        lhs%imag_index = rhs%imag_index
    end subroutine complex_literal_assign

    ! Allocate statement implementations
    subroutine allocate_statement_accept(this, visitor)
        class(allocate_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
    end subroutine allocate_statement_accept

    subroutine allocate_statement_to_json(this, json, parent)
        class(allocate_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'allocate_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (this%stat_var_index > 0) call json%add(obj, 'stat_var_index', this%stat_var_index)
        if (this%errmsg_var_index > 0) call json%add(obj, 'errmsg_var_index', this%errmsg_var_index)
        if (this%source_expr_index > 0) call json%add(obj, 'source_expr_index', this%source_expr_index)
        if (this%mold_expr_index > 0) call json%add(obj, 'mold_expr_index', this%mold_expr_index)
        call json%add(parent, obj)
    end subroutine allocate_statement_to_json

    subroutine allocate_statement_assign(lhs, rhs)
        class(allocate_statement_node), intent(inout) :: lhs
        class(allocate_statement_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        ! Copy specific components
        if (allocated(rhs%var_indices)) then
            if (allocated(lhs%var_indices)) deallocate(lhs%var_indices)
            allocate(lhs%var_indices(size(rhs%var_indices)))
            lhs%var_indices = rhs%var_indices
        end if
        if (allocated(rhs%shape_indices)) then
            if (allocated(lhs%shape_indices)) deallocate(lhs%shape_indices)
            allocate(lhs%shape_indices(size(rhs%shape_indices)))
            lhs%shape_indices = rhs%shape_indices
        end if
        lhs%stat_var_index = rhs%stat_var_index
        lhs%errmsg_var_index = rhs%errmsg_var_index
        lhs%source_expr_index = rhs%source_expr_index
        lhs%mold_expr_index = rhs%mold_expr_index
    end subroutine allocate_statement_assign

    ! Deallocate statement implementations
    subroutine deallocate_statement_accept(this, visitor)
        class(deallocate_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
    end subroutine deallocate_statement_accept

    subroutine deallocate_statement_to_json(this, json, parent)
        class(deallocate_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'deallocate_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (this%stat_var_index > 0) call json%add(obj, 'stat_var_index', this%stat_var_index)
        if (this%errmsg_var_index > 0) call json%add(obj, 'errmsg_var_index', this%errmsg_var_index)
        call json%add(parent, obj)
    end subroutine deallocate_statement_to_json

    subroutine deallocate_statement_assign(lhs, rhs)
        class(deallocate_statement_node), intent(inout) :: lhs
        class(deallocate_statement_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        ! Copy specific components
        if (allocated(rhs%var_indices)) then
            if (allocated(lhs%var_indices)) deallocate(lhs%var_indices)
            allocate(lhs%var_indices(size(rhs%var_indices)))
            lhs%var_indices = rhs%var_indices
        end if
        lhs%stat_var_index = rhs%stat_var_index
        lhs%errmsg_var_index = rhs%errmsg_var_index
    end subroutine deallocate_statement_assign

    ! Use statement implementations
    subroutine use_statement_accept(this, visitor)
        class(use_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
    end subroutine use_statement_accept

    subroutine use_statement_to_json(this, json, parent)
        class(use_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'use_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%module_name)) call json%add(obj, 'module_name', this%module_name)
        call json%add(obj, 'has_only', this%has_only)
        call json%add(parent, obj)
    end subroutine use_statement_to_json

    subroutine use_statement_assign(lhs, rhs)
        class(use_statement_node), intent(inout) :: lhs
        class(use_statement_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        ! Copy specific components
        if (allocated(rhs%module_name)) lhs%module_name = rhs%module_name
        if (allocated(rhs%only_list)) then
            if (allocated(lhs%only_list)) deallocate(lhs%only_list)
            allocate(lhs%only_list(size(rhs%only_list)))
            lhs%only_list = rhs%only_list
        end if
        if (allocated(rhs%rename_list)) then
            if (allocated(lhs%rename_list)) deallocate(lhs%rename_list)
            allocate(lhs%rename_list(size(rhs%rename_list)))
            lhs%rename_list = rhs%rename_list
        end if
        lhs%has_only = rhs%has_only
    end subroutine use_statement_assign

    ! Include statement implementations
    subroutine include_statement_accept(this, visitor)
        class(include_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
    end subroutine include_statement_accept

    subroutine include_statement_to_json(this, json, parent)
        class(include_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'include_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%filename)) call json%add(obj, 'filename', this%filename)
        call json%add(parent, obj)
    end subroutine include_statement_to_json

    subroutine include_statement_assign(lhs, rhs)
        class(include_statement_node), intent(inout) :: lhs
        class(include_statement_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        ! Copy specific components
        if (allocated(rhs%filename)) lhs%filename = rhs%filename
    end subroutine include_statement_assign

    ! Contains node implementations
    subroutine contains_accept(this, visitor)
        class(contains_node), intent(in) :: this
        class(*), intent(inout) :: visitor
    end subroutine contains_accept

    subroutine contains_to_json(this, json, parent)
        class(contains_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'contains')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(parent, obj)
    end subroutine contains_to_json

    subroutine contains_assign(lhs, rhs)
        class(contains_node), intent(inout) :: lhs
        class(contains_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
    end subroutine contains_assign

    ! Interface block implementations
    subroutine interface_block_accept(this, visitor)
        class(interface_block_node), intent(in) :: this
        class(*), intent(inout) :: visitor
    end subroutine interface_block_accept

    subroutine interface_block_to_json(this, json, parent)
        class(interface_block_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'interface_block')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%name)) call json%add(obj, 'name', this%name)
        if (allocated(this%kind)) call json%add(obj, 'kind', this%kind)
        if (allocated(this%operator)) call json%add(obj, 'operator', this%operator)
        call json%add(parent, obj)
    end subroutine interface_block_to_json

    subroutine interface_block_assign(lhs, rhs)
        class(interface_block_node), intent(inout) :: lhs
        class(interface_block_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        ! Copy specific components
        if (allocated(rhs%name)) lhs%name = rhs%name
        if (allocated(rhs%kind)) lhs%kind = rhs%kind
        if (allocated(rhs%operator)) lhs%operator = rhs%operator
        if (allocated(rhs%procedure_indices)) then
            if (allocated(lhs%procedure_indices)) deallocate(lhs%procedure_indices)
            allocate(lhs%procedure_indices(size(rhs%procedure_indices)))
            lhs%procedure_indices = rhs%procedure_indices
        end if
    end subroutine interface_block_assign

    ! Comment node methods
    subroutine comment_accept(this, visitor)
        class(comment_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! For now, do nothing since we don't have a visitor framework in place
    end subroutine comment_accept

    subroutine comment_to_json(this, json, parent)
        class(comment_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'comment')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%text)) call json%add(obj, 'text', this%text)
        call json%add(parent, obj)
    end subroutine comment_to_json

    subroutine comment_assign(lhs, rhs)
        class(comment_node), intent(inout) :: lhs
        class(comment_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        end if
        ! Copy comment text
        if (allocated(rhs%text)) lhs%text = rhs%text
    end subroutine comment_assign

end module ast_nodes_misc