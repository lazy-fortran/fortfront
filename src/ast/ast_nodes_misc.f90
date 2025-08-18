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
        integer, allocatable :: shape_indices(:)       ! Shape expressions
        ! for each variable
        integer :: stat_var_index = 0                  ! Optional stat variable index
        integer :: errmsg_var_index = 0                ! Optional errmsg variable index
        integer :: source_expr_index = 0               ! Optional source
        ! expression index
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
        character(len=:), allocatable :: url_spec          ! Optional URL specification for Go-style imports
        type(string_t), allocatable :: only_list(:)       ! Optional only clause items
        type(string_t), allocatable :: rename_list(:)     ! Optional rename
        ! mappings (new_name => old_name)
        logical :: has_only = .false.                     ! Whether the only
        ! clause is present
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
        character(len=:), allocatable :: kind         ! "interface", "generic",
        ! "operator", "assignment"
        character(len=:), allocatable :: operator     ! Operator symbol
        ! (for operator interfaces)
        integer, allocatable :: procedure_indices(:)  ! Procedure declaration
        ! arena indices
    contains
        procedure :: accept => interface_block_accept
        procedure :: to_json => interface_block_to_json
        procedure :: assign => interface_block_assign
        generic :: assignment(=) => assign
    end type interface_block_node

    ! Letter specification for implicit statements
    type, public :: implicit_letter_spec_t
        character :: start_letter = ' '   ! Starting letter of range
        character :: end_letter = ' '     ! Ending letter of range (same as start for single letters)
    end type implicit_letter_spec_t

    ! Type specification for implicit statements
    type, public :: implicit_type_spec_t
        character(len=:), allocatable :: type_name    ! "real", "integer", "character", etc.
        logical :: has_kind = .false.
        integer :: kind_value = 0
        logical :: has_length = .false.               ! For character types
        integer :: length_value = 0
    end type implicit_type_spec_t

    ! Implicit statement node
    type, extends(ast_node), public :: implicit_statement_node
        logical :: is_none = .false.                           ! True for "implicit none"
        type(implicit_type_spec_t) :: type_spec                ! Type specification
        type(implicit_letter_spec_t), allocatable :: letter_specs(:)  ! Letter ranges/singles
    contains
        procedure :: accept => implicit_statement_accept
        procedure :: to_json => implicit_statement_to_json
        procedure :: assign => implicit_statement_assign
        generic :: assignment(=) => assign
    end type implicit_statement_node

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
        
        ! Copy base class fields using new cycle-safe pattern
        call lhs%copy_base_fields(lhs, rhs)
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
        if (this%stat_var_index > 0) call json%add(obj, 'stat_var_index', &
            this%stat_var_index)
        if (this%errmsg_var_index > 0) call json%add(obj, 'errmsg_var_index', &
            this%errmsg_var_index)
        if (this%source_expr_index > 0) call json%add(obj, 'source_expr_index', &
            this%source_expr_index)
        if (this%mold_expr_index > 0) call json%add(obj, 'mold_expr_index', &
            this%mold_expr_index)
        call json%add(parent, obj)
    end subroutine allocate_statement_to_json

    subroutine allocate_statement_assign(lhs, rhs)
        class(allocate_statement_node), intent(inout) :: lhs
        class(allocate_statement_node), intent(in) :: rhs
        
        ! Copy base class fields using new cycle-safe pattern
        call lhs%copy_base_fields(lhs, rhs)
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
        if (this%stat_var_index > 0) call json%add(obj, 'stat_var_index', &
            this%stat_var_index)
        if (this%errmsg_var_index > 0) call json%add(obj, 'errmsg_var_index', &
            this%errmsg_var_index)
        call json%add(parent, obj)
    end subroutine deallocate_statement_to_json

    subroutine deallocate_statement_assign(lhs, rhs)
        class(deallocate_statement_node), intent(inout) :: lhs
        class(deallocate_statement_node), intent(in) :: rhs
        
        ! Copy base class fields using new cycle-safe pattern
        call lhs%copy_base_fields(lhs, rhs)
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
        if (allocated(this%module_name)) call json%add(obj, 'module_name', &
            this%module_name)
        if (allocated(this%url_spec)) call json%add(obj, 'url_spec', &
            this%url_spec)
        call json%add(obj, 'has_only', this%has_only)
        call json%add(parent, obj)
    end subroutine use_statement_to_json

    subroutine use_statement_assign(lhs, rhs)
        class(use_statement_node), intent(inout) :: lhs
        class(use_statement_node), intent(in) :: rhs
        
        ! Copy base class fields using new cycle-safe pattern
        call lhs%copy_base_fields(lhs, rhs)
        ! Copy specific components
        if (allocated(rhs%module_name)) lhs%module_name = rhs%module_name
        if (allocated(rhs%url_spec)) lhs%url_spec = rhs%url_spec
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
        ! Copy inferred_type with reference-counted safety
        if (allocated(rhs%inferred_type)) then
            if (.not. allocated(lhs%inferred_type)) then
                allocate(lhs%inferred_type)
            end if
            lhs%inferred_type = rhs%inferred_type  ! Reference-counted assignment
        else
            if (allocated(lhs%inferred_type)) then
                deallocate(lhs%inferred_type)
            end if
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
        ! Copy inferred_type with reference-counted safety
        if (allocated(rhs%inferred_type)) then
            if (.not. allocated(lhs%inferred_type)) then
                allocate(lhs%inferred_type)
            end if
            lhs%inferred_type = rhs%inferred_type  ! Reference-counted assignment
        else
            if (allocated(lhs%inferred_type)) then
                deallocate(lhs%inferred_type)
            end if
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
        ! Copy inferred_type with reference-counted safety
        if (allocated(rhs%inferred_type)) then
            if (.not. allocated(lhs%inferred_type)) then
                allocate(lhs%inferred_type)
            end if
            lhs%inferred_type = rhs%inferred_type  ! Reference-counted assignment
        else
            if (allocated(lhs%inferred_type)) then
                deallocate(lhs%inferred_type)
            end if
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
        ! Copy inferred_type with reference-counted safety
        if (allocated(rhs%inferred_type)) then
            if (.not. allocated(lhs%inferred_type)) then
                allocate(lhs%inferred_type)
            end if
            lhs%inferred_type = rhs%inferred_type  ! Reference-counted assignment
        else
            if (allocated(lhs%inferred_type)) then
                deallocate(lhs%inferred_type)
            end if
        end if
        ! Copy comment text
        if (allocated(rhs%text)) lhs%text = rhs%text
    end subroutine comment_assign

    ! Implicit statement implementations
    subroutine implicit_statement_accept(this, visitor)
        class(implicit_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! For now, do nothing since we don't have a visitor framework in place
    end subroutine implicit_statement_accept

    subroutine implicit_statement_to_json(this, json, parent)
        class(implicit_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'implicit_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'is_none', this%is_none)
        
        if (.not. this%is_none) then
            ! Add type specification as simple fields
            if (allocated(this%type_spec%type_name)) then
                call json%add(obj, 'type_name', this%type_spec%type_name)
            end if
            call json%add(obj, 'has_kind', this%type_spec%has_kind)
            if (this%type_spec%has_kind) then
                call json%add(obj, 'kind_value', this%type_spec%kind_value)
            end if
            call json%add(obj, 'has_length', this%type_spec%has_length)
            if (this%type_spec%has_length) then
                call json%add(obj, 'length_value', this%type_spec%length_value)
            end if
            
            ! For now, just add letter count - full letter spec serialization can be added later
            if (allocated(this%letter_specs)) then
                call json%add(obj, 'letter_specs_count', size(this%letter_specs))
            else
                call json%add(obj, 'letter_specs_count', 0)
            end if
        end if
        
        call json%add(parent, obj)
    end subroutine implicit_statement_to_json

    subroutine implicit_statement_assign(lhs, rhs)
        class(implicit_statement_node), intent(inout) :: lhs
        class(implicit_statement_node), intent(in) :: rhs
        integer :: i
        
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        ! Copy inferred_type with reference-counted safety
        if (allocated(rhs%inferred_type)) then
            if (.not. allocated(lhs%inferred_type)) then
                allocate(lhs%inferred_type)
            end if
            lhs%inferred_type = rhs%inferred_type  ! Reference-counted assignment
        else
            if (allocated(lhs%inferred_type)) then
                deallocate(lhs%inferred_type)
            end if
        end if
        
        ! Copy implicit statement specific fields
        lhs%is_none = rhs%is_none
        
        ! Copy type specification
        if (allocated(rhs%type_spec%type_name)) then
            lhs%type_spec%type_name = rhs%type_spec%type_name
        else
            if (allocated(lhs%type_spec%type_name)) deallocate(lhs%type_spec%type_name)
        end if
        lhs%type_spec%has_kind = rhs%type_spec%has_kind
        lhs%type_spec%kind_value = rhs%type_spec%kind_value
        lhs%type_spec%has_length = rhs%type_spec%has_length
        lhs%type_spec%length_value = rhs%type_spec%length_value
        
        ! Copy letter specifications
        if (allocated(rhs%letter_specs)) then
            if (allocated(lhs%letter_specs)) deallocate(lhs%letter_specs)
            allocate(lhs%letter_specs(size(rhs%letter_specs)))
            do i = 1, size(rhs%letter_specs)
                lhs%letter_specs(i) = rhs%letter_specs(i)
            end do
        else
            if (allocated(lhs%letter_specs)) deallocate(lhs%letter_specs)
        end if
    end subroutine implicit_statement_assign

end module ast_nodes_misc