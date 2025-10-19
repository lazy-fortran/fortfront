module ast_nodes_misc
    use json_module
    use ast_base, only: ast_node, visit_interface, to_json_interface, string_t, &
                        ast_visitor_base_t
    use string_utils_mod, only: int_to_string
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

    ! Blank line node (for preserving source formatting)
    type, extends(ast_node), public :: blank_line_node
        integer :: count = 1  ! Number of consecutive blank lines
    contains
        procedure :: accept => blank_line_accept
        procedure :: to_json => blank_line_to_json
        procedure :: assign => blank_line_assign
        generic :: assignment(=) => assign
    end type blank_line_node

    ! Complex literal node
    type, extends(ast_node), public :: complex_literal_node
        integer :: real_index = 0  ! Index to real part expression in arena
        integer :: imag_index = 0  ! Index to imaginary part expression in arena
    contains
        procedure :: accept => complex_literal_accept
        procedure :: to_json => complex_literal_to_json
        procedure :: assign => complex_literal_assign
        generic :: assignment(=) => assign
    end type complex_literal_node

    ! Allocate statement node
    type, extends(ast_node), public :: allocate_statement_node
        integer, allocatable :: var_indices(:)  ! Variables to allocate
        integer, allocatable :: shape_indices(:)  ! Shape expressions
        ! for each variable
        integer :: stat_var_index = 0  ! Optional stat variable index
        integer :: errmsg_var_index = 0  ! Optional errmsg variable index
        integer :: source_expr_index = 0  ! Optional source
        ! expression index
        integer :: mold_expr_index = 0  ! Optional mold expression index
    contains
        procedure :: accept => allocate_statement_accept
        procedure :: to_json => allocate_statement_to_json
        procedure :: assign => allocate_statement_assign
        generic :: assignment(=) => assign
    end type allocate_statement_node

    ! Deallocate statement node
    type, extends(ast_node), public :: deallocate_statement_node
        integer, allocatable :: var_indices(:)  ! Variables to deallocate
        integer :: stat_var_index = 0  ! Optional stat variable index
        integer :: errmsg_var_index = 0  ! Optional errmsg variable index
    contains
        procedure :: accept => deallocate_statement_accept
        procedure :: to_json => deallocate_statement_to_json
        procedure :: assign => deallocate_statement_assign
        generic :: assignment(=) => assign
    end type deallocate_statement_node

    ! Use statement node
    type, extends(ast_node), public :: use_statement_node
        character(len=:), allocatable :: module_name
        character(len=:), allocatable :: url_spec  ! Optional URL specification for Go-style imports
        type(string_t), allocatable :: only_list(:)  ! Optional only clause items
        type(string_t), allocatable :: rename_list(:)  ! Optional rename
        ! mappings (new_name => old_name)
        logical :: has_only = .false.  ! Whether the only
        ! clause is present
        logical :: has_double_colon = .false.
        logical :: is_intrinsic = .false.
        logical :: is_non_intrinsic = .false.
    contains
        procedure :: accept => use_statement_accept
        procedure :: to_json => use_statement_to_json
        procedure :: assign => use_statement_assign
        generic :: assignment(=) => assign
    end type use_statement_node

    type, extends(ast_node), public :: visibility_statement_node
        type(string_t), allocatable :: names(:)
        logical :: is_private = .false.
        logical :: has_list = .false.
        logical :: has_double_colon = .false.
    contains
        procedure :: accept => visibility_statement_accept
        procedure :: to_json => visibility_statement_to_json
        procedure :: assign => visibility_statement_assign
        generic :: assignment(=) => assign
    end type visibility_statement_node

    type, extends(ast_node), public :: namelist_statement_node
        character(len=:), allocatable :: group_name
        type(string_t), allocatable :: variable_names(:)
    contains
        procedure :: accept => namelist_statement_accept
        procedure :: to_json => namelist_statement_to_json
        procedure :: assign => namelist_statement_assign
        generic :: assignment(=) => assign
    end type namelist_statement_node

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

    ! End statement node (for implicit program termination)
    type, extends(ast_node), public :: end_statement_node
    contains
        procedure :: accept => end_statement_accept
        procedure :: to_json => end_statement_to_json
        procedure :: assign => end_statement_assign
        generic :: assignment(=) => assign
    end type end_statement_node

    ! Interface block node
    type, extends(ast_node), public :: interface_block_node
        character(len=:), allocatable :: name  ! Interface name (optional)
        character(len=:), allocatable :: kind  ! "interface", "generic",
        ! "operator", "assignment"
        character(len=:), allocatable :: operator  ! Operator symbol
        ! (for operator interfaces)
        integer, allocatable :: procedure_indices(:)  ! Procedure declaration
        ! arena indices
    contains
        procedure :: accept => interface_block_accept
        procedure :: to_json => interface_block_to_json
        procedure :: assign => interface_block_assign
        generic :: assignment(=) => assign
    end type interface_block_node

    type, extends(ast_node), public :: module_procedure_node
        type(string_t), allocatable :: procedure_names(:)
    contains
        procedure :: accept => module_procedure_accept
        procedure :: to_json => module_procedure_to_json
        procedure :: assign => module_procedure_assign
        generic :: assignment(=) => assign
    end type module_procedure_node

    ! Letter specification for implicit statements
    type, public :: implicit_letter_spec_t
        character :: start_letter = ' '  ! Starting letter of range
        character :: end_letter = ' '  ! Ending letter of range (same as start for single letters)
    end type implicit_letter_spec_t

    ! Type specification for implicit statements
    type, public :: implicit_type_spec_t
        character(len=:), allocatable :: type_name  ! "real", "integer", "character", etc.
        logical :: has_kind = .false.
        integer :: kind_value = 0
        logical :: has_length = .false.  ! For character types
        integer :: length_value = 0
    end type implicit_type_spec_t

    ! Implicit statement node
    type, extends(ast_node), public :: implicit_statement_node
        logical :: is_none = .false.  ! True for "implicit none"
        type(implicit_type_spec_t) :: type_spec  ! Type specification
        type(implicit_letter_spec_t), allocatable :: letter_specs(:)  ! Letter ranges/singles
    contains
        procedure :: accept => implicit_statement_accept
        procedure :: to_json => implicit_statement_to_json
        procedure :: assign => implicit_statement_assign
        generic :: assignment(=) => assign
    end type implicit_statement_node

    ! Constructors migrated from ast_core
    public :: create_comment, create_blank_line, create_end_statement
    public :: create_use_statement, create_visibility_statement, create_include_statement
    public :: create_implicit_statement, create_interface_block, create_module_procedure

contains

    subroutine copy_ast_node_base_fields(lhs, rhs)
        class(ast_node), intent(inout) :: lhs
        class(ast_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
    end subroutine copy_ast_node_base_fields

    ! Constructors
    function create_comment(text, line, column) result(node)
        use uid_generator, only: generate_uid
        character(len=*), intent(in) :: text
        integer, intent(in), optional :: line, column
        type(comment_node) :: node

        node%uid = generate_uid()
        node%text = text
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_comment

    function create_blank_line(count, line, column) result(node)
        use uid_generator, only: generate_uid
        integer, intent(in), optional :: count
        integer, intent(in), optional :: line, column
        type(blank_line_node) :: node

        node%uid = generate_uid()
        if (present(count)) node%count = count
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_blank_line

    function create_end_statement(line, column) result(node)
        use uid_generator, only: generate_uid
        integer, intent(in), optional :: line, column
        type(end_statement_node) :: node

        node%uid = generate_uid()
        if (present(line)) then
            if (line < 1) then
                node%line = 1
            else
                node%line = line
            end if
        end if
        if (present(column)) then
            if (column < 1) then
                node%column = 1
            else
                node%column = column
            end if
        end if
    end function create_end_statement

    function create_use_statement(module_name, only_list, rename_list, &
                                  has_only, line, column, url_spec, &
                                  has_double_colon, is_intrinsic, is_non_intrinsic) &
        result(node)
        use uid_generator, only: generate_uid
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in), optional :: only_list(:), rename_list(:)
        character(len=*), intent(in), optional :: url_spec
        logical, intent(in), optional :: has_only
        integer, intent(in), optional :: line, column
        logical, intent(in), optional :: has_double_colon, is_intrinsic, is_non_intrinsic
        type(use_statement_node) :: node
        integer :: i

        node%module_name = module_name
        node%uid = generate_uid()
        if (present(url_spec)) node%url_spec = url_spec
        if (present(has_only)) node%has_only = has_only
        if (present(has_double_colon)) node%has_double_colon = has_double_colon
        if (present(is_intrinsic)) node%is_intrinsic = is_intrinsic
        if (present(is_non_intrinsic)) node%is_non_intrinsic = is_non_intrinsic

        if (present(only_list)) then
            if (size(only_list) > 0) then
                allocate (node%only_list(size(only_list)))
                do i = 1, size(only_list)
                    node%only_list(i)%s = only_list(i)
                end do
            end if
        end if

        if (present(rename_list)) then
            if (size(rename_list) > 0) then
                allocate (node%rename_list(size(rename_list)))
                do i = 1, size(rename_list)
                    node%rename_list(i)%s = rename_list(i)
                end do
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_use_statement

    function create_visibility_statement(is_private, names, has_double_colon, &
                                         line, column) result(node)
        use uid_generator, only: generate_uid
        logical, intent(in) :: is_private
        character(len=*), intent(in), optional :: names(:)
        logical, intent(in), optional :: has_double_colon
        integer, intent(in), optional :: line, column
        type(visibility_statement_node) :: node
        integer :: i

        node%uid = generate_uid()
        node%is_private = is_private
        if (present(has_double_colon)) node%has_double_colon = has_double_colon

        if (present(names)) then
            if (size(names) > 0) then
                node%has_list = .true.
                allocate (node%names(size(names)))
                do i = 1, size(names)
                    node%names(i)%s = names(i)
                end do
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_visibility_statement

    function create_namelist_statement(group_name, variable_names, line, column) &
        result(node)
        use uid_generator, only: generate_uid
        character(len=*), intent(in) :: group_name
        character(len=*), intent(in), optional :: variable_names(:)
        integer, intent(in), optional :: line, column
        type(namelist_statement_node) :: node
        integer :: i

        node%uid = generate_uid()
        node%group_name = trim(group_name)

        if (present(variable_names)) then
            if (size(variable_names) > 0) then
                allocate (node%variable_names(size(variable_names)))
                do i = 1, size(variable_names)
                    node%variable_names(i)%s = trim(variable_names(i))
                end do
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_namelist_statement

    function create_implicit_statement(is_none, type_name, kind_value, has_kind, &
                                       length_value, has_length, letter_ranges, &
                                       line, column) result(node)
        use uid_generator, only: generate_uid
        logical, intent(in) :: is_none
        character(len=*), intent(in), optional :: type_name
        integer, intent(in), optional :: kind_value
        logical, intent(in), optional :: has_kind
        integer, intent(in), optional :: length_value
        logical, intent(in), optional :: has_length
        character(len=*), intent(in), optional :: letter_ranges(:)
        integer, intent(in), optional :: line, column
        type(implicit_statement_node) :: node
        integer :: i, dash_pos, upper_idx

        node%is_none = is_none
        node%uid = generate_uid()

        if (.not. is_none) then
            if (present(type_name)) node%type_spec%type_name = type_name
            if (present(has_kind)) node%type_spec%has_kind = has_kind
            if (present(kind_value)) node%type_spec%kind_value = kind_value
            if (present(has_length)) node%type_spec%has_length = has_length
            if (present(length_value)) node%type_spec%length_value = length_value

            if (present(letter_ranges)) then
                allocate (node%letter_specs(size(letter_ranges)))
                do i = 1, size(letter_ranges)
                    dash_pos = index(letter_ranges(i), '-')
                    if (dash_pos > 0) then
                        upper_idx = dash_pos + 1
                        node%letter_specs(i)%start_letter = letter_ranges(i) (1:1)
                        node%letter_specs(i)%end_letter = letter_ranges(i) &
                                                          (upper_idx:upper_idx)
                    else
                        node%letter_specs(i)%start_letter = letter_ranges(i) (1:1)
                        node%letter_specs(i)%end_letter = letter_ranges(i) (1:1)
                    end if
                end do
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_implicit_statement

    function create_include_statement(filename, line, column) result(node)
        use uid_generator, only: generate_uid
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: line, column
        type(include_statement_node) :: node

        node%filename = filename
        node%uid = generate_uid()
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_include_statement

    function create_interface_block(name, kind, operator, procedure_indices, &
                                    line, column) result(node)
        use uid_generator, only: generate_uid
        character(len=*), intent(in), optional :: name, kind, operator
        integer, intent(in), optional :: procedure_indices(:)
        integer, intent(in), optional :: line, column
        type(interface_block_node) :: node

        node%uid = generate_uid()
        if (present(name)) node%name = name
        if (present(kind)) node%kind = kind
        if (present(operator)) node%operator = operator

        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                node%procedure_indices = procedure_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_interface_block

    function create_module_procedure(procedure_names, line, column) result(node)
        use uid_generator, only: generate_uid
        type(string_t), intent(in), optional :: procedure_names(:)
        integer, intent(in), optional :: line, column
        type(module_procedure_node) :: node
        integer :: i

        node%uid = generate_uid()
        if (present(procedure_names)) then
            if (size(procedure_names) > 0) then
                allocate (node%procedure_names(size(procedure_names)))
                do i = 1, size(procedure_names)
                    node%procedure_names(i) = procedure_names(i)
                end do
            end if
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_module_procedure

    ! Complex literal implementations
    subroutine complex_literal_accept(this, visitor)
        class(complex_literal_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
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
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy specific components
        lhs%real_index = rhs%real_index
        lhs%imag_index = rhs%imag_index
    end subroutine complex_literal_assign

    ! Allocate statement implementations
    subroutine allocate_statement_accept(this, visitor)
        class(allocate_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
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

        call copy_ast_node_base_fields(lhs, rhs)
        ! Copy specific components
        if (allocated(rhs%var_indices)) then
            if (allocated(lhs%var_indices)) deallocate (lhs%var_indices)
            allocate (lhs%var_indices(size(rhs%var_indices)))
            lhs%var_indices = rhs%var_indices
        end if
        if (allocated(rhs%shape_indices)) then
            if (allocated(lhs%shape_indices)) deallocate (lhs%shape_indices)
            allocate (lhs%shape_indices(size(rhs%shape_indices)))
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
        class(ast_visitor_base_t), intent(inout) :: visitor
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

        call copy_ast_node_base_fields(lhs, rhs)
        ! Copy specific components
        if (allocated(rhs%var_indices)) then
            if (allocated(lhs%var_indices)) deallocate (lhs%var_indices)
            allocate (lhs%var_indices(size(rhs%var_indices)))
            lhs%var_indices = rhs%var_indices
        end if
        lhs%stat_var_index = rhs%stat_var_index
        lhs%errmsg_var_index = rhs%errmsg_var_index
    end subroutine deallocate_statement_assign

    ! Use statement implementations
    subroutine use_statement_accept(this, visitor)
        class(use_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
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
        call json%add(obj, 'has_double_colon', this%has_double_colon)
        call json%add(obj, 'is_intrinsic', this%is_intrinsic)
        call json%add(obj, 'is_non_intrinsic', this%is_non_intrinsic)
        call json%add(parent, obj)
    end subroutine use_statement_to_json

    subroutine use_statement_assign(lhs, rhs)
        class(use_statement_node), intent(inout) :: lhs
        class(use_statement_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy specific components
        if (allocated(rhs%module_name)) lhs%module_name = rhs%module_name
        if (allocated(rhs%url_spec)) lhs%url_spec = rhs%url_spec
        if (allocated(rhs%only_list)) then
            if (allocated(lhs%only_list)) deallocate (lhs%only_list)
            allocate (lhs%only_list(size(rhs%only_list)))
            lhs%only_list = rhs%only_list
        end if
        if (allocated(rhs%rename_list)) then
            if (allocated(lhs%rename_list)) deallocate (lhs%rename_list)
            allocate (lhs%rename_list(size(rhs%rename_list)))
            lhs%rename_list = rhs%rename_list
        end if
        lhs%has_only = rhs%has_only
        lhs%has_double_colon = rhs%has_double_colon
        lhs%is_intrinsic = rhs%is_intrinsic
        lhs%is_non_intrinsic = rhs%is_non_intrinsic
    end subroutine use_statement_assign

    subroutine visibility_statement_accept(this, visitor)
        class(visibility_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine visibility_statement_accept

    subroutine visibility_statement_to_json(this, json, parent)
        class(visibility_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'visibility_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'is_private', this%is_private)
        call json%add(obj, 'has_list', this%has_list)
        call json%add(obj, 'has_double_colon', this%has_double_colon)
        if (allocated(this%names)) then
            do i = 1, size(this%names)
                if (allocated(this%names(i)%s)) then
                    call json%add(obj, 'name_'//trim(adjustl(to_string(i))), &
                                  this%names(i)%s)
                end if
            end do
        end if
        call json%add(parent, obj)
    contains
        pure function to_string(val) result(str)
            integer, intent(in) :: val
            character(len=:), allocatable :: str

            str = int_to_string(val)
        end function to_string
    end subroutine visibility_statement_to_json

    subroutine visibility_statement_assign(lhs, rhs)
        class(visibility_statement_node), intent(inout) :: lhs
        class(visibility_statement_node), intent(in) :: rhs
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
        lhs%is_private = rhs%is_private
        lhs%has_list = rhs%has_list
        lhs%has_double_colon = rhs%has_double_colon

        if (allocated(lhs%names)) deallocate (lhs%names)
        if (allocated(rhs%names)) then
            allocate (lhs%names(size(rhs%names)))
            do i = 1, size(rhs%names)
                lhs%names(i) = rhs%names(i)
            end do
        end if
    end subroutine visibility_statement_assign

    subroutine namelist_statement_accept(this, visitor)
        class(namelist_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine namelist_statement_accept

    subroutine namelist_statement_to_json(this, json, parent)
        class(namelist_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'namelist_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%group_name)) call json%add(obj, 'group', this%group_name)
        if (allocated(this%variable_names)) then
            do i = 1, size(this%variable_names)
                if (allocated(this%variable_names(i)%s)) then
                    call json%add(obj, 'name_'//trim(adjustl(to_string(i))), &
                                  this%variable_names(i)%s)
                end if
            end do
        end if
        call json%add(parent, obj)
    contains
        pure function to_string(val) result(str)
            integer, intent(in) :: val
            character(len=:), allocatable :: str

            str = int_to_string(val)
        end function to_string
    end subroutine namelist_statement_to_json

    subroutine namelist_statement_assign(lhs, rhs)
        class(namelist_statement_node), intent(inout) :: lhs
        class(namelist_statement_node), intent(in) :: rhs
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

        if (allocated(rhs%group_name)) lhs%group_name = rhs%group_name

        if (allocated(lhs%variable_names)) deallocate (lhs%variable_names)
        if (allocated(rhs%variable_names)) then
            allocate (lhs%variable_names(size(rhs%variable_names)))
            do i = 1, size(rhs%variable_names)
                lhs%variable_names(i) = rhs%variable_names(i)
            end do
        end if
    end subroutine namelist_statement_assign

    ! Include statement implementations
    subroutine include_statement_accept(this, visitor)
        class(include_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
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
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy specific components
        if (allocated(rhs%filename)) lhs%filename = rhs%filename
    end subroutine include_statement_assign

    ! Contains node implementations
    subroutine contains_accept(this, visitor)
        class(contains_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
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
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
    end subroutine contains_assign

    ! End statement node implementations
    subroutine end_statement_accept(this, visitor)
        class(end_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine end_statement_accept

    subroutine end_statement_to_json(this, json, parent)
        class(end_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'end_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(parent, obj)
    end subroutine end_statement_to_json

    subroutine end_statement_assign(lhs, rhs)
        class(end_statement_node), intent(inout) :: lhs
        class(end_statement_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
    end subroutine end_statement_assign

    ! Interface block implementations
    subroutine interface_block_accept(this, visitor)
        class(interface_block_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
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
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy specific components
        if (allocated(rhs%name)) lhs%name = rhs%name
        if (allocated(rhs%kind)) lhs%kind = rhs%kind
        if (allocated(rhs%operator)) lhs%operator = rhs%operator
        if (allocated(rhs%procedure_indices)) then
            if (allocated(lhs%procedure_indices)) deallocate (lhs%procedure_indices)
            allocate (lhs%procedure_indices(size(rhs%procedure_indices)))
            lhs%procedure_indices = rhs%procedure_indices
        end if
    end subroutine interface_block_assign

    ! Module procedure implementations
    subroutine module_procedure_accept(this, visitor)
        class(module_procedure_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine module_procedure_accept

    subroutine module_procedure_to_json(this, json, parent)
        class(module_procedure_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        type(json_value), pointer :: arr
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'module_procedure')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%procedure_names)) then
            call json%create_array(arr, 'procedure_names')
            do i = 1, size(this%procedure_names)
                if (allocated(this%procedure_names(i)%s)) then
                    call json%add(arr, '', trim(this%procedure_names(i)%s))
                else
                    call json%add(arr, '', '')
                end if
            end do
            call json%add(obj, arr)
        end if
        call json%add(parent, obj)
    end subroutine module_procedure_to_json

    subroutine module_procedure_assign(lhs, rhs)
        class(module_procedure_node), intent(inout) :: lhs
        class(module_procedure_node), intent(in) :: rhs
        integer :: n
        type(string_t), allocatable :: tmp(:)

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type

        if (allocated(rhs%procedure_names)) then
            n = size(rhs%procedure_names)
            allocate (tmp(n))
            tmp = rhs%procedure_names
            call move_alloc(tmp, lhs%procedure_names)
        else
            if (allocated(lhs%procedure_names)) then
                call move_alloc(lhs%procedure_names, tmp)
            end if
        end if
    end subroutine module_procedure_assign

    ! Comment node methods
    subroutine comment_accept(this, visitor)
        class(comment_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
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
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy comment text
        if (allocated(rhs%text)) lhs%text = rhs%text
    end subroutine comment_assign

    ! Blank line node methods
    subroutine blank_line_accept(this, visitor)
        class(blank_line_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! For now, do nothing since we don't have a visitor framework in place
    end subroutine blank_line_accept

    subroutine blank_line_to_json(this, json, parent)
        class(blank_line_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'blank_line')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'count', this%count)
        call json%add(parent, obj)
    end subroutine blank_line_to_json

    subroutine blank_line_assign(lhs, rhs)
        class(blank_line_node), intent(inout) :: lhs
        class(blank_line_node), intent(in) :: rhs
        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy blank line count
        lhs%count = rhs%count
    end subroutine blank_line_assign

    ! Implicit statement implementations
    subroutine implicit_statement_accept(this, visitor)
        class(implicit_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
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
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type

        ! Copy implicit statement specific fields
        lhs%is_none = rhs%is_none

        ! Copy type specification
        if (allocated(rhs%type_spec%type_name)) then
            lhs%type_spec%type_name = rhs%type_spec%type_name
        else
            if (allocated(lhs%type_spec%type_name)) deallocate (lhs%type_spec%type_name)
        end if
        lhs%type_spec%has_kind = rhs%type_spec%has_kind
        lhs%type_spec%kind_value = rhs%type_spec%kind_value
        lhs%type_spec%has_length = rhs%type_spec%has_length
        lhs%type_spec%length_value = rhs%type_spec%length_value

        ! Copy letter specifications
        if (allocated(rhs%letter_specs)) then
            if (allocated(lhs%letter_specs)) deallocate (lhs%letter_specs)
            allocate (lhs%letter_specs(size(rhs%letter_specs)))
            do i = 1, size(rhs%letter_specs)
                lhs%letter_specs(i) = rhs%letter_specs(i)
            end do
        else
            if (allocated(lhs%letter_specs)) deallocate (lhs%letter_specs)
        end if
    end subroutine implicit_statement_assign

end module ast_nodes_misc
