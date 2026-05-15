        procedure :: accept => comment_accept
        procedure :: assign => comment_assign
        generic :: assignment(=) => assign
    end type comment_node

    ! Directive node (OpenMP/OpenACC sentinels)
    type, extends(ast_node), public :: directive_node
        character(len=:), allocatable :: text
        logical :: is_openmp = .false.
        logical :: is_openacc = .false.
    contains
        procedure :: accept => directive_accept
        procedure :: assign => directive_assign
        generic :: assignment(=) => assign
    end type directive_node

    ! Blank line node (for preserving source formatting)
    type, extends(ast_node), public :: blank_line_node
        integer :: count = 1  ! Number of consecutive blank lines
    contains
        procedure :: accept => blank_line_accept
        procedure :: assign => blank_line_assign
        generic :: assignment(=) => assign
    end type blank_line_node

    ! Complex literal node
    type, extends(ast_node), public :: complex_literal_node
        integer :: real_index = 0  ! Index to real part expression in arena
        integer :: imag_index = 0  ! Index to imaginary part expression in arena
    contains
        procedure :: accept => complex_literal_accept
        procedure :: assign => complex_literal_assign
        generic :: assignment(=) => assign
    end type complex_literal_node

    ! Allocate statement node
    type, extends(ast_node), public :: allocate_statement_node
        integer, allocatable :: var_indices(:)  ! Variables to allocate
        integer, allocatable :: shape_indices(:)  ! Shape expressions
        ! for each variable
        character(len=:), allocatable :: type_spec  ! Optional type-spec
        integer :: stat_var_index = 0  ! Optional stat variable index
        integer :: errmsg_var_index = 0  ! Optional errmsg variable index
        integer :: source_expr_index = 0  ! Optional source
        ! expression index
        integer :: mold_expr_index = 0  ! Optional mold expression index
    contains
        procedure :: accept => allocate_statement_accept
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
        procedure :: assign => deallocate_statement_assign
        generic :: assignment(=) => assign
    end type deallocate_statement_node

    ! Use statement node
    type, extends(ast_node), public :: use_statement_node
        character(len=:), allocatable :: module_name
        character(len=:), allocatable :: url_spec
        ! Optional URL specification for Go-style imports
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
        procedure :: assign => use_statement_assign
        generic :: assignment(=) => assign
    end type use_statement_node

    ! Intrinsic statement node
    type, extends(ast_node), public :: intrinsic_statement_node
        type(string_t), allocatable :: procedure_names(:)
        logical :: has_double_colon = .false.
    contains
        procedure :: accept => intrinsic_statement_accept
        procedure :: assign => intrinsic_statement_assign
        generic :: assignment(=) => assign
    end type intrinsic_statement_node

    type, extends(ast_node), public :: visibility_statement_node
        type(string_t), allocatable :: names(:)
        logical :: is_private = .false.
        logical :: has_list = .false.
        logical :: has_double_colon = .false.
    contains
        procedure :: accept => visibility_statement_accept
        procedure :: assign => visibility_statement_assign
        generic :: assignment(=) => assign
    end type visibility_statement_node

    type, extends(ast_node), public :: namelist_statement_node
        character(len=:), allocatable :: group_name
        type(string_t), allocatable :: variable_names(:)
    contains
        procedure :: accept => namelist_statement_accept
        procedure :: assign => namelist_statement_assign
        generic :: assignment(=) => assign
    end type namelist_statement_node

    ! DATA statement node
    type, extends(ast_node), public :: data_statement_node
        integer, allocatable :: object_indices(:)
        integer, allocatable :: value_indices(:)
    contains
        procedure :: accept => data_statement_accept
        procedure :: assign => data_statement_assign
        generic :: assignment(=) => assign
    end type data_statement_node

    ! Include statement node
    type, extends(ast_node), public :: include_statement_node
        character(len=:), allocatable :: filename
    contains
        procedure :: accept => include_statement_accept
        procedure :: assign => include_statement_assign
        generic :: assignment(=) => assign
    end type include_statement_node

    ! Import statement node
    type, extends(ast_node), public :: import_statement_node
        type(string_t), allocatable :: import_list(:)
        logical :: has_double_colon = .false.
        logical :: has_list = .false.
        logical :: is_all = .false.
        logical :: is_none = .false.
    contains
        procedure :: accept => import_statement_accept
        procedure :: assign => import_statement_assign
        generic :: assignment(=) => assign
    end type import_statement_node

    ! Statement function node
    type, extends(ast_node), public :: statement_function_node
        character(len=:), allocatable :: name
        character(len=:), allocatable :: arg_names(:)
        integer :: body_expr_index = 0
    contains
        procedure :: accept => statement_function_accept
        procedure :: assign => statement_function_assign
        generic :: assignment(=) => assign
    end type statement_function_node

    ! Contains node
    type, extends(ast_node), public :: contains_node
    contains
        procedure :: accept => contains_accept
        procedure :: assign => contains_assign
        generic :: assignment(=) => assign
    end type contains_node

    ! End statement node (for implicit program termination)
    type, extends(ast_node), public :: end_statement_node
    contains
        procedure :: accept => end_statement_accept
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
        logical :: is_abstract = .false.  ! True for abstract interface
        integer, allocatable :: procedure_indices(:)  ! Procedure declaration
        ! arena indices
    contains
        procedure :: accept => interface_block_accept
        procedure :: assign => interface_block_assign
        generic :: assignment(=) => assign
    end type interface_block_node

    type, extends(ast_node), public :: module_procedure_node
        type(string_t), allocatable :: procedure_names(:)
        logical :: has_module_prefix = .true.
        logical :: has_double_colon = .false.
    contains
        procedure :: accept => module_procedure_accept
        procedure :: assign => module_procedure_assign
        generic :: assignment(=) => assign
    end type module_procedure_node

    ! Letter specification for implicit statements
    type, public :: implicit_letter_spec_t
        character :: start_letter = ' '  ! Starting letter of range
        character :: end_letter = ' '
        ! Ending letter of range (same as start for single letters)
    end type implicit_letter_spec_t

    ! Type specification for implicit statements
    type, public :: implicit_type_spec_t
        character(len=:), allocatable :: type_name
        ! "real", "integer", "character", etc.
        logical :: has_kind = .false.
        integer :: kind_value = 0
        logical :: has_length = .false.  ! For character types
        integer :: length_value = 0
    end type implicit_type_spec_t

    ! Implicit statement node
    type, extends(ast_node), public :: implicit_statement_node
        logical :: is_none = .false.  ! True for "implicit none"
        character(len=:), allocatable :: none_spec
        ! Specification for implicit none: type, external, etc.
        type(implicit_type_spec_t) :: type_spec  ! Type specification
        type(implicit_letter_spec_t), allocatable :: letter_specs(:)
        ! Letter ranges/singles
    contains
        procedure :: accept => implicit_statement_accept
        procedure :: assign => implicit_statement_assign
        generic :: assignment(=) => assign
    end type implicit_statement_node

    ! Constructors migrated from ast_core
    public :: create_comment, create_blank_line, create_end_statement
    public :: create_use_statement, create_visibility_statement, &
              create_include_statement
    public :: create_import_statement
    public :: create_implicit_statement, create_interface_block, &
              create_module_procedure

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
        logical, intent(in), optional :: has_double_colon, is_intrinsic, &
                                         is_non_intrinsic
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
                                       line, column, none_spec) result(node)
        use uid_generator, only: generate_uid
        logical, intent(in) :: is_none
        character(len=*), intent(in), optional :: type_name
        integer, intent(in), optional :: kind_value
        logical, intent(in), optional :: has_kind
        integer, intent(in), optional :: length_value
        logical, intent(in), optional :: has_length
        character(len=*), intent(in), optional :: letter_ranges(:)
        integer, intent(in), optional :: line, column
        character(len=*), intent(in), optional :: none_spec
        type(implicit_statement_node) :: node
        integer :: i, dash_pos, upper_idx

        node%is_none = is_none
        node%uid = generate_uid()

        if (is_none .and. present(none_spec)) then
            node%none_spec = trim(none_spec)
        end if

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

    function create_import_statement(import_list, has_double_colon, is_all, &
                                     is_none, line, column) result(node)
        use uid_generator, only: generate_uid
        character(len=*), intent(in), optional :: import_list(:)
        logical, intent(in), optional :: has_double_colon, is_all, is_none
        integer, intent(in), optional :: line, column
        type(import_statement_node) :: node
        integer :: i

        node%uid = generate_uid()
        if (present(has_double_colon)) node%has_double_colon = has_double_colon
        if (present(is_all)) node%is_all = is_all
        if (present(is_none)) node%is_none = is_none

        if (present(import_list)) then
            if (size(import_list) > 0) then
                node%has_list = .true.
                allocate (node%import_list(size(import_list)))
                do i = 1, size(import_list)
                    node%import_list(i)%s = import_list(i)
                end do
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_import_statement

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

    function create_module_procedure(procedure_names, line, column, &
                                     has_module_prefix, has_double_colon) &
        result(node)
        use uid_generator, only: generate_uid
        type(string_t), intent(in), optional :: procedure_names(:)
        integer, intent(in), optional :: line, column
        logical, intent(in), optional :: has_module_prefix
        logical, intent(in), optional :: has_double_colon
        type(module_procedure_node) :: node
        integer :: i

        node%uid = generate_uid()
        if (present(has_module_prefix)) then
            node%has_module_prefix = has_module_prefix
        else
            node%has_module_prefix = .true.
        end if
        if (present(has_double_colon)) then
            node%has_double_colon = has_double_colon
        else
            node%has_double_colon = .false.
        end if
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

    subroutine allocate_statement_assign(lhs, rhs)
        class(allocate_statement_node), intent(inout) :: lhs
        class(allocate_statement_node), intent(in) :: rhs

        call copy_ast_node_base_fields(lhs, rhs)
        ! Copy specific components
        if (allocated(rhs%var_indices)) then
            lhs%var_indices = rhs%var_indices
        end if
        if (allocated(rhs%shape_indices)) then
            lhs%shape_indices = rhs%shape_indices
        end if
        if (allocated(rhs%type_spec)) then
            lhs%type_spec = rhs%type_spec
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

    subroutine deallocate_statement_assign(lhs, rhs)
        class(deallocate_statement_node), intent(inout) :: lhs
        class(deallocate_statement_node), intent(in) :: rhs

        call copy_ast_node_base_fields(lhs, rhs)
        ! Copy specific components
        if (allocated(rhs%var_indices)) then
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
            lhs%only_list = rhs%only_list
        end if
        if (allocated(rhs%rename_list)) then
            lhs%rename_list = rhs%rename_list
        end if
        lhs%has_only = rhs%has_only
        lhs%has_double_colon = rhs%has_double_colon
        lhs%is_intrinsic = rhs%is_intrinsic
        lhs%is_non_intrinsic = rhs%is_non_intrinsic
    end subroutine use_statement_assign

    ! Intrinsic statement implementations
    subroutine intrinsic_statement_accept(this, visitor)
        class(intrinsic_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine intrinsic_statement_accept

    subroutine intrinsic_statement_assign(lhs, rhs)
        class(intrinsic_statement_node), intent(inout) :: lhs
        class(intrinsic_statement_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type

        lhs%has_double_colon = rhs%has_double_colon

        if (allocated(rhs%procedure_names)) then
            lhs%procedure_names = rhs%procedure_names
        end if
    end subroutine intrinsic_statement_assign

    subroutine visibility_statement_accept(this, visitor)
        class(visibility_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine visibility_statement_accept

    subroutine visibility_statement_assign(lhs, rhs)
        class(visibility_statement_node), intent(inout) :: lhs
        class(visibility_statement_node), intent(in) :: rhs

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

        if (allocated(rhs%names)) then
            lhs%names = rhs%names
        end if
    end subroutine visibility_statement_assign

    subroutine namelist_statement_accept(this, visitor)
        class(namelist_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine namelist_statement_accept
