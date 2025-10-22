module ast_nodes_io
    use json_module
    use uid_generator, only: generate_uid
    use ast_base, only: ast_node, visit_interface, to_json_interface, &
                        ast_visitor_base_t
    implicit none
    private

    ! Public factory functions
    public :: create_print_statement
    public :: create_io_implied_do
    public :: create_open_statement
    public :: create_close_statement

    ! I/O statement AST nodes

    ! Print statement node
    type, extends(ast_node), public :: print_statement_node
        integer, allocatable :: expression_indices(:)  ! Indices to expressions to print
        character(len=:), allocatable :: format_spec  ! Optional format specifier
    contains
        procedure :: accept => print_statement_accept
        procedure :: to_json => print_statement_to_json
        procedure :: assign => print_statement_assign
        generic :: assignment(=) => assign
    end type print_statement_node

    ! I/O implied-do node used within I/O lists
    type, extends(ast_node), public :: io_implied_do_node
        integer :: expr_index = 0
        character(len=:), allocatable :: var_name
        integer :: start_expr_index = 0
        integer :: end_expr_index = 0
        integer :: step_expr_index = 0
    contains
        procedure :: accept => io_implied_do_accept
        procedure :: to_json => io_implied_do_to_json
        procedure :: assign => io_implied_do_assign
        generic :: assignment(=) => assign
    end type io_implied_do_node

    ! Write statement node
    type, extends(ast_node), public :: write_statement_node
        character(len=:), allocatable :: unit_spec  ! Unit specifier
        ! (e.g., "10", "*")
        character(len=:), allocatable :: format_spec  ! Optional format
        character(len=:), allocatable :: namelist_group  ! Optional namelist group
        integer, allocatable :: arg_indices(:)  ! Arguments to write
        integer :: iostat_var_index = 0  ! Optional iostat variable index
        integer :: err_label_index = 0  ! Optional err label index
        integer :: end_label_index = 0  ! Optional end label index
        integer :: format_expr_index = 0  ! Optional runtime
        ! format expression
        logical :: is_formatted = .false.  ! True if formatted I/O
    contains
        procedure :: accept => write_statement_accept
        procedure :: to_json => write_statement_to_json
        procedure :: assign => write_statement_assign
        generic :: assignment(=) => assign
    end type write_statement_node

    ! Read statement node
    type, extends(ast_node), public :: read_statement_node
        character(len=:), allocatable :: unit_spec  ! Unit specifier
        ! (e.g., "10", "*")
        character(len=:), allocatable :: format_spec  ! Optional format
        integer, allocatable :: var_indices(:)  ! Variables to read into
        integer :: iostat_var_index = 0  ! Optional iostat variable index
        integer :: err_label_index = 0  ! Optional err label index
        integer :: end_label_index = 0  ! Optional end label index
        integer :: format_expr_index = 0  ! Optional runtime
        ! format expression
        logical :: is_formatted = .false.  ! True if formatted I/O
    contains
        procedure :: accept => read_statement_accept
        procedure :: to_json => read_statement_to_json
        procedure :: assign => read_statement_assign
        generic :: assignment(=) => assign
    end type read_statement_node

    ! Format descriptor node for parsed format specifications
    type, extends(ast_node), public :: format_descriptor_node
        character(len=:), allocatable :: descriptor_type  ! I, F, E, A, X, etc.
        integer :: width = 0  ! Field width
        integer :: decimal_places = 0  ! Decimal places (for F, E)
        integer :: exponent_width = 0  ! Exponent width (for E)
        integer :: repeat_count = 1  ! Repetition count
        logical :: is_literal = .false.  ! True for literal strings
        character(len=:), allocatable :: literal_text  ! For literal format strings
    contains
        procedure :: accept => format_descriptor_accept
        procedure :: to_json => format_descriptor_to_json
        procedure :: assign => format_descriptor_assign
        generic :: assignment(=) => assign
    end type format_descriptor_node

    ! Format statement node
    type, extends(ast_node), public :: format_statement_node
        character(len=:), allocatable :: format_spec
    contains
        procedure :: accept => format_statement_accept
        procedure :: to_json => format_statement_to_json
        procedure :: assign => format_statement_assign
        generic :: assignment(=) => assign
    end type format_statement_node

    ! OPEN statement node
    type, extends(ast_node), public :: open_statement_node
        character(len=:), allocatable :: unit_spec
        character(len=:), allocatable :: file_spec
        character(len=:), allocatable :: status_spec
        character(len=:), allocatable :: access_spec
        character(len=:), allocatable :: form_spec
        character(len=:), allocatable :: recl_spec
        character(len=:), allocatable :: blank_spec
        character(len=:), allocatable :: position_spec
        character(len=:), allocatable :: action_spec
        character(len=:), allocatable :: delim_spec
        character(len=:), allocatable :: pad_spec
        integer :: iostat_var_index = 0
        integer :: err_label_index = 0
    contains
        procedure :: accept => open_statement_accept
        procedure :: to_json => open_statement_to_json
        procedure :: assign => open_statement_assign
        generic :: assignment(=) => assign
    end type open_statement_node

    ! CLOSE statement node
    type, extends(ast_node), public :: close_statement_node
        character(len=:), allocatable :: unit_spec
        character(len=:), allocatable :: status_spec
        integer :: iostat_var_index = 0
        integer :: err_label_index = 0
    contains
        procedure :: accept => close_statement_accept
        procedure :: to_json => close_statement_to_json
        procedure :: assign => close_statement_assign
        generic :: assignment(=) => assign
    end type close_statement_node

contains

    ! Stub implementations for print_statement_node
    subroutine print_statement_accept(this, visitor)
        class(print_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine print_statement_accept

    subroutine print_statement_to_json(this, json, parent)
        class(print_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine print_statement_to_json

    subroutine print_statement_assign(lhs, rhs)
        class(print_statement_node), intent(inout) :: lhs
        class(print_statement_node), intent(in) :: rhs
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
        ! Copy specific fields
        if (allocated(rhs%expression_indices)) then
            if (allocated(lhs%expression_indices)) deallocate (lhs%expression_indices)
            allocate (lhs%expression_indices(size(rhs%expression_indices)))
            lhs%expression_indices = rhs%expression_indices
        end if
        if (allocated(rhs%format_spec)) lhs%format_spec = rhs%format_spec
    end subroutine print_statement_assign

    ! Stub implementations for io_implied_do_node
    subroutine io_implied_do_accept(this, visitor)
        class(io_implied_do_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine io_implied_do_accept

    subroutine io_implied_do_to_json(this, json, parent)
        class(io_implied_do_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine io_implied_do_to_json

    subroutine io_implied_do_assign(lhs, rhs)
        class(io_implied_do_node), intent(inout) :: lhs
        class(io_implied_do_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        lhs%expr_index = rhs%expr_index
        if (allocated(rhs%var_name)) lhs%var_name = rhs%var_name
        lhs%start_expr_index = rhs%start_expr_index
        lhs%end_expr_index = rhs%end_expr_index
        lhs%step_expr_index = rhs%step_expr_index
    end subroutine io_implied_do_assign

    ! Stub implementations for write_statement_node
    subroutine write_statement_accept(this, visitor)
        class(write_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine write_statement_accept

    subroutine write_statement_to_json(this, json, parent)
        class(write_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine write_statement_to_json

    subroutine write_statement_assign(lhs, rhs)
        class(write_statement_node), intent(inout) :: lhs
        class(write_statement_node), intent(in) :: rhs
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
        ! Copy specific fields
        if (allocated(rhs%unit_spec)) lhs%unit_spec = rhs%unit_spec
        if (allocated(rhs%format_spec)) lhs%format_spec = rhs%format_spec
        if (allocated(rhs%namelist_group)) lhs%namelist_group = rhs%namelist_group
        if (allocated(rhs%arg_indices)) then
            if (allocated(lhs%arg_indices)) deallocate (lhs%arg_indices)
            allocate (lhs%arg_indices(size(rhs%arg_indices)))
            lhs%arg_indices = rhs%arg_indices
        end if
        lhs%iostat_var_index = rhs%iostat_var_index
        lhs%err_label_index = rhs%err_label_index
        lhs%end_label_index = rhs%end_label_index
        lhs%format_expr_index = rhs%format_expr_index
        lhs%is_formatted = rhs%is_formatted
    end subroutine write_statement_assign

    ! Read statement implementations
    subroutine read_statement_accept(this, visitor)
        class(read_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine read_statement_accept

    subroutine read_statement_to_json(this, json, parent)
        class(read_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'read_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%unit_spec)) call json%add(obj, 'unit_spec', this%unit_spec)
        if (allocated(this%format_spec)) call json%add(obj, 'format_spec', &
                                                       this%format_spec)
        if (this%iostat_var_index > 0) call json%add(obj, 'iostat_var_index', &
                                                     this%iostat_var_index)
        if (this%err_label_index > 0) call json%add(obj, 'err_label_index', &
                                                    this%err_label_index)
        if (this%end_label_index > 0) call json%add(obj, 'end_label_index', &
                                                    this%end_label_index)
        if (this%format_expr_index > 0) call json%add(obj, 'format_expr_index', &
                                                      this%format_expr_index)
        call json%add(obj, 'is_formatted', this%is_formatted)
        call json%add(parent, obj)
    end subroutine read_statement_to_json

    subroutine read_statement_assign(lhs, rhs)
        class(read_statement_node), intent(inout) :: lhs
        class(read_statement_node), intent(in) :: rhs
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
        ! Copy specific fields
        if (allocated(rhs%unit_spec)) lhs%unit_spec = rhs%unit_spec
        if (allocated(rhs%format_spec)) lhs%format_spec = rhs%format_spec
        if (allocated(rhs%var_indices)) then
            if (allocated(lhs%var_indices)) deallocate (lhs%var_indices)
            allocate (lhs%var_indices(size(rhs%var_indices)))
            lhs%var_indices = rhs%var_indices
        end if
        lhs%iostat_var_index = rhs%iostat_var_index
        lhs%err_label_index = rhs%err_label_index
        lhs%end_label_index = rhs%end_label_index
        lhs%format_expr_index = rhs%format_expr_index
        lhs%is_formatted = rhs%is_formatted
    end subroutine read_statement_assign

    ! Format descriptor implementations
    subroutine format_descriptor_accept(this, visitor)
        class(format_descriptor_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine format_descriptor_accept

    subroutine format_descriptor_to_json(this, json, parent)
        class(format_descriptor_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'format_descriptor')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%descriptor_type)) call json%add(obj, &
                                                           'descriptor_type', &
                                                           this%descriptor_type)
        call json%add(obj, 'width', this%width)
        call json%add(obj, 'decimal_places', this%decimal_places)
        call json%add(obj, 'exponent_width', this%exponent_width)
        call json%add(obj, 'repeat_count', this%repeat_count)
        call json%add(obj, 'is_literal', this%is_literal)
        if (allocated(this%literal_text)) call json%add(obj, 'literal_text', &
                                                        this%literal_text)
        call json%add(parent, obj)
    end subroutine format_descriptor_to_json

    subroutine format_descriptor_assign(lhs, rhs)
        class(format_descriptor_node), intent(inout) :: lhs
        class(format_descriptor_node), intent(in) :: rhs
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
        ! Copy specific fields
        if (allocated(rhs%descriptor_type)) lhs%descriptor_type = rhs%descriptor_type
        lhs%width = rhs%width
        lhs%decimal_places = rhs%decimal_places
        lhs%exponent_width = rhs%exponent_width
        lhs%repeat_count = rhs%repeat_count
        lhs%is_literal = rhs%is_literal
        if (allocated(rhs%literal_text)) lhs%literal_text = rhs%literal_text
    end subroutine format_descriptor_assign

    ! Format statement implementations
    subroutine format_statement_accept(this, visitor)
        class(format_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine format_statement_accept

    subroutine format_statement_to_json(this, json, parent)
        class(format_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'format_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%format_spec)) then
            call json%add(obj, 'format_spec', this%format_spec)
        end if
        call json%add(parent, obj)
    end subroutine format_statement_to_json

    subroutine format_statement_assign(lhs, rhs)
        class(format_statement_node), intent(inout) :: lhs
        class(format_statement_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        if (allocated(rhs%format_spec)) lhs%format_spec = rhs%format_spec
    end subroutine format_statement_assign

    ! OPEN statement implementations
    subroutine open_statement_accept(this, visitor)
        class(open_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine open_statement_accept

    subroutine open_statement_to_json(this, json, parent)
        class(open_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'open_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%unit_spec)) call json%add(obj, 'unit_spec', this%unit_spec)
        if (allocated(this%file_spec)) call json%add(obj, 'file_spec', this%file_spec)
        if (allocated(this%status_spec)) call json%add(obj, 'status_spec', &
                                                       this%status_spec)
        call json%add(parent, obj)
    end subroutine open_statement_to_json

    subroutine open_statement_assign(lhs, rhs)
        class(open_statement_node), intent(inout) :: lhs
        class(open_statement_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        if (allocated(rhs%unit_spec)) lhs%unit_spec = rhs%unit_spec
        if (allocated(rhs%file_spec)) lhs%file_spec = rhs%file_spec
        if (allocated(rhs%status_spec)) lhs%status_spec = rhs%status_spec
        if (allocated(rhs%access_spec)) lhs%access_spec = rhs%access_spec
        if (allocated(rhs%form_spec)) lhs%form_spec = rhs%form_spec
        if (allocated(rhs%recl_spec)) lhs%recl_spec = rhs%recl_spec
        if (allocated(rhs%blank_spec)) lhs%blank_spec = rhs%blank_spec
        if (allocated(rhs%position_spec)) lhs%position_spec = rhs%position_spec
        if (allocated(rhs%action_spec)) lhs%action_spec = rhs%action_spec
        if (allocated(rhs%delim_spec)) lhs%delim_spec = rhs%delim_spec
        if (allocated(rhs%pad_spec)) lhs%pad_spec = rhs%pad_spec
        lhs%iostat_var_index = rhs%iostat_var_index
        lhs%err_label_index = rhs%err_label_index
    end subroutine open_statement_assign

    ! CLOSE statement implementations
    subroutine close_statement_accept(this, visitor)
        class(close_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine close_statement_accept

    subroutine close_statement_to_json(this, json, parent)
        class(close_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'close_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%unit_spec)) call json%add(obj, 'unit_spec', this%unit_spec)
        if (allocated(this%status_spec)) call json%add(obj, 'status_spec', &
                                                       this%status_spec)
        call json%add(parent, obj)
    end subroutine close_statement_to_json

    subroutine close_statement_assign(lhs, rhs)
        class(close_statement_node), intent(inout) :: lhs
        class(close_statement_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        if (allocated(rhs%unit_spec)) lhs%unit_spec = rhs%unit_spec
        if (allocated(rhs%status_spec)) lhs%status_spec = rhs%status_spec
        lhs%iostat_var_index = rhs%iostat_var_index
        lhs%err_label_index = rhs%err_label_index
    end subroutine close_statement_assign

    ! Factory functions
    function create_print_statement(expression_indices, format_spec, &
                                    line, column) result(node)
        integer, intent(in), optional :: expression_indices(:)
        character(len=*), intent(in), optional :: format_spec
        integer, intent(in), optional :: line, column
        type(print_statement_node) :: node

        node%uid = generate_uid()
        if (present(expression_indices)) then
            if (size(expression_indices) > 0) then
                node%expression_indices = expression_indices
            end if
        end if
        if (present(format_spec)) then
            node%format_spec = format_spec
        else
            node%format_spec = "*"
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_print_statement

    function create_io_implied_do(expr_index, var_name, start_expr_index, &
                                  end_expr_index, step_expr_index, line, column) &
        result(node)
        integer, intent(in) :: expr_index
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: start_expr_index, end_expr_index
        integer, intent(in), optional :: step_expr_index
        integer, intent(in), optional :: line, column
        type(io_implied_do_node) :: node

        node%uid = generate_uid()
        node%expr_index = expr_index
        node%var_name = var_name
        node%start_expr_index = start_expr_index
        node%end_expr_index = end_expr_index
        if (present(step_expr_index)) node%step_expr_index = step_expr_index
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_io_implied_do

    function create_open_statement() result(node)
        type(open_statement_node) :: node
        node%uid = generate_uid()
    end function create_open_statement

    function create_close_statement() result(node)
        type(close_statement_node) :: node
        node%uid = generate_uid()
    end function create_close_statement

end module ast_nodes_io
