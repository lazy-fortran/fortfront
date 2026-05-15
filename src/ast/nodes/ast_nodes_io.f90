module ast_nodes_io
    use uid_generator, only: generate_uid
    use ast_base, only: ast_node, visit_interface, &
                        ast_visitor_base_t
    implicit none
    private

    ! Public factory functions
    public :: create_print_statement
    public :: create_io_implied_do
    public :: create_open_statement
    public :: create_close_statement
    public :: create_inquire_statement
    public :: create_backspace_statement
    public :: create_rewind_statement
    public :: create_endfile_statement

    ! I/O statement AST nodes

    ! Print statement node
    type, extends(ast_node), public :: print_statement_node
        integer, allocatable :: expression_indices(:)  ! Indices to expressions to print
        character(len=:), allocatable :: format_spec  ! Optional format specifier
    contains
        procedure :: accept => print_statement_accept
        procedure :: assign => print_statement_assign
        generic :: assignment(=) => assign
    end type print_statement_node

    ! I/O implied-do node used within I/O lists
    type, extends(ast_node), public :: io_implied_do_node
        integer :: expr_index = 0
        integer, allocatable :: object_indices(:)
        character(len=:), allocatable :: var_name
        integer :: start_expr_index = 0
        integer :: end_expr_index = 0
        integer :: step_expr_index = 0
    contains
        procedure :: accept => io_implied_do_accept
        procedure :: assign => io_implied_do_assign
        generic :: assignment(=) => assign
    end type io_implied_do_node

    ! Write statement node
    type, extends(ast_node), public :: write_statement_node
        character(len=:), allocatable :: unit_spec  ! Unit specifier
        ! (e.g., "10", "*")
        character(len=:), allocatable :: format_spec  ! Optional format
        character(len=:), allocatable :: namelist_group  ! Optional namelist group
        character(len=:), allocatable :: io_control_list  ! All I/O control specifiers
        integer, allocatable :: arg_indices(:)  ! Arguments to write
        integer :: iostat_var_index = 0  ! Optional iostat variable index
        integer :: err_label_index = 0  ! Optional err label index
        integer :: end_label_index = 0  ! Optional end label index
        integer :: format_expr_index = 0  ! Optional runtime
        ! format expression
        logical :: is_formatted = .false.  ! True if formatted I/O
    contains
        procedure :: accept => write_statement_accept
        procedure :: assign => write_statement_assign
        generic :: assignment(=) => assign
    end type write_statement_node

    ! Read statement node
    type, extends(ast_node), public :: read_statement_node
        character(len=:), allocatable :: unit_spec  ! Unit specifier
        ! (e.g., "10", "*")
        character(len=:), allocatable :: format_spec  ! Optional format
        character(len=:), allocatable :: namelist_group  ! Optional namelist group
        character(len=:), allocatable :: io_control_list  ! All I/O control specifiers
        integer, allocatable :: var_indices(:)  ! Variables to read into
        integer :: iostat_var_index = 0  ! Optional iostat variable index
        integer :: err_label_index = 0  ! Optional err label index
        integer :: end_label_index = 0  ! Optional end label index
        integer :: format_expr_index = 0  ! Optional runtime
        ! format expression
        logical :: is_formatted = .false.  ! True if formatted I/O
    contains
        procedure :: accept => read_statement_accept
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
        procedure :: assign => format_descriptor_assign
        generic :: assignment(=) => assign
    end type format_descriptor_node

    ! Format statement node
    type, extends(ast_node), public :: format_statement_node
        character(len=:), allocatable :: format_spec
    contains
        procedure :: accept => format_statement_accept
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
        procedure :: assign => close_statement_assign
        generic :: assignment(=) => assign
    end type close_statement_node

    ! INQUIRE statement node
    type, extends(ast_node), public :: inquire_statement_node
        character(len=:), allocatable :: spec_list
        integer :: iostat_var_index = 0
        integer :: err_label_index = 0
    contains
        procedure :: accept => inquire_statement_accept
        procedure :: assign => inquire_statement_assign
        generic :: assignment(=) => assign
    end type inquire_statement_node

    ! BACKSPACE statement node
    type, extends(ast_node), public :: backspace_statement_node
        character(len=:), allocatable :: unit_spec
        integer :: iostat_var_index = 0
        integer :: err_label_index = 0
    contains
        procedure :: accept => backspace_statement_accept
        procedure :: assign => backspace_statement_assign
        generic :: assignment(=) => assign
    end type backspace_statement_node

    ! REWIND statement node
    type, extends(ast_node), public :: rewind_statement_node
        character(len=:), allocatable :: unit_spec
        integer :: iostat_var_index = 0
        integer :: err_label_index = 0
    contains
        procedure :: accept => rewind_statement_accept
        procedure :: assign => rewind_statement_assign
        generic :: assignment(=) => assign
    end type rewind_statement_node

    ! ENDFILE statement node
    type, extends(ast_node), public :: endfile_statement_node
        character(len=:), allocatable :: unit_spec
        integer :: iostat_var_index = 0
        integer :: err_label_index = 0
    contains
        procedure :: accept => endfile_statement_accept
        procedure :: assign => endfile_statement_assign
        generic :: assignment(=) => assign
    end type endfile_statement_node

contains

    ! Stub implementations for print_statement_node
    subroutine print_statement_accept(this, visitor)
        class(print_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine print_statement_accept

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
        if (allocated(lhs%object_indices)) deallocate (lhs%object_indices)
        if (allocated(rhs%object_indices)) then
            allocate (lhs%object_indices(size(rhs%object_indices)))
            lhs%object_indices = rhs%object_indices
        end if
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
        if (allocated(rhs%io_control_list)) lhs%io_control_list = rhs%io_control_list
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
        if (allocated(rhs%namelist_group)) lhs%namelist_group = rhs%namelist_group
        if (allocated(rhs%var_indices)) then
            if (allocated(lhs%var_indices)) deallocate (lhs%var_indices)
            allocate (lhs%var_indices(size(rhs%var_indices)))
            lhs%var_indices = rhs%var_indices
        end if
        if (allocated(rhs%io_control_list)) lhs%io_control_list = rhs%io_control_list
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
                                  end_expr_index, step_expr_index, line, column, &
                                  object_indices) result(node)
        integer, intent(in) :: expr_index
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: start_expr_index, end_expr_index
        integer, intent(in), optional :: step_expr_index
        integer, intent(in), optional :: line, column
        integer, intent(in), optional :: object_indices(:)
        type(io_implied_do_node) :: node

        node%uid = generate_uid()
        node%expr_index = expr_index
        node%var_name = var_name
        node%start_expr_index = start_expr_index
        node%end_expr_index = end_expr_index
        if (present(step_expr_index)) node%step_expr_index = step_expr_index
        if (present(line)) node%line = line
        if (present(column)) node%column = column
        if (present(object_indices)) then
            if (size(object_indices) > 0) then
                node%object_indices = object_indices
                if (node%expr_index <= 0) node%expr_index = object_indices(1)
            end if
        end if
    end function create_io_implied_do

    function create_open_statement() result(node)
        type(open_statement_node) :: node
        node%uid = generate_uid()
    end function create_open_statement

    function create_close_statement() result(node)
        type(close_statement_node) :: node
        node%uid = generate_uid()
    end function create_close_statement

    ! INQUIRE statement implementations
    subroutine inquire_statement_accept(this, visitor)
        class(inquire_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine inquire_statement_accept

    subroutine inquire_statement_assign(lhs, rhs)
        class(inquire_statement_node), intent(inout) :: lhs
        class(inquire_statement_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        if (allocated(rhs%spec_list)) lhs%spec_list = rhs%spec_list
        lhs%iostat_var_index = rhs%iostat_var_index
        lhs%err_label_index = rhs%err_label_index
    end subroutine inquire_statement_assign

    function create_inquire_statement() result(node)
        type(inquire_statement_node) :: node
        node%uid = generate_uid()
    end function create_inquire_statement

    ! BACKSPACE statement implementations
    subroutine backspace_statement_accept(this, visitor)
        class(backspace_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine backspace_statement_accept

    subroutine backspace_statement_assign(lhs, rhs)
        class(backspace_statement_node), intent(inout) :: lhs
        class(backspace_statement_node), intent(in) :: rhs

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
        lhs%iostat_var_index = rhs%iostat_var_index
        lhs%err_label_index = rhs%err_label_index
    end subroutine backspace_statement_assign

    function create_backspace_statement() result(node)
        type(backspace_statement_node) :: node
        node%uid = generate_uid()
    end function create_backspace_statement

    ! REWIND statement implementations
    subroutine rewind_statement_accept(this, visitor)
        class(rewind_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine rewind_statement_accept

    subroutine rewind_statement_assign(lhs, rhs)
        class(rewind_statement_node), intent(inout) :: lhs
        class(rewind_statement_node), intent(in) :: rhs

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
        lhs%iostat_var_index = rhs%iostat_var_index
        lhs%err_label_index = rhs%err_label_index
    end subroutine rewind_statement_assign

    function create_rewind_statement() result(node)
        type(rewind_statement_node) :: node
        node%uid = generate_uid()
    end function create_rewind_statement

    ! ENDFILE statement implementations
    subroutine endfile_statement_accept(this, visitor)
        class(endfile_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine endfile_statement_accept

    subroutine endfile_statement_assign(lhs, rhs)
        class(endfile_statement_node), intent(inout) :: lhs
        class(endfile_statement_node), intent(in) :: rhs

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
        lhs%iostat_var_index = rhs%iostat_var_index
        lhs%err_label_index = rhs%err_label_index
    end subroutine endfile_statement_assign

    function create_endfile_statement() result(node)
        type(endfile_statement_node) :: node
        node%uid = generate_uid()
    end function create_endfile_statement

end module ast_nodes_io
