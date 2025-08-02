module ast_nodes_io
    use json_module
    use ast_base, only: ast_node, visit_interface, to_json_interface
    implicit none
    private

    ! Public factory functions
    public :: create_print_statement

    ! I/O statement AST nodes

    ! Print statement node
    type, extends(ast_node), public :: print_statement_node
        integer, allocatable :: expression_indices(:)  ! Indices to expressions to print
        character(len=:), allocatable :: format_spec   ! Optional format specifier
    contains
        procedure :: accept => print_statement_accept
        procedure :: to_json => print_statement_to_json
        procedure :: assign => print_statement_assign
        generic :: assignment(=) => assign
    end type print_statement_node

    ! Write statement node
    type, extends(ast_node), public :: write_statement_node
        character(len=:), allocatable :: unit_spec     ! Unit specifier
        ! (e.g., "10", "*")
        character(len=:), allocatable :: format_spec   ! Optional format
        integer, allocatable :: arg_indices(:)         ! Arguments to write
        integer :: iostat_var_index = 0                 ! Optional iostat variable index
        integer :: err_label_index = 0                  ! Optional err label index
        integer :: end_label_index = 0                  ! Optional end label index
        integer :: format_expr_index = 0                ! Optional runtime
        ! format expression
        logical :: is_formatted = .false.               ! True if formatted I/O
    contains
        procedure :: accept => write_statement_accept
        procedure :: to_json => write_statement_to_json
        procedure :: assign => write_statement_assign
        generic :: assignment(=) => assign
    end type write_statement_node

    ! Read statement node
    type, extends(ast_node), public :: read_statement_node
        character(len=:), allocatable :: unit_spec     ! Unit specifier
        ! (e.g., "10", "*")
        character(len=:), allocatable :: format_spec   ! Optional format
        integer, allocatable :: var_indices(:)         ! Variables to read into
        integer :: iostat_var_index = 0                 ! Optional iostat variable index
        integer :: err_label_index = 0                  ! Optional err label index
        integer :: end_label_index = 0                  ! Optional end label index
        integer :: format_expr_index = 0                ! Optional runtime
        ! format expression
        logical :: is_formatted = .false.               ! True if formatted I/O
    contains
        procedure :: accept => read_statement_accept
        procedure :: to_json => read_statement_to_json
        procedure :: assign => read_statement_assign
        generic :: assignment(=) => assign
    end type read_statement_node

    ! Format descriptor node for parsed format specifications
    type, extends(ast_node), public :: format_descriptor_node
        character(len=:), allocatable :: descriptor_type  ! I, F, E, A, X, etc.
        integer :: width = 0                             ! Field width
        integer :: decimal_places = 0                     ! Decimal places (for F, E)
        integer :: exponent_width = 0                     ! Exponent width (for E)
        integer :: repeat_count = 1                       ! Repetition count
        logical :: is_literal = .false.                  ! True for literal strings
        character(len=:), allocatable :: literal_text     ! For literal format strings
    contains
        procedure :: accept => format_descriptor_accept
        procedure :: to_json => format_descriptor_to_json
        procedure :: assign => format_descriptor_assign
        generic :: assignment(=) => assign
    end type format_descriptor_node

contains

    ! Stub implementations for print_statement_node
    subroutine print_statement_accept(this, visitor)
        class(print_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
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
        if (allocated(rhs%inferred_type)) then
            if (allocated(lhs%inferred_type)) deallocate(lhs%inferred_type)
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        else
            if (allocated(lhs%inferred_type)) deallocate(lhs%inferred_type)
        end if
        ! Copy specific fields
        if (allocated(rhs%expression_indices)) then
            if (allocated(lhs%expression_indices)) deallocate(lhs%expression_indices)
            allocate(lhs%expression_indices(size(rhs%expression_indices)))
            lhs%expression_indices = rhs%expression_indices
        end if
        if (allocated(rhs%format_spec)) lhs%format_spec = rhs%format_spec
    end subroutine print_statement_assign

    ! Stub implementations for write_statement_node
    subroutine write_statement_accept(this, visitor)
        class(write_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
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
        if (allocated(rhs%inferred_type)) then
            if (allocated(lhs%inferred_type)) deallocate(lhs%inferred_type)
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        else
            if (allocated(lhs%inferred_type)) deallocate(lhs%inferred_type)
        end if
        ! Copy specific fields
        if (allocated(rhs%unit_spec)) lhs%unit_spec = rhs%unit_spec
        if (allocated(rhs%format_spec)) lhs%format_spec = rhs%format_spec
        if (allocated(rhs%arg_indices)) then
            if (allocated(lhs%arg_indices)) deallocate(lhs%arg_indices)
            allocate(lhs%arg_indices(size(rhs%arg_indices)))
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
        class(*), intent(inout) :: visitor
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
        if (allocated(rhs%inferred_type)) then
            if (allocated(lhs%inferred_type)) deallocate(lhs%inferred_type)
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        else
            if (allocated(lhs%inferred_type)) deallocate(lhs%inferred_type)
        end if
        ! Copy specific fields
        if (allocated(rhs%unit_spec)) lhs%unit_spec = rhs%unit_spec
        if (allocated(rhs%format_spec)) lhs%format_spec = rhs%format_spec
        if (allocated(rhs%var_indices)) then
            if (allocated(lhs%var_indices)) deallocate(lhs%var_indices)
            allocate(lhs%var_indices(size(rhs%var_indices)))
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
        class(*), intent(inout) :: visitor
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
            'descriptor_type', this%descriptor_type)
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
        if (allocated(rhs%inferred_type)) then
            if (allocated(lhs%inferred_type)) deallocate(lhs%inferred_type)
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        else
            if (allocated(lhs%inferred_type)) deallocate(lhs%inferred_type)
        end if
        ! Copy specific fields
        if (allocated(rhs%descriptor_type)) lhs%descriptor_type = rhs%descriptor_type
        lhs%width = rhs%width
        lhs%decimal_places = rhs%decimal_places
        lhs%exponent_width = rhs%exponent_width
        lhs%repeat_count = rhs%repeat_count
        lhs%is_literal = rhs%is_literal
        if (allocated(rhs%literal_text)) lhs%literal_text = rhs%literal_text
    end subroutine format_descriptor_assign

    ! Factory functions
    function create_print_statement(expression_indices, format_spec, &
                                   line, column) result(node)
        integer, intent(in), optional :: expression_indices(:)
        character(len=*), intent(in), optional :: format_spec
        integer, intent(in), optional :: line, column
        type(print_statement_node) :: node

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

end module ast_nodes_io