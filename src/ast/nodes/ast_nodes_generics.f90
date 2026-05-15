module ast_nodes_generics
    use ast_base, only: ast_node, ast_visitor_base_t
    implicit none
    private

    public :: template_block_node
    public :: instantiate_statement_node
    public :: trait_block_node
    public :: requirement_block_node
    public :: implements_block_node
    public :: create_template_block
    public :: create_instantiate_statement
    public :: create_trait_block
    public :: create_requirement_block
    public :: create_implements_block

    type, extends(ast_node), public :: template_block_node
        character(len=:), allocatable :: name
        character(len=:), allocatable :: parameter_names(:)
        integer, allocatable :: declaration_indices(:)
        integer, allocatable :: procedure_indices(:)
        logical :: has_contains = .false.
    contains
        procedure :: accept => template_block_accept
        procedure :: assign => template_block_assign
        generic :: assignment(=) => assign
    end type template_block_node

    type, extends(ast_node), public :: instantiate_statement_node
        character(len=:), allocatable :: template_name
        character(len=:), allocatable :: spec_text
    contains
        procedure :: accept => instantiate_statement_accept
        procedure :: assign => instantiate_statement_assign
        generic :: assignment(=) => assign
    end type instantiate_statement_node

    type, extends(ast_node), public :: trait_block_node
        character(len=:), allocatable :: name
        character(len=:), allocatable :: parameter_names(:)
        integer, allocatable :: declaration_indices(:)
        integer, allocatable :: procedure_indices(:)
        logical :: has_contains = .false.
    contains
        procedure :: accept => trait_block_accept
        procedure :: assign => trait_block_assign
        generic :: assignment(=) => assign
    end type trait_block_node

    type, extends(ast_node), public :: requirement_block_node
        character(len=:), allocatable :: name
        character(len=:), allocatable :: parameter_names(:)
        integer, allocatable :: declaration_indices(:)
        integer, allocatable :: procedure_indices(:)
        logical :: has_contains = .false.
    contains
        procedure :: accept => requirement_block_accept
        procedure :: assign => requirement_block_assign
        generic :: assignment(=) => assign
    end type requirement_block_node

    type, extends(ast_node), public :: implements_block_node
        character(len=:), allocatable :: name
        character(len=:), allocatable :: parameter_names(:)
        integer, allocatable :: declaration_indices(:)
        integer, allocatable :: procedure_indices(:)
        logical :: has_contains = .false.
    contains
        procedure :: accept => implements_block_accept
        procedure :: assign => implements_block_assign
        generic :: assignment(=) => assign
    end type implements_block_node

contains

    function create_template_block(name, parameter_names, declaration_indices, &
                                   procedure_indices, has_contains, line, column) &
        result(node)
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: parameter_names(:)
        integer, intent(in), optional :: declaration_indices(:)
        integer, intent(in), optional :: procedure_indices(:)
        logical, intent(in), optional :: has_contains
        integer, intent(in), optional :: line, column
        type(template_block_node) :: node

        node%name = name
        if (present(parameter_names)) then
            if (size(parameter_names) > 0) node%parameter_names = parameter_names
        end if
        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) node%declaration_indices = &
                declaration_indices
        end if
        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) node%procedure_indices = procedure_indices
        end if
        if (present(has_contains)) node%has_contains = has_contains
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_template_block

    function create_instantiate_statement(template_name, spec_text, line, column) &
        result(node)
        character(len=*), intent(in) :: template_name
        character(len=*), intent(in) :: spec_text
        integer, intent(in), optional :: line, column
        type(instantiate_statement_node) :: node

        if (len_trim(template_name) > 0) node%template_name = template_name
        if (len_trim(spec_text) > 0) node%spec_text = spec_text
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_instantiate_statement

    function create_trait_block(name, parameter_names, declaration_indices, &
                                procedure_indices, has_contains, line, column) &
        result(node)
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: parameter_names(:)
        integer, intent(in), optional :: declaration_indices(:)
        integer, intent(in), optional :: procedure_indices(:)
        logical, intent(in), optional :: has_contains
        integer, intent(in), optional :: line, column
        type(trait_block_node) :: node

        node%name = name
        if (present(parameter_names)) then
            if (size(parameter_names) > 0) node%parameter_names = parameter_names
        end if
        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) node%declaration_indices = &
                declaration_indices
        end if
        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) node%procedure_indices = procedure_indices
        end if
        if (present(has_contains)) node%has_contains = has_contains
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_trait_block

    function create_requirement_block(name, parameter_names, declaration_indices, &
                                      procedure_indices, has_contains, line, column) &
        result(node)
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: parameter_names(:)
        integer, intent(in), optional :: declaration_indices(:)
        integer, intent(in), optional :: procedure_indices(:)
        logical, intent(in), optional :: has_contains
        integer, intent(in), optional :: line, column
        type(requirement_block_node) :: node

        node%name = name
        if (present(parameter_names)) then
            if (size(parameter_names) > 0) node%parameter_names = parameter_names
        end if
        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) node%declaration_indices = &
                declaration_indices
        end if
        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) node%procedure_indices = procedure_indices
        end if
        if (present(has_contains)) node%has_contains = has_contains
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_requirement_block

    function create_implements_block(name, parameter_names, declaration_indices, &
                                     procedure_indices, has_contains, line, column) &
        result(node)
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: parameter_names(:)
        integer, intent(in), optional :: declaration_indices(:)
        integer, intent(in), optional :: procedure_indices(:)
        logical, intent(in), optional :: has_contains
        integer, intent(in), optional :: line, column
        type(implements_block_node) :: node

        node%name = name
        if (present(parameter_names)) then
            if (size(parameter_names) > 0) node%parameter_names = parameter_names
        end if
        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) node%declaration_indices = &
                declaration_indices
        end if
        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) node%procedure_indices = procedure_indices
        end if
        if (present(has_contains)) node%has_contains = has_contains
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_implements_block

    subroutine template_block_accept(this, visitor)
        class(template_block_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine template_block_accept

    subroutine instantiate_statement_accept(this, visitor)
        class(instantiate_statement_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine instantiate_statement_accept

    subroutine trait_block_accept(this, visitor)
        class(trait_block_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine trait_block_accept

    subroutine requirement_block_accept(this, visitor)
        class(requirement_block_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine requirement_block_accept

    subroutine implements_block_accept(this, visitor)
        class(implements_block_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine implements_block_accept

    subroutine template_block_assign(lhs, rhs)
        class(template_block_node), intent(inout) :: lhs
        class(template_block_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        lhs%has_contains = rhs%has_contains

        if (allocated(rhs%name)) lhs%name = rhs%name
        if (allocated(rhs%parameter_names)) lhs%parameter_names = rhs%parameter_names
        if (allocated(rhs%declaration_indices)) lhs%declaration_indices = &
            rhs%declaration_indices
        if (allocated(rhs%procedure_indices)) lhs%procedure_indices = &
            rhs%procedure_indices
        if (allocated(rhs%stmt_label)) lhs%stmt_label = rhs%stmt_label
    end subroutine template_block_assign

    subroutine instantiate_statement_assign(lhs, rhs)
        class(instantiate_statement_node), intent(inout) :: lhs
        class(instantiate_statement_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type

        if (allocated(rhs%template_name)) lhs%template_name = rhs%template_name
        if (allocated(rhs%spec_text)) lhs%spec_text = rhs%spec_text
        if (allocated(rhs%stmt_label)) lhs%stmt_label = rhs%stmt_label
    end subroutine instantiate_statement_assign

    subroutine trait_block_assign(lhs, rhs)
        class(trait_block_node), intent(inout) :: lhs
        class(trait_block_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        lhs%has_contains = rhs%has_contains

        if (allocated(rhs%name)) lhs%name = rhs%name
        if (allocated(rhs%parameter_names)) lhs%parameter_names = rhs%parameter_names
        if (allocated(rhs%declaration_indices)) lhs%declaration_indices = &
            rhs%declaration_indices
        if (allocated(rhs%procedure_indices)) lhs%procedure_indices = &
            rhs%procedure_indices
        if (allocated(rhs%stmt_label)) lhs%stmt_label = rhs%stmt_label
    end subroutine trait_block_assign

    subroutine requirement_block_assign(lhs, rhs)
        class(requirement_block_node), intent(inout) :: lhs
        class(requirement_block_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        lhs%has_contains = rhs%has_contains

        if (allocated(rhs%name)) lhs%name = rhs%name
        if (allocated(rhs%parameter_names)) lhs%parameter_names = rhs%parameter_names
        if (allocated(rhs%declaration_indices)) lhs%declaration_indices = &
            rhs%declaration_indices
        if (allocated(rhs%procedure_indices)) lhs%procedure_indices = &
            rhs%procedure_indices
        if (allocated(rhs%stmt_label)) lhs%stmt_label = rhs%stmt_label
    end subroutine requirement_block_assign

    subroutine implements_block_assign(lhs, rhs)
        class(implements_block_node), intent(inout) :: lhs
        class(implements_block_node), intent(in) :: rhs

        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        lhs%has_contains = rhs%has_contains

        if (allocated(rhs%name)) lhs%name = rhs%name
        if (allocated(rhs%parameter_names)) lhs%parameter_names = rhs%parameter_names
        if (allocated(rhs%declaration_indices)) lhs%declaration_indices = &
            rhs%declaration_indices
        if (allocated(rhs%procedure_indices)) lhs%procedure_indices = &
            rhs%procedure_indices
        if (allocated(rhs%stmt_label)) lhs%stmt_label = rhs%stmt_label
    end subroutine implements_block_assign

end module ast_nodes_generics
