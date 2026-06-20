module ast_nodes_core
    use ast_base, only: ast_node, visit_interface, &
                        ast_visitor_base_t, copy_ast_node_base
    use uid_generator, only: generate_uid
    use ast_nodes_procedure, only: subroutine_call_node
    implicit none
    private

    ! Public factory functions
    public :: create_pointer_assignment, create_array_literal, create_component_access
    public :: create_range_subscript
    ! Constructors migrated from ast_core
    public :: create_identifier, create_literal, create_binary_op
    public :: create_call_or_subscript, create_assignment, create_program
    public :: create_subroutine_call

    ! Core AST node types used by all Fortran dialects

    ! Program node
    type, extends(ast_node), public :: program_node
        character(len=:), allocatable :: name
        integer, allocatable :: body_indices(:) ! Indices to body nodes in stack
    contains
        procedure :: accept => program_accept
        procedure :: assign => program_assign
        generic :: assignment(=) => assign
    end type program_node

    ! Assignment node
    type, extends(ast_node), public :: assignment_node
        integer :: target_index ! Index to target node in stack
        integer :: value_index ! Index to value node in stack
        character(len=:), allocatable :: operator
        ! Type inference support (dialect-agnostic)
        logical :: type_was_inferred = .false. ! true if type was inferred
        character(len=:), allocatable :: inferred_type_name
        logical :: suppress_codegen = .false.
        logical :: is_keyword_argument = .false. ! keyword argument in a call
        ! Walrus declaration-with-inference (name := expr, LFortran extension):
        ! declares the target and forbids same-scope redeclaration.
        logical :: is_walrus = .false.
    contains
        procedure :: accept => assignment_accept
        procedure :: assign => assignment_assign
        generic :: assignment(=) => assign
    end type assignment_node

    ! Pointer assignment node (ptr => target)
    type, extends(ast_node), public :: pointer_assignment_node
        integer :: pointer_index ! Index to pointer node in stack
        integer :: target_index ! Index to target node in stack
    contains
        procedure :: accept => pointer_assignment_accept
        procedure :: assign => pointer_assignment_assign
        generic :: assignment(=) => assign
    end type pointer_assignment_node

    ! Identifier node
    type, extends(ast_node), public :: identifier_node
        character(len=:), allocatable :: name
    contains
        procedure :: accept => identifier_accept
        procedure :: assign => identifier_assign
        generic :: assignment(=) => assign
    end type identifier_node

    ! Literal node
    type, extends(ast_node), public :: literal_node
        character(len=:), allocatable :: value
        character(len=:), allocatable :: literal_type ! "integer", "real",
        ! "character", etc.
        integer :: literal_kind = 0 ! INTEGER_LITERAL, REAL_LITERAL, etc.
    contains
        procedure :: accept => literal_accept
        procedure :: assign => literal_assign
        generic :: assignment(=) => assign
    end type literal_node

    ! Binary operation node
    type, extends(ast_node), public :: binary_op_node
        integer :: left_index ! Index to left operand in stack
        integer :: right_index ! Index to right operand in stack
        character(len=:), allocatable :: operator
    contains
        procedure :: accept => binary_op_accept
        procedure :: assign => binary_op_assign
        generic :: assignment(=) => assign
    end type binary_op_node

    ! Call or subscript node (represents both function calls and array indexing)
    type, extends(ast_node), public :: call_or_subscript_node
        character(len=:), allocatable :: name
        integer :: base_expr_index = 0
        integer, allocatable :: arg_indices(:)
        ! Intrinsic function identification
        logical :: is_intrinsic = .false.
        character(len=:), allocatable :: intrinsic_signature
        ! Disambiguation flag (set during semantic analysis)
        logical :: is_array_access = .false. ! true if array indexing,
        ! false if function call
    contains
        procedure :: accept => call_or_subscript_accept
        procedure :: assign => call_or_subscript_assign
        generic :: assignment(=) => assign
    end type call_or_subscript_node

    ! Array literal node
    type, extends(ast_node), public :: array_literal_node
        integer, allocatable :: element_indices(:) ! Indices to array elements
        character(len=:), allocatable :: element_type ! Type of array elements
        character(len=:), allocatable :: type_spec ! Optional explicit type-spec
        character(len=:), allocatable :: syntax_style ! modern [...] or legacy (/ /)
    contains
        procedure :: accept => array_literal_accept
        procedure :: assign => array_literal_assign
        generic :: assignment(=) => assign
    end type array_literal_node

    ! Component access node for % operator
    type, extends(ast_node), public :: component_access_node
        integer :: base_expr_index ! The structure/derived type expression
        character(len=:), allocatable :: component_name ! Name of the component
        ! For chained access (a%b%c), base_expr can be another component_access_node
    contains
        procedure :: accept => component_access_accept
        procedure :: assign => component_access_assign
        generic :: assignment(=) => assign
    end type component_access_node

    ! Range subscript node - represents name(start:end) which could be:
    ! - Array slice (for arrays)
    ! - Character substring (for character variables)
    ! This ambiguity is resolved during semantic analysis
    type, extends(ast_node), public :: range_subscript_node
        integer :: base_expr_index ! The expression being subscripted
        integer :: start_index = -1 ! Start position expression
        ! (-1 if not specified)
        integer :: end_index = -1 ! End position expression (-1 if not specified)
        ! Resolution flag (set during semantic analysis)
        logical :: is_character_substring = .false. ! true if substring
    contains
        procedure :: accept => range_subscript_accept
        procedure :: assign => range_subscript_assign
        generic :: assignment(=) => assign
    end type range_subscript_node

contains

    ! Stub implementations for program_node
    subroutine program_accept(this, visitor)
        class(program_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine program_accept

    subroutine program_assign(lhs, rhs)
        class(program_node), intent(inout) :: lhs
        class(program_node), intent(in) :: rhs
        call copy_ast_node_base(lhs, rhs)
        ! Copy derived class fields
        if (allocated(rhs%name)) then
            lhs%name = rhs%name
        else if (allocated(lhs%name)) then
            deallocate (lhs%name)
        end if
        if (allocated(rhs%body_indices)) then
            lhs%body_indices = rhs%body_indices
        else if (allocated(lhs%body_indices)) then
            deallocate (lhs%body_indices)
        end if
    end subroutine program_assign

    ! Stub implementations for assignment_node
    subroutine assignment_accept(this, visitor)
        class(assignment_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine assignment_accept

    subroutine assignment_assign(lhs, rhs)
        class(assignment_node), intent(inout) :: lhs
        class(assignment_node), intent(in) :: rhs
        call copy_ast_node_base(lhs, rhs)
        ! Copy derived class fields
        lhs%target_index = rhs%target_index
        lhs%value_index = rhs%value_index
        if (allocated(rhs%operator)) then
            lhs%operator = rhs%operator
        else if (allocated(lhs%operator)) then
            deallocate (lhs%operator)
        end if
        lhs%type_was_inferred = rhs%type_was_inferred
        if (allocated(rhs%inferred_type_name)) then
            lhs%inferred_type_name = rhs%inferred_type_name
        else if (allocated(lhs%inferred_type_name)) then
            deallocate (lhs%inferred_type_name)
        end if
        lhs%suppress_codegen = rhs%suppress_codegen
        lhs%is_keyword_argument = rhs%is_keyword_argument
        lhs%is_walrus = rhs%is_walrus
    end subroutine assignment_assign

    ! Stub implementations for pointer_assignment_node
    subroutine pointer_assignment_accept(this, visitor)
        class(pointer_assignment_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine pointer_assignment_accept

    subroutine pointer_assignment_assign(lhs, rhs)
        class(pointer_assignment_node), intent(inout) :: lhs
        class(pointer_assignment_node), intent(in) :: rhs
        call copy_ast_node_base(lhs, rhs)
        ! Copy derived class fields
        lhs%pointer_index = rhs%pointer_index
        lhs%target_index = rhs%target_index
    end subroutine pointer_assignment_assign

    ! Stub implementations for identifier_node
    subroutine identifier_accept(this, visitor)
        class(identifier_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine identifier_accept

    subroutine identifier_assign(lhs, rhs)
        class(identifier_node), intent(inout) :: lhs
        class(identifier_node), intent(in) :: rhs

        call copy_ast_node_base(lhs, rhs)

        ! Copy derived class fields
        if (allocated(rhs%name)) then
            lhs%name = rhs%name
        else if (allocated(lhs%name)) then
            deallocate (lhs%name)
        end if
    end subroutine identifier_assign

    ! Stub implementations for literal_node
    subroutine literal_accept(this, visitor)
        class(literal_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine literal_accept

    subroutine literal_assign(lhs, rhs)
        class(literal_node), intent(inout) :: lhs
        class(literal_node), intent(in) :: rhs

        call copy_ast_node_base(lhs, rhs)

        ! Copy derived class fields
        if (allocated(rhs%value)) then
            lhs%value = rhs%value
        else if (allocated(lhs%value)) then
            deallocate (lhs%value)
        end if
        if (allocated(rhs%literal_type)) then
            lhs%literal_type = rhs%literal_type
        else if (allocated(lhs%literal_type)) then
            deallocate (lhs%literal_type)
        end if
        lhs%literal_kind = rhs%literal_kind
    end subroutine literal_assign

    ! Stub implementations for binary_op_node
    subroutine binary_op_accept(this, visitor)
        class(binary_op_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine binary_op_accept

    subroutine binary_op_assign(lhs, rhs)
        class(binary_op_node), intent(inout) :: lhs
        class(binary_op_node), intent(in) :: rhs
        call copy_ast_node_base(lhs, rhs)
        ! Copy derived class fields
        lhs%left_index = rhs%left_index
        lhs%right_index = rhs%right_index
        if (allocated(rhs%operator)) then
            lhs%operator = rhs%operator
        else if (allocated(lhs%operator)) then
            deallocate (lhs%operator)
        end if
    end subroutine binary_op_assign

    ! Stub implementations for call_or_subscript_node
    subroutine call_or_subscript_accept(this, visitor)
        class(call_or_subscript_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine call_or_subscript_accept

    subroutine call_or_subscript_assign(lhs, rhs)
        class(call_or_subscript_node), intent(inout) :: lhs
        class(call_or_subscript_node), intent(in) :: rhs
        call copy_ast_node_base(lhs, rhs)
        ! Copy derived class fields
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
        lhs%is_intrinsic = rhs%is_intrinsic
        lhs%is_array_access = rhs%is_array_access
        if (allocated(rhs%intrinsic_signature)) then
            lhs%intrinsic_signature = rhs%intrinsic_signature
        else if (allocated(lhs%intrinsic_signature)) then
            deallocate (lhs%intrinsic_signature)
        end if
    end subroutine call_or_subscript_assign

    ! Stub implementations for array_literal_node
    subroutine array_literal_accept(this, visitor)
        class(array_literal_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine array_literal_accept

    subroutine array_literal_assign(lhs, rhs)
        class(array_literal_node), intent(inout) :: lhs
        class(array_literal_node), intent(in) :: rhs
        call copy_ast_node_base(lhs, rhs)
        ! Copy derived class fields
        if (allocated(rhs%element_indices)) then
            lhs%element_indices = rhs%element_indices
        else if (allocated(lhs%element_indices)) then
            deallocate (lhs%element_indices)
        end if
        if (allocated(rhs%element_type)) then
            lhs%element_type = rhs%element_type
        else if (allocated(lhs%element_type)) then
            deallocate (lhs%element_type)
        end if
        if (allocated(rhs%type_spec)) then
            lhs%type_spec = rhs%type_spec
        else if (allocated(lhs%type_spec)) then
            deallocate (lhs%type_spec)
        end if
        if (allocated(rhs%syntax_style)) then
            lhs%syntax_style = rhs%syntax_style
        else if (allocated(lhs%syntax_style)) then
            deallocate (lhs%syntax_style)
        end if
    end subroutine array_literal_assign

    ! Factory functions
    function create_pointer_assignment(pointer_index, target_index, &
                                       line, column) result(node)
        integer, intent(in) :: pointer_index
        integer, intent(in) :: target_index
        integer, intent(in), optional :: line, column
        type(pointer_assignment_node) :: node

        node%pointer_index = pointer_index
        node%target_index = target_index
        node%uid = generate_uid()
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_pointer_assignment

    function create_array_literal(element_indices, line, column, syntax_style, &
                                  type_spec) result(node)
        integer, intent(in) :: element_indices(:)
        integer, intent(in), optional :: line, column
        character(len=*), intent(in), optional :: syntax_style
        character(len=*), intent(in), optional :: type_spec
        type(array_literal_node) :: node
        node%element_indices = element_indices
        node%uid = generate_uid()
        if (present(line)) node%line = line
        if (present(column)) node%column = column
        if (present(syntax_style)) then
            node%syntax_style = syntax_style
        else
            node%syntax_style = "modern" ! default to modern syntax
        end if
        if (present(type_spec)) then
            if (len_trim(type_spec) > 0) node%type_spec = trim(type_spec)
        end if
    end function create_array_literal

    ! Stub implementations for component_access_node
    subroutine component_access_accept(this, visitor)
        class(component_access_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine component_access_accept

    subroutine component_access_assign(lhs, rhs)
        class(component_access_node), intent(inout) :: lhs
        class(component_access_node), intent(in) :: rhs

        call copy_ast_node_base(lhs, rhs)

        ! Copy derived class fields
        lhs%base_expr_index = rhs%base_expr_index
        if (allocated(rhs%component_name)) then
            lhs%component_name = rhs%component_name
        end if
    end subroutine component_access_assign

    ! Factory function for component access
    function create_component_access(base_expr_index, component_name, &
                                     line, column) result(node)
        integer, intent(in) :: base_expr_index
        character(len=*), intent(in) :: component_name
        integer, intent(in), optional :: line, column
        type(component_access_node) :: node

        node%base_expr_index = base_expr_index
        node%component_name = component_name
        node%uid = generate_uid()
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_component_access

    ! Stub implementations for range_subscript_node
    subroutine range_subscript_accept(this, visitor)
        class(range_subscript_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine range_subscript_accept

    subroutine range_subscript_assign(lhs, rhs)
        class(range_subscript_node), intent(inout) :: lhs
        class(range_subscript_node), intent(in) :: rhs

        call copy_ast_node_base(lhs, rhs)

        ! Copy derived class fields
        lhs%base_expr_index = rhs%base_expr_index
        lhs%start_index = rhs%start_index
        lhs%end_index = rhs%end_index
        lhs%is_character_substring = rhs%is_character_substring
    end subroutine range_subscript_assign

    ! Factory function for range subscript
    function create_range_subscript(base_expr_index, start_index, end_index, &
                                    line, column) result(node)
        integer, intent(in) :: base_expr_index
        integer, intent(in), optional :: start_index, end_index
        integer, intent(in), optional :: line, column
        type(range_subscript_node) :: node

        node%base_expr_index = base_expr_index
        node%uid = generate_uid()
        if (present(start_index)) then
            node%start_index = start_index
        else
            node%start_index = -1
        end if
        if (present(end_index)) then
            node%end_index = end_index
        else
            node%end_index = -1
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
        node%is_character_substring = .false. ! Default to array slice
    end function create_range_subscript

    ! New constructors migrated from ast_core
    function create_identifier(name, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: line, column
        type(identifier_node) :: node

        node%uid = generate_uid()
        node%name = name
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_identifier

    function create_literal(value, kind, line, column) result(node)
        character(len=*), intent(in) :: value
        integer, intent(in) :: kind
        integer, intent(in), optional :: line, column
        type(literal_node) :: node

        node%uid = generate_uid()
        node%value = value
        node%literal_kind = kind
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_literal

    function create_binary_op(left_index, right_index, operator, line, column) &
        result(node)
        integer, intent(in) :: left_index, right_index
        character(len=*), intent(in) :: operator
        integer, intent(in), optional :: line, column
        type(binary_op_node) :: node

        node%uid = generate_uid()
        node%left_index = left_index
        node%right_index = right_index
        node%operator = operator
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_binary_op

    function create_call_or_subscript(name, args, line, column) result(node)
        use intrinsic_registry, only: get_intrinsic_info
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: args(:)
        integer, intent(in), optional :: line, column
        type(call_or_subscript_node) :: node

        node%uid = generate_uid()
        node%name = name
        if (present(args)) then
            if (size(args) > 0) then
                node%arg_indices = args
            end if
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
        call get_intrinsic_info(name, node%is_intrinsic, node%intrinsic_signature)
    end function create_call_or_subscript

    function create_assignment(target_index, value_index, line, column, &
                               inferred_type, inferred_type_name) result(node)
        use type_system_unified, only: mono_type_t
        integer, intent(in) :: target_index, value_index
        integer, intent(in), optional :: line, column
        type(mono_type_t), intent(in), optional :: inferred_type
        character(len=*), intent(in), optional :: inferred_type_name
        type(assignment_node) :: node

        node%target_index = target_index
        node%value_index = value_index
        node%operator = "="
        node%uid = generate_uid()
        if (present(inferred_type)) node%inferred_type = inferred_type
        if (present(inferred_type_name)) then
            node%inferred_type_name = inferred_type_name
            node%type_was_inferred = .true.
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_assignment

    function create_program(name, body_indices, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(program_node) :: node

        node%name = name
        node%uid = generate_uid()
        if (present(body_indices)) then
            if (size(body_indices) > 0) node%body_indices = body_indices
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_program

    function create_subroutine_call(name, args, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: args(:)
        integer, intent(in), optional :: line, column
        type(subroutine_call_node) :: node

        node%name = name
        node%uid = generate_uid()
        if (present(args)) then
            if (size(args) > 0) node%arg_indices = args
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_subroutine_call

end module ast_nodes_core
