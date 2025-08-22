module ast_nodes_core
    use json_module
    use ast_base, only: ast_node, visit_interface, to_json_interface
    implicit none
    private

    ! Public factory functions
    public :: create_pointer_assignment, create_array_literal, create_component_access
    public :: create_range_subscript

    ! Core AST node types used by all Fortran dialects

    ! Program node
    type, extends(ast_node), public :: program_node
        character(len=:), allocatable :: name
        integer, allocatable :: body_indices(:)  ! Indices to body nodes in stack
    contains
        procedure :: accept => program_accept
        procedure :: to_json => program_to_json
        procedure :: assign => program_assign
        generic :: assignment(=) => assign
    end type program_node

    ! Assignment node
    type, extends(ast_node), public :: assignment_node
        integer :: target_index      ! Index to target node in stack
        integer :: value_index       ! Index to value node in stack
        character(len=:), allocatable :: operator
        ! Type inference support (dialect-agnostic)
        logical :: type_was_inferred = .false.  ! true if type was inferred
        character(len=:), allocatable :: inferred_type_name
    contains
        procedure :: accept => assignment_accept
        procedure :: to_json => assignment_to_json
        procedure :: assign => assignment_assign
        generic :: assignment(=) => assign
    end type assignment_node

    ! Pointer assignment node (ptr => target)
    type, extends(ast_node), public :: pointer_assignment_node
        integer :: pointer_index     ! Index to pointer node in stack
        integer :: target_index      ! Index to target node in stack
    contains
        procedure :: accept => pointer_assignment_accept
        procedure :: to_json => pointer_assignment_to_json
        procedure :: assign => pointer_assignment_assign
        generic :: assignment(=) => assign
    end type pointer_assignment_node

    ! Identifier node
    type, extends(ast_node), public :: identifier_node
        character(len=:), allocatable :: name
    contains
        procedure :: accept => identifier_accept
        procedure :: to_json => identifier_to_json
        procedure :: assign => identifier_assign
        generic :: assignment(=) => assign
    end type identifier_node

    ! Literal node
    type, extends(ast_node), public :: literal_node
        character(len=:), allocatable :: value
        character(len=:), allocatable :: literal_type  ! "integer", "real",
        ! "character", etc.
        integer :: literal_kind = 0  ! INTEGER_LITERAL, REAL_LITERAL, etc.
    contains
        procedure :: accept => literal_accept
        procedure :: to_json => literal_to_json
        procedure :: assign => literal_assign
        generic :: assignment(=) => assign
    end type literal_node

    ! Binary operation node
    type, extends(ast_node), public :: binary_op_node
        integer :: left_index        ! Index to left operand in stack
        integer :: right_index       ! Index to right operand in stack
        character(len=:), allocatable :: operator
    contains
        procedure :: accept => binary_op_accept
        procedure :: to_json => binary_op_to_json
        procedure :: assign => binary_op_assign
        generic :: assignment(=) => assign
    end type binary_op_node

    ! Call or subscript node (represents both function calls and array indexing)
    type, extends(ast_node), public :: call_or_subscript_node
        character(len=:), allocatable :: name
        integer, allocatable :: arg_indices(:)
        ! Intrinsic function identification
        logical :: is_intrinsic = .false.
        character(len=:), allocatable :: intrinsic_signature
        ! Disambiguation flag (set during semantic analysis)
        logical :: is_array_access = .false.  ! true if array indexing,
        ! false if function call
    contains
        procedure :: accept => call_or_subscript_accept
        procedure :: to_json => call_or_subscript_to_json
        procedure :: assign => call_or_subscript_assign
        generic :: assignment(=) => assign
    end type call_or_subscript_node

    ! Array literal node
    type, extends(ast_node), public :: array_literal_node
        integer, allocatable :: element_indices(:)  ! Indices to array elements
        character(len=:), allocatable :: element_type  ! Type of array elements
        character(len=:), allocatable :: syntax_style  ! "modern" for [...] or "legacy" for (/ ... /)
    contains
        procedure :: accept => array_literal_accept
        procedure :: to_json => array_literal_to_json
        procedure :: assign => array_literal_assign
        generic :: assignment(=) => assign
    end type array_literal_node

    ! Component access node for % operator
    type, extends(ast_node), public :: component_access_node
        integer :: base_expr_index      ! The structure/derived type expression
        character(len=:), allocatable :: component_name  ! Name of the component
        ! For chained access (a%b%c), base_expr can be another component_access_node
    contains
        procedure :: accept => component_access_accept
        procedure :: to_json => component_access_to_json
        procedure :: assign => component_access_assign
        generic :: assignment(=) => assign
    end type component_access_node

    ! Range subscript node - represents name(start:end) which could be:
    ! - Array slice (for arrays)
    ! - Character substring (for character variables)
    ! This ambiguity is resolved during semantic analysis
    type, extends(ast_node), public :: range_subscript_node
        integer :: base_expr_index      ! The expression being subscripted
        integer :: start_index = -1     ! Start position expression
        ! (-1 if not specified)
        integer :: end_index = -1       ! End position expression (-1 if not specified)
        ! Resolution flag (set during semantic analysis)
        logical :: is_character_substring = .false.  ! true if substring
    contains
        procedure :: accept => range_subscript_accept
        procedure :: to_json => range_subscript_to_json
        procedure :: assign => range_subscript_assign
        generic :: assignment(=) => assign
    end type range_subscript_node

contains

    ! Stub implementations for program_node
    subroutine program_accept(this, visitor)
        class(program_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine program_accept

    subroutine program_to_json(this, json, parent)
        class(program_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: body_array, body_item
        integer :: i
        
        ! Add type field
        call json%add(parent, 'type', 'program')
        
        ! Add stack_index if available (we don't have it here, so add a placeholder)
        call json%add(parent, 'stack_index', 1)
        
        ! Add program name
        if (allocated(this%name)) then
            call json%add(parent, 'name', this%name)
        else
            call json%add(parent, 'name', '')
        end if
        
        ! Add body array
        call json%create_array(body_array, 'body')
        if (allocated(this%body_indices)) then
            do i = 1, size(this%body_indices)
                call json%create_object(body_item, '')
                call json%add(body_item, 'index', this%body_indices(i))
                call json%add(body_array, body_item)
            end do
        end if
        call json%add(parent, body_array)
    end subroutine program_to_json

    subroutine program_assign(lhs, rhs)
        class(program_node), intent(inout) :: lhs
        class(program_node), intent(in) :: rhs
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        ! TEMPORARY: Skip inferred_type copying to prevent memory corruption
        ! TODO: Implement proper cycle-safe deep copy for mono_type_t
        ! (Disabled as documented in CLAUDE.md until proper fix)
        ! Copy derived class fields
        if (allocated(rhs%name)) then
            lhs%name = rhs%name
        end if
        if (allocated(rhs%body_indices)) then
            lhs%body_indices = rhs%body_indices
        end if
    end subroutine program_assign

    ! Stub implementations for assignment_node
    subroutine assignment_accept(this, visitor)
        class(assignment_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine assignment_accept

    subroutine assignment_to_json(this, json, parent)
        class(assignment_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine assignment_to_json

    subroutine assignment_assign(lhs, rhs)
        class(assignment_node), intent(inout) :: lhs
        class(assignment_node), intent(in) :: rhs
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        ! TEMPORARY: Skip inferred_type copying to prevent memory corruption
        ! TODO: Implement proper cycle-safe deep copy for mono_type_t
        ! (Disabled as documented in CLAUDE.md until proper fix)
        ! Copy derived class fields
        lhs%target_index = rhs%target_index
        lhs%value_index = rhs%value_index
        if (allocated(rhs%operator)) then
            lhs%operator = rhs%operator
        end if
        lhs%type_was_inferred = rhs%type_was_inferred
        if (allocated(rhs%inferred_type_name)) then
            lhs%inferred_type_name = rhs%inferred_type_name
        end if
    end subroutine assignment_assign

    ! Stub implementations for pointer_assignment_node
    subroutine pointer_assignment_accept(this, visitor)
        class(pointer_assignment_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine pointer_assignment_accept

    subroutine pointer_assignment_to_json(this, json, parent)
        class(pointer_assignment_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine pointer_assignment_to_json

    subroutine pointer_assignment_assign(lhs, rhs)
        class(pointer_assignment_node), intent(inout) :: lhs
        class(pointer_assignment_node), intent(in) :: rhs
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        ! TEMPORARY: Skip inferred_type copying to prevent memory corruption
        ! TODO: Implement proper cycle-safe deep copy for mono_type_t
        ! (Disabled as documented in CLAUDE.md until proper fix)
        ! Copy derived class fields
        lhs%pointer_index = rhs%pointer_index
        lhs%target_index = rhs%target_index
    end subroutine pointer_assignment_assign

    ! Stub implementations for identifier_node
    subroutine identifier_accept(this, visitor)
        class(identifier_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine identifier_accept

    subroutine identifier_to_json(this, json, parent)
        class(identifier_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine identifier_to_json

    subroutine identifier_assign(lhs, rhs)
        class(identifier_node), intent(inout) :: lhs
        class(identifier_node), intent(in) :: rhs
        
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        ! TEMPORARY: Skip inferred_type copying to prevent memory corruption
        ! TODO: Implement proper cycle-safe deep copy for mono_type_t
        ! (Disabled as documented in CLAUDE.md until proper fix)
        
        ! Copy derived class fields
        if (allocated(rhs%name)) then
            lhs%name = rhs%name
        end if
    end subroutine identifier_assign

    ! Stub implementations for literal_node
    subroutine literal_accept(this, visitor)
        class(literal_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine literal_accept

    subroutine literal_to_json(this, json, parent)
        class(literal_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine literal_to_json

    subroutine literal_assign(lhs, rhs)
        class(literal_node), intent(inout) :: lhs
        class(literal_node), intent(in) :: rhs
        
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        ! TEMPORARY: Skip inferred_type copying to prevent memory corruption
        ! TODO: Implement proper cycle-safe deep copy for mono_type_t
        ! (Disabled as documented in CLAUDE.md until proper fix)
        
        ! Copy derived class fields
        if (allocated(rhs%value)) then
            lhs%value = rhs%value
        end if
        if (allocated(rhs%literal_type)) then
            lhs%literal_type = rhs%literal_type
        end if
        lhs%literal_kind = rhs%literal_kind
    end subroutine literal_assign

    ! Stub implementations for binary_op_node
    subroutine binary_op_accept(this, visitor)
        class(binary_op_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine binary_op_accept

    subroutine binary_op_to_json(this, json, parent)
        class(binary_op_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine binary_op_to_json

    subroutine binary_op_assign(lhs, rhs)
        class(binary_op_node), intent(inout) :: lhs
        class(binary_op_node), intent(in) :: rhs
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        ! TEMPORARY: Skip inferred_type copying to prevent memory corruption
        ! TODO: Implement proper cycle-safe deep copy for mono_type_t
        ! (Disabled as documented in CLAUDE.md until proper fix)
        ! Copy derived class fields
        lhs%left_index = rhs%left_index
        lhs%right_index = rhs%right_index
        if (allocated(rhs%operator)) lhs%operator = rhs%operator
    end subroutine binary_op_assign

    ! Stub implementations for call_or_subscript_node
    subroutine call_or_subscript_accept(this, visitor)
        class(call_or_subscript_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine call_or_subscript_accept

    subroutine call_or_subscript_to_json(this, json, parent)
        class(call_or_subscript_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: args_array, arg_item
        integer :: i
        
        ! Add type field
        call json%add(parent, 'type', 'call_or_subscript')
        
        ! Add function/subscript name
        if (allocated(this%name)) then
            call json%add(parent, 'name', this%name)
        else
            call json%add(parent, 'name', '')
        end if
        
        ! Add intrinsic information
        call json%add(parent, 'is_intrinsic', this%is_intrinsic)
        if (allocated(this%intrinsic_signature) .and. &
            len_trim(this%intrinsic_signature) > 0) then
            call json%add(parent, 'intrinsic_signature', this%intrinsic_signature)
        end if
        
        ! Add disambiguation information
        call json%add(parent, 'is_array_access', this%is_array_access)
        
        ! Add arguments array
        call json%create_array(args_array, 'arguments')
        if (allocated(this%arg_indices)) then
            do i = 1, size(this%arg_indices)
                call json%create_object(arg_item, '')
                call json%add(arg_item, 'index', this%arg_indices(i))
                call json%add(args_array, arg_item)
            end do
        end if
        call json%add(parent, args_array)
    end subroutine call_or_subscript_to_json

    subroutine call_or_subscript_assign(lhs, rhs)
        class(call_or_subscript_node), intent(inout) :: lhs
        class(call_or_subscript_node), intent(in) :: rhs
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        ! TEMPORARY: Skip inferred_type copying to prevent memory corruption
        ! TODO: Implement proper cycle-safe deep copy for mono_type_t
        ! (Disabled as documented in CLAUDE.md until proper fix)
        ! Copy derived class fields
        if (allocated(rhs%name)) lhs%name = rhs%name
        if (allocated(rhs%arg_indices)) lhs%arg_indices = rhs%arg_indices
        lhs%is_intrinsic = rhs%is_intrinsic
        lhs%is_array_access = rhs%is_array_access
        if (allocated(rhs%intrinsic_signature)) then
            lhs%intrinsic_signature = rhs%intrinsic_signature
        end if
    end subroutine call_or_subscript_assign

    ! Stub implementations for array_literal_node
    subroutine array_literal_accept(this, visitor)
        class(array_literal_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine array_literal_accept

    subroutine array_literal_to_json(this, json, parent)
        class(array_literal_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine array_literal_to_json

    subroutine array_literal_assign(lhs, rhs)
        class(array_literal_node), intent(inout) :: lhs
        class(array_literal_node), intent(in) :: rhs
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        ! TEMPORARY: Skip inferred_type copying to prevent memory corruption
        ! TODO: Implement proper cycle-safe deep copy for mono_type_t
        ! (Disabled as documented in CLAUDE.md until proper fix)
        ! Copy derived class fields
        if (allocated(rhs%element_indices)) lhs%element_indices = rhs%element_indices
        if (allocated(rhs%element_type)) lhs%element_type = rhs%element_type
        if (allocated(rhs%syntax_style)) lhs%syntax_style = rhs%syntax_style
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
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_pointer_assignment

    function create_array_literal(element_indices, line, column, syntax_style) result(node)
        integer, intent(in) :: element_indices(:)
        integer, intent(in), optional :: line, column
        character(len=*), intent(in), optional :: syntax_style
        type(array_literal_node) :: node
        node%element_indices = element_indices
        if (present(line)) node%line = line
        if (present(column)) node%column = column
        if (present(syntax_style)) then
            node%syntax_style = syntax_style
        else
            node%syntax_style = "modern"  ! default to modern syntax
        end if
    end function create_array_literal

    ! Stub implementations for component_access_node
    subroutine component_access_accept(this, visitor)
        class(component_access_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine component_access_accept

    subroutine component_access_to_json(this, json, parent)
        class(component_access_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: base_expr
        
        ! Add type field
        call json%add(parent, 'type', 'component_access')
        
        ! Add base expression index
        call json%add(parent, 'base_expr_index', this%base_expr_index)
        
        ! Add component name
        if (allocated(this%component_name)) then
            call json%add(parent, 'component_name', this%component_name)
        else
            call json%add(parent, 'component_name', '')
        end if
    end subroutine component_access_to_json

    subroutine component_access_assign(lhs, rhs)
        class(component_access_node), intent(inout) :: lhs
        class(component_access_node), intent(in) :: rhs
        
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        ! TEMPORARY: Skip inferred_type copying to prevent memory corruption
        ! TODO: Implement proper cycle-safe deep copy for mono_type_t
        ! (Disabled as documented in CLAUDE.md until proper fix)
        
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
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_component_access

    ! Stub implementations for range_subscript_node
    subroutine range_subscript_accept(this, visitor)
        class(range_subscript_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine range_subscript_accept

    subroutine range_subscript_to_json(this, json, parent)
        class(range_subscript_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        
        ! Add type field
        call json%add(parent, 'type', 'range_subscript')
        
        ! Add base expression index
        call json%add(parent, 'base_expr_index', this%base_expr_index)
        
        ! Add start index
        call json%add(parent, 'start_index', this%start_index)
        
        ! Add end index
        call json%add(parent, 'end_index', this%end_index)
        
        ! Add resolution flag
        call json%add(parent, 'is_character_substring', this%is_character_substring)
    end subroutine range_subscript_to_json

    subroutine range_subscript_assign(lhs, rhs)
        class(range_subscript_node), intent(inout) :: lhs
        class(range_subscript_node), intent(in) :: rhs
        
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        ! TEMPORARY: Skip inferred_type copying to prevent memory corruption
        ! TODO: Implement proper cycle-safe deep copy for mono_type_t
        ! (Disabled as documented in CLAUDE.md until proper fix)
        
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
        node%is_character_substring = .false.  ! Default to array slice
    end function create_range_subscript

end module ast_nodes_core