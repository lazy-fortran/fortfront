module ast_nodes_data
    use json_module
    use ast_base, only: ast_node, visit_interface, to_json_interface
    implicit none
    private

    ! Public constants for parameter intent
    public :: INTENT_NONE, INTENT_IN, INTENT_OUT, INTENT_INOUT
    integer, parameter :: INTENT_NONE = 0
    integer, parameter :: INTENT_IN = 1
    integer, parameter :: INTENT_OUT = 2
    integer, parameter :: INTENT_INOUT = 3

    ! Public factory functions
    public :: create_declaration, create_derived_type
    
    ! Public utility functions
    public :: intent_type_to_string

    ! Data structure AST nodes

    ! Declaration node
    type, extends(ast_node), public :: declaration_node
        character(len=:), allocatable :: type_name     ! real, integer, etc.
        character(len=:), allocatable :: var_name      ! Variable name
        ! (for single declarations)
        character(len=:), allocatable :: var_names(:)  ! Variable names
        ! (for multi declarations)
        logical :: is_multi_declaration = .false.      ! Whether this
        ! declares multiple variables
        integer :: kind_value                          ! Kind parameter
        ! (e.g., 8 for real(8))
        logical :: has_kind                            ! Whether kind was specified
        character(len=:), allocatable :: intent        ! in, out, inout (for parameters)
        logical :: has_intent = .false.                ! Whether intent was specified
        logical :: is_optional = .false.               ! Whether optional attribute is present
        integer :: initializer_index = 0              ! Initializer index (stack-based)
        logical :: has_initializer = .false.          ! Whether initializer is present
        ! Array dimension support
        logical :: is_array = .false.                  ! Whether this is
        ! an array declaration
        integer, allocatable :: dimension_indices(:)  ! Dimension indices (stack-based)
        logical :: is_allocatable = .false.           ! Whether allocatable
        ! attribute is present
        logical :: is_pointer = .false.                ! Whether pointer
        ! attribute is present
        logical :: is_target = .false.                 ! Whether target
        ! attribute is present
    contains
        procedure :: accept => declaration_accept
        procedure :: to_json => declaration_to_json
        procedure :: assign => declaration_assign
        generic :: assignment(=) => assign
    end type declaration_node

    ! Parameter declaration node (for function/subroutine parameters)
    type, extends(ast_node), public :: parameter_declaration_node
        character(len=:), allocatable :: name          ! Parameter name
        character(len=:), allocatable :: type_name     ! real, integer, etc.
        integer :: kind_value                          ! Kind parameter
        ! (e.g., 8 for real(8))
        logical :: has_kind                            ! Whether kind was specified
        integer :: intent_type = INTENT_NONE          ! INTENT_IN/OUT/INOUT
        logical :: is_optional = .false.              ! Whether parameter is optional
        ! Array dimension support
        logical :: is_array = .false.                  ! Whether this is
        ! an array parameter
        integer, allocatable :: dimension_indices(:)   ! Dimension indices (stack-based)
    contains
        procedure :: accept => parameter_declaration_accept
        procedure :: to_json => parameter_declaration_to_json
        procedure :: assign => parameter_declaration_assign
        generic :: assignment(=) => assign
    end type parameter_declaration_node

    ! Module node
    type, extends(ast_node), public :: module_node
        character(len=:), allocatable :: name         ! Module name
        integer, allocatable :: declaration_indices(:) ! Module declaration
        ! arena indices
        integer, allocatable :: procedure_indices(:)   ! Module procedure
        ! arena indices (after contains)
        logical :: has_contains = .false.             ! Whether module has
        ! a contains section
    contains
        procedure :: accept => module_accept
        procedure :: to_json => module_to_json
        procedure :: assign => module_assign
        generic :: assignment(=) => assign
    end type module_node

    ! Derived type node
    type, extends(ast_node), public :: derived_type_node
        character(len=:), allocatable :: name          ! Type name
        integer, allocatable :: component_indices(:)   ! Component indices (stack-based)
        logical :: has_parameters = .false.            ! Whether it has parameters
        integer, allocatable :: param_indices(:)       ! Parameter indices (stack-based)
    contains
        procedure :: accept => derived_type_accept
        procedure :: to_json => derived_type_to_json
        procedure :: assign => derived_type_assign
        generic :: assignment(=) => assign
    end type derived_type_node

contains

    ! Stub implementations for declaration_node
    subroutine declaration_accept(this, visitor)
        class(declaration_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine declaration_accept

    subroutine declaration_to_json(this, json, parent)
        class(declaration_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine declaration_to_json

    subroutine declaration_assign(lhs, rhs)
        class(declaration_node), intent(inout) :: lhs
        class(declaration_node), intent(in) :: rhs
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            if (allocated(lhs%inferred_type)) deallocate(lhs%inferred_type)
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        else
            if (allocated(lhs%inferred_type)) deallocate(lhs%inferred_type)
        end if
        ! Copy derived class fields
        if (allocated(rhs%type_name)) lhs%type_name = rhs%type_name
        if (allocated(rhs%var_name)) lhs%var_name = rhs%var_name
        if (allocated(rhs%var_names)) lhs%var_names = rhs%var_names
        lhs%is_multi_declaration = rhs%is_multi_declaration
        lhs%kind_value = rhs%kind_value
        lhs%has_kind = rhs%has_kind
        if (allocated(rhs%intent)) lhs%intent = rhs%intent
        lhs%has_intent = rhs%has_intent
        lhs%initializer_index = rhs%initializer_index
        lhs%has_initializer = rhs%has_initializer
        lhs%is_array = rhs%is_array
        lhs%is_allocatable = rhs%is_allocatable
        lhs%is_pointer = rhs%is_pointer
        lhs%is_target = rhs%is_target
        if (allocated(rhs%dimension_indices)) then
            if (allocated(lhs%dimension_indices)) deallocate(lhs%dimension_indices)
            allocate(lhs%dimension_indices(size(rhs%dimension_indices)))
            lhs%dimension_indices = rhs%dimension_indices
        end if
    end subroutine declaration_assign

    ! Stub implementations for parameter_declaration_node
    subroutine parameter_declaration_accept(this, visitor)
        class(parameter_declaration_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine parameter_declaration_accept

    subroutine parameter_declaration_to_json(this, json, parent)
        class(parameter_declaration_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: node, intent_str
        character(len=:), allocatable :: intent_name
        
        call json%create_object(node, '')
        call json%add(parent, node)
        
        call json%add(node, 'type', 'parameter_declaration')
        call json%add(node, 'name', this%name)
        call json%add(node, 'type_name', this%type_name)
        call json%add(node, 'kind_value', this%kind_value)
        call json%add(node, 'has_kind', this%has_kind)
        call json%add(node, 'is_optional', this%is_optional)
        call json%add(node, 'is_array', this%is_array)
        
        ! Add intent as a readable string (single field for clarity)
        select case (this%intent_type)
        case (INTENT_NONE)
            intent_name = ''
        case (INTENT_IN)
            intent_name = 'in'
        case (INTENT_OUT)
            intent_name = 'out'
        case (INTENT_INOUT)
            intent_name = 'inout'
        case default
            intent_name = 'unknown'
        end select
        call json%add(node, 'intent', intent_name)
        
        call json%add(node, 'line', this%line)
        call json%add(node, 'column', this%column)
    end subroutine parameter_declaration_to_json

    subroutine parameter_declaration_assign(lhs, rhs)
        class(parameter_declaration_node), intent(inout) :: lhs
        class(parameter_declaration_node), intent(in) :: rhs
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            if (allocated(lhs%inferred_type)) deallocate(lhs%inferred_type)
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        else
            if (allocated(lhs%inferred_type)) deallocate(lhs%inferred_type)
        end if
        ! Copy derived class fields
        if (allocated(rhs%name)) lhs%name = rhs%name
        if (allocated(rhs%type_name)) lhs%type_name = rhs%type_name
        lhs%kind_value = rhs%kind_value
        lhs%has_kind = rhs%has_kind
        lhs%intent_type = rhs%intent_type
        lhs%is_optional = rhs%is_optional
        lhs%is_array = rhs%is_array
        if (allocated(rhs%dimension_indices)) then
            if (allocated(lhs%dimension_indices)) deallocate(lhs%dimension_indices)
            allocate(lhs%dimension_indices(size(rhs%dimension_indices)))
            lhs%dimension_indices = rhs%dimension_indices
        end if
    end subroutine parameter_declaration_assign

    ! Stub implementations for module_node
    subroutine module_accept(this, visitor)
        class(module_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine module_accept

    subroutine module_to_json(this, json, parent)
        class(module_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine module_to_json

    subroutine module_assign(lhs, rhs)
        class(module_node), intent(inout) :: lhs
        class(module_node), intent(in) :: rhs
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            if (allocated(lhs%inferred_type)) deallocate(lhs%inferred_type)
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        else
            if (allocated(lhs%inferred_type)) deallocate(lhs%inferred_type)
        end if
        ! Copy derived class fields
        if (allocated(rhs%name)) lhs%name = rhs%name
        if (allocated(rhs%declaration_indices)) then
            if (allocated(lhs%declaration_indices)) deallocate(lhs%declaration_indices)
            allocate(lhs%declaration_indices(size(rhs%declaration_indices)))
            lhs%declaration_indices = rhs%declaration_indices
        end if
        if (allocated(rhs%procedure_indices)) then
            if (allocated(lhs%procedure_indices)) deallocate(lhs%procedure_indices)
            allocate(lhs%procedure_indices(size(rhs%procedure_indices)))
            lhs%procedure_indices = rhs%procedure_indices
        end if
        lhs%has_contains = rhs%has_contains
    end subroutine module_assign

    ! Stub implementations for derived_type_node
    subroutine derived_type_accept(this, visitor)
        class(derived_type_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation
    end subroutine derived_type_accept

    subroutine derived_type_to_json(this, json, parent)
        class(derived_type_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        ! Stub implementation
    end subroutine derived_type_to_json

    subroutine derived_type_assign(lhs, rhs)
        class(derived_type_node), intent(inout) :: lhs
        class(derived_type_node), intent(in) :: rhs
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            if (allocated(lhs%inferred_type)) deallocate(lhs%inferred_type)
            allocate(lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type
        else
            if (allocated(lhs%inferred_type)) deallocate(lhs%inferred_type)
        end if
        ! Copy derived class fields
        if (allocated(rhs%name)) lhs%name = rhs%name
        if (allocated(rhs%component_indices)) then
            if (allocated(lhs%component_indices)) deallocate(lhs%component_indices)
            allocate(lhs%component_indices(size(rhs%component_indices)))
            lhs%component_indices = rhs%component_indices
        end if
        lhs%has_parameters = rhs%has_parameters
        if (allocated(rhs%param_indices)) then
            if (allocated(lhs%param_indices)) deallocate(lhs%param_indices)
            allocate(lhs%param_indices(size(rhs%param_indices)))
            lhs%param_indices = rhs%param_indices
        end if
    end subroutine derived_type_assign

    ! Factory functions
    function create_declaration(type_name, var_name, kind_value, &
                               initializer_index, dimension_indices, &
                               is_allocatable, is_pointer, is_target, &
                               line, column) result(node)
        character(len=*), intent(in) :: type_name
        character(len=*), intent(in) :: var_name
        integer, intent(in), optional :: kind_value
        integer, intent(in), optional :: initializer_index
        integer, intent(in), optional :: dimension_indices(:)
        logical, intent(in), optional :: is_allocatable
        logical, intent(in), optional :: is_pointer
        logical, intent(in), optional :: is_target
        integer, intent(in), optional :: line, column
        type(declaration_node) :: node

        node%type_name = type_name
        node%var_name = var_name

        if (present(kind_value)) then
            node%kind_value = kind_value
            node%has_kind = .true.
        else
            node%kind_value = 0
            node%has_kind = .false.
        end if

        if (present(initializer_index)) then
            node%initializer_index = initializer_index
            node%has_initializer = .true.
        else
            node%initializer_index = 0
            node%has_initializer = .false.
        end if

        if (present(dimension_indices)) then
            if (size(dimension_indices) > 0) then
                node%dimension_indices = dimension_indices
                node%is_array = .true.
            end if
        end if

        if (present(is_allocatable)) node%is_allocatable = is_allocatable
        if (present(is_pointer)) node%is_pointer = is_pointer
        if (present(is_target)) node%is_target = is_target
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_declaration

    function create_derived_type(name, component_indices, param_indices, &
                                line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: component_indices(:)
        integer, intent(in), optional :: param_indices(:)
        integer, intent(in), optional :: line, column
        type(derived_type_node) :: node

        node%name = name

        if (present(component_indices)) then
            if (size(component_indices) > 0) then
                node%component_indices = component_indices
            end if
        end if

        if (present(param_indices)) then
            if (size(param_indices) > 0) then
                node%param_indices = param_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_derived_type

    ! Utility function to convert intent_type to string
    function intent_type_to_string(intent_type) result(intent_str)
        integer, intent(in) :: intent_type
        character(len=:), allocatable :: intent_str
        
        select case (intent_type)
        case (INTENT_NONE)
            intent_str = ""
        case (INTENT_IN)
            intent_str = "in"
        case (INTENT_OUT)
            intent_str = "out"
        case (INTENT_INOUT)
            intent_str = "inout"
        case default
            intent_str = ""
        end select
    end function intent_type_to_string

end module ast_nodes_data