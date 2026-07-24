module ast_nodes_data
    use uid_generator, only: generate_uid
    use ast_base, only: ast_node, &
        ast_visitor_base_t
    use string_types, only: string_t
    implicit none
    private

    ! Public constants for parameter intent
    public :: INTENT_NONE, INTENT_IN, INTENT_OUT, INTENT_INOUT
    integer, parameter :: INTENT_NONE = 0
    integer, parameter :: INTENT_IN = 1
    integer, parameter :: INTENT_OUT = 2
    integer, parameter :: INTENT_INOUT = 3

    ! Public factory functions
    public :: create_declaration, create_derived_type, create_mixed_construct_container
    public :: create_multi_unit_container
    public :: create_type_binding

    ! Synthetic name for the multi-unit aggregate in program analysis output
    ! (call graph). The AST carries a dedicated multi_unit_container_node; this
    ! name labels the corresponding aggregate procedure.
    character(len=*), parameter, public :: MULTI_UNIT_NAME = '__MULTI_UNIT__'
    ! Constructors migrated from ast_core
    public :: create_module, create_block_data, create_submodule

    ! Public utility functions
    public :: intent_type_to_string

    ! Data structure AST nodes

    ! Declaration node
    type, extends(ast_node), public :: declaration_node
        character(len=:), allocatable :: type_name ! real, integer, etc.
        character(len=:), allocatable :: var_name ! Variable name
        ! (for single declarations)
        character(len=:), allocatable :: var_names(:) ! Variable names
        ! (for multi declarations)
        logical :: is_multi_declaration = .false. ! Whether this
        ! declares multiple variables
        integer :: kind_value = 0 ! Kind parameter (default none)
        ! (e.g., 8 for real(8))
        logical :: has_kind = .false. ! Whether kind was specified
        character(len=:), allocatable :: character_length_expr
        ! Character length expression
        logical :: has_character_length = .false.
        ! Whether character length was specified
        character(len=:), allocatable :: intent ! in, out, inout (for parameters)
        logical :: has_intent = .false. ! Whether intent was specified
        logical :: is_optional = .false. ! Whether optional attribute is present
        integer :: initializer_index = 0 ! Initializer index (stack-based)
        logical :: has_initializer = .false. ! Whether initializer is present
        ! Array dimension support
        logical :: is_array = .false. ! Whether this is
        ! an array declaration
        integer, allocatable :: dimension_indices(:) ! Dimension indices (stack-based)
        logical :: is_allocatable = .false. ! Whether allocatable
        ! attribute is present
        logical :: is_pointer = .false. ! Whether pointer
        ! attribute is present
        logical :: is_target = .false. ! Whether target
        ! attribute is present
        logical :: is_external = .false. ! Whether external
        ! attribute is present
        logical :: is_unsigned = .false. ! Whether unsigned
        ! attribute is present (nonstandard extension)
        logical :: is_parameter = .false. ! Whether parameter
        ! attribute is present (for constants)
        logical :: is_save = .false. ! Whether save
        ! attribute is present
        logical :: is_volatile = .false. ! Whether volatile
        ! attribute is present
        logical :: is_protected = .false. ! Whether protected
        ! attribute is present
        logical :: is_asynchronous = .false. ! Whether asynchronous
        ! attribute is present
        logical :: is_contiguous = .false. ! Whether contiguous
        ! attribute is present
        logical :: is_value = .false. ! Whether value
        ! attribute is present (F2003 C interop)
        logical :: is_bind_c = .false. ! Whether bind(c)
        ! attribute is present (F2003 C interop)
        logical :: is_inferred = .false.
        logical :: disable_grouping = .false. ! Skip declaration grouping
        character(len=:), allocatable :: accessibility ! 'public'/'private'
        character(len=:), allocatable :: bind_name ! bind(c, name="...") value
    contains
        procedure :: accept => declaration_accept
        procedure :: assign => declaration_assign
        generic :: assignment(=) => assign
    end type declaration_node

    ! Parameter declaration node (for function/subroutine parameters)
    type, extends(ast_node), public :: parameter_declaration_node
        character(len=:), allocatable :: name ! Parameter name
        character(len=:), allocatable :: type_name ! real, integer, etc.
        integer :: kind_value = 0 ! Kind parameter (default none)
        ! (e.g., 8 for real(8))
        logical :: has_kind = .false. ! Whether kind was specified
        character(len=:), allocatable :: character_length_expr
        ! Character length expression
        logical :: has_character_length = .false.
        ! Whether character length was specified
        integer :: intent_type = INTENT_NONE ! INTENT_IN/OUT/INOUT
        logical :: is_optional = .false. ! Whether parameter is optional
        logical :: is_target = .false. ! Whether target attribute is present
        logical :: is_unsigned = .false. ! Whether unsigned attribute is present
        ! Array dimension support
        logical :: is_array = .false. ! Whether this is
        ! an array parameter
        integer, allocatable :: dimension_indices(:) ! Dimension indices (stack-based)
    contains
        procedure :: accept => parameter_declaration_accept
        procedure :: assign => parameter_declaration_assign
        generic :: assignment(=) => assign
    end type parameter_declaration_node

    ! Module node
    type, extends(ast_node), public :: module_node
        character(len=:), allocatable :: name ! Module name
        integer, allocatable :: declaration_indices(:) ! Module declaration
        ! arena indices
        integer, allocatable :: procedure_indices(:) ! Module procedure
        ! arena indices (after contains)
        logical :: has_contains = .false. ! Whether module has
        ! a contains section
    contains
        procedure :: accept => module_accept
        procedure :: assign => module_assign
        generic :: assignment(=) => assign
    end type module_node

    ! Submodule node (Fortran 2008, ISO/IEC 1539-1:2008 Section 11.2)
    type, extends(ast_node), public :: submodule_node
        character(len=:), allocatable :: name ! Submodule name
        character(len=:), allocatable :: parent_identifier ! Parent reference
        ! Format: parent_mod or parent_mod:parent_sub:...
        integer, allocatable :: declaration_indices(:) ! Submodule declaration
        ! arena indices
        integer, allocatable :: procedure_indices(:) ! Submodule procedure
        ! arena indices (after contains)
        logical :: has_contains = .false. ! Whether submodule has
        ! a contains section
    contains
        procedure :: accept => submodule_accept
        procedure :: assign => submodule_assign
        generic :: assignment(=) => assign
    end type submodule_node

    ! BLOCK DATA node
    type, extends(ast_node), public :: block_data_node
        character(len=:), allocatable :: name ! BLOCK DATA name (optional)
        character(len=:), allocatable :: header_label ! Optional statement label
        character(len=:), allocatable :: end_label ! Optional END label
        integer, allocatable :: statement_indices(:) ! Statement indices
    contains
        procedure :: accept => block_data_accept
        procedure :: assign => block_data_assign
        generic :: assignment(=) => assign
    end type block_data_node

    ! Type-bound procedure binding node (F2003)
    type, extends(ast_node), public :: type_binding_node
        character(len=:), allocatable :: binding_name ! Name of binding
        character(len=:), allocatable :: implementation ! Procedure name (single)
        type(string_t), allocatable :: generic_list(:) ! List for generic bindings
        character(len=:), allocatable :: interface_name ! Interface reference
        logical :: is_generic = .false. ! Generic binding
        logical :: is_final = .false. ! FINAL procedure
        logical :: is_deferred = .false. ! DEFERRED (for abstract)
        logical :: pass_arg = .true. ! PASS vs NOPASS
        character(len=:), allocatable :: pass_name ! PASS(name) dummy, if named
        character(len=:), allocatable :: accessibility ! PUBLIC/PRIVATE
    contains
        procedure :: accept => type_binding_accept
        procedure :: assign => type_binding_assign
        generic :: assignment(=) => assign
    end type type_binding_node

    ! Derived type node
    type, extends(ast_node), public :: derived_type_node
        character(len=:), allocatable :: name ! Type name
        character(len=:), allocatable :: extends_parent ! Parent type name (F2003)
        character(len=:), allocatable :: attribute_clause ! Header attributes
        logical :: has_attributes = .false. ! Whether header has attributes
        integer, allocatable :: component_indices(:) ! Component indices (stack-based)
        logical :: has_parameters = .false. ! Whether it has parameters
        integer, allocatable :: param_indices(:) ! Parameter indices (stack-based)
        logical :: has_contains = .false. ! Whether type has CONTAINS section
        integer, allocatable :: binding_indices(:) ! Type-bound procedure indices
    contains
        procedure :: accept => derived_type_accept
        procedure :: assign => derived_type_assign
        generic :: assignment(=) => assign
    end type derived_type_node

    ! Mixed construct container node (for Issue #511 mixed construct support)
    type, extends(ast_node), public :: mixed_construct_container_node
        character(len=:), allocatable :: module_name ! Generated module name
        integer, allocatable :: implicit_declaration_indices(:)
        ! Declarations for module
        integer, allocatable :: explicit_program_indices(:) ! Explicit program units
    contains
        procedure :: accept => mixed_construct_container_accept
        procedure :: assign => mixed_construct_container_assign
        generic :: assignment(=) => assign
    end type mixed_construct_container_node

    ! Multi-unit container node: aggregates several top-level program units
    ! (modules, procedures, an implicit main) parsed from one source. Replaces
    ! the former sentinel program_node named '__MULTI_UNIT__'.
    type, extends(ast_node), public :: multi_unit_container_node
        integer, allocatable :: body_indices(:) ! Contained top-level unit indices
    contains
        procedure :: accept => multi_unit_container_accept
        procedure :: assign => multi_unit_container_assign
        generic :: assignment(=) => assign
    end type multi_unit_container_node

contains

    ! Stub implementations for declaration_node
    subroutine declaration_accept(this, visitor)
        class(declaration_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine declaration_accept

    subroutine declaration_assign(lhs, rhs)
        class(declaration_node), intent(inout) :: lhs
        class(declaration_node), intent(in) :: rhs
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%resolved_type_found = rhs%resolved_type_found
        lhs%resolved_type_kind = rhs%resolved_type_kind
        lhs%resolved_kind_value = rhs%resolved_kind_value
        lhs%resolved_storage_bits = rhs%resolved_storage_bits
        lhs%resolved_rank = rhs%resolved_rank
        lhs%resolved_derived_type_name = rhs%resolved_derived_type_name
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy derived class fields
        if (allocated(rhs%type_name)) then
            lhs%type_name = rhs%type_name
        else if (allocated(lhs%type_name)) then
            deallocate (lhs%type_name)
        end if
        if (allocated(rhs%var_name)) then
            lhs%var_name = rhs%var_name
        else if (allocated(lhs%var_name)) then
            deallocate (lhs%var_name)
        end if
        if (allocated(rhs%var_names)) then
            lhs%var_names = rhs%var_names
        else if (allocated(lhs%var_names)) then
            deallocate (lhs%var_names)
        end if
        lhs%is_multi_declaration = rhs%is_multi_declaration
        lhs%kind_value = rhs%kind_value
        lhs%has_kind = rhs%has_kind
        if (allocated(rhs%character_length_expr)) then
            lhs%character_length_expr = rhs%character_length_expr
        else if (allocated(lhs%character_length_expr)) then
            deallocate (lhs%character_length_expr)
        end if
        lhs%has_character_length = rhs%has_character_length
        if (allocated(rhs%intent)) then
            lhs%intent = rhs%intent
        else if (allocated(lhs%intent)) then
            deallocate (lhs%intent)
        end if
        lhs%has_intent = rhs%has_intent
        lhs%is_optional = rhs%is_optional
        lhs%initializer_index = rhs%initializer_index
        lhs%has_initializer = rhs%has_initializer
        lhs%is_array = rhs%is_array
        lhs%is_allocatable = rhs%is_allocatable
        lhs%is_pointer = rhs%is_pointer
        lhs%is_target = rhs%is_target
        lhs%is_external = rhs%is_external
        lhs%is_unsigned = rhs%is_unsigned
        lhs%is_parameter = rhs%is_parameter
        lhs%is_save = rhs%is_save
        lhs%is_volatile = rhs%is_volatile
        lhs%is_protected = rhs%is_protected
        lhs%is_asynchronous = rhs%is_asynchronous
        lhs%is_contiguous = rhs%is_contiguous
        lhs%is_value = rhs%is_value
        lhs%is_bind_c = rhs%is_bind_c
        lhs%is_inferred = rhs%is_inferred
        lhs%disable_grouping = rhs%disable_grouping
        if (allocated(rhs%accessibility)) then
            lhs%accessibility = rhs%accessibility
        else if (allocated(lhs%accessibility)) then
            deallocate (lhs%accessibility)
        end if
        if (allocated(rhs%bind_name)) then
            lhs%bind_name = rhs%bind_name
        else if (allocated(lhs%bind_name)) then
            deallocate (lhs%bind_name)
        end if
        if (allocated(rhs%dimension_indices)) then
            lhs%dimension_indices = rhs%dimension_indices
        else if (allocated(lhs%dimension_indices)) then
            deallocate (lhs%dimension_indices)
        end if
    end subroutine declaration_assign

    ! Stub implementations for parameter_declaration_node
    subroutine parameter_declaration_accept(this, visitor)
        class(parameter_declaration_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine parameter_declaration_accept

    subroutine parameter_declaration_assign(lhs, rhs)
        class(parameter_declaration_node), intent(inout) :: lhs
        class(parameter_declaration_node), intent(in) :: rhs
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%resolved_type_found = rhs%resolved_type_found
        lhs%resolved_type_kind = rhs%resolved_type_kind
        lhs%resolved_kind_value = rhs%resolved_kind_value
        lhs%resolved_storage_bits = rhs%resolved_storage_bits
        lhs%resolved_rank = rhs%resolved_rank
        lhs%resolved_derived_type_name = rhs%resolved_derived_type_name
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy derived class fields
        if (allocated(rhs%name)) then
            lhs%name = rhs%name
        else if (allocated(lhs%name)) then
            deallocate (lhs%name)
        end if
        if (allocated(rhs%type_name)) then
            lhs%type_name = rhs%type_name
        else if (allocated(lhs%type_name)) then
            deallocate (lhs%type_name)
        end if
        lhs%kind_value = rhs%kind_value
        lhs%has_kind = rhs%has_kind
        if (allocated(rhs%character_length_expr)) then
            lhs%character_length_expr = rhs%character_length_expr
        else if (allocated(lhs%character_length_expr)) then
            deallocate (lhs%character_length_expr)
        end if
        lhs%has_character_length = rhs%has_character_length
        lhs%intent_type = rhs%intent_type
        lhs%is_optional = rhs%is_optional
        lhs%is_target = rhs%is_target
        lhs%is_unsigned = rhs%is_unsigned
        lhs%is_array = rhs%is_array
        if (allocated(rhs%dimension_indices)) then
            lhs%dimension_indices = rhs%dimension_indices
        else if (allocated(lhs%dimension_indices)) then
            deallocate (lhs%dimension_indices)
        end if
    end subroutine parameter_declaration_assign

    ! Stub implementations for module_node
    subroutine module_accept(this, visitor)
        class(module_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine module_accept

    subroutine module_assign(lhs, rhs)
        class(module_node), intent(inout) :: lhs
        class(module_node), intent(in) :: rhs
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy derived class fields
        if (allocated(rhs%name)) lhs%name = rhs%name
        ! Handle declaration_indices array
        if (allocated(rhs%declaration_indices)) then
            lhs%declaration_indices = rhs%declaration_indices
        end if

        ! Handle procedure_indices array
        if (allocated(rhs%procedure_indices)) then
            lhs%procedure_indices = rhs%procedure_indices
        end if
        lhs%has_contains = rhs%has_contains
    end subroutine module_assign

    subroutine submodule_accept(this, visitor)
        class(submodule_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Submodule nodes are not handled by the legacy ast_visitor_base_t
        ! interface. Fail fast if this entry point is used.
        error stop "submodule_accept is not supported for ast_visitor_base_t"
    end subroutine submodule_accept

    subroutine submodule_assign(lhs, rhs)
        class(submodule_node), intent(inout) :: lhs
        class(submodule_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        if (allocated(rhs%name)) lhs%name = rhs%name
        if (allocated(rhs%parent_identifier)) then
            lhs%parent_identifier = rhs%parent_identifier
        end if
        if (allocated(rhs%declaration_indices)) then
            lhs%declaration_indices = rhs%declaration_indices
        end if
        if (allocated(rhs%procedure_indices)) then
            lhs%procedure_indices = rhs%procedure_indices
        end if
        lhs%has_contains = rhs%has_contains
    end subroutine submodule_assign

    ! Stub implementations for block_data_node
    subroutine block_data_accept(this, visitor)
        class(block_data_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine block_data_accept

    subroutine block_data_assign(lhs, rhs)
        class(block_data_node), intent(inout) :: lhs
        class(block_data_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        if (allocated(rhs%name)) then
            lhs%name = rhs%name
        else if (allocated(lhs%name)) then
            deallocate (lhs%name)
        end if

        if (allocated(rhs%header_label)) then
            lhs%header_label = rhs%header_label
        else if (allocated(lhs%header_label)) then
            deallocate (lhs%header_label)
        end if

        if (allocated(rhs%end_label)) then
            lhs%end_label = rhs%end_label
        else if (allocated(lhs%end_label)) then
            deallocate (lhs%end_label)
        end if

        if (allocated(rhs%statement_indices)) then
            lhs%statement_indices = rhs%statement_indices
        else if (allocated(lhs%statement_indices)) then
            deallocate (lhs%statement_indices)
        end if
    end subroutine block_data_assign

    ! Stub implementations for type_binding_node
    subroutine type_binding_accept(this, visitor)
        class(type_binding_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
    end subroutine type_binding_accept

    subroutine type_binding_assign(lhs, rhs)
        class(type_binding_node), intent(inout) :: lhs
        class(type_binding_node), intent(in) :: rhs
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        if (allocated(rhs%binding_name)) lhs%binding_name = rhs%binding_name
        if (allocated(rhs%implementation)) lhs%implementation = rhs%implementation
        if (allocated(rhs%generic_list)) then
            if (allocated(lhs%generic_list)) then
                if (size(lhs%generic_list) /= size(rhs%generic_list)) then
                    deallocate (lhs%generic_list)
                    allocate (lhs%generic_list(size(rhs%generic_list)))
                end if
            else
                allocate (lhs%generic_list(size(rhs%generic_list)))
            end if
            lhs%generic_list = rhs%generic_list
        else if (allocated(lhs%generic_list)) then
            deallocate (lhs%generic_list)
        end if
        lhs%is_generic = rhs%is_generic
        lhs%is_final = rhs%is_final
        lhs%is_deferred = rhs%is_deferred
        lhs%pass_arg = rhs%pass_arg
        if (allocated(rhs%pass_name)) lhs%pass_name = rhs%pass_name
        if (allocated(rhs%accessibility)) lhs%accessibility = rhs%accessibility
    end subroutine type_binding_assign

    ! Stub implementations for derived_type_node
    subroutine derived_type_accept(this, visitor)
        class(derived_type_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine derived_type_accept

    subroutine derived_type_assign(lhs, rhs)
        class(derived_type_node), intent(inout) :: lhs
        class(derived_type_node), intent(in) :: rhs
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy derived class fields
        if (allocated(rhs%name)) lhs%name = rhs%name
        ! Handle extends_parent field
        if (allocated(rhs%extends_parent)) then
            lhs%extends_parent = rhs%extends_parent
        else if (allocated(lhs%extends_parent)) then
            deallocate (lhs%extends_parent)
        end if
        ! Handle attribute clause
        if (allocated(rhs%attribute_clause)) then
            lhs%attribute_clause = rhs%attribute_clause
        else if (allocated(lhs%attribute_clause)) then
            deallocate (lhs%attribute_clause)
        end if
        lhs%has_attributes = rhs%has_attributes

        ! Handle component_indices array
        if (allocated(rhs%component_indices)) then
            lhs%component_indices = rhs%component_indices
        end if

        lhs%has_parameters = rhs%has_parameters

        ! Handle param_indices array
        if (allocated(rhs%param_indices)) then
            lhs%param_indices = rhs%param_indices
        end if

        lhs%has_contains = rhs%has_contains

        ! Handle binding_indices array
        if (allocated(rhs%binding_indices)) then
            lhs%binding_indices = rhs%binding_indices
        end if
    end subroutine derived_type_assign

    ! Constructor for module node (migrated from ast_core)
    function create_module(name, declaration_indices, procedure_indices, &
            has_contains, line, column) result(node)
        use uid_generator, only: generate_uid
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: declaration_indices(:), procedure_indices(:)
        logical, intent(in), optional :: has_contains
        integer, intent(in), optional :: line, column
        type(module_node) :: node

        node%uid = generate_uid()
        node%name = name
        if (present(has_contains)) node%has_contains = has_contains

        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) then
                node%declaration_indices = declaration_indices
            end if
        end if

        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                node%procedure_indices = procedure_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_module

    ! Constructor for block data node
    function create_block_data(name, statement_indices, line, column, header_label, &
            end_label) result(node)
        use uid_generator, only: generate_uid
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: statement_indices(:)
        integer, intent(in), optional :: line, column
        character(len=*), intent(in), optional :: header_label, end_label
        type(block_data_node) :: node

        node%uid = generate_uid()
        node%name = name
        if (present(header_label)) then
            if (len_trim(header_label) > 0) node%header_label = header_label
        end if
        if (present(end_label)) then
            if (len_trim(end_label) > 0) node%end_label = end_label
        end if

        if (present(statement_indices)) then
            if (size(statement_indices) > 0) then
                node%statement_indices = statement_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_block_data

    ! Constructor for submodule node (Fortran 2008)
    function create_submodule(name, parent_identifier, declaration_indices, &
            procedure_indices, has_contains, line, column) &
            result(node)
        use uid_generator, only: generate_uid
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: parent_identifier
        integer, intent(in), optional :: declaration_indices(:), procedure_indices(:)
        logical, intent(in), optional :: has_contains
        integer, intent(in), optional :: line, column
        type(submodule_node) :: node

        node%uid = generate_uid()
        node%name = name
        node%parent_identifier = parent_identifier
        if (present(has_contains)) node%has_contains = has_contains

        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) then
                node%declaration_indices = declaration_indices
            end if
        end if

        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                node%procedure_indices = procedure_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_submodule

    ! Factory functions
    function create_declaration(type_name, var_name, kind_value, &
            initializer_index, dimension_indices, &
            is_allocatable, is_pointer, is_target, &
            is_external, line, column) result(node)
        character(len=*), intent(in) :: type_name
        character(len=*), intent(in) :: var_name
        integer, intent(in), optional :: kind_value
        integer, intent(in), optional :: initializer_index
        integer, intent(in), optional :: dimension_indices(:)
        logical, intent(in), optional :: is_allocatable
        logical, intent(in), optional :: is_pointer
        logical, intent(in), optional :: is_target
        logical, intent(in), optional :: is_external
        integer, intent(in), optional :: line, column
        type(declaration_node) :: node

        node%uid = generate_uid()
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
        if (present(is_external)) node%is_external = is_external
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_declaration

    function create_derived_type(name, component_indices, param_indices, &
            extends_parent, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: component_indices(:)
        integer, intent(in), optional :: param_indices(:)
        character(len=*), intent(in), optional :: extends_parent
        integer, intent(in), optional :: line, column
        type(derived_type_node) :: node

        node%uid = generate_uid()
        node%name = name

        if (present(extends_parent)) node%extends_parent = extends_parent

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

    function create_type_binding(binding_name, implementation, is_generic, &
            is_final, is_deferred, pass_arg, &
            accessibility, line, column) result(node)
        character(len=*), intent(in) :: binding_name
        character(len=*), intent(in), optional :: implementation
        logical, intent(in), optional :: is_generic
        logical, intent(in), optional :: is_final
        logical, intent(in), optional :: is_deferred
        logical, intent(in), optional :: pass_arg
        character(len=*), intent(in), optional :: accessibility
        integer, intent(in), optional :: line, column
        type(type_binding_node) :: node

        node%uid = generate_uid()
        node%binding_name = binding_name

        if (present(implementation)) node%implementation = implementation
        if (present(is_generic)) node%is_generic = is_generic
        if (present(is_final)) node%is_final = is_final
        if (present(is_deferred)) node%is_deferred = is_deferred
        if (present(pass_arg)) node%pass_arg = pass_arg
        if (present(accessibility)) node%accessibility = accessibility
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_type_binding

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

    ! Stub implementations for mixed_construct_container_node
    subroutine mixed_construct_container_accept(this, visitor)
        class(mixed_construct_container_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation
    end subroutine mixed_construct_container_accept

    subroutine mixed_construct_container_assign(lhs, rhs)
        class(mixed_construct_container_node), intent(inout) :: lhs
        class(mixed_construct_container_node), intent(in) :: rhs
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy derived class fields
        if (allocated(rhs%module_name)) then
            lhs%module_name = rhs%module_name
        else if (allocated(lhs%module_name)) then
            deallocate (lhs%module_name)
        end if
        if (allocated(rhs%implicit_declaration_indices)) then
            lhs%implicit_declaration_indices = rhs%implicit_declaration_indices
        else if (allocated(lhs%implicit_declaration_indices)) then
            deallocate (lhs%implicit_declaration_indices)
        end if
        if (allocated(rhs%explicit_program_indices)) then
            lhs%explicit_program_indices = rhs%explicit_program_indices
        else if (allocated(lhs%explicit_program_indices)) then
            deallocate (lhs%explicit_program_indices)
        end if
    end subroutine mixed_construct_container_assign

    subroutine multi_unit_container_accept(this, visitor)
        class(multi_unit_container_node), intent(in) :: this
        class(ast_visitor_base_t), intent(inout) :: visitor
        ! Stub implementation; traversal uses body_indices directly
    end subroutine multi_unit_container_accept

    subroutine multi_unit_container_assign(lhs, rhs)
        class(multi_unit_container_node), intent(inout) :: lhs
        class(multi_unit_container_node), intent(in) :: rhs
        ! Copy base class fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        lhs%uid = rhs%uid
        lhs%inferred_type = rhs%inferred_type
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        ! Copy derived class fields
        if (allocated(rhs%body_indices)) then
            lhs%body_indices = rhs%body_indices
        else if (allocated(lhs%body_indices)) then
            deallocate (lhs%body_indices)
        end if
    end subroutine multi_unit_container_assign

    ! Factory function for the multi-unit container
    function create_multi_unit_container(body_indices, line, column) result(node)
        integer, intent(in) :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(multi_unit_container_node) :: node

        node%uid = generate_uid()
        if (size(body_indices) > 0) node%body_indices = body_indices
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_multi_unit_container

    ! Factory function for mixed construct container
    function create_mixed_construct_container(module_name, implicit_indices, &
            explicit_indices, line, column) &
            result(node)
        character(len=*), intent(in) :: module_name
        integer, intent(in), optional :: implicit_indices(:)
        integer, intent(in), optional :: explicit_indices(:)
        integer, intent(in), optional :: line, column
        type(mixed_construct_container_node) :: node

        node%uid = generate_uid()
        node%module_name = module_name

        if (present(implicit_indices)) then
            if (size(implicit_indices) > 0) then
                node%implicit_declaration_indices = implicit_indices
            end if
        end if

        if (present(explicit_indices)) then
            if (size(explicit_indices) > 0) then
                node%explicit_program_indices = explicit_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_mixed_construct_container

end module ast_nodes_data
