module ast_base
    use type_system_unified, only: mono_type_t
    use string_types, only: string_t
    use uid_generator, only: uid_t
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    private

    ! Re-export string_t for convenience
    public :: string_t

    ! Literal type constants
    integer, parameter, public :: LITERAL_INTEGER = 1
    integer, parameter, public :: LITERAL_REAL = 2
    integer, parameter, public :: LITERAL_STRING = 3
    integer, parameter, public :: LITERAL_LOGICAL = 4
    integer, parameter, public :: LITERAL_ARRAY = 5
    integer, parameter, public :: LITERAL_COMPLEX = 6

    ! Base AST node type used by all dialects
    type, abstract, public :: ast_node
        integer :: line = 1
        integer :: column = 1
        type(mono_type_t) :: inferred_type ! Type information
        ! from semantic analysis

        ! Exact compiler-facing type metadata.  The broad inferred_type kind is
        ! retained for compatibility; these fields preserve the resolved
        ! Fortran kind selector and scalar/array identity without requiring a
        ! consumer to inspect source spelling.
        logical :: resolved_type_found = .false.
        integer :: resolved_type_kind = 0
        integer :: resolved_kind_value = 0
        integer :: resolved_storage_bits = 0
        integer :: resolved_rank = -1
        character(len=64) :: resolved_derived_type_name = ""

        ! Unique identifier for CST/AST bidirectional linking
        type(uid_t) :: uid

        ! Statement label (for GOTO targets, like 10  i = i + 1)
        character(len=:), allocatable :: stmt_label

        ! Trailing inline comment (e.g. "x = 1  ! set x")
        character(len=:), allocatable :: trailing_comment

        ! Constant folding information
        logical :: is_constant = .false. ! True if this node is a compile-time constant
        logical :: constant_logical = .false. ! For logical constants
        integer :: constant_integer = 0 ! For integer constants
        real(dp) :: constant_real = 0.0_dp ! For real constants
        integer :: constant_type = 0 ! Type of constant (LITERAL_* constants)
    contains
        procedure(visit_interface), deferred :: accept
    end type ast_node

    ! Wrapper type for polymorphic arrays - BUT NOW BACKED BY STACK
    type, public :: ast_node_wrapper
        class(ast_node), allocatable :: node
        integer :: stack_index = 0 ! NEW: Index in AST stack for O(depth) access
    contains
        procedure :: assign => ast_node_wrapper_assign
        generic :: assignment(=) => assign
    end type ast_node_wrapper

    ! Forward declaration for abstract visitor
    type, abstract :: ast_visitor_base_t
    end type ast_visitor_base_t

    ! Abstract interface for visitor pattern
    abstract interface
        subroutine visit_interface(this, visitor)
            import :: ast_node, ast_visitor_base_t
            class(ast_node), intent(in) :: this
            class(ast_visitor_base_t), intent(inout) :: visitor
        end subroutine visit_interface
    end interface

    ! Make interface and visitor base public
    public :: visit_interface, ast_visitor_base_t
    public :: copy_ast_node_base

contains

    subroutine copy_ast_node_base(lhs, rhs)
        class(ast_node), intent(inout) :: lhs
        class(ast_node), intent(in) :: rhs
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
        if (allocated(rhs%stmt_label)) then
            lhs%stmt_label = rhs%stmt_label
        else if (allocated(lhs%stmt_label)) then
            deallocate (lhs%stmt_label)
        end if
        if (allocated(rhs%trailing_comment)) then
            lhs%trailing_comment = rhs%trailing_comment
        else if (allocated(lhs%trailing_comment)) then
            deallocate (lhs%trailing_comment)
        end if
    end subroutine copy_ast_node_base

    subroutine ast_node_wrapper_assign(lhs, rhs)
        class(ast_node_wrapper), intent(inout) :: lhs
        class(ast_node_wrapper), intent(in) :: rhs

        if (allocated(rhs%node)) then
            if (allocated(lhs%node)) deallocate (lhs%node)
            allocate (lhs%node, source=rhs%node)
        else if (allocated(lhs%node)) then
            deallocate (lhs%node)
        end if

        lhs%stack_index = rhs%stack_index
    end subroutine ast_node_wrapper_assign

end module ast_base
