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
        type(mono_type_t) :: inferred_type  ! Type information
        ! from semantic analysis

        ! Unique identifier for CST/AST bidirectional linking
        type(uid_t) :: uid

        ! Statement label (for GOTO targets, like 10  i = i + 1)
        character(len=:), allocatable :: stmt_label

        ! Constant folding information
        logical :: is_constant = .false.  ! True if this node is a compile-time constant
        logical :: constant_logical = .false.  ! For logical constants
        integer :: constant_integer = 0  ! For integer constants
        real(dp) :: constant_real = 0.0_dp  ! For real constants
        integer :: constant_type = 0  ! Type of constant (LITERAL_* constants)
    contains
        procedure(visit_interface), deferred :: accept
    end type ast_node

    ! Wrapper type for polymorphic arrays - BUT NOW BACKED BY STACK
    type, public :: ast_node_wrapper
        class(ast_node), allocatable :: node
        integer :: stack_index = 0  ! NEW: Index in AST stack for O(depth) access
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

contains

    subroutine ast_node_wrapper_assign(lhs, rhs)
        class(ast_node_wrapper), intent(inout) :: lhs
        class(ast_node_wrapper), intent(in) :: rhs

        ! Deep copy the node if allocated
        if (allocated(rhs%node)) then
            if (allocated(lhs%node)) deallocate (lhs%node)
            allocate (lhs%node, source=rhs%node)
        end if

        ! Copy the stack index
        lhs%stack_index = rhs%stack_index
    end subroutine ast_node_wrapper_assign

end module ast_base
