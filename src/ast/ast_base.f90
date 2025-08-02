module ast_base
    use json_module
    use type_system_hm, only: mono_type_t
    use string_types, only: string_t
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
        type(mono_type_t), allocatable :: inferred_type  ! Type information
        ! from semantic analysis
    contains
        procedure(visit_interface), deferred :: accept
        procedure(to_json_interface), deferred :: to_json
    end type ast_node

    ! Wrapper type for polymorphic arrays - BUT NOW BACKED BY STACK
    type, public :: ast_node_wrapper
        class(ast_node), allocatable :: node
        integer :: stack_index = 0  ! NEW: Index in AST stack for O(depth) access
    contains
        procedure :: assign => ast_node_wrapper_assign
        generic :: assignment(=) => assign
    end type ast_node_wrapper

    ! Abstract interfaces for visitor pattern and JSON serialization
    abstract interface
        subroutine visit_interface(this, visitor)
            import :: ast_node
            class(ast_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine visit_interface

        subroutine to_json_interface(this, json, parent)
            use json_module
            import :: ast_node
            class(ast_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine to_json_interface
    end interface

    ! Make interfaces public
    public :: visit_interface, to_json_interface

contains

    subroutine ast_node_wrapper_assign(lhs, rhs)
        class(ast_node_wrapper), intent(inout) :: lhs
        class(ast_node_wrapper), intent(in) :: rhs
        
        ! Deep copy the node if allocated
        if (allocated(rhs%node)) then
            if (allocated(lhs%node)) deallocate(lhs%node)
            allocate(lhs%node, source=rhs%node)
        end if
        
        ! Copy the stack index
        lhs%stack_index = rhs%stack_index
    end subroutine ast_node_wrapper_assign

end module ast_base