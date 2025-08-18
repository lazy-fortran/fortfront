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
        
        ! Constant folding information
        logical :: is_constant = .false.  ! True if this node is a compile-time constant
        logical :: constant_logical       ! For logical constants
        integer :: constant_integer       ! For integer constants
        real :: constant_real             ! For real constants
        integer :: constant_type = 0      ! Type of constant (LITERAL_* constants)
    contains
        procedure(visit_interface), deferred :: accept
        procedure(to_json_interface), deferred :: to_json
        procedure :: copy_base_fields => ast_node_copy_base_fields
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

    ! Base assignment procedure for all AST nodes (cycle-safe type copying)
    subroutine ast_node_copy_base_fields(this, lhs, rhs)
        class(ast_node), intent(in) :: this  ! Polymorphic receiver (not used but required)
        class(ast_node), intent(inout) :: lhs
        class(ast_node), intent(in) :: rhs
        
        ! Copy basic fields
        lhs%line = rhs%line
        lhs%column = rhs%column
        
        ! Copy constant folding information
        lhs%is_constant = rhs%is_constant
        lhs%constant_logical = rhs%constant_logical
        lhs%constant_integer = rhs%constant_integer
        lhs%constant_real = rhs%constant_real
        lhs%constant_type = rhs%constant_type
        
        ! TEMPORARY: Skip inferred_type copying to prevent memory corruption
        ! The issue is that shared allocatable components in mono_type_t cause
        ! double-free errors during program finalization. This breaks type
        ! information flow but prevents crashes until proper solution implemented.
        ! TODO: Implement proper cycle-safe deep copying or reference counting
        if (allocated(rhs%inferred_type)) then
            ! Skip copying for now - this breaks semantic information but prevents crashes
            if (allocated(lhs%inferred_type)) then
                deallocate(lhs%inferred_type)
            end if
        else
            if (allocated(lhs%inferred_type)) then
                deallocate(lhs%inferred_type)
            end if
        end if
    end subroutine ast_node_copy_base_fields

    subroutine ast_node_wrapper_assign(lhs, rhs)
        class(ast_node_wrapper), intent(inout) :: lhs
        class(ast_node_wrapper), intent(in) :: rhs
        
        ! Deep copy the node if allocated
        if (allocated(rhs%node)) then
            ! Only deallocate if we need to change type
            if (allocated(lhs%node)) then
                if (.not. same_type_as(lhs%node, rhs%node)) then
                    deallocate(lhs%node)
                end if
            end if
            if (.not. allocated(lhs%node)) then
                allocate(lhs%node, source=rhs%node)
            else
                ! Use assignment if same type (safer)
                lhs%node = rhs%node
            end if
        end if
        
        ! Copy the stack index
        lhs%stack_index = rhs%stack_index
    end subroutine ast_node_wrapper_assign

end module ast_base