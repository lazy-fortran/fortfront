module ast_typed
    ! Extended AST nodes with type information for the compiler frontend
    use ast_core
    use type_system_hm, only: mono_type_t
    implicit none
    private

    public :: typed_ast_node

    ! Extended AST node with type information
    type, extends(ast_node), abstract :: typed_ast_node
        type(mono_type_t), allocatable :: inferred_type
    end type typed_ast_node

end module ast_typed
