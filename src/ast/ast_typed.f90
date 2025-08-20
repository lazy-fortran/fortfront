module ast_typed
    ! Extended AST nodes with type information for the compiler frontend
    use ast_core
    use type_system_hm, only: mono_type_t
    implicit none
    private

    public :: typed_ast_node

    ! Extended AST node with type information
    ! Note: inferred_type is already present in the base ast_node type
    type, extends(ast_node), abstract :: typed_ast_node
        ! No additional fields needed - inferred_type is inherited from ast_node
    end type typed_ast_node

end module ast_typed
