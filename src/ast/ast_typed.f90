module ast_typed
    ! Extended AST nodes with type information for the compiler frontend
    use ast_base, only: ast_node
    implicit none
    private

    public :: typed_ast_node

    ! Extended AST node with type information
    type, extends(ast_node), abstract :: typed_ast_node
    end type typed_ast_node

end module ast_typed
