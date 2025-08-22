module ast_typed
    ! Extended AST nodes with type information for the compiler frontend
    use ast_core
    use type_system_arena, only: mono_handle_t, null_mono_handle
    implicit none
    private

    public :: typed_ast_node

    ! Extended AST node with type information
    type, extends(ast_node), abstract :: typed_ast_node
        type(mono_handle_t) :: inferred_type = null_mono_handle()
    end type typed_ast_node

end module ast_typed
