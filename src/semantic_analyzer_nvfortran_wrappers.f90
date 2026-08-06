!! Compatibility entry points for nvfortran's submodule procedure lowering.
!!
!! nvfortran 26.5 emits unqualified external calls for these two private
!! module-subroutine references from semantic_analyzer_infer_impl. The normal
!! module procedures remain the source of truth; these thin wrappers only
!! provide the ABI names emitted by that compiler.

subroutine infer_allocate_statement(ctx, arena, alloc_stmt, stmt_index, typ)
    use semantic_analyzer, only: semantic_context_t, &
        infer_allocate_statement_impl => infer_allocate_statement
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_misc, only: allocate_statement_node
    use type_system_unified, only: mono_type_t
    implicit none
    type(semantic_context_t), intent(inout) :: ctx
    type(ast_arena_t), intent(inout) :: arena
    type(allocate_statement_node), intent(in) :: alloc_stmt
    integer, intent(in) :: stmt_index
    type(mono_type_t), intent(out) :: typ

    call infer_allocate_statement_impl(ctx, arena, alloc_stmt, stmt_index, typ)
end subroutine infer_allocate_statement

subroutine set_node_inferred_type(arena, index, typ)
    use semantic_analyzer, only: set_node_inferred_type_impl => set_node_inferred_type
    use ast_arena_modern, only: ast_arena_t
    use type_system_unified, only: mono_type_t
    implicit none
    type(ast_arena_t), intent(inout) :: arena
    integer, intent(in) :: index
    type(mono_type_t), intent(in) :: typ

    call set_node_inferred_type_impl(arena, index, typ)
end subroutine set_node_inferred_type
