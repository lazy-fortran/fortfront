
    ! Module-level wrapper function that uses global context (workaround for gfortran bug with internal procedures as procedure pointers)
    function module_get_node_type_wrapper(a, idx) result(res_type)
        type(ast_arena_t), intent(inout) :: a
        integer, intent(in) :: idx
        type(mono_type_t) :: res_type

        if (associated(g_current_context) .and. associated(g_current_arena)) then
            res_type = get_inferred_type_from_arena(g_current_context, g_current_arena, idx)
        else
            res_type = create_mono_type(TREAL)
        end if
    end function module_get_node_type_wrapper

    module function infer_statement_type(this, arena, stmt_index) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_index
        type(mono_type_t) :: typ

        typ = this%infer(arena, stmt_index)
    end function infer_statement_type
