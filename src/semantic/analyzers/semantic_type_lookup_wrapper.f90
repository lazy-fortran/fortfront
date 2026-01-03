module semantic_type_lookup_wrapper
    !> Provides a workaround for Fortran's lack of closures
    !> Stores semantic context in a module variable so it can be accessed
    !> by a wrapper function that can be passed as a procedure pointer
    use type_system_unified, only: mono_type_t, create_mono_type, TVAR, TREAL
    use ast_arena_modern, only: ast_arena_t
    use semantic_validation_utils, only: int_to_str
    implicit none
    private

    public :: set_semantic_context_for_lookup, clear_semantic_context_for_lookup
    public :: get_type_with_stored_context

    ! Forward declare the semantic context type
    ! We can't import it directly due to circular dependencies
    ! So we use class(*) and cast as needed
    class(*), pointer, save :: g_semantic_ctx => null()

contains

    !> Store the semantic context for later use by get_type_with_stored_context
    subroutine set_semantic_context_for_lookup(ctx)
        class(*), target :: ctx
        g_semantic_ctx => ctx
    end subroutine set_semantic_context_for_lookup

    !> Clear the stored context
    subroutine clear_semantic_context_for_lookup()
        nullify (g_semantic_ctx)
    end subroutine clear_semantic_context_for_lookup

    !> Get type using the stored semantic context
    !> This function matches the get_type_lookup interface and can be passed
    !> as a procedure pointer
    function get_type_with_stored_context(arena, idx) result(typ)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: idx
        type(mono_type_t) :: typ

        ! Simply return the inferred type from the node
        ! The substitution will be applied by the caller if needed
        typ = create_mono_type(TREAL)
        if (.not. arena%has_node_at(idx)) return

        typ = arena%entries(idx)%node%inferred_type
        if (typ%kind == TVAR .and. len_trim(typ%var%name) == 0) then
            typ%var%name = "v" // int_to_str(typ%var%id)
        end if
    end function get_type_with_stored_context

end module semantic_type_lookup_wrapper
