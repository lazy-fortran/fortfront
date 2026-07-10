module semantic_unsigned_integer_mix_diagnostics
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: LITERAL_INTEGER
    use ast_nodes_core, only: literal_node
    use error_handling, only: create_error_result, ERROR_SEMANTIC, error_collection_t
    use type_system_unified, only: mono_type_t, TARRAY, TINT
    implicit none
    private

    public :: emit_unsigned_integer_mix_error
    public :: extract_integer_signedness
    public :: is_integer_literal_expr

contains

    subroutine emit_unsigned_integer_mix_error(errors, line, column)
        type(error_collection_t), intent(inout) :: errors
        integer, intent(in) :: line, column

        call errors%add_result(create_error_result( &
            "Implicit mixing of signed and unsigned " // &
            "integers is forbidden; use uint() or int()", &
            ERROR_SEMANTIC, &
            component="semantic_analyzer", &
            context="unsigned_integer_mix", &
            suggestion="Convert explicitly with uint(...) or " // &
            "int(...)", line=line, column=column, end_line=line, &
            end_column=column + 1))
    end subroutine emit_unsigned_integer_mix_error

    subroutine extract_integer_signedness(typ, is_int, is_unsigned)
        type(mono_type_t), intent(in) :: typ
        logical, intent(out) :: is_int
        logical, intent(out) :: is_unsigned
        type(mono_type_t) :: base

        base = typ
        do while (base%kind == TARRAY)
            if (.not. base%has_args()) exit
            if (base%get_args_count() < 1) exit
            base = base%get_arg(1)
        end do

        is_int = base%kind == TINT
        if (is_int) then
            is_unsigned = base%is_unsigned
        else
            is_unsigned = .false.
        end if
    end subroutine extract_integer_signedness

    logical function is_integer_literal_expr(arena, expr_index) result(is_lit)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index

        is_lit = .false.
        if (.not. arena%has_node_at(expr_index)) return

        select type (lit => arena%entries(expr_index)%node)
            type is (literal_node)
            is_lit = lit%literal_kind == LITERAL_INTEGER
        class default
            is_lit = .false.
        end select
    end function is_integer_literal_expr

end module semantic_unsigned_integer_mix_diagnostics
