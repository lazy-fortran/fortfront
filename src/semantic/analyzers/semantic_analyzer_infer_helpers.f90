submodule(semantic_analyzer) semantic_analyzer_infer_helpers
use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
    allocation_info_t, create_mono_type, &
    create_poly_type, create_type_var, &
    TREAL, TVAR, TINT, TLOGICAL, TCHAR, &
    TCOMPLEX, TARRAY
implicit none

contains

    module subroutine infer_allocate_statement(ctx, arena, alloc_stmt, stmt_index, typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(allocate_statement_node), intent(in) :: alloc_stmt
        integer, intent(in) :: stmt_index
        type(mono_type_t), intent(out) :: typ
        integer :: i, var_index, rank, j
        type(mono_type_t) :: var_type, element_type
        type(mono_type_t), allocatable :: args(:)
        type(poly_type_t) :: var_scheme
        type(poly_type_t), allocatable :: existing_scheme
        character(len=:), allocatable :: var_name
        character(len=:), allocatable :: type_spec_buf

        typ = create_mono_type(TVAR, var=create_type_var(0, "mem"))

        if (.not. allocated(alloc_stmt%var_indices)) return

        do i = 1, size(alloc_stmt%var_indices)
            var_index = alloc_stmt%var_indices(i)
            if (var_index <= 0) cycle
            if (.not. allocated(arena%entries(var_index)%node)) cycle

            var_name = ""
            rank = 0
            select type (node => arena%entries(var_index)%node)
                type is (identifier_node)
                var_name = node%name
                if (allocated(alloc_stmt%shape_indices)) then
                    rank = size(alloc_stmt%shape_indices)
                end if
                type is (call_or_subscript_node)
                var_name = node%name
                if (allocated(node%arg_indices)) then
                    rank = size(node%arg_indices)
                else if (allocated(alloc_stmt%shape_indices)) then
                    rank = size(alloc_stmt%shape_indices)
                end if
            end select

            if (len_trim(var_name) > 0) then
                call ctx%scopes%lookup(var_name, existing_scheme)

                if (.not. allocated(existing_scheme)) then
                    element_type = get_inferred_type_from_arena(ctx, arena, var_index)

                    if (allocated(alloc_stmt%type_spec)) then
                        if (len_trim(alloc_stmt%type_spec) > 0) then
                            type_spec_buf = to_lower(trim(alloc_stmt%type_spec))
                            select case (trim(type_spec_buf))
                            case ('integer')
                                element_type = create_mono_type(TINT)
                            case ('real')
                                element_type = create_mono_type(TREAL)
                            case ('logical')
                                element_type = create_mono_type(TLOGICAL)
                            case ('character')
                                element_type = create_mono_type(TCHAR)
                            case ('complex')
                                element_type = create_mono_type(TCOMPLEX)
                            end select
                        end if
                    else if (element_type%kind == TVAR .and. &
                            element_type%var%id == 0) then
                        element_type = create_mono_type(TINT)
                    else if (element_type%kind == TREAL) then
                        element_type = create_mono_type(TINT)
                    end if

                    if (rank > 0) then
                        var_type = element_type
                        do j = 1, rank
                            allocate (args(1))
                            args(1) = var_type
                            var_type = create_mono_type(TARRAY, args=args)
                            var_type%alloc_info%is_allocatable = .true.
                            deallocate (args)
                        end do
                    else
                        var_type = element_type
                        var_type%alloc_info%is_allocatable = .true.
                    end if

                    if (var_type%kind == TCHAR .or. element_type%kind == TCHAR) then
                        var_type%alloc_info%needs_allocatable_string = .true.
                    end if

                    call update_identifier_type_in_arena(arena, var_name, var_type)

                    var_scheme = create_poly_type(forall_vars=[type_var_t ::], &
                        mono=var_type)
                    call ctx%scopes%define(var_name, var_scheme)

                    call set_node_inferred_type(arena, var_index, var_type)
                end if
            end if
        end do
    end subroutine infer_allocate_statement

    module subroutine ensure_string_literal_type(arena, value_index, expr_typ)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: value_index
        type(mono_type_t), intent(inout) :: expr_typ
        integer :: literal_length

        if (value_index <= 0) return
        if (value_index > arena%size) return
        if (.not. allocated(arena%entries(value_index)%node)) return

        select type (value_node => arena%entries(value_index)%node)
            type is (literal_node)
            if (value_node%literal_kind == LITERAL_STRING) then
                literal_length = compute_string_literal_length(value_node)
                expr_typ = create_mono_type(TCHAR, char_size=literal_length)
                call set_node_inferred_type(arena, value_index, expr_typ)
            end if
        end select
    end subroutine ensure_string_literal_type

    pure module integer function compute_string_literal_length(literal) &
            result(len_value)
        type(literal_node), intent(in) :: literal
        character(len=:), allocatable :: trimmed_value
        character(len=:), allocatable :: inner_value
        character(len=1) :: quote_char
        integer :: trimmed_length
        integer :: i

        len_value = 0
        if (.not. allocated(literal%value)) return

        trimmed_value = adjustl(trim(literal%value))
        trimmed_length = len_trim(trimmed_value)
        if (trimmed_length < 2) then
            len_value = trimmed_length
            return
        end if

        quote_char = trimmed_value(1:1)
        if (quote_char /= '"' .and. quote_char /= "'") then
            len_value = trimmed_length
            return
        end if

        if (trimmed_value(trimmed_length:trimmed_length) /= quote_char) then
            len_value = trimmed_length
            return
        end if

        if (trimmed_length == 2) then
            len_value = 0
            return
        end if

        inner_value = trimmed_value(2:trimmed_length - 1)
        len_value = len(inner_value)

        i = 1
        do while (i <= len(inner_value) - 1)
            if (inner_value(i:i) == quote_char .and. &
                inner_value(i + 1:i + 1) == quote_char) then
                len_value = len_value - 1
                i = i + 2
            else
                i = i + 1
            end if
        end do
    end function compute_string_literal_length

end submodule semantic_analyzer_infer_helpers
