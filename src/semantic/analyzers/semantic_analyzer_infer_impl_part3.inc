    module function infer_assignment(ctx, arena, assignment, assignment_index) &
        result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assignment
        integer, intent(in) :: assignment_index
        type(mono_type_t) :: typ
        type(mono_type_t) :: expr_typ, updated_expr_typ
        integer :: lhs_index
        lhs_index = assignment%target_index
        expr_typ = get_inferred_type_from_arena(ctx, arena, assignment%value_index)
        call ensure_string_literal_type(arena, assignment%value_index, expr_typ)
        updated_expr_typ = expr_typ

        ! Skip processing for keyword arguments - they don't create variable declarations
        if (.not. assignment%is_keyword_argument) then
            ! Use extracted assignment processing
            call process_assignment_inference( &
                arena, assignment, assignment_index, lhs_index, expr_typ, &
                    updated_expr_typ, &
                ctx%scopes, ctx%errors, ctx%input_mode, ctx%next_var_id, &
                    & ctx%parser_type_hints)
        end if

        typ = updated_expr_typ

        ! Store the actual assignment type
        call set_node_inferred_type(arena, assignment_index, typ)
    end function infer_assignment

    module function infer_pointer_assignment(ctx, arena, ptr_assign, &
                                             ptr_assign_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(pointer_assignment_node), intent(in) :: ptr_assign
        integer, intent(in) :: ptr_assign_index
        type(mono_type_t) :: typ
        type(mono_type_t) :: target_type
        type(poly_type_t) :: scheme
        type(poly_type_t), allocatable :: existing_scheme
        integer :: pointer_index

        pointer_index = ptr_assign%pointer_index

        ! Get type of target (what the pointer will point to)
        target_type = get_inferred_type_from_arena(ctx, arena, ptr_assign%target_index)

        if (is_null_pointer_target(arena, ptr_assign%target_index)) then
            target_type = create_mono_type(TINT)
        end if

        ! If pointer is an identifier, ensure it's declared
        if (pointer_index > 0 .and. pointer_index <= arena%size) then
            if (allocated(arena%entries(pointer_index)%node)) then
                select type (ptr_node => arena%entries(pointer_index)%node)
                type is (identifier_node)
                    ! Check if already defined in current or parent scope
                    call ctx%scopes%lookup(ptr_node%name, existing_scheme)

                    if (.not. allocated(existing_scheme)) then
                        ! Variable not declared yet - declare it with target type
                        ! Default to INTEGER if target type is unclear (e.g., null())
                        if (target_type%kind == TVAR .or. target_type%kind == 0) then
                            target_type = create_mono_type(TINT)
                        end if

                        scheme = create_poly_type(forall_vars=[type_var_t ::], &
                                                  mono=target_type)
                        call ctx%scopes%define(ptr_node%name, scheme)

                        ! Update all identifier nodes with this name
                        call update_identifier_type_in_arena(arena, ptr_node%name, &
                                                             target_type)
                    end if
                end select
            end if
        end if

        typ = target_type
        call set_node_inferred_type(arena, ptr_assign_index, typ)
    end function infer_pointer_assignment

    logical function is_null_pointer_target(arena, expr_index) result(is_null_target)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index

        is_null_target = .false.
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return

        select type (target_node => arena%entries(expr_index)%node)
        type is (call_or_subscript_node)
            if (allocated(target_node%name)) then
                if (to_lower(trim(target_node%name)) == 'null') then
                    is_null_target = .true.
                    return
                end if
            end if
        type is (identifier_node)
            if (allocated(target_node%name)) then
                if (to_lower(trim(target_node%name)) == 'null') then
                    is_null_target = .true.
                    return
                end if
            end if
        end select
    end function is_null_pointer_target

    module subroutine infer_read_statement(ctx, arena, read_stmt, stmt_index, typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(read_statement_node), intent(in) :: read_stmt
        integer, intent(in) :: stmt_index
        type(mono_type_t), intent(out) :: typ
        integer :: i, var_index
        type(mono_type_t) :: var_type
        type(poly_type_t) :: var_scheme
        type(poly_type_t), allocatable :: existing_scheme

        typ = create_mono_type(TVAR, var=create_type_var(0, "io"))

        if (.not. allocated(read_stmt%var_indices)) return

        do i = 1, size(read_stmt%var_indices)
            var_index = read_stmt%var_indices(i)
            if (var_index <= 0) cycle
            if (.not. allocated(arena%entries(var_index)%node)) cycle

            select type (node => arena%entries(var_index)%node)
            type is (identifier_node)
                call ctx%scopes%lookup(node%name, existing_scheme)

                if (.not. allocated(existing_scheme)) then
                    var_type = get_inferred_type_from_arena(ctx, arena, var_index)

                    if (var_type%kind == TVAR .and. var_type%var%id == 0) then
                        var_type = create_mono_type(TREAL)
                    end if

                    call update_identifier_type_in_arena(arena, node%name, var_type)

                    var_scheme = create_poly_type(forall_vars=[type_var_t ::], &
                                                  mono=var_type)
                    call ctx%scopes%define(node%name, var_scheme)

                    call set_node_inferred_type(arena, var_index, var_type)
                end if
            end select
        end do
    end subroutine infer_read_statement
