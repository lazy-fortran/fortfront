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

        call validate_strict_allocatable_lhs_reallocation(ctx, arena, &
                                                          assignment, &
                                                          assignment_index)

        ! Skip processing for keyword arguments - they don't create declarations
        ! for variables.
        if (.not. assignment%is_keyword_argument) then
            ! Use extracted assignment processing
            call process_assignment_inference( &
                arena, assignment, assignment_index, lhs_index, expr_typ, &
                updated_expr_typ, &
                ctx%scopes, ctx%errors, ctx%input_mode, ctx%next_var_id, &
                ctx%parser_type_hints)
        end if

        typ = updated_expr_typ

        ! Store the actual assignment type
        call set_node_inferred_type(arena, assignment_index, typ)
    end function infer_assignment

    subroutine validate_strict_allocatable_lhs_reallocation(ctx, arena, &
                                                            assignment, &
                                                            assignment_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assignment
        integer, intent(in) :: assignment_index

        integer :: prog_index
        integer :: prev_size
        integer :: rhs_size
        character(len=:), allocatable :: message
        character(len=:), allocatable :: name

        if (ctx%operating_mode /= OPERATING_MODE_STRICT) return

        call get_lhs_identifier_name(arena, assignment%target_index, name)
        if (.not. allocated(name)) return

        rhs_size = static_array_literal_size(arena, assignment%value_index)
        if (rhs_size <= 0) return

        prog_index = find_containing_program_index(arena, assignment_index)
        if (prog_index <= 0) return

        if (.not. is_allocatable_array_decl_in_program(arena, &
                                                       prog_index, name)) &
            return

        prev_size = find_prev_literal_assignment_size(arena, prog_index, &
                                                      assignment_index, name)
        if (prev_size <= 0) return
        if (prev_size == rhs_size) return

        message = 'Strict mode forbids automatic allocatable array ' // &
                  'reallocation on LHS for ' // trim(name) // ': existing size ' // &
                  trim(int_to_string(prev_size)) // ', assigned size ' // &
                  trim(int_to_string(rhs_size))
        call ctx%errors%add_result(create_error_result( &
                                   trim(message), ERROR_SEMANTIC, &
                                   component='semantic_analyzer', &
                                   context='strict_allocatable_lhs_realloc', &
                                   suggestion='Resize explicitly with deallocate/'// &
                                   'allocate, or use infer mode'))
    end subroutine validate_strict_allocatable_lhs_reallocation

    subroutine get_lhs_identifier_name(arena, target_index, name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: target_index
        character(len=:), allocatable, intent(out) :: name

        if (target_index <= 0 .or. target_index > arena%size) return
        if (.not. allocated(arena%entries(target_index)%node)) return

        select type (lhs => arena%entries(target_index)%node)
        type is (identifier_node)
            if (.not. allocated(lhs%name)) return
            name = trim(lhs%name)
        class default
            return
        end select
    end subroutine get_lhs_identifier_name

    integer function find_containing_program_index(arena, node_index) &
        result(prog_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        integer :: i
        integer :: j

        prog_index = 0
        if (node_index <= 0 .or. node_index > arena%size) return

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (program_node)
                if (.not. allocated(node%body_indices)) cycle
                do j = 1, size(node%body_indices)
                    if (node%body_indices(j) == node_index) then
                        prog_index = i
                        return
                    end if
                end do
            class default
                cycle
            end select
        end do
    end function find_containing_program_index

    integer function static_array_literal_size(arena, expr_index) &
        result(arr_size)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index

        integer :: i

        arr_size = 0
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return

        select type (node => arena%entries(expr_index)%node)
        type is (array_literal_node)
            if (.not. allocated(node%element_indices)) return
            do i = 1, size(node%element_indices)
                if (node%element_indices(i) <= 0) cycle
                if (node%element_indices(i) > arena%size) cycle
                if (.not. &
                    allocated(arena%entries(node%element_indices(i))%node)) &
                    cycle
                select type (elem => &
                             arena%entries(node%element_indices(i))%node)
                type is (do_loop_node)
                    arr_size = 0
                    return
                class default
                    continue
                end select
            end do
            arr_size = size(node%element_indices)
        class default
            return
        end select
    end function static_array_literal_size

    logical function is_allocatable_array_decl_in_program(arena, &
                                                          prog_index, name) &
        result(is_allocatable)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=*), intent(in) :: name

        integer :: i
        integer :: j
        integer :: idx

        is_allocatable = .false.
        if (prog_index <= 0 .or. prog_index > arena%size) return
        if (.not. allocated(arena%entries(prog_index)%node)) return

        select type (prog => arena%entries(prog_index)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do i = 1, size(prog%body_indices)
                idx = prog%body_indices(i)
                if (idx <= 0 .or. idx > arena%size) cycle
                if (.not. allocated(arena%entries(idx)%node)) cycle
                select type (node => arena%entries(idx)%node)
                type is (declaration_node)
                    if (.not. node%is_array) cycle
                    if (.not. node%is_allocatable) cycle
                    if (allocated(node%var_name)) then
                        if (trim(node%var_name) == trim(name)) then
                            is_allocatable = .true.
                            return
                        end if
                    end if
                    if (node%is_multi_declaration .and. &
                        allocated(node%var_names)) then
                        do j = 1, size(node%var_names)
                            if (trim(node%var_names(j)) == trim(name)) then
                                is_allocatable = .true.
                                return
                            end if
                        end do
                    end if
                class default
                    cycle
                end select
            end do
        class default
            return
        end select
    end function is_allocatable_array_decl_in_program

    integer function find_prev_literal_assignment_size(arena, prog_index, &
                                                       current_index, name) &
        result(prev_size)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        integer, intent(in) :: current_index
        character(len=*), intent(in) :: name

        integer :: i
        integer :: idx
        integer :: pos
        integer :: candidate_size

        prev_size = 0
        pos = program_body_position(arena, prog_index, current_index)
        if (pos <= 1) return

        do i = pos - 1, 1, -1
            idx = program_body_index_at(arena, prog_index, i)
            if (idx <= 0) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (node => arena%entries(idx)%node)
            type is (assignment_node)
                candidate_size = assignment_literal_size_if_target_name( &
                                 arena, node, name)
                if (candidate_size > 0) then
                    prev_size = candidate_size
                    return
                end if
            class default
                cycle
            end select
        end do
    end function find_prev_literal_assignment_size

    integer function program_body_position(arena, prog_index, node_index) &
        result(pos)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        integer, intent(in) :: node_index

        integer :: i
        integer :: idx

        pos = 0
        if (prog_index <= 0 .or. prog_index > arena%size) return
        if (.not. allocated(arena%entries(prog_index)%node)) return

        select type (prog => arena%entries(prog_index)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do i = 1, size(prog%body_indices)
                idx = prog%body_indices(i)
                if (idx == node_index) then
                    pos = i
                    return
                end if
            end do
        class default
            return
        end select
    end function program_body_position

    integer function program_body_index_at(arena, prog_index, pos) result(idx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        integer, intent(in) :: pos

        idx = 0
        if (prog_index <= 0 .or. prog_index > arena%size) return
        if (.not. allocated(arena%entries(prog_index)%node)) return

        select type (prog => arena%entries(prog_index)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            if (pos <= 0 .or. pos > size(prog%body_indices)) return
            idx = prog%body_indices(pos)
        class default
            return
        end select
    end function program_body_index_at

    integer function assignment_literal_size_if_target_name(arena, assignment, &
                                                            name) &
        result(arr_size)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: assignment
        character(len=*), intent(in) :: name

        integer :: lhs_index

        arr_size = 0
        lhs_index = assignment%target_index
        if (lhs_index <= 0 .or. lhs_index > arena%size) return
        if (.not. allocated(arena%entries(lhs_index)%node)) return

        select type (lhs => arena%entries(lhs_index)%node)
        type is (identifier_node)
            if (.not. allocated(lhs%name)) return
            if (trim(lhs%name) /= trim(name)) return
        class default
            return
        end select

        arr_size = static_array_literal_size(arena, assignment%value_index)
    end function assignment_literal_size_if_target_name

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
