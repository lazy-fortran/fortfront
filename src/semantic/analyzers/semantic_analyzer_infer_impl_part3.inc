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
        if (rhs_size < 0) return

        prog_index = find_program_owner(arena, assignment_index)
        if (prog_index <= 0) return

        if (.not. is_allocatable_array_decl_in_program(arena, &
                                                       prog_index, name)) &
            return

        prev_size = find_prev_literal_assignment_size(arena, prog_index, &
                                                      assignment_index, name)
        if (prev_size < 0) return
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
                                   'allocate, or use infer mode', &
                                   line=assignment%line, &
                                   column=assignment%column, &
                                   end_line=assignment%line, &
                                   end_column=assignment%column + 1))
    end subroutine validate_strict_allocatable_lhs_reallocation

    subroutine get_lhs_identifier_name(arena, target_index, name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: target_index
        character(len=:), allocatable, intent(out) :: name

        if (.not. arena%has_node_at(target_index)) return

        select type (lhs => arena%entries(target_index)%node)
        type is (identifier_node)
            if (.not. allocated(lhs%name)) return
            name = trim(lhs%name)
        class default
            return
        end select
    end subroutine get_lhs_identifier_name

    integer function static_array_literal_size(arena, expr_index) &
        result(arr_size)
        use, intrinsic :: iso_fortran_env, only: int64
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index

        integer :: i
        integer(int64) :: total_size
        integer(int64) :: max_arr_size
        integer :: elem_size

        arr_size = UNKNOWN_STATIC_SIZE
        if (.not. arena%has_node_at(expr_index)) return

        select type (node => arena%entries(expr_index)%node)
        type is (array_literal_node)
            if (.not. allocated(node%element_indices)) then
                arr_size = 0
                return
            end if
            total_size = 0_int64
            max_arr_size = int(huge(arr_size), kind=int64)
            do i = 1, size(node%element_indices)
                if (node%element_indices(i) <= 0) then
                    arr_size = UNKNOWN_STATIC_SIZE
                    return
                end if
                if (node%element_indices(i) > arena%size) then
                    arr_size = UNKNOWN_STATIC_SIZE
                    return
                end if
                if (.not. arena%has_node_at(node%element_indices(i))) &
                    return
                elem_size = static_array_literal_element_size(arena, &
                                                              node%element_indices(i))
                if (elem_size < 0) then
                    arr_size = UNKNOWN_STATIC_SIZE
                    return
                end if
                if (total_size > max_arr_size - int(elem_size, kind=int64)) then
                    arr_size = UNKNOWN_STATIC_SIZE
                    return
                end if
                total_size = total_size + int(elem_size, kind=int64)
            end do
            arr_size = int(total_size, kind=kind(arr_size))
        class default
            return
        end select
    end function static_array_literal_size

    integer function static_array_literal_element_size(arena, expr_index) &
        result(elem_size)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index

        elem_size = UNKNOWN_STATIC_SIZE
        if (.not. arena%has_node_at(expr_index)) return

        select type (node => arena%entries(expr_index)%node)
        type is (do_loop_node)
            elem_size = static_implied_do_size(arena, expr_index)
        type is (array_literal_node)
            elem_size = static_array_literal_size(arena, expr_index)
        class default
            elem_size = 1
        end select
    end function static_array_literal_element_size

    integer function static_implied_do_size(arena, do_index) &
        result(implied_size)
        use, intrinsic :: iso_fortran_env, only: int64
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: do_index

        integer :: start_value
        integer :: end_value
        integer :: step_value
        integer :: i
        integer :: body_elem_size
        integer(int64) :: start64
        integer(int64) :: end64
        integer(int64) :: step64
        integer(int64) :: diff64
        integer(int64) :: iter_count
        integer(int64) :: body_size
        integer(int64) :: total_size
        integer(int64) :: max_implied_size

        implied_size = UNKNOWN_STATIC_SIZE
        if (.not. arena%has_node_at(do_index)) return

        select type (loop => arena%entries(do_index)%node)
        type is (do_loop_node)
            if (.not. get_static_integer(arena, loop%start_expr_index, &
                                         start_value)) return
            if (.not. get_static_integer(arena, loop%end_expr_index, &
                                         end_value)) return

            if (loop%step_expr_index > 0) then
                if (.not. get_static_integer(arena, loop%step_expr_index, &
                                             step_value)) return
            else
                step_value = 1
            end if
            if (step_value == 0) return

            if (step_value > 0) then
                if (start_value > end_value) then
                    implied_size = 0
                    return
                end if
            else
                if (start_value < end_value) then
                    implied_size = 0
                    return
                end if
            end if

            start64 = int(start_value, kind=int64)
            end64 = int(end_value, kind=int64)
            step64 = int(step_value, kind=int64)
            diff64 = end64 - start64
            iter_count = diff64 / step64 + 1_int64
            if (iter_count <= 0_int64) then
                implied_size = 0
                return
            end if

            if (.not. allocated(loop%body_indices)) return
            if (size(loop%body_indices) == 0) return
            body_size = 0_int64
            max_implied_size = int(huge(implied_size), kind=int64)
            do i = 1, size(loop%body_indices)
                body_elem_size = static_array_literal_element_size(arena, &
                                                                   loop%body_indices(i))
                if (body_elem_size < 0) then
                    implied_size = UNKNOWN_STATIC_SIZE
                    return
                end if
                if (body_size > max_implied_size - int(body_elem_size, kind=int64)) then
                    implied_size = UNKNOWN_STATIC_SIZE
                    return
                end if
                body_size = body_size + int(body_elem_size, kind=int64)
            end do

            if (iter_count > 0_int64) then
                if (body_size > max_implied_size / iter_count) then
                    implied_size = UNKNOWN_STATIC_SIZE
                    return
                end if
            end if
            total_size = iter_count * body_size
            if (total_size > max_implied_size) then
                implied_size = UNKNOWN_STATIC_SIZE
                return
            end if
            implied_size = int(total_size, kind=kind(implied_size))
        class default
            return
        end select
    end function static_implied_do_size

    logical function get_static_integer(arena, expr_index, value) &
        result(is_static)
        use standardizer_types, only: get_integer_literal_value, INVALID_INTEGER
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        integer, intent(out) :: value

        value = 0
        is_static = .false.
        if (.not. arena%has_node_at(expr_index)) return

        value = get_integer_literal_value(arena, expr_index)
        if (value == INVALID_INTEGER) return
        is_static = .true.
    end function get_static_integer

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
        if (.not. arena%has_node_at(prog_index)) return

        select type (prog => arena%entries(prog_index)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do i = 1, size(prog%body_indices)
                idx = prog%body_indices(i)
                if (.not. arena%has_node_at(idx)) cycle
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

        prev_size = UNKNOWN_STATIC_SIZE
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
                if (candidate_size >= 0) then
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
        if (.not. arena%has_node_at(prog_index)) return

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
        if (.not. arena%has_node_at(prog_index)) return

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

        arr_size = UNKNOWN_STATIC_SIZE
        lhs_index = assignment%target_index
        if (.not. arena%has_node_at(lhs_index)) return

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
        type(type_var_t), allocatable :: empty_vars(:)
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

                        allocate (empty_vars(0))
                        scheme = create_poly_type(forall_vars=empty_vars, &
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
        if (.not. arena%has_node_at(expr_index)) return

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
        type(type_var_t), allocatable :: empty_vars(:)

        typ = create_mono_type(TVAR, var=create_type_var(0, "io"))

        if (.not. allocated(read_stmt%var_indices)) return
        allocate (empty_vars(0))

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

                    var_scheme = create_poly_type(forall_vars=empty_vars, &
                                                  mono=var_type)
                    call ctx%scopes%define(node%name, var_scheme)

                    call set_node_inferred_type(arena, var_index, var_type)
                end if
            end select
        end do
    end subroutine infer_read_statement
