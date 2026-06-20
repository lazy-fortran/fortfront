
        subroutine ensure_capacity(required)
            integer, intent(in) :: required
            type(infer_frame_t), allocatable :: new_stack(:)
            integer :: new_capacity

            if (required <= stack_capacity) return

            new_capacity = max(required, stack_capacity * 2)
            allocate (new_stack(new_capacity))
            if (stack_size > 0) new_stack(1:stack_size) = stack(1:stack_size)
            call move_alloc(new_stack, stack)
            stack_capacity = new_capacity
        end subroutine ensure_capacity

        subroutine push_frame_local(f)
            type(infer_frame_t), intent(in) :: f

            call ensure_capacity(stack_size + 1)
            stack_size = stack_size + 1
            stack(stack_size) = f
        end subroutine push_frame_local

        subroutine push_pre(index)
            integer, intent(in) :: index
            type(infer_frame_t) :: new_frame

            new_frame%node_index = index
            new_frame%state = STATE_PRE
            new_frame%aux_index = 0
            new_frame%leave_scope = .false.
            new_frame%has_cached_type = .false.
            if (allocated(new_frame%param_types)) deallocate (new_frame%param_types)
            call push_frame_local(new_frame)
        end subroutine push_pre

        subroutine push_child(index)
            integer, intent(in) :: index

            if (index <= 0) return
            call push_pre(index)
        end subroutine push_child

        subroutine handle_previsit(current)
            type(infer_frame_t), intent(in) :: current
            integer :: node_index
            type(mono_type_t) :: node_type

            node_index = current%node_index
            if (node_index <= 0 .or. node_index > arena%size) then
                call finalize_node(node_index, create_mono_type(TREAL))
                return
            end if
            if (.not. allocated(arena%entries(node_index)%node)) then
                call finalize_node(node_index, create_mono_type(TREAL))
                return
            end if

            select type (expr => arena%entries(node_index)%node)
            type is (literal_node)
                call finalize_node(node_index, infer_literal_type(expr))
            type is (complex_literal_node)
                call finalize_node(node_index, create_mono_type(TCOMPLEX))
            type is (identifier_node)
                node_type = infer_identifier_type(expr, this%scopes, this%errors, &
                                                  this%input_mode, this%next_var_id)
                call finalize_node(node_index, node_type)
            type is (binary_op_node)
                call schedule_binary_operation(current, expr)
            type is (call_or_subscript_node)
                call schedule_call_or_subscript(current, expr)
            type is (program_node)
                call schedule_program_body(current, expr)
            type is (array_slice_node)
                call schedule_array_slice_node(current, expr)
            type is (subroutine_call_node)
                call schedule_subroutine_call(current, expr)
            type is (function_def_node)
                call prepare_function_definition(current, expr, node_index)
            type is (subroutine_def_node)
                call prepare_subroutine_definition(current, expr, node_index)
            type is (assignment_node)
                call schedule_assignment_node(current, expr)
            type is (pointer_assignment_node)
                call schedule_pointer_assignment_node(current, expr)
            type is (array_literal_node)
                call schedule_array_literal_node(current, expr)
            type is (do_loop_node)
                call prepare_do_loop_node(current, expr)
            type is (declaration_node)
                call handle_declaration(expr, node_index)
            type is (derived_type_node)
                call validate_bind_c_derived_type(arena, expr, this%errors)
                call schedule_default_post(current)
            type is (if_node)
                call schedule_if_node(current, expr)
            type is (do_while_node)
                call schedule_do_while_node(current, expr)
            type is (where_node)
                call schedule_where_node(current, expr)
            type is (where_stmt_node)
                call schedule_where_stmt_node(current, expr)
            type is (forall_node)
                call prepare_forall_node(current, expr)
            type is (select_case_node)
                call schedule_select_case_node(current, expr)
            type is (associate_node)
                call prepare_associate_node(current, expr)
            type is (stop_node)
                call schedule_single_child_frame(current, expr%stop_code_index)
            type is (pause_node)
                call schedule_single_child_frame(current, expr%pause_code_index)
            type is (nullify_node)
                call schedule_nullify_node(current, expr)
            type is (read_statement_node)
                call schedule_read_statement_node(current, expr)
            type is (allocate_statement_node)
                call schedule_allocate_statement_node(current, expr)
            type is (print_statement_node)
                call schedule_print_statement_node(current, expr)
            type is (data_statement_node)
                call schedule_data_statement_node(current, expr)
            type is (cycle_node)
                call finalize_node(node_index, control_flow_type())
            type is (exit_node)
                call finalize_node(node_index, control_flow_type())
            type is (return_node)
                call finalize_node(node_index, control_flow_type())
            type is (entry_node)
                call finalize_node(node_index, control_flow_type())
            type is (continue_node)
                call finalize_node(node_index, control_flow_type())
            class default
                call schedule_default_post(current)
            end select
        end subroutine handle_previsit

        subroutine handle_postvisit(current)
            type(infer_frame_t), intent(inout) :: current
            integer :: node_index
            type(mono_type_t) :: node_type
            type(poly_type_t) :: dummy_scheme

            node_index = current%node_index
            if (current%leave_scope) call this%scopes%leave_scope()
            if (.not. arena%has_node_at(node_index)) return

            select type (expr => arena%entries(node_index)%node)
            type is (binary_op_node)
                call finalize_binary_operation(node_index, expr)
            type is (call_or_subscript_node)
                call finalize_call_or_subscript(node_index, expr)
            type is (subroutine_call_node)
                call finalize_subroutine_call(node_index)
            type is (do_loop_node)
                call finalize_do_loop_node(node_index)
            type is (function_def_node)
                call finalize_function_definition(current, expr, node_index)
            type is (assignment_node)
                node_type = infer_assignment(this, arena, expr, node_index)
                call finalize_node(node_index, node_type)
            type is (pointer_assignment_node)
                node_type = infer_pointer_assignment(this, arena, expr, node_index)
                call finalize_node(node_index, node_type)
            type is (data_statement_node)
                call finalize_data_statement_node(node_index, expr)
            type is (array_literal_node)
                call set_semantic_context_for_lookup(this)
                node_type = infer_array_literal_type(arena, expr, &
                                                     module_get_node_type_wrapper)
                call finalize_node(node_index, node_type)
            type is (array_slice_node)
                call set_semantic_context_for_lookup(this)
                node_type = infer_array_slice_type(arena, expr, &
                                                   module_get_node_type_wrapper)
                call finalize_node(node_index, node_type)
            type is (if_node)
                call process_if_node_branches(expr, node_type)
                call finalize_node(node_index, node_type)
            type is (do_while_node)
                call process_do_while_node_body(expr, node_type)
                call finalize_node(node_index, node_type)
            type is (where_node)
                call process_where_node_clauses(expr, node_type)
                call finalize_node(node_index, node_type)
            type is (where_stmt_node)
                call process_where_stmt_node(expr, node_type)
                call finalize_node(node_index, node_type)
            type is (forall_node)
                if (current%has_cached_type) then
                    node_type = current%cached_type
                else
                    call process_forall_node_body(expr, dummy_scheme, node_type)
                end if
                call finalize_node(node_index, node_type)
            type is (select_case_node)
                call process_select_case_blocks(expr, node_type)
                call finalize_node(node_index, node_type)
            type is (associate_node)
                call process_associate_node_body(expr, node_type)
                call finalize_node(node_index, node_type)
            type is (stop_node)
                call process_stop_node_code(expr, node_type)
                call finalize_node(node_index, node_type)
            type is (pause_node)
                call process_pause_node_code(expr, node_type)
                call finalize_node(node_index, node_type)
            type is (nullify_node)
                call process_nullify_node_code(expr, node_type)
                call finalize_node(node_index, node_type)
            type is (read_statement_node)
                call infer_read_statement(this, arena, expr, node_index, node_type)
                call finalize_node(node_index, node_type)
            type is (allocate_statement_node)
                call infer_allocate_statement(this, arena, expr, node_index, node_type)
                call finalize_node(node_index, node_type)
            type is (print_statement_node)
                node_type = create_mono_type(TVAR, var=create_type_var(0, "io"))
                call finalize_node(node_index, node_type)
            class default
                node_type = get_node_type(node_index)
                call finalize_node(node_index, node_type)
            end select

            if (allocated(current%param_types)) deallocate (current%param_types)
        end subroutine handle_postvisit

        subroutine handle_association(current)
            type(infer_frame_t), intent(in) :: current
            integer :: parent_index
            integer :: assoc_index
            type(mono_type_t) :: assoc_type
            type(poly_type_t) :: assoc_scheme

            parent_index = current%node_index
            assoc_index = current%aux_index
            if (.not. arena%has_node_at(parent_index)) return

            select type (assoc_node => arena%entries(parent_index)%node)
            type is (associate_node)
                if (.not. allocated(assoc_node%associations)) return
                if (assoc_index < 1 .or. assoc_index > &
                    & size(assoc_node%associations)) return
                if (assoc_node%associations(assoc_index)%expr_index <= 0) return
                assoc_type = &
                    & get_node_type(assoc_node%associations(assoc_index)%expr_index)
                assoc_scheme = create_poly_type(forall_vars=[type_var_t ::], &
                    & mono=assoc_type)
                if (allocated(assoc_node%associations(assoc_index)%name)) then
                    call this%scopes%define(assoc_node%associations(assoc_index)%name, &
                                            assoc_scheme)
                end if
            end select
        end subroutine handle_association

        subroutine init_post_frame(source, target)
            type(infer_frame_t), intent(in) :: source
            type(infer_frame_t), intent(out) :: target

            target = source
            if (allocated(target%param_types)) deallocate (target%param_types)
            target%state = STATE_POST
            target%aux_index = 0
            target%leave_scope = .false.
            target%has_cached_type = .false.
        end subroutine init_post_frame

        subroutine adopt_param_types(frame, param_types)
            type(infer_frame_t), intent(inout) :: frame
            type(mono_type_t), allocatable, intent(inout) :: param_types(:)

            if (allocated(frame%param_types)) deallocate (frame%param_types)
            if (allocated(param_types)) then
                call move_alloc(param_types, frame%param_types)
            else
                allocate (frame%param_types(0))
            end if
        end subroutine adopt_param_types

        function control_flow_type() result(t)
            type(mono_type_t) :: t

            t = create_mono_type(TVAR, var=create_type_var(0, "control"))
        end function control_flow_type

        subroutine schedule_binary_operation(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(binary_op_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            call push_child(expr%right_index)
            call push_child(expr%left_index)
        end subroutine schedule_binary_operation

        subroutine schedule_call_or_subscript(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(call_or_subscript_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%arg_indices)) then
                do i = size(expr%arg_indices), 1, -1
                    call push_child(expr%arg_indices(i))
                end do
            end if
        end subroutine schedule_call_or_subscript

        subroutine schedule_program_body(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(program_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%body_indices)) then
                do i = size(expr%body_indices), 1, -1
                    call push_child(expr%body_indices(i))
                end do
            end if
        end subroutine schedule_program_body

        subroutine schedule_subroutine_call(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(subroutine_call_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%arg_indices)) then
                do i = size(expr%arg_indices), 1, -1
                    call push_child(expr%arg_indices(i))
                end do
            end if
        end subroutine schedule_subroutine_call

        subroutine schedule_assignment_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(assignment_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            call push_child(expr%target_index)
            call push_child(expr%value_index)
        end subroutine schedule_assignment_node

        subroutine schedule_array_slice_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(array_slice_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            do i = min(expr%num_dimensions, size(expr%bounds_indices)), 1, -1
                call push_child(expr%bounds_indices(i))
            end do
            call push_child(expr%array_index)
        end subroutine schedule_array_slice_node

        subroutine schedule_pointer_assignment_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(pointer_assignment_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            call push_child(expr%pointer_index)
            call push_child(expr%target_index)
        end subroutine schedule_pointer_assignment_node

        subroutine schedule_array_literal_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(array_literal_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%element_indices)) then
                do i = size(expr%element_indices), 1, -1
                    call push_child(expr%element_indices(i))
                end do
            end if
        end subroutine schedule_array_literal_node

        subroutine prepare_do_loop_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(do_loop_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            type(poly_type_t) :: int_scheme
            type(mono_type_t) :: control_type
            integer :: i

            call process_do_loop_body(expr, int_scheme, control_type)
            if (allocated(expr%var_name)) then
                call this%scopes%define(expr%var_name, int_scheme)
            end if
            call this%scopes%enter_block()
            if (allocated(expr%var_name)) then
                call this%scopes%define(expr%var_name, int_scheme)
            end if

            call init_post_frame(current, post_frame)
            post_frame%leave_scope = .true.
            post_frame%has_cached_type = .true.
            post_frame%cached_type = control_type
            call push_frame_local(post_frame)

            if (expr%step_expr_index > 0) call push_child(expr%step_expr_index)
            if (expr%end_expr_index > 0) call push_child(expr%end_expr_index)
            if (expr%start_expr_index > 0) call push_child(expr%start_expr_index)
            if (allocated(expr%body_indices)) then
                do i = size(expr%body_indices), 1, -1
                    call push_child(expr%body_indices(i))
                end do
            end if
        end subroutine prepare_do_loop_node

        subroutine schedule_if_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(if_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%else_body_indices)) then
                do i = size(expr%else_body_indices), 1, -1
                    call push_child(expr%else_body_indices(i))
                end do
            end if
            if (allocated(expr%then_body_indices)) then
                do i = size(expr%then_body_indices), 1, -1
                    call push_child(expr%then_body_indices(i))
                end do
            end if
            call push_child(expr%condition_index)
        end subroutine schedule_if_node

        subroutine schedule_do_while_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(do_while_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%body_indices)) then
                do i = size(expr%body_indices), 1, -1
                    call push_child(expr%body_indices(i))
                end do
            end if
            call push_child(expr%condition_index)
        end subroutine schedule_do_while_node

        subroutine schedule_where_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(where_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i, j

            ! Validate WHERE body per F2018 Section 10.2.3.2
            call validate_where_construct(arena, expr, this%errors)

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%elsewhere_clauses)) then
                do i = size(expr%elsewhere_clauses), 1, -1
                    if (allocated(expr%elsewhere_clauses(i)%body_indices)) then
                        do j = size(expr%elsewhere_clauses(i)%body_indices), 1, -1
                            call push_child(expr%elsewhere_clauses(i)%body_indices(j))
                        end do
                    end if
                    if (expr%elsewhere_clauses(i)%mask_index > 0) then
                        call push_child(expr%elsewhere_clauses(i)%mask_index)
                    end if
                end do
            end if
            if (allocated(expr%where_body_indices)) then
                do i = size(expr%where_body_indices), 1, -1
                    call push_child(expr%where_body_indices(i))
                end do
            end if
            call push_child(expr%mask_expr_index)
        end subroutine schedule_where_node

        subroutine schedule_where_stmt_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(where_stmt_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            call push_child(expr%assignment_index)
            call push_child(expr%mask_expr_index)
        end subroutine schedule_where_stmt_node
