submodule(semantic_analyzer) semantic_analyzer_infer_impl
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
                                   allocation_info_t, create_mono_type, &
                                   create_fun_type, create_poly_type, &
                                   create_type_var, TREAL, TVAR, TCOMPLEX, &
                                   TFUN, TARRAY, TLOGICAL, TINT, TCHAR
    use semantic_type_operations, only: get_common_type
    implicit none

    type :: infer_frame_t
        integer :: node_index = 0
        integer :: state = 0
        integer :: aux_index = 0
        logical :: leave_scope = .false.
        logical :: has_cached_type = .false.
        type(mono_type_t) :: cached_type
        type(mono_type_t), allocatable :: param_types(:)
    end type infer_frame_t

contains

    module function infer_statement_type(this, arena, stmt_index) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_index
        type(mono_type_t) :: typ

        typ = this%infer(arena, stmt_index)
    end function infer_statement_type

    module function infer_type(this, arena, expr_index) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: expr_index
        type(mono_type_t) :: typ
        type(infer_frame_t), allocatable :: stack(:)
        type(infer_frame_t) :: frame
        integer :: stack_size
        integer :: stack_capacity
        integer, parameter :: STATE_PRE = 0
        integer, parameter :: STATE_POST = 1
        integer, parameter :: STATE_ASSOC_DEFINE = 2

        typ = create_mono_type(TREAL)
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return

        stack_capacity = max(16, arena%size / 4 + 1)
        allocate (stack(stack_capacity))
        stack_size = 0

        call push_pre(expr_index)

        do while (stack_size > 0)
            frame = stack(stack_size)
            stack_size = stack_size - 1

            select case (frame%state)
            case (STATE_PRE)
                call handle_previsit(frame)
            case (STATE_POST)
                call handle_postvisit(frame)
            case (STATE_ASSOC_DEFINE)
                call handle_association(frame)
            end select
        end do

        typ = get_node_type(expr_index)

    contains

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
                                                  this%strict_mode, this%next_var_id)
                call finalize_node(node_index, node_type)
            type is (binary_op_node)
                call schedule_binary_operation(current, expr)
            type is (call_or_subscript_node)
                call schedule_call_or_subscript(current, expr)
            type is (program_node)
                call schedule_program_body(current, expr)
            type is (array_slice_node)
                node_type = infer_array_slice_type(arena, expr, &
                                                   get_node_type_with_arena)
                call finalize_node(node_index, node_type)
            type is (subroutine_call_node)
                call schedule_subroutine_call(current, expr)
            type is (function_def_node)
                call prepare_function_definition(current, expr, node_index)
            type is (subroutine_def_node)
                call prepare_subroutine_definition(current, expr, node_index)
            type is (assignment_node)
                call schedule_assignment_node(current, expr)
            type is (array_literal_node)
                call schedule_array_literal_node(current, expr)
            type is (do_loop_node)
                call prepare_do_loop_node(current, expr)
            type is (declaration_node)
                call handle_declaration(expr, node_index)
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
            if (node_index <= 0 .or. node_index > arena%size) return
            if (.not. allocated(arena%entries(node_index)%node)) return

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
            type is (array_literal_node)
                node_type = infer_array_literal_type(arena, expr, &
                                                     get_node_type_with_arena)
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
            if (parent_index <= 0 .or. parent_index > arena%size) return
            if (.not. allocated(arena%entries(parent_index)%node)) return

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

        subroutine prepare_function_definition(current, expr, node_index)
            type(infer_frame_t), intent(in) :: current
            type(function_def_node), intent(in) :: expr
            integer, intent(in) :: node_index
            type(infer_frame_t) :: post_frame
            type(mono_type_t), allocatable :: param_types(:)
            character(len=64), allocatable :: param_names(:)
            type(mono_type_t) :: return_type
            integer :: i

            call analyze_function_parameters(arena, expr, param_types, param_names, &
                                             this%scopes, this%next_var_id)
            return_type = determine_function_return_type(arena, expr, param_names, &
                                                         param_types, this%next_var_id)
            call create_function_scope(arena, expr, node_index, return_type, &
                                       this%scopes)

            call init_post_frame(current, post_frame)
            post_frame%leave_scope = .true.
            post_frame%has_cached_type = .true.
            post_frame%cached_type = return_type
            call adopt_param_types(post_frame, param_types)
            call push_frame_local(post_frame)

            if (allocated(param_names)) deallocate (param_names)
            if (allocated(expr%body_indices)) then
                do i = size(expr%body_indices), 1, -1
                    call push_child(expr%body_indices(i))
                end do
            end if
        end subroutine prepare_function_definition

        subroutine prepare_subroutine_definition(current, expr, node_index)
            type(infer_frame_t), intent(in) :: current
            type(subroutine_def_node), intent(in) :: expr
            integer, intent(in) :: node_index
            type(infer_frame_t) :: post_frame
            type(mono_type_t), allocatable :: param_types(:)
            character(len=64), allocatable :: param_names(:)
            integer :: i

            call analyze_subroutine_parameters(arena, expr, param_types, param_names, &
                                               this%scopes, this%next_var_id)
            call create_subroutine_scope(arena, expr, node_index, this%scopes)

            call init_post_frame(current, post_frame)
            post_frame%leave_scope = .true.
            call adopt_param_types(post_frame, param_types)
            call push_frame_local(post_frame)

            if (allocated(param_names)) deallocate (param_names)
            if (allocated(expr%body_indices)) then
                do i = size(expr%body_indices), 1, -1
                    call push_child(expr%body_indices(i))
                end do
            end if
        end subroutine prepare_subroutine_definition

        subroutine prepare_forall_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(forall_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            type(poly_type_t) :: int_scheme
            type(mono_type_t) :: control_type
            integer :: i

            call process_forall_node_body(expr, int_scheme, control_type)
            call this%scopes%enter_block()
            if (allocated(expr%index_names)) then
                do i = 1, size(expr%index_names)
                    call this%scopes%define(expr%index_names(i), int_scheme)
                end do
            end if

            call init_post_frame(current, post_frame)
            post_frame%leave_scope = .true.
            post_frame%has_cached_type = .true.
            post_frame%cached_type = control_type
            call push_frame_local(post_frame)

            if (allocated(expr%body_indices)) then
                do i = size(expr%body_indices), 1, -1
                    call push_child(expr%body_indices(i))
                end do
            end if
        end subroutine prepare_forall_node

        subroutine schedule_select_case_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(select_case_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%case_indices)) then
                do i = size(expr%case_indices), 1, -1
                    call push_child(expr%case_indices(i))
                end do
            end if
            call push_child(expr%selector_index)
        end subroutine schedule_select_case_node

        subroutine prepare_associate_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(associate_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            post_frame%leave_scope = .true.
            call push_frame_local(post_frame)
            call this%scopes%enter_block()

            if (allocated(expr%body_indices)) then
                do i = size(expr%body_indices), 1, -1
                    call push_child(expr%body_indices(i))
                end do
            end if

            if (allocated(expr%associations)) then
                do i = size(expr%associations), 1, -1
                    if (expr%associations(i)%expr_index <= 0) cycle
                    call init_post_frame(current, post_frame)
                    post_frame%node_index = current%node_index
                    post_frame%state = STATE_ASSOC_DEFINE
                    post_frame%aux_index = i
                    call push_frame_local(post_frame)
                    call push_child(expr%associations(i)%expr_index)
                end do
            end if
        end subroutine prepare_associate_node

        subroutine schedule_nullify_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(nullify_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%pointer_indices)) then
                do i = size(expr%pointer_indices), 1, -1
                    if (expr%pointer_indices(i) > 0) then
                        call push_child(expr%pointer_indices(i))
                    end if
                end do
            end if
        end subroutine schedule_nullify_node

        subroutine schedule_read_statement_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(read_statement_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%var_indices)) then
                do i = size(expr%var_indices), 1, -1
                    call push_child(expr%var_indices(i))
                end do
            end if
        end subroutine schedule_read_statement_node

        subroutine schedule_allocate_statement_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(allocate_statement_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%var_indices)) then
                do i = size(expr%var_indices), 1, -1
                    call push_child(expr%var_indices(i))
                end do
            end if
        end subroutine schedule_allocate_statement_node

        subroutine schedule_print_statement_node(current, expr)
            type(infer_frame_t), intent(in) :: current
            type(print_statement_node), intent(in) :: expr
            type(infer_frame_t) :: post_frame
            integer :: i

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            if (allocated(expr%expression_indices)) then
                do i = size(expr%expression_indices), 1, -1
                    call push_child(expr%expression_indices(i))
                end do
            end if
        end subroutine schedule_print_statement_node

        subroutine schedule_single_child_frame(current, child_index)
            type(infer_frame_t), intent(in) :: current
            integer, intent(in) :: child_index
            type(infer_frame_t) :: post_frame

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
            call push_child(child_index)
        end subroutine schedule_single_child_frame

        subroutine schedule_default_post(current)
            type(infer_frame_t), intent(in) :: current
            type(infer_frame_t) :: post_frame

            call init_post_frame(current, post_frame)
            call push_frame_local(post_frame)
        end subroutine schedule_default_post

        subroutine finalize_binary_operation(node_index, expr)
            integer, intent(in) :: node_index
            type(binary_op_node), intent(in) :: expr
            type(mono_type_t) :: left_t
            type(mono_type_t) :: right_t
            type(mono_type_t) :: node_type
            type(mono_type_t) :: common_t

            left_t = get_node_type(expr%left_index)
            right_t = get_node_type(expr%right_index)
            node_type = infer_binary_operation(arena, node_index, expr, &
                                               left_t, right_t)

            ! Unify operand types based on operator kind
            if (expr%operator == "//") then
                ! String concatenation: both operands must be CHARACTER
                call this%unify(left_t, create_mono_type(TCHAR))
                call this%unify(right_t, create_mono_type(TCHAR))
            else if (expr%operator == "==" .or. expr%operator == "/=" .or. &
                     expr%operator == "<" .or. expr%operator == "<=" .or. &
                     expr%operator == ">" .or. expr%operator == ">=") then
                ! Comparison operators: operands must have compatible types
                ! For string comparisons, if either operand is TCHAR, both must be
                if (left_t%kind == TCHAR .or. right_t%kind == TCHAR) then
                    call this%unify(left_t, create_mono_type(TCHAR))
                    call this%unify(right_t, create_mono_type(TCHAR))
                else
                    ! For numeric comparisons, use common type promotion
                    common_t = get_common_type(left_t, right_t)
                    call this%unify(left_t, common_t)
                    call this%unify(right_t, common_t)
                end if
            end if

            call finalize_node(node_index, node_type)
        end subroutine finalize_binary_operation

        subroutine finalize_call_or_subscript(node_index, expr)
            integer, intent(in) :: node_index
            type(call_or_subscript_node), intent(inout) :: expr
            type(mono_type_t) :: node_type

            node_type = infer_function_call_type(arena, expr, this%scopes, &
                                                 get_node_type_with_arena)
            call collect_call_signature(this%signatures, arena, expr, node_type, &
                                        node_index)
            call finalize_node(node_index, node_type)
        end subroutine finalize_call_or_subscript

        subroutine finalize_subroutine_call(node_index)
            integer, intent(in) :: node_index
            type(mono_type_t) :: node_type

            if (node_index > 0 .and. node_index <= arena%size) then
                if (allocated(arena%entries(node_index)%node)) then
                    select type (call_expr => arena%entries(node_index)%node)
                    type is (subroutine_call_node)
                        call collect_subroutine_signature(this%signatures, arena, &
                                                          call_expr, node_index)
                    end select
                end if
            end if

            node_type = create_mono_type(TVAR, var=create_type_var(0, "error"))
            call finalize_node(node_index, node_type)
        end subroutine finalize_subroutine_call

        subroutine finalize_do_loop_node(node_index)
            integer, intent(in) :: node_index
            type(mono_type_t), allocatable :: args(:)
            type(mono_type_t) :: node_type
            type(mono_type_t) :: element_type
            type(mono_type_t) :: child_type
            integer :: parent_index
            integer :: i
            logical :: is_implied_do

            element_type = create_mono_type(TINT)
            is_implied_do = .false.

            if (node_index > 0 .and. node_index <= arena%size) then
                parent_index = arena%entries(node_index)%parent_index
                if (parent_index > 0 .and. parent_index <= arena%size) then
                    if (allocated(arena%entries(parent_index)%node)) then
                        select type (parent_node => arena%entries(parent_index)%node)
                        type is (array_literal_node)
                            is_implied_do = .true.
                        end select
                    end if
                end if
            end if

            if (is_implied_do) then
                element_type%kind = 0
                element_type%size = 0
                if (allocated(arena%entries(node_index)%node)) then
                    select type (loop_node => arena%entries(node_index)%node)
                    type is (do_loop_node)
                        if (allocated(loop_node%body_indices)) then
                            do i = 1, size(loop_node%body_indices)
                                child_type = get_node_type(loop_node%body_indices(i))
                                if (child_type%kind == 0) cycle
                                if (element_type%kind == 0) then
                                    element_type = child_type
                                else
                                    element_type = &
                                        get_common_type(element_type, child_type)
                                end if
                            end do
                        end if
                    end select
                end if
                if (element_type%kind == 0) element_type = create_mono_type(TINT)
            end if

            allocate (args(1))
            args(1) = element_type
            node_type = create_mono_type(TARRAY, args=args)
            call finalize_node(node_index, node_type)
            if (allocated(args)) deallocate (args)
        end subroutine finalize_do_loop_node

        subroutine finalize_function_definition(current, expr, node_index)
            type(infer_frame_t), intent(in) :: current
            type(function_def_node), intent(in) :: expr
            integer, intent(in) :: node_index
            type(mono_type_t) :: node_type
            type(poly_type_t) :: func_scheme

            node_type = build_function_type(current)
            call finalize_node(node_index, node_type)
            if (allocated(expr%name)) then
                func_scheme = this%generalize(node_type)
                call this%scopes%define(trim(expr%name), func_scheme)
            end if
        end subroutine finalize_function_definition

        subroutine finalize_node(node_idx, raw_type)
            integer, intent(in) :: node_idx
            type(mono_type_t), intent(in) :: raw_type
            type(mono_type_t) :: final_type

            final_type = this%apply_subst_to_type(raw_type)
            if (final_type%kind == TVAR) then
                if (len_trim(final_type%var%name) == 0) then
                    final_type%var%name = "v" // int_to_str(final_type%var%id)
                end if
            end if
            call set_node_inferred_type(arena, node_idx, final_type)
        end subroutine finalize_node

        function get_node_type(idx) result(res_type)
            integer, intent(in) :: idx
            type(mono_type_t) :: res_type

            res_type = get_inferred_type_from_arena(this, arena, idx)
        end function get_node_type

        function get_node_type_with_arena(a, idx) result(res_type)
            type(ast_arena_t), intent(inout) :: a
            integer, intent(in) :: idx
            type(mono_type_t) :: res_type

            res_type = get_inferred_type_from_arena(this, a, idx)
        end function get_node_type_with_arena

        subroutine handle_declaration(node, node_index)
            type(declaration_node), intent(in) :: node
            integer, intent(in) :: node_index
            type(mono_type_t) :: decl_type
            type(poly_type_t) :: scheme
            integer :: j

            call process_declaration_variables(node, decl_type)
            scheme = this%generalize(decl_type)
            if (node%is_multi_declaration .and. allocated(node%var_names)) then
                do j = 1, size(node%var_names)
                    call this%scopes%define(node%var_names(j), scheme)
                end do
            else if (allocated(node%var_name)) then
                call this%scopes%define(node%var_name, scheme)
            end if
            call finalize_node(node_index, decl_type)
        end subroutine handle_declaration

        function build_function_type(frame) result(fun_type)
            type(infer_frame_t), intent(in) :: frame
            type(mono_type_t) :: fun_type
            type(mono_type_t) :: return_type
            type(mono_type_t), allocatable :: params(:)

            if (frame%has_cached_type) then
                return_type = frame%cached_type
            else
                return_type = create_mono_type(TREAL)
            end if

            if (allocated(frame%param_types)) then
                allocate (params(size(frame%param_types)))
                params = frame%param_types
            else
                allocate (params(0))
            end if

            if (size(params) == 0) then
                fun_type = create_fun_type(create_mono_type(TCHAR), return_type)
            else if (size(params) == 1) then
                fun_type = create_fun_type(params(1), return_type)
            else
                fun_type = create_fun_type(params(1), return_type)
            end if

            if (allocated(params)) deallocate (params)
        end function build_function_type

    end function infer_type
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

        ! Use extracted assignment processing
        call process_assignment_inference( &
            arena, assignment, assignment_index, lhs_index, expr_typ, &
                updated_expr_typ, &
            ctx%scopes, ctx%errors, ctx%strict_mode, ctx%next_var_id, &
                & ctx%parser_type_hints)

        typ = updated_expr_typ

        ! Store the actual assignment type
        call set_node_inferred_type(arena, assignment_index, typ)
    end function infer_assignment

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

end submodule semantic_analyzer_infer_impl
