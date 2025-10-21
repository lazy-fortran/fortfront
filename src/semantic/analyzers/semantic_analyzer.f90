module semantic_analyzer
    ! Core semantic analysis - split to comply with 1000-line limit (Issue #1593)
    use type_system_unified, only: type_env_t, type_var_t, mono_type_t, poly_type_t, &
        substitution_t, allocation_info_t, create_mono_type, create_type_var, &
        create_poly_type, create_fun_type, free_type_vars, compose_substitutions, &
        occurs_check, TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN, TARRAY, TCOMPLEX, &
        TDOUBLE, TDERIVED, type_args_allocated, type_args_size, type_args_element
    use scope_manager
    use ast_arena_modern, only: ast_arena_t
    use semantic_inference_helpers, only: check_implicit_none
    use semantic_validation_utils, only: validate_array_bounds, &
        check_shape_conformance, update_identifier_type_in_arena, int_to_str
    use semantic_function_analysis, only: infer_type_from_usage_context, &
        analyze_function_parameters, determine_function_return_type, &
        create_function_scope
    use semantic_type_operations, only: generate_fresh_type_var_op, &
        apply_substitution_to_type, generalize_type_op, &
        instantiate_type_scheme_op, get_common_type
    use semantic_assignment_inference, only: process_assignment_inference, &
        ensure_var_declared_from_arena
    use semantic_binary_operations, only: infer_string_concatenation, &
        infer_comparison_operation, infer_logical_operation
    use semantic_inference_helpers, only: process_if_node_branches, &
        process_do_while_node_body, process_where_node_clauses, &
        process_where_stmt_node, process_forall_node_body, &
        process_select_case_blocks, process_associate_node_body, &
        process_stop_node_code, process_declaration_variables
    use parser_type_hooks_module, only: type_annotation_t, &
        consume_type_annotations, has_type_annotations
    use semantic_annotation_utils, only: type_from_annotation
    use semantic_literal_identifier, only: infer_literal_type, infer_identifier_type
    use semantic_binary_ops_core, only: infer_binary_operation, rewrite_operator
    use semantic_function_array, only: infer_function_call_type, &
        infer_array_slice_type, infer_array_literal_type
    use lexer_core, only: to_lower
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    use ast_nodes_core, only: literal_node, identifier_node, binary_op_node, &
        assignment_node, call_or_subscript_node, array_literal_node, program_node
    use ast_nodes_procedure, only: subroutine_call_node, function_def_node, &
        subroutine_def_node
    use ast_nodes_control, only: do_loop_node, if_node, do_while_node, where_node, &
        where_stmt_node, forall_node, select_case_node, case_block_node, &
        associate_node, association_t, cycle_node, exit_node, stop_node, &
        return_node, continue_node, elsewhere_clause_t
    use ast_nodes_data, only: intent_type_to_string, declaration_node, module_node
    use ast_nodes_bounds, only: array_spec_t, array_bounds_t, array_slice_node, &
        array_bounds_node, range_expression_node, get_array_slice_node
    use constant_transformation, only: fold_constants_in_arena
    use error_handling, only: error_collection_t, create_error_collection, result_t, &
        create_error_result, ERROR_SEMANTIC
    use semantic_context_types, only: semantic_context_base_t
    use semantic_undefined_variable_checker, only: check_undefined_variables_generic
    implicit none
    private

    public :: semantic_context_t, create_semantic_context, analyze_program, &
              has_semantic_errors

    type, extends(semantic_context_base_t) :: semantic_context_t
        type(scope_stack_t) :: scopes
        integer :: next_var_id = 0
        type(substitution_t) :: subst
        type(error_collection_t) :: errors
        logical :: strict_mode = .false.
        logical :: respect_implicit_none = .true.
        type(type_annotation_t), allocatable :: parser_type_hints(:)
    contains
        procedure :: get_context_name => semantic_get_context_name
        procedure :: clone_context => semantic_clone_context
        procedure :: infer => infer_type
        procedure :: infer_stmt => infer_statement_type
        procedure :: unify => unify_types
        procedure :: instantiate => instantiate_type_scheme
        procedure :: generalize => generalize_type
        procedure :: fresh_type_var => generate_fresh_type_var
        procedure :: apply_subst_to_type => apply_current_substitution
        procedure :: get_builtin_function_type
        procedure :: compose_with_subst
        procedure :: deep_copy => semantic_context_deep_copy
        procedure :: assign => semantic_context_assign
        procedure :: has_errors => semantic_context_has_errors
        procedure :: get_type_hint => semantic_get_type_hint

        generic :: assignment(=) => assign
    end type semantic_context_t

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

    subroutine create_semantic_context(ctx)
        type(semantic_context_t), intent(out) :: ctx
        type(poly_type_t) :: builtin_scheme
        type(mono_type_t) :: real_to_real, real_type

        ! Initialize base class components
        ctx%context_id = 1
        ctx%context_name = "semantic_context"

        ! Initialize basic components
        call create_scope_stack(ctx%scopes)
        ctx%subst%count = 0
        ctx%subst%capacity = 64
        if (allocated(ctx%subst%vars)) deallocate (ctx%subst%vars)
        if (allocated(ctx%subst%types)) deallocate (ctx%subst%types)
        allocate (ctx%subst%vars(ctx%subst%capacity))
        allocate (ctx%subst%types(ctx%subst%capacity))
        ! No parameter/temporary tracking in lean build
        ctx%errors = create_error_collection()
        ctx%next_var_id = 1  ! Start from 1 (main branch compatibility)
        ctx%respect_implicit_none = .true.

        ! Create real -> real type for math functions
        real_type = create_mono_type(TREAL)
        real_to_real = create_fun_type(real_type, real_type)

        ! Create polymorphic type scheme (no type variables to generalize)
        builtin_scheme = create_poly_type(forall_vars=[type_var_t ::], &
                                          mono=real_to_real)

        call ctx%scopes%define("exp", builtin_scheme)

        if (has_type_annotations()) then
            call consume_type_annotations(ctx%parser_type_hints)
        else
            allocate (ctx%parser_type_hints(0))
        end if
        call ctx%scopes%define("log", builtin_scheme)
        call ctx%scopes%define("abs", builtin_scheme)
    end subroutine create_semantic_context

    subroutine analyze_program(ctx, arena, root_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index

        if (root_index <= 0 .or. root_index > arena%size) return
        if (.not. allocated(arena%entries(root_index)%node)) return

        select type (ast => arena%entries(root_index)%node)
        type is (program_node)
            ! Only enable strict mode when implicit none is present
            if (ctx%respect_implicit_none .and. .not. ctx%strict_mode) then
                ctx%strict_mode = check_implicit_none(arena, ast)
            end if
            call analyze_program_node_arena(ctx, arena, ast, root_index)
        type is (module_node)
            return  ! Skip module analysis
        class default
            call infer_and_store_type(ctx, arena, root_index)
        end select

        call fold_constants_in_arena(arena)
    end subroutine analyze_program

    subroutine analyze_program_node_arena(ctx, arena, prog, prog_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: prog_index
        integer :: i

        ! Prepass: define all declared variables in scope before inference
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. &
                    prog%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        select type (node => arena%entries(prog%body_indices(i))%node)
                        type is (declaration_node)
                            block
                                type(mono_type_t) :: decl_type
                                type(poly_type_t) :: scheme
                                type(type_annotation_t) :: hint
                                integer :: j

                                if (ctx%get_type_hint(prog%body_indices(i), hint)) then
                                    call type_from_annotation(hint, decl_type)
                                else
                                    call process_declaration_variables(node, decl_type)
                                end if

                                scheme = ctx%generalize(decl_type)
                                if (node%is_multi_declaration .and. &
                                    allocated(node%var_names)) then
                                    do j = 1, size(node%var_names)
                                        call &
                                            ctx%scopes%define(node%var_names(j), &
                                                              scheme)
                                    end do
                                else if (allocated(node%var_name)) then
                                    call ctx%scopes%define(node%var_name, scheme)
                                end if
                            end block
                        class default
                            continue
                        end select
                    end if
                end if
            end do
        end if

        ! Main inference over statements
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. &
                    prog%body_indices(i) <= arena%size) then
                    call infer_and_store_type(ctx, arena, prog%body_indices(i))
                end if
            end do
        end if
        call check_undefined_variables_generic(ctx%scopes, ctx%errors, &
                                               ctx%strict_mode, &
                                               arena, prog_index)
    end subroutine analyze_program_node_arena

    subroutine infer_and_store_type(ctx, arena, node_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(mono_type_t) :: inferred

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        inferred = ctx%infer_stmt(arena, node_index)

        ! Direct assignment without allocation since inferred_type is not allocatable
    end subroutine infer_and_store_type

    function infer_statement_type(this, arena, stmt_index) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_index
        type(mono_type_t) :: typ

        typ = this%infer(arena, stmt_index)
    end function infer_statement_type

    function infer_type(this, arena, expr_index) result(typ)
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
            type(infer_frame_t) :: post_frame
            integer :: node_index
            integer :: i
            type(mono_type_t) :: local_type
            type(poly_type_t) :: int_scheme
            type(mono_type_t), allocatable :: param_types(:)
            character(len=64), allocatable :: param_names(:)
            type(mono_type_t) :: return_type
            type(mono_type_t) :: control_type

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
                local_type = infer_literal_type(expr)
                call finalize_node(node_index, local_type)
            type is (identifier_node)
                local_type = infer_identifier_type(expr, this%scopes, this%errors, &
                                                    this%strict_mode, this%next_var_id)
                call finalize_node(node_index, local_type)
            type is (binary_op_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                call push_child(expr%right_index)
                call push_child(expr%left_index)
            type is (call_or_subscript_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                if (allocated(expr%arg_indices)) then
                    do i = size(expr%arg_indices), 1, -1
                        call push_child(expr%arg_indices(i))
                    end do
                end if
            type is (program_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                if (allocated(expr%body_indices)) then
                    do i = size(expr%body_indices), 1, -1
                        call push_child(expr%body_indices(i))
                    end do
                end if
            type is (array_slice_node)
                local_type = infer_array_slice_type(arena, expr, &
                                                     get_node_type_with_arena)
                call finalize_node(node_index, local_type)
            type is (subroutine_call_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                if (allocated(expr%arg_indices)) then
                    do i = size(expr%arg_indices), 1, -1
                        call push_child(expr%arg_indices(i))
                    end do
                end if
            type is (function_def_node)
                call analyze_function_parameters( &
                    arena, expr, param_types, param_names, this%scopes, &
                    this%next_var_id)
                return_type = determine_function_return_type( &
                              arena, expr, param_names, param_types, this%next_var_id)
                call create_function_scope( &
                    arena, expr, node_index, return_type, this%scopes)
                post_frame = current
                post_frame%state = STATE_POST
                post_frame%leave_scope = .true.
                post_frame%has_cached_type = .true.
                post_frame%cached_type = return_type
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                if (allocated(param_types)) then
                    post_frame%param_types = param_types
                else
                    allocate (post_frame%param_types(0))
                end if
                call push_frame_local(post_frame)
                if (allocated(param_types)) deallocate (param_types)
                if (allocated(param_names)) deallocate (param_names)
                if (allocated(expr%body_indices)) then
                    do i = size(expr%body_indices), 1, -1
                        call push_child(expr%body_indices(i))
                    end do
                end if
            type is (assignment_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                call push_child(expr%target_index)
                call push_child(expr%value_index)
            type is (array_literal_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                if (allocated(expr%element_indices)) then
                    do i = size(expr%element_indices), 1, -1
                        call push_child(expr%element_indices(i))
                    end do
                end if
            type is (do_loop_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                if (expr%step_expr_index > 0) call push_child(expr%step_expr_index)
                if (expr%end_expr_index > 0) call push_child(expr%end_expr_index)
                if (expr%start_expr_index > 0) call push_child(expr%start_expr_index)
                if (allocated(expr%body_indices)) then
                    do i = size(expr%body_indices), 1, -1
                        call push_child(expr%body_indices(i))
                    end do
                end if
            type is (declaration_node)
                call handle_declaration(expr, node_index)
            type is (if_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                if (allocated(expr%then_body_indices)) then
                    do i = size(expr%then_body_indices), 1, -1
                        call push_child(expr%then_body_indices(i))
                    end do
                end if
                call push_child(expr%condition_index)
            type is (do_while_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                if (allocated(expr%body_indices)) then
                    do i = size(expr%body_indices), 1, -1
                        call push_child(expr%body_indices(i))
                    end do
                end if
                call push_child(expr%condition_index)
            type is (where_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                if (allocated(expr%where_body_indices)) then
                    do i = size(expr%where_body_indices), 1, -1
                        call push_child(expr%where_body_indices(i))
                    end do
                end if
                call push_child(expr%mask_expr_index)
            type is (where_stmt_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                call push_child(expr%assignment_index)
                call push_child(expr%mask_expr_index)
            type is (forall_node)
                call process_forall_node_body(expr, int_scheme, control_type)
                call this%scopes%enter_block()
                if (allocated(expr%index_names)) then
                    do i = 1, size(expr%index_names)
                        call this%scopes%define(expr%index_names(i), int_scheme)
                    end do
                end if
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                post_frame%state = STATE_POST
                post_frame%leave_scope = .true.
                post_frame%has_cached_type = .true.
                post_frame%cached_type = control_type
                call push_frame_local(post_frame)
                if (allocated(expr%body_indices)) then
                    do i = size(expr%body_indices), 1, -1
                        call push_child(expr%body_indices(i))
                    end do
                end if
            type is (select_case_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                if (allocated(expr%case_indices)) then
                    do i = size(expr%case_indices), 1, -1
                        call push_child(expr%case_indices(i))
                    end do
                end if
                call push_child(expr%selector_index)
            type is (associate_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                post_frame%state = STATE_POST
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
                        if (expr%associations(i)%expr_index > 0) then
                            post_frame = current
                            if (allocated(post_frame%param_types)) then
                                deallocate (post_frame%param_types)
                            end if
                            post_frame%node_index = current%node_index
                            post_frame%state = STATE_ASSOC_DEFINE
                            post_frame%aux_index = i
                            post_frame%leave_scope = .false.
                            post_frame%has_cached_type = .false.
                            call push_frame_local(post_frame)
                            call push_child(expr%associations(i)%expr_index)
                        end if
                    end do
                end if
            type is (stop_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                call push_child(expr%stop_code_index)
            type is (cycle_node)
                local_type = create_mono_type(TVAR, var=create_type_var(0, "control"))
                call finalize_node(node_index, local_type)
            type is (exit_node)
                local_type = create_mono_type(TVAR, var=create_type_var(0, "control"))
                call finalize_node(node_index, local_type)
            type is (return_node)
                local_type = create_mono_type(TVAR, var=create_type_var(0, "control"))
                call finalize_node(node_index, local_type)
            type is (continue_node)
                local_type = create_mono_type(TVAR, var=create_type_var(0, "control"))
                call finalize_node(node_index, local_type)
            class default
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate &
                    (post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
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
                block
                    type(mono_type_t) :: left_t, right_t
                    left_t = get_node_type(expr%left_index)
                    right_t = get_node_type(expr%right_index)
                    node_type = infer_binary_operation(arena, node_index, expr, &
                                                        left_t, right_t)
                    call this%unify(left_t, create_mono_type(TCHAR))
                    call this%unify(right_t, create_mono_type(TCHAR))
                end block
                call finalize_node(node_index, node_type)
            type is (call_or_subscript_node)
                node_type = infer_function_call_type(arena, expr, this%scopes, &
                                                      get_node_type_with_arena)
                call finalize_node(node_index, node_type)
            type is (subroutine_call_node)
                node_type = create_mono_type(TVAR, var=create_type_var(0, "error"))
                call finalize_node(node_index, node_type)
            type is (do_loop_node)
                block
                    type(mono_type_t), allocatable :: args(:)
                    allocate (args(1))
                    args(1) = create_mono_type(TINT)
                    node_type = create_mono_type(TARRAY, args=args)
                end block
                call finalize_node(node_index, node_type)
            type is (function_def_node)
                node_type = build_function_type(current)
                call finalize_node(node_index, node_type)
                if (allocated(expr%name)) then
                    block
                        type(poly_type_t) :: func_scheme
                        func_scheme = this%generalize(node_type)
                        call this%scopes%define(trim(expr%name), func_scheme)
                    end block
                end if
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

    subroutine set_node_inferred_type(arena, index, typ)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: index
        type(mono_type_t), intent(in) :: typ

        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return
        arena%entries(index)%node%inferred_type = typ
    end subroutine set_node_inferred_type

    function get_inferred_type_from_arena(ctx, arena, index) result(typ)
        class(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: index
        type(mono_type_t) :: typ

        typ = create_mono_type(TREAL)
        if (index <= 0 .or. index > arena%size) return
        if (.not. allocated(arena%entries(index)%node)) return

        typ = ctx%apply_subst_to_type(arena%entries(index)%node%inferred_type)
        if (typ%kind == TVAR) then
            if (len_trim(typ%var%name) == 0) typ%var%name = "v" // &
                                                            int_to_str(typ%var%id)
        end if
        arena%entries(index)%node%inferred_type = typ
    end function get_inferred_type_from_arena

    subroutine unify_types(this, t1, t2)
        class(semantic_context_t), intent(inout) :: this
        type(mono_type_t), intent(in) :: t1, t2

        ! Simplified unification
    end subroutine unify_types

    function instantiate_type_scheme(this, scheme) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(poly_type_t), intent(in) :: scheme
        type(mono_type_t) :: typ

        typ = instantiate_type_scheme_op(scheme, this%next_var_id)
    end function instantiate_type_scheme

    function generalize_type(this, typ) result(scheme)
        class(semantic_context_t), intent(in) :: this
        type(mono_type_t), intent(in) :: typ
        type(poly_type_t) :: scheme

        scheme = generalize_type_op(typ)
    end function generalize_type

    function generate_fresh_type_var(this) result(tv)
        class(semantic_context_t), intent(inout) :: this
        type(type_var_t) :: tv

        tv = generate_fresh_type_var_op(this%next_var_id)
    end function generate_fresh_type_var

    function apply_current_substitution(this, typ) result(result_type)
        class(semantic_context_t), intent(in) :: this
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t) :: result_type

        result_type = apply_substitution_to_type(typ, this%subst)
    end function apply_current_substitution

    function get_builtin_function_type(this, name) result(typ)
        class(semantic_context_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(mono_type_t) :: typ
        type(poly_type_t), allocatable :: scheme

        call this%scopes%lookup(name, scheme)
        if (allocated(scheme)) then
            typ = this%instantiate(scheme)
        else
            ! Default to real -> real
            typ = create_fun_type(create_mono_type(TREAL), create_mono_type(TREAL))
        end if
    end function get_builtin_function_type

    subroutine compose_with_subst(this, new_subst)
        class(semantic_context_t), intent(inout) :: this
        type(substitution_t), intent(in) :: new_subst

        this%subst = compose_substitutions(new_subst, this%subst)
    end subroutine compose_with_subst

    subroutine semantic_context_deep_copy(this, copy)
        class(semantic_context_t), intent(in) :: this
        type(semantic_context_t), intent(out) :: copy

        copy%scopes = this%scopes
        copy%next_var_id = this%next_var_id
        copy%subst = this%subst
        ! Tracking fields removed in lean build
        copy%errors = this%errors
        copy%strict_mode = this%strict_mode
        copy%respect_implicit_none = this%respect_implicit_none
    end subroutine semantic_context_deep_copy

    subroutine semantic_context_assign(lhs, rhs)
        class(semantic_context_t), intent(inout) :: lhs
        type(semantic_context_t), intent(in) :: rhs

        lhs%scopes = rhs%scopes
        lhs%next_var_id = rhs%next_var_id
        lhs%subst = rhs%subst
        ! Tracking fields removed in lean build
        lhs%errors = rhs%errors
        lhs%strict_mode = rhs%strict_mode
        lhs%respect_implicit_none = rhs%respect_implicit_none
    end subroutine semantic_context_assign

    function semantic_context_has_errors(this) result(has_errors)
        class(semantic_context_t), intent(in) :: this
        logical :: has_errors
        has_errors = this%errors%has_errors()
    end function semantic_context_has_errors

    logical function semantic_get_type_hint(this, decl_index, annotation)
        class(semantic_context_t), intent(in) :: this
        integer, intent(in) :: decl_index
        type(type_annotation_t), intent(out) :: annotation
        integer :: i

        semantic_get_type_hint = .false.
        if (.not. allocated(this%parser_type_hints)) return
        if (size(this%parser_type_hints) == 0) return

        do i = 1, size(this%parser_type_hints)
            if (this%parser_type_hints(i)%decl_index == decl_index) then
                annotation = this%parser_type_hints(i)
                semantic_get_type_hint = .true.
                return
            end if
        end do
    end function semantic_get_type_hint



    function infer_assignment(ctx, arena, assignment, assignment_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assignment
        integer, intent(in) :: assignment_index
        type(mono_type_t) :: typ
        type(mono_type_t) :: expr_typ, updated_expr_typ
        integer :: lhs_index
        lhs_index = assignment%target_index
        expr_typ = get_inferred_type_from_arena(ctx, arena, assignment%value_index)
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

    function has_semantic_errors(ctx) result(has_errors)
        type(semantic_context_t), intent(in) :: ctx
        logical :: has_errors
        has_errors = ctx%errors%has_errors()
    end function has_semantic_errors



    function semantic_get_context_name(this) result(name)
        class(semantic_context_t), intent(in) :: this
        character(:), allocatable :: name
        name = "semantic_context"
    end function semantic_get_context_name

    function semantic_clone_context(this) result(cloned)
        class(semantic_context_t), intent(in) :: this
        class(semantic_context_base_t), allocatable :: cloned
        type(semantic_context_t) :: temp_context
        temp_context%context_id = this%context_id
        temp_context%context_name = this%context_name
        temp_context%scopes = this%scopes
        temp_context%next_var_id = this%next_var_id
        temp_context%subst = this%subst
        ! Tracking fields removed in lean build
        temp_context%errors = this%errors
        temp_context%strict_mode = this%strict_mode

        allocate (cloned, source=temp_context)
    end function semantic_clone_context

end module semantic_analyzer
