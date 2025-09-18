module semantic_analyzer
    ! Main semantic analysis module - refactored for maintainability
    ! Architectural compliance: <1000 lines (Issue #1016)
    use type_system_unified, only: type_env_t, type_var_t, mono_type_t, poly_type_t, &
                                   substitution_t, allocation_info_t, &
                                   create_mono_type, create_type_var, &
                                   create_poly_type, create_fun_type, free_type_vars, &
                                   compose_substitutions, occurs_check, &
                                   TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN, TARRAY, &
                                   TCOMPLEX, TDOUBLE, TDERIVED, &
                                   type_args_allocated, type_args_size, type_args_element
    use scope_manager
    use ast_core
    use semantic_inference_helpers, only: check_implicit_none
    use semantic_validation_utils, only: validate_array_bounds, check_shape_conformance, &
                                          update_identifier_type_in_arena, int_to_str
    use semantic_function_analysis, only: infer_type_from_usage_context, &
                                          analyze_function_parameters, &
                                          determine_function_return_type, &
                                          create_function_scope
    use semantic_type_operations, only: generate_fresh_type_var_op, &
                                        apply_substitution_to_type, &
                                        generalize_type_op, &
                                        instantiate_type_scheme_op, &
                                        get_common_type
    use semantic_assignment_inference, only: process_assignment_inference
    use semantic_binary_operations, only: infer_string_concatenation, &
                                          infer_comparison_operation, &
                                          infer_logical_operation
    use semantic_inference_helpers, only: process_if_node_branches, process_do_while_node_body, &
                                           process_where_node_clauses, process_where_stmt_node, &
                                           process_forall_node_body, process_select_case_blocks, &
                                           process_associate_node_body, process_stop_node_code, &
                                           process_declaration_variables
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    use ast_nodes_core, only: literal_node, identifier_node, binary_op_node, &
                               assignment_node, call_or_subscript_node, &
                               array_literal_node, program_node
    use ast_nodes_procedure, only: subroutine_call_node, function_def_node, subroutine_def_node
    use ast_nodes_control, only: do_loop_node, if_node, do_while_node, &
                                  where_node, where_stmt_node, forall_node, &
                                  select_case_node, case_block_node, &
                                  associate_node, association_t, cycle_node, exit_node, &
                                  stop_node, return_node, elsewhere_clause_t
    use ast_nodes_data, only: intent_type_to_string, declaration_node, module_node
    use ast_nodes_bounds, only: array_spec_t, array_bounds_t, array_slice_node, &
                                range_expression_node, get_array_slice_node
    ! Removed legacy parameter/temp trackers for lean build
    use constant_transformation, only: fold_constants_in_arena
    use error_handling, only: error_collection_t, create_error_collection, result_t, &
                               create_error_result, ERROR_SEMANTIC
    use semantic_context_types, only: semantic_context_base_t
    use semantic_undefined_variable_checker, only: check_undefined_variables_generic
    implicit none
    private

    public :: semantic_context_t, create_semantic_context
    public :: analyze_program
    public :: validate_array_bounds, check_shape_conformance
    public :: has_semantic_errors

    ! Semantic analysis context
    type, extends(semantic_context_base_t) :: semantic_context_t
        type(scope_stack_t) :: scopes  ! Hierarchical scope management
        integer :: next_var_id = 0
        type(substitution_t) :: subst
        type(error_collection_t) :: errors  ! Collect semantic errors
        logical :: strict_mode = .false.  ! Default lazy mode; callers may enable strict
        logical :: respect_implicit_none = .true.
    contains
        ! Implement required abstract procedures from semantic_context_base_t FIRST
        procedure :: get_context_name => semantic_get_context_name
        procedure :: clone_context => semantic_clone_context
        ! Other procedures
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
        procedure :: validate_bounds => validate_array_access_bounds
        procedure :: check_conformance => check_array_shape_conformance
        procedure :: has_errors => semantic_context_has_errors

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

    ! Create a new semantic context with builtin functions (avoid large return-by-value)
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
        if (allocated(ctx%subst%vars)) deallocate(ctx%subst%vars)
        if (allocated(ctx%subst%types)) deallocate(ctx%subst%types)
        allocate(ctx%subst%vars(ctx%subst%capacity))
        allocate(ctx%subst%types(ctx%subst%capacity))
        ! No parameter/temporary tracking in lean build
        ctx%errors = create_error_collection()
        ctx%next_var_id = 1  ! Start from 1 (main branch compatibility)
        ctx%respect_implicit_none = .true.
        
        ! Create real -> real type for math functions
        real_type = create_mono_type(TREAL)
        real_to_real = create_fun_type(real_type, real_type)
        
        ! Create polymorphic type scheme (no type variables to generalize)
        builtin_scheme = create_poly_type(forall_vars=[type_var_t::], mono=real_to_real)
        
        ! Add common math functions to global scope
        call ctx%scopes%define("sin", builtin_scheme)
        call ctx%scopes%define("cos", builtin_scheme)
        call ctx%scopes%define("tan", builtin_scheme)
        call ctx%scopes%define("sqrt", builtin_scheme)
        call ctx%scopes%define("exp", builtin_scheme)
        call ctx%scopes%define("log", builtin_scheme)
        call ctx%scopes%define("abs", builtin_scheme)
    end subroutine create_semantic_context

    ! Main entry point: analyze entire program
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

    ! Analyze a program node with arena-based AST  
    subroutine analyze_program_node_arena(ctx, arena, prog, prog_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: prog_index
        integer :: i

        ! Prepass: define all declared variables in scope before inference
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        select type (node => arena%entries(prog%body_indices(i))%node)
                        type is (declaration_node)
                            block
                                type(mono_type_t) :: decl_type
                                type(poly_type_t) :: scheme
                                integer :: j
                                call process_declaration_variables(node, decl_type)
                                scheme = ctx%generalize(decl_type)
                                if (node%is_multi_declaration .and. allocated(node%var_names)) then
                                    do j = 1, size(node%var_names)
                                        call ctx%scopes%define(node%var_names(j), scheme)
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
                if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                    call infer_and_store_type(ctx, arena, prog%body_indices(i))
                end if
            end do
        end if
        call check_undefined_variables_generic(ctx%scopes, ctx%errors, ctx%strict_mode, arena, prog_index)
    end subroutine analyze_program_node_arena

    ! Infer type and store in AST node
    subroutine infer_and_store_type(ctx, arena, node_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(mono_type_t) :: inferred

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        inferred = ctx%infer_stmt(arena, node_index)

        ! Direct assignment without allocation since inferred_type is not allocatable
        call set_node_inferred_type(arena, node_index, inferred)
    end subroutine infer_and_store_type

    ! Simplified type inference entry point
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
        allocate(stack(stack_capacity))
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
            allocate(new_stack(new_capacity))
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
            if (allocated(new_frame%param_types)) deallocate(new_frame%param_types)
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
                local_type = infer_literal(this, expr)
                call finalize_node(node_index, local_type)
            type is (identifier_node)
                local_type = infer_identifier(this, expr)
                call finalize_node(node_index, local_type)
            type is (binary_op_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate(post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                call push_child(expr%right_index)
                call push_child(expr%left_index)
            type is (call_or_subscript_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate(post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                if (allocated(expr%arg_indices)) then
                    do i = size(expr%arg_indices), 1, -1
                        call push_child(expr%arg_indices(i))
                    end do
                end if
            type is (array_slice_node)
                local_type = infer_array_slice(this, arena, expr)
                call finalize_node(node_index, local_type)
            type is (subroutine_call_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate(post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                if (allocated(expr%arg_indices)) then
                    do i = size(expr%arg_indices), 1, -1
                        call push_child(expr%arg_indices(i))
                    end do
                end if
            type is (function_def_node)
                call analyze_function_parameters(arena, expr, param_types, this%scopes, this%next_var_id)
                return_type = determine_function_return_type(expr, this%next_var_id)
                call create_function_scope(expr, return_type, this%scopes)
                post_frame = current
                post_frame%state = STATE_POST
                post_frame%leave_scope = .true.
                post_frame%has_cached_type = .true.
                post_frame%cached_type = return_type
                if (allocated(post_frame%param_types)) deallocate(post_frame%param_types)
                if (allocated(param_types)) then
                    post_frame%param_types = param_types
                else
                    allocate(post_frame%param_types(0))
                end if
                call push_frame_local(post_frame)
                if (allocated(param_types)) deallocate(param_types)
                if (allocated(expr%body_indices)) then
                    do i = size(expr%body_indices), 1, -1
                        call push_child(expr%body_indices(i))
                    end do
                end if
            type is (assignment_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate(post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                call push_child(expr%target_index)
                call push_child(expr%value_index)
            type is (array_literal_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate(post_frame%param_types)
                post_frame%state = STATE_POST
                call push_frame_local(post_frame)
                if (allocated(expr%element_indices)) then
                    do i = size(expr%element_indices), 1, -1
                        call push_child(expr%element_indices(i))
                    end do
                end if
            type is (do_loop_node)
                local_type = infer_implied_do_loop(this, arena, expr, node_index)
                call finalize_node(node_index, local_type)
            type is (declaration_node)
                call handle_declaration(expr, node_index)
            type is (if_node)
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate(post_frame%param_types)
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
                if (allocated(post_frame%param_types)) deallocate(post_frame%param_types)
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
                if (allocated(post_frame%param_types)) deallocate(post_frame%param_types)
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
                if (allocated(post_frame%param_types)) deallocate(post_frame%param_types)
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
                if (allocated(post_frame%param_types)) deallocate(post_frame%param_types)
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
                if (allocated(post_frame%param_types)) deallocate(post_frame%param_types)
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
                if (allocated(post_frame%param_types)) deallocate(post_frame%param_types)
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
                            if (allocated(post_frame%param_types)) deallocate(post_frame%param_types)
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
                if (allocated(post_frame%param_types)) deallocate(post_frame%param_types)
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
            class default
                post_frame = current
                if (allocated(post_frame%param_types)) deallocate(post_frame%param_types)
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
                node_type = infer_binary_op(this, arena, expr, node_index)
                call finalize_node(node_index, node_type)
            type is (call_or_subscript_node)
                node_type = infer_function_call(this, arena, expr)
                call finalize_node(node_index, node_type)
            type is (subroutine_call_node)
                node_type = create_mono_type(TVAR, var=create_type_var(0, "error"))
                call finalize_node(node_index, node_type)
            type is (function_def_node)
                node_type = build_function_type(current)
                call finalize_node(node_index, node_type)
            type is (assignment_node)
                node_type = infer_assignment(this, arena, expr, node_index)
                call finalize_node(node_index, node_type)
            type is (array_literal_node)
                node_type = infer_array_literal(this, arena, expr, node_index)
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

            if (allocated(current%param_types)) deallocate(current%param_types)
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
                if (assoc_index < 1 .or. assoc_index > size(assoc_node%associations)) return
                if (assoc_node%associations(assoc_index)%expr_index <= 0) return
                assoc_type = get_node_type(assoc_node%associations(assoc_index)%expr_index)
                assoc_scheme = create_poly_type(forall_vars=[type_var_t::], mono=assoc_type)
                if (allocated(assoc_node%associations(assoc_index)%name)) then
                    call this%scopes%define(assoc_node%associations(assoc_index)%name, assoc_scheme)
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
                    final_type%var%name = "v"//int_to_str(final_type%var%id)
                end if
            end if
            call set_node_inferred_type(arena, node_idx, final_type)
        end subroutine finalize_node

        function get_node_type(idx) result(res_type)
            integer, intent(in) :: idx
            type(mono_type_t) :: res_type

            res_type = get_inferred_type_from_arena(this, arena, idx)
        end function get_node_type

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
                allocate(params(size(frame%param_types)))
                params = frame%param_types
            else
                allocate(params(0))
            end if

            if (size(params) == 0) then
                fun_type = create_fun_type(create_mono_type(TCHAR), return_type)
            else if (size(params) == 1) then
                fun_type = create_fun_type(params(1), return_type)
            else
                fun_type = create_fun_type(params(1), return_type)
            end if

            if (allocated(params)) deallocate(params)
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
            if (len_trim(typ%var%name) == 0) typ%var%name = "v"//int_to_str(typ%var%id)
        end if
        arena%entries(index)%node%inferred_type = typ
    end function get_inferred_type_from_arena


    ! Type unification (simplified)
    subroutine unify_types(this, t1, t2)
        class(semantic_context_t), intent(inout) :: this
        type(mono_type_t), intent(in) :: t1, t2

        ! Simplified unification
    end subroutine unify_types

    ! Instantiate type scheme (using extracted module)
    function instantiate_type_scheme(this, scheme) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(poly_type_t), intent(in) :: scheme
        type(mono_type_t) :: typ

        typ = instantiate_type_scheme_op(scheme, this%next_var_id)
    end function instantiate_type_scheme

    ! Generalize type (using extracted module)
    function generalize_type(this, typ) result(scheme)
        class(semantic_context_t), intent(in) :: this
        type(mono_type_t), intent(in) :: typ
        type(poly_type_t) :: scheme
        
        scheme = generalize_type_op(typ)
    end function generalize_type

    ! Generate fresh type variable (using extracted module)
    function generate_fresh_type_var(this) result(tv)
        class(semantic_context_t), intent(inout) :: this
        type(type_var_t) :: tv

        tv = generate_fresh_type_var_op(this%next_var_id)
    end function generate_fresh_type_var

    ! Apply current substitution to type (using extracted module)
    function apply_current_substitution(this, typ) result(result_type)
        class(semantic_context_t), intent(in) :: this
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t) :: result_type

        result_type = apply_substitution_to_type(typ, this%subst)
    end function apply_current_substitution

    ! Get builtin function type
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

    ! Compose with substitution
    subroutine compose_with_subst(this, new_subst)
        class(semantic_context_t), intent(inout) :: this
        type(substitution_t), intent(in) :: new_subst

        this%subst = compose_substitutions(new_subst, this%subst)
    end subroutine compose_with_subst

    ! Deep copy (use subroutine to avoid large return-by-value on stack)
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


    ! Enhanced function definition semantic analysis (simplified main version)
    function infer_function_definition(ctx, arena, func_node, func_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(in) :: func_node
        integer, intent(in) :: func_index
        type(mono_type_t) :: typ
        type(mono_type_t), allocatable :: param_types(:)
        type(mono_type_t) :: return_type
        integer :: i
        
        ! Use extracted function analysis modules
        call analyze_function_parameters(arena, func_node, param_types, ctx%scopes, ctx%next_var_id)
        return_type = determine_function_return_type(func_node, ctx%next_var_id)
        call create_function_scope(func_node, return_type, ctx%scopes)
        
        ! Analyze function body with parameters and result in scope
        if (allocated(func_node%body_indices)) then
            do i = 1, size(func_node%body_indices)
                typ = get_inferred_type_from_arena(ctx, arena, func_node%body_indices(i))
            end do
        end if
        
        ! Pop function scope
        call ctx%scopes%leave_scope()
        
        ! Create function type
        if (size(param_types) == 0) then
            typ = create_fun_type(create_mono_type(TCHAR), return_type)  ! No params
        else if (size(param_types) == 1) then
            typ = create_fun_type(param_types(1), return_type)  ! Single param
        else
            ! Multiple parameters - create tuple type or simplified signature
            typ = create_fun_type(param_types(1), return_type)  ! Simplified for now
        end if
    end function infer_function_definition

    ! Assignment operator
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

    ! Array bounds validation (simplified)
    subroutine validate_array_access_bounds(ctx, arena, slice_node)
        class(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(array_slice_node), intent(in) :: slice_node
        
        ! Simplified bounds validation
    end subroutine validate_array_access_bounds

    ! Array shape conformance checking
    subroutine check_array_shape_conformance(ctx, lhs_type, rhs_type, is_conformant)
        class(semantic_context_t), intent(inout) :: ctx
        type(mono_type_t), intent(in) :: lhs_type, rhs_type
        logical, intent(out) :: is_conformant

        ! Simplified conformance
        is_conformant = (lhs_type%kind == TARRAY .and. rhs_type%kind == TARRAY)
    end subroutine check_array_shape_conformance

    function semantic_context_has_errors(this) result(has_errors)
        class(semantic_context_t), intent(in) :: this
        logical :: has_errors
        has_errors = this%errors%has_errors()
    end function semantic_context_has_errors



    ! Infer type of literal
    function infer_literal(ctx, lit) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(literal_node), intent(in) :: lit
        type(mono_type_t) :: typ

        ! Use literal_kind for integer comparisons
        select case (lit%literal_kind)
        case (LITERAL_INTEGER)
            typ = create_mono_type(TINT)
        case (LITERAL_REAL)
            typ = create_mono_type(TREAL)
        case (LITERAL_STRING)
            ! Calculate character length from string literal (excluding quotes)
            if (allocated(lit%value) .and. len(lit%value) >= 2) then
                typ = create_mono_type(TCHAR, char_size=len(lit%value) - 2)
            else
                typ = create_mono_type(TCHAR, char_size=0)
            end if
        case (LITERAL_LOGICAL)
            typ = create_mono_type(TLOGICAL)
        case default
            typ = create_mono_type(TREAL)
        end select
    end function infer_literal

    ! Infer type of identifier
    function infer_identifier(ctx, ident) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(identifier_node), intent(in) :: ident
        type(mono_type_t) :: typ
        type(poly_type_t), allocatable :: scheme
        type(result_t) :: error_result

        ! Safety check: ensure identifier name is allocated and not empty
        if (.not. allocated(ident%name) .or. len_trim(ident%name) == 0) then
            typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
            return
        end if

        ! Look up identifier in hierarchical scopes
        call ctx%scopes%lookup(ident%name, scheme)

        if (allocated(scheme)) then
            ! Found in environment - instantiate the type scheme
            typ = ctx%instantiate(scheme)
        else
            ! Not found - behavior depends on mode
            if (ctx%strict_mode) then
                ! Standard Fortran mode: undefined variable is an error
                error_result = create_error_result( &
                    "Undefined variable '" // ident%name // "' in strict mode", &
                    ERROR_SEMANTIC, &
                    component="semantic_analyzer", &
                    context="infer_identifier", &
                    suggestion="Declare the variable with 'integer :: " // ident%name // &
                    "' or remove 'implicit none' for lazy Fortran mode" &
                )
                call ctx%errors%add_result(error_result)
                
                ! Create fresh type variable for continued analysis
                typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
            else
                ! Lazy Fortran mode: auto-declare undefined variables with type inference
                ! Try to infer the type from context or create a fresh type variable
                typ = infer_type_from_usage_context(ident%name, ctx%next_var_id)
                
                ! Create polymorphic type scheme and add to scope for future use
                block
                    type(poly_type_t) :: new_scheme
                    new_scheme = create_poly_type(forall_vars=[type_var_t::], mono=typ)
                    call ctx%scopes%define(ident%name, new_scheme)
                end block
            end if
        end if
    end function infer_identifier

    ! Infer type of binary operation (simplified using extracted modules)
    function infer_binary_op(ctx, arena, binop, binop_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(binary_op_node), intent(in) :: binop
        integer, intent(in) :: binop_index
        type(mono_type_t) :: typ
        type(mono_type_t) :: left_typ, right_typ

        ! Infer types of operands
        left_typ = get_inferred_type_from_arena(ctx, arena, binop%left_index)
        right_typ = get_inferred_type_from_arena(ctx, arena, binop%right_index)

        ! Dispatch to appropriate operation handler
        if (binop%operator == "+") then
            if (left_typ%kind == TCHAR .and. right_typ%kind == TCHAR) then
                typ = infer_string_concatenation(left_typ, right_typ)
                call ctx%unify(left_typ, create_mono_type(TCHAR))
                call ctx%unify(right_typ, create_mono_type(TCHAR))
                call rewrite_binary_operator(arena, binop_index, "//")
                call set_node_inferred_type(arena, binop_index, typ)
                return
            end if
        end if

        if (binop%operator == "//") then
            typ = infer_string_concatenation(left_typ, right_typ)
            call ctx%unify(left_typ, create_mono_type(TCHAR))
            call ctx%unify(right_typ, create_mono_type(TCHAR))
        else if (binop%operator == "==" .or. binop%operator == "/=" .or. &
                 binop%operator == "<" .or. binop%operator == "<=" .or. &
                 binop%operator == ">" .or. binop%operator == ">=") then
            typ = infer_comparison_operation(left_typ, right_typ)
            call ctx%unify(left_typ, right_typ)
        else if (binop%operator == ".and." .or. binop%operator == ".or." .or. &
                 binop%operator == ".not." .or. binop%operator == ".eqv." .or. &
                 binop%operator == ".neqv.") then
            typ = infer_logical_operation()
            call ctx%unify(left_typ, typ)
            call ctx%unify(right_typ, typ)
        else
            ! Arithmetic operators
            typ = get_common_type(left_typ, right_typ)
            if (typ%kind == 0) then; call ctx%unify(left_typ, right_typ); typ = left_typ; end if
        end if

        ! Store inferred type in node
        call set_node_inferred_type(arena, binop_index, typ)
    end function infer_binary_op

    subroutine rewrite_binary_operator(arena, node_index, new_operator)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: new_operator

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (binary_op_node)
            node%operator = new_operator
            arena%entries(node_index)%node = node
        end select
    end subroutine rewrite_binary_operator

    ! Infer type of function call (simplified)
    function infer_function_call(ctx, arena, call_node) result(typ)
        use intrinsic_registry, only: get_intrinsic_signature, is_intrinsic_function
        use iso_fortran_env, only: error_unit
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
        type(mono_type_t) :: typ
        type(poly_type_t), allocatable :: scheme
        type(mono_type_t) :: arg_type
        character(len=:), allocatable :: intrinsic_sig
        integer :: i
        logical :: is_intrinsic_func

        ! Process arguments to detect undefined variables
        if (allocated(call_node%arg_indices)) then
            do i = 1, size(call_node%arg_indices)
                arg_type = get_inferred_type_from_arena(ctx, arena, call_node%arg_indices(i))
            end do
        end if

        ! Look up function in scope
        call ctx%scopes%lookup(call_node%name, scheme)
        
        if (allocated(scheme)) then
            typ = ctx%instantiate(scheme)
            ! Extract return type from function type
            if (typ%kind == TFUN .and. type_args_allocated(typ) .and. type_args_size(typ) >= 2) then
                typ = type_args_element(typ, 2)  ! Second arg is return type
            end if
        else 
            ! Check if it's an intrinsic function
            is_intrinsic_func = is_intrinsic_function(call_node%name)
            ! DEBUG: Print to stderr for debugging
            ! write(error_unit, '(A,A,A,L1)') "DEBUG: Checking function '", call_node%name, "' - is_intrinsic: ", is_intrinsic_func
            
            if (is_intrinsic_func) then
                ! Handle intrinsic functions - double check with registry
                intrinsic_sig = get_intrinsic_signature(call_node%name)
                ! DEBUG: Print signature
                ! if (allocated(intrinsic_sig)) write(error_unit, '(A,A)') "DEBUG: Signature: ", intrinsic_sig
                
                if (len_trim(intrinsic_sig) > 0) then
                    ! Parse the return type from the signature
                    ! Signature format: "return_type(arg_types)"
                    ! For mathematical intrinsics, return type is typically "real"
                    if (index(intrinsic_sig, "real(") == 1) then
                        typ = create_mono_type(TREAL)
                    else if (index(intrinsic_sig, "integer(") == 1) then
                        typ = create_mono_type(TINT)
                    else if (index(intrinsic_sig, "logical(") == 1) then
                        typ = create_mono_type(TLOGICAL)
                    else if (index(intrinsic_sig, "character(") == 1) then
                        typ = create_mono_type(TCHAR)
                    else
                        ! Default to real for unknown intrinsic types
                        typ = create_mono_type(TREAL)
                    end if
                else
                    ! Unknown intrinsic - default to real type
                    typ = create_mono_type(TREAL)
                end if
            else
                ! Unknown function - default to real type
                typ = create_mono_type(TREAL)
            end if
        end if
    end function infer_function_call

    ! Infer type of array slice
    function infer_array_slice(ctx, arena, slice_node) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(array_slice_node), intent(in) :: slice_node
        type(mono_type_t) :: typ
        
        ! For now, return real array type
        typ = create_mono_type(TARRAY)
    end function infer_array_slice

    ! Infer type of assignment (simplified using extracted module)
    function infer_assignment(ctx, arena, assignment, assignment_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assignment
        integer, intent(in) :: assignment_index
        type(mono_type_t) :: typ
        type(mono_type_t) :: expr_typ
        integer :: lhs_index

        lhs_index = assignment%target_index
        expr_typ = get_inferred_type_from_arena(ctx, arena, assignment%value_index)

        ! Use extracted assignment processing
        call process_assignment_inference(arena, assignment, assignment_index, &
                                         lhs_index, expr_typ, &
                                         ctx%scopes, ctx%errors, ctx%strict_mode, ctx%next_var_id)

        ! For array assignments, return the element type instead of array type
        if (expr_typ%kind == TARRAY .and. expr_typ%get_args_count() > 0) then
            typ = expr_typ%get_arg(1)  ! Return element type
        else
            typ = expr_typ
        end if

        ! Store the actual assignment type
        call set_node_inferred_type(arena, assignment_index, typ)
    end function infer_assignment

    ! Best-effort: if a declaration for the given name exists in the arena, define it in scope
    subroutine ensure_declared_from_arena(ctx, arena, name)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer :: i, j
        type(poly_type_t) :: scheme
        type(mono_type_t) :: decl_type

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (declaration_node)
                if (allocated(node%var_name)) then
                    if (trim(node%var_name) == trim(name)) then
                        call process_declaration_variables(node, decl_type)
                        scheme = ctx%generalize(decl_type)
                        call ctx%scopes%define(name, scheme)
                        return
                    end if
                end if
                if (node%is_multi_declaration .and. allocated(node%var_names)) then
                    do j = 1, size(node%var_names)
                        if (trim(node%var_names(j)) == trim(name)) then
                            call process_declaration_variables(node, decl_type)
                            scheme = ctx%generalize(decl_type)
                            call ctx%scopes%define(name, scheme)
                            return
                        end if
                    end do
                end if
            end select
        end do
    end subroutine ensure_declared_from_arena


    ! Infer type of array literal with type promotion
    function infer_array_literal(ctx, arena, array_lit, array_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(array_literal_node), intent(in) :: array_lit
        integer, intent(in) :: array_index
        type(mono_type_t) :: typ
        type(mono_type_t) :: element_type, promoted_type, first_type
        type(mono_type_t), allocatable :: args(:), inner_args(:)
        integer :: i, elem_array_size, first_array_size
        logical :: has_real, all_arrays, consistent_sizes

        ! If empty array, default to integer
        if (.not. allocated(array_lit%element_indices) .or. &
            size(array_lit%element_indices) == 0) then
            allocate(args(1))
            args(1) = create_mono_type(TINT)
            typ = create_mono_type(TARRAY, args=args)
            return
        end if

        ! Start with first element type
        first_type = get_inferred_type_from_arena(ctx, arena, array_lit%element_indices(1))
        promoted_type = first_type
        has_real = (first_type%kind == TREAL)
        all_arrays = (first_type%kind == TARRAY)
        consistent_sizes = .true.
        
        
        ! If first element is an array, get its size
        if (all_arrays) then
            first_array_size = first_type%size
        end if
        
        ! Check all elements for type promotion and consistency
        do i = 2, size(array_lit%element_indices)
            element_type = get_inferred_type_from_arena(ctx, arena, array_lit%element_indices(i))
            
            ! Check if all elements are arrays (for nested arrays)
            if (all_arrays .and. element_type%kind /= TARRAY) then
                all_arrays = .false.
            else if (all_arrays .and. element_type%kind == TARRAY) then
                ! Check for consistent sizes in nested arrays
                elem_array_size = element_type%size
                if (elem_array_size /= first_array_size) then
                    consistent_sizes = .false.
                end if
            end if
            
            ! If we encounter a real type, promote the entire array to real
            if (element_type%kind == TREAL) then
                has_real = .true.
                if (.not. all_arrays) promoted_type = create_mono_type(TREAL)
            else if (element_type%kind == TARRAY .and. element_type%has_args()) then
                ! Check element type of nested array for real promotion
                if (element_type%get_args_count() > 0) then
                    promoted_type = element_type%get_arg(1)
                    if (promoted_type%kind == TREAL) then
                        has_real = .true.
                    end if
                end if
            end if
        end do
        
        ! Handle nested arrays (multi-dimensional)
        if (all_arrays .and. consistent_sizes) then
            ! All elements are arrays of the same size - create a 2D array
            ! Get the element type of the nested arrays
            if (first_type%has_args() .and. first_type%get_args_count() > 0) then
                ! Determine base element type (integer or real)
                if (has_real) then
                    promoted_type = create_mono_type(TREAL)
                else 
                    promoted_type = first_type%get_arg(1)
                end if
            else
                promoted_type = create_mono_type(TINT)
            end if
            
            ! Create nested array type: array of arrays
            ! The outer array has size equal to number of sub-arrays
            ! The inner arrays have their original size
            allocate(inner_args(1))
            inner_args(1) = promoted_type
            
            allocate(args(1))
            args(1) = create_mono_type(TARRAY, args=inner_args, &
                                        array_size=first_array_size)
            typ = create_mono_type(TARRAY, args=args, &
                                   array_size=size(array_lit%element_indices))
            deallocate(inner_args)
        else
            ! Regular 1D array (not nested)
            ! If any element is real, promote to real
            if (has_real .and. promoted_type%kind == TINT) then
                promoted_type = create_mono_type(TREAL)
            end if
            
            ! Create array type with correct size
            allocate(args(1))
            args(1) = promoted_type
            typ = create_mono_type(TARRAY, args=args, &
                                   array_size=size(array_lit%element_indices))
        end if
        
        ! Store in node
        call set_node_inferred_type(arena, array_index, typ)
    end function infer_array_literal

    ! Infer type of implied do loop (simplified)
    function infer_implied_do_loop(ctx, arena, do_loop, do_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(do_loop_node), intent(in) :: do_loop
        integer, intent(in) :: do_index
        type(mono_type_t) :: typ
        type(mono_type_t), allocatable :: args(:)

        ! For now, return integer array type
        allocate(args(1))
        args(1) = create_mono_type(TINT)
        typ = create_mono_type(TARRAY, args=args)
    end function infer_implied_do_loop



    function has_semantic_errors(ctx) result(has_errors)
        type(semantic_context_t), intent(in) :: ctx
        logical :: has_errors
        has_errors = ctx%errors%has_errors()
    end function has_semantic_errors

    ! Note: check_undefined_variables_internal is now provided by
    ! semantic_undefined_variable_checker module for architectural compliance
    ! Control flow helpers are now provided by semantic_control_flow_helpers module

    ! Implementation of required abstract procedures from semantic_context_base_t
    
    ! Get context name
    function semantic_get_context_name(this) result(name)
        class(semantic_context_t), intent(in) :: this
        character(:), allocatable :: name
        name = "semantic_context"
    end function semantic_get_context_name
    
    ! Clone context (simplified implementation)  
    function semantic_clone_context(this) result(cloned)
        class(semantic_context_t), intent(in) :: this
        class(semantic_context_base_t), allocatable :: cloned
        type(semantic_context_t) :: temp_context
        
        ! Copy basic components - simplified for compatibility
        temp_context%context_id = this%context_id
        temp_context%context_name = this%context_name
        temp_context%scopes = this%scopes
        temp_context%next_var_id = this%next_var_id
        temp_context%subst = this%subst
        ! Tracking fields removed in lean build
        temp_context%errors = this%errors
        temp_context%strict_mode = this%strict_mode
        
        allocate(cloned, source=temp_context)
    end function semantic_clone_context

end module semantic_analyzer
