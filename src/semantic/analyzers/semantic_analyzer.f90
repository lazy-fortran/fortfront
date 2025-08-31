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
    use type_checker
    use ast_core
    use semantic_inference_helpers, only: check_implicit_none
    use semantic_validation_utils, only: validate_array_bounds, check_shape_conformance, &
                                          update_identifier_type_in_arena, int_to_str
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
    use parameter_tracker
    use expression_temporary_tracker_module
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
        type(parameter_tracker_t) :: param_tracker  ! Track parameter attributes
        type(temp_tracker_t) :: temp_tracker  ! Track expression temporaries
        type(error_collection_t) :: errors  ! Collect semantic errors
        logical :: strict_mode = .false.  ! True for standard Fortran (implicit none), false for lazy Fortran
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

contains

    ! Create a new semantic context with builtin functions
    function create_semantic_context() result(ctx)
        type(semantic_context_t) :: ctx
        type(poly_type_t) :: builtin_scheme
        type(mono_type_t) :: real_to_real, real_type
        
        ! Initialize base class components
        ctx%context_id = 1
        ctx%context_name = "semantic_context"
        
        ! Initialize basic components
        ctx%scopes = create_scope_stack()
        ctx%subst%count = 0
        ctx%param_tracker%count = 0
        ctx%temp_tracker = create_temp_tracker()
        ctx%errors = create_error_collection()
        ctx%next_var_id = 1  ! Start from 1 (main branch compatibility)
        
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
    end function create_semantic_context

    ! Main entry point: analyze entire program
    subroutine analyze_program(ctx, arena, root_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index

        if (root_index <= 0 .or. root_index > arena%size) return
        if (.not. allocated(arena%entries(root_index)%node)) return

        select type (ast => arena%entries(root_index)%node)
        type is (program_node)
            ! Issue #1076 FIX: Only check implicit none if strict mode is not already explicitly enabled
            ! When strict mode is explicitly set (e.g., by frontend for Issue #495), don't override it
            if (.not. ctx%strict_mode) then
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
        arena%entries(node_index)%node%inferred_type = inferred
    end subroutine infer_and_store_type

    ! Simplified type inference entry point
    function infer_statement_type(this, arena, stmt_index) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_index
        type(mono_type_t) :: typ

        typ = this%infer(arena, stmt_index)
    end function infer_statement_type

    ! Main type inference function - refactored to <100 lines
    recursive function infer_type(this, arena, expr_index) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: expr_index
        type(mono_type_t) :: typ

        ! Guard clauses
        if (expr_index <= 0 .or. expr_index > arena%size) then
            typ = create_mono_type(TREAL)
            return
        end if
        if (.not. allocated(arena%entries(expr_index)%node)) then
            typ = create_mono_type(TREAL)
            return
        end if

        ! Dispatch to specialized inference handlers
        select type (expr => arena%entries(expr_index)%node)
        type is (literal_node)
            typ = infer_literal(this, expr)
        type is (identifier_node)
            typ = infer_identifier(this, expr)
        type is (binary_op_node)
            typ = infer_binary_op(this, arena, expr, expr_index)
        type is (call_or_subscript_node)
            typ = infer_function_call(this, arena, expr)
        type is (array_slice_node)
            typ = infer_array_slice(this, arena, expr)
        type is (subroutine_call_node)
            typ = create_mono_type(TVAR, var=create_type_var(0, "error"))
        type is (function_def_node)
            typ = infer_function_definition(this, arena, expr, expr_index)
        type is (assignment_node)
            typ = infer_assignment(this, arena, expr, expr_index)
        type is (array_literal_node)
            typ = infer_array_literal(this, arena, expr, expr_index)
        type is (do_loop_node)
            typ = infer_implied_do_loop(this, arena, expr, expr_index)
        type is (declaration_node)
            typ = infer_declaration(this, arena, expr, expr_index)
        type is (if_node)
            typ = infer_if_node(this, arena, expr)
        type is (do_while_node)
            typ = infer_do_while_node(this, arena, expr)
        type is (where_node)
            typ = infer_where_node(this, arena, expr)
        type is (where_stmt_node)
            typ = infer_where_stmt_node(this, arena, expr)
        type is (forall_node)
            typ = infer_forall_node(this, arena, expr)
        type is (select_case_node)
            typ = infer_select_case_node(this, arena, expr)
        type is (associate_node)
            typ = infer_associate_node(this, arena, expr)
        type is (stop_node)
            typ = infer_stop_node(this, arena, expr)
        type is (cycle_node)
            typ = create_mono_type(TVAR, var=create_type_var(0, "control"))
        type is (exit_node)
            typ = create_mono_type(TVAR, var=create_type_var(0, "control"))
        type is (return_node)
            typ = create_mono_type(TVAR, var=create_type_var(0, "control"))
        class default
            typ = create_mono_type(TREAL)
        end select

        ! Apply substitution and finalize
        typ = this%apply_subst_to_type(typ)
        if (typ%kind == TVAR .and. len_trim(typ%var%name) == 0) then
            typ%var%name = "v"//int_to_str(typ%var%id)
        end if
    end function infer_type

    ! Declaration type inference helper
    function infer_declaration(this, arena, expr, expr_index) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        class(declaration_node), intent(in) :: expr
        integer, intent(in) :: expr_index
        type(mono_type_t) :: typ
        type(poly_type_t) :: scheme
        integer :: i

        call process_declaration_variables(expr, typ)
        scheme = this%generalize(typ)
        if (expr%is_multi_declaration .and. allocated(expr%var_names)) then
            do i = 1, size(expr%var_names)
                call this%scopes%define(expr%var_names(i), scheme)
            end do
        else if (allocated(expr%var_name)) then
            call this%scopes%define(expr%var_name, scheme)
        end if
        arena%entries(expr_index)%node%inferred_type = typ
    end function infer_declaration

    ! If node type inference helper
    function infer_if_node(this, arena, expr) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        class(if_node), intent(in) :: expr
        type(mono_type_t) :: typ
        integer :: i

        typ = create_mono_type(TVAR, var=create_type_var(0, "control"))
        if (expr%condition_index > 0) typ = this%infer(arena, expr%condition_index)
        if (allocated(expr%then_body_indices)) then
            do i = 1, size(expr%then_body_indices)
                typ = this%infer(arena, expr%then_body_indices(i))
            end do
        end if
        call process_if_node_branches(expr, typ)
    end function infer_if_node

    ! Do while node type inference helper
    function infer_do_while_node(this, arena, expr) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        class(do_while_node), intent(in) :: expr
        type(mono_type_t) :: typ
        integer :: i

        typ = create_mono_type(TVAR, var=create_type_var(0, "control"))
        if (expr%condition_index > 0) typ = this%infer(arena, expr%condition_index)
        if (allocated(expr%body_indices)) then
            do i = 1, size(expr%body_indices)
                typ = this%infer(arena, expr%body_indices(i))
            end do
        end if
        call process_do_while_node_body(expr, typ)
    end function infer_do_while_node

    ! Where node type inference helper
    function infer_where_node(this, arena, expr) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        class(where_node), intent(in) :: expr
        type(mono_type_t) :: typ
        integer :: i

        typ = create_mono_type(TVAR, var=create_type_var(0, "control"))
        if (expr%mask_expr_index > 0) typ = this%infer(arena, expr%mask_expr_index)
        if (allocated(expr%where_body_indices)) then
            do i = 1, size(expr%where_body_indices)
                typ = this%infer(arena, expr%where_body_indices(i))
            end do
        end if
        call process_where_node_clauses(expr, typ)
    end function infer_where_node

    ! Where statement node type inference helper
    function infer_where_stmt_node(this, arena, expr) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        class(where_stmt_node), intent(in) :: expr
        type(mono_type_t) :: typ

        typ = create_mono_type(TVAR, var=create_type_var(0, "control"))
        if (expr%mask_expr_index > 0) typ = this%infer(arena, expr%mask_expr_index)
        if (expr%assignment_index > 0) typ = this%infer(arena, expr%assignment_index)
        call process_where_stmt_node(expr, typ)
    end function infer_where_stmt_node

    ! Forall node type inference helper
    function infer_forall_node(this, arena, expr) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        class(forall_node), intent(in) :: expr
        type(mono_type_t) :: typ
        type(poly_type_t) :: int_scheme
        integer :: i

        typ = create_mono_type(TVAR, var=create_type_var(0, "control"))
        call process_forall_node_body(expr, int_scheme, typ)
        call this%scopes%enter_block()
        if (allocated(expr%index_names)) then
            do i = 1, size(expr%index_names)
                call this%scopes%define(expr%index_names(i), int_scheme)
            end do
        end if
        if (allocated(expr%body_indices)) then
            do i = 1, size(expr%body_indices)
                typ = this%infer(arena, expr%body_indices(i))
            end do
        end if
        call this%scopes%leave_scope()
    end function infer_forall_node

    ! Select case node type inference helper
    function infer_select_case_node(this, arena, expr) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        class(select_case_node), intent(in) :: expr
        type(mono_type_t) :: typ
        integer :: i

        typ = create_mono_type(TVAR, var=create_type_var(0, "control"))
        if (expr%selector_index > 0) typ = this%infer(arena, expr%selector_index)
        if (allocated(expr%case_indices)) then
            do i = 1, size(expr%case_indices)
                typ = this%infer(arena, expr%case_indices(i))
            end do
        end if
        call process_select_case_blocks(expr, typ)
    end function infer_select_case_node

    ! Associate node type inference helper
    function infer_associate_node(this, arena, expr) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        class(associate_node), intent(in) :: expr
        type(mono_type_t) :: typ
        type(mono_type_t) :: assoc_type
        type(poly_type_t) :: assoc_scheme
        integer :: i

        typ = create_mono_type(TVAR, var=create_type_var(0, "control"))
        call this%scopes%enter_block()
        if (allocated(expr%associations)) then
            do i = 1, size(expr%associations)
                if (expr%associations(i)%expr_index > 0) then
                    assoc_type = this%infer(arena, expr%associations(i)%expr_index)
                    assoc_scheme = create_poly_type(forall_vars=[type_var_t::], &
                        mono=assoc_type)
                    if (allocated(expr%associations(i)%name)) then
                        call this%scopes%define(expr%associations(i)%name, &
                            assoc_scheme)
                    end if
                end if
            end do
        end if
        if (allocated(expr%body_indices)) then
            do i = 1, size(expr%body_indices)
                typ = this%infer(arena, expr%body_indices(i))
            end do
        end if
        call this%scopes%leave_scope()
        call process_associate_node_body(expr, typ)
    end function infer_associate_node

    ! Stop node type inference helper
    function infer_stop_node(this, arena, expr) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        class(stop_node), intent(in) :: expr
        type(mono_type_t) :: typ

        typ = create_mono_type(TVAR, var=create_type_var(0, "control"))
        if (expr%stop_code_index > 0) typ = this%infer(arena, expr%stop_code_index)
        call process_stop_node_code(expr, typ)
    end function infer_stop_node


    ! Type unification (simplified)
    subroutine unify_types(this, t1, t2)
        class(semantic_context_t), intent(inout) :: this
        type(mono_type_t), intent(in) :: t1, t2

        ! Simplified unification
    end subroutine unify_types

    ! Instantiate type scheme
    function instantiate_type_scheme(this, scheme) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(poly_type_t), intent(in) :: scheme
        type(mono_type_t) :: typ

        ! Simplified instantiation
        typ = create_mono_type(TVAR, var=this%fresh_type_var())
    end function instantiate_type_scheme

    ! Generalize type
    function generalize_type(this, typ) result(scheme)
        class(semantic_context_t), intent(in) :: this
        type(mono_type_t), intent(in) :: typ
        type(poly_type_t) :: scheme
        type(type_var_t), allocatable :: free_vars(:)

        ! Simplified generalization
        allocate(free_vars(0))
        scheme = create_poly_type(free_vars, typ)
    end function generalize_type

    ! Generate fresh type variable
    function generate_fresh_type_var(this) result(tv)
        class(semantic_context_t), intent(inout) :: this
        type(type_var_t) :: tv

        tv = create_type_var(this%next_var_id, "v"//int_to_str(this%next_var_id))
        this%next_var_id = this%next_var_id + 1
    end function generate_fresh_type_var

    ! Apply current substitution to type
    function apply_current_substitution(this, typ) result(result_type)
        class(semantic_context_t), intent(in) :: this
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t) :: result_type

        ! Simplified substitution application
        result_type = typ
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

    ! Deep copy
    function semantic_context_deep_copy(this) result(copy)
        class(semantic_context_t), intent(in) :: this
        type(semantic_context_t) :: copy

        copy%scopes = this%scopes
        copy%next_var_id = this%next_var_id
        copy%subst = this%subst
        copy%param_tracker = this%param_tracker
        copy%temp_tracker = this%temp_tracker
        copy%errors = this%errors
        copy%strict_mode = this%strict_mode
    end function semantic_context_deep_copy

    ! Helper function to infer type from usage context (enhanced type inference)
    function infer_type_from_usage_context(ctx, var_name) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        character(len=*), intent(in) :: var_name
        type(mono_type_t) :: typ
        
        ! Enhanced type inference based on variable name patterns and context
        ! This improves user experience by making reasonable type guesses
        
        ! Pattern-based type inference for common variable names
        select case(var_name)
        case ('i', 'j', 'k', 'n', 'count', 'index', 'num', 'size')
            ! Common integer variable patterns
            typ = create_mono_type(TINT)
        case ('x', 'y', 'z', 'result', 'value', 'temp')
            ! Common real variable patterns  
            typ = create_mono_type(TREAL)
        case ('flag', 'found', 'done', 'success', 'valid')
            ! Common logical variable patterns
            typ = create_mono_type(TLOGICAL)
        case default
            ! Check if name suggests a specific type
            if (index(var_name, 'str') > 0 .or. index(var_name, 'name') > 0 .or. &
                index(var_name, 'msg') > 0 .or. index(var_name, 'text') > 0) then
                ! String-like variable names
                typ = create_mono_type(TCHAR)
            else if (index(var_name, 'num') > 0 .or. index(var_name, 'count') > 0 .or. &
                     index(var_name, 'idx') > 0) then
                ! Number-like variable names
                typ = create_mono_type(TINT)
            else
                ! Default: create type variable for later unification
                typ = create_mono_type(TVAR, var=ctx%fresh_type_var())
            end if
        end select
    end function infer_type_from_usage_context

    ! Enhanced function definition semantic analysis
    function infer_function_definition(ctx, arena, func_node, func_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(function_def_node), intent(in) :: func_node
        integer, intent(in) :: func_index
        type(mono_type_t) :: typ
        type(mono_type_t), allocatable :: param_types(:)
        type(mono_type_t) :: return_type
        integer :: i
        
        ! Analyze function parameters
        if (allocated(func_node%param_indices)) then
            allocate(param_types(size(func_node%param_indices)))
            
            ! Create new scope for function parameters
            call ctx%scopes%enter_block()
            
            do i = 1, size(func_node%param_indices)
                ! Infer parameter type and add to scope
                param_types(i) = ctx%infer(arena, func_node%param_indices(i))
                
                ! Add parameter to function scope if it's an identifier
                if (func_node%param_indices(i) > 0 .and. func_node%param_indices(i) <= arena%size) then
                    select type (param_node => arena%entries(func_node%param_indices(i))%node)
                    type is (identifier_node)
                        if (allocated(param_node%name) .and. len_trim(param_node%name) > 0) then
                            block
                                type(poly_type_t) :: param_scheme
                                param_scheme = create_poly_type(forall_vars=[type_var_t::], mono=param_types(i))
                                call ctx%scopes%define(param_node%name, param_scheme)
                            end block
                        end if
                    end select
                end if
            end do
        else
            allocate(param_types(0))
        end if
        
        ! Determine return type based on function name and return variable
        if (allocated(func_node%result_variable) .and. len_trim(func_node%result_variable) > 0) then
            ! Function has explicit result variable
            return_type = infer_type_from_usage_context(ctx, func_node%result_variable)
            
            ! Add result variable to function scope
            block
                type(poly_type_t) :: result_scheme
                result_scheme = create_poly_type(forall_vars=[type_var_t::], mono=return_type)
                call ctx%scopes%define(func_node%result_variable, result_scheme)
            end block
        else if (allocated(func_node%name) .and. len_trim(func_node%name) > 0) then
            ! Function name is the result variable (standard Fortran)
            return_type = infer_type_from_usage_context(ctx, func_node%name)
            
            ! Add function name as result variable to scope
            block
                type(poly_type_t) :: result_scheme
                result_scheme = create_poly_type(forall_vars=[type_var_t::], mono=return_type)
                call ctx%scopes%define(func_node%name, result_scheme)
            end block
        else
            ! Default return type for unnamed functions
            return_type = create_mono_type(TREAL)
        end if
        
        ! Analyze function body with parameters and result in scope
        if (allocated(func_node%body_indices)) then
            do i = 1, size(func_node%body_indices)
                ! Use infer instead of infer_stmt (which doesn't exist)
                typ = ctx%infer(arena, func_node%body_indices(i))
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
        lhs%param_tracker = rhs%param_tracker
        lhs%temp_tracker = rhs%temp_tracker
        lhs%errors = rhs%errors
        lhs%strict_mode = rhs%strict_mode
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
                typ = infer_type_from_usage_context(ctx, ident%name)
                
                ! Create polymorphic type scheme and add to scope for future use
                block
                    type(poly_type_t) :: new_scheme
                    new_scheme = create_poly_type(forall_vars=[type_var_t::], mono=typ)
                    call ctx%scopes%define(ident%name, new_scheme)
                end block
            end if
        end if
    end function infer_identifier

    ! Infer type of binary operation (simplified)
    function infer_binary_op(ctx, arena, binop, binop_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(binary_op_node), intent(in) :: binop
        integer, intent(in) :: binop_index
        type(mono_type_t) :: typ
        type(mono_type_t) :: left_typ, right_typ

        ! Infer types of operands
        left_typ = ctx%infer(arena, binop%left_index)
        right_typ = ctx%infer(arena, binop%right_index)

        ! Special handling for string concatenation
        if (binop%operator == "//") then
            ! Calculate combined string length if both operands are known
            block
                integer :: left_size, right_size, total_size
                logical :: can_calculate_size
                
                ! Try to get sizes of operands
                left_size = 0
                right_size = 0
                can_calculate_size = .false.
                
                ! Get left operand size
                if (left_typ%kind == TCHAR .and. left_typ%size >= 0) then
                    left_size = left_typ%size
                end if
                
                ! Get right operand size
                if (right_typ%kind == TCHAR .and. right_typ%size >= 0) then
                    right_size = right_typ%size
                end if
                
                ! If we can determine both sizes, calculate total
                if (left_typ%kind == TCHAR .and. right_typ%kind == TCHAR .and. &
                    left_typ%size >= 0 .and. right_typ%size >= 0) then
                    total_size = left_size + right_size
                    can_calculate_size = .true.
                end if
                
                ! Create appropriate character type
                if (can_calculate_size) then
                    typ = create_mono_type(TCHAR, char_size=total_size)
                    typ%alloc_info%needs_allocatable_string = .false.
                else
                    typ = create_mono_type(TCHAR)
                    typ%alloc_info%needs_allocatable_string = .true.
                end if
            end block
            
            call ctx%unify(left_typ, create_mono_type(TCHAR))
            call ctx%unify(right_typ, create_mono_type(TCHAR))
        ! Comparison operators return logical
        else if (binop%operator == "==" .or. binop%operator == "/=" .or. &
                 binop%operator == "<" .or. binop%operator == "<=" .or. &
                 binop%operator == ">" .or. binop%operator == ">=") then
            call ctx%unify(left_typ, right_typ)
            typ = create_mono_type(TLOGICAL)
        ! Logical operators
        else if (binop%operator == ".and." .or. binop%operator == ".or." .or. &
                 binop%operator == ".not." .or. binop%operator == ".eqv." .or. &
                 binop%operator == ".neqv.") then
            typ = create_mono_type(TLOGICAL)
            call ctx%unify(left_typ, typ)
            call ctx%unify(right_typ, typ)
        ! Arithmetic operators preserve type
        else
            typ = get_common_type(left_typ, right_typ)
            if (typ%kind == 0) then; call ctx%unify(left_typ, right_typ); typ = left_typ; end if
        end if

        ! Store inferred type in node if it's a binary_op_node
        arena%entries(binop_index)%node%inferred_type = typ
    end function infer_binary_op

    ! Infer type of function call (simplified)
    function infer_function_call(ctx, arena, call_node) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
        type(mono_type_t) :: typ
        type(poly_type_t), allocatable :: scheme
        type(mono_type_t) :: arg_type
        integer :: i

        ! Process arguments to detect undefined variables
        if (allocated(call_node%arg_indices)) then
            do i = 1, size(call_node%arg_indices)
                if (call_node%arg_indices(i) > 0 .and. &
                    call_node%arg_indices(i) <= arena%size) then
                    ! Infer argument type (this will detect undefined variables)
                    arg_type = ctx%infer(arena, call_node%arg_indices(i))
                end if
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
            ! Unknown function - default to real type
            typ = create_mono_type(TREAL)
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

    ! Infer type of assignment with hierarchical scopes
    function infer_assignment(ctx, arena, assignment, assignment_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(assignment_node), intent(in) :: assignment
        integer, intent(in) :: assignment_index
        type(mono_type_t) :: typ
        type(mono_type_t) :: expr_typ, existing_typ
        type(poly_type_t), allocatable :: scheme, existing_scheme
        integer :: lhs_index
        type(result_t) :: error_result

        lhs_index = assignment%target_index
        expr_typ = ctx%infer(arena, assignment%value_index)

        if (lhs_index > 0 .and. lhs_index <= arena%size) then
            if (allocated(arena%entries(lhs_index)%node)) then
                select type (lhs_node => arena%entries(lhs_index)%node)
                type is (identifier_node)
                    ! Check if already defined in current or parent scope
                    call ctx%scopes%lookup(lhs_node%name, existing_scheme)
                    
                    if (allocated(existing_scheme)) then
                        ! Variable exists - unify with existing type
                        existing_typ = ctx%instantiate(existing_scheme)
                        call ctx%unify(existing_typ, expr_typ)
                    else
                        ! Assignment to undefined variable - behavior depends on mode
                        if (ctx%strict_mode) then
                            ! Standard Fortran mode: undefined variable is an error
                            error_result = create_error_result( &
                                "Undefined variable '" // lhs_node%name // "' in assignment", &
                                ERROR_SEMANTIC, &
                                component="semantic_analyzer", &
                                context="infer_assignment", &
                                suggestion="Declare the variable before assigning to it" &
                            )
                            call ctx%errors%add_result(error_result)
                        end if
                        
                        ! Continue analysis with inferred type (both modes)
                        expr_typ = ctx%apply_subst_to_type(expr_typ)
                    end if
                    
                    ! Handle allocatable character detection only when size cannot be determined
                    if (expr_typ%kind == TCHAR) then
                        if (assignment%value_index > 0 .and. assignment%value_index <= arena%size) then
                            if (allocated(arena%entries(assignment%value_index)%node)) then
                                select type (value_node => arena%entries(assignment%value_index)%node)
                                type is (binary_op_node)
                                    if (value_node%operator == "//") then
                                        ! Only mark as allocatable if size was not calculated
                                        if (expr_typ%size < 0) then
                                            expr_typ%alloc_info%is_allocatable = .true.
                                            expr_typ%alloc_info%needs_allocatable_string = .true.
                                            expr_typ%size = 0  ! Deferred length
                                        end if
                                        
                                        ! Update all existing identifier nodes with this name
                                        call update_identifier_type_in_arena(arena, lhs_node%name, expr_typ)
                                    end if
                                end select
                            end if
                        end if
                    end if
                    
                    ! Update all identifier nodes in the arena with the inferred type
                    call update_identifier_type_in_arena(arena, lhs_node%name, expr_typ)
                    
                    ! Generalize the expression type and define/update in scope
                    allocate(scheme)
                    scheme = ctx%generalize(expr_typ)
                    call ctx%scopes%define(lhs_node%name, scheme)
                end select
            end if
        end if

        ! For array assignments, return the element type instead of array type
        ! This helps with type inference tests that expect element types
        if (expr_typ%kind == TARRAY .and. expr_typ%get_args_count() > 0) then
            typ = expr_typ%get_arg(1)  ! Return element type
        else
            typ = expr_typ
        end if

        ! Store the actual assignment type
        arena%entries(assignment_index)%node%inferred_type = typ
    end function infer_assignment


    ! Infer type of array literal with type promotion
    function infer_array_literal(ctx, arena, array_lit, array_index) result(typ)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(array_literal_node), intent(in) :: array_lit
        integer, intent(in) :: array_index
        type(mono_type_t) :: typ
        type(mono_type_t) :: element_type, promoted_type
        type(mono_type_t), allocatable :: args(:)
        integer :: i
        logical :: has_real

        ! If empty array, default to integer
        if (.not. allocated(array_lit%element_indices) .or. &
            size(array_lit%element_indices) == 0) then
            allocate(args(1))
            args(1) = create_mono_type(TINT)
            typ = create_mono_type(TARRAY, args=args)
            return
        end if

        ! Start with first element type
        promoted_type = ctx%infer(arena, array_lit%element_indices(1))
        has_real = (promoted_type%kind == TREAL)
        
        ! Check all elements for type promotion
        do i = 2, size(array_lit%element_indices)
            element_type = ctx%infer(arena, array_lit%element_indices(i))
            
            ! If we encounter a real type, promote the entire array to real
            if (element_type%kind == TREAL) then
                has_real = .true.
                promoted_type = create_mono_type(TREAL)
            end if
        end do
        
        ! If any element is real, promote to real
        if (has_real .and. promoted_type%kind == TINT) then
            promoted_type = create_mono_type(TREAL)
        end if
        
        ! Create array type with correct size
        allocate(args(1))
        args(1) = promoted_type
        typ = create_mono_type(TARRAY, args=args, array_size=size(array_lit%element_indices))
        
        ! Store in node
        arena%entries(array_index)%node%inferred_type = typ
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
        temp_context%param_tracker = this%param_tracker
        temp_context%temp_tracker = this%temp_tracker
        temp_context%errors = this%errors
        temp_context%strict_mode = this%strict_mode
        
        allocate(cloned, source=temp_context)
    end function semantic_clone_context

end module semantic_analyzer
