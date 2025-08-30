module semantic_analyzer_core
    ! Core semantic analysis context and main functions
    use type_system_unified, only: type_env_t, type_var_t, mono_type_t, poly_type_t, &
                                   substitution_t, allocation_info_t, &
                                   create_mono_type, create_type_var, &
                                   create_poly_type, create_fun_type, free_type_vars, &
                                   compose_substitutions, occurs_check, &
                                   TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN, TARRAY, &
                                   type_args_allocated, type_args_size, type_args_element
    use scope_manager
    use type_checker
    use ast_core
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    use ast_nodes_core, only: literal_node, identifier_node, binary_op_node, &
                               assignment_node, call_or_subscript_node, &
                               array_literal_node, program_node
    use ast_nodes_procedure, only: subroutine_call_node
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
    use semantic_inference_helpers
    implicit none
    private

    public :: semantic_context_t, create_semantic_context
    public :: analyze_program
    public :: validate_array_bounds, check_shape_conformance
    public :: has_semantic_errors
    public :: infer_statement_type

    ! Semantic analysis context
    type :: semantic_context_t
        type(scope_stack_t) :: scopes  ! Hierarchical scope management
        integer :: next_var_id = 0
        type(substitution_t) :: subst
        type(parameter_tracker_t) :: param_tracker  ! Track parameter attributes
        type(temp_tracker_t) :: temp_tracker  ! Track expression temporaries
        type(error_collection_t) :: errors  ! Collect semantic errors
        logical :: strict_mode = .false.  ! True for standard Fortran (implicit none), false for lazy Fortran
    contains
        procedure :: infer_stmt => infer_statement_type
    end type semantic_context_t

contains

    ! Create a new semantic context with builtin functions
    function create_semantic_context() result(ctx)
        type(semantic_context_t) :: ctx
        type(poly_type_t) :: builtin_scheme
        type(mono_type_t) :: real_to_real, real_type
        
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

    ! Main entry point for semantic analysis
    subroutine analyze_program(ctx, arena, root_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        type(ast_entry_t) :: root_node
        
        ! Validate root node
        if (root_index <= 0 .or. root_index > arena%size) then
            call ctx%errors%add_error("Invalid program root node index", ERROR_SEMANTIC)
            return
        end if
        
        root_node = arena%entries(root_index)
        if (allocated(root_node%node)) then
            select type (node => root_node%node)
            type is (program_node)
                call analyze_program_node_arena(ctx, arena, root_node, root_index)
            class default
                call ctx%errors%add_error("Root node must be a program node", ERROR_SEMANTIC)
            end select
        else
            call ctx%errors%add_error("Root node is not allocated", ERROR_SEMANTIC)
        end if
        
        ! Check for undefined variables at program level
        call check_undefined_variables_internal(ctx, arena, root_index)
    end subroutine analyze_program

    ! Analyze a specific program node within the arena
    subroutine analyze_program_node_arena(ctx, arena, prog, prog_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(ast_entry_t), intent(in) :: prog
        integer, intent(in) :: prog_index
        integer :: i
        
        call ctx%scopes%push(create_scope(1, "program"))
        
        ! Process each statement in the program body
        if (allocated(prog%node)) then
            select type (node => prog%node)
            type is (program_node)
                if (allocated(node%body_indices)) then
                    do i = 1, size(node%body_indices)
                        if (node%body_indices(i) > 0) then
                            call infer_and_store_type(ctx, arena, node%body_indices(i))
                        end if
                    end do
                end if
            end select
        end if
        
        call ctx%scopes%pop()
    end subroutine analyze_program_node_arena

    ! Infer type and store result in arena
    subroutine infer_and_store_type(ctx, arena, node_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(mono_type_t) :: inferred_type
        
        ! Use inference dispatcher
        inferred_type = ctx%infer_stmt(arena, node_index)
        
        ! Store type in arena for later use
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                arena%entries(node_index)%node%inferred_type = inferred_type
            end if
        end if
    end subroutine infer_and_store_type

    ! Utility functions for compatibility (implementations moved to inference module)

    ! Utility functions
    subroutine validate_array_bounds(arena, slice_node, result)
        type(ast_arena_t), intent(in) :: arena
        type(ast_entry_t), intent(in) :: slice_node
        type(result_t), intent(out) :: result
        
        result = create_error_result("Array bounds validation not implemented")
    end subroutine validate_array_bounds

    subroutine check_shape_conformance(lhs_shape, rhs_shape, result)
        integer, dimension(:), intent(in) :: lhs_shape, rhs_shape
        type(result_t), intent(out) :: result
        
        result = create_error_result("Shape conformance check not implemented")
    end subroutine check_shape_conformance

    function has_semantic_errors(ctx) result(has_errors)
        type(semantic_context_t), intent(in) :: ctx
        logical :: has_errors
        
        has_errors = ctx%errors%count > 0
    end function has_semantic_errors

    subroutine check_undefined_variables_internal(ctx, arena, prog_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        
        ! Placeholder for undefined variable checking
        continue
    end subroutine check_undefined_variables_internal

    ! Forward declaration to inference module - actual implementation is in semantic_analyzer_inference
    ! This placeholder exists only for bootstrapping
    function infer_statement_type(this, arena, stmt_index) result(typ)
        class(semantic_context_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: stmt_index
        type(mono_type_t) :: typ
        
        ! Simple placeholder - return real type for now
        typ = create_mono_type(TREAL)
    end function infer_statement_type

end module semantic_analyzer_core