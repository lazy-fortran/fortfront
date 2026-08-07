module semantic_analyzer
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
        substitution_t, allocation_info_t
    use scope_manager, only: scope_stack_t, create_scope_stack
    use ast_arena_modern, only: ast_arena_t
    use semantic_validation_utils, only: &
        check_shape_conformance, &
        update_identifier_type_in_arena, int_to_str
    use semantic_function_analysis, only: &
        analyze_function_parameters, &
        determine_function_return_type, &
        create_function_scope
    use semantic_subroutine_analysis, only: analyze_subroutine_parameters, &
        create_subroutine_scope
    use semantic_type_operations, only: generate_fresh_type_var_op, &
        apply_substitution_to_type, &
        generalize_type_op, &
        instantiate_type_scheme_op, get_common_type
    use semantic_assignment_inference, only: process_assignment_inference, &
        ensure_var_declared_from_arena
    use semantic_binary_operations, only: &
        infer_comparison_operation, &
        infer_logical_operation
    use semantic_inference_helpers, only: check_implicit_none, &
        process_if_node_branches, &
        process_do_loop_body, &
        process_do_while_node_body, &
        process_where_node_clauses, &
        process_where_stmt_node, &
        process_forall_node_body, &
        process_select_case_blocks, &
        process_associate_node_body, &
        process_stop_node_code, &
        process_pause_node_code, &
        process_nullify_node_code, &
        process_declaration_variables
    use parser_type_hooks_module, only: type_annotation_t, &
        consume_type_annotations, has_type_annotations
    use semantic_annotation_utils, only: type_from_annotation
    use semantic_literal_identifier, only: infer_literal_type, infer_identifier_type
    use semantic_binary_ops_core, only: infer_binary_operation
    use semantic_function_call, only: infer_function_call_type
    use semantic_array_slice, only: infer_array_slice_type
    use semantic_array_literal, only: infer_array_literal_type
    use lexer_core, only: to_lower
    use ast_base, only: LITERAL_STRING
    use ast_nodes_core, only: literal_node, identifier_node, binary_op_node, &
        assignment_node, pointer_assignment_node, &
        call_or_subscript_node, &
        array_literal_node, program_node
    use ast_nodes_procedure, only: subroutine_call_node, function_def_node, &
        subroutine_def_node
    use ast_nodes_control, only: do_loop_node, if_node, do_while_node, where_node, &
        where_stmt_node, forall_node, select_case_node, &
        case_block_node, associate_node, association_t, &
        cycle_node, exit_node, stop_node, return_node, &
        entry_node, continue_node, elsewhere_clause_t, &
        pause_node, nullify_node
    use ast_nodes_data, only: declaration_node, &
        module_node, derived_type_node, &
        multi_unit_container_node
    use ast_nodes_bounds, only: array_slice_node, &
        array_bounds_node, range_expression_node, &
        get_array_slice_node
    use ast_nodes_misc, only: complex_literal_node, allocate_statement_node, &
        data_statement_node
    use ast_nodes_io, only: read_statement_node, print_statement_node
    use constant_transformation, only: fold_constants_in_arena
    use error_handling, only: error_collection_t, create_error_collection, result_t, &
        create_error_result, ERROR_SEMANTIC
    use semantic_context_types, only: semantic_context_base_t
    use semantic_undefined_variable_checker, only: check_undefined_variables_generic
    use type_hierarchy, only: type_hierarchy_t, create_type_hierarchy
    use call_graph_signatures_mod, only: signatures_map_t, create_signatures_map
    use identifier_table, only: identifier_table_t
    use semantic_call_signature_collector, only: collect_call_signature, &
        collect_subroutine_signature
    use semantic_input_mode, only: INPUT_MODE_LAZY, INPUT_MODE_STANDARD
    use semantic_operating_mode, only: OPERATING_MODE_INFER
    implicit none
    private

    public :: semantic_context_t
    public :: create_semantic_context
    public :: analyze_program
    public :: has_semantic_errors
    ! nvfortran 26.5 lowers calls to these submodule procedures as external
    ! references; the compatibility shim provides those two ABI entry points.
    public :: infer_allocate_statement, set_node_inferred_type

    type, extends(semantic_context_base_t) :: semantic_context_t
        type(scope_stack_t) :: scopes
        integer :: next_var_id = 0
        type(substitution_t) :: subst
        type(error_collection_t) :: errors
        integer :: input_mode = INPUT_MODE_LAZY
        integer :: operating_mode = OPERATING_MODE_INFER
        logical :: respect_implicit_none = .true.
        ! Compiler/API analysis enforces IMPLICIT NONE references; Lazy
        ! transformation retains its historical inference boundary.
        logical :: enforce_implicit_none_references = .true.
        type(type_annotation_t), allocatable :: parser_type_hints(:)
        type(type_hierarchy_t) :: type_hierarchy
        type(signatures_map_t) :: signatures
        type(identifier_table_t) :: explicit_interface_procedure_names
        integer :: explicit_interface_cache_arena_size = 0
        logical :: explicit_interface_cache_valid = .false.
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

    interface
        module subroutine create_semantic_context(ctx)
            type(semantic_context_t), intent(out) :: ctx
        end subroutine create_semantic_context

        module subroutine analyze_program(ctx, arena, root_index)
            type(semantic_context_t), intent(inout) :: ctx
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in) :: root_index
        end subroutine analyze_program

        module subroutine analyze_program_node_arena(ctx, arena, prog, prog_index)
            type(semantic_context_t), intent(inout) :: ctx
            type(ast_arena_t), intent(inout) :: arena
            type(program_node), intent(inout) :: prog
            integer, intent(in) :: prog_index
        end subroutine analyze_program_node_arena

        module subroutine infer_and_store_type(ctx, arena, node_index)
            type(semantic_context_t), intent(inout) :: ctx
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in) :: node_index
        end subroutine infer_and_store_type

        module function infer_statement_type(this, arena, stmt_index) result(typ)
            class(semantic_context_t), intent(inout) :: this
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in) :: stmt_index
            type(mono_type_t) :: typ
        end function infer_statement_type

        module function infer_type(this, arena, expr_index) result(typ)
            class(semantic_context_t), intent(inout), target :: this
            type(ast_arena_t), intent(inout), target :: arena
            integer, intent(in) :: expr_index
            type(mono_type_t) :: typ
        end function infer_type

        module subroutine set_node_inferred_type(arena, index, typ)
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in) :: index
            type(mono_type_t), intent(in) :: typ
        end subroutine set_node_inferred_type

        module function get_inferred_type_from_arena(ctx, arena, index) result(typ)
            class(semantic_context_t), intent(inout) :: ctx
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in) :: index
            type(mono_type_t) :: typ
        end function get_inferred_type_from_arena

        module subroutine unify_types(this, t1, t2)
            class(semantic_context_t), intent(inout) :: this
            type(mono_type_t), intent(in) :: t1
            type(mono_type_t), intent(in) :: t2
        end subroutine unify_types

        module function instantiate_type_scheme(this, scheme) result(typ)
            class(semantic_context_t), intent(inout) :: this
            type(poly_type_t), intent(in) :: scheme
            type(mono_type_t) :: typ
        end function instantiate_type_scheme

        module function generalize_type(this, typ) result(scheme)
            class(semantic_context_t), intent(in) :: this
            type(mono_type_t), intent(in) :: typ
            type(poly_type_t) :: scheme
        end function generalize_type

        module function generate_fresh_type_var(this) result(tv)
            class(semantic_context_t), intent(inout) :: this
            type(type_var_t) :: tv
        end function generate_fresh_type_var

        module function apply_current_substitution(this, typ) result(result_type)
            class(semantic_context_t), intent(in) :: this
            type(mono_type_t), intent(in) :: typ
            type(mono_type_t) :: result_type
        end function apply_current_substitution

        module function get_builtin_function_type(this, name) result(typ)
            class(semantic_context_t), intent(inout) :: this
            character(len=*), intent(in) :: name
            type(mono_type_t) :: typ
        end function get_builtin_function_type

        module subroutine compose_with_subst(this, new_subst)
            class(semantic_context_t), intent(inout) :: this
            type(substitution_t), intent(in) :: new_subst
        end subroutine compose_with_subst

        module subroutine semantic_context_deep_copy(this, copy)
            class(semantic_context_t), intent(in) :: this
            type(semantic_context_t), intent(out) :: copy
        end subroutine semantic_context_deep_copy

        module subroutine semantic_context_assign(lhs, rhs)
            class(semantic_context_t), intent(inout) :: lhs
            type(semantic_context_t), intent(in) :: rhs
        end subroutine semantic_context_assign

        module function semantic_context_has_errors(this) result(has_errors)
            class(semantic_context_t), intent(in) :: this
            logical :: has_errors
        end function semantic_context_has_errors

        module function infer_assignment(ctx, arena, assignment, assignment_index) &
                result(typ)
            type(semantic_context_t), intent(inout) :: ctx
            type(ast_arena_t), intent(inout) :: arena
            type(assignment_node), intent(in) :: assignment
            integer, intent(in) :: assignment_index
            type(mono_type_t) :: typ
        end function infer_assignment

        module function infer_pointer_assignment(ctx, arena, ptr_assign, &
                ptr_assign_index) result(typ)
            type(semantic_context_t), intent(inout) :: ctx
            type(ast_arena_t), intent(inout) :: arena
            type(pointer_assignment_node), intent(in) :: ptr_assign
            integer, intent(in) :: ptr_assign_index
            type(mono_type_t) :: typ
        end function infer_pointer_assignment

        module subroutine infer_read_statement(ctx, arena, read_stmt, stmt_index, typ)
            type(semantic_context_t), intent(inout) :: ctx
            type(ast_arena_t), intent(inout) :: arena
            type(read_statement_node), intent(in) :: read_stmt
            integer, intent(in) :: stmt_index
            type(mono_type_t), intent(out) :: typ
        end subroutine infer_read_statement

        module subroutine infer_allocate_statement(ctx, arena, alloc_stmt, &
                stmt_index, &
                typ)
            type(semantic_context_t), intent(inout) :: ctx
            type(ast_arena_t), intent(inout) :: arena
            type(allocate_statement_node), intent(in) :: alloc_stmt
            integer, intent(in) :: stmt_index
            type(mono_type_t), intent(out) :: typ
        end subroutine infer_allocate_statement

        module subroutine ensure_string_literal_type(arena, value_index, expr_typ)
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in) :: value_index
            type(mono_type_t), intent(inout) :: expr_typ
        end subroutine ensure_string_literal_type

        module function has_semantic_errors(ctx) result(has_errors)
            type(semantic_context_t), intent(in) :: ctx
            logical :: has_errors
        end function has_semantic_errors

        module function semantic_get_context_name(this) result(name)
            class(semantic_context_t), intent(in) :: this
            character(:), allocatable :: name
        end function semantic_get_context_name

        module function semantic_clone_context(this) result(cloned)
            class(semantic_context_t), intent(in) :: this
            class(semantic_context_base_t), allocatable :: cloned
        end function semantic_clone_context

        module function semantic_get_type_hint(this, decl_index, annotation) &
                result(found)
            class(semantic_context_t), intent(in) :: this
            integer, intent(in) :: decl_index
            type(type_annotation_t), intent(out) :: annotation
            logical :: found
        end function semantic_get_type_hint
    end interface

end module semantic_analyzer
