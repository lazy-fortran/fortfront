submodule(semantic_analyzer) semantic_analyzer_context_impl
use, intrinsic :: iso_fortran_env, only: error_unit, int32
use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
    create_mono_type, create_fun_type, &
    create_poly_type, &
    TVAR, TREAL
use scope_manager, only: create_scope_stack
use parser_type_hooks_module, only: consume_type_annotations, &
    has_type_annotations
use semantic_annotation_utils, only: type_from_annotation
use semantic_inference_helpers, only: check_implicit_none, &
    process_declaration_variables
use semantic_undefined_variable_checker, only: check_undefined_variables_generic
use semantic_walrus_checker, only: check_walrus_redeclaration
use constant_transformation, only: fold_constants_in_arena
use error_handling, only: create_error_collection
use type_hierarchy, only: create_type_hierarchy
use semantic_type_hierarchy_validation, only: populate_type_hierarchy
use semantic_enum_validation, only: validate_enum_definitions
use semantic_literal_form_validation, only: validate_literal_forms
use semantic_use_nature_validation, only: validate_use_module_nature
use semantic_local_name_collision_validation, only: &
    validate_local_name_collisions
use semantic_submodule_validation, only: validate_submodule_interfaces
use semantic_bind_c_validation, only: validate_global_binding_labels
use call_graph_signatures_mod, only: create_signatures_map
use semantic_validation_utils, only: int_to_str
use ast_nodes_data, only: declaration_node
use semantic_context_types, only: semantic_context_base_t
use semantic_input_mode, only: INPUT_MODE_LAZY, INPUT_MODE_STANDARD
use semantic_operating_mode, only: OPERATING_MODE_INFER, &
    OPERATING_MODE_STRICT
use debug_trace, only: trace_is_enabled
implicit none
contains

    module subroutine create_semantic_context(ctx)
        type(semantic_context_t), intent(out) :: ctx
        type(poly_type_t) :: builtin_scheme
        type(mono_type_t) :: real_to_real, real_type

        ctx%context_id = 1
        ctx%context_name = "semantic_context"

        call create_scope_stack(ctx%scopes)
        ctx%subst%count = 0
        ctx%subst%capacity = 64
        if (allocated(ctx%subst%vars)) deallocate (ctx%subst%vars)
        if (allocated(ctx%subst%types)) deallocate (ctx%subst%types)
        allocate (ctx%subst%vars(ctx%subst%capacity))
        allocate (ctx%subst%types(ctx%subst%capacity))
        ctx%errors = create_error_collection()
        ctx%next_var_id = 1
        ctx%operating_mode = OPERATING_MODE_INFER
        ctx%respect_implicit_none = .true.
        ctx%type_hierarchy = create_type_hierarchy()
        ctx%signatures = create_signatures_map()
        ctx%explicit_interface_procedure_names%count = 0_int32
        ctx%explicit_interface_procedure_names%entry_capacity = 0_int32
        ctx%explicit_interface_procedure_names%bucket_count = 0_int32
        if (allocated(ctx%explicit_interface_procedure_names%entries)) then
            deallocate (ctx%explicit_interface_procedure_names%entries)
        end if
        if (allocated(ctx%explicit_interface_procedure_names%buckets)) then
            deallocate (ctx%explicit_interface_procedure_names%buckets)
        end if
        ctx%explicit_interface_cache_arena_size = 0
        ctx%explicit_interface_cache_valid = .false.

        real_type = create_mono_type(TREAL)
        real_to_real = create_fun_type(real_type, real_type)

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

    module subroutine analyze_program(ctx, arena, root_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index

        if (.not. arena%has_node_at(root_index)) return

        ! Register derived types and their EXTENDS parents so polymorphic
        ! resolution can consult the (now populated) inheritance hierarchy.
        call populate_type_hierarchy(arena, ctx%type_hierarchy, ctx%errors)

        ! Report ENUM constraint violations recorded by the parser. The sweep
        ! is arena-wide so module-level enumerations are covered too.
        call validate_enum_definitions(arena, ctx%errors)

        ! Reject malformed or disallowed literal forms before inference runs.
        call validate_literal_forms(arena, ctx%errors, &
                                    ctx%input_mode == INPUT_MODE_STANDARD)

        ! Whole-arena scoping-unit checks. They run for every root kind,
        ! including a bare module root, which the dispatch below skips.
        call validate_use_module_nature(arena, ctx%errors)
        call validate_local_name_collisions(arena, ctx%errors)

        ! Separate module subprograms are checked against the interface bodies
        ! of their ancestor module, which only the whole arena exposes.
        call validate_submodule_interfaces(arena, ctx%errors)

        ! Reject duplicate global BIND(C) binding labels (F2018 C1553). The
        ! binding label namespace spans the whole compilation unit, so this
        ! runs once over the arena rather than per scoping unit.
        call validate_global_binding_labels(arena, ctx%errors)

        if (trace_is_enabled()) then
            select type (ast => arena%entries(root_index)%node)
                type is (program_node)
                write (error_unit, '(A)') 'TRACE: Root is program_node'
                type is (module_node)
                write (error_unit, '(A)') 'TRACE: Root is module_node'
            class default
                write (error_unit, '(A)') 'TRACE: Root is OTHER node type'
            end select
        end if

        select type (ast => arena%entries(root_index)%node)
            type is (program_node)
            if (trace_is_enabled()) then
                write (error_unit, '(A,L1,A,I0)') 'TRACE: respect_implicit_none=', &
                    ctx%respect_implicit_none, ' input_mode=', ctx%input_mode
            end if
            if (ctx%respect_implicit_none .and. ctx%input_mode == INPUT_MODE_LAZY) then
                if (ctx%operating_mode == OPERATING_MODE_INFER) then
                    if (check_implicit_none(arena, ast)) then
                        ctx%input_mode = INPUT_MODE_STANDARD
                    end if
                end if
            end if
            call analyze_program_node_arena(ctx, arena, ast, root_index)
            type is (multi_unit_container_node)
            call analyze_multi_unit_container_arena(ctx, arena, ast, root_index)
            type is (module_node)
            return
        class default
            call infer_and_store_type(ctx, arena, root_index)
        end select

        ! Disabled for performance - constant folding causes O(n²) behavior
        ! on large files and is not needed for standard Fortran round-trip
        ! call fold_constants_in_arena(arena)
    end subroutine analyze_program

    module subroutine analyze_program_node_arena(ctx, arena, prog, prog_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: prog_index
        integer :: i

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
                                        call ctx%scopes%define(node%var_names(j), &
                                            scheme)
                                    end do
                                else if (allocated(node%var_name)) then
                                    call ctx%scopes%define(node%var_name, scheme)
                                end if
                                ! NOTE: update_identifier_type_in_arena is available
                                ! but not needed here.
                            end block
                        class default
                            continue
                        end select
                    end if
                end if
            end do
        end if

        if (ctx%operating_mode == OPERATING_MODE_STRICT) then
            call check_undefined_variables_generic(ctx%scopes, ctx%errors, &
                ctx%input_mode, arena, &
                prog_index)
            if (ctx%errors%has_errors()) return
        end if

        call check_walrus_redeclaration(ctx%errors, ctx%input_mode, arena, &
            prog_index)
        if (ctx%errors%has_errors()) return

        ! Enable constant folding for lazy Fortran to support constant propagation
        ! (e.g., n=3; reshape([...], [n,n]) needs to know n=3)
        ! MUST happen BEFORE type inference so reshape can see constant values
        ! Skip for standard Fortran to avoid O(n²) performance issues
        if (trace_is_enabled()) then
            write (error_unit, '(A,I0)') 'TRACE: input_mode=', ctx%input_mode
        end if
        if (ctx%input_mode == INPUT_MODE_LAZY) then
            call fold_constants_in_arena(arena)
        end if

        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. &
                    prog%body_indices(i) <= arena%size) then
                    call infer_and_store_type(ctx, arena, prog%body_indices(i))
                end if
            end do
        end if
        call check_undefined_variables_generic(ctx%scopes, ctx%errors, &
            ctx%input_mode, arena, prog_index)
    end subroutine analyze_program_node_arena

    ! All units of a multi-unit container share one context: a call in one
    ! unit must unify argument types into a procedure defined in a sibling
    ! unit and collect its call signature (the pre-container sentinel-program
    ! behavior, where the container was itself a program_node).
    subroutine analyze_multi_unit_container_arena(ctx, arena, container, &
            container_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        type(multi_unit_container_node), intent(inout) :: container
        integer, intent(in) :: container_index
        integer :: i

        if (.not. allocated(container%body_indices)) return

        if (ctx%operating_mode == OPERATING_MODE_STRICT) then
            call check_undefined_variables_generic(ctx%scopes, ctx%errors, &
                ctx%input_mode, arena, &
                container_index)
            if (ctx%errors%has_errors()) return
        end if

        call check_walrus_redeclaration(ctx%errors, ctx%input_mode, arena, &
            container_index)
        if (ctx%errors%has_errors()) return

        if (ctx%input_mode == INPUT_MODE_LAZY) then
            call fold_constants_in_arena(arena)
        end if

        do i = 1, size(container%body_indices)
            if (container%body_indices(i) > 0 .and. &
                container%body_indices(i) <= arena%size) then
                call infer_and_store_type(ctx, arena, container%body_indices(i))
            end if
        end do
        call check_undefined_variables_generic(ctx%scopes, ctx%errors, &
            ctx%input_mode, arena, &
            container_index)
    end subroutine analyze_multi_unit_container_arena

    module subroutine infer_and_store_type(ctx, arena, node_index)
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(mono_type_t) :: inferred

        if (.not. arena%has_node_at(node_index)) return

        inferred = ctx%infer_stmt(arena, node_index)
    end subroutine infer_and_store_type

    module subroutine set_node_inferred_type(arena, index, typ)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: index
        type(mono_type_t), intent(in) :: typ

        if (.not. arena%has_node_at(index)) return
        arena%entries(index)%node%inferred_type = typ
    end subroutine set_node_inferred_type

    module function get_inferred_type_from_arena(ctx, arena, index) result(typ)
        class(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: index
        type(mono_type_t) :: typ

        typ = create_mono_type(TREAL)
        if (.not. arena%has_node_at(index)) return

        typ = ctx%apply_subst_to_type(arena%entries(index)%node%inferred_type)
        if (typ%kind == TVAR) then
            if (len_trim(typ%var%name) == 0) typ%var%name = "v"// &
                int_to_str(typ%var%id)
        end if
        arena%entries(index)%node%inferred_type = typ
    end function get_inferred_type_from_arena

    module function has_semantic_errors(ctx) result(has_errors)
        type(semantic_context_t), intent(in) :: ctx
        logical :: has_errors
        has_errors = ctx%errors%has_errors()
    end function has_semantic_errors

    module function semantic_get_context_name(this) result(name)
        class(semantic_context_t), intent(in) :: this
        character(:), allocatable :: name
        name = "semantic_context"
    end function semantic_get_context_name

    module function semantic_clone_context(this) result(cloned)
        class(semantic_context_t), intent(in) :: this
        class(semantic_context_base_t), allocatable :: cloned
        type(semantic_context_t) :: temp_context

        temp_context%context_id = this%context_id
        temp_context%context_name = this%context_name
        temp_context%scopes = this%scopes
        temp_context%next_var_id = this%next_var_id
        temp_context%subst = this%subst
        temp_context%errors = this%errors
        temp_context%input_mode = this%input_mode
        temp_context%operating_mode = this%operating_mode
        temp_context%respect_implicit_none = this%respect_implicit_none
        temp_context%signatures = this%signatures

        allocate (cloned, source=temp_context)
    end function semantic_clone_context

    module function semantic_get_type_hint(this, decl_index, annotation) &
            result(found)
        class(semantic_context_t), intent(in) :: this
        integer, intent(in) :: decl_index
        type(type_annotation_t), intent(out) :: annotation
        integer :: i
        logical :: found

        found = .false.
        if (.not. allocated(this%parser_type_hints)) return
        if (size(this%parser_type_hints) == 0) return

        do i = 1, size(this%parser_type_hints)
            if (this%parser_type_hints(i)%decl_index == decl_index) then
                annotation = this%parser_type_hints(i)
                found = .true.
                return
            end if
        end do
    end function semantic_get_type_hint

end submodule semantic_analyzer_context_impl
