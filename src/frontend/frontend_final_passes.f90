module frontend_final_passes
    ! Pass implementations for the final transformation phases
    ! (semantic analysis, standardization, monomorphization, codegen)
    use, intrinsic :: iso_fortran_env, only: error_unit
    use frontend_pass_manager, only: pass_context_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                 analyze_program, has_semantic_errors
    use semantic_input_mode, only: INPUT_MODE_LAZY
    use ast_nodes_data, only: mixed_construct_container_node
    use ast_nodes_core, only: program_node
    use call_graph_signatures_mod, only: create_signatures_map
    use frontend_transformation_semantics, only: analyze_container_semantics, &
                                                 get_detailed_semantic_errors
    use standardizer, only: standardize_ast, mark_pointer_targets
    use frontend_transformation_structure, only: normalize_multi_unit_container, &
                                                 run_code_generation_phase
    use ast_monomorphization, only: transform_monomorphization
    use codegen_arena_interface, only: generate_code_from_arena
    use frontend_transformation_analysis, only: analyze_ast_content, &
                                                promote_functions_to_internal_program, &
                                                requires_lazy_internalization, &
                                                has_existing_module_in_ast
    use frontend_transformation_structure, only: wrap_ast_in_module_only
    use frontend_transformation_common, only: transform_context_t
    use semantic_input_mode, only: INPUT_MODE_LAZY
    implicit none
    private

    public :: semantic_pass, standardization_pass, monomorphization_pass, &
              codegen_pass

contains

    ! Semantic analysis pass - type inference and validation
    subroutine semantic_pass(context)
        type(pass_context_t), intent(inout) :: context
        type(semantic_context_t) :: ctx
        logical :: handled

        call context%compiler_arena%next_phase("semantic")

        handled = .false.
        call create_semantic_context(ctx)
        ctx%input_mode = INPUT_MODE_LAZY
        ctx%operating_mode = context%operating_mode

        if (context%prog_index > 0 .and. &
            context%prog_index <= context%compiler_arena%ast%size) then
            if (allocated(context%compiler_arena%ast%entries( &
                          context%prog_index)%node)) then
                select type (root_node => context%compiler_arena%ast%entries( &
                             context%prog_index)%node)
                type is (mixed_construct_container_node)
                    call analyze_container_semantics(context%compiler_arena%ast, &
                                                     root_node, &
                                                     context%signatures, &
                                                     context%error_msg)
                    if (len(context%error_msg) > 0) then
                        ! Generate output even with semantic errors
                        call run_code_generation_phase(context%compiler_arena, &
                                                       context%prog_index, &
                                                       context%output)
                        return
                    end if
                    handled = .true.
                class default
                    call analyze_program(ctx, context%compiler_arena%ast, &
                                         context%prog_index)
                    context%signatures = ctx%signatures
                    if (has_semantic_errors(ctx)) then
                        context%error_msg = get_detailed_semantic_errors(ctx)
                        ! Generate output even with semantic errors
                        call run_code_generation_phase(context%compiler_arena, &
                                                       context%prog_index, &
                                                       context%output)
                        return
                    end if
                    handled = .true.
                end select
            end if
        end if

        if (.not. handled) then
            context%signatures = create_signatures_map()
        end if

        context%error_msg = ""
    end subroutine semantic_pass

    ! Standardization pass - normalize AST structure
    subroutine standardization_pass(context)
        type(pass_context_t), intent(inout) :: context
        logical :: skip_standardization

        call context%compiler_arena%next_phase("standardization")

        ! Normalize multi-unit containers
        call normalize_multi_unit_container(context%compiler_arena%ast, &
                                            context%prog_index)

        ! Check if we should skip standardization for multi-unit containers
        skip_standardization = .false.
        if (context%prog_index > 0 .and. &
            context%prog_index <= context%compiler_arena%ast%size) then
            if (allocated(context%compiler_arena%ast%entries( &
                          context%prog_index)%node)) then
                select type (node => context%compiler_arena%ast%entries( &
                             context%prog_index)%node)
                type is (program_node)
                    if (node%name == "__MULTI_UNIT__") then
                        skip_standardization = .true.
                    end if
                end select
            end if
        end if

        if (.not. skip_standardization) then
            call standardize_ast(context%compiler_arena%ast, context%prog_index)
        end if

        call mark_pointer_targets(context%compiler_arena%ast)
    end subroutine standardization_pass

    ! Monomorphization pass - specialize generic code
    subroutine monomorphization_pass(context)
        type(pass_context_t), intent(inout) :: context

        call context%compiler_arena%next_phase("monomorphization")
        call transform_monomorphization(context%compiler_arena%ast, &
                                        context%prog_index, context%signatures)

        ! Analyze AST content after monomorphization
        call analyze_ast_content(context%compiler_arena%ast, context%prog_index, &
                                 context%has_functions, context%has_subroutines, &
                                 context%has_main_code)
    end subroutine monomorphization_pass

    ! Code generation pass - emit Fortran source
    subroutine codegen_pass(context)
        type(pass_context_t), intent(inout) :: context
        logical :: force_internal_wrapping
        type(transform_context_t) :: transform_ctx

        ! Determine if AST wrapping is needed
        force_internal_wrapping = requires_lazy_internalization( &
                                  context%compiler_arena%ast, context%prog_index)

        ! Initialize default context for wrapping
        transform_ctx%source_name = "main"
        transform_ctx%module_name = "main_module"
        transform_ctx%program_name = "main"
        transform_ctx%has_filename = .false.
        transform_ctx%input_mode = INPUT_MODE_LAZY

        if (.not. has_existing_module_in_ast(context%compiler_arena%ast)) then
            if ((context%has_functions .or. context%has_subroutines) .and. &
                context%has_main_code) then
                if (context%enable_ast_wrapping .or. force_internal_wrapping) then
                    call promote_functions_to_internal_program( &
                        context%compiler_arena%ast, context%prog_index)
                end if
            else if (context%enable_ast_wrapping .and. &
                     (context%has_functions .or. context%has_subroutines) .and. &
                     .not. context%has_main_code) then
                call wrap_ast_in_module_only(context%compiler_arena%ast, &
                                             context%prog_index, transform_ctx)
            end if
        end if

        ! Generate code
        call run_code_generation_phase(context%compiler_arena, &
                                       context%prog_index, context%output)
    end subroutine codegen_pass

end module frontend_final_passes
