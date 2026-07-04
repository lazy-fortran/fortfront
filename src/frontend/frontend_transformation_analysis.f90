module frontend_transformation_analysis
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_data, only: mixed_construct_container_node, &
        multi_unit_container_node
    use frontend_analysis_helpers, only: build_procedure_membership, &
        analyze_ast_content, analyze_single_unit, &
        collect_host_assignment_names, &
        collect_program_assignment_names, &
        collect_procedure_assignment_names, &
        collect_assignment_from_node, &
        record_identifier_name, append_unique_name, &
        requires_lazy_internalization, &
        has_existing_module_in_ast
    use frontend_program_builders, only: handle_mixed_construct_container, &
        scan_multi_unit_program, &
        create_program_from_bare_statements, &
        merge_procedures_into_program, &
        filter_procs_with_entry
    implicit none
    private

    public :: build_procedure_membership
    public :: analyze_ast_content
    public :: analyze_single_unit
    public :: collect_host_assignment_names
    public :: collect_program_assignment_names
    public :: collect_procedure_assignment_names
    public :: collect_assignment_from_node
    public :: record_identifier_name
    public :: append_unique_name
    public :: promote_functions_to_internal_program
    public :: requires_lazy_internalization
    public :: has_existing_module_in_ast

contains

    subroutine promote_functions_to_internal_program(arena, root_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        integer :: main_prog_index, candidate_prog_index
        integer, allocatable :: proc_indices(:)
        integer, allocatable :: main_stmts(:)

        if (.not. arena%has_node_at(root_index)) return

        allocate (proc_indices(0))
        allocate (main_stmts(0))
        main_prog_index = 0
        candidate_prog_index = 0

        select type (root => arena%entries(root_index)%node)
            type is (mixed_construct_container_node)
            call handle_mixed_construct_container(arena, root_index, root, &
                proc_indices, main_stmts)
            return
            type is (multi_unit_container_node)
            call scan_multi_unit_program(arena, root, main_prog_index, &
                candidate_prog_index, proc_indices, main_stmts)
            type is (program_node)
            call scan_multi_unit_program(arena, root, main_prog_index, &
                candidate_prog_index, proc_indices, main_stmts)
        class default
            return
        end select

        ! Filter out procedures with ENTRY statements - they cannot be contained
        ! in internal program procedures per ISO/IEC 1539-1:2018 Section 15.6.2.6.
        call filter_procs_with_entry(arena, proc_indices)

        call create_program_from_bare_statements(arena, root_index, &
            main_prog_index, proc_indices, &
            main_stmts)
        if (main_prog_index > 0 .and. size(proc_indices) == 0) return

        call merge_procedures_into_program(arena, main_prog_index, &
            proc_indices, main_stmts)

        if (main_prog_index > 0) root_index = main_prog_index
    end subroutine promote_functions_to_internal_program

end module frontend_transformation_analysis
