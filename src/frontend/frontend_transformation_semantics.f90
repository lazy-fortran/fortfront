module frontend_transformation_semantics
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: mixed_construct_container_node
    use fortfront_constants, only: MAX_DIAGNOSTIC_MESSAGE_LEN
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
        analyze_program, has_semantic_errors
    use semantic_input_mode, only: INPUT_MODE_LAZY
    use call_graph_signatures_mod, only: signatures_map_t, type_signature_t, &
        add_signature, create_signatures_map
    use debug_trace, only: trace_enter, trace_leave
    implicit none
    private

    public :: analyze_container_semantics
    public :: merge_signature_maps
    public :: add_signature_from_entry
    public :: get_detailed_semantic_errors

contains
    subroutine analyze_container_semantics(arena, container, signatures, error_msg)
        type(ast_arena_t), intent(inout) :: arena
        type(mixed_construct_container_node), intent(in) :: container
        type(signatures_map_t), intent(out) :: signatures
        character(len=:), allocatable, intent(inout) :: error_msg
        type(signatures_map_t) :: combined
        type(semantic_context_t) :: local_ctx
        integer :: i, child_idx
        character(len=:), allocatable :: local_error
        logical :: have_error
        character(len=8) :: dump_flag
        integer :: dump_status
        logical :: debug_enabled

        combined = create_signatures_map()
        error_msg = ""
        have_error = .false.
        call get_environment_variable('FORTFRONT_DEBUG_DUMP_AST', dump_flag, &
            status=dump_status)
        debug_enabled = (dump_status == 0 .and. len_trim(dump_flag) > 0)

        if (allocated(container%implicit_declaration_indices)) then
            do i = 1, size(container%implicit_declaration_indices)
                call analyze_program_by_index(container%implicit_declaration_indices(i))
            end do
        end if
        if (allocated(container%explicit_program_indices)) then
            do i = 1, size(container%explicit_program_indices)
                call analyze_program_by_index(container%explicit_program_indices(i))
            end do
        end if

        signatures = combined
        if (.not. have_error) error_msg = ""
    contains
        subroutine analyze_program_by_index(node_idx)
            integer, intent(in) :: node_idx

            if (node_idx < 1 .or. node_idx > arena%size) return
            if (.not. allocated(arena%entries(node_idx)%node)) return

            if (debug_enabled) then
                write (error_unit, '(A,1X,I0)') &
                    'DEBUG semantic container child', node_idx
                if (allocated(arena%entries(node_idx)%node_type)) then
                    write (error_unit, '(A,1X,A)') 'DEBUG node_type', &
                        trim(arena%entries(node_idx)%node_type)
                end if
            end if

            call create_semantic_context(local_ctx)
            local_ctx%input_mode = INPUT_MODE_LAZY

            call trace_enter('semantic:analyze_program')
            call analyze_program(local_ctx, arena, node_idx)
            call trace_leave('semantic:analyze_program')

            if (debug_enabled) then
                write (error_unit, '(A,1X,I0)') 'DEBUG local signatures count', &
                    local_ctx%signatures%proc_count
            end if

            call merge_signature_maps(combined, local_ctx%signatures)

            if (has_semantic_errors(local_ctx)) then
                local_error = get_detailed_semantic_errors(local_ctx)
                if (len_trim(local_error) > 0) then
                    if (have_error) then
                        error_msg = error_msg // new_line('A') // trim(local_error)
                    else
                        error_msg = local_error
                    end if
                    have_error = .true.
                end if
            end if
        end subroutine analyze_program_by_index
    end subroutine analyze_container_semantics

    subroutine merge_signature_maps(target, source)
        type(signatures_map_t), intent(inout) :: target
        type(signatures_map_t), intent(in) :: source
        integer :: i, j

        if (source%proc_count <= 0) return

        do i = 1, source%proc_count
            if (.not. allocated(source%proc_sigs(i)%procedure_name)) cycle
            if (source%proc_sigs(i)%sig_count <= 0) cycle
            do j = 1, source%proc_sigs(i)%sig_count
                call add_signature_from_entry(target, &
                    &                 trim(source%proc_sigs(i)%procedure_name), &
                    &                 source%proc_sigs(i)%signatures(j))
            end do
        end do
    end subroutine merge_signature_maps

    subroutine add_signature_from_entry(target, name, sig)
        type(signatures_map_t), intent(inout) :: target
        character(len=*), intent(in) :: name
        type(type_signature_t), intent(in) :: sig
        integer, allocatable :: kinds(:)
        logical :: has_param_types
        logical :: has_return_type

        if (allocated(sig%param_kinds)) then
            allocate (kinds(size(sig%param_kinds)))
            if (size(sig%param_kinds) > 0) kinds = sig%param_kinds
        else
            allocate (kinds(0))
        end if

        has_param_types = allocated(sig%param_type_strings)
        has_return_type = allocated(sig%return_type_string)

        if (has_param_types) then
            if (has_return_type) then
                call add_signature(target, name, kinds, sig%return_kind, &
                    sig%call_site_node, sig%line, sig%column, &
                    sig%param_type_strings, sig%return_type_string)
            else
                call add_signature(target, name, kinds, sig%return_kind, &
                    sig%call_site_node, sig%line, sig%column, &
                    param_type_strings=sig%param_type_strings)
            end if
        else
            if (has_return_type) then
                call add_signature(target, name, kinds, sig%return_kind, &
                    sig%call_site_node, sig%line, sig%column, &
                    return_type_string=sig%return_type_string)
            else
                call add_signature(target, name, kinds, sig%return_kind, &
                    sig%call_site_node, sig%line, sig%column)
            end if
        end if
    end subroutine add_signature_from_entry

    ! Helper function to get detailed semantic error messages
    function get_detailed_semantic_errors(ctx) result(error_msg)
        type(semantic_context_t), intent(in) :: ctx
        character(len=:), allocatable :: error_msg
        integer :: i, total_errors
        character(len=MAX_DIAGNOSTIC_MESSAGE_LEN) :: buffer

        total_errors = ctx%errors%count
        if (total_errors == 0) then
            error_msg = "No semantic errors found"
            return
        end if

        ! Build comprehensive error message
        write (buffer, '(A,I0,A)') "Found ", total_errors, " semantic error(s):"
        error_msg = trim(buffer)

        ! Add first few error messages for details
        do i = 1, min(3, total_errors)
            if (i <= size(ctx%errors%errors)) then
                if (allocated(ctx%errors%errors(i)%error_message)) then
                    error_msg = error_msg // new_line('a') // "  - " // &
                        ctx%errors%errors(i)%error_message
                    if (allocated(ctx%errors%errors(i)%suggestion)) then
                        error_msg = error_msg // new_line('a') // &
                            "    Suggestion: " // ctx%errors%errors(i)%suggestion
                    end if
                end if
            end if
        end do

        ! Add summary if there are more errors
        if (total_errors > 3) then
            write (buffer, '(A,I0,A)') "  ... and ", (total_errors - 3), &
                " more error(s)"
            error_msg = error_msg // new_line('a') // trim(buffer)
        end if
    end function get_detailed_semantic_errors

end module frontend_transformation_semantics
