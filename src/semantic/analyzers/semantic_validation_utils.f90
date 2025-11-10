module semantic_validation_utils
    ! Utility functions for semantic validation and array operations
    ! Extracted from semantic_analyzer for architectural compliance (Issue #1016)
    use type_system_unified, only: mono_type_t, type_var_t, &
                                   create_mono_type, create_type_var, &
                                   TARRAY, TCHAR, TVAR
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, call_or_subscript_node
    use ast_nodes_bounds, only: array_slice_node
    use string_utils_mod, only: int_to_string
    implicit none
    private

    public :: validate_array_bounds, check_shape_conformance
    public :: update_identifier_type_in_arena
    public :: rename_identifier_in_arena
    public :: int_to_str

contains

    ! Character function to convert integer to string
    character(len=20) function int_to_str(n)
        integer, intent(in) :: n

        int_to_str = int_to_string(n)
    end function int_to_str

    ! Helper functions for validate_array_bounds
    subroutine validate_array_bounds(arena, slice_node, result)
        type(ast_arena_t), intent(in) :: arena
        type(array_slice_node), intent(in) :: slice_node
        logical, intent(out) :: result

        result = .true.  ! Always valid for now
    end subroutine validate_array_bounds

    subroutine check_shape_conformance(lhs_shape, rhs_shape, result)
        integer, intent(in) :: lhs_shape(:), rhs_shape(:)
        logical, intent(out) :: result

        result = size(lhs_shape) == size(rhs_shape)
    end subroutine check_shape_conformance

    ! Helper: Update identifier type throughout arena
    subroutine update_identifier_type_in_arena(arena, name, new_type)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        type(mono_type_t), intent(in) :: new_type
        integer :: i
        type(mono_type_t) :: merged_type

        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (identifier_node)
                    if (node%name == name) then
                        merged_type = new_type
                        call merge_allocatable_flags(node%inferred_type, &
                                                     merged_type)
                        arena%entries(i)%node%inferred_type = merged_type
                    end if
                type is (call_or_subscript_node)
                    if (node%name == name) then
                        merged_type = new_type
                        call merge_allocatable_flags(node%inferred_type, &
                                                     merged_type)
                        arena%entries(i)%node%inferred_type = merged_type
                    end if
                end select
            end if
        end do
    end subroutine update_identifier_type_in_arena

    ! Helper: Rename identifier within function scope
    ! Simple scan-based approach since AST doesn't use parent_index consistently
    subroutine rename_identifier_in_arena(arena, old_name, new_name, &
                                          body_indices, func_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: old_name
        character(len=*), intent(in) :: new_name
        integer, allocatable, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: func_index
        integer :: i, min_idx, max_idx

        ! If scope indices provided, determine the range to scan
        if (present(body_indices) .and. present(func_index)) then
            ! Find min and max indices in body
            min_idx = func_index
            max_idx = func_index
            do i = 1, size(body_indices)
                if (body_indices(i) > 0) then
                    if (min_idx == 0 .or. body_indices(i) < min_idx) then
                        min_idx = body_indices(i)
                    end if
                    if (body_indices(i) > max_idx) max_idx = body_indices(i)
                end if
            end do
            ! Scan arena from min to max+50 (buffer for nested nodes)
            do i = min_idx, min(max_idx + 50, arena%size)
                call rename_at_index(arena, i, old_name, new_name)
            end do
        else
            ! Global rename (fallback)
            do i = 1, arena%size
                call rename_at_index(arena, i, old_name, new_name)
            end do
        end if
    end subroutine rename_identifier_in_arena

    pure subroutine merge_allocatable_flags(source_type, target_type)
        type(mono_type_t), intent(in) :: source_type
        type(mono_type_t), intent(inout) :: target_type

        if (source_type%alloc_info%is_allocatable) then
            target_type%alloc_info%is_allocatable = .true.
        end if
        if (source_type%alloc_info%needs_allocatable_string) then
            target_type%alloc_info%needs_allocatable_string = .true.
        end if
    end subroutine merge_allocatable_flags

    subroutine rename_at_index(arena, idx, old_name, new_name)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: idx
        character(len=*), intent(in) :: old_name
        character(len=*), intent(in) :: new_name
        type(identifier_node) :: temp_identifier

        if (idx <= 0 .or. idx > arena%size) return
        if (.not. allocated(arena%entries(idx)%node)) return

        select type (node => arena%entries(idx)%node)
        type is (identifier_node)
            ! Rename variable references (assignment targets, not function calls)
            if (trim(node%name) == trim(old_name)) then
                temp_identifier = node
                temp_identifier%name = trim(new_name)
                arena%entries(idx)%node = temp_identifier
            end if
        type is (call_or_subscript_node)
            ! EXPLICITLY SKIP: Do NOT rename function/subroutine calls
            ! Function calls should always use the original function name,
            ! not the result variable name. Only identifier_node (assignment targets)
            ! should be renamed.
            ! Even if node%name == old_name, DO NOT CHANGE IT.
            return
        end select
    end subroutine rename_at_index

end module semantic_validation_utils
