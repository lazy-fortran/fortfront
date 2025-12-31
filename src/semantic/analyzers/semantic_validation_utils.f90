module semantic_validation_utils
    ! Utility functions for semantic validation and array operations
    ! Extracted from semantic_analyzer for architectural compliance (Issue #1016)
    use type_system_unified, only: mono_type_t, type_var_t, &
                                   create_mono_type, create_type_var, &
                                   TARRAY, TCHAR, TVAR
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, call_or_subscript_node
    use ast_nodes_data, only: declaration_node
    use ast_nodes_bounds, only: array_slice_node
    use string_utils_mod, only: int_to_string
    use semantic_input_mode, only: INPUT_MODE_LAZY, INPUT_MODE_STANDARD
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
    ! PERFORMANCE: This is O(n²) when called repeatedly - only needed for lazy Fortran
    subroutine update_identifier_type_in_arena(arena, name, new_type, input_mode)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        type(mono_type_t), intent(in) :: new_type
        integer, intent(in), optional :: input_mode
        integer :: i
        type(mono_type_t) :: merged_type

        ! PERFORMANCE NOTE: This is O(n) per call, O(n²) when called repeatedly
        !
        ! Input modes:
        ! - INPUT_MODE_LAZY: Type propagation required (lazy Fortran .lf files)
        ! - INPUT_MODE_STANDARD: Skip propagation (standard .f90 with implicit none)
        ! - Standard .f90 WITHOUT implicit none: Currently unsupported (future work)
        if (present(input_mode)) then
            if (input_mode == INPUT_MODE_STANDARD) return  ! Skip propagation if standard mode
        end if
        ! Default: Always propagate (needed for lazy Fortran type inference)

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
    ! Uses parent_index chain to ensure nodes belong to the specified function
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
            ! But verify each node belongs to the function via parent chain
            do i = min_idx, min(max_idx + 50, arena%size)
                if (node_belongs_to_function(arena, i, func_index)) then
                    call rename_at_index(arena, i, old_name, new_name)
                end if
            end do
        else
            ! Global rename (fallback)
            do i = 1, arena%size
                call rename_at_index(arena, i, old_name, new_name)
            end do
        end if
    end subroutine rename_identifier_in_arena

    ! Check if a node belongs to a function by following parent_index chain
    logical function node_belongs_to_function(arena, node_idx, func_index) &
        result(belongs)
        use ast_nodes_procedure, only: function_def_node, subroutine_def_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        integer, intent(in) :: func_index
        integer :: current_idx, max_depth

        belongs = .false.
        if (node_idx <= 0 .or. node_idx > arena%size) return
        if (func_index <= 0 .or. func_index > arena%size) return

        ! Direct match
        if (node_idx == func_index) then
            belongs = .true.
            return
        end if

        ! Follow parent chain up to the function
        current_idx = node_idx
        max_depth = 100  ! Prevent infinite loops
        do while (max_depth > 0)
            max_depth = max_depth - 1
            if (current_idx <= 0 .or. current_idx > arena%size) exit
            if (.not. allocated(arena%entries(current_idx)%node)) exit

            ! Check if we reached the target function
            if (current_idx == func_index) then
                belongs = .true.
                return
            end if

            ! Check if we hit a DIFFERENT function/subroutine (sibling scope)
            if (current_idx /= node_idx) then
                select type (node => arena%entries(current_idx)%node)
                type is (function_def_node)
                    ! Found a different function before reaching our target
                    if (current_idx /= func_index) return
                type is (subroutine_def_node)
                    ! Found a subroutine, node doesn't belong to our function
                    return
                end select
            end if

            ! Move up to parent
            current_idx = arena%entries(current_idx)%parent_index
        end do
    end function node_belongs_to_function

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
        type(declaration_node) :: temp_decl

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
        type is (declaration_node)
            ! Rename declarations (e.g., function result variable declarations)
            ! BUT: Do NOT rename EXTERNAL declarations - they refer to function names,
            ! not result variables!
            if (node%is_external) return

            if (allocated(node%var_name)) then
                if (trim(node%var_name) == trim(old_name)) then
                    temp_decl = node
                    temp_decl%var_name = trim(new_name)
                    arena%entries(idx)%node = temp_decl
                end if
            end if
        end select
    end subroutine rename_at_index

end module semantic_validation_utils
