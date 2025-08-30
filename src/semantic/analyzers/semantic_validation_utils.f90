module semantic_validation_utils
    ! Utility functions for semantic validation and array operations
    ! Extracted from semantic_analyzer for architectural compliance (Issue #1016)
    use type_system_unified, only: mono_type_t, type_var_t, &
                                   create_mono_type, create_type_var, &
                                   TARRAY, TCHAR, TVAR
    use ast_core, only: ast_arena_t
    use ast_nodes_core, only: identifier_node
    use ast_nodes_bounds, only: array_slice_node
    implicit none
    private

    public :: validate_array_bounds, check_shape_conformance
    public :: update_identifier_type_in_arena
    public :: int_to_str

contains

    ! Character function to convert integer to string  
    character(len=20) function int_to_str(n)
        integer, intent(in) :: n
        write(int_to_str, '(I0)') n
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

        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (identifier_node)
                    if (node%name == name) then
                        arena%entries(i)%node%inferred_type = new_type
                    end if
                end select
            end if
        end do
    end subroutine update_identifier_type_in_arena

end module semantic_validation_utils