module standardizer_module
    ! Module-specific transformations module
    ! Handles module node standardization and content processing
    
    use ast_core
    implicit none
    private

    public :: standardize_module
    
    ! Forward declaration - this will be resolved by compiler
    ! when standardizer_core is compiled
    interface
        recursive subroutine standardize_ast(arena, root_index, in_module)
            import :: ast_arena_t
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(inout) :: root_index
            logical, intent(in), optional :: in_module
        end subroutine standardize_ast
    end interface

contains

    ! Standardize a module node
    subroutine standardize_module(arena, mod, mod_index)
        type(ast_arena_t), intent(inout) :: arena
        type(module_node), intent(inout) :: mod
        integer, intent(in) :: mod_index
        integer :: i
        
        ! Standardize declarations in the module (pass in_module=.true.)
        if (allocated(mod%declaration_indices)) then
            do i = 1, size(mod%declaration_indices)
                if (mod%declaration_indices(i) > 0 .and. &
                    mod%declaration_indices(i) <= arena%size) then
                    call standardize_ast(arena, mod%declaration_indices(i), &
                                       in_module=.true.)
                end if
            end do
        end if
        
        ! Standardize procedures in the module (pass in_module=.true.)
        if (allocated(mod%procedure_indices)) then
            do i = 1, size(mod%procedure_indices)
                if (mod%procedure_indices(i) > 0 .and. &
                    mod%procedure_indices(i) <= arena%size) then
                    call standardize_ast(arena, mod%procedure_indices(i), &
                                       in_module=.true.)
                end if
            end do
        end if
    end subroutine standardize_module

end module standardizer_module