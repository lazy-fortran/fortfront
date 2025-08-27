module standardizer_module
    ! Module-specific transformations module
    ! Handles module node standardization and content processing
    
    use ast_core
    implicit none
    private

    public :: standardize_module
    

contains

    ! Local implementation of standardize_ast for module context
    ! This handles AST nodes within a module - simplified version that
    ! doesn't need to handle top-level programs or module wrapping
    recursive subroutine standardize_ast(arena, root_index, in_module)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index
        logical, intent(in), optional :: in_module
        
        if (root_index <= 0 .or. root_index > arena%size) return
        if (.not. allocated(arena%entries(root_index)%node)) return
        
        ! For module contents, we only need to handle function/subroutine definitions
        ! All other declarations are left as-is
        select type (node => arena%entries(root_index)%node)
        type is (function_def_node)
            ! Inside a module - don't wrap in program, just leave as-is
            ! Functions inside modules are already properly structured
        type is (subroutine_def_node)
            ! Inside a module - don't wrap in program, just leave as-is  
            ! Subroutines inside modules are already properly structured
        class default
            ! For declarations and other node types inside modules, no action needed
        end select
    end subroutine standardize_ast

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