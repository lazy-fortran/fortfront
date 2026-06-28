module semantic_declaration_utils
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node
    use semantic_inference_helpers, only: process_declaration_variables
    use type_system_unified, only: mono_type_t
    implicit none
    private

    public :: fetch_declaration_type

contains

    logical function fetch_declaration_type(arena, name, decl_type) result(found)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        type(mono_type_t), intent(out) :: decl_type
        integer :: i, j

        found = .false.

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
                type is (declaration_node)
                if (allocated(node%var_name)) then
                    if (trim(node%var_name) == trim(name)) then
                        call process_declaration_variables(node, decl_type)
                        found = .true.
                        return
                    end if
                end if
                if (node%is_multi_declaration .and. allocated(node%var_names)) then
                    do j = 1, size(node%var_names)
                        if (trim(node%var_names(j)) == trim(name)) then
                            call process_declaration_variables(node, decl_type)
                            found = .true.
                            return
                        end if
                    end do
                end if
            end select
        end do
    end function fetch_declaration_type

end module semantic_declaration_utils
