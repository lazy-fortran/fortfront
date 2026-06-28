program test_find_double_decl
    use fortfront, only: tooling_parse_options_t, tooling_load_ast_from_file, &
        ast_arena_t
    use ast_nodes_data, only: declaration_node
    use standardizer_core, only: standardize_ast
    implicit none

    type(ast_arena_t) :: arena
    type(tooling_parse_options_t) :: options
    character(len=:), allocatable :: error_msg
    integer :: root_index
    integer :: i

    options = tooling_parse_options_t()
    options%run_semantics = .false.
    call tooling_load_ast_from_file('examples/f90/issue_2250_pure_interface.f90', &
        arena, root_index, error_msg, options)
    call report('before', arena)
    call standardize_ast(arena, root_index)
    call report('after', arena)
contains
    subroutine report(label, arena)
        character(len=*), intent(in) :: label
        type(ast_arena_t), intent(in) :: arena
        integer :: i

        print *, '--- ', trim(label)
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
                type is (declaration_node)
                if (allocated(node%var_name)) then
                    if (trim(node%var_name) == 'double') then
                        print *, 'found decl at ', i, ' type=', trim(node%type_name)
                    end if
                end if
            end select
        end do
    end subroutine report
end program test_find_double_decl
