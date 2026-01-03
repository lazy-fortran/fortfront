program test_interface_decl_inspect
    use fortfront, only: tooling_parse_options_t, tooling_load_ast_from_file, &
                         ast_arena_t
    use ast_nodes_data, only: declaration_node
    use ast_nodes_procedure, only: function_def_node
    use standardizer_core, only: standardize_ast
    implicit none

    type(ast_arena_t) :: arena
    type(tooling_parse_options_t) :: options
    character(len=:), allocatable :: error_msg
    integer :: root_index

    options = tooling_parse_options_t()
    options%run_semantics = .false.

    call tooling_load_ast_from_file('examples/f90/issue_2250_pure_interface.f90', &
                                    arena, root_index, error_msg, options)
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) stop 1
    end if

    print *, '=== Before standardize ==='
    call dump_decls(arena)

    call standardize_ast(arena, root_index)

    print *, '=== After standardize ==='
    call dump_decls(arena)
contains
    subroutine dump_decls(arena)
        type(ast_arena_t), intent(in) :: arena
        integer :: i, j

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (function_def_node)
                print *, 'function idx', i
                if (allocated(node%return_type)) then
                    print *, '  return_type=', trim(node%return_type)
                else
                    print *, '  return_type=<none>'
                end if
                if (.not. allocated(node%body_indices)) cycle
                do j = 1, size(node%body_indices)
                    call print_decl(arena, node%body_indices(j))
                end do
            end select
        end do
    end subroutine dump_decls

    subroutine print_decl(arena, idx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx

        if (.not. arena%has_node_at(idx)) return
        select type (decl => arena%entries(idx)%node)
        type is (declaration_node)
            if (allocated(decl%var_name)) then
                print *, '  decl', idx, trim(decl%var_name), 'type=', &
                    trim(decl%type_name)
            end if
            if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                print *, '    vars:', decl%var_names
            end if
        end select
    end subroutine print_decl
end program test_interface_decl_inspect
