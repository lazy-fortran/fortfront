program test_legacy_mixed_declaration
    use fortfront, only: ast_arena_t, token_t, tooling_load_ast_from_string, &
        tooling_parse_options_t
    use ast_nodes_data, only: declaration_node
    implicit none

    type(ast_arena_t) :: arena
    type(tooling_parse_options_t) :: options
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    integer :: root_index, i
    logical :: found_a, found_i

    options = tooling_parse_options_t()
    options%run_semantics = .false.
    call tooling_load_ast_from_string( &
        'integer a(10), i'//new_line('a')//'print *, i'//new_line('a')// &
        'end', arena, root_index, error_msg, options, tokens)

    found_a = .false.
    found_i = .false.
    do i = 1, arena%size
        if (.not. arena%has_node_at(i)) cycle
        select type (node => arena%entries(i)%node)
            type is (declaration_node)
            if (.not. allocated(node%var_name)) cycle
            select case (trim(node%var_name))
            case ('a')
                found_a = allocated(node%dimension_indices)
                if (found_a) found_a = size(node%dimension_indices) == 1
            case ('i')
                found_i = .not. allocated(node%dimension_indices)
                if (allocated(node%dimension_indices)) then
                    found_i = size(node%dimension_indices) == 0
                end if
            end select
        end select
    end do

    if (.not. found_a) error stop 'a must be a rank-one declaration'
    if (.not. found_i) error stop 'i must be a scalar declaration'
end program test_legacy_mixed_declaration
