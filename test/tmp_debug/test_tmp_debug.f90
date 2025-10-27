program test_tmp_debug
    use fortfront, only: tooling_parse_options_t, tooling_load_ast_from_string, &
                         ast_arena_t, token_t
    use ast_nodes_control, only: if_node
    implicit none

    type(ast_arena_t) :: arena
    type(tooling_parse_options_t) :: options
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    integer :: root_index
    character(len=:), allocatable :: lazy_code
    integer :: i

    lazy_code = 'a = [-5, -3, -1, 1, 3, 5]' // new_line('A') // &
                'sum_neg = 0' // new_line('A') // &
                'sum_pos = 0' // new_line('A') // new_line('A') // &
                'do i = 1, 6' // new_line('A') // &
                '    if (a(i) < 0) then' // new_line('A') // &
                '        sum_neg = sum_neg + a(i)' // new_line('A') // &
                '    else' // new_line('A') // &
                '        sum_pos = sum_pos + a(i)' // new_line('A') // &
                '    end if' // new_line('A') // &
                'end do' // new_line('A') // new_line('A') // &
                'print *, ''Sum negative:'', sum_neg' // new_line('A') // &
                'print *, ''Sum positive:'', sum_pos'

    options = tooling_parse_options_t()
    options%run_semantics = .false.

    call tooling_load_ast_from_string(lazy_code, arena, root_index, error_msg, &
                                      options, tokens)

    write (*,'(A,I0)') 'Arena size: ', arena%size

    do i = 1, arena%size
        if (.not. allocated(arena%entries(i)%node)) cycle
        select type (node => arena%entries(i)%node)
        type is (if_node)
            write (*,'(A,I0)') 'Found if_node at index ', i
            if (allocated(node%then_body_indices)) then
                write (*,'(A,I0)') '  then_body_size:', size(node%then_body_indices)
                if (size(node%then_body_indices) > 0) then
                    write (*,'(A,*(I0,1X))') '  then_body:', node%then_body_indices
                end if
            else
                write (*,'(A)') '  then_body: <not allocated>'
            end if
        end select
    end do

    stop 0
end program test_tmp_debug
