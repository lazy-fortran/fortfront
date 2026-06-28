program tooling_lightweight_ast
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortfront, only: tooling_parse_options_t, tooling_load_ast_from_string, &
        ast_arena_t, token_t, get_node_type_at, ast_to_json
    implicit none

    type(ast_arena_t) :: arena
    type(tooling_parse_options_t) :: options
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: json_output
    integer :: root_index
    integer :: start_clock
    integer :: end_clock
    integer :: clock_rate
    real(dp) :: elapsed_seconds
    character(len=*), parameter :: lazy_code = &
        'a = [-5, -3, -1, 1, 3, 5]' // new_line('a') // &
        'sum_neg = 0' // new_line('a') // &
        'sum_pos = 0' // new_line('a') // &
        '' // new_line('a') // &
        'do i = 1, 6' // new_line('a') // &
        '    if (a(i) < 0) then' // new_line('a') // &
        '        sum_neg = sum_neg + a(i)' // new_line('a') // &
        '    else' // new_line('a') // &
        '        sum_pos = sum_pos + a(i)' // new_line('a') // &
        '    end if' // new_line('a') // &
        'end do' // new_line('a') // &
        '' // new_line('a') // &
        'print *, ''Sum negative:'', sum_neg' // new_line('a') // &
        'print *, ''Sum positive:'', sum_pos'

    options = tooling_parse_options_t()
    options%run_semantics = .false.

    call system_clock(start_clock, clock_rate)
    call tooling_load_ast_from_string(lazy_code, arena, root_index, error_msg, &
        options, tokens)
    call system_clock(end_clock)

    if (len_trim(error_msg) > 0) then
        print *, 'Failed to load AST:'
        print *, trim(error_msg)
        stop 1
    end if

    if (clock_rate > 0) then
        elapsed_seconds = real(end_clock - start_clock, dp) / &
            real(clock_rate, dp)
    else
        elapsed_seconds = 0.0_dp
    end if

    print *, 'Lightweight AST load successful:'
    print *, '  Root node type  :', get_node_type_at(arena, root_index)
    print *, '  Arena node count:', arena%size
    print *, '  Token count     :', merge(size(tokens), 0, allocated(tokens))
    print *, '  Parse time (s)  :', elapsed_seconds

    call ast_to_json(arena, root_index, json_output)
    print *, '  JSON snapshot   :', trim(json_output)
end program tooling_lightweight_ast
