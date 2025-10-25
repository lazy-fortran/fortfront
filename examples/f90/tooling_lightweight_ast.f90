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
                                   'program tooling_demo' // new_line('a') // &
                                   '  implicit none' // new_line('a') // &
                                   '  integer :: value' // new_line('a') // &
                                   '  value = 42' // new_line('a') // &
                                   '  print *, value' // new_line('a') // &
                                   'end program tooling_demo'

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
