program tmp_goto_ast
    use fortfront, only: tooling_parse_options_t, tooling_load_ast_from_string, &
        ast_arena_t, ast_to_json
    implicit none

    type(ast_arena_t) :: arena
    type(tooling_parse_options_t) :: options
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: json_output
    character(len=:), allocatable :: source
    integer :: root_index

    options = tooling_parse_options_t()
    options%run_semantics = .false.

    source = 'program demo' // new_line('a') // &
        '    implicit none' // new_line('a') // &
        '    integer :: i' // new_line('a') // &
        '    i = 0' // new_line('a') // &
        '10  i = i + 1' // new_line('a') // &
        '    if (i < 3) goto 10' // new_line('a') // &
        '    stop' // new_line('a') // &
        'end program demo'

    call tooling_load_ast_from_string(source, arena, root_index, error_msg, &
        options)

    print *, 'error:', merge(trim(error_msg), '<none>', &
        allocated(error_msg) .and. len_trim(error_msg) > 0)

    call ast_to_json(arena, root_index, json_output)
    print *, 'json:', trim(json_output)
end program tmp_goto_ast
