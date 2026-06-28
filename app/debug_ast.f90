program debug_ast
    use fortfront, only: tooling_parse_options_t, tooling_load_ast_from_string, &
        ast_arena_t, token_t, ast_to_json
    implicit none

    type(ast_arena_t) :: arena
    type(tooling_parse_options_t) :: options
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: json_output
    integer :: root_index
    character(len=:), allocatable :: lazy_code

    lazy_code = ''
    lazy_code = trim(lazy_code)//'a = [-5, -3, -1, 1, 3, 5]'//new_line('A')
    lazy_code = lazy_code//'sum_neg = 0'//new_line('A')
    lazy_code = lazy_code//'sum_pos = 0'//new_line('A')//new_line('A')
    lazy_code = lazy_code//'do i = 1, 6'//new_line('A')
    lazy_code = lazy_code//'    if (a(i) < 0) then'//new_line('A')
    lazy_code = lazy_code//'        sum_neg = sum_neg + a(i)'//new_line('A')
    lazy_code = lazy_code//'    else'//new_line('A')
    lazy_code = lazy_code//'        sum_pos = sum_pos + a(i)'//new_line('A')
    lazy_code = lazy_code//'    end if'//new_line('A')
    lazy_code = lazy_code//'end do'//new_line('A')//new_line('A')
    lazy_code = lazy_code//'print *, ''Sum negative:'', sum_neg'//new_line('A')
    lazy_code = lazy_code//'print *, ''Sum positive:'', sum_pos'

    options = tooling_parse_options_t()
    options%run_semantics = .false.

    call tooling_load_ast_from_string(lazy_code, arena, root_index, error_msg, &
        options, tokens)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (*, '(A)') 'Error: '//trim(error_msg)
        end if
    end if

    call ast_to_json(arena, root_index, json_output)
    write (*, '(A)') trim(json_output)
end program debug_ast
