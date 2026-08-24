program test_save_common
    use fortfront, only: tooling_parse_options_t, tooling_load_ast_from_string, &
        ast_arena_t
    implicit none

    type(tooling_parse_options_t) :: options
    type(ast_arena_t) :: arena
    character(len=:), allocatable :: error_msg
    integer :: root_index
    character(len=*), parameter :: source = &
        'program p'//new_line('A')// &
        'integer :: value'//new_line('A')// &
        'common /argmnt2/ value'//new_line('A')// &
        'save /argmnt2/'//new_line('A')// &
        'end program p'

    options = tooling_parse_options_t()
    options%run_semantics = .false.
    call tooling_load_ast_from_string(source, arena, root_index, error_msg, options)

    if (len_trim(error_msg) /= 0) then
        write (*, '(A)') 'FAIL: SAVE common block parsing: '//trim(error_msg)
        error stop 1
    end if
    write (*, '(A)') 'PASS: SAVE common block parsing'
end program test_save_common
