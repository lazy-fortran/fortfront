program debug_ast
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_ast, only: ast_arena_t, ast_to_json
    use fortfront_tooling, only: tooling_parse_options_t, &
                                 tooling_load_ast_from_file, message_has_error
    use process_exit, only: exit_quiet
    implicit none

    type(ast_arena_t) :: arena
    type(tooling_parse_options_t) :: options
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: json_output
    integer :: root_index, path_length
    character(len=:), allocatable :: path

    if (command_argument_count() /= 1) then
        write (error_unit, '(A)') 'Usage: debug_ast FILE'
        call exit_quiet(1)
    end if
    call get_command_argument(1, length=path_length)
    allocate (character(len=path_length) :: path)
    call get_command_argument(1, path)

    options = tooling_parse_options_t()
    options%run_semantics = .false.

    call tooling_load_ast_from_file(path, arena, root_index, error_msg, options)

    if (message_has_error(error_msg)) then
        write (error_unit, '(A)') 'Error: '//trim(error_msg)
        call exit_quiet(1)
    end if

    call ast_to_json(arena, root_index, json_output)
    write (*, '(A)') trim(json_output)
end program debug_ast
