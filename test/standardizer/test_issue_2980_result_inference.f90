program test_issue_2980_result_inference
    use, intrinsic :: iso_fortran_env, only: error_unit
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use standardizer, only: standardize_ast
    use codegen_core, only: codegen_core_generate_arena, initialize_codegen
    use ast_arena_modern, only: ast_arena_t
    use lexer_core, only: token_t
    implicit none
    character(len=:), allocatable :: source, code, error_msg
    character(len=:), allocatable :: temp_dir, source_path, exe_path
    character(len=:), allocatable :: compile_command, run_command
    character(len=1) :: separator
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: root_index, unit, exit_code
    logical :: is_windows

    source = 'program issue_2980'//new_line('a')// &
        '  if (abs(twice(2.5) - 5.0) > 1.0e-6) error stop 1'//new_line('a')// &
        'contains'//new_line('a')// &
        '  function twice(x)'//new_line('a')// &
        '    twice = 2 * x'//new_line('a')// &
        '  end function twice'//new_line('a')// &
        'end program issue_2980'//new_line('a')

    call initialize_codegen()
    call lex_source(source, tokens, error_msg)
    call assert_empty(error_msg, 'lexer rejected issue #2980 source')
    call parse_tokens(tokens, arena, root_index, error_msg)
    call assert_empty(error_msg, 'parser rejected issue #2980 source')
    call standardize_ast(arena, root_index)
    code = codegen_core_generate_arena(arena, root_index)

    if (index(code, 'real function twice') <= 0 .or. &
        index(code, 'integer function twice') > 0) then
        write (error_unit, '(A)') &
            'FAIL: result type was not inferred from real RHS expression'
        write (error_unit, '(A)') trim(code)
        error stop 1
    end if

    is_windows = check_if_windows()
    call create_temp_directory(temp_dir, is_windows)
    if (len_trim(temp_dir) == 0) error stop 'FAIL: temp directory unavailable'
    separator = path_separator_for(temp_dir)
    source_path = join_path(temp_dir, 'issue_2980.f90', separator)
    exe_path = join_path(temp_dir, 'issue_2980.exe', separator)
    open (newunit=unit, file=source_path, status='replace', action='write')
    write (unit, '(A)') trim(code)
    close (unit)

    compile_command = 'gfortran '//quote_for_shell(source_path, is_windows, &
        escape_for_cmd=is_windows)//' -o '//quote_for_shell(exe_path, &
        is_windows, escape_for_cmd=is_windows)
    call execute_command_line(compile_command, exitstat=exit_code, wait=.true.)
    if (exit_code /= 0) then
        call cleanup_temp_directory(temp_dir, is_windows)
        error stop 'FAIL: gfortran rejected standardized issue #2980 source'
    end if

    run_command = quote_for_shell(exe_path, is_windows, &
        escape_for_cmd=is_windows)
    call execute_command_line(run_command, exitstat=exit_code, wait=.true.)
    call cleanup_temp_directory(temp_dir, is_windows)
    if (exit_code /= 0) then
        write (error_unit, '(A)') 'FAIL: independent gfortran oracle mismatch'
        error stop 1
    end if

    print '(A)', 'PASS: issue #2980 result follows real RHS and gfortran output'

contains

    include '../common/filesystem_helpers.inc'
    include '../common/shell_commands.inc'

    subroutine assert_empty(message, context)
        character(len=:), allocatable, intent(in) :: message
        character(len=*), intent(in) :: context
        if (len_trim(message) > 0) then
            write (error_unit, '(A)') trim(context)//': '//trim(message)
            error stop 1
        end if
    end subroutine assert_empty

end program test_issue_2980_result_inference
