program test_library_example_modules
    use test_module_discovery, only: get_module_directory
    use test_module_discovery, only: resolve_compile_commands_path
    implicit none

    character(len=48), parameter :: examples(*) = [character(len=48) :: &
        'tmp_goto_ast.f90', 'external_tool_example.f90', &
        'library_usage_ast_node_counter.f90', &
        'library_usage_ast_node_position.f90', 'tooling_lightweight_ast.f90']
    character(len=2048) :: executable_path
    character(len=:), allocatable :: module_dir, temp_dir, command, commands_path
    integer :: i, exit_code
    logical :: is_windows, commands_exist, root_commands_exist

    is_windows = check_if_windows()
    if (.not. is_windows) then
        commands_path = resolve_compile_commands_path('/missing_fortfront/app/tool')
        inquire (file='/compile_commands.json', exist=root_commands_exist)
        inquire (file='compile_commands.json', exist=commands_exist)
        if (root_commands_exist) then
            if (commands_path /= '/compile_commands.json') &
                error stop 'Cannot resolve root compile commands'
        else if (commands_exist) then
            if (commands_path /= 'compile_commands.json') &
                error stop 'Cannot resolve working-directory compile commands'
        else
            if (len(commands_path) /= 0) error stop 'Unexpected compile commands'
        end if
    end if
    call get_command_argument(0, executable_path)
    module_dir = get_module_directory(trim(executable_path))
    call create_temp_directory(temp_dir, is_windows)
    if (len_trim(temp_dir) == 0) error stop 'Cannot create module cache'

    do i = 1, size(examples)
        command = build_compile_command('examples/f90/'//trim(examples(i)), &
            module_dir, temp_dir, is_windows)
        call execute_command_line(command, exitstat=exit_code)
        if (exit_code /= 0) then
            print *, 'FAIL: compile library example ', trim(examples(i))
            print *, 'Module directory: ', module_dir
            call cleanup_temp_directory(temp_dir, is_windows)
            stop 1
        end if
    end do

    call cleanup_temp_directory(temp_dir, is_windows)
    print *, 'PASS: library examples compile using discovered public modules'

contains

    include '../common/filesystem_helpers.inc'
    include '../common/shell_commands.inc'
end program test_library_example_modules
