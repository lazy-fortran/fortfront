module test_shell_commands
    implicit none
    private
    public :: build_compile_command
    public :: quote_for_shell
    public :: verify_shell_helpers

contains

    function build_compile_command(output_file, module_dir, temp_dir, &
                                   is_windows) result(command)
        character(len=*), intent(in) :: output_file
        character(len=*), intent(in) :: module_dir, temp_dir
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: command
        character(len=:), allocatable :: module_arg
        character(len=:), allocatable :: output_arg
        character(len=:), allocatable :: temp_arg
        character(len=32) :: env_value
        integer :: env_status
        logical :: disable_redirect

        command = ''

        output_arg = quote_for_shell(output_file, is_windows, &
                                     escape_for_cmd=is_windows)
        if (len_trim(output_arg) == 0) return

        command = 'gfortran -c -fsyntax-only '

        if (len_trim(module_dir) > 0) then
            module_arg = quote_for_shell(module_dir, is_windows, &
                                         escape_for_cmd=is_windows)
            if (len_trim(module_arg) > 0) command = command // '-I ' // &
                                                    module_arg // ' '
        end if

        if (len_trim(temp_dir) > 0) then
            temp_arg = quote_for_shell(temp_dir, is_windows, &
                                       escape_for_cmd=is_windows)
            if (len_trim(temp_arg) > 0) command = command // '-J ' // temp_arg // ' '
        end if

        command = command // output_arg

        env_value = ''
        env_status = 1
        disable_redirect = .false.
        call get_environment_variable('FORTFRONT_SHOW_COMPILE_OUTPUT', &
                                      env_value, status=env_status)
        if (env_status == 0) then
            if (len_trim(env_value) > 0) then
                if (env_value(1:1) /= '0') disable_redirect = .true.
            end if
        end if

        if (.not. disable_redirect) then
            if (is_windows) then
                command = command // ' > nul 2>&1'
            else
                command = command // ' > /dev/null 2>&1'
            end if
        end if
    end function build_compile_command

    pure function quote_for_shell(path, is_windows, escape_for_cmd) result(argument)
        character(len=*), intent(in) :: path
        logical, intent(in) :: is_windows
        logical, intent(in), optional :: escape_for_cmd
        character(len=:), allocatable :: argument
        logical :: needs_cmd_escape

        needs_cmd_escape = .false.
        if (present(escape_for_cmd)) needs_cmd_escape = escape_for_cmd

        if (len_trim(path) == 0) then
            argument = ''
        else if (is_windows .and. needs_cmd_escape) then
            argument = '"' // trim(path) // '"'
        else
            argument = '"' // trim(path) // '"'
        end if
    end function quote_for_shell

    subroutine verify_shell_helpers(is_windows)
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: quoted
        character(len=:), allocatable :: command
        integer :: trimmed_len

        quoted = quote_for_shell('path with spaces/example.lf', is_windows)
        if (len_trim(quoted) == 0) then
            print *, 'ERROR: quote_for_shell rejected safe path'
            stop 1
        end if
        trimmed_len = len_trim(quoted)
        if (quoted(1:1) /= '"' .or. quoted(trimmed_len:trimmed_len) /= '"') then
            print *, 'ERROR: quote_for_shell missing quotes'
            stop 1
        end if

        command = build_compile_command('output file.f90', 'modules dir', 'temp dir', &
                                        is_windows)
        if (len_trim(command) == 0) then
            print *, 'ERROR: build_compile_command returned empty command'
            stop 1
        end if
        if (is_windows) then
            if (index(command, '"modules dir"') == 0) then
                print *, 'ERROR: module directory not quoted for cmd'
                stop 1
            end if
            if (index(command, '"output file.f90"') == 0) then
                print *, 'ERROR: output path not quoted for cmd'
                stop 1
            end if
        else
            if (index(command, '"modules dir"') == 0) then
                print *, 'ERROR: module directory not quoted'
                stop 1
            end if
            if (index(command, '"output file.f90"') == 0) then
                print *, 'ERROR: output path not quoted'
                stop 1
            end if
        end if
        if (is_windows) then
            if (index(quote_for_shell('pipe path', is_windows, &
                                      escape_for_cmd=.true.), &
                      '"pipe path"') == 0) then
                print *, 'ERROR: Windows cmd escaping missing'
                stop 1
            end if
        end if
    end subroutine verify_shell_helpers

end module test_shell_commands
