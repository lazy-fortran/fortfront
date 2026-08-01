module test_module_discovery
    implicit none
    private
    public :: get_module_directory
    public :: find_module_dir_from_compile_commands
    public :: resolve_compile_commands_path
    public :: fallback_module_dir_search

contains

    include '../common/filesystem_helpers.inc'

    function get_module_directory(executable_path) result(module_dir)
        character(len=*), intent(in) :: executable_path
        character(len=:), allocatable :: module_dir
        character(len=:), allocatable :: candidate
        character(len=:), allocatable :: current_dir
        character(len=:), allocatable :: parent_dir
        character(len=1) :: sep
        character(len=1024) :: env_dir
        integer :: env_status

        module_dir = ''

        env_dir = ''
        call get_environment_variable('FORTFRONT_MODULE_DIR', env_dir, &
            status=env_status)
        if (env_status == 0) then
            candidate = trim(env_dir)
            if (len_trim(candidate) > 0) then
                if (index(candidate, '.mod', back=.true.) == &
                    len_trim(candidate) - 3) then
                    candidate = directory_from_path(candidate)
                end if
                if (len_trim(candidate) > 0) then
                    sep = path_separator_for(candidate)
                    if (module_directory_has_module(candidate, sep)) then
                        module_dir = trim(candidate)
                        return
                    end if
                end if
            end if
        end if

        candidate = find_module_dir_from_compile_commands(executable_path)
        if (len_trim(candidate) > 0) then
            module_dir = trim(candidate)
            return
        end if

        current_dir = directory_from_path(executable_path)
        do while (len_trim(current_dir) > 0)
            if (set_module_dir_if_exists(current_dir, module_dir)) return
            parent_dir = directory_from_path(current_dir)
            if (len_trim(parent_dir) == 0) exit
            if (trim(parent_dir) == trim(current_dir)) exit
            current_dir = parent_dir
        end do

        candidate = extract_module_candidate(executable_path, '/app/')
        if (len_trim(candidate) > 0) then
            if (set_module_dir_if_exists(candidate, module_dir)) return
        end if

        candidate = extract_module_candidate(executable_path, '\\app\\')
        if (len_trim(candidate) > 0) then
            if (set_module_dir_if_exists(candidate, module_dir)) return
        end if

        call fallback_module_dir_search(module_dir)

        if (len_trim(module_dir) == 0) then
            sep = path_separator_for('fortfront_modules')
            if (module_directory_has_module('fortfront_modules', sep)) then
                module_dir = 'fortfront_modules'
            end if
        end if
    end function get_module_directory

    pure function extract_module_candidate(path, marker) result(value)
        character(len=*), intent(in) :: path, marker
        character(len=:), allocatable :: value
        integer :: pos

        value = ''
        pos = index(path, marker, back=.true.)
        if (pos > 0) then
            value = trim(path(1:pos - 1))
        end if
    end function extract_module_candidate

    logical function module_directory_has_module(base, sep)
        character(len=*), intent(in) :: base
        character(len=1), intent(in) :: sep
        character(len=:), allocatable :: module_path
        logical :: exists

        module_directory_has_module = .false.
        if (len_trim(base) == 0) return

        module_path = trim(base) // sep // 'fortfront.mod'
        inquire (file=trim(module_path), exist=exists)
        if (exists) then
            module_directory_has_module = .true.
        end if
    end function module_directory_has_module

    function find_module_dir_from_compile_commands(executable_path) result(module_dir)
        character(len=*), intent(in) :: executable_path
        character(len=:), allocatable :: module_dir
        integer :: unit_num, ios
        character(len=512) :: line
        logical :: awaiting_path
        character(len=:), allocatable :: candidate
        character(len=1) :: sep
        character(len=:), allocatable :: commands_path
        logical :: exists

        module_dir = ''
        awaiting_path = .false.

        commands_path = 'build/compile_commands.json'
        inquire (file=trim(commands_path), exist=exists)
        if (.not. exists) then
            commands_path = resolve_compile_commands_path(executable_path)
            if (len_trim(commands_path) == 0) return
        end if

        open (newunit=unit_num, file=trim(commands_path), status='old', &
            action='read', iostat=ios)
        if (ios /= 0) return

        do
            read (unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit

            if (awaiting_path) then
                candidate = extract_argument_path(line)
                if (len_trim(candidate) > 0) then
                    sep = path_separator_for(candidate)
                    if (module_directory_has_module(candidate, sep)) then
                        module_dir = trim(candidate)
                        exit
                    end if
                end if
                awaiting_path = .false.
            else if (index(line, '"-J"') > 0) then
                awaiting_path = .true.
            end if
        end do

        close (unit_num)
    end function find_module_dir_from_compile_commands

    function resolve_compile_commands_path(executable_path) result(path)
        character(len=*), intent(in) :: executable_path
        character(len=:), allocatable :: path
        character(len=:), allocatable :: current_dir
        character(len=:), allocatable :: candidate
        character(len=1) :: sep
        logical :: exists

        path = ''
        current_dir = directory_from_path(executable_path)

        do while (len_trim(current_dir) > 0)
            sep = path_separator_for(current_dir)
            candidate = join_path(current_dir, 'compile_commands.json', sep)
            inquire (file=trim(candidate), exist=exists)
            if (exists) then
                path = trim(candidate)
                return
            end if
            current_dir = directory_from_path(current_dir)
        end do

        inquire (file='compile_commands.json', exist=exists)
        if (exists) path = 'compile_commands.json'
    end function resolve_compile_commands_path

    subroutine fallback_module_dir_search(module_dir)
        character(len=:), allocatable, intent(inout) :: module_dir
        character(len=256) :: search_file
        integer :: exit_code, unit_num, ios
        character(len=512) :: line
        integer :: sep_pos
        logical :: is_win

        if (len_trim(module_dir) > 0) return

        is_win = check_if_windows()
        search_file = 'fortfront_module_search.txt'

        if (is_win) then
            call execute_command_line('cmd /C "dir /s /b fortfront.mod > '// &
                trim(search_file)//' 2>nul"', &
                exitstat=exit_code)
        else
            call execute_command_line( &
                'find build -name "fortfront.mod" -print -quit > '// &
                trim(search_file)//' 2>/dev/null', exitstat=exit_code)
        end if

        if (exit_code /= 0) then
            call cleanup_file(search_file)
            return
        end if

        open (newunit=unit_num, file=trim(search_file), status='old', action='read', &
            iostat=ios)
        if (ios /= 0) then
            call cleanup_file(search_file)
            return
        end if

        read (unit_num, '(A)', iostat=ios) line
        close (unit_num)
        call cleanup_file(search_file)

        if (ios /= 0) return
        if (len_trim(line) == 0) return

        sep_pos = find_last_separator(trim(line))
        if (sep_pos > 0) then
            module_dir = trim(line(1:sep_pos - 1))
        else
            module_dir = directory_from_path(trim(line))
        end if
    end subroutine fallback_module_dir_search

    pure function extract_argument_path(line) result(path)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: path
        integer :: first_quote, second_quote

        path = ''

        first_quote = index(line, '"')
        if (first_quote == 0) return

        second_quote = index(line(first_quote + 1:), '"')
        if (second_quote == 0) return

        second_quote = second_quote + first_quote
        if (second_quote - first_quote <= 1) return

        path = trim(line(first_quote + 1:second_quote - 1))
    end function extract_argument_path

    logical function set_module_dir_if_exists(base, module_dir)
        character(len=*), intent(in) :: base
        character(len=:), allocatable, intent(inout) :: module_dir
        character(len=1) :: sep
        character(len=:), allocatable :: candidate

        set_module_dir_if_exists = .false.
        if (len_trim(base) == 0) return

        sep = path_separator_for(base)
        if (module_directory_has_module(base, sep)) then
            module_dir = trim(base)
            set_module_dir_if_exists = .true.
            return
        end if

        candidate = join_path(base, 'build', sep)
        if (len_trim(candidate) == 0) return
        if (module_directory_has_module(candidate, sep)) then
            module_dir = trim(candidate)
            set_module_dir_if_exists = .true.
        end if
    end function set_module_dir_if_exists

end module test_module_discovery
