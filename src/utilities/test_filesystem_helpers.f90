module test_filesystem_helpers
    use fortfront_constants, only: MAX_EXAMPLE_PATH_LEN, MAX_TEST_SEARCH_LINE_LEN
    implicit none
    private
    public :: find_fortfront_executable
    public :: check_if_windows
    public :: cleanup_file
    public :: create_temp_directory
    public :: get_temp_base_directory
    public :: get_environment_value
    public :: make_temp_file_path
    public :: ensure_directory_exists
    public :: cleanup_temp_directory
    public :: extract_example_basename
    public :: extract_relative_example_path
    public :: normalize_path_string
    public :: join_path
    public :: directory_from_path
    public :: find_last_separator
    public :: path_separator_for

contains

    function get_environment_value(name) result(value)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: value
        integer :: value_len, status

        call get_environment_variable(trim(name), length=value_len, status=status)
        if (status /= 0 .or. value_len <= 0) then
            value = ''
            return
        end if

        allocate (character(len=value_len) :: value)
        call get_environment_variable(trim(name), value=value, status=status)
        if (status /= 0) then
            value = ''
            return
        end if

        value = trim(value)
    end function get_environment_value

    function find_fortfront_executable(is_windows) result(executable_path)
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: executable_path

        executable_path = ''

        if (is_windows) then
            executable_path = find_fortfront_on_windows()
        else
            executable_path = find_fortfront_on_posix()
        end if

        if (len_trim(executable_path) == 0) then
            executable_path = search_known_build_paths(is_windows)
        end if
    end function find_fortfront_executable

    function find_fortfront_on_windows() result(executable_path)
        character(len=:), allocatable :: executable_path
        character(len=16), dimension(5) :: roots
        integer :: r, exit_code

        executable_path = ''
        roots = [character(len=16) :: '.', '..', '..\\..', '..\\..\\..', &
                 '..\\..\\..\\..']

        do r = 1, size(roots)
            call execute_command_line( &
                'cmd /C where /R '//trim(roots(r))// &
                ' fortfront.exe > fortfront_search_win.txt', &
                exitstat=exit_code)
            if (exit_code /= 0) cycle
            call select_search_result('fortfront_search_win.txt', &
                                      'app\\fortfront.exe', executable_path)
            call execute_command_line( &
                'cmd /C del /F /Q fortfront_search_win.txt', exitstat=exit_code)
            if (len_trim(executable_path) > 0) exit
        end do

        if (len_trim(executable_path) == 0) then
            executable_path = existing_path('app\\fortfront.exe')
        end if
    end function find_fortfront_on_windows

    function find_fortfront_on_posix() result(executable_path)
        character(len=:), allocatable :: executable_path
        integer :: exit_code

        executable_path = ''

        call execute_command_line( &
            'find build -name "fortfront" -type f | head -1 > fortfront_search.txt', &
            exitstat=exit_code)
        if (exit_code == 0) then
            call select_search_result('fortfront_search.txt', '/app/fortfront', &
                                      executable_path)
        end if
        call execute_command_line('rm -f fortfront_search.txt', exitstat=exit_code)
    end function find_fortfront_on_posix

    function search_known_build_paths(is_windows) result(executable_path)
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: executable_path
        character(len=64), dimension(24) :: build_patterns
        logical :: file_exists
        integer :: i
        character(len=64) :: candidate_path

        executable_path = ''

        build_patterns = [ &
                         'build/gfortran_266FF454AB2555FE/app/fortfront   ', &
                         'build/gfortran_9ABCD662468F5A74/app/fortfront   ', &
                         'build/gfortran_C79DEB301B8081FC/app/fortfront   ', &
                         'build/gfortran_C523F0F8A99FF060/app/fortfront   ', &
                         'build/gfortran_1F2DC83CBD1DC595/app/fortfront   ', &
                         'build/gfortran_35CFD5CFC35942D6/app/fortfront   ', &
                         'build/gfortran_4AE9E4ED7A89B913/app/fortfront   ', &
                         'build/gfortran_66DBF6172AF51040/app/fortfront   ', &
                         'build/gfortran_A56298966DD7666C/app/fortfront   ', &
                         'build/gfortran_E3D58E6D75301430/app/fortfront   ', &
                         'build/gfortran_9CBC8EEC13D00A4A/app/fortfront   ', &
                         './build/gfortran_266FF454AB2555FE/app/fortfront ', &
                         './build/gfortran_9ABCD662468F5A74/app/fortfront ', &
                         './build/gfortran_C79DEB301B8081FC/app/fortfront ', &
                         './build/gfortran_C523F0F8A99FF060/app/fortfront ', &
                         'fortfront                                       ', &
                         './fortfront                                     ', &
                         'app/fortfront                                   ', &
                         './app/fortfront                                 ', &
                         '../fortfront                                    ', &
                         'fortfront.exe                                   ', &
                         '.\\fortfront.exe                                ', &
                         'app\\fortfront.exe                              ', &
                         '.\\app\\fortfront.exe                           ']

        do i = 1, size(build_patterns)
            candidate_path = trim(build_patterns(i))
            inquire (file=candidate_path, exist=file_exists)
            if (file_exists) then
                executable_path = candidate_path
                return
            end if
        end do

        if (.not. is_windows) then
            executable_path = existing_path('app/fortfront')
        end if
    end function search_known_build_paths

    function existing_path(candidate) result(found)
        character(len=*), intent(in) :: candidate
        character(len=:), allocatable :: found
        logical :: exists

        inquire (file=trim(candidate), exist=exists)
        if (exists) then
            found = trim(candidate)
        else
            found = ''
        end if
    end function existing_path

    subroutine select_search_result(filename, preferred_suffix, executable_path)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: preferred_suffix
        character(len=:), allocatable, intent(inout) :: executable_path
        character(len=:), allocatable :: fallback
        character(len=MAX_TEST_SEARCH_LINE_LEN) :: line
        integer :: unit_num, ios
        logical :: exists

        executable_path = ''
        fallback = ''

        open (newunit=unit_num, file=trim(filename), status='old', action='read', &
              iostat=ios)
        if (ios /= 0) return

        do
            read (unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) == 0) cycle
            inquire (file=trim(line), exist=exists)
            if (.not. exists) cycle
            if (index(adjustl(line), preferred_suffix) > 0) then
                executable_path = trim(line)
                exit
            end if
            if (len_trim(fallback) == 0) fallback = trim(line)
        end do

        close (unit_num)

        if (len_trim(executable_path) == 0) then
            executable_path = fallback
        end if
    end subroutine select_search_result

    function check_if_windows() result(is_win)
        logical :: is_win
        character(len=10) :: os_name
        integer :: stat

        call get_environment_variable('OS', os_name, status=stat)
        is_win = (stat == 0 .and. os_name(1:7) == 'Windows')

        if (.not. is_win) then
            call get_environment_variable('WINDIR', os_name, status=stat)
            is_win = (stat == 0)
        end if
    end function check_if_windows

    subroutine cleanup_file(file)
        character(len=*), intent(in) :: file
        logical :: exists
        integer :: unit_num, ios
        character(len=:), allocatable :: trimmed

        trimmed = trim(file)
        if (len_trim(trimmed) == 0) return

        inquire (file=trimmed, exist=exists)
        if (.not. exists) return

        open (newunit=unit_num, file=trimmed, status='old', action='readwrite', &
              iostat=ios)
        if (ios /= 0) then
            open (newunit=unit_num, file=trimmed, status='old', action='read', &
                  iostat=ios)
        end if
        if (ios == 0) then
            close (unit_num, status='delete', iostat=ios)
        end if
    end subroutine cleanup_file

    subroutine create_temp_directory(temp_dir, is_windows)
        character(len=:), allocatable, intent(out) :: temp_dir
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: base_dir
        character(len=64) :: suffix
        character(len=1) :: sep
        integer :: ios
        character(len=:), allocatable :: mkdir_cmd

        if (is_windows) then
            sep = '\'
        else
            sep = '/'
        end if
        base_dir = get_temp_base_directory(is_windows)
        if (len_trim(base_dir) == 0) base_dir = '.'

        call generate_temp_suffix(suffix)
        if (len_trim(suffix) == 0) suffix = 'default'

        temp_dir = join_path(base_dir, 'fortfront_test_'//trim(suffix), sep)

        if (is_windows) then
            mkdir_cmd = 'cmd /C "if not exist "' // trim(temp_dir) // &
                        '" mkdir "' // trim(temp_dir) // '""'
        else
            mkdir_cmd = 'mkdir -p "' // trim(temp_dir) // '"'
        end if

        call execute_command_line(trim(mkdir_cmd), exitstat=ios)
        if (ios /= 0) temp_dir = ''
    end subroutine create_temp_directory

    function get_temp_base_directory(is_windows) result(base_dir)
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: base_dir
        character(len=:), allocatable :: envtmp
        character(len=:), allocatable :: windir
        character(len=1) :: sep
        integer :: i, last_index

        if (is_windows) then
            sep = '\'
            envtmp = get_environment_value('TEMP')
            if (len_trim(envtmp) == 0) envtmp = get_environment_value('TMP')
            if (len_trim(envtmp) == 0) then
                windir = get_environment_value('WINDIR')
                if (len_trim(windir) > 0) then
                    last_index = len_trim(windir)
                    do while (last_index > 0)
                        if (windir(last_index:last_index) == '/' .or. &
                            windir(last_index:last_index) == '\') then
                            windir = windir(1:last_index - 1)
                            last_index = len_trim(windir)
                        else
                            exit
                        end if
                    end do
                    envtmp = join_path(windir, 'Temp', sep)
                else
                    envtmp = '.'
                end if
            end if
        else
            sep = '/'
            envtmp = get_environment_value('TMPDIR')
            if (len_trim(envtmp) == 0) envtmp = '/tmp'
        end if

        base_dir = trim(envtmp)
        if (is_windows) then
            do i = 1, len(base_dir)
                if (base_dir(i:i) == '/') base_dir(i:i) = '\'
            end do
        end if

        last_index = len_trim(base_dir)
        if (last_index > 0) then
            do while (last_index > 0)
                if (base_dir(last_index:last_index) == '/' .or. &
                    base_dir(last_index:last_index) == '\') then
                    base_dir = base_dir(1:last_index - 1)
                    last_index = len_trim(base_dir)
                else
                    exit
                end if
            end do
        end if
    end function get_temp_base_directory

    function make_temp_file_path(prefix, extension, is_windows) result(path)
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in) :: extension
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: path
        character(len=:), allocatable :: base_dir
        character(len=64) :: suffix
        character(len=1) :: sep

        if (is_windows) then
            sep = '\'
        else
            sep = '/'
        end if

        base_dir = get_temp_base_directory(is_windows)
        call generate_temp_suffix(suffix)
        if (len_trim(suffix) == 0) suffix = 'default'

        path = join_path(base_dir, trim(prefix)//trim(suffix)//trim(extension), sep)
    end function make_temp_file_path

    logical function ensure_directory_exists(path, is_windows)
        character(len=*), intent(in) :: path
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: mkdir_cmd
        integer :: ios
        logical :: exists

        ensure_directory_exists = .false.
        if (len_trim(path) == 0) return

        inquire (file=trim(path), exist=exists)
        if (exists) then
            ensure_directory_exists = .true.
            return
        end if

        if (is_windows) then
            mkdir_cmd = 'cmd /C "if not exist "' // trim(path) // &
                        '" mkdir "' // trim(path) // '""'
        else
            mkdir_cmd = 'mkdir -p "' // trim(path) // '"'
        end if

        call execute_command_line(trim(mkdir_cmd), exitstat=ios)
        ensure_directory_exists = (ios == 0)
    end function ensure_directory_exists

    subroutine generate_temp_suffix(suffix)
        character(len=*), intent(out) :: suffix
        integer :: date_vals(8)
        integer :: clock_count, clock_rate, clock_max

        suffix = ''
        call date_and_time(values=date_vals)
        call system_clock(count=clock_count, count_rate=clock_rate, &
                          count_max=clock_max)

        write (suffix, '(I4.4,I2.2,I2.2,"_",I2.2,I2.2,I2.2,"_",I0)') &
            date_vals(1), date_vals(2), date_vals(3), date_vals(5), &
            date_vals(6), date_vals(7), abs(clock_count)

        suffix = trim(adjustl(suffix))
        if (len_trim(suffix) == 0) then
            write (suffix, '(I0)') abs(clock_count)
            suffix = trim(adjustl(suffix))
        end if
    end subroutine generate_temp_suffix

    subroutine cleanup_temp_directory(temp_dir, is_windows)
        character(len=*), intent(in) :: temp_dir
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: rm_cmd
        integer :: ios

        if (len_trim(temp_dir) == 0) return

        if (is_windows) then
            rm_cmd = 'cmd /C "rmdir /S /Q "' // trim(temp_dir) // '""'
        else
            rm_cmd = 'rm -rf "' // trim(temp_dir) // '"'
        end if

        call execute_command_line(trim(rm_cmd), exitstat=ios)
    end subroutine cleanup_temp_directory

    pure function extract_example_basename(filepath) result(name)
        character(len=*), intent(in) :: filepath
        character(len=MAX_EXAMPLE_PATH_LEN) :: name
        character(len=:), allocatable :: trimmed
        integer :: sep_pos

        name = ''
        if (len_trim(filepath) == 0) return

        trimmed = trim(filepath)
        sep_pos = find_last_separator(trimmed)

        if (sep_pos > 0 .and. sep_pos < len(trimmed)) then
            name = trim(trimmed(sep_pos + 1:))
        else
            name = trim(trimmed)
        end if
        name = adjustl(name)
    end function extract_example_basename

    pure function extract_relative_example_path(filepath) result(relative)
        character(len=*), intent(in) :: filepath
        character(len=MAX_EXAMPLE_PATH_LEN) :: relative
        character(len=MAX_EXAMPLE_PATH_LEN) :: normalized
        character(len=:), allocatable :: trimmed
        integer :: pos

        relative = ''
        if (len_trim(filepath) == 0) return

        trimmed = adjustl(trim(filepath))
        normalized = normalize_path_string(trimmed)
        pos = index(normalized, 'examples/')

        if (pos > 0) then
            if (pos + len_trim('examples/') <= len_trim(normalized)) then
                relative = normalized(pos + len_trim('examples/'):)
            else
                relative = ''
            end if
        else
            relative = normalized
        end if

        relative = adjustl(relative)
        if (len_trim(relative) == 0) then
            relative = extract_example_basename(filepath)
        end if

        relative = trim(relative)
        relative = adjustl(relative)
    end function extract_relative_example_path

    pure function normalize_path_string(value) result(normalized)
        character(len=*), intent(in) :: value
        character(len=MAX_EXAMPLE_PATH_LEN) :: normalized
        integer :: i

        normalized = adjustl(trim(value))
        do i = 1, len(normalized)
            if (normalized(i:i) == '\') normalized(i:i) = '/'
        end do
        normalized = trim(normalized)
        normalized = adjustl(normalized)
    end function normalize_path_string

    pure function join_path(base, component, sep) result(path)
        character(len=*), intent(in) :: base, component
        character(len=1), intent(in) :: sep
        character(len=:), allocatable :: path

        if (len_trim(base) == 0) then
            path = trim(component)
        else if (len_trim(component) == 0) then
            path = trim(base)
        else
            path = trim(base) // sep // trim(component)
        end if
    end function join_path

    pure function directory_from_path(path) result(directory)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: directory
        character(len=:), allocatable :: trimmed_path
        integer :: sep_pos

        trimmed_path = trim(path)
        if (len_trim(trimmed_path) == 0) then
            directory = ''
            return
        end if

        sep_pos = find_last_separator(trimmed_path)
        if (sep_pos <= 0) then
            directory = ''
        else if (sep_pos == 1) then
            directory = trimmed_path(1:1)
        else
            directory = trim(trimmed_path(1:sep_pos - 1))
        end if
    end function directory_from_path

    pure integer function find_last_separator(path) result(position)
        character(len=*), intent(in) :: path
        integer :: i

        position = 0
        do i = len(path), 1, -1
            if (path(i:i) == '/' .or. path(i:i) == '\') then
                position = i
                return
            end if
        end do
    end function find_last_separator

    function path_separator_for(path) result(sep)
        character(len=*), intent(in) :: path
        character(len=1) :: sep
        integer :: pos
        logical :: is_win

        sep = '/'

        if (len_trim(path) == 0) then
            is_win = check_if_windows()
            if (is_win) sep = '\'
            return
        end if

        pos = find_last_separator(path)
        if (pos > 0) then
            sep = path(pos:pos)
        else
            is_win = check_if_windows()
            if (is_win) sep = '\'
        end if
    end function path_separator_for

end module test_filesystem_helpers
