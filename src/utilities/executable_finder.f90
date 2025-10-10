module executable_finder
    ! Utility to locate the fortfront executable for testing
    ! Supports both Unix and Windows platforms
    implicit none
    private

    public :: find_fortfront_executable

contains

    ! Find the fortfront executable using multiple search strategies
    function find_fortfront_executable() result(executable_path)
        character(len=:), allocatable :: executable_path
        logical :: file_exists, on_windows
        character(len=500) :: candidate_path
        integer :: i, exit_code, unit_num
        character(len=256) :: search_output
        character(len=50), dimension(20) :: build_patterns

        executable_path = ""
        on_windows = is_windows_platform()

        ! Windows: locate built executable reliably via dir search
        if (on_windows) then
            ! Try multiple search roots to reliably locate build sibling directories
            block
                character(len=64), allocatable :: roots(:)
                integer :: r
                allocate(roots(5))
                roots = [ character(len=16) :: '.', '..', '..\\..', '..\\..\\..', &
                          '..\\..\\..\\..' ]
                do r = 1, size(roots)
                    call execute_command_line('cmd /C where /R ' // trim(roots(r)) // &
                        ' fortfront.exe > fortfront_search_win.txt', exitstat=exit_code)
                    if (exit_code == 0) then
                        open(newunit=unit_num, file='fortfront_search_win.txt', &
                             status='old', action='read', iostat=exit_code)
                        if (exit_code == 0) then
                            do
                                read(unit_num, '(A)', iostat=exit_code) search_output
                                if (exit_code /= 0) exit
                                if (len_trim(search_output) > 0) then
                                    ! Prefer app\fortfront.exe path if present
                                    if (index(adjustl(search_output), &
                                        'app\\fortfront.exe') > 0) then
                                        inquire(file=trim(search_output), exist=file_exists)
                                        if (file_exists) then
                                            executable_path = trim(search_output)
                                            exit
                                        end if
                                    end if
                                end if
                            end do
                            rewind(unit_num)
                            if (len(executable_path) == 0) then
                                ! Fallback: take first found fortfront.exe
                                read(unit_num, '(A)', iostat=exit_code) search_output
                                if (exit_code == 0 .and. len_trim(search_output) > 0) then
                                    inquire(file=trim(search_output), exist=file_exists)
                                    if (file_exists) executable_path = trim(search_output)
                                end if
                            end if
                            close(unit_num)
                        end if
                        call execute_command_line('cmd /C del /F /Q fortfront_search_win.txt', &
                                                  exitstat=exit_code)
                    end if
                    if (len(executable_path) > 0) return
                end do
            end block

            ! Fallback candidates
            candidate_path = 'app\\fortfront.exe'
            inquire(file=candidate_path, exist=file_exists)
            if (file_exists) then
                executable_path = trim(candidate_path)
                return
            end if
            executable_path = ''
            return
        end if

        ! Unix: Use find command to dynamically locate fortfront executable
        call execute_command_line( &
            'find build -name "fortfront" -type f | head -1 > fortfront_search.txt', &
            exitstat=exit_code)
        if (exit_code == 0) then
            open(newunit=unit_num, file='fortfront_search.txt', status='old', &
                 action='read', iostat=exit_code)
            if (exit_code == 0) then
                read(unit_num, '(A)', iostat=exit_code) search_output
                close(unit_num)
                call execute_command_line('rm -f fortfront_search.txt', exitstat=exit_code)
                if (exit_code == 0 .and. len_trim(search_output) > 0) then
                    inquire(file=trim(search_output), exist=file_exists)
                    if (file_exists) then
                        executable_path = trim(search_output)
                        return
                    end if
                end if
            end if
        end if

        ! Fallback: Check hardcoded patterns as last resort
        build_patterns = [ &
            "build/gfortran_266FF454AB2555FE/app/fortfront   ", &
            "build/gfortran_9ABCD662468F5A74/app/fortfront   ", &
            "build/gfortran_C79DEB301B8081FC/app/fortfront   ", &
            "build/gfortran_C523F0F8A99FF060/app/fortfront   ", &
            "build/gfortran_1F2DC83CBD1DC595/app/fortfront   ", &
            "build/gfortran_35CFD5CFC35942D6/app/fortfront   ", &
            "build/gfortran_4AE9E4ED7A89B913/app/fortfront   ", &
            "build/gfortran_66DBF6172AF51040/app/fortfront   ", &
            "build/gfortran_A56298966DD7666C/app/fortfront   ", &
            "build/gfortran_E3D58E6D75301430/app/fortfront   ", &
            "build/gfortran_9CBC8EEC13D00A4A/app/fortfront   ", &
            "./build/gfortran_266FF454AB2555FE/app/fortfront ", &
            "./build/gfortran_9ABCD662468F5A74/app/fortfront ", &
            "./build/gfortran_C79DEB301B8081FC/app/fortfront ", &
            "./build/gfortran_C523F0F8A99FF060/app/fortfront ", &
            "fortfront                                       ", &
            "./fortfront                                     ", &
            "app/fortfront                                   ", &
            "./app/fortfront                                 ", &
            "../fortfront                                    " ]

        ! Check each candidate path
        do i = 1, size(build_patterns)
            candidate_path = trim(build_patterns(i))
            inquire(file=candidate_path, exist=file_exists)

            if (file_exists) then
                executable_path = trim(candidate_path)
                return
            end if
        end do

    end function find_fortfront_executable

    ! Detect if running on Windows platform
    function is_windows_platform() result(is_win)
        logical :: is_win
        character(len=10) :: os_name
        integer :: stat

        call get_environment_variable('OS', os_name, status=stat)
        is_win = (stat == 0 .and. os_name(1:7) == 'Windows')

        if (.not. is_win) then
            call get_environment_variable('WINDIR', os_name, status=stat)
            is_win = (stat == 0)
        end if
    end function is_windows_platform

end module executable_finder
