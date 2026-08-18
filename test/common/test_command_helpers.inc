    function test_executable_path(stem) result(path)
        character(len=*), intent(in) :: stem
        character(len=:), allocatable :: path

        if (is_windows()) then
            path = 'build/'//trim(stem)//'.exe'
        else
            path = '/tmp/'//trim(stem)
        end if
    end function test_executable_path

    subroutine test_remove_file(path)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: command

        if (is_windows()) then
            command = 'cmd /c del /q "'//trim(path)//'" >nul 2>&1'
        else
            command = 'rm -f "'//trim(path)//'"'
        end if
        call execute_command_line(command, wait=.true.)
    end subroutine test_remove_file

    logical function is_windows()
        character(len=128) :: os_value
        integer :: os_len, os_status

        is_windows = .false.
        call get_environment_variable('OS', value=os_value, length=os_len, &
                                      status=os_status)
        if (os_status == 0 .and. os_len > 0) then
            is_windows = trim(os_value(:os_len)) == 'Windows_NT'
        end if
        if (.not. is_windows) then
            call get_environment_variable('WINDIR', value=os_value, &
                                          length=os_len, status=os_status)
            is_windows = os_status == 0
        end if
    end function is_windows
