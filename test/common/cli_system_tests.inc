    subroutine make_tmpfile(path)
        character(len=*), intent(out) :: path
        integer :: clock_count
        character(len=256) :: buffer

        call system_clock(count=clock_count)
        write (buffer, '(A,I0,A)') 'build/tmp_cli_sanitize_', clock_count, '.tmp'
        path = trim(buffer)
    end subroutine make_tmpfile

    subroutine delete_file(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios

        open (newunit=unit, file=path, status='old', action='readwrite', &
              iostat=ios)
        if (ios == 0) then
            close (unit, status='delete')
        end if
    end subroutine delete_file

    subroutine assert_equal_int(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) then
            write (error_unit, '(A,A,I0,A,I0)') trim(message), ' (actual=', &
                actual, ', expected=', expected, ')'
            error stop 1
        end if
    end subroutine assert_equal_int

    function should_run_system_tests() result(run)
        logical :: run
        character(len=16) :: val
        integer :: stat

        run = .false.
        call get_environment_variable('RUN_SYSTEM_TESTS', val, status=stat)
        if (stat == 0) then
            if (len_trim(val) > 0) then
                select case (adjustl(val(1:1)))
                case ('1', 'y', 'Y', 't', 'T')
                    run = .true.
                end select
            end if
        end if
    end function should_run_system_tests

    subroutine find_executable(exe_path, found)
        character(len=*), intent(out) :: exe_path
        logical, intent(out) :: found
        integer :: exit_code, unit_num
        character(len=512) :: search_output
        logical :: file_exists
        character(len=32) :: temp_file

        ! Cross-platform executable search
        if (is_windows()) then
            temp_file = 'fortfront_search.txt'
            call execute_command_line( &
                'dir /s /b build\fortfront.exe 2>nul > ' // trim(temp_file), &
                exitstat=exit_code)
            if (exit_code == 0) then
                open (newunit=unit_num, file=trim(temp_file), status='old', &
                    action='read', iostat=exit_code)
                if (exit_code == 0) then
                    read (unit_num, '(A)', iostat=exit_code) search_output
                    close (unit_num)
                    call execute_command_line('del ' // trim(temp_file), &
                                              exitstat=exit_code)
                    if (exit_code == 0 .and. len_trim(search_output) > 0) then
                        inquire (file=trim(search_output), exist=file_exists)
                        if (file_exists) then
                            exe_path = trim(search_output)
                            found = .true.
                            return
                        end if
                    end if
                end if
            end if
        else
            temp_file = 'fortfront_search.txt'
            call execute_command_line( &
                'find build -name "fortfront" -type f | head -1 > ' // &
                trim(temp_file), exitstat=exit_code)
            if (exit_code == 0) then
                open (newunit=unit_num, file=trim(temp_file), status='old', &
                    action='read', iostat=exit_code)
                if (exit_code == 0) then
                    read (unit_num, '(A)', iostat=exit_code) search_output
                    close (unit_num)
                    call execute_command_line('rm -f ' // trim(temp_file), &
                                              exitstat=exit_code)
                    if (exit_code == 0 .and. len_trim(search_output) > 0) then
                        inquire (file=trim(search_output), exist=file_exists)
                        if (file_exists) then
                            exe_path = trim(search_output)
                            found = .true.
                            return
                        end if
                    end if
                end if
            end if
        end if

        exe_path = ''
        found = .false.
    end subroutine find_executable

    function is_windows() result(windows)
        logical :: windows
        character(len=32) :: val
        integer :: stat

        windows = .false.
        call get_environment_variable('OS', val, status=stat)
        if (stat == 0) then
            if (index(trim(val), 'Windows') > 0) then
                windows = .true.
                return
            end if
        end if

        call get_environment_variable('WINDIR', val, status=stat)
        if (stat == 0) then
            windows = .true.
            return
        end if

        call get_environment_variable('COMSPEC', val, status=stat)
        if (stat == 0) then
            if (index(trim(val), 'cmd.exe') > 0 .or. &
                index(trim(val), 'CMD.EXE') > 0) then
                windows = .true.
                return
            end if
        end if
    end function is_windows

