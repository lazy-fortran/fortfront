module temp_file_helper
    ! Cross-platform temporary file path utilities for testing
    implicit none
    private
    public :: get_temp_dir, get_temp_filepath

contains

    function get_temp_dir() result(temp_dir)
        ! Returns platform-specific temporary directory path
        character(len=:), allocatable :: temp_dir
        character(len=512) :: env_value
        integer :: status
        logical :: dir_exists

        if (is_windows_platform()) then
            ! Try TEMP, then TMP environment variables
            call get_environment_variable("TEMP", env_value, status=status)
            if (status /= 0 .or. len_trim(env_value) == 0) then
                call get_environment_variable("TMP", env_value, status=status)
            end if
            if (status == 0 .and. len_trim(env_value) > 0) then
                temp_dir = trim(env_value)
            else
                ! Fallback to current directory tmp subdirectory
                temp_dir = "tmp"
            end if
        else
            ! Unix/Linux: use /tmp
            inquire (file="/tmp/.", exist=dir_exists)
            if (dir_exists) then
                temp_dir = "/tmp"
            else
                ! Fallback to current directory tmp subdirectory
                temp_dir = "tmp"
            end if
        end if
    end function get_temp_dir

    function get_temp_filepath(filename) result(filepath)
        ! Returns platform-specific temporary file path
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: filepath
        character(len=:), allocatable :: temp_dir
        character(len=1) :: sep

        temp_dir = get_temp_dir()

        if (is_windows_platform()) then
            sep = "\"
        else
            sep = "/"
        end if

        filepath = trim(temp_dir) // sep // trim(filename)
    end function get_temp_filepath

    function is_windows_platform() result(is_win)
        ! Detect if running on Windows
        logical :: is_win
        character(len=512) :: os_value
        integer :: status

        call get_environment_variable("OS", os_value, status=status)
        is_win = (status == 0 .and. index(os_value, "Windows") > 0)

        if (.not. is_win) then
            call get_environment_variable("COMSPEC", os_value, status=status)
            is_win = (status == 0 .and. len_trim(os_value) > 0)
        end if
    end function is_windows_platform

end module temp_file_helper
