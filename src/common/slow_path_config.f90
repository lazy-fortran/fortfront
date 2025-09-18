module slow_path_config
    implicit none
    private

    logical, save :: slow_path_flag = .false.
    logical, save :: config_initialized = .false.

    public :: initialize_slow_path_from_env
    public :: set_slow_path_enabled
    public :: is_slow_path_enabled
    public :: reset_slow_path_config

contains

    subroutine initialize_slow_path_from_env()
        character(len=16) :: flag
        character(len=:), allocatable :: normalized
        integer :: status

        if (config_initialized) return

        slow_path_flag = .false.
        flag = ''
        call get_environment_variable('FORTFRONT_ENABLE_SLOW_PATH', flag, status=status)
        if (status == 0) then
            normalized = to_lower_trimmed(flag)
            if (len(normalized) == 0) then
                slow_path_flag = .true.
            else
                select case (normalized)
                case ('1', 'true', 'yes', 'on')
                    slow_path_flag = .true.
                case ('0', 'false', 'no', 'off')
                    slow_path_flag = .false.
                end select
            end if
        end if

        config_initialized = .true.
    end subroutine initialize_slow_path_from_env

    subroutine set_slow_path_enabled(enable)
        logical, intent(in) :: enable

        slow_path_flag = enable
        config_initialized = .true.
    end subroutine set_slow_path_enabled

    logical function is_slow_path_enabled()
        if (.not. config_initialized) call initialize_slow_path_from_env()
        is_slow_path_enabled = slow_path_flag
    end function is_slow_path_enabled

    subroutine reset_slow_path_config()
        slow_path_flag = .false.
        config_initialized = .false.
    end subroutine reset_slow_path_config

    pure function to_lower_trimmed(value) result(lowered)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: lowered
        integer :: i, length
        character(len=:), allocatable :: trimmed

        trimmed = adjustl(value)
        length = len_trim(trimmed)
        if (length <= 0) then
            allocate(character(len=0) :: lowered)
            return
        end if

        allocate(character(len=length) :: lowered)
        do i = 1, length
            lowered(i:i) = to_lower_char(trimmed(i:i))
        end do
    end function to_lower_trimmed

    pure function to_lower_char(ch) result(lower_ch)
        character(len=1), intent(in) :: ch
        character(len=1) :: lower_ch
        integer :: code

        lower_ch = ch
        code = iachar(ch)
        if (code >= iachar('A') .and. code <= iachar('Z')) then
            lower_ch = achar(code + (iachar('a') - iachar('A')))
        end if
    end function to_lower_char

end module slow_path_config
