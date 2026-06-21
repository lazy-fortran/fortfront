module cli_env
    use fortfront_constants, only: MAX_TRACE_FILE_PATH_LEN
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: init_cli_trace
    public :: compute_cli_trace_settings
    public :: is_truthy
    public :: parse_trace_option
    public :: parse_trace_flag_value

contains

    subroutine init_cli_trace(trace_enabled, trace_file_path)
        logical, intent(out) :: trace_enabled
        character(len=:), allocatable, intent(out) :: trace_file_path
        character(len=64) :: trace_env
        character(len=MAX_TRACE_FILE_PATH_LEN) :: file_env
        integer :: s1, s2

        trace_env = ''
        file_env = ''
        call get_environment_variable('FORTFRONT_TRACE', trace_env, status=s1)
        call get_environment_variable('FORTFRONT_TRACE_FILE', file_env, status=s2)

        call compute_cli_trace_settings(trim(trace_env), trim(file_env), &
                                        trace_enabled, &
                                        trace_file_path)
    end subroutine init_cli_trace

    pure subroutine compute_cli_trace_settings(trace_env, file_env, trace_enabled, &
                                               trace_file_path)
        character(len=*), intent(in) :: trace_env
        character(len=*), intent(in) :: file_env
        logical, intent(out) :: trace_enabled
        character(len=:), allocatable, intent(out) :: trace_file_path

        trace_enabled = .false.
        trace_file_path = 'cli_trace.txt'

        if (len_trim(trace_env) > 0) then
            trace_enabled = is_truthy(trim(trace_env))
        end if

        if (len_trim(file_env) > 0) then
            trace_file_path = trim(file_env)
        end if
    end subroutine compute_cli_trace_settings

    pure logical function parse_trace_flag_value(s) result(val)
        character(len=*), intent(in) :: s
        if (len_trim(s) == 0) then
            val = .true.
        else
            val = is_truthy(s)
        end if
    end function parse_trace_flag_value

    pure subroutine parse_trace_option(arg, recognized, is_file, value)
        character(len=*), intent(in) :: arg
        logical, intent(out) :: recognized
        logical, intent(out) :: is_file
        character(len=:), allocatable, intent(out) :: value
        integer :: eq
        recognized = .false.
        is_file = .false.
        value = ''
        if (len_trim(arg) == 0) return
        if (index(arg, '--trace-file') == 1) then
            recognized = .true.
            is_file = .true.
            eq = index(arg, '=')
            if (eq > 0 .and. eq < len(arg)) then
                value = trim(arg(eq + 1:))
            else
                value = ''
            end if
            return
        end if
        if (index(arg, '--trace') == 1) then
            recognized = .true.
            eq = index(arg, '=')
            if (eq > 0 .and. eq < len(arg)) then
                value = trim(arg(eq + 1:))
            else
                value = ''
            end if
            return
        end if
    end subroutine parse_trace_option

    pure logical function is_truthy(s) result(val)
        character(len=*), intent(in) :: s
        character(len=len_trim(s)) :: t
        t = to_lower(trim(s))
        select case (t)
        case ('0', 'false', 'off', 'no')
            val = .false.
        case default
            ! Any non-empty value not explicitly falsey is truthy
            val = (len(t) > 0)
        end select
    end function is_truthy

end module cli_env
