module path_validation
    ! Secure path validation utility for preventing path traversal attacks
    ! Validates file paths against common security vulnerabilities

    use string_utils_mod, only: to_lower

    implicit none
    private

    ! Public interface
    public :: validate_file_path, validate_input_path, validate_output_path
    public :: path_validation_result_t, PATH_VALID, PATH_INVALID_TRAVERSAL, &
              PATH_INVALID_ABSOLUTE, PATH_INVALID_CHARACTERS, PATH_INVALID_EXTENSION

    ! Validation result codes
    integer, parameter :: PATH_VALID = 0
    integer, parameter :: PATH_INVALID_TRAVERSAL = 1
    integer, parameter :: PATH_INVALID_ABSOLUTE = 2
    integer, parameter :: PATH_INVALID_CHARACTERS = 3
    integer, parameter :: PATH_INVALID_EXTENSION = 4

    ! Path validation result type
    type :: path_validation_result_t
        integer :: code = PATH_VALID
        character(len=:), allocatable :: message
    contains
        procedure :: is_valid => path_validation_result_is_valid
        procedure :: get_message => path_validation_result_get_message
    end type path_validation_result_t

contains

    ! Main path validation function for file operations
    function validate_file_path(filepath, allow_absolute) result(result)
        character(len=*), intent(in) :: filepath
        logical, intent(in), optional :: allow_absolute
        type(path_validation_result_t) :: result

        logical :: allow_abs

        ! Default to not allowing absolute paths for security
        allow_abs = .false.
        if (present(allow_absolute)) allow_abs = allow_absolute

        ! Initialize result
        result%code = PATH_VALID

        ! Check for directory traversal sequences
        if (contains_traversal_sequences(filepath)) then
            result%code = PATH_INVALID_TRAVERSAL
            result%message = "Path contains directory traversal sequences (../ or ..\)"
            return
        end if

        ! Check for absolute paths when not allowed
        if (.not. allow_abs .and. is_absolute_path(filepath)) then
            result%code = PATH_INVALID_ABSOLUTE
            result%message = "Absolute paths are not allowed in this context"
            return
        end if

        ! Check for invalid characters
        if (contains_invalid_characters(filepath)) then
            result%code = PATH_INVALID_CHARACTERS
            result%message = &
                "Path contains invalid or potentially dangerous characters"
            return
        end if

        ! Check for suspicious file extensions (optional additional security)
        if (has_suspicious_extension(filepath)) then
            result%code = PATH_INVALID_EXTENSION
            result%message = "File extension is not allowed"
            return
        end if

        result%message = "Path validation successful"
    end function validate_file_path

    ! Specialized validation for input file paths (reading)
    function validate_input_path(filepath) result(result)
        character(len=*), intent(in) :: filepath
        type(path_validation_result_t) :: result

        ! Allow absolute paths for input files for flexibility
        result = validate_file_path(filepath, allow_absolute=.true.)

        ! Additional checks specific to input files
        if (result%is_valid()) then
            ! Check if path is empty or whitespace only
            if (len_trim(filepath) == 0) then
                result%code = PATH_INVALID_CHARACTERS
                result%message = "Input file path cannot be empty"
                return
            end if
        end if
    end function validate_input_path

    ! Specialized validation for output file paths (writing)
    function validate_output_path(filepath) result(result)
        character(len=*), intent(in) :: filepath
        type(path_validation_result_t) :: result

        ! Be more restrictive with output paths - don't allow absolute paths by default
        result = validate_file_path(filepath, allow_absolute=.false.)

        ! Additional checks specific to output files
        if (result%is_valid()) then
            ! Check if path is empty or whitespace only
            if (len_trim(filepath) == 0) then
                result%code = PATH_INVALID_CHARACTERS
                result%message = "Output file path cannot be empty"
                return
            end if

            ! Ensure output path doesn't try to overwrite system files
            if (is_system_path(filepath)) then
                result%code = PATH_INVALID_CHARACTERS
                result%message = "Cannot write to system directories or files"
                return
            end if
        end if
    end function validate_output_path

    ! Check for directory traversal sequences
    function contains_traversal_sequences(path) result(has_traversal)
        character(len=*), intent(in) :: path
        logical :: has_traversal

        ! Check for common traversal patterns
        has_traversal = .false.

        ! Check for ../ (Unix-style)
        if (index(path, '../') > 0) then
            has_traversal = .true.
            return
        end if

        ! Check for ..\ (Windows-style)
        if (index(path, '..\') > 0) then
            has_traversal = .true.
            return
        end if

        ! Check for .. at the beginning or end
        if (len(path) >= 2) then
            if (path(1:2) == '..' .or. path(len(path) - 1:len(path)) == '..') then
                has_traversal = .true.
                return
            end if
        end if

        ! Check for encoded traversal sequences
        if (index(path, '%2e%2e') > 0 .or. index(path, '%2E%2E') > 0) then
            has_traversal = .true.
            return
        end if

        ! Check for double-encoded sequences
        if (index(path, '%252e%252e') > 0 .or. index(path, '%252E%252E') > 0) then
            has_traversal = .true.
            return
        end if
    end function contains_traversal_sequences

    ! Check if path is absolute
    function is_absolute_path(path) result(is_absolute)
        character(len=*), intent(in) :: path
        logical :: is_absolute

        is_absolute = .false.

        if (len_trim(path) == 0) return

        ! Unix/Linux absolute path (starts with /)
        if (path(1:1) == '/') then
            is_absolute = .true.
            return
        end if

        ! Windows absolute path (C:\ or similar)
        if (len(path) >= 3) then
            if (path(2:2) == ':' .and. (path(3:3) == '\' .or. path(3:3) == '/')) then
                ! Check if first character is a letter
                if ((path(1:1) >= 'A' .and. path(1:1) <= 'Z') .or. &
                    (path(1:1) >= 'a' .and. path(1:1) <= 'z')) then
                    is_absolute = .true.
                    return
                end if
            end if
        end if

        ! Windows UNC path (\\server\share)
        if (len(path) >= 2) then
            if (path(1:2) == '\\' .or. path(1:2) == '//') then
                is_absolute = .true.
                return
            end if
        end if
    end function is_absolute_path

    ! Check for invalid or dangerous characters
    function contains_invalid_characters(path) result(has_invalid)
        character(len=*), intent(in) :: path
        logical :: has_invalid
        integer :: i
        character(len=1) :: c
        logical :: windows_like

        has_invalid = .false.
        windows_like = is_likely_windows_path(path)

        do i = 1, len(path)
            c = path(i:i)

            ! Check for null bytes
            if (ichar(c) == 0) then
                has_invalid = .true.
                return
            end if

     ! Check for control characters (except newline and tab which are handled elsewhere)
            if (ichar(c) < 32 .and. ichar(c) /= 9 .and. ichar(c) /= 10) then
                has_invalid = .true.
                return
            end if

            ! Check for dangerous characters that could be used in attacks
            select case (c)
            case ('|', '&', ';', '`', '$', '(', ')', '{', '}', '[', ']')
                ! These could be used for command injection
                has_invalid = .true.
                return
            case ('<', '>')
                ! These could be used for redirection attacks
                has_invalid = .true.
                return
            case ('*', '?')
                ! On Windows, these are invalid path characters. On POSIX they are valid
                ! filename characters; allow them to avoid false positives.
                if (windows_like) then
                    has_invalid = .true.
                    return
                end if
            end select
        end do
    end function contains_invalid_characters

    ! Heuristic check for Windows-style paths (drive letter, backslashes, UNC)
    pure logical function is_likely_windows_path(path) result(is_win)
        character(len=*), intent(in) :: path
        integer :: n
        is_win = .false.
        n = len(path)
        if (n >= 2) then
            if (path(1:2) == '\\' .or. path(1:2) == '//') then
                is_win = .true.
                return
            end if
        end if
        if (n >= 3) then
            if (path(2:2) == ':' .and. (path(3:3) == '\\' .or. path(3:3) == '/')) then
                if ((path(1:1) >= 'A' .and. path(1:1) <= 'Z') .or. &
                    (path(1:1) >= 'a' .and. path(1:1) <= 'z')) then
                    is_win = .true.
                    return
                end if
            end if
        end if
        ! If the path contains backslashes anywhere, treat it as Windows-like
        if (index(path, '\\') > 0) then
            is_win = .true.
            return
        end if
    end function is_likely_windows_path

    ! Check for suspicious file extensions
    function has_suspicious_extension(path) result(is_suspicious)
        character(len=*), intent(in) :: path
        logical :: is_suspicious
        character(len=:), allocatable :: extension
        integer :: dot_pos

        is_suspicious = .false.

        ! Find the last dot in the path
        dot_pos = 0
        block
            integer :: i
            do i = len(path), 1, -1
                if (path(i:i) == '.') then
                    dot_pos = i
                    exit
                end if
                ! Stop at directory separators
                if (path(i:i) == '/' .or. path(i:i) == '\') exit
            end do
        end block

        if (dot_pos == 0 .or. dot_pos == len(path)) return

        ! Extract extension (convert to lowercase for comparison)
        extension = to_lower(path(dot_pos + 1:))

        ! Check against list of potentially dangerous extensions
        select case (extension)
        case ('exe', 'bat', 'cmd', 'com', 'scr', 'pif', 'vbs', 'js', 'jar')
            is_suspicious = .true.
        case ('sh', 'bash', 'csh', 'fish', 'ksh', 'zsh')
            is_suspicious = .true.
        case ('py', 'pl', 'rb', 'php')
            is_suspicious = .true.
        end select
    end function has_suspicious_extension

    ! Check if path points to system directories
    function is_system_path(path) result(is_system)
        character(len=*), intent(in) :: path
        logical :: is_system
        character(len=:), allocatable :: normalized_path

        is_system = .false.
        normalized_path = to_lower(trim(path))

        ! Check for common system directories (Unix/Linux)
        if (starts_with(normalized_path, '/etc/') .or. &
            starts_with(normalized_path, '/bin/') .or. &
            starts_with(normalized_path, '/sbin/') .or. &
            starts_with(normalized_path, '/usr/bin/') .or. &
            starts_with(normalized_path, '/usr/sbin/') .or. &
            starts_with(normalized_path, '/boot/') .or. &
            starts_with(normalized_path, '/dev/') .or. &
            starts_with(normalized_path, '/proc/') .or. &
            starts_with(normalized_path, '/sys/')) then
            is_system = .true.
            return
        end if

        ! Check for Windows system directories
        if (starts_with(normalized_path, 'c:\windows\') .or. &
            starts_with(normalized_path, 'c:\program files\') .or. &
            starts_with(normalized_path, 'c:\program files (x86)\') .or. &
            starts_with(normalized_path, 'c:\system32\')) then
            is_system = .true.
            return
        end if
    end function is_system_path

    ! Helper function to check if string starts with prefix
    function starts_with(str, prefix) result(matches)
        character(len=*), intent(in) :: str, prefix
        logical :: matches

        matches = .false.
        if (len(str) >= len(prefix)) then
            matches = (str(1:len(prefix)) == prefix)
        end if
    end function starts_with

    ! Check if validation result indicates valid path
    logical function path_validation_result_is_valid(this)
        class(path_validation_result_t), intent(in) :: this
        path_validation_result_is_valid = (this%code == PATH_VALID)
    end function path_validation_result_is_valid

    ! Get validation result message
    function path_validation_result_get_message(this) result(message)
        class(path_validation_result_t), intent(in) :: this
        character(len=:), allocatable :: message

        if (allocated(this%message)) then
            message = this%message
        else
            message = "No message available"
        end if
    end function path_validation_result_get_message

end module path_validation
