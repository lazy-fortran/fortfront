module frontend_diagnostics
    use fortfront_constants, only: MAX_DIAGNOSTIC_MESSAGE_LEN
    use fortfront_types, only: diagnostic_t, source_range_t, &
        DIAGNOSTIC_ERROR, DIAGNOSTIC_WARNING, DIAGNOSTIC_INFO, DIAGNOSTIC_HINT
    implicit none
    private

    public :: make_diagnostic, format_diagnostic, &
        DIAG_EMPTY_INPUT, DIAG_BINARY_DATA, DIAG_LEXICAL_ERROR, &
        DIAG_SYNTAX_ERROR, DIAG_SEMANTIC_ERROR, DIAG_PARSE_ERROR, &
        DIAG_NO_PROGRAM_UNIT, &
        DIAGNOSTIC_ERROR, DIAGNOSTIC_WARNING, DIAGNOSTIC_INFO, DIAGNOSTIC_HINT

    ! Diagnostic codes following GCC-style convention
    character(len=*), parameter :: DIAG_EMPTY_INPUT = "F001"
    character(len=*), parameter :: DIAG_BINARY_DATA = "F002"
    character(len=*), parameter :: DIAG_LEXICAL_ERROR = "F003"
    character(len=*), parameter :: DIAG_SYNTAX_ERROR = "F004"
    character(len=*), parameter :: DIAG_SEMANTIC_ERROR = "F005"
    character(len=*), parameter :: DIAG_PARSE_ERROR = "F006"
    character(len=*), parameter :: DIAG_NO_PROGRAM_UNIT = "F007"

contains

    function make_diagnostic(code, severity, message, location, category) &
            result(diag)
        character(len=*), intent(in) :: code
        integer, intent(in) :: severity
        character(len=*), intent(in) :: message
        type(source_range_t), intent(in), optional :: location
        character(len=*), intent(in), optional :: category
        type(diagnostic_t) :: diag

        diag%code = code
        diag%severity = severity
        diag%message = message

        if (present(location)) then
            diag%location = location
        else
            ! Default location: line 1, column 1
            diag%location%start%line = 1
            diag%location%start%column = 1
            diag%location%start%byte_offset = 0
            diag%location%end%line = 1
            diag%location%end%column = 1
            diag%location%end%byte_offset = 0
        end if

        if (present(category)) then
            diag%category = category
        else
            diag%category = ""
        end if
    end function make_diagnostic

    function format_diagnostic(diag) result(formatted)
        type(diagnostic_t), intent(in) :: diag
        character(len=:), allocatable :: formatted
        character(len=:), allocatable :: severity_str
        character(len=MAX_DIAGNOSTIC_MESSAGE_LEN) :: buffer

        ! Determine severity string
        select case (diag%severity)
        case (DIAGNOSTIC_ERROR)
            severity_str = "ERROR"
        case (DIAGNOSTIC_WARNING)
            severity_str = "WARNING"
        case (DIAGNOSTIC_INFO)
            severity_str = "INFO"
        case (DIAGNOSTIC_HINT)
            severity_str = "HINT"
        case default
            severity_str = "UNKNOWN"
        end select

        ! Format: [CODE] SEVERITY at line X:Y: message
        if (diag%location%start%line > 0) then
            write (buffer, '(A,I0,A,I0,A)') &
                " at line ", diag%location%start%line, &
                ":", diag%location%start%column, ":"
            formatted = "["//trim(diag%code)//"] "//trim(severity_str) &
                //trim(buffer)//" "//diag%message
        else
            formatted = "["//trim(diag%code)//"] "//trim(severity_str) &
                //": "//diag%message
        end if
    end function format_diagnostic

end module frontend_diagnostics
