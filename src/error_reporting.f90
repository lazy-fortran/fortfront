module error_reporting
    use lexer_core, only: token_t
    implicit none
    private

    ! Error severity levels
    integer, parameter, public :: ERROR_INFO = 1
    integer, parameter, public :: ERROR_WARNING = 2
    integer, parameter, public :: ERROR_ERROR = 3
    integer, parameter, public :: ERROR_FATAL = 4

    ! Error context type for tracking location and context
    type, public :: error_context_t
        integer :: line = 0
        integer :: column = 0
        character(len=:), allocatable :: filename
        character(len=:), allocatable :: source_line
    end type error_context_t

    ! Individual error record
    type, public :: error_record_t
        integer :: severity = ERROR_ERROR
        character(len=:), allocatable :: message
        character(len=:), allocatable :: suggestion
        type(error_context_t) :: context
    end type error_record_t

    ! Error collection for accumulating multiple errors
    type, public :: error_collection_t
        type(error_record_t), allocatable :: errors(:)
        integer :: count = 0
        logical :: has_fatal = .false.
    contains
        procedure :: add_error => error_collection_add_error
        procedure :: add_error_with_token => error_collection_add_error_with_token
        procedure :: add_error_with_context => error_collection_add_error_with_context
        procedure :: has_errors => error_collection_has_errors
        procedure :: format_messages => error_collection_format_messages
        procedure :: clear => error_collection_clear
    end type error_collection_t

    ! Public interface
    public :: create_error_context
    public :: create_error_context_from_token
    public :: format_error_message

contains

    ! Create error context from line/column information
    function create_error_context(line, column, filename, source_line) result(context)
        integer, intent(in) :: line, column
        character(len=*), intent(in), optional :: filename, source_line
        type(error_context_t) :: context
        
        context%line = line
        context%column = column
        
        if (present(filename)) then
            context%filename = filename
        end if
        
        if (present(source_line)) then
            context%source_line = source_line
        end if
    end function create_error_context

    ! Create error context from token
    function create_error_context_from_token(token, filename, source_lines) result(context)
        type(token_t), intent(in) :: token
        character(len=*), intent(in), optional :: filename
        character(len=*), intent(in), optional :: source_lines(:)
        type(error_context_t) :: context
        
        context%line = token%line
        context%column = token%column
        
        if (present(filename)) then
            context%filename = filename
        end if
        
        if (present(source_lines) .and. token%line > 0 .and. token%line <= size(source_lines)) then
            context%source_line = source_lines(token%line)
        end if
    end function create_error_context_from_token

    ! Add error to collection
    subroutine error_collection_add_error(self, message, severity, suggestion)
        class(error_collection_t), intent(inout) :: self
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: severity
        character(len=*), intent(in), optional :: suggestion
        
        type(error_record_t), allocatable :: temp_errors(:)
        integer :: new_size, sev
        
        sev = ERROR_ERROR
        if (present(severity)) sev = severity
        
        ! Expand array if needed
        if (.not. allocated(self%errors)) then
            allocate(self%errors(10))
        else if (self%count >= size(self%errors)) then
            new_size = size(self%errors) * 2
            allocate(temp_errors(new_size))
            temp_errors(1:self%count) = self%errors(1:self%count)
            call move_alloc(temp_errors, self%errors)
        end if
        
        ! Add new error
        self%count = self%count + 1
        self%errors(self%count)%message = message
        self%errors(self%count)%severity = sev
        
        if (present(suggestion)) then
            self%errors(self%count)%suggestion = suggestion
        end if
        
        if (sev == ERROR_FATAL) self%has_fatal = .true.
    end subroutine error_collection_add_error

    ! Add error with token context
    subroutine error_collection_add_error_with_token(self, message, token, severity, suggestion)
        class(error_collection_t), intent(inout) :: self
        character(len=*), intent(in) :: message
        type(token_t), intent(in) :: token
        integer, intent(in), optional :: severity
        character(len=*), intent(in), optional :: suggestion
        
        call self%add_error(message, severity, suggestion)
        self%errors(self%count)%context = create_error_context_from_token(token)
    end subroutine error_collection_add_error_with_token

    ! Add error with explicit context
    subroutine error_collection_add_error_with_context(self, message, context, severity, suggestion)
        class(error_collection_t), intent(inout) :: self
        character(len=*), intent(in) :: message
        type(error_context_t), intent(in) :: context
        integer, intent(in), optional :: severity
        character(len=*), intent(in), optional :: suggestion
        
        call self%add_error(message, severity, suggestion)
        self%errors(self%count)%context = context
    end subroutine error_collection_add_error_with_context

    ! Check if collection has any errors
    function error_collection_has_errors(self) result(has_errors)
        class(error_collection_t), intent(in) :: self
        logical :: has_errors
        has_errors = self%count > 0
    end function error_collection_has_errors

    ! Format all error messages into a single string
    function error_collection_format_messages(self) result(formatted)
        class(error_collection_t), intent(in) :: self
        character(len=:), allocatable :: formatted
        integer :: i
        character(len=500) :: temp_msg
        
        if (self%count == 0) then
            formatted = ""
            return
        end if
        
        formatted = ""
        do i = 1, self%count
            formatted = formatted // format_error_message(self%errors(i)) // new_line('a')
        end do
    end function error_collection_format_messages

    ! Clear all errors
    subroutine error_collection_clear(self)
        class(error_collection_t), intent(inout) :: self
        if (allocated(self%errors)) deallocate(self%errors)
        self%count = 0
        self%has_fatal = .false.
    end subroutine error_collection_clear

    ! Format a single error message
    function format_error_message(error) result(formatted)
        type(error_record_t), intent(in) :: error
        character(len=:), allocatable :: formatted
        character(len=20) :: severity_str
        character(len=20) :: location_str
        
        ! Format severity
        select case (error%severity)
        case (ERROR_INFO)
            severity_str = "INFO"
        case (ERROR_WARNING)
            severity_str = "WARNING"
        case (ERROR_ERROR)
            severity_str = "ERROR"
        case (ERROR_FATAL)
            severity_str = "FATAL"
        case default
            severity_str = "UNKNOWN"
        end select
        
        ! Format location if available
        if (error%context%line > 0) then
            write(location_str, '("line ", I0, ", col ", I0)') error%context%line, error%context%column
        else
            location_str = ""
        end if
        
        ! Build formatted message
        if (len_trim(location_str) > 0) then
            formatted = trim(severity_str) // " at " // trim(location_str) // ": " // error%message
        else
            formatted = trim(severity_str) // ": " // error%message
        end if
        
        ! Add source line context if available
        if (allocated(error%context%source_line) .and. error%context%column > 0) then
            formatted = formatted // new_line('a') // "  " // error%context%source_line
            if (error%context%column <= len(error%context%source_line)) then
                formatted = formatted // new_line('a') // "  " // &
                           repeat(" ", error%context%column - 1) // "^"
            end if
        end if
        
        ! Add suggestion if available
        if (allocated(error%suggestion)) then
            formatted = formatted // new_line('a') // "  Suggestion: " // error%suggestion
        end if
    end function format_error_message

end module error_reporting