module error_handling
    implicit none
    private

    ! Error severity levels
    integer, parameter, public :: ERROR_INFO = 1
    integer, parameter, public :: ERROR_WARNING = 2
    integer, parameter, public :: ERROR_ERROR = 3
    integer, parameter, public :: ERROR_CRITICAL = 4

    ! Error category codes
    integer, parameter, public :: ERROR_VALIDATION = 100
    integer, parameter, public :: ERROR_TYPE_SYSTEM = 200
    integer, parameter, public :: ERROR_MEMORY = 300
    integer, parameter, public :: ERROR_IO = 400
    integer, parameter, public :: ERROR_PARSER = 500
    integer, parameter, public :: ERROR_SEMANTIC = 600
    integer, parameter, public :: ERROR_INTERNAL = 700

    ! Result type for structured error handling
    type, public :: result_t
        logical :: success = .true.
        character(len=:), allocatable :: error_message
        integer :: error_code = 0
        integer :: severity = ERROR_INFO
        character(len=:), allocatable :: component
        character(len=:), allocatable :: context
        character(len=:), allocatable :: suggestion
    contains
        procedure :: is_success
        procedure :: is_failure
        procedure :: get_message
        procedure :: get_full_message
        procedure :: set_error
        procedure :: set_warning
        procedure :: set_critical
        procedure :: clear
        procedure :: combine_result
        generic :: assignment(=) => assign_result
        procedure, private :: assign_result
    end type result_t

    ! Error collection for multiple errors
    type, public :: error_collection_t
        type(result_t), allocatable :: errors(:)
        integer :: count = 0
        integer :: capacity = 0
    contains
        procedure :: add_error
        procedure :: add_result
        procedure :: has_errors
        procedure :: has_critical_errors
        procedure :: get_error_count
        procedure :: get_worst_severity
        procedure :: clear_errors
        procedure :: get_summary
        final :: cleanup_collection
    end type error_collection_t

    ! Public interface for creating results
    public :: success_result, create_error_result, warning_result, critical_result
    public :: create_error_collection, combine_results

contains

    ! Result type methods
    pure function is_success(this) result(success)
        class(result_t), intent(in) :: this
        logical :: success
        success = this%success
    end function is_success

    pure function is_failure(this) result(failure)
        class(result_t), intent(in) :: this
        logical :: failure
        failure = .not. this%success
    end function is_failure

    function get_message(this) result(message)
        class(result_t), intent(in) :: this
        character(len=:), allocatable :: message
        
        if (allocated(this%error_message)) then
            message = this%error_message
        else
            message = ""
        end if
    end function get_message

    function get_full_message(this) result(message)
        class(result_t), intent(in) :: this
        character(len=:), allocatable :: message
        character(len=20) :: severity_str, code_str
        
        ! Format severity
        select case (this%severity)
        case (ERROR_INFO)
            severity_str = "INFO"
        case (ERROR_WARNING)
            severity_str = "WARNING"
        case (ERROR_ERROR)
            severity_str = "ERROR"
        case (ERROR_CRITICAL)
            severity_str = "CRITICAL"
        case default
            severity_str = "UNKNOWN"
        end select
        
        ! Format error code
        write(code_str, '(I0)') this%error_code
        
        ! Build comprehensive message
        message = trim(severity_str)
        if (allocated(this%component)) then
            message = message // " [" // this%component // "]"
        end if
        if (this%error_code > 0) then
            message = message // " (" // trim(code_str) // ")"
        end if
        message = message // ": "
        
        if (allocated(this%error_message)) then
            message = message // this%error_message
        else
            message = message // "Unknown error"
        end if
        
        if (allocated(this%context)) then
            message = message // " [Context: " // this%context // "]"
        end if
        
        if (allocated(this%suggestion)) then
            message = message // " [Suggestion: " // this%suggestion // "]"
        end if
    end function get_full_message

    subroutine set_error(this, message, code, component, context, suggestion)
        class(result_t), intent(inout) :: this
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code
        character(len=*), intent(in), optional :: component, context, suggestion
        
        this%success = .false.
        this%severity = ERROR_ERROR
        this%error_message = trim(message)
        
        if (present(code)) this%error_code = code
        if (present(component)) this%component = trim(component)
        if (present(context)) this%context = trim(context)
        if (present(suggestion)) this%suggestion = trim(suggestion)
    end subroutine set_error

    subroutine set_warning(this, message, code, component, context, suggestion)
        class(result_t), intent(inout) :: this
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code
        character(len=*), intent(in), optional :: component, context, suggestion
        
        this%success = .true.  ! Warnings don't fail the operation
        this%severity = ERROR_WARNING
        this%error_message = trim(message)
        
        if (present(code)) this%error_code = code
        if (present(component)) this%component = trim(component)
        if (present(context)) this%context = trim(context)
        if (present(suggestion)) this%suggestion = trim(suggestion)
    end subroutine set_warning

    subroutine set_critical(this, message, code, component, context, suggestion)
        class(result_t), intent(inout) :: this
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code
        character(len=*), intent(in), optional :: component, context, suggestion
        
        this%success = .false.
        this%severity = ERROR_CRITICAL
        this%error_message = trim(message)
        
        if (present(code)) this%error_code = code
        if (present(component)) this%component = trim(component)
        if (present(context)) this%context = trim(context)
        if (present(suggestion)) this%suggestion = trim(suggestion)
    end subroutine set_critical

    subroutine clear(this)
        class(result_t), intent(inout) :: this
        
        this%success = .true.
        this%error_code = 0
        this%severity = ERROR_INFO
        if (allocated(this%error_message)) deallocate(this%error_message)
        if (allocated(this%component)) deallocate(this%component)
        if (allocated(this%context)) deallocate(this%context)
        if (allocated(this%suggestion)) deallocate(this%suggestion)
    end subroutine clear

    function combine_result(this, other) result(combined)
        class(result_t), intent(in) :: this, other
        type(result_t) :: combined
        
        ! Combined result fails if either fails
        combined%success = this%success .and. other%success
        
        ! Use worst severity
        combined%severity = max(this%severity, other%severity)
        
        ! Combine error codes (if both present, use the more severe one)
        if (this%severity >= other%severity) then
            combined%error_code = this%error_code
            if (allocated(this%error_message)) combined%error_message = this%error_message
            if (allocated(this%component)) combined%component = this%component
            if (allocated(this%context)) combined%context = this%context
            if (allocated(this%suggestion)) combined%suggestion = this%suggestion
        else
            combined%error_code = other%error_code
            if (allocated(other%error_message)) combined%error_message = other%error_message
            if (allocated(other%component)) combined%component = other%component
            if (allocated(other%context)) combined%context = other%context
            if (allocated(other%suggestion)) combined%suggestion = other%suggestion
        end if
    end function combine_result

    subroutine assign_result(lhs, rhs)
        class(result_t), intent(inout) :: lhs
        type(result_t), intent(in) :: rhs
        
        lhs%success = rhs%success
        lhs%error_code = rhs%error_code
        lhs%severity = rhs%severity
        
        if (allocated(rhs%error_message)) lhs%error_message = rhs%error_message
        if (allocated(rhs%component)) lhs%component = rhs%component
        if (allocated(rhs%context)) lhs%context = rhs%context
        if (allocated(rhs%suggestion)) lhs%suggestion = rhs%suggestion
    end subroutine assign_result

    ! Factory functions for creating results
    function success_result() result(res)
        type(result_t) :: res
        res%success = .true.
        res%severity = ERROR_INFO
    end function success_result

    function create_error_result(message, code, component, context, suggestion) result(res)
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code
        character(len=*), intent(in), optional :: component, context, suggestion
        type(result_t) :: res
        
        call res%set_error(message, code, component, context, suggestion)
    end function create_error_result

    function warning_result(message, code, component, context, suggestion) result(res)
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code
        character(len=*), intent(in), optional :: component, context, suggestion
        type(result_t) :: res
        
        call res%set_warning(message, code, component, context, suggestion)
    end function warning_result

    function critical_result(message, code, component, context, suggestion) result(res)
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code
        character(len=*), intent(in), optional :: component, context, suggestion
        type(result_t) :: res
        
        call res%set_critical(message, code, component, context, suggestion)
    end function critical_result

    ! Error collection methods
    function create_error_collection(initial_capacity) result(collection)
        integer, intent(in), optional :: initial_capacity
        type(error_collection_t) :: collection
        integer :: cap
        
        cap = 16
        if (present(initial_capacity)) cap = max(initial_capacity, 4)
        
        collection%capacity = cap
        allocate(collection%errors(cap))
        collection%count = 0
    end function create_error_collection

    subroutine add_error(this, message, code, severity, component, context, suggestion)
        class(error_collection_t), intent(inout) :: this
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: code, severity
        character(len=*), intent(in), optional :: component, context, suggestion
        
        type(result_t) :: error_result
        integer :: sev
        
        sev = ERROR_ERROR
        if (present(severity)) sev = severity
        
        select case (sev)
        case (ERROR_WARNING)
            error_result = warning_result(message, code, component, context, suggestion)
        case (ERROR_CRITICAL)
            error_result = critical_result(message, code, component, context, suggestion)
        case default
            error_result = create_error_result(message, code, component, context, suggestion)
        end select
        
        call this%add_result(error_result)
    end subroutine add_error

    subroutine add_result(this, result)
        class(error_collection_t), intent(inout) :: this
        type(result_t), intent(in) :: result
        
        type(result_t), allocatable :: temp_errors(:)
        integer :: new_capacity, i
        
        ! Grow array if needed
        if (this%count >= this%capacity) then
            new_capacity = this%capacity * 2
            allocate(temp_errors(new_capacity))
            
            do i = 1, this%count
                temp_errors(i) = this%errors(i)
            end do
            
            call move_alloc(temp_errors, this%errors)
            this%capacity = new_capacity
        end if
        
        this%count = this%count + 1
        this%errors(this%count) = result
    end subroutine add_result

    function has_errors(this) result(has_err)
        class(error_collection_t), intent(in) :: this
        logical :: has_err
        integer :: i
        
        has_err = .false.
        do i = 1, this%count
            if (this%errors(i)%severity >= ERROR_ERROR) then
                has_err = .true.
                return
            end if
        end do
    end function has_errors

    function has_critical_errors(this) result(has_crit)
        class(error_collection_t), intent(in) :: this
        logical :: has_crit
        integer :: i
        
        has_crit = .false.
        do i = 1, this%count
            if (this%errors(i)%severity >= ERROR_CRITICAL) then
                has_crit = .true.
                return
            end if
        end do
    end function has_critical_errors

    function get_error_count(this) result(count)
        class(error_collection_t), intent(in) :: this
        integer :: count
        count = this%count
    end function get_error_count

    function get_worst_severity(this) result(severity)
        class(error_collection_t), intent(in) :: this
        integer :: severity
        integer :: i
        
        severity = ERROR_INFO
        do i = 1, this%count
            severity = max(severity, this%errors(i)%severity)
        end do
    end function get_worst_severity

    subroutine clear_errors(this)
        class(error_collection_t), intent(inout) :: this
        integer :: i
        
        do i = 1, this%count
            call this%errors(i)%clear()
        end do
        this%count = 0
    end subroutine clear_errors

    function get_summary(this) result(summary)
        class(error_collection_t), intent(in) :: this
        character(len=:), allocatable :: summary
        character(len=20) :: count_str
        integer :: warnings, errors, critical, i
        
        warnings = 0
        errors = 0
        critical = 0
        
        do i = 1, this%count
            select case (this%errors(i)%severity)
            case (ERROR_WARNING)
                warnings = warnings + 1
            case (ERROR_ERROR)
                errors = errors + 1
            case (ERROR_CRITICAL)
                critical = critical + 1
            end select
        end do
        
        write(count_str, '(I0)') this%count
        summary = "Total: " // trim(count_str)
        
        if (critical > 0) then
            write(count_str, '(I0)') critical
            summary = summary // ", Critical: " // trim(count_str)
        end if
        
        if (errors > 0) then
            write(count_str, '(I0)') errors
            summary = summary // ", Errors: " // trim(count_str)
        end if
        
        if (warnings > 0) then
            write(count_str, '(I0)') warnings
            summary = summary // ", Warnings: " // trim(count_str)
        end if
    end function get_summary

    subroutine cleanup_collection(this)
        type(error_collection_t), intent(inout) :: this
        
        call this%clear_errors()
        if (allocated(this%errors)) deallocate(this%errors)
    end subroutine cleanup_collection

    ! Utility function for combining multiple results
    function combine_results(results) result(combined)
        type(result_t), intent(in) :: results(:)
        type(result_t) :: combined
        integer :: i
        
        combined = success_result()
        
        do i = 1, size(results)
            combined = combined%combine_result(results(i))
        end do
    end function combine_results

end module error_handling