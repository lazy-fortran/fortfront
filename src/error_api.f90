module error_api
    use error_reporting, only: &
        error_record_t, &
        error_collection_t, &
        error_context_t, &
        create_error_context, &
        create_error_context_from_token, &
        format_error_message, &
        ERROR_INFO, &
        ERROR_WARNING, &
        ERROR_ERROR, &
        ERROR_FATAL
    implicit none

    public :: error_record_t
    public :: error_collection_t
    public :: error_context_t
    public :: create_error_context
    public :: create_error_context_from_token
    public :: format_error_message
    public :: ERROR_INFO
    public :: ERROR_WARNING
    public :: ERROR_ERROR
    public :: ERROR_FATAL

end module error_api
