module semantic_events
    !! Core event system for semantic analysis plugin architecture
    !! Provides event type definitions, data structures, and validation
    implicit none
    private
    
    ! Event type constants
    integer, parameter, public :: EVENT_NODE_ENTER = 1
    integer, parameter, public :: EVENT_NODE_EXIT = 2
    integer, parameter, public :: EVENT_SCOPE_ENTER = 3
    integer, parameter, public :: EVENT_SCOPE_EXIT = 4
    integer, parameter, public :: EVENT_TYPE_INFERRED = 5
    integer, parameter, public :: EVENT_ANALYSIS_COMPLETE = 6
    integer, parameter, public :: EVENT_ERROR_DETECTED = 7
    integer, parameter, public :: EVENT_BUILTIN_REQUIRED = 8
    
    ! Event data structures
    type, public :: analysis_event_t
        integer :: event_type = 0
        integer :: node_index = 0
        integer :: source_analyzer_id = 0
        class(*), allocatable :: event_data
        logical :: consumed = .false.
        logical :: propagate = .true.
    end type
    
    type, public :: event_subscription_t
        integer :: event_type = 0
        integer :: analyzer_id = 0
        real :: priority = 1.0
        logical :: enabled = .true.
    end type
    
    ! Event data payload types
    type, public :: type_inference_event_data_t
        integer :: node_index = 0
        logical :: inference_successful = .false.
        character(len=256) :: error_message = ""
        integer :: confidence_level = 0
    end type
    
    type, public :: scope_event_data_t
        character(len=64) :: scope_name = ""
        integer :: scope_level = 0
        logical :: entering_scope = .true.
        integer :: parent_scope_index = 0
    end type
    
    ! Event handler interface
    abstract interface
        subroutine event_handler_interface(event, context, arena)
            import :: analysis_event_t
            type(analysis_event_t), intent(inout) :: event
            class(*), intent(inout) :: context
            class(*), intent(inout) :: arena
        end subroutine
    end interface
    
    ! Public procedures
    public :: validate_event
    public :: serialize_event
    public :: deserialize_event
    public :: create_type_inference_event_data
    public :: create_scope_event_data
    
contains

    subroutine validate_event(event, is_valid, error_message)
        !! Validate event data integrity and consistency
        type(analysis_event_t), intent(in) :: event
        logical, intent(out) :: is_valid
        character(len=*), intent(out) :: error_message
        
        is_valid = .true.
        error_message = ""
        
        ! Validate event type
        if (event%event_type < EVENT_NODE_ENTER .or. &
            event%event_type > EVENT_BUILTIN_REQUIRED) then
            is_valid = .false.
            error_message = "Invalid event type"
            return
        end if
        
        ! Validate node index
        if (event%node_index < 0) then
            is_valid = .false.
            error_message = "Invalid node index"
            return
        end if
        
        ! Validate source analyzer ID
        if (event%source_analyzer_id < 0) then
            is_valid = .false.
            error_message = "Invalid source analyzer ID"
            return
        end if
    end subroutine
    
    subroutine serialize_event(event, serialized_data, success)
        !! Serialize event to string format for debugging and persistence
        type(analysis_event_t), intent(in) :: event
        character(len=*), intent(out) :: serialized_data
        logical, intent(out) :: success
        
        character(len=32) :: temp_string
        
        success = .true.
        serialized_data = ""
        
        ! Build serialized representation
        write(temp_string, '(I0)') event%event_type
        serialized_data = "type:" // trim(temp_string)
        
        write(temp_string, '(I0)') event%node_index
        serialized_data = trim(serialized_data) // ";node:" // trim(temp_string)
        
        write(temp_string, '(I0)') event%source_analyzer_id
        serialized_data = trim(serialized_data) // ";analyzer:" // trim(temp_string)
        
        if (event%consumed) then
            serialized_data = trim(serialized_data) // ";consumed:true"
        else
            serialized_data = trim(serialized_data) // ";consumed:false"
        end if
        
        if (event%propagate) then
            serialized_data = trim(serialized_data) // ";propagate:true"
        else
            serialized_data = trim(serialized_data) // ";propagate:false"
        end if
    end subroutine
    
    subroutine deserialize_event(serialized_data, event, success)
        !! Deserialize event from string format
        character(len=*), intent(in) :: serialized_data
        type(analysis_event_t), intent(out) :: event
        logical, intent(out) :: success
        
        integer :: colon_pos, semicolon_pos, start_pos
        character(len=256) :: work_string
        character(len=64) :: field_name, field_value
        
        success = .true.
        work_string = serialized_data
        start_pos = 1
        
        ! Initialize event
        event%event_type = 0
        event%node_index = 0
        event%source_analyzer_id = 0
        event%consumed = .false.
        event%propagate = .true.
        
        ! Parse serialized data
        do while (start_pos <= len_trim(work_string))
            semicolon_pos = index(work_string(start_pos:), ";")
            if (semicolon_pos == 0) then
                ! Last field
                field_name = work_string(start_pos:)
                start_pos = len_trim(work_string) + 1
            else
                ! Extract field
                field_name = work_string(start_pos:start_pos + semicolon_pos - 2)
                start_pos = start_pos + semicolon_pos
            end if
            
            colon_pos = index(field_name, ":")
            if (colon_pos == 0) then
                success = .false.
                return
            end if
            
            field_value = field_name(colon_pos + 1:)
            field_name = field_name(1:colon_pos - 1)
            
            ! Process field
            select case (trim(field_name))
            case ("type")
                read(field_value, *, iostat=semicolon_pos) event%event_type
                if (semicolon_pos /= 0) success = .false.
            case ("node")
                read(field_value, *, iostat=semicolon_pos) event%node_index
                if (semicolon_pos /= 0) success = .false.
            case ("analyzer")
                read(field_value, *, iostat=semicolon_pos) event%source_analyzer_id
                if (semicolon_pos /= 0) success = .false.
            case ("consumed")
                event%consumed = (trim(field_value) == "true")
            case ("propagate")
                event%propagate = (trim(field_value) == "true")
            end select
            
            if (.not. success) return
        end do
    end subroutine
    
    function create_type_inference_event_data(node_index, successful, &
                                            error_msg, confidence) result(data)
        !! Create type inference event data payload
        integer, intent(in) :: node_index
        logical, intent(in) :: successful
        character(len=*), intent(in) :: error_msg
        integer, intent(in) :: confidence
        type(type_inference_event_data_t) :: data
        
        data%node_index = node_index
        data%inference_successful = successful
        data%error_message = error_msg
        data%confidence_level = confidence
    end function
    
    function create_scope_event_data(scope_name, level, entering, &
                                   parent_index) result(data)
        !! Create scope event data payload
        character(len=*), intent(in) :: scope_name
        integer, intent(in) :: level
        logical, intent(in) :: entering
        integer, intent(in) :: parent_index
        type(scope_event_data_t) :: data
        
        data%scope_name = scope_name
        data%scope_level = level
        data%entering_scope = entering
        data%parent_scope_index = parent_index
    end function

end module semantic_events