module analysis_event_data
    ! Concrete event data types to replace class(*) in analysis events
    use ast_core, only: ast_arena_t
    use semantic_context_types, only: semantic_context_base_t
    implicit none
    private

    ! Base event data type
    type, abstract, public :: event_data_base_t
        integer :: data_type = 0
        character(:), allocatable :: data_name
    contains
        procedure(get_data_type_interface), deferred :: get_data_type
        procedure(clone_data_interface), deferred :: clone_data
    end type event_data_base_t

    ! Node entry event data
    type, extends(event_data_base_t), public :: node_entry_data_t
        integer :: node_index = 0
        integer :: parent_index = 0
        integer :: depth_level = 0
    contains
        procedure :: get_data_type => node_entry_get_data_type
        procedure :: clone_data => node_entry_clone_data
        procedure :: assign => node_entry_assign
        generic :: assignment(=) => assign
    end type node_entry_data_t

    ! Node exit event data
    type, extends(event_data_base_t), public :: node_exit_data_t
        integer :: node_index = 0
        integer :: processing_time_ms = 0
        logical :: processing_successful = .true.
    contains
        procedure :: get_data_type => node_exit_get_data_type
        procedure :: clone_data => node_exit_clone_data
        procedure :: assign => node_exit_assign
        generic :: assignment(=) => assign
    end type node_exit_data_t

    ! Scope entry event data  
    type, extends(event_data_base_t), public :: scope_entry_data_t
        character(:), allocatable :: scope_name
        integer :: scope_level = 0
        integer :: parent_scope_index = 0
    contains
        procedure :: get_data_type => scope_entry_get_data_type
        procedure :: clone_data => scope_entry_clone_data
        procedure :: assign => scope_entry_assign
        generic :: assignment(=) => assign
    end type scope_entry_data_t

    ! Scope exit event data
    type, extends(event_data_base_t), public :: scope_exit_data_t
        character(:), allocatable :: scope_name
        integer :: symbols_processed = 0
        integer :: resolution_errors = 0
    contains
        procedure :: get_data_type => scope_exit_get_data_type
        procedure :: clone_data => scope_exit_clone_data
        procedure :: assign => scope_exit_assign
        generic :: assignment(=) => assign
    end type scope_exit_data_t

    ! Type inference event data
    type, extends(event_data_base_t), public :: type_inference_data_t
        integer :: node_index = 0
        logical :: inference_successful = .false.
        character(:), allocatable :: inferred_type_name
        character(:), allocatable :: error_message
        integer :: confidence_level = 0
    contains
        procedure :: get_data_type => type_inference_get_data_type
        procedure :: clone_data => type_inference_clone_data
        procedure :: assign => type_inference_assign
        generic :: assignment(=) => assign
    end type type_inference_data_t

    ! Analysis complete event data
    type, extends(event_data_base_t), public :: analysis_complete_data_t
        character(:), allocatable :: analyzer_name
        integer :: nodes_processed = 0
        integer :: errors_found = 0
        integer :: warnings_found = 0
        integer :: processing_time_ms = 0
    contains
        procedure :: get_data_type => analysis_complete_get_data_type
        procedure :: clone_data => analysis_complete_clone_data
        procedure :: assign => analysis_complete_assign
        generic :: assignment(=) => assign
    end type analysis_complete_data_t

    ! Error detected event data
    type, extends(event_data_base_t), public :: error_detected_data_t
        integer :: node_index = 0
        character(:), allocatable :: error_message
        character(:), allocatable :: error_code
        integer :: severity_level = 0  ! 1=info, 2=warning, 3=error, 4=critical
    contains
        procedure :: get_data_type => error_detected_get_data_type
        procedure :: clone_data => error_detected_clone_data
        procedure :: assign => error_detected_assign
        generic :: assignment(=) => assign
    end type error_detected_data_t

    ! Abstract interfaces
    abstract interface
        function get_data_type_interface(this) result(data_type)
            import :: event_data_base_t
            class(event_data_base_t), intent(in) :: this
            character(:), allocatable :: data_type
        end function get_data_type_interface

        function clone_data_interface(this) result(cloned)
            import :: event_data_base_t
            class(event_data_base_t), intent(in) :: this
            class(event_data_base_t), allocatable :: cloned
        end function clone_data_interface
    end interface

    ! Public interface
    public :: event_data_base_t, node_entry_data_t, node_exit_data_t, &
              scope_entry_data_t, scope_exit_data_t, type_inference_data_t, &
              analysis_complete_data_t, error_detected_data_t

contains

    ! Node entry implementations
    function node_entry_get_data_type(this) result(data_type)
        class(node_entry_data_t), intent(in) :: this
        character(:), allocatable :: data_type
        data_type = "node_entry"
    end function node_entry_get_data_type

    function node_entry_clone_data(this) result(cloned)
        class(node_entry_data_t), intent(in) :: this
        class(event_data_base_t), allocatable :: cloned
        type(node_entry_data_t) :: temp_data
        
        temp_data = this
        allocate(cloned, source=temp_data)
    end function node_entry_clone_data

    subroutine node_entry_assign(lhs, rhs)
        class(node_entry_data_t), intent(out) :: lhs
        type(node_entry_data_t), intent(in) :: rhs
        
        lhs%data_type = rhs%data_type
        lhs%data_name = rhs%data_name
        lhs%node_index = rhs%node_index
        lhs%parent_index = rhs%parent_index
        lhs%depth_level = rhs%depth_level
    end subroutine node_entry_assign

    ! Node exit implementations
    function node_exit_get_data_type(this) result(data_type)
        class(node_exit_data_t), intent(in) :: this
        character(:), allocatable :: data_type
        data_type = "node_exit"
    end function node_exit_get_data_type

    function node_exit_clone_data(this) result(cloned)
        class(node_exit_data_t), intent(in) :: this
        class(event_data_base_t), allocatable :: cloned
        type(node_exit_data_t) :: temp_data
        
        temp_data = this
        allocate(cloned, source=temp_data)
    end function node_exit_clone_data

    subroutine node_exit_assign(lhs, rhs)
        class(node_exit_data_t), intent(out) :: lhs
        type(node_exit_data_t), intent(in) :: rhs
        
        lhs%data_type = rhs%data_type
        lhs%data_name = rhs%data_name
        lhs%node_index = rhs%node_index
        lhs%processing_time_ms = rhs%processing_time_ms
        lhs%processing_successful = rhs%processing_successful
    end subroutine node_exit_assign

    ! Scope entry implementations
    function scope_entry_get_data_type(this) result(data_type)
        class(scope_entry_data_t), intent(in) :: this
        character(:), allocatable :: data_type
        data_type = "scope_entry"
    end function scope_entry_get_data_type

    function scope_entry_clone_data(this) result(cloned)
        class(scope_entry_data_t), intent(in) :: this
        class(event_data_base_t), allocatable :: cloned
        type(scope_entry_data_t) :: temp_data
        
        temp_data = this
        allocate(cloned, source=temp_data)
    end function scope_entry_clone_data

    subroutine scope_entry_assign(lhs, rhs)
        class(scope_entry_data_t), intent(out) :: lhs
        type(scope_entry_data_t), intent(in) :: rhs
        
        lhs%data_type = rhs%data_type
        lhs%data_name = rhs%data_name
        lhs%scope_name = rhs%scope_name
        lhs%scope_level = rhs%scope_level
        lhs%parent_scope_index = rhs%parent_scope_index
    end subroutine scope_entry_assign

    ! Scope exit implementations
    function scope_exit_get_data_type(this) result(data_type)
        class(scope_exit_data_t), intent(in) :: this
        character(:), allocatable :: data_type
        data_type = "scope_exit"
    end function scope_exit_get_data_type

    function scope_exit_clone_data(this) result(cloned)
        class(scope_exit_data_t), intent(in) :: this
        class(event_data_base_t), allocatable :: cloned
        type(scope_exit_data_t) :: temp_data
        
        temp_data = this
        allocate(cloned, source=temp_data)
    end function scope_exit_clone_data

    subroutine scope_exit_assign(lhs, rhs)
        class(scope_exit_data_t), intent(out) :: lhs
        type(scope_exit_data_t), intent(in) :: rhs
        
        lhs%data_type = rhs%data_type
        lhs%data_name = rhs%data_name
        lhs%scope_name = rhs%scope_name
        lhs%symbols_processed = rhs%symbols_processed
        lhs%resolution_errors = rhs%resolution_errors
    end subroutine scope_exit_assign

    ! Type inference implementations
    function type_inference_get_data_type(this) result(data_type)
        class(type_inference_data_t), intent(in) :: this
        character(:), allocatable :: data_type
        data_type = "type_inference"
    end function type_inference_get_data_type

    function type_inference_clone_data(this) result(cloned)
        class(type_inference_data_t), intent(in) :: this
        class(event_data_base_t), allocatable :: cloned
        type(type_inference_data_t) :: temp_data
        
        temp_data = this
        allocate(cloned, source=temp_data)
    end function type_inference_clone_data

    subroutine type_inference_assign(lhs, rhs)
        class(type_inference_data_t), intent(out) :: lhs
        type(type_inference_data_t), intent(in) :: rhs
        
        lhs%data_type = rhs%data_type
        lhs%data_name = rhs%data_name
        lhs%node_index = rhs%node_index
        lhs%inference_successful = rhs%inference_successful
        lhs%inferred_type_name = rhs%inferred_type_name
        lhs%error_message = rhs%error_message
        lhs%confidence_level = rhs%confidence_level
    end subroutine type_inference_assign

    ! Analysis complete implementations
    function analysis_complete_get_data_type(this) result(data_type)
        class(analysis_complete_data_t), intent(in) :: this
        character(:), allocatable :: data_type
        data_type = "analysis_complete"
    end function analysis_complete_get_data_type

    function analysis_complete_clone_data(this) result(cloned)
        class(analysis_complete_data_t), intent(in) :: this
        class(event_data_base_t), allocatable :: cloned
        type(analysis_complete_data_t) :: temp_data
        
        temp_data = this
        allocate(cloned, source=temp_data)
    end function analysis_complete_clone_data

    subroutine analysis_complete_assign(lhs, rhs)
        class(analysis_complete_data_t), intent(out) :: lhs
        type(analysis_complete_data_t), intent(in) :: rhs
        
        lhs%data_type = rhs%data_type
        lhs%data_name = rhs%data_name
        lhs%analyzer_name = rhs%analyzer_name
        lhs%nodes_processed = rhs%nodes_processed
        lhs%errors_found = rhs%errors_found
        lhs%warnings_found = rhs%warnings_found
        lhs%processing_time_ms = rhs%processing_time_ms
    end subroutine analysis_complete_assign

    ! Error detected implementations
    function error_detected_get_data_type(this) result(data_type)
        class(error_detected_data_t), intent(in) :: this
        character(:), allocatable :: data_type
        data_type = "error_detected"
    end function error_detected_get_data_type

    function error_detected_clone_data(this) result(cloned)
        class(error_detected_data_t), intent(in) :: this
        class(event_data_base_t), allocatable :: cloned
        type(error_detected_data_t) :: temp_data
        
        temp_data = this
        allocate(cloned, source=temp_data)
    end function error_detected_clone_data

    subroutine error_detected_assign(lhs, rhs)
        class(error_detected_data_t), intent(out) :: lhs
        type(error_detected_data_t), intent(in) :: rhs
        
        lhs%data_type = rhs%data_type
        lhs%data_name = rhs%data_name
        lhs%node_index = rhs%node_index
        lhs%error_message = rhs%error_message
        lhs%error_code = rhs%error_code
        lhs%severity_level = rhs%severity_level
    end subroutine error_detected_assign

end module analysis_event_data