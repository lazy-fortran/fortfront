module event_system
    ! Event-driven analysis system for Issue #222
    !
    ! This module provides event subscription and dispatch capabilities for
    ! semantic analyzers, including AST traversal with event generation,
    ! event filtering by node type, and multi-subscriber event handling.
    
    use semantic_analyzer, only: semantic_context_t
    use ast_core, only: ast_arena_t
    implicit none
    private
    
    public :: analysis_event_system_t, create_analysis_event_system
    public :: node_event_data_t, event_performance_metrics_t
    public :: event_error_report_t, event_subscriber_t
    
    ! Maximum number of subscribers per event type
    integer, parameter :: MAX_SUBSCRIBERS = 100
    integer, parameter :: MAX_EVENT_TYPES = 50
    integer, parameter :: MAX_NODE_FILTERS = 20
    integer, parameter :: MAX_BATCHED_EVENTS = 1000
    
    ! Event data passed to subscribers
    type :: node_event_data_t
        integer :: node_index = 0
        character(len=32) :: node_type = ""
        character(len=32) :: event_type = ""
        logical :: cancelled = .false.
        character(len=256) :: metadata = ""
        logical :: has_arena = .false.
        logical :: has_context = .false.
    contains
        procedure :: has_node_info => event_data_has_node_info
        procedure :: has_semantic_context => event_data_has_semantic_context
    end type node_event_data_t
    
    ! Abstract event subscriber interface
    type, abstract :: event_subscriber_t
    contains
        procedure(handle_event_interface), deferred :: handle_event
        procedure :: get_subscriber_name => default_get_subscriber_name
    end type event_subscriber_t
    
    ! Interface for event handling
    abstract interface
        subroutine handle_event_interface(this, event_data)
            import :: event_subscriber_t, node_event_data_t
            class(event_subscriber_t), intent(inout) :: this
            type(node_event_data_t), intent(inout) :: event_data
        end subroutine handle_event_interface
    end interface
    
    ! Event subscription entry
    type :: event_subscription_t
        class(event_subscriber_t), allocatable :: subscriber
        character(len=32) :: event_type = ""
        character(len=32) :: node_filters(MAX_NODE_FILTERS)
        integer :: filter_count = 0
        integer :: priority = 1
        logical :: is_active = .false.
    end type event_subscription_t
    
    ! Performance metrics for event system
    type :: event_performance_metrics_t
        integer :: total_events_dispatched = 0
        real :: average_dispatch_time_us = 0.0
        real :: events_per_second = 0.0
        integer :: subscriber_count = 0
        real :: total_dispatch_time_ms = 0.0
    end type event_performance_metrics_t
    
    ! Error reporting for event processing
    type :: event_error_report_t
        character(len=:), allocatable :: errors(:)
        integer :: error_count = 0
        character(len=256) :: last_error_message = ""
        character(len=32) :: last_error_event_type = ""
    end type event_error_report_t
    
    ! Event batching for bulk operations
    type :: event_batch_t
        type(node_event_data_t) :: events(MAX_BATCHED_EVENTS)
        integer :: event_count = 0
        integer :: batch_size_limit = 10
    end type event_batch_t
    
    ! Main event system
    type :: analysis_event_system_t
        type(event_subscription_t) :: subscriptions(MAX_SUBSCRIBERS)
        integer :: num_subscriptions = 0
        character(len=32) :: supported_events(MAX_EVENT_TYPES)
        integer :: supported_event_count = 0
        logical :: initialized = .false.
        logical :: enable_metrics = .false.
        logical :: enable_batching = .false.
        integer :: batch_size = 10
        logical :: continue_on_error = .true.
        type(event_performance_metrics_t) :: metrics
        type(event_error_report_t) :: error_report
        type(event_batch_t) :: current_batch
        character(len=:), allocatable :: execution_log
    contains
        procedure :: subscribe => event_system_subscribe
        procedure :: unsubscribe => event_system_unsubscribe
        procedure :: dispatch_event => event_system_dispatch_event
        procedure :: traverse_with_events => event_system_traverse_with_events
        procedure :: register_event_type => event_system_register_event_type
        procedure :: dispatch_custom_event => event_system_dispatch_custom_event
        procedure :: supports_event_types => event_system_supports_event_types
        procedure :: subscription_count => event_system_subscription_count
        procedure :: has_subscriber => event_system_has_subscriber
        procedure :: get_performance_metrics => event_system_get_metrics
        procedure :: get_error_report => event_system_get_error_report
        procedure :: get_execution_order => event_system_get_execution_order
        procedure :: cleanup => event_system_cleanup
        procedure :: is_clean => event_system_is_clean
        procedure :: is_initialized => event_system_is_initialized
    end type analysis_event_system_t
    
contains
    
    ! Create a new analysis event system
    function create_analysis_event_system(enable_metrics, enable_batching, &
                                         batch_size) result(event_system)
        logical, optional, intent(in) :: enable_metrics, enable_batching
        integer, optional, intent(in) :: batch_size
        type(analysis_event_system_t) :: event_system
        integer :: i
        
        ! Initialize event system
        event_system%num_subscriptions = 0
        event_system%supported_event_count = 0
        event_system%initialized = .true.
        
        ! Configure options
        if (present(enable_metrics)) then
            event_system%enable_metrics = enable_metrics
        else
            event_system%enable_metrics = .false.
        end if
        
        if (present(enable_batching)) then
            event_system%enable_batching = enable_batching
        else
            event_system%enable_batching = .false.
        end if
        
        if (present(batch_size)) then
            event_system%batch_size = batch_size
            event_system%current_batch%batch_size_limit = batch_size
        else
            event_system%batch_size = 10
            event_system%current_batch%batch_size_limit = 10
        end if
        
        event_system%continue_on_error = .true.
        
        ! Initialize supported events
        event_system%supported_events(1) = "on_enter_node"
        event_system%supported_events(2) = "on_exit_node"
        event_system%supported_event_count = 2
        
        ! Initialize subscriptions as inactive
        do i = 1, MAX_SUBSCRIBERS
            event_system%subscriptions(i)%is_active = .false.
            event_system%subscriptions(i)%filter_count = 0
            event_system%subscriptions(i)%priority = 1
        end do
        
        ! Initialize metrics
        event_system%metrics%total_events_dispatched = 0
        event_system%metrics%average_dispatch_time_us = 0.0
        event_system%metrics%events_per_second = 0.0
        event_system%metrics%subscriber_count = 0
        event_system%metrics%total_dispatch_time_ms = 0.0
        
        ! Initialize error reporting
        event_system%error_report%error_count = 0
        event_system%error_report%last_error_message = ""
        event_system%error_report%last_error_event_type = ""
        
        ! Initialize execution log
        event_system%execution_log = ""
    end function create_analysis_event_system
    
    ! Subscribe to events
    function event_system_subscribe(this, subscriber, event_type, &
                                   node_filter, priority) result(success)
        class(analysis_event_system_t), intent(inout) :: this
        class(event_subscriber_t), intent(in) :: subscriber
        character(len=*), intent(in) :: event_type
        character(len=*), optional, intent(in) :: node_filter(:)
        integer, optional, intent(in) :: priority
        logical :: success
        integer :: sub_index, i
        
        success = .false.
        
        ! Check if we have room for another subscription
        if (this%num_subscriptions >= MAX_SUBSCRIBERS) then
            return
        end if
        
        ! Find next available slot
        sub_index = 0
        do i = 1, MAX_SUBSCRIBERS
            if (.not. this%subscriptions(i)%is_active) then
                sub_index = i
                exit
            end if
        end do
        
        if (sub_index == 0) then
            return
        end if
        
        ! Add the subscription
        allocate(this%subscriptions(sub_index)%subscriber, source=subscriber)
        this%subscriptions(sub_index)%event_type = trim(event_type)
        this%subscriptions(sub_index)%is_active = .true.
        
        ! Set priority
        if (present(priority)) then
            this%subscriptions(sub_index)%priority = priority
        else
            this%subscriptions(sub_index)%priority = 1
        end if
        
        ! Add node filters if provided
        if (present(node_filter)) then
            this%subscriptions(sub_index)%filter_count = &
                min(size(node_filter), MAX_NODE_FILTERS)
            do i = 1, this%subscriptions(sub_index)%filter_count
                this%subscriptions(sub_index)%node_filters(i) = &
                    trim(node_filter(i))
            end do
        end if
        
        this%num_subscriptions = this%num_subscriptions + 1
        success = .true.
    end function event_system_subscribe
    
    ! Unsubscribe from events
    function event_system_unsubscribe(this, subscriber, event_type) &
        result(success)
        class(analysis_event_system_t), intent(inout) :: this
        class(event_subscriber_t), intent(in) :: subscriber
        character(len=*), intent(in) :: event_type
        logical :: success
        integer :: i
        character(len=:), allocatable :: sub_name
        
        success = .false.
        sub_name = subscriber%get_subscriber_name()
        
        do i = 1, MAX_SUBSCRIBERS
            if (this%subscriptions(i)%is_active .and. &
                trim(this%subscriptions(i)%event_type) == trim(event_type)) then
                
                ! Check if this is the same subscriber (by name comparison)
                if (allocated(this%subscriptions(i)%subscriber)) then
                    if (this%subscriptions(i)%subscriber%get_subscriber_name() == &
                        sub_name) then
                        
                        ! Remove subscription
                        deallocate(this%subscriptions(i)%subscriber)
                        this%subscriptions(i)%is_active = .false.
                        this%subscriptions(i)%event_type = ""
                        this%subscriptions(i)%filter_count = 0
                        this%num_subscriptions = this%num_subscriptions - 1
                        success = .true.
                        return
                    end if
                end if
            end if
        end do
    end function event_system_unsubscribe
    
    ! Dispatch a single event
    subroutine event_system_dispatch_event(this, event_data)
        class(analysis_event_system_t), intent(inout) :: this
        type(node_event_data_t), intent(inout) :: event_data
        integer :: i, j
        logical :: matches_filter, error_occurred
        character(len=256) :: error_msg
        real :: start_time, end_time, dispatch_time
        
        if (.not. this%initialized) return
        
        if (this%enable_metrics) then
            call cpu_time(start_time)
        end if
        
        ! Sort subscriptions by priority (higher priority first)
        call sort_subscriptions_by_priority(this)
        
        ! Dispatch to all matching subscribers
        do i = 1, MAX_SUBSCRIBERS
            if (.not. this%subscriptions(i)%is_active) cycle
            if (trim(this%subscriptions(i)%event_type) /= &
                trim(event_data%event_type)) cycle
            
            ! Check if event matches node filters
            matches_filter = .true.
            if (this%subscriptions(i)%filter_count > 0) then
                matches_filter = .false.
                do j = 1, this%subscriptions(i)%filter_count
                    if (trim(this%subscriptions(i)%node_filters(j)) == &
                        trim(event_data%node_type)) then
                        matches_filter = .true.
                        exit
                    end if
                end do
            end if
            
            if (.not. matches_filter) cycle
            
            ! Dispatch event to subscriber
            error_occurred = .false.
            if (allocated(this%subscriptions(i)%subscriber)) then
                
                ! Add to execution log
                if (allocated(this%execution_log)) then
                    this%execution_log = this%execution_log // &
                        this%subscriptions(i)%subscriber%get_subscriber_name() // &
                        ","
                else
                    this%execution_log = &
                        this%subscriptions(i)%subscriber%get_subscriber_name() // &
                        ","
                end if
                
                ! Handle event with error protection
                if (this%continue_on_error) then
                    call handle_event_with_error_protection(&
                        this%subscriptions(i)%subscriber, event_data, &
                        error_occurred, error_msg)
                    
                    if (error_occurred) then
                        call record_event_error(this, error_msg, &
                                              event_data%event_type)
                    end if
                else
                    call this%subscriptions(i)%subscriber%handle_event(event_data)
                end if
            end if
            
            ! Check if event was cancelled
            if (event_data%cancelled) then
                exit
            end if
        end do
        
        if (this%enable_metrics) then
            call cpu_time(end_time)
            dispatch_time = (end_time - start_time) * 1000.0  ! Convert to ms
            this%metrics%total_events_dispatched = &
                this%metrics%total_events_dispatched + 1
            this%metrics%total_dispatch_time_ms = &
                this%metrics%total_dispatch_time_ms + dispatch_time
            this%metrics%average_dispatch_time_us = &
                (this%metrics%total_dispatch_time_ms / &
                 real(this%metrics%total_events_dispatched)) * 1000.0
        end if
    end subroutine event_system_dispatch_event
    
    ! Traverse AST with event generation
    subroutine event_system_traverse_with_events(this, arena, root_index, &
                                                context)
        class(analysis_event_system_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        type(semantic_context_t), optional, intent(inout) :: context
        
        call traverse_node_recursive(this, arena, root_index, context)
        
        ! Flush any remaining batched events
        if (this%enable_batching .and. this%current_batch%event_count > 0) then
            call flush_event_batch(this)
        end if
        
        ! Update final metrics
        if (this%enable_metrics) then
            this%metrics%subscriber_count = this%num_subscriptions
            if (this%metrics%total_dispatch_time_ms > 0.0) then
                this%metrics%events_per_second = &
                    real(this%metrics%total_events_dispatched) / &
                    (this%metrics%total_dispatch_time_ms / 1000.0)
            end if
        end if
    end subroutine event_system_traverse_with_events
    
    ! Register custom event type
    function event_system_register_event_type(this, event_type, node_filter) &
        result(success)
        class(analysis_event_system_t), intent(inout) :: this
        character(len=*), intent(in) :: event_type
        character(len=*), optional, intent(in) :: node_filter(:)
        logical :: success
        
        success = .false.
        
        ! Check if we have room for another event type
        if (this%supported_event_count >= MAX_EVENT_TYPES) then
            return
        end if
        
        ! Add the event type
        this%supported_event_count = this%supported_event_count + 1
        this%supported_events(this%supported_event_count) = trim(event_type)
        success = .true.
    end function event_system_register_event_type
    
    ! Dispatch custom event
    function event_system_dispatch_custom_event(this, event_type, arena, &
                                               node_index) result(success)
        class(analysis_event_system_t), intent(inout) :: this
        character(len=*), intent(in) :: event_type
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        logical :: success
        type(node_event_data_t) :: event_data
        
        success = .false.
        
        ! Create event data
        event_data%node_index = node_index
        event_data%event_type = trim(event_type)
        event_data%cancelled = .false.
        event_data%has_arena = .true.
        
        ! Get node type from arena
        call get_node_type_from_arena(arena, node_index, event_data%node_type)
        
        ! Dispatch the event
        call this%dispatch_event(event_data)
        success = .true.
    end function event_system_dispatch_custom_event
    
    ! Check if event types are supported
    function event_system_supports_event_types(this, event_types) &
        result(supported)
        class(analysis_event_system_t), intent(in) :: this
        character(len=*), intent(in) :: event_types(:)
        logical :: supported
        integer :: i, j
        logical :: found
        
        supported = .true.
        do i = 1, size(event_types)
            found = .false.
            do j = 1, this%supported_event_count
                if (trim(this%supported_events(j)) == &
                    trim(event_types(i))) then
                    found = .true.
                    exit
                end if
            end do
            if (.not. found) then
                supported = .false.
                return
            end if
        end do
    end function event_system_supports_event_types
    
    ! Get subscription count
    function event_system_subscription_count(this) result(count)
        class(analysis_event_system_t), intent(in) :: this
        integer :: count
        
        count = this%num_subscriptions
    end function event_system_subscription_count
    
    ! Check if has subscriber
    function event_system_has_subscriber(this, subscriber, event_type) &
        result(has_sub)
        class(analysis_event_system_t), intent(in) :: this
        class(event_subscriber_t), intent(in) :: subscriber
        character(len=*), intent(in) :: event_type
        logical :: has_sub
        integer :: i
        character(len=:), allocatable :: sub_name
        
        has_sub = .false.
        sub_name = subscriber%get_subscriber_name()
        
        do i = 1, MAX_SUBSCRIBERS
            if (this%subscriptions(i)%is_active .and. &
                trim(this%subscriptions(i)%event_type) == trim(event_type)) then
                
                if (allocated(this%subscriptions(i)%subscriber)) then
                    if (this%subscriptions(i)%subscriber%get_subscriber_name() == &
                        sub_name) then
                        has_sub = .true.
                        return
                    end if
                end if
            end if
        end do
    end function event_system_has_subscriber
    
    ! Get performance metrics
    function event_system_get_metrics(this) result(metrics)
        class(analysis_event_system_t), intent(in) :: this
        type(event_performance_metrics_t) :: metrics
        
        metrics = this%metrics
    end function event_system_get_metrics
    
    ! Get error report
    function event_system_get_error_report(this) result(error_report)
        class(analysis_event_system_t), intent(in) :: this
        type(event_error_report_t) :: error_report
        
        error_report = this%error_report
    end function event_system_get_error_report
    
    ! Get execution order
    function event_system_get_execution_order(this) result(order)
        class(analysis_event_system_t), intent(in) :: this
        character(len=:), allocatable :: order
        
        if (allocated(this%execution_log)) then
            order = this%execution_log
        else
            order = ""
        end if
    end function event_system_get_execution_order
    
    ! Cleanup event system
    subroutine event_system_cleanup(this)
        class(analysis_event_system_t), intent(inout) :: this
        integer :: i
        
        ! Deallocate all subscribers
        do i = 1, MAX_SUBSCRIBERS
            if (this%subscriptions(i)%is_active) then
                if (allocated(this%subscriptions(i)%subscriber)) then
                    deallocate(this%subscriptions(i)%subscriber)
                end if
                this%subscriptions(i)%is_active = .false.
                this%subscriptions(i)%event_type = ""
                this%subscriptions(i)%filter_count = 0
            end if
        end do
        
        this%num_subscriptions = 0
        this%supported_event_count = 0
        this%initialized = .false.
        
        if (allocated(this%execution_log)) then
            deallocate(this%execution_log)
        end if
    end subroutine event_system_cleanup
    
    ! Check if clean
    function event_system_is_clean(this) result(is_clean)
        class(analysis_event_system_t), intent(in) :: this
        logical :: is_clean
        
        is_clean = (.not. this%initialized) .and. &
                   (this%num_subscriptions == 0)
    end function event_system_is_clean
    
    ! Check if initialized
    function event_system_is_initialized(this) result(initialized)
        class(analysis_event_system_t), intent(in) :: this
        logical :: initialized
        
        initialized = this%initialized
    end function event_system_is_initialized
    
    ! Event data methods
    function event_data_has_node_info(this) result(has_info)
        class(node_event_data_t), intent(in) :: this
        logical :: has_info
        
        has_info = this%has_arena .and. (this%node_index > 0)
    end function event_data_has_node_info
    
    function event_data_has_semantic_context(this) result(has_context)
        class(node_event_data_t), intent(in) :: this
        logical :: has_context
        
        has_context = this%has_context
    end function event_data_has_semantic_context
    
    ! Default subscriber name
    function default_get_subscriber_name(this) result(name)
        class(event_subscriber_t), intent(in) :: this
        character(len=:), allocatable :: name
        
        name = "unknown_subscriber"
    end function default_get_subscriber_name
    
    ! Helper subroutines
    
    ! Recursive AST traversal with event generation
    recursive subroutine traverse_node_recursive(event_system, arena, &
                                                node_index, context)
        type(analysis_event_system_t), intent(inout) :: event_system
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(semantic_context_t), optional, intent(inout) :: context
        type(node_event_data_t) :: enter_event, exit_event
        
        ! Create enter event
        enter_event%node_index = node_index
        enter_event%event_type = "on_enter_node"
        enter_event%cancelled = .false.
        enter_event%has_arena = .true.
        if (present(context)) then
            enter_event%has_context = .true.
        end if
        
        ! Get node type
        call get_node_type_from_arena(arena, node_index, enter_event%node_type)
        
        ! Dispatch enter event
        if (event_system%enable_batching) then
            call add_event_to_batch(event_system, enter_event)
        else
            call event_system%dispatch_event(enter_event)
        end if
        
        ! Skip children if event was cancelled
        if (.not. enter_event%cancelled) then
            ! Traverse children (simplified - actual implementation would
            ! depend on AST structure)
            call traverse_children(event_system, arena, node_index, context)
        end if
        
        ! Create exit event
        exit_event = enter_event
        exit_event%event_type = "on_exit_node"
        exit_event%cancelled = .false.
        
        ! Dispatch exit event
        if (event_system%enable_batching) then
            call add_event_to_batch(event_system, exit_event)
        else
            call event_system%dispatch_event(exit_event)
        end if
    end subroutine traverse_node_recursive
    
    ! Traverse children nodes
    subroutine traverse_children(event_system, arena, node_index, context)
        type(analysis_event_system_t), intent(inout) :: event_system
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(semantic_context_t), optional, intent(inout) :: context
        
        ! Simplified implementation - in practice would examine node structure
        ! and recursively traverse child nodes based on node type
        ! For now, we'll just handle a few simple cases
        
        ! This is a simplified stub - actual implementation would depend
        ! on the specific AST structure and node types
    end subroutine traverse_children
    
    ! Get node type from arena
    subroutine get_node_type_from_arena(arena, node_index, node_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=32), intent(out) :: node_type
        
        ! Simplified implementation - would examine arena structure
        ! to determine actual node type
        if (node_index <= 0 .or. node_index > arena%current_index) then
            node_type = "unknown"
        else
            ! This is simplified - actual implementation would examine
            ! the node structure in the arena
            node_type = "ast_node"
        end if
    end subroutine get_node_type_from_arena
    
    ! Sort subscriptions by priority
    subroutine sort_subscriptions_by_priority(event_system)
        type(analysis_event_system_t), intent(inout) :: event_system
        integer :: i, j
        type(event_subscription_t) :: temp
        
        ! Simple bubble sort by priority (higher priority first)
        do i = 1, MAX_SUBSCRIBERS - 1
            do j = i + 1, MAX_SUBSCRIBERS
                if (event_system%subscriptions(i)%is_active .and. &
                    event_system%subscriptions(j)%is_active .and. &
                    event_system%subscriptions(j)%priority > &
                    event_system%subscriptions(i)%priority) then
                    
                    ! Swap subscriptions
                    temp = event_system%subscriptions(i)
                    event_system%subscriptions(i) = &
                        event_system%subscriptions(j)
                    event_system%subscriptions(j) = temp
                end if
            end do
        end do
    end subroutine sort_subscriptions_by_priority
    
    ! Handle event with error protection
    subroutine handle_event_with_error_protection(subscriber, event_data, &
                                                 error_occurred, error_msg)
        class(event_subscriber_t), intent(inout) :: subscriber
        type(node_event_data_t), intent(inout) :: event_data
        logical, intent(out) :: error_occurred
        character(len=256), intent(out) :: error_msg
        
        error_occurred = .false.
        error_msg = ""
        
        ! In a real implementation, this would use proper exception handling
        ! For now, we'll just call the handler directly
        call subscriber%handle_event(event_data)
    end subroutine handle_event_with_error_protection
    
    ! Record event error
    subroutine record_event_error(event_system, error_msg, event_type)
        type(analysis_event_system_t), intent(inout) :: event_system
        character(len=*), intent(in) :: error_msg
        character(len=*), intent(in) :: event_type
        
        event_system%error_report%error_count = &
            event_system%error_report%error_count + 1
        event_system%error_report%last_error_message = trim(error_msg)
        event_system%error_report%last_error_event_type = trim(event_type)
    end subroutine record_event_error
    
    ! Add event to batch
    subroutine add_event_to_batch(event_system, event_data)
        type(analysis_event_system_t), intent(inout) :: event_system
        type(node_event_data_t), intent(in) :: event_data
        
        if (event_system%current_batch%event_count < MAX_BATCHED_EVENTS) then
            event_system%current_batch%event_count = &
                event_system%current_batch%event_count + 1
            event_system%current_batch%events(&
                event_system%current_batch%event_count) = event_data
            
            ! Check if batch is full
            if (event_system%current_batch%event_count >= &
                event_system%current_batch%batch_size_limit) then
                call flush_event_batch(event_system)
            end if
        end if
    end subroutine add_event_to_batch
    
    ! Flush event batch
    subroutine flush_event_batch(event_system)
        type(analysis_event_system_t), intent(inout) :: event_system
        integer :: i
        
        do i = 1, event_system%current_batch%event_count
            call event_system%dispatch_event(&
                event_system%current_batch%events(i))
        end do
        
        event_system%current_batch%event_count = 0
    end subroutine flush_event_batch
    
end module event_system