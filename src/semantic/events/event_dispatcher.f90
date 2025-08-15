module event_dispatcher_module
    !! Central event dispatch system for plugin architecture
    !! Handles event routing, subscription management, and performance optimization
    use semantic_events
    implicit none
    private
    
    ! Maximum number of subscriptions per event type
    integer, parameter :: MAX_SUBSCRIPTIONS = 100
    
    type :: subscription_entry_t
        integer :: subscription_id = 0
        integer :: analyzer_id = 0
        real :: priority = 1.0
        logical :: active = .false.
    end type
    
    type, public :: event_dispatcher_t
        type(subscription_entry_t) :: subscriptions(8, MAX_SUBSCRIPTIONS)
        integer :: subscription_counts(8) = 0
        integer :: next_subscription_id = 1
        logical :: initialized = .false.
        logical :: thread_safe_mode = .false.
    contains
        procedure :: initialize => dispatcher_initialize
        procedure :: cleanup => dispatcher_cleanup
        procedure :: subscribe => dispatcher_subscribe
        procedure :: subscribe_with_priority => dispatcher_subscribe_priority
        procedure :: unsubscribe => dispatcher_unsubscribe
        procedure :: dispatch => dispatcher_dispatch
        procedure :: dispatch_batch => dispatcher_dispatch_batch
        procedure :: emit_and_dispatch => dispatcher_emit_and_dispatch
        procedure :: dispatch_with_error_handling => dispatcher_dispatch_errors
        procedure :: dispatch_batch_performance => dispatcher_batch_performance
        procedure :: dispatch_sequence => dispatcher_sequence
        procedure :: get_subscribers => dispatcher_get_subscribers
    end type
    
contains

    subroutine dispatcher_initialize(this)
        !! Initialize event dispatcher with clean state
        class(event_dispatcher_t), intent(inout) :: this
        
        integer :: i, j
        
        ! Clear all subscriptions
        do i = 1, 8
            this%subscription_counts(i) = 0
            do j = 1, MAX_SUBSCRIPTIONS
                this%subscriptions(i, j)%subscription_id = 0
                this%subscriptions(i, j)%analyzer_id = 0
                this%subscriptions(i, j)%priority = 1.0
                this%subscriptions(i, j)%active = .false.
            end do
        end do
        
        this%next_subscription_id = 1
        this%initialized = .true.
        this%thread_safe_mode = .false.
    end subroutine
    
    subroutine dispatcher_cleanup(this)
        !! Clean up dispatcher resources
        class(event_dispatcher_t), intent(inout) :: this
        
        call this%initialize()  ! Reset to clean state
        this%initialized = .false.
    end subroutine
    
    subroutine dispatcher_subscribe(this, event_type, analyzer_id, &
                                  subscription_id, success)
        !! Subscribe analyzer to event type
        class(event_dispatcher_t), intent(inout) :: this
        integer, intent(in) :: event_type
        integer, intent(in) :: analyzer_id
        integer, intent(out) :: subscription_id
        logical, intent(out) :: success
        
        call this%subscribe_with_priority(event_type, analyzer_id, &
                                        1.0, subscription_id, success)
    end subroutine
    
    subroutine dispatcher_subscribe_priority(this, event_type, analyzer_id, &
                                           priority, subscription_id, &
                                           success)
        !! Subscribe with explicit priority
        class(event_dispatcher_t), intent(inout) :: this
        integer, intent(in) :: event_type
        integer, intent(in) :: analyzer_id
        real, intent(in) :: priority
        integer, intent(out) :: subscription_id
        logical, intent(out) :: success
        
        integer :: slot, i, insert_pos
        
        success = .false.
        subscription_id = -1
        
        ! Validate inputs
        if (.not. this%initialized) return
        if (event_type < EVENT_NODE_ENTER .or. &
            event_type > EVENT_BUILTIN_REQUIRED) return
        if (analyzer_id <= 0) return
        if (this%subscription_counts(event_type) >= MAX_SUBSCRIPTIONS) return
        
        ! Check for duplicate subscription
        do i = 1, this%subscription_counts(event_type)
            if (this%subscriptions(event_type, i)%analyzer_id == analyzer_id .and. &
                this%subscriptions(event_type, i)%active) then
                return  ! Duplicate subscription
            end if
        end do
        
        ! Find insertion position to maintain priority order (high to low)
        insert_pos = this%subscription_counts(event_type) + 1
        do i = 1, this%subscription_counts(event_type)
            if (priority > this%subscriptions(event_type, i)%priority) then
                insert_pos = i
                exit
            end if
        end do
        
        ! Shift existing subscriptions to make room
        do i = this%subscription_counts(event_type), insert_pos, -1
            this%subscriptions(event_type, i + 1) = &
                this%subscriptions(event_type, i)
        end do
        
        ! Insert new subscription
        slot = insert_pos
        this%subscriptions(event_type, slot)%subscription_id = &
            this%next_subscription_id
        this%subscriptions(event_type, slot)%analyzer_id = analyzer_id
        this%subscriptions(event_type, slot)%priority = priority
        this%subscriptions(event_type, slot)%active = .true.
        
        subscription_id = this%next_subscription_id
        this%next_subscription_id = this%next_subscription_id + 1
        this%subscription_counts(event_type) = &
            this%subscription_counts(event_type) + 1
        
        success = .true.
    end subroutine
    
    subroutine dispatcher_unsubscribe(this, subscription_id, success)
        !! Remove subscription by ID
        class(event_dispatcher_t), intent(inout) :: this
        integer, intent(in) :: subscription_id
        logical, intent(out) :: success
        
        integer :: event_type, i, j
        
        success = .false.
        
        if (.not. this%initialized) return
        
        ! Find and remove subscription
        do event_type = 1, 8
            do i = 1, this%subscription_counts(event_type)
                if (this%subscriptions(event_type, i)%subscription_id == &
                    subscription_id .and. &
                    this%subscriptions(event_type, i)%active) then
                    
                    ! Shift remaining subscriptions
                    do j = i, this%subscription_counts(event_type) - 1
                        this%subscriptions(event_type, j) = &
                            this%subscriptions(event_type, j + 1)
                    end do
                    
                    ! Clear last slot
                    this%subscriptions(event_type, &
                        this%subscription_counts(event_type))%subscription_id = 0
                    this%subscriptions(event_type, &
                        this%subscription_counts(event_type))%analyzer_id = 0
                    this%subscriptions(event_type, &
                        this%subscription_counts(event_type))%active = .false.
                    
                    this%subscription_counts(event_type) = &
                        this%subscription_counts(event_type) - 1
                    success = .true.
                    return
                end if
            end do
        end do
    end subroutine
    
    subroutine dispatcher_dispatch(this, event, success)
        !! Dispatch event to all subscribers in priority order
        class(event_dispatcher_t), intent(inout) :: this
        type(analysis_event_t), intent(inout) :: event
        logical, intent(out) :: success
        
        integer :: i
        type(analysis_event_t) :: dummy_context, dummy_arena
        
        success = .false.
        
        if (.not. this%initialized) return
        if (event%event_type < EVENT_NODE_ENTER .or. &
            event%event_type > EVENT_BUILTIN_REQUIRED) return
        
        ! Dispatch to subscribers in priority order
        do i = 1, this%subscription_counts(event%event_type)
            if (.not. this%subscriptions(event%event_type, i)%active) cycle
            if (event%consumed .and. .not. event%propagate) exit
            
            ! For now, just mark that dispatching occurred
            ! Handler calling will be implemented when handler registry is added
        end do
        
        success = .true.
    end subroutine
    
    subroutine dispatcher_dispatch_batch(this, events, success)
        !! Dispatch multiple events efficiently
        class(event_dispatcher_t), intent(inout) :: this
        type(analysis_event_t), intent(inout) :: events(:)
        logical, intent(out) :: success
        
        integer :: i
        logical :: event_success
        
        success = .true.
        
        do i = 1, size(events)
            call this%dispatch(events(i), event_success)
            if (.not. event_success) success = .false.
        end do
    end subroutine
    
    subroutine dispatcher_emit_and_dispatch(this, event, success)
        !! Emit and immediately dispatch event
        class(event_dispatcher_t), intent(inout) :: this
        type(analysis_event_t), intent(inout) :: event
        logical, intent(out) :: success
        
        logical :: is_valid
        character(len=256) :: error_message
        
        ! Validate event before dispatch
        call validate_event(event, is_valid, error_message)
        
        if (.not. is_valid) then
            success = .false.
            return
        end if
        
        call this%dispatch(event, success)
    end subroutine
    
    subroutine dispatcher_dispatch_errors(this, event, success, error_message)
        !! Dispatch with comprehensive error handling
        class(event_dispatcher_t), intent(inout) :: this
        type(analysis_event_t), intent(inout) :: event
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        logical :: is_valid
        
        success = .false.
        error_message = ""
        
        ! Validate event
        call validate_event(event, is_valid, error_message)
        
        if (.not. is_valid) return
        
        ! Attempt dispatch with error handling
        call this%dispatch(event, success)
        
        if (.not. success) then
            error_message = "Event dispatch failed"
        end if
    end subroutine
    
    subroutine dispatcher_batch_performance(this, events, success)
        !! High-performance batch dispatch
        class(event_dispatcher_t), intent(inout) :: this
        type(analysis_event_t), intent(inout) :: events(:)
        logical, intent(out) :: success
        
        ! For now, use regular batch dispatch
        call this%dispatch_batch(events, success)
    end subroutine
    
    subroutine dispatcher_sequence(this, events, success)
        !! Dispatch events in sequence with state consistency
        class(event_dispatcher_t), intent(inout) :: this
        type(analysis_event_t), intent(inout) :: events(:)
        logical, intent(out) :: success
        
        ! For now, use regular batch dispatch
        call this%dispatch_batch(events, success)
    end subroutine
    
    subroutine dispatcher_get_subscribers(this, event_type, subscriber_ids)
        !! Get list of subscribers for event type
        class(event_dispatcher_t), intent(in) :: this
        integer, intent(in) :: event_type
        integer, allocatable, intent(out) :: subscriber_ids(:)
        
        integer :: i, count
        
        if (.not. this%initialized .or. event_type < EVENT_NODE_ENTER .or. &
            event_type > EVENT_BUILTIN_REQUIRED) then
            allocate(subscriber_ids(0))
            return
        end if
        
        count = this%subscription_counts(event_type)
        allocate(subscriber_ids(count))
        
        do i = 1, count
            subscriber_ids(i) = this%subscriptions(event_type, i)%analyzer_id
        end do
    end subroutine

end module event_dispatcher_module