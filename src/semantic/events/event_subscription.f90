module event_subscription_module
    !! Subscription management for event system
    !! Handles dynamic subscription operations with priority ordering
    use semantic_events
    implicit none
    private
    
    integer, parameter :: MAX_TOTAL_SUBSCRIPTIONS = 1000
    
    type :: managed_subscription_t
        integer :: subscription_id = 0
        integer :: event_type = 0
        integer :: analyzer_id = 0
        real :: priority = 1.0
        logical :: enabled = .true.
        logical :: active = .false.
    end type
    
    type, public :: subscription_manager_t
        type(managed_subscription_t) :: subscriptions(MAX_TOTAL_SUBSCRIPTIONS)
        integer :: subscription_count = 0
        integer :: next_id = 1
        logical :: initialized = .false.
    contains
        procedure :: initialize => manager_initialize
        procedure :: cleanup => manager_cleanup
        procedure :: add_subscription => manager_add_subscription
        procedure :: remove_subscription => manager_remove_subscription
        procedure :: has_subscription => manager_has_subscription
        procedure :: get_subscription_count => manager_get_count
        procedure :: get_subscriptions_by_priority => manager_get_by_priority
        procedure :: update_subscription_priority => manager_update_priority
        procedure :: disable_subscription => manager_disable
        procedure :: enable_subscription => manager_enable
        procedure :: is_subscription_enabled => manager_is_enabled
        procedure :: get_subscription_priority => manager_get_priority
    end type
    
contains

    subroutine manager_initialize(this)
        !! Initialize subscription manager
        class(subscription_manager_t), intent(inout) :: this
        
        integer :: i
        
        do i = 1, MAX_TOTAL_SUBSCRIPTIONS
            this%subscriptions(i)%subscription_id = 0
            this%subscriptions(i)%event_type = 0
            this%subscriptions(i)%analyzer_id = 0
            this%subscriptions(i)%priority = 1.0
            this%subscriptions(i)%enabled = .true.
            this%subscriptions(i)%active = .false.
        end do
        
        this%subscription_count = 0
        this%next_id = 1
        this%initialized = .true.
    end subroutine
    
    subroutine manager_cleanup(this)
        !! Clean up subscription manager
        class(subscription_manager_t), intent(inout) :: this
        
        call this%initialize()
        this%initialized = .false.
    end subroutine
    
    subroutine manager_add_subscription(this, subscription, subscription_id, &
                                       success, error_message)
        !! Add new subscription with validation
        class(subscription_manager_t), intent(inout) :: this
        type(event_subscription_t), intent(in) :: subscription
        integer, intent(out) :: subscription_id
        logical, intent(out) :: success
        character(len=*), intent(out), optional :: error_message
        
        integer :: i, slot
        character(len=256) :: local_error
        
        success = .false.
        subscription_id = -1
        local_error = ""
        
        if (.not. this%initialized) then
            local_error = "Subscription manager not initialized"
            if (present(error_message)) error_message = local_error
            return
        end if
        
        if (this%subscription_count >= MAX_TOTAL_SUBSCRIPTIONS) then
            local_error = "Maximum subscription limit reached"
            if (present(error_message)) error_message = local_error
            return
        end if
        
        ! Validate subscription
        if (subscription%event_type < EVENT_NODE_ENTER .or. &
            subscription%event_type > EVENT_BUILTIN_REQUIRED) then
            local_error = "Invalid event type"
            if (present(error_message)) error_message = local_error
            return
        end if
        
        if (subscription%analyzer_id <= 0) then
            local_error = "Invalid analyzer ID"
            if (present(error_message)) error_message = local_error
            return
        end if
        
        if (subscription%priority < 0.0) then
            local_error = "Invalid priority value"
            if (present(error_message)) error_message = local_error
            return
        end if
        
        ! Check for duplicate subscription
        do i = 1, this%subscription_count
            if (this%subscriptions(i)%active .and. &
                this%subscriptions(i)%event_type == subscription%event_type .and. &
                this%subscriptions(i)%analyzer_id == subscription%analyzer_id) then
                local_error = "Duplicate subscription"
                if (present(error_message)) error_message = local_error
                return
            end if
        end do
        
        ! Find available slot
        slot = 0
        do i = 1, MAX_TOTAL_SUBSCRIPTIONS
            if (.not. this%subscriptions(i)%active) then
                slot = i
                exit
            end if
        end do
        
        if (slot == 0) then
            local_error = "No available subscription slot"
            if (present(error_message)) error_message = local_error
            return
        end if
        
        ! Add subscription
        this%subscriptions(slot)%subscription_id = this%next_id
        this%subscriptions(slot)%event_type = subscription%event_type
        this%subscriptions(slot)%analyzer_id = subscription%analyzer_id
        this%subscriptions(slot)%priority = subscription%priority
        this%subscriptions(slot)%enabled = .true.
        this%subscriptions(slot)%active = .true.
        
        subscription_id = this%next_id
        this%next_id = this%next_id + 1
        this%subscription_count = this%subscription_count + 1
        success = .true.
        
        if (present(error_message)) error_message = ""
    end subroutine
    
    subroutine manager_remove_subscription(this, subscription_id, success)
        !! Remove subscription by ID
        class(subscription_manager_t), intent(inout) :: this
        integer, intent(in) :: subscription_id
        logical, intent(out) :: success
        
        integer :: i
        
        success = .false.
        
        if (.not. this%initialized) return
        
        do i = 1, MAX_TOTAL_SUBSCRIPTIONS
            if (this%subscriptions(i)%active .and. &
                this%subscriptions(i)%subscription_id == subscription_id) then
                
                this%subscriptions(i)%subscription_id = 0
                this%subscriptions(i)%event_type = 0
                this%subscriptions(i)%analyzer_id = 0
                this%subscriptions(i)%priority = 1.0
                this%subscriptions(i)%enabled = .true.
                this%subscriptions(i)%active = .false.
                
                this%subscription_count = this%subscription_count - 1
                success = .true.
                return
            end if
        end do
    end subroutine
    
    function manager_has_subscription(this, subscription_id) result(has_sub)
        !! Check if subscription exists
        class(subscription_manager_t), intent(in) :: this
        integer, intent(in) :: subscription_id
        logical :: has_sub
        
        integer :: i
        
        has_sub = .false.
        
        if (.not. this%initialized) return
        
        do i = 1, MAX_TOTAL_SUBSCRIPTIONS
            if (this%subscriptions(i)%active .and. &
                this%subscriptions(i)%subscription_id == subscription_id) then
                has_sub = .true.
                return
            end if
        end do
    end function
    
    function manager_get_count(this, event_type) result(count)
        !! Get subscription count for event type
        class(subscription_manager_t), intent(in) :: this
        integer, intent(in) :: event_type
        integer :: count
        
        integer :: i
        
        count = 0
        
        if (.not. this%initialized) return
        
        do i = 1, MAX_TOTAL_SUBSCRIPTIONS
            if (this%subscriptions(i)%active .and. &
                this%subscriptions(i)%event_type == event_type) then
                count = count + 1
            end if
        end do
    end function
    
    subroutine manager_get_by_priority(this, event_type, ordered_ids)
        !! Get subscriptions ordered by priority (high to low)
        class(subscription_manager_t), intent(in) :: this
        integer, intent(in) :: event_type
        integer, allocatable, intent(out) :: ordered_ids(:)
        
        integer :: i, j, count, temp_id
        real :: temp_priority
        integer, allocatable :: ids(:)
        real, allocatable :: priorities(:)
        
        ! Count matching subscriptions
        count = 0
        do i = 1, MAX_TOTAL_SUBSCRIPTIONS
            if (this%subscriptions(i)%active .and. &
                this%subscriptions(i)%event_type == event_type .and. &
                this%subscriptions(i)%enabled) then
                count = count + 1
            end if
        end do
        
        if (count == 0) then
            allocate(ordered_ids(0))
            return
        end if
        
        ! Collect IDs and priorities
        allocate(ids(count))
        allocate(priorities(count))
        
        j = 0
        do i = 1, MAX_TOTAL_SUBSCRIPTIONS
            if (this%subscriptions(i)%active .and. &
                this%subscriptions(i)%event_type == event_type .and. &
                this%subscriptions(i)%enabled) then
                j = j + 1
                ids(j) = this%subscriptions(i)%subscription_id
                priorities(j) = this%subscriptions(i)%priority
            end if
        end do
        
        ! Simple bubble sort by priority (high to low)
        do i = 1, count - 1
            do j = 1, count - i
                if (priorities(j) < priorities(j + 1)) then
                    temp_priority = priorities(j)
                    priorities(j) = priorities(j + 1)
                    priorities(j + 1) = temp_priority
                    
                    temp_id = ids(j)
                    ids(j) = ids(j + 1)
                    ids(j + 1) = temp_id
                end if
            end do
        end do
        
        allocate(ordered_ids(count))
        ordered_ids = ids
    end subroutine
    
    subroutine manager_update_priority(this, subscription_id, new_priority, &
                                     success)
        !! Update subscription priority
        class(subscription_manager_t), intent(inout) :: this
        integer, intent(in) :: subscription_id
        real, intent(in) :: new_priority
        logical, intent(out) :: success
        
        integer :: i
        
        success = .false.
        
        if (.not. this%initialized .or. new_priority < 0.0) return
        
        do i = 1, MAX_TOTAL_SUBSCRIPTIONS
            if (this%subscriptions(i)%active .and. &
                this%subscriptions(i)%subscription_id == subscription_id) then
                this%subscriptions(i)%priority = new_priority
                success = .true.
                return
            end if
        end do
    end subroutine
    
    subroutine manager_disable(this, subscription_id, success)
        !! Disable subscription
        class(subscription_manager_t), intent(inout) :: this
        integer, intent(in) :: subscription_id
        logical, intent(out) :: success
        
        integer :: i
        
        success = .false.
        
        if (.not. this%initialized) return
        
        do i = 1, MAX_TOTAL_SUBSCRIPTIONS
            if (this%subscriptions(i)%active .and. &
                this%subscriptions(i)%subscription_id == subscription_id) then
                this%subscriptions(i)%enabled = .false.
                success = .true.
                return
            end if
        end do
    end subroutine
    
    subroutine manager_enable(this, subscription_id, success)
        !! Enable subscription
        class(subscription_manager_t), intent(inout) :: this
        integer, intent(in) :: subscription_id
        logical, intent(out) :: success
        
        integer :: i
        
        success = .false.
        
        if (.not. this%initialized) return
        
        do i = 1, MAX_TOTAL_SUBSCRIPTIONS
            if (this%subscriptions(i)%active .and. &
                this%subscriptions(i)%subscription_id == subscription_id) then
                this%subscriptions(i)%enabled = .true.
                success = .true.
                return
            end if
        end do
    end subroutine
    
    function manager_is_enabled(this, subscription_id) result(enabled)
        !! Check if subscription is enabled
        class(subscription_manager_t), intent(in) :: this
        integer, intent(in) :: subscription_id
        logical :: enabled
        
        integer :: i
        
        enabled = .false.
        
        if (.not. this%initialized) return
        
        do i = 1, MAX_TOTAL_SUBSCRIPTIONS
            if (this%subscriptions(i)%active .and. &
                this%subscriptions(i)%subscription_id == subscription_id) then
                enabled = this%subscriptions(i)%enabled
                return
            end if
        end do
    end function
    
    function manager_get_priority(this, subscription_id) result(priority)
        !! Get subscription priority
        class(subscription_manager_t), intent(in) :: this
        integer, intent(in) :: subscription_id
        real :: priority
        
        integer :: i
        
        priority = -1.0  ! Invalid priority indicates not found
        
        if (.not. this%initialized) return
        
        do i = 1, MAX_TOTAL_SUBSCRIPTIONS
            if (this%subscriptions(i)%active .and. &
                this%subscriptions(i)%subscription_id == subscription_id) then
                priority = this%subscriptions(i)%priority
                return
            end if
        end do
    end function

end module event_subscription_module