module semantic_event_system
    ! Event-driven AST traversal framework for extensible semantic analysis
    ! Manages analyzer subscriptions and coordinates event firing
    use ast_core, only: ast_arena_t
    use base_analyzer, only: base_analyzer_t
    implicit none
    private

    public :: semantic_event_system_t, event_data_t, create_event_system

    ! Event data container for passing information to handlers
    type :: event_data_t
        type(ast_arena_t), pointer :: arena => null()
        integer :: node_index = 0
        character(len=:), allocatable :: event_type
        character(len=:), allocatable :: additional_data
    end type event_data_t

    ! Event subscription entry
    type :: event_subscription_t
        character(len=:), allocatable :: event_name
        class(base_analyzer_t), pointer :: analyzer => null()
        integer :: priority = 0
    end type event_subscription_t

    ! Event system managing subscriptions and event firing
    type :: semantic_event_system_t
        type(event_subscription_t), allocatable :: subscriptions(:)
        integer :: subscription_count = 0
        integer :: capacity = 0
    contains
        procedure :: subscribe => event_subscribe
        procedure :: fire_event => event_fire
        procedure :: unsubscribe => event_unsubscribe
        procedure :: clear_subscriptions => event_clear_all
        procedure :: expand_capacity => event_expand_capacity
        procedure :: process_event_handler
    end type semantic_event_system_t

contains

    ! Create a new event system
    function create_event_system() result(event_system)
        type(semantic_event_system_t) :: event_system
        event_system%capacity = 10
        event_system%subscription_count = 0
        allocate(event_system%subscriptions(event_system%capacity))
    end function create_event_system

    ! Subscribe an analyzer to an event
    subroutine event_subscribe(this, event_name, analyzer, priority)
        class(semantic_event_system_t), intent(inout) :: this
        character(len=*), intent(in) :: event_name
        class(base_analyzer_t), target, intent(in) :: analyzer
        integer, intent(in), optional :: priority
        integer :: actual_priority
        integer :: i, insert_pos

        ! Set default priority
        actual_priority = 0
        if (present(priority)) actual_priority = priority

        ! Expand capacity if needed
        if (this%subscription_count >= this%capacity) then
            call this%expand_capacity()
        end if

        ! Find insertion position to maintain priority order (higher priority first)
        insert_pos = this%subscription_count + 1
        do i = 1, this%subscription_count
            if (actual_priority > this%subscriptions(i)%priority) then
                insert_pos = i
                exit
            end if
        end do

        ! Shift existing subscriptions to make room
        do i = this%subscription_count, insert_pos, -1
            this%subscriptions(i + 1) = this%subscriptions(i)
        end do

        ! Insert new subscription
        this%subscriptions(insert_pos)%event_name = event_name
        this%subscriptions(insert_pos)%analyzer => analyzer
        this%subscriptions(insert_pos)%priority = actual_priority
        this%subscription_count = this%subscription_count + 1
    end subroutine event_subscribe

    ! Fire an event to all subscribed analyzers
    subroutine event_fire(this, event_name, arena, node_index, additional_data)
        class(semantic_event_system_t), intent(inout) :: this
        character(len=*), intent(in) :: event_name
        type(ast_arena_t), target, intent(inout) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in), optional :: additional_data
        type(event_data_t) :: event_data
        integer :: i

        ! Prepare event data
        event_data%arena => arena
        event_data%node_index = node_index
        event_data%event_type = event_name
        if (present(additional_data)) then
            event_data%additional_data = additional_data
        end if

        ! Fire event to all matching subscriptions in priority order
        do i = 1, this%subscription_count
            if (allocated(this%subscriptions(i)%event_name)) then
                if (trim(this%subscriptions(i)%event_name) == trim(event_name)) then
                    if (associated(this%subscriptions(i)%analyzer)) then
                        ! Process node with this analyzer
                        call this%process_event_handler(this%subscriptions(i)%analyzer, &
                                                       event_data)
                    end if
                end if
            end if
        end do
    end subroutine event_fire

    ! Process event with a specific analyzer (internal method)
    subroutine process_event_handler(this, analyzer, event_data)
        class(semantic_event_system_t), intent(inout) :: this
        class(base_analyzer_t), pointer, intent(inout) :: analyzer
        type(event_data_t), intent(in) :: event_data

        ! Only process if analyzer is initialized
        if (analyzer%is_initialized()) then
            block
                use base_analyzer, only: analysis_result_t
                type(analysis_result_t) :: result
                ! Call analyzer to process the node
                result = analyzer%process_node(event_data%arena, event_data%node_index)
                ! Result handling could be added here if needed
            end block
        end if
    end subroutine process_event_handler

    ! Unsubscribe an analyzer from an event
    subroutine event_unsubscribe(this, event_name, analyzer)
        class(semantic_event_system_t), intent(inout) :: this
        character(len=*), intent(in) :: event_name
        class(base_analyzer_t), target, intent(in) :: analyzer
        integer :: i, j

        ! Find and remove matching subscriptions
        i = 1
        do while (i <= this%subscription_count)
            if (allocated(this%subscriptions(i)%event_name)) then
                if (trim(this%subscriptions(i)%event_name) == trim(event_name) .and. &
                    associated(this%subscriptions(i)%analyzer, analyzer)) then
                    ! Shift remaining subscriptions down
                    do j = i, this%subscription_count - 1
                        this%subscriptions(j) = this%subscriptions(j + 1)
                    end do
                    this%subscription_count = this%subscription_count - 1
                    cycle  ! Don't increment i, check same position again
                end if
            end if
            i = i + 1
        end do
    end subroutine event_unsubscribe

    ! Clear all subscriptions
    subroutine event_clear_all(this)
        class(semantic_event_system_t), intent(inout) :: this
        integer :: i

        ! Deallocate all event names
        do i = 1, this%subscription_count
            if (allocated(this%subscriptions(i)%event_name)) then
                deallocate(this%subscriptions(i)%event_name)
            end if
            this%subscriptions(i)%analyzer => null()
        end do

        this%subscription_count = 0
    end subroutine event_clear_all

    ! Expand subscription capacity
    subroutine event_expand_capacity(this)
        class(semantic_event_system_t), intent(inout) :: this
        type(event_subscription_t), allocatable :: temp_subscriptions(:)
        integer :: new_capacity, i

        new_capacity = this%capacity * 2
        allocate(temp_subscriptions(new_capacity))

        ! Copy existing subscriptions
        do i = 1, this%subscription_count
            temp_subscriptions(i) = this%subscriptions(i)
        end do

        ! Replace with expanded array
        deallocate(this%subscriptions)
        allocate(this%subscriptions(new_capacity))
        this%subscriptions = temp_subscriptions
        this%capacity = new_capacity
    end subroutine event_expand_capacity

end module semantic_event_system