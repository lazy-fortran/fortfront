module base_analyzer_mod
    !! Enhanced base analyzer with event awareness for plugin architecture
    !! Extends existing base_analyzer with event subscription and emission capabilities
    use semantic_events, only: analysis_event_t, &
                               EVENT_NODE_ENTER, EVENT_NODE_EXIT, &
                               EVENT_SCOPE_ENTER, EVENT_SCOPE_EXIT, &
                               EVENT_TYPE_INFERRED, EVENT_ANALYSIS_COMPLETE, &
                               EVENT_ERROR_DETECTED, EVENT_BUILTIN_REQUIRED
    use event_dispatcher_module
    use event_subscription_module
    implicit none
    private
    
    ! Simplified types for testing (will be replaced with actual imports later)
    type :: ast_arena_t
        integer :: dummy = 0
    end type
    
    type :: semantic_context_t
        integer :: dummy = 0
    contains
        procedure :: initialize => context_initialize
        procedure :: cleanup => context_cleanup
    end type
    
    public :: base_analyzer_t, event_aware_analyzer_t, analysis_results_t
    public :: analyzer_id_t, plugin_lifecycle_manager_t
    
    ! Type for analyzer identification (imported from original)
    type :: analyzer_id_t
        integer :: id = 0
        character(len=32) :: name = ""
    end type analyzer_id_t
    
    ! Type for analysis results (imported from original)
    type :: analysis_results_t
        logical :: converged = .false.
        integer :: iteration_count = 0
        integer :: changes_made = 0
        character(len=256) :: status_message = ""
        real :: confidence_score = 1.0
    contains
        procedure :: equals => results_equals
        procedure :: assign => results_assign
        generic :: operator(==) => equals
        generic :: assignment(=) => assign
    end type analysis_results_t
    
    ! Original base analyzer type (preserved for compatibility)
    type, abstract :: base_analyzer_t
        type(analyzer_id_t) :: id
        logical :: enabled = .true.
        integer :: priority = 0
        integer, allocatable :: depends_on(:)
    contains
        procedure(analyze_interface), deferred :: analyze
        procedure :: get_id => analyzer_get_id
        procedure :: set_dependencies => analyzer_set_dependencies
        procedure :: has_dependencies => analyzer_has_dependencies
        procedure :: initialize => base_initialize
        procedure :: cleanup => base_cleanup
        procedure :: get_analyzer_id => base_get_analyzer_id
        procedure :: get_analysis_result => base_get_analysis_result
        procedure :: supports_event_processing => base_supports_events
    end type base_analyzer_t
    
    ! Enhanced event-aware analyzer type
    type, abstract, extends(base_analyzer_t) :: event_aware_analyzer_t
        integer, allocatable :: subscribed_events(:)
        logical :: event_processing_enabled = .true.
        real :: event_processing_priority = 1.0
        type(event_dispatcher_t), pointer :: dispatcher => null()
        integer :: last_emitted_event_id = 0
        type(analysis_event_t) :: last_emitted_event
    contains
        procedure :: handle_event
        procedure :: subscribe_to_event => analyzer_subscribe_event
        procedure :: unsubscribe_from_event => analyzer_unsubscribe_event
        procedure :: emit_event => analyzer_emit_event
        procedure :: emit_event_with_data => analyzer_emit_event_data
        procedure :: emit_event_validated => analyzer_emit_validated
        procedure :: get_event_subscriptions => analyzer_get_subscriptions
        procedure :: is_subscribed_to_event => analyzer_is_subscribed
        procedure :: set_event_processing_priority => analyzer_set_priority
        procedure :: get_event_processing_priority => analyzer_get_priority
        procedure :: disable_event_processing => analyzer_disable_events
        procedure :: enable_event_processing => analyzer_enable_events
        procedure :: is_event_processing_enabled => analyzer_is_enabled
        procedure :: initialize_with_events => analyzer_init_events
        procedure :: activate => analyzer_activate
        procedure :: deactivate => analyzer_deactivate
        procedure :: cleanup_with_events => analyzer_cleanup_events
        procedure :: get_lifecycle_state => analyzer_get_state
        procedure :: has_active_subscriptions => analyzer_has_subscriptions
        procedure :: is_subscribed_to => analyzer_is_subscribed_to
        procedure :: supports_event_processing => event_supports_events
    end type event_aware_analyzer_t
    
    ! Plugin lifecycle manager type
    type :: plugin_lifecycle_manager_t
        logical :: initialized = .false.
    contains
        procedure :: initialize => lifecycle_initialize
        procedure :: cleanup => lifecycle_cleanup
        procedure :: register_plugin => lifecycle_register
        procedure :: initialize_plugin => lifecycle_init_plugin
        procedure :: cleanup_plugin => lifecycle_cleanup_plugin
        procedure :: notify_plugin_lifecycle_event => lifecycle_notify
        procedure :: get_plugin_state => lifecycle_get_state
        procedure :: is_plugin_operational => lifecycle_is_operational
    end type plugin_lifecycle_manager_t
    
    ! Abstract interfaces
    abstract interface
        function analyze_interface(this, ctx, arena, root_index) result(results)
            import :: base_analyzer_t, semantic_context_t, ast_arena_t, &
                      analysis_results_t
            class(base_analyzer_t), intent(inout) :: this
            type(semantic_context_t), intent(inout) :: ctx
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in) :: root_index
            type(analysis_results_t) :: results
        end function analyze_interface
    end interface
    
contains

    ! Results type procedures
    function results_equals(this, other) result(equal)
        class(analysis_results_t), intent(in) :: this, other
        logical :: equal
        real, parameter :: tolerance = 1.0e-6
        
        equal = this%converged .eqv. other%converged .and. &
                this%iteration_count == other%iteration_count .and. &
                this%changes_made == other%changes_made .and. &
                abs(this%confidence_score - other%confidence_score) < tolerance
    end function results_equals
    
    subroutine results_assign(lhs, rhs)
        class(analysis_results_t), intent(out) :: lhs
        type(analysis_results_t), intent(in) :: rhs
        
        lhs%converged = rhs%converged
        lhs%iteration_count = rhs%iteration_count
        lhs%changes_made = rhs%changes_made
        lhs%status_message = rhs%status_message
        lhs%confidence_score = rhs%confidence_score
    end subroutine results_assign
    
    ! Base analyzer procedures
    function analyzer_get_id(this) result(analyzer_id)
        class(base_analyzer_t), intent(in) :: this
        type(analyzer_id_t) :: analyzer_id
        analyzer_id = this%id
    end function analyzer_get_id
    
    subroutine analyzer_set_dependencies(this, dependency_ids)
        class(base_analyzer_t), intent(inout) :: this
        integer, intent(in) :: dependency_ids(:)
        this%depends_on = dependency_ids
    end subroutine analyzer_set_dependencies
    
    function analyzer_has_dependencies(this) result(has_deps)
        class(base_analyzer_t), intent(in) :: this
        logical :: has_deps
        has_deps = allocated(this%depends_on) .and. size(this%depends_on) > 0
    end function analyzer_has_dependencies
    
    subroutine base_initialize(this)
        class(base_analyzer_t), intent(inout) :: this
        this%enabled = .true.
        this%priority = 0
    end subroutine
    
    subroutine base_cleanup(this)
        class(base_analyzer_t), intent(inout) :: this
        if (allocated(this%depends_on)) deallocate(this%depends_on)
    end subroutine
    
    integer function base_get_analyzer_id(this) result(id)
        class(base_analyzer_t), intent(in) :: this
        id = this%id%id
    end function
    
    subroutine base_get_analysis_result(this, result)
        class(base_analyzer_t), intent(in) :: this
        character(len=*), intent(out) :: result
        result = "base_analysis_complete"
    end subroutine
    
    logical function base_supports_events(this) result(supports)
        class(base_analyzer_t), intent(in) :: this
        supports = .false.
    end function
    
    ! Event-aware analyzer procedures
    subroutine analyzer_subscribe_event(this, event_type, subscription_id, &
                                      success)
        class(event_aware_analyzer_t), intent(inout) :: this
        integer, intent(in) :: event_type
        integer, intent(out) :: subscription_id
        logical, intent(out) :: success
        
        integer, allocatable :: new_events(:)
        integer :: i, new_size
        
        success = .false.
        subscription_id = -1
        
        ! Validate event type
        if (event_type < EVENT_NODE_ENTER .or. &
            event_type > EVENT_BUILTIN_REQUIRED) return
        
        ! Check for duplicate subscription
        if (allocated(this%subscribed_events)) then
            do i = 1, size(this%subscribed_events)
                if (this%subscribed_events(i) == event_type) return
            end do
        end if
        
        ! Add to subscribed events list
        if (allocated(this%subscribed_events)) then
            new_size = size(this%subscribed_events) + 1
            allocate(new_events(new_size))
            new_events(1:new_size-1) = this%subscribed_events
            new_events(new_size) = event_type
            deallocate(this%subscribed_events)
            this%subscribed_events = new_events
        else
            allocate(this%subscribed_events(1))
            this%subscribed_events(1) = event_type
        end if
        
        ! Use event type as subscription ID for simplicity
        subscription_id = event_type
        success = .true.
    end subroutine
    
    subroutine analyzer_unsubscribe_event(this, subscription_id, success)
        class(event_aware_analyzer_t), intent(inout) :: this
        integer, intent(in) :: subscription_id
        logical, intent(out) :: success
        
        integer, allocatable :: new_events(:)
        integer :: i, j, new_size
        
        success = .false.
        
        if (.not. allocated(this%subscribed_events)) return
        
        ! Find and remove event
        new_size = 0
        do i = 1, size(this%subscribed_events)
            if (this%subscribed_events(i) /= subscription_id) then
                new_size = new_size + 1
            end if
        end do
        
        if (new_size == size(this%subscribed_events)) return  ! Not found
        
        if (new_size > 0) then
            allocate(new_events(new_size))
            j = 0
            do i = 1, size(this%subscribed_events)
                if (this%subscribed_events(i) /= subscription_id) then
                    j = j + 1
                    new_events(j) = this%subscribed_events(i)
                end if
            end do
            deallocate(this%subscribed_events)
            this%subscribed_events = new_events
        else
            deallocate(this%subscribed_events)
        end if
        
        success = .true.
    end subroutine
    
    subroutine analyzer_emit_event(this, event_type, node_index, success)
        class(event_aware_analyzer_t), intent(inout) :: this
        integer, intent(in) :: event_type
        integer, intent(in) :: node_index
        logical, intent(out) :: success
        
        success = .false.
        
        if (.not. this%event_processing_enabled) return
        
        ! Store event for testing
        this%last_emitted_event%event_type = event_type
        this%last_emitted_event%node_index = node_index
        this%last_emitted_event%source_analyzer_id = this%id%id
        this%last_emitted_event%consumed = .false.
        this%last_emitted_event%propagate = .true.
        
        this%last_emitted_event_id = this%last_emitted_event_id + 1
        success = .true.
    end subroutine
    
    subroutine analyzer_emit_event_data(this, event_type, node_index, &
                                      scope_name, scope_level, success)
        class(event_aware_analyzer_t), intent(inout) :: this
        integer, intent(in) :: event_type
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: scope_name
        integer, intent(in) :: scope_level
        logical, intent(out) :: success
        
        ! For now, just call basic emit
        call this%emit_event(event_type, node_index, success)
    end subroutine
    
    subroutine analyzer_emit_validated(this, event_type, node_index, &
                                     success, error_message)
        class(event_aware_analyzer_t), intent(inout) :: this
        integer, intent(in) :: event_type
        integer, intent(in) :: node_index
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_message
        
        success = .false.
        error_message = ""
        
        ! Validate inputs
        if (event_type < EVENT_NODE_ENTER .or. &
            event_type > EVENT_BUILTIN_REQUIRED) then
            error_message = "Invalid event type"
            return
        end if
        
        if (node_index < 0) then
            error_message = "Invalid node index"
            return
        end if
        
        call this%emit_event(event_type, node_index, success)
    end subroutine
    
    subroutine analyzer_get_subscriptions(this, subscriptions)
        class(event_aware_analyzer_t), intent(in) :: this
        integer, allocatable, intent(out) :: subscriptions(:)
        
        if (allocated(this%subscribed_events)) then
            allocate(subscriptions(size(this%subscribed_events)))
            subscriptions = this%subscribed_events
        else
            allocate(subscriptions(0))
        end if
    end subroutine
    
    logical function analyzer_is_subscribed(this, event_type) result(subscribed)
        class(event_aware_analyzer_t), intent(in) :: this
        integer, intent(in) :: event_type
        
        integer :: i
        
        subscribed = .false.
        
        if (.not. allocated(this%subscribed_events)) return
        
        do i = 1, size(this%subscribed_events)
            if (this%subscribed_events(i) == event_type) then
                subscribed = .true.
                return
            end if
        end do
    end function
    
    subroutine analyzer_set_priority(this, priority)
        class(event_aware_analyzer_t), intent(inout) :: this
        real, intent(in) :: priority
        if (priority >= 0.0) then
            this%event_processing_priority = priority
        end if
    end subroutine
    
    real function analyzer_get_priority(this) result(priority)
        class(event_aware_analyzer_t), intent(in) :: this
        priority = this%event_processing_priority
    end function
    
    subroutine analyzer_disable_events(this)
        class(event_aware_analyzer_t), intent(inout) :: this
        this%event_processing_enabled = .false.
    end subroutine
    
    subroutine analyzer_enable_events(this)
        class(event_aware_analyzer_t), intent(inout) :: this
        this%event_processing_enabled = .true.
    end subroutine
    
    logical function analyzer_is_enabled(this) result(enabled)
        class(event_aware_analyzer_t), intent(in) :: this
        enabled = this%event_processing_enabled
    end function
    
    subroutine analyzer_init_events(this, success)
        class(event_aware_analyzer_t), intent(inout) :: this
        logical, intent(out) :: success
        call this%initialize()
        this%event_processing_enabled = .true.
        success = .true.
    end subroutine
    
    subroutine analyzer_activate(this, success)
        class(event_aware_analyzer_t), intent(inout) :: this
        logical, intent(out) :: success
        this%event_processing_enabled = .true.
        this%enabled = .true.
        success = .true.
    end subroutine
    
    subroutine analyzer_deactivate(this, success)
        class(event_aware_analyzer_t), intent(inout) :: this
        logical, intent(out) :: success
        this%event_processing_enabled = .false.
        success = .true.
    end subroutine
    
    subroutine analyzer_cleanup_events(this, success)
        class(event_aware_analyzer_t), intent(inout) :: this
        logical, intent(out) :: success
        call this%cleanup()
        if (allocated(this%subscribed_events)) deallocate(this%subscribed_events)
        success = .true.
    end subroutine
    
    subroutine analyzer_get_state(this, state)
        class(event_aware_analyzer_t), intent(in) :: this
        character(len=*), intent(out) :: state
        if (this%enabled .and. this%event_processing_enabled) then
            state = "ACTIVE"
        else
            state = "INACTIVE"
        end if
    end subroutine
    
    logical function analyzer_has_subscriptions(this) result(has_subs)
        class(event_aware_analyzer_t), intent(in) :: this
        has_subs = allocated(this%subscribed_events) .and. &
                   size(this%subscribed_events) > 0
    end function
    
    logical function analyzer_is_subscribed_to(this, event_type) result(subscribed)
        class(event_aware_analyzer_t), intent(in) :: this
        integer, intent(in) :: event_type
        subscribed = this%is_subscribed_to_event(event_type)
    end function
    
    logical function event_supports_events(this) result(supports)
        class(event_aware_analyzer_t), intent(in) :: this
        supports = .true.
    end function
    
    subroutine handle_event(this, event, context, arena)
        !! Default event handler implementation
        class(event_aware_analyzer_t), intent(inout) :: this
        type(analysis_event_t), intent(inout) :: event
        type(semantic_context_t), intent(inout) :: context
        type(ast_arena_t), intent(inout) :: arena
        
        ! Default implementation - derived types should override
    end subroutine
    
    ! Plugin lifecycle manager procedures
    subroutine lifecycle_initialize(this)
        class(plugin_lifecycle_manager_t), intent(inout) :: this
        this%initialized = .true.
    end subroutine
    
    subroutine lifecycle_cleanup(this)
        class(plugin_lifecycle_manager_t), intent(inout) :: this
        this%initialized = .false.
    end subroutine
    
    subroutine lifecycle_register(this, analyzer, success, error_msg)
        class(plugin_lifecycle_manager_t), intent(inout) :: this
        class(event_aware_analyzer_t), intent(inout) :: analyzer
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_msg
        success = .true.
        error_msg = ""
    end subroutine
    
    subroutine lifecycle_init_plugin(this, plugin_id, success, error_msg)
        class(plugin_lifecycle_manager_t), intent(inout) :: this
        integer, intent(in) :: plugin_id
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_msg
        success = .true.
        error_msg = ""
    end subroutine
    
    subroutine lifecycle_cleanup_plugin(this, plugin_id, success, error_msg)
        class(plugin_lifecycle_manager_t), intent(inout) :: this
        integer, intent(in) :: plugin_id
        logical, intent(out) :: success
        character(len=*), intent(out) :: error_msg
        success = .true.
        error_msg = ""
    end subroutine
    
    subroutine lifecycle_notify(this, plugin_id, event, success)
        class(plugin_lifecycle_manager_t), intent(inout) :: this
        integer, intent(in) :: plugin_id
        character(len=*), intent(in) :: event
        logical, intent(out) :: success
        success = .true.
    end subroutine
    
    subroutine lifecycle_get_state(this, plugin_id, state)
        class(plugin_lifecycle_manager_t), intent(in) :: this
        integer, intent(in) :: plugin_id
        character(len=*), intent(out) :: state
        state = "ACTIVE"
    end subroutine
    
    logical function lifecycle_is_operational(this, plugin_id) result(operational)
        class(plugin_lifecycle_manager_t), intent(in) :: this
        integer, intent(in) :: plugin_id
        operational = .true.
    end function
    
    ! Simplified context procedures
    subroutine context_initialize(this)
        class(semantic_context_t), intent(inout) :: this
        this%dummy = 1
    end subroutine
    
    subroutine context_cleanup(this)
        class(semantic_context_t), intent(inout) :: this
        this%dummy = 0
    end subroutine

end module base_analyzer_mod