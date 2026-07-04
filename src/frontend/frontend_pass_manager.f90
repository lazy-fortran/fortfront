module frontend_pass_manager
    ! Lightweight pass manager for frontend transformation pipeline
    ! Based on GCC's gfc_run_passes architecture
    use compiler_arena, only: compiler_arena_t
    use call_graph_signatures_mod, only: signatures_map_t
    use debug_trace, only: trace_enter, trace_leave
    use semantic_operating_mode, only: OPERATING_MODE_INFER
    implicit none
    private

    public :: pass_t, pass_manager_t, pass_context_t, pass_config_t
    public :: create_pass_manager, create_default_config
    public :: PASS_SEMANTIC, PASS_STANDARDIZATION, PASS_MONOMORPHIZATION, &
        PASS_CODEGEN

    ! Pass identifiers for final phases
    integer, parameter :: PASS_SEMANTIC = 1
    integer, parameter :: PASS_STANDARDIZATION = 2
    integer, parameter :: PASS_MONOMORPHIZATION = 3
    integer, parameter :: PASS_CODEGEN = 4

    ! Pass configuration - control which passes run
    type :: pass_config_t
        logical :: enable_semantic
        logical :: enable_standardization
        logical :: enable_monomorphization
        logical :: enable_codegen
        logical :: stop_after_semantic
        logical :: stop_after_standardization
    end type pass_config_t

    ! Pass context - shared data passed between passes
    type :: pass_context_t
        type(compiler_arena_t), pointer :: compiler_arena => null()
        integer :: prog_index
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        type(signatures_map_t) :: signatures
        integer :: operating_mode = OPERATING_MODE_INFER
        logical :: enable_ast_wrapping
        logical :: has_wrapping_flag
        logical :: has_functions
        logical :: has_subroutines
        logical :: has_main_code
    end type pass_context_t

    ! Pass procedure interface
    abstract interface
        subroutine pass_proc(context)
            import :: pass_context_t
            type(pass_context_t), intent(inout) :: context
        end subroutine pass_proc
    end interface

    ! Pass metadata
    type :: pass_t
        integer :: pass_id = 0
        character(len=64) :: name = ""
        character(len=64) :: trace_key = ""
        logical :: required = .false.
        procedure(pass_proc), nopass, pointer :: execute => null()
    end type pass_t

    ! Pass manager
    type :: pass_manager_t
        type(pass_t), allocatable :: passes(:)
        integer :: num_passes
        type(pass_config_t) :: config
    contains
        procedure :: add_pass => pass_manager_add_pass
        procedure :: run => pass_manager_run
        procedure :: clear => pass_manager_clear
    end type pass_manager_t

contains

    ! Create a new pass manager with default configuration
    function create_pass_manager() result(manager)
        type(pass_manager_t) :: manager

        manager%num_passes = 0
        allocate (manager%passes(0))
        manager%config = create_default_config()
    end function create_pass_manager

    ! Create default configuration (all passes enabled)
    function create_default_config() result(config)
        type(pass_config_t) :: config

        config%enable_semantic = .true.
        config%enable_standardization = .true.
        config%enable_monomorphization = .true.
        config%enable_codegen = .true.
        config%stop_after_semantic = .false.
        config%stop_after_standardization = .false.
    end function create_default_config

    ! Add pass to manager
    subroutine pass_manager_add_pass(this, pass_id, name, trace_key, &
            required, execute)
        class(pass_manager_t), intent(inout) :: this
        integer, intent(in) :: pass_id
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: trace_key
        logical, intent(in) :: required
        procedure(pass_proc) :: execute
        type(pass_t), allocatable :: temp_passes(:)
        integer :: i

        ! Reallocate passes array
        allocate (temp_passes(this%num_passes + 1))
        do i = 1, this%num_passes
            temp_passes(i)%pass_id = this%passes(i)%pass_id
            temp_passes(i)%name = this%passes(i)%name
            temp_passes(i)%trace_key = this%passes(i)%trace_key
            temp_passes(i)%required = this%passes(i)%required
            if (associated(this%passes(i)%execute)) then
                temp_passes(i)%execute => this%passes(i)%execute
            else
                temp_passes(i)%execute => null()
            end if
        end do

        ! Add new pass
        temp_passes(this%num_passes + 1)%pass_id = pass_id
        temp_passes(this%num_passes + 1)%name = name
        temp_passes(this%num_passes + 1)%trace_key = trace_key
        temp_passes(this%num_passes + 1)%required = required
        temp_passes(this%num_passes + 1)%execute => execute

        ! Move allocation
        call move_alloc(temp_passes, this%passes)
        this%num_passes = this%num_passes + 1
    end subroutine pass_manager_add_pass

    ! Run all registered passes
    subroutine pass_manager_run(this, context)
        class(pass_manager_t), intent(in) :: this
        type(pass_context_t), intent(inout) :: context
        integer :: i
        logical :: should_run

        do i = 1, this%num_passes
            ! Check if pass should run based on configuration
            should_run = pass_should_run(this%passes(i), this%config)

            if (should_run) then
                ! Trace enter
                if (len_trim(this%passes(i)%trace_key) > 0) then
                    call trace_enter(trim(this%passes(i)%trace_key))
                end if

                ! Execute pass
                if (associated(this%passes(i)%execute)) then
                    call this%passes(i)%execute(context)
                end if

                ! Trace leave
                if (len_trim(this%passes(i)%trace_key) > 0) then
                    call trace_leave(trim(this%passes(i)%trace_key))
                end if

                ! Check for errors - stop pipeline if error in required pass
                if (allocated(context%error_msg)) then
                    if (len(context%error_msg) > 0 .and. this%passes(i)%required) then
                        return
                    end if
                end if

                ! Check for early stop conditions
                if (this%config%stop_after_semantic .and. &
                    this%passes(i)%pass_id == PASS_SEMANTIC) then
                    return
                end if

                if (this%config%stop_after_standardization .and. &
                    this%passes(i)%pass_id == PASS_STANDARDIZATION) then
                    return
                end if
            end if
        end do
    end subroutine pass_manager_run

    ! Check if a pass should run based on configuration
    pure function pass_should_run(pass, config) result(should_run)
        type(pass_t), intent(in) :: pass
        type(pass_config_t), intent(in) :: config
        logical :: should_run

        should_run = .false.

        select case (pass%pass_id)
        case (PASS_SEMANTIC)
            should_run = config%enable_semantic
        case (PASS_STANDARDIZATION)
            should_run = config%enable_standardization
        case (PASS_MONOMORPHIZATION)
            should_run = config%enable_monomorphization
        case (PASS_CODEGEN)
            should_run = config%enable_codegen
        case default
            ! Unknown/custom pass - run by default
            should_run = .true.
        end select
    end function pass_should_run

    ! Clear all registered passes
    subroutine pass_manager_clear(this)
        class(pass_manager_t), intent(inout) :: this

        if (allocated(this%passes)) then
            deallocate (this%passes)
        end if
        this%num_passes = 0
        allocate (this%passes(0))
    end subroutine pass_manager_clear

end module frontend_pass_manager
