module base_analyzer
    ! Abstract base interface for extensible semantic analyzers
    ! Provides the foundation for plugin-based semantic analysis
    use ast_core, only: ast_arena_t
    use type_system_hm, only: mono_type_t
    implicit none
    private

    public :: base_analyzer_t, analyzer_config_t, analysis_result_t

    ! Configuration for analyzer initialization
    type :: analyzer_config_t
        logical :: enabled = .true.
        integer :: priority = 0
        character(len=:), allocatable :: name
    end type analyzer_config_t

    ! Analysis result container
    type :: analysis_result_t
        logical :: has_findings = .false.
        character(len=:), allocatable :: result_data
        integer :: error_count = 0
        integer :: warning_count = 0
    contains
        procedure :: clear => result_clear
    end type analysis_result_t

    ! Abstract base analyzer interface
    type, abstract :: base_analyzer_t
        type(analyzer_config_t) :: config
        logical :: initialized = .false.
    contains
        procedure(initialize_interface), deferred :: initialize
        procedure(process_node_interface), deferred :: process_node
        procedure(get_results_interface), deferred :: get_results
        procedure(cleanup_interface), deferred :: cleanup
        procedure :: is_initialized
    end type base_analyzer_t

    ! Abstract interfaces that must be implemented by concrete analyzers
    abstract interface
        subroutine initialize_interface(this, config)
            import :: base_analyzer_t, analyzer_config_t
            class(base_analyzer_t), intent(inout) :: this
            type(analyzer_config_t), intent(in) :: config
        end subroutine initialize_interface

        function process_node_interface(this, arena, node_index) result(result)
            import :: base_analyzer_t, ast_arena_t, analysis_result_t
            class(base_analyzer_t), intent(inout) :: this
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in) :: node_index
            type(analysis_result_t) :: result
        end function process_node_interface

        function get_results_interface(this) result(results)
            import :: base_analyzer_t, analysis_result_t
            class(base_analyzer_t), intent(in) :: this
            type(analysis_result_t) :: results
        end function get_results_interface

        subroutine cleanup_interface(this)
            import :: base_analyzer_t
            class(base_analyzer_t), intent(inout) :: this
        end subroutine cleanup_interface
    end interface

contains

    ! Check if analyzer is properly initialized
    function is_initialized(this) result(status)
        class(base_analyzer_t), intent(in) :: this
        logical :: status
        status = this%initialized
    end function is_initialized

    ! Clear analysis results
    subroutine result_clear(this)
        class(analysis_result_t), intent(inout) :: this
        this%has_findings = .false.
        this%error_count = 0
        this%warning_count = 0
        if (allocated(this%result_data)) then
            deallocate(this%result_data)
        end if
    end subroutine result_clear

end module base_analyzer