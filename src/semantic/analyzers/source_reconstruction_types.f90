module source_reconstruction_types
    ! Source reconstruction type definitions
    ! Split from source_reconstruction_analyzer.f90 for architectural compliance (Issue #1067)
    use semantic_result_types, only: semantic_result_base_t
    use semantic_analyzer_base, only: semantic_analyzer_t
    use semantic_context_types, only: semantic_context_base_t
    use ast_core, only: ast_arena_t
    implicit none
    private

    public :: source_location_t, source_context_t, source_strategy_t
    public :: exact_source_strategy_t, generated_source_strategy_t
    public :: node_registry_entry_t, node_registry_t, strategy_dispatcher_t
    public :: reconstruction_quality_t, source_map_t, source_reconstruction_result_t
    public :: source_reconstruction_analyzer_t
    public :: strategy_name_interface, reconstruct_interface

    ! Enhanced source mapping information with exact character ranges
    type :: source_location_t
        integer :: line = 0
        integer :: column = 0
        integer :: start_char = 0    ! Exact character position start
        integer :: end_char = 0      ! Exact character position end
    end type

    ! Source context for storing original source and line information
    type :: source_context_t
        character(:), allocatable :: original_source
        integer, allocatable :: line_starts(:)
        integer :: total_lines = 0
    contains
        procedure :: initialize_source
        procedure :: get_line_text => get_line_text_from_context
        procedure :: extract_range => extract_range_from_context
    end type

    ! Abstract base strategy for source reconstruction
    type, abstract :: source_strategy_t
    contains
        procedure(strategy_name_interface), deferred :: get_name
        procedure(reconstruct_interface), deferred :: reconstruct_node
    end type

    ! Exact source strategy - uses original source text when available
    type, extends(source_strategy_t) :: exact_source_strategy_t
    contains
        procedure :: get_name => get_exact_strategy_name
        procedure :: reconstruct_node => reconstruct_exact_source
    end type

    ! Generated source strategy - regenerates from AST nodes
    type, extends(source_strategy_t) :: generated_source_strategy_t
    contains
        procedure :: get_name => get_generated_strategy_name
        procedure :: reconstruct_node => reconstruct_generated_source
        procedure :: reconstruct_node_with_arena
    end type

    ! Node registry for strategy mapping
    type :: node_registry_entry_t
        character(:), allocatable :: node_type
        class(source_strategy_t), allocatable :: strategy
    end type

    type :: node_registry_t
        type(node_registry_entry_t), allocatable :: entries(:)
        integer :: count = 0
    contains
        procedure :: register_strategy
        procedure :: has_strategy
        procedure :: get_strategy
    end type

    ! Strategy dispatcher
    type :: strategy_dispatcher_t
        type(node_registry_t) :: registry
        type(exact_source_strategy_t) :: exact_strategy
        type(generated_source_strategy_t) :: generated_strategy
    contains
        procedure :: initialize_default_strategies
        procedure :: reconstruct_node => dispatch_reconstruct_node
    end type

    ! Reconstruction quality assessment
    type :: reconstruction_quality_t
        integer :: total_nodes = 0
        integer :: exact_matches = 0
        integer :: generated_fallbacks = 0
        integer :: failed_reconstructions = 0
    contains
        procedure :: initialize => initialize_quality
        procedure :: record_exact_match
        procedure :: record_generated_fallback
        procedure :: record_failed_reconstruction
        procedure :: get_accuracy
    end type

    ! Abstract interfaces
    abstract interface
        function strategy_name_interface(this) result(name)
            import :: source_strategy_t
            class(source_strategy_t), intent(in) :: this
            character(:), allocatable :: name
        end function

        function reconstruct_interface(this, context, location, node_index) &
                 result(source_text)
            import :: source_strategy_t, source_context_t, source_location_t
            class(source_strategy_t), intent(in) :: this
            type(source_context_t), intent(in) :: context
            type(source_location_t), intent(in) :: location
            integer, intent(in) :: node_index
            character(:), allocatable :: source_text
        end function
    end interface

    ! Source mapping table
    type :: source_map_t
        integer, allocatable :: node_indices(:)
        type(source_location_t), allocatable :: locations(:)
        integer :: entry_count = 0
    end type

    ! Source reconstruction result
    type, extends(semantic_result_base_t) :: source_reconstruction_result_t
        character(:), allocatable :: original_source
        type(source_map_t) :: node_map
        integer :: total_lines = 0
        character(:), allocatable :: line_starts(:)  ! Character positions
    contains
        procedure :: get_result_type => source_reconstruction_get_result_type
        procedure :: clone_result => source_reconstruction_clone_result
        procedure :: merge_results => source_reconstruction_merge_results
        procedure :: assign => source_reconstruction_result_assign
        generic :: assignment(=) => assign
    end type

    ! Source reconstruction analyzer plugin base type
    type, extends(semantic_analyzer_t) :: source_reconstruction_analyzer_t
        type(source_reconstruction_result_t) :: result
        logical :: analysis_complete = .false.
    contains
        procedure :: analyze => analyze_source_reconstruction_base
        procedure :: get_results => get_source_reconstruction_results_base
        procedure :: get_name => get_source_reconstruction_name_base
        procedure :: assign => assign_source_reconstruction_analyzer_base
        procedure :: get_dependencies => get_source_reconstruction_dependencies_base
        
        ! Analysis methods for fluff rules (base implementations)
        procedure :: get_node_source_text => get_node_source_text_base
        procedure :: extract_text_span => extract_text_span_base
        procedure :: get_line_text => get_line_text_base
        procedure :: get_context_around_node => get_context_around_node_base
        procedure :: format_source_location => format_source_location_base
    end type

contains

    ! Placeholder implementations - actual implementations will be in strategy modules
    subroutine initialize_source(this, source_text)
        class(source_context_t), intent(inout) :: this
        character(len=*), intent(in) :: source_text
        ! Implementation moved to source_reconstruction_strategies.f90
        this%original_source = source_text
    end subroutine initialize_source

    function get_line_text_from_context(this, line_number) result(line_text)
        class(source_context_t), intent(in) :: this
        integer, intent(in) :: line_number
        character(:), allocatable :: line_text
        ! Implementation moved to source_reconstruction_strategies.f90
        line_text = ""
    end function get_line_text_from_context

    function extract_range_from_context(this, start_pos, end_pos) result(text)
        class(source_context_t), intent(in) :: this
        integer, intent(in) :: start_pos, end_pos
        character(:), allocatable :: text
        ! Implementation moved to source_reconstruction_strategies.f90
        text = ""
    end function extract_range_from_context

    function get_exact_strategy_name(this) result(name)
        class(exact_source_strategy_t), intent(in) :: this
        character(:), allocatable :: name
        name = "exact_source"
    end function get_exact_strategy_name

    function reconstruct_exact_source(this, context, location, node_index) result(source_text)
        class(exact_source_strategy_t), intent(in) :: this
        type(source_context_t), intent(in) :: context
        type(source_location_t), intent(in) :: location
        integer, intent(in) :: node_index
        character(:), allocatable :: source_text
        ! Implementation moved to source_reconstruction_strategies.f90
        source_text = ""
    end function reconstruct_exact_source

    function get_generated_strategy_name(this) result(name)
        class(generated_source_strategy_t), intent(in) :: this
        character(:), allocatable :: name
        name = "generated_source"
    end function get_generated_strategy_name

    function reconstruct_generated_source(this, context, location, node_index) result(source_text)
        class(generated_source_strategy_t), intent(in) :: this
        type(source_context_t), intent(in) :: context
        type(source_location_t), intent(in) :: location
        integer, intent(in) :: node_index
        character(:), allocatable :: source_text
        ! Implementation moved to source_reconstruction_strategies.f90
        source_text = ""
    end function reconstruct_generated_source

    subroutine reconstruct_node_with_arena(this, context, location, arena, node_index, source_text)
        class(generated_source_strategy_t), intent(in) :: this
        type(source_context_t), intent(in) :: context
        type(source_location_t), intent(in) :: location
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(:), allocatable, intent(out) :: source_text
        ! Implementation moved to source_reconstruction_strategies.f90
        source_text = ""
    end subroutine reconstruct_node_with_arena

    ! Registry methods - placeholder implementations
    subroutine register_strategy(this, node_type, strategy)
        class(node_registry_t), intent(inout) :: this
        character(len=*), intent(in) :: node_type
        class(source_strategy_t), intent(in) :: strategy
        ! Implementation moved to source_reconstruction_strategies.f90
    end subroutine register_strategy

    function has_strategy(this, node_type) result(found)
        class(node_registry_t), intent(in) :: this
        character(len=*), intent(in) :: node_type
        logical :: found
        found = .false.
    end function has_strategy

    function get_strategy(this, node_type) result(strategy)
        class(node_registry_t), intent(in) :: this
        character(len=*), intent(in) :: node_type
        class(source_strategy_t), pointer :: strategy
        strategy => null()
    end function get_strategy

    ! Dispatcher methods - placeholder implementations
    subroutine initialize_default_strategies(this)
        class(strategy_dispatcher_t), intent(inout) :: this
        ! Implementation moved to source_reconstruction_strategies.f90
    end subroutine initialize_default_strategies

    function dispatch_reconstruct_node(this, context, location, node_index) result(source_text)
        class(strategy_dispatcher_t), intent(in) :: this
        type(source_context_t), intent(in) :: context
        type(source_location_t), intent(in) :: location
        integer, intent(in) :: node_index
        character(:), allocatable :: source_text
        source_text = ""
    end function dispatch_reconstruct_node

    ! Quality assessment methods - placeholder implementations
    subroutine initialize_quality(this)
        class(reconstruction_quality_t), intent(inout) :: this
        this%total_nodes = 0
        this%exact_matches = 0
        this%generated_fallbacks = 0
        this%failed_reconstructions = 0
    end subroutine initialize_quality

    subroutine record_exact_match(this)
        class(reconstruction_quality_t), intent(inout) :: this
        this%exact_matches = this%exact_matches + 1
        this%total_nodes = this%total_nodes + 1
    end subroutine record_exact_match

    subroutine record_generated_fallback(this)
        class(reconstruction_quality_t), intent(inout) :: this
        this%generated_fallbacks = this%generated_fallbacks + 1
        this%total_nodes = this%total_nodes + 1
    end subroutine record_generated_fallback

    subroutine record_failed_reconstruction(this)
        class(reconstruction_quality_t), intent(inout) :: this
        this%failed_reconstructions = this%failed_reconstructions + 1
        this%total_nodes = this%total_nodes + 1
    end subroutine record_failed_reconstruction

    function get_accuracy(this) result(accuracy)
        class(reconstruction_quality_t), intent(in) :: this
        real :: accuracy
        if (this%total_nodes > 0) then
            accuracy = real(this%exact_matches) / real(this%total_nodes)
        else
            accuracy = 0.0
        end if
    end function get_accuracy

    ! Result type methods - placeholder implementations
    function source_reconstruction_get_result_type(this) result(type_name)
        class(source_reconstruction_result_t), intent(in) :: this
        character(:), allocatable :: type_name
        type_name = "source_reconstruction"
    end function source_reconstruction_get_result_type

    function source_reconstruction_clone_result(this) result(clone)
        class(source_reconstruction_result_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: clone
        type(source_reconstruction_result_t), allocatable :: typed_clone
        allocate(typed_clone)
        typed_clone = this
        call move_alloc(typed_clone, clone)
    end function source_reconstruction_clone_result

    subroutine source_reconstruction_merge_results(this, other)
        class(source_reconstruction_result_t), intent(inout) :: this
        class(semantic_result_base_t), intent(in) :: other
        ! Placeholder implementation
    end subroutine source_reconstruction_merge_results

    subroutine source_reconstruction_result_assign(this, rhs)
        class(source_reconstruction_result_t), intent(inout) :: this
        type(source_reconstruction_result_t), intent(in) :: rhs
        this%original_source = rhs%original_source
        this%node_map = rhs%node_map
        this%total_lines = rhs%total_lines
        this%line_starts = rhs%line_starts
    end subroutine source_reconstruction_result_assign

    ! Base implementations for source_reconstruction_analyzer_t
    subroutine analyze_source_reconstruction_base(this, shared_context, arena, node_index)
        class(source_reconstruction_analyzer_t), intent(inout) :: this
        class(semantic_context_base_t), intent(in) :: shared_context
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        ! Base implementation - to be overridden
        associate(dummy => shared_context, dummy2 => arena, dummy3 => node_index)
        end associate
    end subroutine analyze_source_reconstruction_base

    function get_source_reconstruction_results_base(this) result(results)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: results
        ! Base implementation - to be overridden
        allocate(source_reconstruction_result_t :: results)
    end function get_source_reconstruction_results_base

    function get_source_reconstruction_name_base(this) result(name)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        character(:), allocatable :: name
        name = "source_reconstruction_analyzer_base"
    end function get_source_reconstruction_name_base

    subroutine assign_source_reconstruction_analyzer_base(lhs, rhs)
        class(source_reconstruction_analyzer_t), intent(out) :: lhs
        class(semantic_analyzer_t), intent(in) :: rhs
        ! Base implementation - to be overridden
        associate(dummy => lhs, dummy2 => rhs)
        end associate
    end subroutine assign_source_reconstruction_analyzer_base

    function get_source_reconstruction_dependencies_base(this) result(deps)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        character(:), allocatable :: deps(:)
        allocate(character(len=0) :: deps(0))
        associate(dummy => this)
        end associate
    end function get_source_reconstruction_dependencies_base

    ! Base analysis method implementations
    function get_node_source_text_base(this, node_index) result(text)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        integer, intent(in) :: node_index
        character(:), allocatable :: text
        text = ""
        associate(dummy => this, dummy2 => node_index)
        end associate
    end function get_node_source_text_base

    function extract_text_span_base(this, start_line, start_col, end_line, end_col) result(text)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        integer, intent(in) :: start_line, start_col, end_line, end_col
        character(:), allocatable :: text
        text = ""
        associate(dummy => this, dummy2 => start_line, dummy3 => start_col, &
                  dummy4 => end_line, dummy5 => end_col)
        end associate
    end function extract_text_span_base

    function get_line_text_base(this, line_number) result(line_text)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        integer, intent(in) :: line_number
        character(:), allocatable :: line_text
        line_text = ""
        associate(dummy => this, dummy2 => line_number)
        end associate
    end function get_line_text_base

    function get_context_around_node_base(this, node_index, context_lines) result(context)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        integer, intent(in) :: node_index, context_lines
        character(:), allocatable :: context
        context = ""
        associate(dummy => this, dummy2 => node_index, dummy3 => context_lines)
        end associate
    end function get_context_around_node_base

    function format_source_location_base(this, node_index) result(location_str)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        integer, intent(in) :: node_index
        character(:), allocatable :: location_str
        location_str = "unknown"
        associate(dummy => this, dummy2 => node_index)
        end associate
    end function format_source_location_base

end module source_reconstruction_types