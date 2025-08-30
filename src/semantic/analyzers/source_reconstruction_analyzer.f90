module source_reconstruction_analyzer
    use semantic_analyzer_base, only: semantic_analyzer_t
    use semantic_context_types, only: semantic_context_base_t
    use semantic_result_types, only: semantic_result_base_t
    use ast_core, only: ast_arena_t, ast_entry_t, identifier_node, literal_node, &
                        program_node, assignment_node, if_node, do_loop_node, &
                        function_def_node, declaration_node
    use iso_fortran_env, only: error_unit
    implicit none
    private

    public :: source_reconstruction_analyzer_t, source_location_t, &
              source_context_t, exact_source_strategy_t, &
              generated_source_strategy_t, strategy_dispatcher_t, &
              node_registry_t, reconstruction_quality_t

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

    ! Source reconstruction analyzer plugin
    type, extends(semantic_analyzer_t) :: source_reconstruction_analyzer_t
        type(source_reconstruction_result_t) :: result
        logical :: analysis_complete = .false.
    contains
        procedure :: analyze => analyze_source_reconstruction
        procedure :: get_results => get_source_reconstruction_results
        procedure :: get_name => get_source_reconstruction_name
        procedure :: assign => assign_source_reconstruction_analyzer
        procedure :: get_dependencies => get_source_reconstruction_dependencies
        
        ! Analysis methods for fluff rules
        procedure :: get_node_source_text
        procedure :: extract_text_span
        procedure :: get_line_text
        procedure :: get_context_around_node
        procedure :: format_source_location
    end type

contains

    subroutine analyze_source_reconstruction(this, shared_context, arena, node_index)
        class(source_reconstruction_analyzer_t), intent(inout) :: this
        class(semantic_context_base_t), intent(in) :: shared_context
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! Build comprehensive mapping from AST nodes to source locations
        call build_source_mapping(this%result, arena, node_index)
        this%analysis_complete = .true.
        
        associate(dummy => shared_context)
        end associate
    end subroutine

    function get_source_reconstruction_results(this) result(results)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: results
        
        ! Return the source reconstruction result
        allocate(source_reconstruction_result_t :: results)
        select type(results)
        type is (source_reconstruction_result_t)
            results = this%result
        end select
    end function

    function get_source_reconstruction_name(this) result(name)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        character(:), allocatable :: name
        
        name = "source_reconstruction_analyzer"
    end function

    subroutine assign_source_reconstruction_analyzer(lhs, rhs)
        use semantic_analyzer_base, only: semantic_analyzer_t
        class(source_reconstruction_analyzer_t), intent(out) :: lhs
        class(semantic_analyzer_t), intent(in) :: rhs
        
        select type(rhs)
        type is (source_reconstruction_analyzer_t)
            ! Deep copy the result
            lhs%result = rhs%result
            lhs%analysis_complete = rhs%analysis_complete
        class default
            write(error_unit, '(A)') "ERROR [source_reconstruction_analyzer]: Type mismatch " &
                // "in source_reconstruction_analyzer assignment - assignment ignored"
            ! Don't perform assignment on type mismatch
        end select
    end subroutine

    ! Analysis methods for fluff rules
    function get_node_source_text(this, node_index) result(text)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        integer, intent(in) :: node_index
        character(:), allocatable :: text
        
        integer :: i
        
        if (.not. this%analysis_complete) then
            text = ""
            return
        end if
        
        ! Find the node in our mapping
        do i = 1, this%result%node_map%entry_count
            if (this%result%node_map%node_indices(i) == node_index) then
                if (allocated(this%result%original_source)) then
                    text = extract_substring(this%result%original_source, &
                                           this%result%node_map%locations(i))
                    return
                end if
            end if
        end do
        
        ! Not found
        text = "<source not available>"
    end function

    function extract_text_span(this, start_line, start_col, end_line, &
                                end_col) result(text)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        integer, intent(in) :: start_line, start_col, end_line, end_col
        character(:), allocatable :: text
        
        type(source_context_t) :: context
        integer :: start_char, end_char, i
        character(:), allocatable :: current_line
        
        if (.not. this%analysis_complete .or. &
            .not. allocated(this%result%original_source)) then
            text = ""
            return
        end if
        
        ! Validate line numbers
        if (start_line <= 0 .or. end_line <= 0 .or. &
            start_line > this%result%total_lines .or. &
            end_line > this%result%total_lines .or. &
            start_line > end_line) then
            text = ""
            return
        end if
        
        ! Initialize context and calculate character positions
        call context%initialize_source(this%result%original_source)
        
        ! Calculate start character position
        if (start_line <= context%total_lines .and. start_col > 0) then
            start_char = context%line_starts(start_line) + start_col - 1
        else
            text = ""
            return
        end if
        
        ! Calculate end character position
        if (end_line <= context%total_lines) then
            if (end_line == context%total_lines) then
                ! Last line case
                end_char = min(len(context%original_source), &
                              context%line_starts(end_line) + end_col - 1)
            else
                ! Regular line case
                current_line = context%get_line_text(end_line)
                end_char = context%line_starts(end_line) + &
                          min(end_col - 1, len(current_line))
            end if
        else
            text = ""
            return
        end if
        
        ! Extract the text span with bounds checking
        if (start_char >= 1 .and. end_char >= start_char .and. &
            end_char <= len(context%original_source)) then
            text = context%original_source(start_char:end_char)
        else
            text = ""
        end if
    end function

    function get_line_text(this, line_number) result(line_text)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        integer, intent(in) :: line_number
        character(:), allocatable :: line_text
        
        type(source_context_t) :: context
        
        if (.not. this%analysis_complete .or. &
            .not. allocated(this%result%original_source) .or. &
            line_number <= 0 .or. line_number > this%result%total_lines) then
            line_text = ""
            return
        end if
        
        ! Initialize context and extract the line
        call context%initialize_source(this%result%original_source)
        line_text = context%get_line_text(line_number)
    end function

    function get_context_around_node(this, node_index, context_lines) result(context)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        integer, intent(in) :: node_index
        integer, intent(in) :: context_lines
        character(:), allocatable :: context
        
        integer :: i, node_line
        
        if (.not. this%analysis_complete) then
            context = ""
            return
        end if
        
        ! Find node's line number
        node_line = 0
        do i = 1, this%result%node_map%entry_count
            if (this%result%node_map%node_indices(i) == node_index) then
                node_line = this%result%node_map%locations(i)%line
                exit
            end if
        end do
        
        if (node_line == 0) then
            context = "<context not available>"
            return
        end if
        
        ! Build context with lines around the node
        context = build_context_string(this, node_line, context_lines)
    end function

    function format_source_location(this, node_index) result(location_str)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        integer, intent(in) :: node_index
        character(:), allocatable :: location_str
        
        integer :: i
        
        if (.not. this%analysis_complete) then
            location_str = "unknown"
            return
        end if
        
        ! Find node location
        do i = 1, this%result%node_map%entry_count
            if (this%result%node_map%node_indices(i) == node_index) then
                ! Build location string safely
                location_str = int_to_str(this%result%node_map%locations(i)%line) &
                    // ':' // int_to_str(this%result%node_map%locations(i)%column)
                return
            end if
        end do
        
        location_str = "unknown"
    end function

    ! Source context procedures
    subroutine initialize_source(this, source_text)
        class(source_context_t), intent(inout) :: this
        character(*), intent(in) :: source_text
        
        integer :: i, line_count, pos
        
        ! Store the original source
        this%original_source = source_text
        
        ! Count lines
        line_count = 1
        do i = 1, len(source_text)
            if (source_text(i:i) == new_line('a')) then
                line_count = line_count + 1
            end if
        end do
        this%total_lines = line_count
        
        ! Build line starts array
        allocate(this%line_starts(line_count))
        line_count = 1
        this%line_starts(1) = 1
        
        do i = 1, len(source_text)
            if (source_text(i:i) == new_line('a')) then
                line_count = line_count + 1
                if (line_count <= size(this%line_starts)) then
                    this%line_starts(line_count) = i + 1
                end if
            end if
        end do
    end subroutine initialize_source

    function get_line_text_from_context(this, line_number) result(line_text)
        class(source_context_t), intent(in) :: this
        integer, intent(in) :: line_number
        character(:), allocatable :: line_text
        
        integer :: start_pos, end_pos
        
        if (.not. allocated(this%original_source) .or. &
            line_number <= 0 .or. line_number > this%total_lines) then
            line_text = ""
            return
        end if
        
        start_pos = this%line_starts(line_number)
        
        if (line_number == this%total_lines) then
            ! Last line - go to end of source
            end_pos = len(this%original_source)
        else
            ! Find end of line (before newline)
            end_pos = this%line_starts(line_number + 1) - 2
        end if
        
        if (end_pos >= start_pos) then
            line_text = this%original_source(start_pos:end_pos)
        else
            line_text = ""
        end if
    end function get_line_text_from_context

    function extract_range_from_context(this, location) result(extracted)
        class(source_context_t), intent(in) :: this
        type(source_location_t), intent(in) :: location
        character(:), allocatable :: extracted
        
        if (.not. allocated(this%original_source) .or. &
            location%start_char <= 0 .or. location%end_char <= 0 .or. &
            location%start_char > location%end_char .or. &
            location%end_char > len(this%original_source)) then
            extracted = ""
            return
        end if
        
        extracted = this%original_source(location%start_char:location%end_char)
    end function extract_range_from_context

    ! Strategy implementations
    function get_exact_strategy_name(this) result(name)
        class(exact_source_strategy_t), intent(in) :: this
        character(:), allocatable :: name
        
        name = "exact_source"
        
        associate(dummy => this)
        end associate
    end function get_exact_strategy_name

    function reconstruct_exact_source(this, context, location, node_index) &
             result(source_text)
        class(exact_source_strategy_t), intent(in) :: this
        type(source_context_t), intent(in) :: context
        type(source_location_t), intent(in) :: location
        integer, intent(in) :: node_index
        character(:), allocatable :: source_text
        
        ! Use exact source if available
        if (allocated(context%original_source)) then
            source_text = context%extract_range(location)
        else
            source_text = ""
        end if
        
        associate(dummy => this, dummy2 => node_index)
        end associate
    end function reconstruct_exact_source

    function get_generated_strategy_name(this) result(name)
        class(generated_source_strategy_t), intent(in) :: this
        character(:), allocatable :: name
        
        name = "generated_source"
        
        associate(dummy => this)
        end associate
    end function get_generated_strategy_name

    function reconstruct_generated_source(this, context, location, node_index) &
             result(source_text)
        class(generated_source_strategy_t), intent(in) :: this
        type(source_context_t), intent(in) :: context
        type(source_location_t), intent(in) :: location
        integer, intent(in) :: node_index
        character(:), allocatable :: source_text
        
        ! Basic fallback - needs AST arena for full implementation
        source_text = "<generated>"
        
        associate(dummy => this, dummy2 => context, dummy3 => location, &
                  dummy4 => node_index)
        end associate
    end function reconstruct_generated_source

    function reconstruct_node_with_arena(this, context, location, &
                                        node_index, arena) result(source_text)
        class(generated_source_strategy_t), intent(in) :: this
        type(source_context_t), intent(in) :: context
        type(source_location_t), intent(in) :: location
        integer, intent(in) :: node_index
        type(ast_arena_t), intent(in) :: arena
        character(:), allocatable :: source_text
        
        ! Generate source from AST node
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                select type(node => arena%entries(node_index)%node)
                type is (identifier_node)
                    source_text = node%name
                type is (literal_node)
                    source_text = node%value
                type is (program_node)
                    source_text = "program " // node%name
                type is (assignment_node)
                    source_text = "<assignment>"
                type is (if_node)
                    source_text = "if (<condition>) then"
                type is (do_loop_node)
                    source_text = "do " // node%var_name // " = <start>, <end>"
                type is (function_def_node)
                    source_text = "function " // node%name // "()"
                type is (declaration_node)
                    source_text = node%type_name // " :: " // node%var_name
                class default
                    source_text = "<unknown_node>"
                end select
            else
                source_text = "<invalid_node>"
            end if
        else
            source_text = "<out_of_bounds>"
        end if
        
        associate(dummy => this, dummy2 => context, dummy3 => location)
        end associate
    end function reconstruct_node_with_arena

    ! Node registry procedures
    subroutine register_strategy(this, node_type, strategy)
        class(node_registry_t), intent(inout) :: this
        character(*), intent(in) :: node_type
        class(source_strategy_t), intent(in) :: strategy
        
        integer :: new_size
        type(node_registry_entry_t), allocatable :: temp(:)
        
        ! Expand registry if needed
        if (.not. allocated(this%entries)) then
            allocate(this%entries(10))
        end if
        
        if (this%count >= size(this%entries)) then
            new_size = size(this%entries) * 2
            allocate(temp(new_size))
            temp(1:this%count) = this%entries(1:this%count)
            call move_alloc(temp, this%entries)
        end if
        
        ! Add new entry
        this%count = this%count + 1
        this%entries(this%count)%node_type = node_type
        allocate(this%entries(this%count)%strategy, source=strategy)
    end subroutine register_strategy

    function has_strategy(this, node_type) result(found)
        class(node_registry_t), intent(in) :: this
        character(*), intent(in) :: node_type
        logical :: found
        
        integer :: i
        
        found = .false.
        do i = 1, this%count
            if (allocated(this%entries(i)%node_type)) then
                if (this%entries(i)%node_type == node_type) then
                    found = .true.
                    return
                end if
            end if
        end do
    end function has_strategy

    function get_strategy(this, node_type) result(strategy)
        class(node_registry_t), intent(in) :: this
        character(*), intent(in) :: node_type
        class(source_strategy_t), allocatable :: strategy
        
        integer :: i
        
        do i = 1, this%count
            if (allocated(this%entries(i)%node_type)) then
                if (this%entries(i)%node_type == node_type) then
                    if (allocated(this%entries(i)%strategy)) then
                        allocate(strategy, source=this%entries(i)%strategy)
                        return
                    end if
                end if
            end if
        end do
        
        ! Strategy not found
    end function get_strategy

    ! Strategy dispatcher procedures
    subroutine initialize_default_strategies(this)
        class(strategy_dispatcher_t), intent(inout) :: this
        
        ! Register default strategies for common node types
        call this%registry%register_strategy("identifier", this%generated_strategy)
        call this%registry%register_strategy("literal", this%generated_strategy)
        call this%registry%register_strategy("program", this%generated_strategy)
        call this%registry%register_strategy("assignment", this%generated_strategy)
        call this%registry%register_strategy("if", this%generated_strategy)
        call this%registry%register_strategy("do_loop", this%generated_strategy)
        call this%registry%register_strategy("function_def", this%generated_strategy)
        call this%registry%register_strategy("declaration", this%generated_strategy)
    end subroutine initialize_default_strategies

    function dispatch_reconstruct_node(this, context, arena, node_index) &
             result(source_text)
        class(strategy_dispatcher_t), intent(in) :: this
        type(source_context_t), intent(in) :: context
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(:), allocatable :: source_text
        
        character(:), allocatable :: node_type
        class(source_strategy_t), allocatable :: strategy
        type(source_location_t) :: location
        
        if (node_index > 0 .and. node_index <= arena%size) then
            if (allocated(arena%entries(node_index)%node)) then
                node_type = arena%entries(node_index)%node_type
                
                if (this%registry%has_strategy(node_type)) then
                    strategy = this%registry%get_strategy(node_type)
                    
                    ! Create location from node info
                    location%line = arena%entries(node_index)%node%line
                    location%column = arena%entries(node_index)%node%column
                    location%start_char = 0
                    location%end_char = 0
                    
                    if (allocated(strategy)) then
                        select type(strategy)
                        type is (generated_source_strategy_t)
                            source_text = strategy%reconstruct_node_with_arena( &
                                context, location, node_index, arena)
                        class default
                            source_text = strategy%reconstruct_node( &
                                context, location, node_index)
                        end select
                    else
                        source_text = "<no_strategy>"
                    end if
                else
                    source_text = "<unknown_node_type:" // node_type // ">"
                end if
            else
                source_text = "<unallocated_node>"
            end if
        else
            source_text = "<invalid_index>"
        end if
    end function dispatch_reconstruct_node

    ! Quality assessment procedures
    subroutine initialize_quality(this)
        class(reconstruction_quality_t), intent(inout) :: this
        
        this%total_nodes = 0
        this%exact_matches = 0
        this%generated_fallbacks = 0
        this%failed_reconstructions = 0
    end subroutine initialize_quality

    subroutine record_exact_match(this)
        class(reconstruction_quality_t), intent(inout) :: this
        
        this%total_nodes = this%total_nodes + 1
        this%exact_matches = this%exact_matches + 1
    end subroutine record_exact_match

    subroutine record_generated_fallback(this)
        class(reconstruction_quality_t), intent(inout) :: this
        
        this%total_nodes = this%total_nodes + 1
        this%generated_fallbacks = this%generated_fallbacks + 1
    end subroutine record_generated_fallback

    subroutine record_failed_reconstruction(this)
        class(reconstruction_quality_t), intent(inout) :: this
        
        this%total_nodes = this%total_nodes + 1
        this%failed_reconstructions = this%failed_reconstructions + 1
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

    ! Helper subroutines
    subroutine build_source_mapping(result, arena, root_index)
        type(source_reconstruction_result_t), intent(inout) :: result
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        
        integer :: i, valid_nodes
        type(source_context_t) :: context
        character(:), allocatable :: node_text
        integer :: estimated_start, estimated_end
        
        ! Count nodes with location information
        valid_nodes = 0
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                if (arena%entries(i)%node%line > 0) then
                    valid_nodes = valid_nodes + 1
                end if
            end if
        end do
        
        if (valid_nodes == 0) then
            result%node_map%entry_count = 0
            return
        end if
        
        ! Create synthetic source text if not available
        if (.not. allocated(result%original_source)) then
            result%original_source = reconstruct_source_from_arena(arena)
        end if
        
        ! Initialize context for position calculations
        call context%initialize_source(result%original_source)
        
        ! Allocate mapping arrays
        allocate(result%node_map%node_indices(valid_nodes))
        allocate(result%node_map%locations(valid_nodes))
        
        ! Fill mapping with real character positions
        valid_nodes = 0
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                if (arena%entries(i)%node%line > 0) then
                    valid_nodes = valid_nodes + 1
                    result%node_map%node_indices(valid_nodes) = i
                    result%node_map%locations(valid_nodes)%line = &
                        arena%entries(i)%node%line
                    result%node_map%locations(valid_nodes)%column = &
                        arena%entries(i)%node%column
                    
                    ! Calculate character positions from line/column
                    call calculate_char_positions( &
                        context, &
                        arena%entries(i)%node%line, &
                        arena%entries(i)%node%column, &
                        arena%entries(i), &
                        estimated_start, estimated_end)
                    
                    result%node_map%locations(valid_nodes)%start_char = &
                        estimated_start
                    result%node_map%locations(valid_nodes)%end_char = &
                        estimated_end
                end if
            end if
        end do
        
        result%node_map%entry_count = valid_nodes
        result%total_lines = context%total_lines
        
        associate(dummy => root_index)
        end associate
    end subroutine

    function extract_substring(source, location) result(substring)
        character(*), intent(in) :: source
        type(source_location_t), intent(in) :: location
        character(:), allocatable :: substring
        
        ! Validate character bounds and extract substring
        if (location%start_char > 0 .and. &
            location%end_char > location%start_char .and. &
            location%end_char <= len(source)) then
            substring = source(location%start_char:location%end_char)
        else
            substring = "<invalid range>"
        end if
    end function

    function build_context_string(this, center_line, context_lines) result(context)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        integer, intent(in) :: center_line, context_lines
        character(:), allocatable :: context
        
        integer :: start_line, end_line, i
        character(:), allocatable :: line_text
        
        start_line = max(1, center_line - context_lines)
        end_line = min(this%result%total_lines, center_line + context_lines)
        
        context = ""
        do i = start_line, end_line
            line_text = this%get_line_text(i)
            if (i == center_line) then
                context = context // ">>> " // line_text // new_line('a')
            else
                context = context // "    " // line_text // new_line('a')
            end if
        end do
    end function

    function count_lines_in_arena(arena) result(line_count)
        type(ast_arena_t), intent(in) :: arena
        integer :: line_count
        
        integer :: i, max_line
        
        max_line = 0
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                max_line = max(max_line, arena%entries(i)%node%line)
            end if
        end do
        
        line_count = max_line
    end function

    function get_source_reconstruction_dependencies(this) result(deps)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        character(:), allocatable :: deps(:)
        
        ! Source reconstruction analyzer has no dependencies
        allocate(character(len=0) :: deps(0))
        
        associate(dummy => this)
        end associate
    end function

    ! Additional helper functions for real source mapping
    function reconstruct_source_from_arena(arena) result(source_text)
        type(ast_arena_t), intent(in) :: arena
        character(:), allocatable :: source_text
        
        integer :: i, line_num, max_lines
        character(:), allocatable :: lines(:)
        character(len=500) :: temp_line
        
        ! Find maximum line number
        max_lines = count_lines_in_arena(arena)
        
        if (max_lines == 0) then
            source_text = ""
            return
        end if
        
        allocate(character(len=500) :: lines(max_lines))
        
        ! Initialize all lines
        do i = 1, max_lines
            lines(i) = ""
        end do
        
        ! Reconstruct lines from AST nodes
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                line_num = arena%entries(i)%node%line
                if (line_num > 0 .and. line_num <= max_lines) then
                    call append_node_to_line(arena%entries(i), lines(line_num))
                end if
            end if
        end do
        
        ! Combine lines into source text
        source_text = ""
        do i = 1, max_lines
            if (len_trim(lines(i)) > 0) then
                source_text = source_text // trim(lines(i)) // new_line('a')
            else
                source_text = source_text // new_line('a')
            end if
        end do
    end function reconstruct_source_from_arena

    subroutine append_node_to_line(entry, line)
        use ast_core, only: ast_entry_t, identifier_node, literal_node
        type(ast_entry_t), intent(in) :: entry
        character(*), intent(inout) :: line
        
        character(:), allocatable :: node_text
        
        if (.not. allocated(entry%node)) return
        
        select type(node => entry%node)
        type is (identifier_node)
            node_text = node%name
        type is (literal_node)
            node_text = node%value
        class default
            node_text = "<" // entry%node_type // ">"
        end select
        
        if (len_trim(line) == 0) then
            line = node_text
        else
            line = trim(line) // " " // node_text
        end if
    end subroutine append_node_to_line

    subroutine calculate_char_positions(context, line_num, column_num, &
                                       entry, start_pos, end_pos)
        use ast_core, only: ast_entry_t, identifier_node, literal_node
        type(source_context_t), intent(in) :: context
        integer, intent(in) :: line_num, column_num
        type(ast_entry_t), intent(in) :: entry
        integer, intent(out) :: start_pos, end_pos
        
        integer :: node_length
        
        ! Calculate start position from line/column
        if (line_num > 0 .and. line_num <= context%total_lines .and. &
            column_num > 0) then
            start_pos = context%line_starts(line_num) + column_num - 1
        else
            start_pos = 1
        end if
        
        ! Estimate node length
        node_length = estimate_node_length(entry)
        end_pos = start_pos + node_length - 1
        
        ! Bounds checking
        if (start_pos < 1) start_pos = 1
        if (end_pos > len(context%original_source)) then
            end_pos = len(context%original_source)
        end if
        if (end_pos < start_pos) end_pos = start_pos
    end subroutine calculate_char_positions

    function estimate_node_length(entry) result(length)
        use ast_core, only: ast_entry_t, identifier_node, literal_node, &
                            program_node, assignment_node, if_node, &
                            do_loop_node, function_def_node, declaration_node
        type(ast_entry_t), intent(in) :: entry
        integer :: length
        
        if (.not. allocated(entry%node)) then
            length = 1
            return
        end if
        
        select type(node => entry%node)
        type is (identifier_node)
            length = len(node%name)
        type is (literal_node)
            length = len(node%value)
        type is (program_node)
            length = len("program ") + len(node%name)
        type is (function_def_node)
            length = len("function ") + len(node%name) + 2
        type is (declaration_node)
            length = len(node%type_name) + len(" :: ") + len(node%var_name)
        type is (assignment_node)
            length = 10  ! Approximate for assignment operator
        type is (if_node)
            length = 20  ! Approximate for if construct
        type is (do_loop_node)
            length = len("do ") + len(node%var_name) + 20
        class default
            length = len(entry%node_type) + 2
        end select
        
        if (length <= 0) length = 1
    end function estimate_node_length

    function int_to_str(value) result(str)
        integer, intent(in) :: value
        character(:), allocatable :: str
        
        character(len=20) :: temp_str
        
        write(temp_str, '(I0)') value
        str = trim(temp_str)
    end function int_to_str

    ! Source reconstruction result implementations
    function source_reconstruction_get_result_type(this) result(type_name)
        class(source_reconstruction_result_t), intent(in) :: this
        character(:), allocatable :: type_name
        type_name = "source_reconstruction_result"
    end function source_reconstruction_get_result_type

    function source_reconstruction_clone_result(this) result(cloned)
        class(source_reconstruction_result_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: cloned
        type(source_reconstruction_result_t) :: temp_result
        
        temp_result = this
        allocate(cloned, source=temp_result)
    end function source_reconstruction_clone_result

    subroutine source_reconstruction_merge_results(this, other)
        class(source_reconstruction_result_t), intent(inout) :: this
        class(semantic_result_base_t), intent(in) :: other
        
        select type (other_result => other)
        type is (source_reconstruction_result_t)
            ! Merge source reconstruction results
            this%total_lines = this%total_lines + other_result%total_lines
        end select
    end subroutine source_reconstruction_merge_results

    subroutine source_reconstruction_result_assign(lhs, rhs)
        class(source_reconstruction_result_t), intent(out) :: lhs
        type(source_reconstruction_result_t), intent(in) :: rhs
        
        lhs%result_id = rhs%result_id
        lhs%result_type_name = rhs%result_type_name
        lhs%has_errors = rhs%has_errors
        lhs%has_warnings = rhs%has_warnings
        lhs%summary = rhs%summary
        lhs%original_source = rhs%original_source
        lhs%node_map = rhs%node_map
        lhs%total_lines = rhs%total_lines
        
        ! Handle deferred-length allocatable character array
        if (allocated(rhs%line_starts)) then
            allocate(lhs%line_starts(size(rhs%line_starts)), source=rhs%line_starts)
        end if
    end subroutine source_reconstruction_result_assign

end module source_reconstruction_analyzer