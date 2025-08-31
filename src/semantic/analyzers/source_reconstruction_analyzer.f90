module source_reconstruction_analyzer
    ! Source reconstruction analyzer - refactored for architectural compliance
    ! Uses extracted types from source_reconstruction_types.f90 (Issue #1067)
    use semantic_analyzer_base, only: semantic_analyzer_t
    use semantic_context_types, only: semantic_context_base_t
    use semantic_result_types, only: semantic_result_base_t
    use source_reconstruction_types, only: source_reconstruction_analyzer_t, source_location_t, &
                                           source_context_t, exact_source_strategy_t, &
                                           generated_source_strategy_t, strategy_dispatcher_t, &
                                           node_registry_t, reconstruction_quality_t, &
                                           source_map_t, source_reconstruction_result_t
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

    ! Source reconstruction analyzer plugin - extended from types module
    type, extends(source_reconstruction_analyzer_t) :: source_reconstruction_analyzer_impl_t
    contains
        ! Override base methods with full implementations
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
        class(source_reconstruction_analyzer_impl_t), intent(inout) :: this
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
        class(source_reconstruction_analyzer_impl_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: results
        
        ! Return the source reconstruction result
        allocate(source_reconstruction_result_t :: results)
        select type(results)
        type is (source_reconstruction_result_t)
            results = this%result
        end select
    end function

    function get_source_reconstruction_name(this) result(name)
        class(source_reconstruction_analyzer_impl_t), intent(in) :: this
        character(:), allocatable :: name
        
        name = "source_reconstruction_analyzer"
    end function

    subroutine assign_source_reconstruction_analyzer(lhs, rhs)
        use semantic_analyzer_base, only: semantic_analyzer_t
        class(source_reconstruction_analyzer_impl_t), intent(out) :: lhs
        class(semantic_analyzer_t), intent(in) :: rhs
        
        select type(rhs)
        type is (source_reconstruction_analyzer_impl_t)
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
        class(source_reconstruction_analyzer_impl_t), intent(in) :: this
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
        class(source_reconstruction_analyzer_impl_t), intent(in) :: this
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
        class(source_reconstruction_analyzer_impl_t), intent(in) :: this
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
        class(source_reconstruction_analyzer_impl_t), intent(in) :: this
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
        class(source_reconstruction_analyzer_impl_t), intent(in) :: this
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

    function get_source_reconstruction_dependencies(this) result(deps)
        class(source_reconstruction_analyzer_impl_t), intent(in) :: this
        character(:), allocatable :: deps(:)
        
        ! Source reconstruction analyzer has no dependencies
        allocate(character(len=0) :: deps(0))
        
        associate(dummy => this)
        end associate
    end function

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
        class(source_reconstruction_analyzer_impl_t), intent(in) :: this
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

end module source_reconstruction_analyzer