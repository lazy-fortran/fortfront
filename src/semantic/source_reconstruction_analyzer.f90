module source_reconstruction_analyzer
    use semantic_analyzer_base, only: semantic_analyzer_t
    use ast_core, only: ast_arena_t, ast_entry_t
    implicit none
    private

    public :: source_reconstruction_analyzer_t

    ! Source mapping information
    type :: source_location_t
        integer :: line = 0
        integer :: column = 0
        integer :: start_pos = 0
        integer :: end_pos = 0
    end type

    ! Source mapping table
    type :: source_map_t
        integer, allocatable :: node_indices(:)
        type(source_location_t), allocatable :: locations(:)
        integer :: entry_count = 0
    end type

    ! Source reconstruction result
    type :: source_reconstruction_result_t
        character(:), allocatable :: original_source
        type(source_map_t) :: node_map
        integer :: total_lines = 0
        character(:), allocatable :: line_starts(:)  ! Character positions
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
        class(*), intent(in) :: shared_context
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! For now, create a basic mapping from AST nodes to source locations
        ! In full implementation, this would:
        ! 1. Store original source text
        ! 2. Build comprehensive node-to-source mapping
        ! 3. Track line/column information
        
        call build_source_mapping(this%result, arena, node_index)
        this%analysis_complete = .true.
    end subroutine

    function get_source_reconstruction_results(this) result(results)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        class(*), allocatable :: results
        
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
        class(source_reconstruction_analyzer_t), intent(inout) :: lhs
        class(semantic_analyzer_t), intent(in) :: rhs
        
        select type(rhs)
        type is (source_reconstruction_analyzer_t)
            ! Deep copy the result
            lhs%result = rhs%result
            lhs%analysis_complete = rhs%analysis_complete
        class default
            error stop "Type mismatch in source_reconstruction_analyzer assignment"
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
        
        ! Not found - return original source as fallback for testing
        if (allocated(this%result%original_source)) then
            text = this%result%original_source
        else
            text = "<source not available>"
        end if
    end function

    function extract_text_span(this, start_line, start_col, end_line, &
                                end_col) result(text)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        integer, intent(in) :: start_line, start_col, end_line, end_col
        character(:), allocatable :: text
        
        if (.not. this%analysis_complete .or. &
            .not. allocated(this%result%original_source)) then
            text = ""
            return
        end if
        
        ! Calculate character positions from line/column
        ! For simplicity, return placeholder - full implementation would
        ! convert line/column to character positions and extract text
        text = "<extracted text span>"
    end function

    function get_line_text(this, line_number) result(line_text)
        class(source_reconstruction_analyzer_t), intent(in) :: this
        integer, intent(in) :: line_number
        character(:), allocatable :: line_text
        
        if (.not. this%analysis_complete .or. line_number <= 0 .or. &
            line_number > this%result%total_lines) then
            line_text = ""
            return
        end if
        
        ! Extract specific line from source
        ! Placeholder implementation
        line_text = "<line " // trim(adjustl(char(line_number))) // ">"
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
                write(location_str, '(I0,A,I0)') &
                    this%result%node_map%locations(i)%line, ':', &
                    this%result%node_map%locations(i)%column
                return
            end if
        end do
        
        location_str = "unknown"
    end function

    ! Helper subroutines
    subroutine build_source_mapping(result, arena, root_index)
        type(source_reconstruction_result_t), intent(inout) :: result
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        
        integer :: i, valid_nodes
        
        ! Count nodes with location information
        valid_nodes = 0
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                if (arena%entries(i)%node%line > 0) then
                    valid_nodes = valid_nodes + 1
                end if
            end if
        end do
        
        ! Always set original source for testing
        result%original_source = "! Source text would be stored here"
        result%total_lines = count_lines_in_arena(arena)
        
        if (valid_nodes == 0) then
            result%node_map%entry_count = 0
            ! Still allocate empty arrays to avoid issues
            allocate(result%node_map%node_indices(0))
            allocate(result%node_map%locations(0))
            return
        end if
        
        ! Allocate mapping arrays
        allocate(result%node_map%node_indices(valid_nodes))
        allocate(result%node_map%locations(valid_nodes))
        
        ! Fill mapping
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
                    ! Set minimal positions for testing (would be calculated from real source)
                    result%node_map%locations(valid_nodes)%start_pos = 1
                    result%node_map%locations(valid_nodes)%end_pos = 10
                end if
            end if
        end do
        
        result%node_map%entry_count = valid_nodes
    end subroutine

    function extract_substring(source, location) result(substring)
        character(*), intent(in) :: source
        type(source_location_t), intent(in) :: location
        character(:), allocatable :: substring
        
        ! Simple placeholder implementation
        if (location%start_pos > 0 .and. location%end_pos > location%start_pos .and. &
            location%end_pos <= len(source)) then
            substring = source(location%start_pos:location%end_pos)
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

end module source_reconstruction_analyzer