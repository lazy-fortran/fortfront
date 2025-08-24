module call_graph_module
    use iso_fortran_env, only: error_unit
    ! Explicit AST imports replace ast_core god module
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node, call_or_subscript_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
                                   subroutine_call_node
    implicit none
    private

    ! Public interface
    public :: call_graph_t, procedure_info_t, call_edge_t, create_call_graph
    public :: add_procedure, add_call, find_unused_procedures
    public :: get_callers, get_callees, is_procedure_used
    public :: get_all_procedures, get_call_count, find_recursive_cycles
    public :: print_call_graph, build_call_graph_from_ast

    ! Type to represent a procedure in the call graph
    type :: procedure_info_t
        character(len=:), allocatable :: name
        integer :: definition_node      ! AST node where defined
        integer :: line
        integer :: column
        logical :: is_main_program
        logical :: is_intrinsic
        logical :: is_external
    end type procedure_info_t

    ! Type to represent a call edge in the graph
    type :: call_edge_t
        character(len=:), allocatable :: caller
        character(len=:), allocatable :: callee
        integer :: call_site_node      ! AST node of the call
        integer :: line
        integer :: column
    end type call_edge_t

    ! Main call graph type
    type :: call_graph_t
        type(procedure_info_t), allocatable :: procedures(:)
        type(call_edge_t), allocatable :: calls(:)
        integer :: proc_count = 0
        integer :: call_count = 0
        integer :: proc_capacity = 0
        integer :: call_capacity = 0
    contains
        procedure :: add_proc => graph_add_procedure
        procedure :: add_call_edge => graph_add_call
        procedure :: find_proc_index => graph_find_procedure_index
        procedure :: get_proc_callers => graph_get_callers
        procedure :: get_proc_callees => graph_get_callees
        procedure :: is_used => graph_is_procedure_used
        procedure :: find_unused => graph_find_unused_procedures
        procedure :: print => graph_print_call_graph
        procedure :: deep_copy => call_graph_deep_copy
        procedure :: assign => call_graph_assign
        generic :: assignment(=) => assign
    end type call_graph_t

contains

    ! Create a new empty call graph
    function create_call_graph() result(graph)
        type(call_graph_t) :: graph
        ! Initialize with small capacity to avoid immediate reallocation
        graph%proc_capacity = 16
        graph%call_capacity = 16
        allocate(graph%procedures(graph%proc_capacity))
        allocate(graph%calls(graph%call_capacity))
        graph%proc_count = 0
        graph%call_count = 0
    end function create_call_graph

    ! Add a procedure to the call graph
    subroutine add_procedure(graph, name, def_node, line, column, is_main, &
                           is_intrinsic, is_external)
        type(call_graph_t), intent(inout) :: graph
        character(len=*), intent(in) :: name
        integer, intent(in) :: def_node
        integer, intent(in) :: line, column
        logical, intent(in), optional :: is_main
        logical, intent(in), optional :: is_intrinsic
        logical, intent(in), optional :: is_external
        
        type(procedure_info_t) :: new_proc
        type(procedure_info_t), allocatable :: temp_procs(:)
        integer :: i
        
        ! Check if procedure already exists
        do i = 1, graph%proc_count
            if (graph%procedures(i)%name == name) then
                ! Update existing procedure info if needed
                if (present(is_main)) graph%procedures(i)%is_main_program = is_main
                if (present(is_intrinsic)) graph%procedures(i)%is_intrinsic = is_intrinsic
                if (present(is_external)) graph%procedures(i)%is_external = is_external
                return
            end if
        end do
        
        ! Create new procedure
        new_proc%name = name
        new_proc%definition_node = def_node
        new_proc%line = line
        new_proc%column = column
        new_proc%is_main_program = .false.
        new_proc%is_intrinsic = .false.
        new_proc%is_external = .false.
        if (present(is_main)) new_proc%is_main_program = is_main
        if (present(is_intrinsic)) new_proc%is_intrinsic = is_intrinsic
        if (present(is_external)) new_proc%is_external = is_external
        
        ! Expand procedures array if needed
        if (graph%proc_count >= graph%proc_capacity) then
            ! Grow capacity by 50% or at least 16 elements
            graph%proc_capacity = max(graph%proc_capacity + graph%proc_capacity/2, &
                                     graph%proc_capacity + 16, 16)
            allocate(temp_procs(graph%proc_capacity))
            if (graph%proc_count > 0) then
                temp_procs(1:graph%proc_count) = graph%procedures
            end if
            call move_alloc(temp_procs, graph%procedures)
        end if
        
        ! Add new procedure
        graph%proc_count = graph%proc_count + 1
        graph%procedures(graph%proc_count) = new_proc
    end subroutine add_procedure

    ! Add a call from one procedure to another
    subroutine add_call(graph, caller_name, callee_name, call_node, line, column)
        type(call_graph_t), intent(inout) :: graph
        character(len=*), intent(in) :: caller_name
        character(len=*), intent(in) :: callee_name
        integer, intent(in) :: call_node
        integer, intent(in) :: line, column
        
        type(call_edge_t) :: new_call
        type(call_edge_t), allocatable :: temp_calls(:)
        
        ! Create new call edge
        new_call%caller = caller_name
        new_call%callee = callee_name
        new_call%call_site_node = call_node
        new_call%line = line
        new_call%column = column
        
        ! Expand calls array if needed
        if (graph%call_count >= graph%call_capacity) then
            ! Grow capacity by 50% or at least 16 elements
            graph%call_capacity = max(graph%call_capacity + graph%call_capacity/2, &
                                     graph%call_capacity + 16, 16)
            allocate(temp_calls(graph%call_capacity))
            if (graph%call_count > 0) then
                temp_calls(1:graph%call_count) = graph%calls
            end if
            call move_alloc(temp_calls, graph%calls)
        end if
        
        ! Add new call
        graph%call_count = graph%call_count + 1
        graph%calls(graph%call_count) = new_call
    end subroutine add_call

    ! Find all procedures that are never called
    function find_unused_procedures(graph) result(unused_names)
        type(call_graph_t), intent(in) :: graph
        character(len=:), allocatable :: unused_names(:)
        logical, allocatable :: is_called(:)
        integer :: i, j, unused_count, sep_pos
        character(len=256), allocatable :: temp_names(:)
        character(len=256) :: simple_name
        
        ! Initialize all procedures as not called
        allocate(is_called(graph%proc_count))
        is_called = .false.
        
        ! Mark main programs and intrinsics as "called" (they don't need to be)
        do i = 1, graph%proc_count
            if (graph%procedures(i)%is_main_program .or. &
                graph%procedures(i)%is_intrinsic) then
                is_called(i) = .true.
            end if
        end do
        
        ! Mark all called procedures
        do i = 1, graph%call_count
            ! Extract simple name from callee for comparison
            simple_name = graph%calls(i)%callee
            sep_pos = index(simple_name, "::", back=.true.)
            if (sep_pos > 0) then
                simple_name = simple_name(sep_pos+2:)
            end if
            
            do j = 1, graph%proc_count
                ! Extract simple name from procedure for comparison
                block
                    character(len=256) :: proc_simple_name
                    integer :: proc_sep_pos
                    proc_simple_name = graph%procedures(j)%name
                    proc_sep_pos = index(proc_simple_name, "::", back=.true.)
                    if (proc_sep_pos > 0) then
                        proc_simple_name = proc_simple_name(proc_sep_pos+2:)
                    end if
                    
                    if (proc_simple_name == simple_name) then
                        is_called(j) = .true.
                        exit
                    end if
                end block
            end do
        end do
        
        ! Count unused procedures
        unused_count = 0
        do i = 1, graph%proc_count
            if (.not. is_called(i)) then
                unused_count = unused_count + 1
            end if
        end do
        
        ! Collect unused procedure names (return simple names without scope)
        if (unused_count > 0) then
            allocate(temp_names(unused_count))
            j = 0
            do i = 1, graph%proc_count
                if (.not. is_called(i)) then
                    j = j + 1
                    simple_name = graph%procedures(i)%name
                    sep_pos = index(simple_name, "::", back=.true.)
                    if (sep_pos > 0) then
                        simple_name = simple_name(sep_pos+2:)
                    end if
                    temp_names(j) = simple_name
                end if
            end do
            
            ! Convert to allocatable array of proper size
            allocate(character(len=maxval(len_trim(temp_names))) :: unused_names(unused_count))
            do i = 1, unused_count
                unused_names(i) = trim(temp_names(i))
            end do
        else
            allocate(character(len=1) :: unused_names(0))
        end if
    end function find_unused_procedures

    ! Get all procedures that call a given procedure
    function get_callers(graph, procedure_name) result(caller_names)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: procedure_name
        character(len=:), allocatable :: caller_names(:)
        character(len=256), allocatable :: temp_names(:)
        integer :: i, count, sep_pos
        logical, allocatable :: unique_check(:)
        character(len=256) :: simple_callee
        
        ! Count unique callers
        allocate(temp_names(graph%call_count))
        allocate(unique_check(graph%call_count))
        unique_check = .false.
        count = 0
        
        do i = 1, graph%call_count
            ! Extract simple name from scoped name for comparison
            simple_callee = graph%calls(i)%callee
            sep_pos = index(simple_callee, "::", back=.true.)
            if (sep_pos > 0) then
                simple_callee = simple_callee(sep_pos+2:)
            end if
            
            if (graph%calls(i)%callee == procedure_name .or. &
                trim(simple_callee) == procedure_name) then
                block
                    character(len=256) :: extracted_caller
                    extracted_caller = graph%calls(i)%caller
                    sep_pos = index(extracted_caller, "::", back=.true.)
                    if (sep_pos > 0) then
                        extracted_caller = extracted_caller(sep_pos+2:)
                    end if
                    
                    ! Check if this caller is already in list  
                    if (.not. any(temp_names(1:count) == trim(extracted_caller))) then
                        count = count + 1
                        temp_names(count) = trim(extracted_caller)
                    end if
                end block
            end if
        end do
        
        ! Convert to properly sized result
        if (count > 0) then
            allocate(character(len=maxval(len_trim(temp_names(1:count)))) :: &
                     caller_names(count))
            do i = 1, count
                caller_names(i) = trim(temp_names(i))
            end do
        else
            allocate(character(len=1) :: caller_names(0))
        end if
    end function get_callers

    ! Get all procedures called by a given procedure
    function get_callees(graph, procedure_name) result(callee_names)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: procedure_name
        character(len=:), allocatable :: callee_names(:)
        character(len=256), allocatable :: temp_names(:)
        integer :: i, count, sep_pos
        character(len=256) :: simple_caller, extracted_callee
        
        ! Count unique callees
        allocate(temp_names(graph%call_count))
        count = 0
        
        do i = 1, graph%call_count
            ! Extract simple name from scoped name for comparison
            simple_caller = graph%calls(i)%caller
            sep_pos = index(simple_caller, "::", back=.true.)
            if (sep_pos > 0) then
                simple_caller = simple_caller(sep_pos+2:)
            end if
            
            if (graph%calls(i)%caller == procedure_name .or. &
                trim(simple_caller) == procedure_name) then
                block
                    character(len=256) :: extracted_callee
                    ! Extract simple name from callee for output
                    extracted_callee = graph%calls(i)%callee
                    sep_pos = index(extracted_callee, "::", back=.true.)
                    if (sep_pos > 0) then
                        extracted_callee = extracted_callee(sep_pos+2:)
                    end if
                    
                    ! Check if this callee is already in list
                    if (.not. any(temp_names(1:count) == trim(extracted_callee))) then
                        count = count + 1
                        temp_names(count) = trim(extracted_callee)
                    end if
                end block
            end if
        end do
        
        ! Convert to properly sized result
        if (count > 0) then
            allocate(character(len=maxval(len_trim(temp_names(1:count)))) :: &
                     callee_names(count))
            do i = 1, count
                callee_names(i) = trim(temp_names(i))
            end do
        else
            allocate(character(len=1) :: callee_names(0))
        end if
    end function get_callees

    ! Check if a procedure is called by any other procedure
    function is_procedure_used(graph, procedure_name) result(is_used)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: procedure_name
        logical :: is_used
        integer :: i, sep_pos
        character(len=256) :: simple_name, simple_callee
        
        ! Main programs are always "used"
        do i = 1, graph%proc_count
            simple_name = graph%procedures(i)%name
            sep_pos = index(simple_name, "::", back=.true.)
            if (sep_pos > 0) then
                simple_name = simple_name(sep_pos+2:)
            end if
            
            if ((graph%procedures(i)%name == procedure_name .or. &
                 trim(simple_name) == procedure_name) .and. &
                graph%procedures(i)%is_main_program) then
                is_used = .true.
                return
            end if
        end do
        
        ! Check if called by anyone
        do i = 1, graph%call_count
            simple_callee = graph%calls(i)%callee
            sep_pos = index(simple_callee, "::", back=.true.)
            if (sep_pos > 0) then
                simple_callee = simple_callee(sep_pos+2:)
            end if
            
            if (graph%calls(i)%callee == procedure_name .or. &
                trim(simple_callee) == procedure_name) then
                is_used = .true.
                return
            end if
        end do
        
        is_used = .false.
    end function is_procedure_used

    ! Get all procedure names in the graph (returns simple names without scope)
    function get_all_procedures(graph) result(proc_names)
        type(call_graph_t), intent(in) :: graph
        character(len=:), allocatable :: proc_names(:)
        integer :: i, max_len, sep_pos
        character(len=256) :: simple_name
        
        if (graph%proc_count > 0) then
            ! Find max length of simple names
            max_len = 0
            do i = 1, graph%proc_count
                if (allocated(graph%procedures(i)%name)) then
                    simple_name = graph%procedures(i)%name
                    sep_pos = index(simple_name, "::", back=.true.)
                    if (sep_pos > 0) then
                        simple_name = simple_name(sep_pos+2:)
                    end if
                    max_len = max(max_len, len_trim(simple_name))
                end if
            end do
            
            allocate(character(len=max_len) :: proc_names(graph%proc_count))
            do i = 1, graph%proc_count
                if (allocated(graph%procedures(i)%name)) then
                    simple_name = graph%procedures(i)%name
                    sep_pos = index(simple_name, "::", back=.true.)
                    if (sep_pos > 0) then
                        simple_name = simple_name(sep_pos+2:)
                    end if
                    proc_names(i) = trim(simple_name)
                else
                    proc_names(i) = ""
                end if
            end do
        else
            allocate(character(len=1) :: proc_names(0))
        end if
    end function get_all_procedures

    ! Get total number of calls in the graph
    function get_call_count(graph) result(count)
        type(call_graph_t), intent(in) :: graph
        integer :: count
        count = graph%call_count
    end function get_call_count

    ! Print the call graph for debugging
    subroutine print_call_graph(graph, unit)
        type(call_graph_t), intent(in) :: graph
        integer, intent(in), optional :: unit
        integer :: out_unit, i, j
        character(len=:), allocatable :: callers(:), callees(:)
        
        out_unit = 6  ! stdout
        if (present(unit)) out_unit = unit
        
        write(out_unit, '(A)') "=== Call Graph ==="
        write(out_unit, '(A,I0)') "Total procedures: ", graph%proc_count
        write(out_unit, '(A,I0)') "Total calls: ", graph%call_count
        write(out_unit, *)
        
        ! List all procedures
        write(out_unit, '(A)') "Procedures:"
        do i = 1, graph%proc_count
            write(out_unit, '(A,A)', advance='no') "  ", graph%procedures(i)%name
            if (graph%procedures(i)%is_main_program) write(out_unit, '(A)', advance='no') " [MAIN]"
            if (graph%procedures(i)%is_intrinsic) write(out_unit, '(A)', advance='no') " [INTRINSIC]"
            if (graph%procedures(i)%is_external) write(out_unit, '(A)', advance='no') " [EXTERNAL]"
            write(out_unit, '(A,I0,A,I0,A)') " (line ", graph%procedures(i)%line, &
                                             ", col ", graph%procedures(i)%column, ")"
            
            ! Show callers and callees
            callers = get_callers(graph, graph%procedures(i)%name)
            callees = get_callees(graph, graph%procedures(i)%name)
            
            if (size(callers) > 0) then
                write(out_unit, '(A)', advance='no') "    Called by: "
                do j = 1, size(callers)
                    if (j > 1) write(out_unit, '(A)', advance='no') ", "
                    write(out_unit, '(A)', advance='no') trim(callers(j))
                end do
                write(out_unit, *)
            end if
            
            if (size(callees) > 0) then
                write(out_unit, '(A)', advance='no') "    Calls: "
                do j = 1, size(callees)
                    if (j > 1) write(out_unit, '(A)', advance='no') ", "
                    write(out_unit, '(A)', advance='no') trim(callees(j))
                end do
                write(out_unit, *)
            end if
        end do
        
        ! Show unused procedures
        block
            character(len=:), allocatable :: unused(:)
            integer :: k
            unused = find_unused_procedures(graph)
            if (size(unused) > 0) then
                write(out_unit, *)
                write(out_unit, '(A)') "Unused procedures:"
                do k = 1, size(unused)
                    write(out_unit, '(A,A)') "  ", trim(unused(k))
                end do
            end if
        end block
    end subroutine print_call_graph

    ! Build call graph from AST by traversing it
    subroutine build_call_graph_from_ast(graph, arena, root_index)
        type(call_graph_t), intent(inout) :: graph
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        
        ! This will be implemented using the visitor pattern
        ! For now, this is a placeholder
        call traverse_ast_for_calls(graph, arena, root_index, "")
    end subroutine build_call_graph_from_ast

    ! Recursive traversal to build call graph
    recursive subroutine traverse_ast_for_calls(graph, arena, node_index, &
                                              current_scope)
        type(call_graph_t), intent(inout) :: graph
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: current_scope
        
        character(len=:), allocatable :: node_type
        character(len=256) :: new_scope
        integer :: i
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        node_type = arena%entries(node_index)%node_type
        
        select case (node_type)
        case ("program")
            ! Handle program node
            select type (node => arena%entries(node_index)%node)
            type is (program_node)
                call add_procedure(graph, node%name, node_index, &
                                 node%line, node%column, is_main=.true.)
                new_scope = node%name
                
                ! Traverse body
                if (allocated(node%body_indices)) then
                    do i = 1, size(node%body_indices)
                        call traverse_ast_for_calls(graph, arena, &
                                                  node%body_indices(i), new_scope)
                    end do
                end if
            end select
            
        case ("function")
            ! Handle function definition
            select type (node => arena%entries(node_index)%node)
            type is (function_def_node)
                call add_procedure(graph, node%name, node_index, &
                                 node%line, node%column)
                new_scope = node%name
                
                ! Traverse body
                if (allocated(node%body_indices)) then
                    do i = 1, size(node%body_indices)
                        call traverse_ast_for_calls(graph, arena, &
                                                  node%body_indices(i), new_scope)
                    end do
                end if
            end select
            
        case ("subroutine")
            ! Handle subroutine definition
            select type (node => arena%entries(node_index)%node)
            type is (subroutine_def_node)
                call add_procedure(graph, node%name, node_index, &
                                 node%line, node%column)
                new_scope = node%name
                
                ! Traverse body
                if (allocated(node%body_indices)) then
                    do i = 1, size(node%body_indices)
                        call traverse_ast_for_calls(graph, arena, &
                                                  node%body_indices(i), new_scope)
                    end do
                end if
            end select
            
        case ("call", "subroutine_call")
            ! Handle subroutine call
            select type (node => arena%entries(node_index)%node)
            type is (subroutine_call_node)
                if (len_trim(current_scope) > 0) then
                    call add_call(graph, current_scope, node%name, node_index, &
                                node%line, node%column)
                end if
            end select
            
        case ("call_or_subscript")
            ! Handle function call (need to check if it's actually a call)
            select type (node => arena%entries(node_index)%node)
            type is (call_or_subscript_node)
                ! For now, assume it's a function call if in expression context
                if (len_trim(current_scope) > 0) then
                    call add_call(graph, current_scope, node%name, node_index, &
                                node%line, node%column)
                end if
            end select
            
        case default
            ! For other node types, just traverse children
            call traverse_children(graph, arena, node_index, current_scope)
        end select
    end subroutine traverse_ast_for_calls

    ! Helper to traverse all children of a node
    subroutine traverse_children(graph, arena, node_index, current_scope)
        type(call_graph_t), intent(inout) :: graph
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: current_scope
        
        integer, allocatable :: children(:)
        integer :: i
        
        ! Get children indices based on node type
        ! This is simplified - in reality we'd need to handle each node type
        children = arena%get_children(node_index)
        
        if (allocated(children)) then
            do i = 1, size(children)
                call traverse_ast_for_calls(graph, arena, children(i), &
                                          current_scope)
            end do
        end if
    end subroutine traverse_children

    ! Type-bound procedures
    subroutine graph_add_procedure(this, name, def_node, line, column, &
                                  is_main, is_intrinsic, is_external)
        class(call_graph_t), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer, intent(in) :: def_node
        integer, intent(in) :: line, column
        logical, intent(in), optional :: is_main, is_intrinsic, is_external
        
        call add_procedure(this, name, def_node, line, column, &
                         is_main, is_intrinsic, is_external)
    end subroutine graph_add_procedure

    subroutine graph_add_call(this, caller_name, callee_name, call_node, &
                            line, column)
        class(call_graph_t), intent(inout) :: this
        character(len=*), intent(in) :: caller_name
        character(len=*), intent(in) :: callee_name
        integer, intent(in) :: call_node
        integer, intent(in) :: line, column
        
        call add_call(this, caller_name, callee_name, call_node, line, column)
    end subroutine graph_add_call

    function graph_find_procedure_index(this, name) result(index)
        class(call_graph_t), intent(in) :: this
        character(len=*), intent(in) :: name
        integer :: index
        integer :: i
        
        index = 0
        do i = 1, this%proc_count
            if (this%procedures(i)%name == name) then
                index = i
                return
            end if
        end do
    end function graph_find_procedure_index

    function graph_get_callers(this, procedure_name) result(caller_names)
        class(call_graph_t), intent(in) :: this
        character(len=*), intent(in) :: procedure_name
        character(len=:), allocatable :: caller_names(:)
        
        caller_names = get_callers(this, procedure_name)
    end function graph_get_callers

    function graph_get_callees(this, procedure_name) result(callee_names)
        class(call_graph_t), intent(in) :: this
        character(len=*), intent(in) :: procedure_name
        character(len=:), allocatable :: callee_names(:)
        
        callee_names = get_callees(this, procedure_name)
    end function graph_get_callees

    function graph_is_procedure_used(this, procedure_name) result(is_used)
        class(call_graph_t), intent(in) :: this
        character(len=*), intent(in) :: procedure_name
        logical :: is_used
        
        is_used = is_procedure_used(this, procedure_name)
    end function graph_is_procedure_used

    function graph_find_unused_procedures(this) result(unused_names)
        class(call_graph_t), intent(in) :: this
        character(len=:), allocatable :: unused_names(:)
        
        unused_names = find_unused_procedures(this)
    end function graph_find_unused_procedures

    ! Find recursive cycles in the call graph
    function find_recursive_cycles(graph) result(cycles)
        type(call_graph_t), intent(in) :: graph
        character(len=:), allocatable :: cycles(:)
        
        character(len=256), allocatable :: temp_cycles(:)
        logical, allocatable :: visited(:), in_stack(:)
        integer :: cycle_count, i
        
        allocate(visited(graph%proc_count))
        allocate(in_stack(graph%proc_count))
        allocate(temp_cycles(graph%proc_count))
        
        visited = .false.
        in_stack = .false.
        cycle_count = 0
        
        ! Use depth-first search to detect cycles
        do i = 1, graph%proc_count
            if (.not. visited(i)) then
                call dfs_cycle_detect(graph, i, visited, in_stack, &
                                    temp_cycles, cycle_count)
            end if
        end do
        
        ! Convert to properly sized result
        if (cycle_count > 0) then
            allocate(character(len=maxval(len_trim(temp_cycles(1:cycle_count)))) :: &
                     cycles(cycle_count))
            do i = 1, cycle_count
                cycles(i) = trim(temp_cycles(i))
            end do
        else
            allocate(character(len=1) :: cycles(0))
        end if
    end function find_recursive_cycles
    
    ! Helper for cycle detection using DFS
    recursive subroutine dfs_cycle_detect(graph, proc_idx, visited, in_stack, &
                                        cycles, cycle_count)
        type(call_graph_t), intent(in) :: graph
        integer, intent(in) :: proc_idx
        logical, intent(inout) :: visited(:), in_stack(:)
        character(len=256), intent(inout) :: cycles(:)
        integer, intent(inout) :: cycle_count
        
        integer :: i, callee_idx
        
        visited(proc_idx) = .true.
        in_stack(proc_idx) = .true.
        
        ! Check all callees of this procedure
        do i = 1, graph%call_count
            if (graph%calls(i)%caller == graph%procedures(proc_idx)%name) then
                ! Find callee index
                callee_idx = 0
                do callee_idx = 1, graph%proc_count
                    if (graph%procedures(callee_idx)%name == graph%calls(i)%callee) then
                        exit
                    end if
                end do
                
                if (callee_idx > 0 .and. callee_idx <= graph%proc_count) then
                    if (in_stack(callee_idx)) then
                        ! Found a cycle
                        cycle_count = cycle_count + 1
                        cycles(cycle_count) = graph%procedures(proc_idx)%name
                    else if (.not. visited(callee_idx)) then
                        call dfs_cycle_detect(graph, callee_idx, visited, in_stack, &
                                            cycles, cycle_count)
                    end if
                end if
            end if
        end do
        
        in_stack(proc_idx) = .false.
    end subroutine dfs_cycle_detect

    subroutine graph_print_call_graph(this, unit)
        class(call_graph_t), intent(in) :: this
        integer, intent(in), optional :: unit
        
        call print_call_graph(this, unit)
    end subroutine graph_print_call_graph

    ! Deep copy for call_graph_t
    subroutine call_graph_deep_copy(dst, src)
        class(call_graph_t), intent(out) :: dst
        class(call_graph_t), intent(in) :: src
        integer :: i
        
        dst%proc_count = src%proc_count
        dst%call_count = src%call_count
        dst%proc_capacity = src%proc_capacity
        dst%call_capacity = src%call_capacity
        
        ! Deep copy procedures
        if (allocated(src%procedures)) then
            allocate(dst%procedures(dst%proc_capacity))
            do i = 1, dst%proc_count
                if (allocated(src%procedures(i)%name)) then
                    dst%procedures(i)%name = src%procedures(i)%name
                end if
                dst%procedures(i)%definition_node = src%procedures(i)%definition_node
                dst%procedures(i)%line = src%procedures(i)%line
                dst%procedures(i)%column = src%procedures(i)%column
                dst%procedures(i)%is_main_program = src%procedures(i)%is_main_program
                dst%procedures(i)%is_intrinsic = src%procedures(i)%is_intrinsic
                dst%procedures(i)%is_external = src%procedures(i)%is_external
            end do
        end if
        
        ! Deep copy calls
        if (allocated(src%calls)) then
            allocate(dst%calls(dst%call_capacity))
            do i = 1, dst%call_count
                if (allocated(src%calls(i)%caller)) then
                    dst%calls(i)%caller = src%calls(i)%caller
                end if
                if (allocated(src%calls(i)%callee)) then
                    dst%calls(i)%callee = src%calls(i)%callee
                end if
                dst%calls(i)%call_site_node = src%calls(i)%call_site_node
                dst%calls(i)%line = src%calls(i)%line
                dst%calls(i)%column = src%calls(i)%column
            end do
        end if
    end subroutine call_graph_deep_copy

    ! Assignment operator
    subroutine call_graph_assign(dst, src)
        class(call_graph_t), intent(out) :: dst
        class(call_graph_t), intent(in) :: src
        
        call call_graph_deep_copy(dst, src)
    end subroutine call_graph_assign

end module call_graph_module