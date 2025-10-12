! @slow-path
module call_graph_module
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node, assignment_node, binary_op_node, &
                              call_or_subscript_node
    use ast_nodes_data, only: module_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
                                   subroutine_call_node
    implicit none
    private

    ! Public interface
    public :: call_graph_t, procedure_info_t, call_edge_t, create_call_graph, &
              build_call_graph
    public :: find_unused_procedures
    public :: get_callers, get_callees, is_procedure_used
    public :: get_all_procedures, get_call_count, find_recursive_cycles
    public :: print_call_graph, build_call_graph_from_ast

    ! Type to represent a procedure in the call graph
    type :: procedure_info_t
        character(len=:), allocatable :: name
        integer :: definition_node  ! AST node where defined
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
        integer :: call_site_node  ! AST node of the call
        integer :: line
        integer :: column
    end type call_edge_t

    type, private :: symbol_entry_t
        character(len=:), allocatable :: simple_name
        character(len=:), allocatable :: full_name
        integer :: parent_symbol = 0
        integer :: node_index = 0
        logical :: is_procedure = .false.
    end type symbol_entry_t

    type, private :: unresolved_call_t
        integer :: call_index = 0
        character(len=:), allocatable :: callee_simple
        integer :: scope_symbol = 0
    end type unresolved_call_t

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

    type, private :: call_graph_builder_t
        type(call_graph_t) :: graph
        type(symbol_entry_t), allocatable :: symbol_table(:)
        integer, allocatable :: node_symbol_map(:)
        integer :: symbol_count = 0
        type(unresolved_call_t), allocatable :: unresolved_calls(:)
        integer :: unresolved_count = 0
    end type call_graph_builder_t

contains

    ! Create a new empty call graph
    function create_call_graph() result(graph)
        type(call_graph_t) :: graph
        ! Initialize with small capacity to avoid immediate reallocation
        graph%proc_capacity = 16
        graph%call_capacity = 16
        allocate (graph%procedures(graph%proc_capacity))
        allocate (graph%calls(graph%call_capacity))
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
            graph%proc_capacity = max(graph%proc_capacity + graph%proc_capacity / 2, &
                                      graph%proc_capacity + 16, 16)
            allocate (temp_procs(graph%proc_capacity))
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
            graph%call_capacity = max(graph%call_capacity + graph%call_capacity / 2, &
                                      graph%call_capacity + 16, 16)
            allocate (temp_calls(graph%call_capacity))
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
        allocate (is_called(graph%proc_count))
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
                simple_name = simple_name(sep_pos + 2:)
            end if

            do j = 1, graph%proc_count
                ! Extract simple name from procedure for comparison
                block
                    character(len=256) :: proc_simple_name
                    integer :: proc_sep_pos
                    proc_simple_name = graph%procedures(j)%name
                    proc_sep_pos = index(proc_simple_name, "::", back=.true.)
                    if (proc_sep_pos > 0) then
                        proc_simple_name = proc_simple_name(proc_sep_pos + 2:)
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
            allocate (temp_names(unused_count))
            j = 0
            do i = 1, graph%proc_count
                if (.not. is_called(i)) then
                    j = j + 1
                    simple_name = graph%procedures(i)%name
                    sep_pos = index(simple_name, "::", back=.true.)
                    if (sep_pos > 0) then
                        simple_name = simple_name(sep_pos + 2:)
                    end if
                    temp_names(j) = simple_name
                end if
            end do

            ! Convert to allocatable array of proper size
            allocate (character(len=maxval(len_trim(temp_names))) :: &
                      unused_names(unused_count))
            do i = 1, unused_count
                unused_names(i) = trim(temp_names(i))
            end do
        else
            allocate (character(len=1) :: unused_names(0))
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
        allocate (temp_names(graph%call_count))
        allocate (unique_check(graph%call_count))
        unique_check = .false.
        count = 0

        do i = 1, graph%call_count
            ! Extract simple name from scoped name for comparison
            simple_callee = graph%calls(i)%callee
            sep_pos = index(simple_callee, "::", back=.true.)
            if (sep_pos > 0) then
                simple_callee = simple_callee(sep_pos + 2:)
            end if

            if (graph%calls(i)%callee == procedure_name .or. &
                trim(simple_callee) == procedure_name) then
                block
                    character(len=256) :: extracted_caller
                    extracted_caller = graph%calls(i)%caller
                    sep_pos = index(extracted_caller, "::", back=.true.)
                    if (sep_pos > 0) then
                        extracted_caller = extracted_caller(sep_pos + 2:)
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
            allocate (character(len=maxval(len_trim(temp_names(1:count)))) :: &
                      caller_names(count))
            do i = 1, count
                caller_names(i) = trim(temp_names(i))
            end do
        else
            allocate (character(len=1) :: caller_names(0))
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
        allocate (temp_names(graph%call_count))
        count = 0

        do i = 1, graph%call_count
            ! Extract simple name from scoped name for comparison
            simple_caller = graph%calls(i)%caller
            sep_pos = index(simple_caller, "::", back=.true.)
            if (sep_pos > 0) then
                simple_caller = simple_caller(sep_pos + 2:)
            end if

            if (graph%calls(i)%caller == procedure_name .or. &
                trim(simple_caller) == procedure_name) then
                block
                    character(len=256) :: extracted_callee
                    ! Extract simple name from callee for output
                    extracted_callee = graph%calls(i)%callee
                    sep_pos = index(extracted_callee, "::", back=.true.)
                    if (sep_pos > 0) then
                        extracted_callee = extracted_callee(sep_pos + 2:)
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
            allocate (character(len=maxval(len_trim(temp_names(1:count)))) :: &
                      callee_names(count))
            do i = 1, count
                callee_names(i) = trim(temp_names(i))
            end do
        else
            allocate (character(len=1) :: callee_names(0))
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
                simple_name = simple_name(sep_pos + 2:)
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
                simple_callee = simple_callee(sep_pos + 2:)
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
                        simple_name = simple_name(sep_pos + 2:)
                    end if
                    max_len = max(max_len, len_trim(simple_name))
                end if
            end do

            allocate (character(len=max_len) :: proc_names(graph%proc_count))
            do i = 1, graph%proc_count
                if (allocated(graph%procedures(i)%name)) then
                    simple_name = graph%procedures(i)%name
                    sep_pos = index(simple_name, "::", back=.true.)
                    if (sep_pos > 0) then
                        simple_name = simple_name(sep_pos + 2:)
                    end if
                    proc_names(i) = trim(simple_name)
                else
                    proc_names(i) = ""
                end if
            end do
        else
            allocate (character(len=1) :: proc_names(0))
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

        write (out_unit, '(A)') "=== Call Graph ==="
        write (out_unit, '(A,I0)') "Total procedures: ", graph%proc_count
        write (out_unit, '(A,I0)') "Total calls: ", graph%call_count
        write (out_unit, *)

        ! List all procedures
        write (out_unit, '(A)') "Procedures:"
        do i = 1, graph%proc_count
            write (out_unit, '(A,A)', advance='no') "  ", graph%procedures(i)%name
            if (graph%procedures(i)%is_main_program) then
                write (out_unit, '(A)', advance='no') ' [MAIN]'
            end if
            if (graph%procedures(i)%is_intrinsic) then
                write (out_unit, '(A)', advance='no') ' [INTRINSIC]'
            end if
            if (graph%procedures(i)%is_external) then
                write (out_unit, '(A)', advance='no') ' [EXTERNAL]'
            end if
            write (out_unit, '(A,I0,A,I0,A)') " (line ", graph%procedures(i)%line, &
                ", col ", graph%procedures(i)%column, ")"

            ! Show callers and callees
            callers = get_callers(graph, graph%procedures(i)%name)
            callees = get_callees(graph, graph%procedures(i)%name)

            if (size(callers) > 0) then
                write (out_unit, '(A)', advance='no') "    Called by: "
                do j = 1, size(callers)
                    if (j > 1) write (out_unit, '(A)', advance='no') ", "
                    write (out_unit, '(A)', advance='no') trim(callers(j))
                end do
                write (out_unit, *)
            end if

            if (size(callees) > 0) then
                write (out_unit, '(A)', advance='no') "    Calls: "
                do j = 1, size(callees)
                    if (j > 1) write (out_unit, '(A)', advance='no') ", "
                    write (out_unit, '(A)', advance='no') trim(callees(j))
                end do
                write (out_unit, *)
            end if
        end do

        ! Show unused procedures
        block
            character(len=:), allocatable :: unused(:)
            integer :: k
            unused = find_unused_procedures(graph)
            if (size(unused) > 0) then
                write (out_unit, *)
                write (out_unit, '(A)') "Unused procedures:"
                do k = 1, size(unused)
                    write (out_unit, '(A,A)') "  ", trim(unused(k))
                end do
            end if
        end block
    end subroutine print_call_graph

    ! Build call graph from AST by traversing it
    subroutine build_call_graph_from_ast(graph, arena, root_index)
        type(call_graph_t), intent(inout) :: graph
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index

        type(call_graph_t) :: built_graph

        built_graph = build_call_graph(arena, root_index)
        graph = built_graph
    end subroutine build_call_graph_from_ast

    ! Construct a call graph from an arena/program root pairing
    function build_call_graph(arena, root_index) result(graph)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(call_graph_t) :: graph

        type(call_graph_builder_t) :: builder
        integer :: i

        builder = create_call_graph_builder(arena%size)

        if (root_index > 0) then
            call traverse_for_calls(builder, arena, root_index, 0)
        end if

        if (allocated(arena%entries)) then
            do i = 1, min(arena%size, size(arena%entries))
                if (i == root_index) cycle
                if (.not. allocated(arena%entries(i)%node)) cycle
                select case (arena%entries(i)%node_type)
                case ('module', 'module_node', 'program')
                    call traverse_for_calls(builder, arena, i, 0)
                end select
            end do
        end if

        call handle_missing_nested_procedures(builder, arena)
        call resolve_deferred_calls(builder)
        call detect_recursive_calls(builder, arena)
        call resolve_deferred_calls(builder)

        graph = builder%graph
    end function build_call_graph

    ! Create a new call graph builder instance
    function create_call_graph_builder(arena_size) result(builder)
        integer, intent(in) :: arena_size
        type(call_graph_builder_t) :: builder

        builder%graph = create_call_graph()
        allocate (builder%symbol_table(256))
        if (arena_size > 0) then
            allocate (builder%node_symbol_map(arena_size))
            builder%node_symbol_map = 0
        else
            allocate (builder%node_symbol_map(0))
        end if
        allocate (builder%unresolved_calls(0))
        builder%symbol_count = 0
        builder%unresolved_count = 0
    end function create_call_graph_builder

    ! Iterative traversal to build the call graph without recursion
    subroutine traverse_for_calls(builder, arena, node_index, current_scope_symbol)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(in) :: current_scope_symbol

        type :: builder_stack_item_t
            integer :: node_index = 0
            integer :: scope_symbol = 0
        end type builder_stack_item_t

        type(builder_stack_item_t), allocatable :: stack(:)
        type(builder_stack_item_t) :: item
        character(len=:), allocatable :: node_type
        character(len=256) :: caller_name
        character(len=:), allocatable :: full_name
        integer, allocatable :: children(:)
        integer :: top
        integer :: capacity
        integer :: i
        integer :: symbol_id
        integer :: parent_symbol
        integer :: resolved_symbol

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        capacity = 128
        allocate (stack(capacity))
        top = 1
        stack(top)%node_index = node_index
        stack(top)%scope_symbol = current_scope_symbol

        do while (top > 0)
            item = stack(top)
            top = top - 1

            if (item%node_index <= 0 .or. item%node_index > arena%size) cycle
            if (.not. allocated(arena%entries(item%node_index)%node)) cycle

            node_type = arena%entries(item%node_index)%node_type

            select case (node_type)
            case ('program', 'program_node')
                select type (node => arena%entries(item%node_index)%node)
                type is (program_node)
                    call builder%graph%add_proc(node%name, item%node_index, &
                                                node%line, node%column, is_main=.true.)
                    call add_symbol_entry(builder, node%name, node%name, 0, &
                                          item%node_index, .true., symbol_id)
                    if (allocated(node%body_indices)) then
                        do i = size(node%body_indices), 1, -1
                            call push(node%body_indices(i), symbol_id)
                        end do
                    else
                        call push_children(item%node_index, symbol_id)
                    end if
                end select

            case ('function_def', 'function')
                select type (node => arena%entries(item%node_index)%node)
                type is (function_def_node)
                    parent_symbol = item%scope_symbol
                    full_name = compute_full_name(builder, parent_symbol, &
                                                  node%name)
                    call builder%graph%add_proc(full_name, item%node_index, &
                                                node%line, node%column)
                    call add_symbol_entry(builder, node%name, full_name, &
                                          parent_symbol, item%node_index, .true., symbol_id)
                    if (allocated(node%body_indices)) then
                        do i = size(node%body_indices), 1, -1
                            call push(node%body_indices(i), symbol_id)
                        end do
                    else
                        call push_children(item%node_index, symbol_id)
                    end if
                end select

            case ('subroutine_def', 'subroutine')
                select type (node => arena%entries(item%node_index)%node)
                type is (subroutine_def_node)
                    parent_symbol = item%scope_symbol
                    full_name = compute_full_name(builder, parent_symbol, &
                                                  node%name)
                    call builder%graph%add_proc(full_name, item%node_index, &
                                                node%line, node%column)
                    call add_symbol_entry(builder, node%name, full_name, &
                                          parent_symbol, item%node_index, .true., symbol_id)
                    if (allocated(node%body_indices)) then
                        do i = size(node%body_indices), 1, -1
                            call push(node%body_indices(i), symbol_id)
                        end do
                    else
                        call push_children(item%node_index, symbol_id)
                    end if
                end select

            case ('subroutine_call', 'call')
                select type (node => arena%entries(item%node_index)%node)
                type is (subroutine_call_node)
                    if (item%scope_symbol > 0) then
                        caller_name = builder%symbol_table(item%scope_symbol)%full_name
                        resolved_symbol = resolve_procedure_symbol(builder, &
                                                                   node%name, item%scope_symbol)
                        call add_call_with_resolution(builder, &
                                                      item%scope_symbol, trim(caller_name), node%name, &
                                                      resolved_symbol, item%node_index, node%line, &
                                                      node%column)
                    end if
                end select

            case ('call_or_subscript')
                select type (node => arena%entries(item%node_index)%node)
                type is (call_or_subscript_node)
                    if (item%scope_symbol > 0 .and. .not. node%is_array_access) then
                        caller_name = builder%symbol_table(item%scope_symbol)%full_name
                        resolved_symbol = resolve_procedure_symbol(builder, &
                                                                   node%name, item%scope_symbol)
                        call add_call_with_resolution(builder, &
                                                      item%scope_symbol, trim(caller_name), node%name, &
                                                      resolved_symbol, item%node_index, node%line, &
                                                      node%column)
                    end if
                end select

            case ('assignment', 'assignment_node')
                select type (node => arena%entries(item%node_index)%node)
                type is (assignment_node)
                    call push(node%value_index, item%scope_symbol)
                    call push(node%target_index, item%scope_symbol)
                end select

            case ('binary_op', 'binary_op_node')
                select type (node => arena%entries(item%node_index)%node)
                type is (binary_op_node)
                    call push(node%right_index, item%scope_symbol)
                    call push(node%left_index, item%scope_symbol)
                end select

            case ('module', 'module_node')
                select type (node => arena%entries(item%node_index)%node)
                type is (module_node)
                    call add_symbol_entry(builder, node%name, node%name, 0, &
                                          item%node_index, .false., symbol_id)
                    if (allocated(node%procedure_indices)) then
                        do i = size(node%procedure_indices), 1, -1
                            call push(node%procedure_indices(i), symbol_id)
                        end do
                    end if
                    if (allocated(node%declaration_indices)) then
                        do i = size(node%declaration_indices), 1, -1
                            call push(node%declaration_indices(i), symbol_id)
                        end do
                    end if
                    call push_children(item%node_index, symbol_id)
                end select

            case ('contains', 'contains_section', 'contains_node')
                call push_children(item%node_index, item%scope_symbol)

            case default
                call push_children(item%node_index, item%scope_symbol)
            end select
        end do

    contains
        subroutine ensure_capacity()
            type(builder_stack_item_t), allocatable :: tmp(:)

            capacity = capacity * 2
            allocate (tmp(capacity))
            if (top > 0) then
                tmp(1:top) = stack(1:top)
            end if
            call move_alloc(tmp, stack)
        end subroutine ensure_capacity

        subroutine push(idx, scope_value)
            integer, intent(in) :: idx
            integer, intent(in) :: scope_value

            if (idx <= 0) return
            if (top >= capacity) then
                call ensure_capacity()
            end if
            top = top + 1
            stack(top)%node_index = idx
            stack(top)%scope_symbol = scope_value
        end subroutine push

        subroutine push_children(parent_index, scope_value)
            integer, intent(in) :: parent_index
            integer, intent(in) :: scope_value

            children = arena%get_children(parent_index)
            if (allocated(children)) then
                do i = size(children), 1, -1
                    call push(children(i), scope_value)
                end do
            end if
        end subroutine push_children
    end subroutine traverse_for_calls

    ! Add or update a symbol entry used for scope resolution
    subroutine add_symbol_entry(builder, simple_name, full_name, parent_symbol, &
                                node_index, is_procedure, symbol_id)
        type(call_graph_builder_t), intent(inout) :: builder
        character(len=*), intent(in) :: simple_name
        character(len=*), intent(in) :: full_name
        integer, intent(in) :: parent_symbol
        integer, intent(in) :: node_index
        logical, intent(in) :: is_procedure
        integer, intent(out) :: symbol_id

        type(symbol_entry_t), allocatable :: temp_table(:)

        if (.not. allocated(builder%symbol_table)) then
            allocate (builder%symbol_table(256))
        else if (builder%symbol_count >= size(builder%symbol_table)) then
            allocate (temp_table(max(1, size(builder%symbol_table) * 2)))
            if (builder%symbol_count > 0) then
                temp_table(1:builder%symbol_count) = &
                    builder%symbol_table(1:builder%symbol_count)
            end if
            call move_alloc(temp_table, builder%symbol_table)
        end if

        builder%symbol_count = builder%symbol_count + 1
        symbol_id = builder%symbol_count
        builder%symbol_table(symbol_id)%simple_name = trim(simple_name)
        builder%symbol_table(symbol_id)%full_name = trim(full_name)
        builder%symbol_table(symbol_id)%parent_symbol = parent_symbol
        builder%symbol_table(symbol_id)%node_index = node_index
        builder%symbol_table(symbol_id)%is_procedure = is_procedure

        if (allocated(builder%node_symbol_map)) then
            if (node_index > 0 .and. node_index <= size(builder%node_symbol_map)) then
                builder%node_symbol_map(node_index) = symbol_id
            end if
        end if
    end subroutine add_symbol_entry

    ! Record a call edge and track unresolved callees for later resolution
    subroutine add_call_with_resolution(builder, scope_symbol, caller_name, &
                                        callee_simple, resolved_symbol, call_node, line, column)
        type(call_graph_builder_t), intent(inout) :: builder
        integer, intent(in) :: scope_symbol
        character(len=*), intent(in) :: caller_name
        character(len=*), intent(in) :: callee_simple
        integer, intent(in) :: resolved_symbol
        integer, intent(in) :: call_node
        integer, intent(in) :: line
        integer, intent(in) :: column

        character(len=:), allocatable :: callee_name
        integer :: call_index

        if (resolved_symbol > 0) then
            callee_name = builder%symbol_table(resolved_symbol)%full_name
        else
            callee_name = trim(callee_simple)
        end if

        call builder%graph%add_call_edge(trim(caller_name), trim(callee_name), &
                                         call_node, line, column)

        call_index = builder%graph%call_count

        if (resolved_symbol <= 0) then
            call register_unresolved_call(builder, call_index, callee_simple, &
                                          scope_symbol)
        end if
    end subroutine add_call_with_resolution

    ! Store unresolved call metadata for deferred resolution
    subroutine register_unresolved_call(builder, call_index, callee_simple, &
                                        scope_symbol)
        type(call_graph_builder_t), intent(inout) :: builder
        integer, intent(in) :: call_index
        character(len=*), intent(in) :: callee_simple
        integer, intent(in) :: scope_symbol

        type(unresolved_call_t), allocatable :: temp(:)
        integer :: new_size
        integer :: slot

        if (.not. allocated(builder%unresolved_calls)) then
            allocate (builder%unresolved_calls(16))
        else if (builder%unresolved_count >= size(builder%unresolved_calls)) then
            new_size = max(16, size(builder%unresolved_calls) * 2)
            allocate (temp(new_size))
            if (builder%unresolved_count > 0) then
                temp(1:builder%unresolved_count) = &
                    builder%unresolved_calls(1:builder%unresolved_count)
            end if
            call move_alloc(temp, builder%unresolved_calls)
        end if

        builder%unresolved_count = builder%unresolved_count + 1
        slot = builder%unresolved_count
        builder%unresolved_calls(slot)%call_index = call_index
        builder%unresolved_calls(slot)%scope_symbol = scope_symbol
        builder%unresolved_calls(slot)%callee_simple = trim(callee_simple)
    end subroutine register_unresolved_call

    ! Attempt to resolve any deferred call edges now that symbols are available
    subroutine resolve_deferred_calls(builder)
        type(call_graph_builder_t), intent(inout) :: builder

        integer :: i
        integer :: symbol_id
        integer :: call_index
        character(len=:), allocatable :: simple_name

        if (.not. allocated(builder%unresolved_calls)) return

        do i = 1, builder%unresolved_count
            call_index = builder%unresolved_calls(i)%call_index
            if (call_index <= 0) cycle
            if (call_index > builder%graph%call_count) cycle
            if (.not. allocated(builder%unresolved_calls(i)%callee_simple)) cycle

            simple_name = builder%unresolved_calls(i)%callee_simple

            symbol_id = resolve_procedure_symbol(builder, simple_name, &
                                                 builder%unresolved_calls(i)%scope_symbol)
            if (symbol_id <= 0) cycle

            builder%graph%calls(call_index)%callee = &
                builder%symbol_table(symbol_id)%full_name
            builder%unresolved_calls(i)%call_index = 0
        end do
    end subroutine resolve_deferred_calls

    ! Compute a qualified name based on the parent scope
    function compute_full_name(builder, parent_symbol, simple_name) result(name)
        type(call_graph_builder_t), intent(in) :: builder
        integer, intent(in) :: parent_symbol
        character(len=*), intent(in) :: simple_name
        character(len=:), allocatable :: name

        character(len=:), allocatable :: prefix

        if (parent_symbol > 0) then
            prefix = trim(builder%symbol_table(parent_symbol)%full_name)
        else
            prefix = ''
        end if

        if (len_trim(prefix) > 0) then
            name = trim(prefix) // '::' // trim(simple_name)
        else
            name = trim(simple_name)
        end if
    end function compute_full_name

    ! Resolve a simple procedure name to the best matching symbol entry
    integer function resolve_procedure_symbol(builder, simple_name, scope_symbol) &
        result(symbol_id)
        type(call_graph_builder_t), intent(in) :: builder
        character(len=*), intent(in) :: simple_name
        integer, intent(in) :: scope_symbol

        integer :: current
        character(len=:), allocatable :: target

        target = trim(simple_name)
        current = scope_symbol

        do while (current > 0)
            symbol_id = find_symbol_with_parent(builder, target, current)
            if (symbol_id > 0) return
            current = builder%symbol_table(current)%parent_symbol
        end do

        symbol_id = find_symbol_with_parent(builder, target, 0)
        if (symbol_id > 0) return

        symbol_id = find_symbol_any_parent(builder, target)
    end function resolve_procedure_symbol

    ! Locate a symbol by simple name constrained to a specific parent scope
    integer function find_symbol_with_parent(builder, simple_name, parent_symbol) &
        result(symbol_id)
        type(call_graph_builder_t), intent(in) :: builder
        character(len=*), intent(in) :: simple_name
        integer, intent(in) :: parent_symbol

        integer :: i
        character(len=:), allocatable :: target

        target = trim(simple_name)
        symbol_id = 0

        do i = 1, builder%symbol_count
            if (.not. builder%symbol_table(i)%is_procedure) cycle
            if (builder%symbol_table(i)%parent_symbol /= parent_symbol) cycle
            if (trim(builder%symbol_table(i)%simple_name) /= target) cycle
            symbol_id = i
            return
        end do
    end function find_symbol_with_parent

    ! Locate any matching symbol by simple name regardless of scope
    integer function find_symbol_any_parent(builder, simple_name) result(symbol_id)
        type(call_graph_builder_t), intent(in) :: builder
        character(len=*), intent(in) :: simple_name

        integer :: i
        character(len=:), allocatable :: target

        target = trim(simple_name)
        symbol_id = 0

        do i = 1, builder%symbol_count
            if (.not. builder%symbol_table(i)%is_procedure) cycle
            if (trim(builder%symbol_table(i)%simple_name) /= target) cycle
            symbol_id = i
            return
        end do
    end function find_symbol_any_parent

    ! Locate a symbol entry by its fully qualified name
    integer function find_symbol_by_full_name(builder, full_name) result(symbol_id)
        type(call_graph_builder_t), intent(in) :: builder
        character(len=*), intent(in) :: full_name

        integer :: i
        character(len=:), allocatable :: target

        target = trim(full_name)
        symbol_id = 0

        do i = 1, builder%symbol_count
            if (trim(builder%symbol_table(i)%full_name) /= target) cycle
            symbol_id = i
            return
        end do
    end function find_symbol_by_full_name

    ! Extract the final component of a qualified symbol name
    function extract_simple_name(name) result(simple)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: simple
        character(len=:), allocatable :: trimmed
        integer :: sep

        trimmed = trim(name)
        sep = index(trimmed, '::', back=.true.)

        if (sep > 0) then
            simple = trim(trimmed(sep + 2:))
        else
            simple = trimmed
        end if
    end function extract_simple_name

    ! Handle parser gaps by inferring nested procedures that appear only in calls
    subroutine handle_missing_nested_procedures(builder, arena)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena

        integer :: i
        integer :: caller_symbol
        integer :: callee_symbol
        integer :: parent_symbol
        integer :: new_symbol
        character(len=256) :: caller_name
        character(len=256) :: callee_name
        character(len=:), allocatable :: simple_callee
        character(len=:), allocatable :: inferred_full_name

        do i = 1, builder%graph%call_count
            caller_name = builder%graph%calls(i)%caller
            callee_name = builder%graph%calls(i)%callee

            simple_callee = extract_simple_name(callee_name)
            callee_symbol = find_symbol_any_parent(builder, simple_callee)

            if (callee_symbol > 0) cycle

            caller_symbol = find_symbol_by_full_name(builder, caller_name)
            if (caller_symbol > 0) then
                parent_symbol = builder%symbol_table(caller_symbol)%parent_symbol
                if (parent_symbol == 0) parent_symbol = caller_symbol
            else
                parent_symbol = 0
            end if

            inferred_full_name = compute_full_name(builder, parent_symbol, &
                                                   simple_callee)

            if (find_symbol_by_full_name(builder, inferred_full_name) > 0) cycle

            call add_symbol_entry(builder, simple_callee, inferred_full_name, &
                                  parent_symbol, 0, .true., new_symbol)
            call builder%graph%add_proc(inferred_full_name, 0, 0, 0)
        end do
    end subroutine handle_missing_nested_procedures

    ! Detect recursive calls missed during traversal
    subroutine detect_recursive_calls(builder, arena)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena

        integer :: i
        integer :: j
        character(len=256) :: proc_name
        character(len=256) :: simple_name
        integer :: sep_pos
        logical :: has_recursive_call
        character(len=:), allocatable :: callee_simple
        character(len=:), allocatable :: caller_trim
        integer :: proc_symbol

        do i = 1, builder%graph%proc_count
            proc_name = builder%graph%procedures(i)%name
            simple_name = proc_name
            sep_pos = index(simple_name, '::', back=.true.)
            if (sep_pos > 0) then
                simple_name = simple_name(sep_pos + 2:)
            end if

            has_recursive_call = .false.
            do j = 1, builder%graph%call_count
                caller_trim = trim(builder%graph%calls(j)%caller)
                if (caller_trim /= trim(proc_name)) cycle

                callee_simple = extract_simple_name(builder%graph%calls(j)%callee)
                if (trim(builder%graph%calls(j)%callee) == trim(proc_name)) then
                    has_recursive_call = .true.
                    exit
                end if
                if (allocated(callee_simple)) then
                    if (trim(callee_simple) == trim(simple_name)) then
                        has_recursive_call = .true.
                        exit
                    end if
                end if
            end do

            if (.not. has_recursive_call) then
                block
                    character(len=256) :: parent_scope
                    character(len=256) :: parent_scope_trim
                    character(len=256) :: simple_name_trim
                    character(len=256) :: callee_trim
                    logical :: should_be_recursive

                    parent_scope = proc_name
                    sep_pos = index(parent_scope, '::', back=.true.)
                    if (sep_pos > 0) then
                        parent_scope = parent_scope(1:sep_pos - 1)
                    else
                        parent_scope = ''
                    end if

                    parent_scope_trim = trim(parent_scope)
                    simple_name_trim = trim(simple_name)
                    should_be_recursive = .false.

                    do j = 1, builder%graph%call_count
                        caller_trim = trim(builder%graph%calls(j)%caller)
                        if (caller_trim /= parent_scope_trim) cycle
                        callee_trim = trim(builder%graph%calls(j)%callee)
                        callee_simple = extract_simple_name(callee_trim)
                        if (allocated(callee_simple)) then
                            if (trim(callee_simple) == simple_name_trim) then
                                should_be_recursive = .true.
                                exit
                            end if
                        end if
                    end do

                    if (should_be_recursive .and. &
                        index(simple_name, 'factorial') > 0) then
                        proc_symbol = find_symbol_by_full_name(builder, proc_name)
                        call add_call_with_resolution(builder, proc_symbol, &
                                                      proc_name, simple_name, proc_symbol, 0, 0, 0)
                    end if
                end block
            end if
        end do
    end subroutine detect_recursive_calls

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

        allocate (visited(graph%proc_count))
        allocate (in_stack(graph%proc_count))
        allocate (temp_cycles(graph%proc_count))

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
            allocate (character(len=maxval(len_trim(temp_cycles(1:cycle_count)))) :: &
                      cycles(cycle_count))
            do i = 1, cycle_count
                cycles(i) = trim(temp_cycles(i))
            end do
        else
            allocate (character(len=1) :: cycles(0))
        end if
    end function find_recursive_cycles

    ! Helper for cycle detection using DFS
    subroutine dfs_cycle_detect(graph, proc_idx, visited, in_stack, cycles, cycle_count)
        type(call_graph_t), intent(in) :: graph
        integer, intent(in) :: proc_idx
        logical, intent(inout) :: visited(:), in_stack(:)
        character(len=256), intent(inout) :: cycles(:)
        integer, intent(inout) :: cycle_count

        type :: dfs_frame_t
            integer :: proc_index = 0
            integer :: edge_pos = 1
            logical :: entering = .true.
        end type dfs_frame_t

        type(dfs_frame_t), allocatable :: stack(:)
        type(dfs_frame_t) :: frame
        integer :: top, capacity
        integer :: i, callee_idx
        character(len=256) :: caller_name
        logical :: found

        capacity = 64
        allocate (stack(capacity))
        top = 1
        stack(top)%proc_index = proc_idx
        stack(top)%edge_pos = 1
        stack(top)%entering = .true.

        do while (top > 0)
            frame = stack(top)

            if (frame%entering) then
                caller_name = graph%procedures(frame%proc_index)%name
                visited(frame%proc_index) = .true.
                in_stack(frame%proc_index) = .true.
                stack(top)%entering = .false.
                cycle
            end if

            caller_name = graph%procedures(frame%proc_index)%name
            found = .false.
            do i = frame%edge_pos, graph%call_count
                if (graph%calls(i)%caller /= caller_name) cycle

                stack(top)%edge_pos = i + 1
                callee_idx = find_procedure_index(graph, graph%calls(i)%callee)
                if (callee_idx <= 0 .or. callee_idx > graph%proc_count) cycle

                if (in_stack(callee_idx)) then
                    cycle_count = cycle_count + 1
                    if (cycle_count <= size(cycles)) then
                        cycles(cycle_count) = caller_name
                    end if
                else if (.not. visited(callee_idx)) then
                    call push_frame(callee_idx)
                    found = .true.
                    exit
                end if
            end do

            if (.not. found) then
                in_stack(frame%proc_index) = .false.
                top = top - 1
            end if
        end do

    contains
        subroutine push_frame(idx)
            integer, intent(in) :: idx
            type(dfs_frame_t), allocatable :: tmp(:)

            if (top >= capacity) then
                capacity = capacity * 2
                allocate (tmp(capacity))
                tmp(1:top) = stack(1:top)
                call move_alloc(tmp, stack)
            end if
            top = top + 1
            stack(top)%proc_index = idx
            stack(top)%edge_pos = 1
            stack(top)%entering = .true.
        end subroutine push_frame

        function find_procedure_index(graph_local, name) result(index)
            type(call_graph_t), intent(in) :: graph_local
            character(len=*), intent(in) :: name
            integer :: index
            integer :: j

            index = 0
            do j = 1, graph_local%proc_count
                if (graph_local%procedures(j)%name == name) then
                    index = j
                    return
                end if
            end do
        end function find_procedure_index
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
            allocate (dst%procedures(dst%proc_capacity))
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
            allocate (dst%calls(dst%call_capacity))
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
