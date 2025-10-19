module call_graph_queries_mod
    use call_graph_core_mod, only: call_graph_t
    use call_graph_constants_mod, only: max_proc_name_len
    implicit none
    private

    public :: find_unused_procedures
    public :: get_callers, get_callees
    public :: is_procedure_used
    public :: get_all_procedures, get_call_count
    public :: print_call_graph
    public :: find_recursive_cycles

contains

    function find_unused_procedures(graph) result(unused_names)
        type(call_graph_t), intent(in) :: graph
        character(len=:), allocatable :: unused_names(:)
        logical, allocatable :: is_called(:)
        character(len=max_proc_name_len), allocatable :: temp_names(:)
        integer :: unused_count
        integer :: i

        allocate (is_called(graph%proc_count))
        is_called = .false.

        call mark_always_used_procs(graph, is_called)

        do i = 1, graph%call_count
            call mark_called_procedures(graph, graph%calls(i)%callee, is_called)
        end do

        unused_count = count(.not. is_called)
        if (unused_count == 0) then
            allocate (character(len=1) :: unused_names(0))
            return
        end if

        allocate (temp_names(unused_count))
        call collect_unused_simple_names(graph, is_called, temp_names)

        allocate (character(len=maxval(len_trim(temp_names))) :: &
                  unused_names(unused_count))
        do i = 1, unused_count
            unused_names(i) = trim(temp_names(i))
        end do
    end function find_unused_procedures

    subroutine mark_always_used_procs(graph, is_called)
        type(call_graph_t), intent(in) :: graph
        logical, intent(inout) :: is_called(:)
        integer :: i

        do i = 1, graph%proc_count
            if (graph%procedures(i)%is_main_program .or. &
                graph%procedures(i)%is_intrinsic) then
                is_called(i) = .true.
            end if
        end do
    end subroutine mark_always_used_procs

    subroutine mark_called_procedures(graph, callee_name, is_called)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: callee_name
        logical, intent(inout) :: is_called(:)
        integer :: j
        character(len=:), allocatable :: simple_callee

        simple_callee = simple_name_of(callee_name)
        do j = 1, graph%proc_count
            if (names_match(graph%procedures(j)%name, simple_callee)) then
                is_called(j) = .true.
            end if
        end do
    end subroutine mark_called_procedures

    subroutine collect_unused_simple_names(graph, is_called, temp_names)
        type(call_graph_t), intent(in) :: graph
        logical, intent(in) :: is_called(:)
        character(len=max_proc_name_len), intent(out) :: temp_names(:)
        integer :: i
        integer :: pos

        pos = 0
        do i = 1, graph%proc_count
            if (is_called(i)) cycle
            pos = pos + 1
            temp_names(pos) = simple_name_of(graph%procedures(i)%name)
        end do
    end subroutine collect_unused_simple_names

    pure function simple_name_of(name) result(simple_name)
        character(len=*), intent(in) :: name
        character(len=max_proc_name_len) :: simple_name
        integer :: sep

        simple_name = trim(name)
        sep = index(simple_name, '::', back=.true.)
        if (sep > 0) simple_name = simple_name(sep + 2:)
        simple_name = trim(simple_name)
    end function simple_name_of

    pure logical function names_match(full_name, target_simple)
        character(len=*), intent(in) :: full_name
        character(len=*), intent(in) :: target_simple

        character(len=max_proc_name_len) :: candidate

        if (trim(full_name) == trim(target_simple)) then
            names_match = .true.
            return
        end if

        candidate = simple_name_of(full_name)
        names_match = trim(candidate) == trim(target_simple)
    end function names_match

    function get_callers(graph, procedure_name) result(caller_names)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: procedure_name
        character(len=:), allocatable :: caller_names(:)
        character(len=max_proc_name_len), allocatable :: temp_names(:)
        integer :: i, count, sep_pos
        character(len=max_proc_name_len) :: simple_callee

        allocate (temp_names(graph%call_count))
        count = 0

        do i = 1, graph%call_count
            simple_callee = graph%calls(i)%callee
            sep_pos = index(simple_callee, "::", back=.true.)
            if (sep_pos > 0) simple_callee = simple_callee(sep_pos + 2:)

            if (graph%calls(i)%callee == procedure_name .or. &
                trim(simple_callee) == procedure_name) then
                block
                    character(len=max_proc_name_len) :: extracted_caller
                    extracted_caller = graph%calls(i)%caller
                    sep_pos = index(extracted_caller, "::", back=.true.)
                    if (sep_pos > 0) &
                        extracted_caller = extracted_caller(sep_pos + 2:)

                    if (.not. any(temp_names(1:count) == trim(extracted_caller))) then
                        count = count + 1
                        temp_names(count) = trim(extracted_caller)
                    end if
                end block
            end if
        end do

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

    function get_callees(graph, procedure_name) result(callee_names)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: procedure_name
        character(len=:), allocatable :: callee_names(:)
        character(len=max_proc_name_len), allocatable :: temp_names(:)
        integer :: i, count, sep_pos
        character(len=max_proc_name_len) :: simple_caller

        allocate (temp_names(graph%call_count))
        count = 0

        do i = 1, graph%call_count
            simple_caller = graph%calls(i)%caller
            sep_pos = index(simple_caller, "::", back=.true.)
            if (sep_pos > 0) simple_caller = simple_caller(sep_pos + 2:)

            if (graph%calls(i)%caller == procedure_name .or. &
                trim(simple_caller) == procedure_name) then
                block
                    character(len=max_proc_name_len) :: extracted_callee
                    extracted_callee = graph%calls(i)%callee
                    sep_pos = index(extracted_callee, "::", back=.true.)
                    if (sep_pos > 0) &
                        extracted_callee = extracted_callee(sep_pos + 2:)

                    if (.not. any(temp_names(1:count) == trim(extracted_callee))) then
                        count = count + 1
                        temp_names(count) = trim(extracted_callee)
                    end if
                end block
            end if
        end do

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

    function is_procedure_used(graph, procedure_name) result(is_used)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: procedure_name
        logical :: is_used
        integer :: i, sep_pos
        character(len=max_proc_name_len) :: simple_name, simple_callee

        do i = 1, graph%proc_count
            simple_name = graph%procedures(i)%name
            sep_pos = index(simple_name, "::", back=.true.)
            if (sep_pos > 0) simple_name = simple_name(sep_pos + 2:)

            if ((graph%procedures(i)%name == procedure_name .or. &
                 trim(simple_name) == procedure_name) .and. &
                graph%procedures(i)%is_main_program) then
                is_used = .true.
                return
            end if
        end do

        do i = 1, graph%call_count
            simple_callee = graph%calls(i)%callee
            sep_pos = index(simple_callee, "::", back=.true.)
            if (sep_pos > 0) simple_callee = simple_callee(sep_pos + 2:)

            if (graph%calls(i)%callee == procedure_name .or. &
                trim(simple_callee) == procedure_name) then
                is_used = .true.
                return
            end if
        end do

        is_used = .false.
    end function is_procedure_used

    function get_all_procedures(graph) result(proc_names)
        type(call_graph_t), intent(in) :: graph
        character(len=:), allocatable :: proc_names(:)
        integer :: i, max_len, sep_pos
        character(len=max_proc_name_len) :: simple_name

        if (graph%proc_count > 0) then
            max_len = 0
            do i = 1, graph%proc_count
                if (allocated(graph%procedures(i)%name)) then
                    simple_name = graph%procedures(i)%name
                    sep_pos = index(simple_name, "::", back=.true.)
                    if (sep_pos > 0) simple_name = simple_name(sep_pos + 2:)
                    max_len = max(max_len, len_trim(simple_name))
                end if
            end do

            allocate (character(len=max_len) :: proc_names(graph%proc_count))
            do i = 1, graph%proc_count
                if (allocated(graph%procedures(i)%name)) then
                    simple_name = graph%procedures(i)%name
                    sep_pos = index(simple_name, "::", back=.true.)
                    if (sep_pos > 0) simple_name = simple_name(sep_pos + 2:)
                    proc_names(i) = trim(simple_name)
                else
                    proc_names(i) = ""
                end if
            end do
        else
            allocate (character(len=1) :: proc_names(0))
        end if
    end function get_all_procedures

    function get_call_count(graph) result(count)
        type(call_graph_t), intent(in) :: graph
        integer :: count

        count = graph%call_count
    end function get_call_count

    subroutine print_call_graph(graph, unit)
        type(call_graph_t), intent(in) :: graph
        integer, intent(in), optional :: unit
        integer :: out_unit, i, j
        character(len=:), allocatable :: callers(:), callees(:)

        out_unit = 6
        if (present(unit)) out_unit = unit

        write (out_unit, '(A)') "=== Call Graph ==="
        write (out_unit, '(A,I0)') "Total procedures: ", graph%proc_count
        write (out_unit, '(A,I0)') "Total calls: ", graph%call_count
        write (out_unit, *)

        write (out_unit, '(A)') "Procedures:"
        do i = 1, graph%proc_count
            write (out_unit, '(A,A)', advance='no') "  ", graph%procedures(i)%name
            if (graph%procedures(i)%is_main_program) &
                write (out_unit, '(A)', advance='no') ' [MAIN]'
            if (graph%procedures(i)%is_intrinsic) &
                write (out_unit, '(A)', advance='no') ' [INTRINSIC]'
            if (graph%procedures(i)%is_external) &
                write (out_unit, '(A)', advance='no') ' [EXTERNAL]'
            write (out_unit, '(A,I0,A,I0,A)') " (line ", graph%procedures(i)%line, &
                ", col ", graph%procedures(i)%column, ")"

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
    end subroutine print_call_graph

    function find_recursive_cycles(graph) result(cycles)
        type(call_graph_t), intent(in) :: graph
        character(len=:), allocatable :: cycles(:)

        character(len=max_proc_name_len), allocatable :: temp_cycles(:)
        logical, allocatable :: visited(:), in_stack(:)
        integer :: cycle_count, i

        allocate (visited(graph%proc_count))
        allocate (in_stack(graph%proc_count))
        allocate (temp_cycles(graph%proc_count))

        visited = .false.
        in_stack = .false.
        cycle_count = 0

        do i = 1, graph%proc_count
            if (.not. visited(i)) then
                call dfs_cycle_detect(graph, i, visited, in_stack, &
                                      temp_cycles, cycle_count)
            end if
        end do

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

    subroutine dfs_cycle_detect(graph, proc_idx, visited, in_stack, cycles, &
                                cycle_count)
        type(call_graph_t), intent(in) :: graph
        integer, intent(in) :: proc_idx
        logical, intent(inout) :: visited(:), in_stack(:)
        character(len=max_proc_name_len), intent(inout) :: cycles(:)
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
        character(len=max_proc_name_len) :: caller_name
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
                    if (cycle_count <= size(cycles)) cycles(cycle_count) = caller_name
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

end module call_graph_queries_mod
