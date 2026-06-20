module call_graph_builder_state_mod
    use call_graph_core_mod, only: call_graph_t, create_call_graph
    use call_graph_constants_mod, only: initial_symbol_table_capacity
    implicit none
    private

    public :: call_graph_builder_t
    public :: symbol_entry_t
    public :: unresolved_call_t
    public :: create_call_graph_builder
    public :: add_symbol_entry
    public :: add_call_with_resolution
    public :: resolve_deferred_calls
    public :: compute_full_name
    public :: resolve_procedure_symbol
    public :: find_symbol_any_parent
    public :: find_symbol_by_full_name
    public :: extract_simple_name

    type :: symbol_entry_t
        character(len=:), allocatable :: simple_name
        character(len=:), allocatable :: full_name
        integer :: parent_symbol = 0
        integer :: node_index = 0
        logical :: is_procedure = .false.
    end type symbol_entry_t

    type :: unresolved_call_t
        integer :: call_index = 0
        character(len=:), allocatable :: callee_simple
        integer :: scope_symbol = 0
    end type unresolved_call_t

    type :: call_graph_builder_t
        type(call_graph_t) :: graph
        type(symbol_entry_t), allocatable :: symbol_table(:)
        integer, allocatable :: node_symbol_map(:)
        integer :: symbol_count = 0
        type(unresolved_call_t), allocatable :: unresolved_calls(:)
        integer :: unresolved_count = 0
    end type call_graph_builder_t

contains

    function create_call_graph_builder(arena_size) result(builder)
        integer, intent(in) :: arena_size
        type(call_graph_builder_t) :: builder

        builder%graph = create_call_graph()
        allocate (builder%symbol_table(initial_symbol_table_capacity))
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
            allocate (builder%symbol_table(initial_symbol_table_capacity))
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

    subroutine add_call_with_resolution(builder, scope_symbol, caller_name, &
                                        callee_simple, resolved_symbol, call_node, &
                                        line, column)
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

        callee_name = trim(callee_simple)
        if (resolved_symbol > 0 .and. allocated(builder%symbol_table)) then
            if (resolved_symbol <= size(builder%symbol_table)) then
                callee_name = builder%symbol_table(resolved_symbol)%full_name
            end if
        end if

        call builder%graph%add_call_edge(trim(caller_name), trim(callee_name), &
                                         call_node, line, column)

        call_index = builder%graph%call_count

        if (resolved_symbol <= 0) then
            call register_unresolved_call(builder, call_index, callee_simple, &
                                          scope_symbol)
        end if
    end subroutine add_call_with_resolution

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

    subroutine resolve_deferred_calls(builder)
        type(call_graph_builder_t), intent(inout) :: builder

        integer :: i
        integer :: symbol_id
        integer :: call_index
        integer :: scope_symbol
        character(len=:), allocatable :: simple_name

        if (.not. allocated(builder%unresolved_calls)) return

        do i = 1, builder%unresolved_count
            call_index = builder%unresolved_calls(i)%call_index
            if (call_index <= 0) cycle
            if (call_index > builder%graph%call_count) cycle
            if (.not. allocated(builder%unresolved_calls(i)%callee_simple)) cycle

            simple_name = builder%unresolved_calls(i)%callee_simple

            scope_symbol = builder%unresolved_calls(i)%scope_symbol
            symbol_id = resolve_procedure_symbol(builder, simple_name, scope_symbol)
            if (symbol_id <= 0) cycle

            builder%graph%calls(call_index)%callee = &
                builder%symbol_table(symbol_id)%full_name
            builder%unresolved_calls(i)%call_index = 0
        end do
    end subroutine resolve_deferred_calls

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

    integer function find_symbol_any_parent(builder, simple_name) result(symbol_id)
        type(call_graph_builder_t), intent(in) :: builder
        character(len=*), intent(in) :: simple_name

        integer :: i

        symbol_id = 0
        do i = 1, builder%symbol_count
            if (.not. builder%symbol_table(i)%is_procedure) cycle
            if (.not. allocated(builder%symbol_table(i)%simple_name)) cycle
            if (trim(builder%symbol_table(i)%simple_name) /= trim(simple_name)) cycle
            symbol_id = i
            return
        end do
    end function find_symbol_any_parent

    integer function find_symbol_by_full_name(builder, full_name) result(symbol_id)
        type(call_graph_builder_t), intent(in) :: builder
        character(len=*), intent(in) :: full_name

        integer :: i

        symbol_id = 0
        do i = 1, builder%symbol_count
            if (.not. allocated(builder%symbol_table(i)%full_name)) cycle
            if (trim(builder%symbol_table(i)%full_name) /= trim(full_name)) cycle
            symbol_id = i
            return
        end do
    end function find_symbol_by_full_name

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

end module call_graph_builder_state_mod
