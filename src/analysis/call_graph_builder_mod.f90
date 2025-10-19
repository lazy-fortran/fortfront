module call_graph_builder_mod
    use call_graph_core_mod, only: call_graph_t, create_call_graph
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node, assignment_node, binary_op_node, &
                              call_or_subscript_node
    use ast_nodes_data, only: module_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
                                   subroutine_call_node
    implicit none
    private

    public :: build_call_graph_from_ast
    public :: build_call_graph

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

    type, private :: call_graph_builder_t
        type(call_graph_t) :: graph
        type(symbol_entry_t), allocatable :: symbol_table(:)
        integer, allocatable :: node_symbol_map(:)
        integer :: symbol_count = 0
        type(unresolved_call_t), allocatable :: unresolved_calls(:)
        integer :: unresolved_count = 0
    end type call_graph_builder_t

    type, private :: rename_entry_t
        character(len=:), allocatable :: old_name
        character(len=:), allocatable :: new_name
    end type rename_entry_t

    type, private :: stack_item_t
        integer :: node_index = 0
        integer :: scope_symbol = 0
    end type stack_item_t

contains

    subroutine build_call_graph_from_ast(graph, arena, root_index)
        type(call_graph_t), intent(inout) :: graph
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index

        type(call_graph_t) :: built_graph

        built_graph = build_call_graph(arena, root_index)
        graph = built_graph
    end subroutine build_call_graph_from_ast

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
        call normalize_program_scopes(builder, arena)

        graph = builder%graph
    end function build_call_graph

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

    subroutine traverse_for_calls(builder, arena, node_index, current_scope_symbol)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(in) :: current_scope_symbol

        type(stack_item_t), allocatable :: stack(:)
        type(stack_item_t) :: item
        character(len=:), allocatable :: node_type
        integer, allocatable :: children(:)
        integer :: top
        integer :: capacity

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
            call process_node(builder, arena, item%node_index, item%scope_symbol, &
                              node_type, stack, top, capacity, children)
        end do
    end subroutine traverse_for_calls

    subroutine process_node(builder, arena, node_idx, scope_sym, node_type, &
                            stack, top, capacity, children)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        integer, intent(in) :: scope_sym
        character(len=*), intent(in) :: node_type
        type(stack_item_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: top
        integer, intent(inout) :: capacity
        integer, allocatable, intent(inout) :: children(:)

        select case (node_type)
        case ('program', 'program_node')
            call handle_program_node(builder, arena, node_idx, stack, top, &
                                     capacity, children)
        case ('function_def', 'function')
            call handle_function_node(builder, arena, node_idx, scope_sym, stack, &
                                      top, capacity, children)
        case ('subroutine_def', 'subroutine')
            call handle_subroutine_node(builder, arena, node_idx, scope_sym, stack, &
                                        top, capacity, children)
        case ('subroutine_call', 'call')
            call handle_call_node(builder, arena, node_idx, scope_sym)
        case ('call_or_subscript')
            call handle_call_or_subscript_node(builder, arena, node_idx, scope_sym)
        case ('assignment', 'assignment_node')
            select type (node => arena%entries(node_idx)%node)
            type is (assignment_node)
                call push_on_stack(arena, node%value_index, scope_sym, stack, top, &
                                   capacity)
                call push_on_stack(arena, node%target_index, scope_sym, stack, top, &
                                   capacity)
            end select
        case ('binary_op', 'binary_op_node')
            select type (node => arena%entries(node_idx)%node)
            type is (binary_op_node)
                call push_on_stack(arena, node%right_index, scope_sym, stack, top, &
                                   capacity)
                call push_on_stack(arena, node%left_index, scope_sym, stack, top, &
                                   capacity)
            end select
        case ('module', 'module_node')
            call handle_module_node(builder, arena, node_idx, stack, top, capacity, &
                                    children)
        case ('contains', 'contains_section', 'contains_node')
            call push_children_on_stack(arena, node_idx, scope_sym, stack, top, &
                                        capacity, children)
        case default
            call push_children_on_stack(arena, node_idx, scope_sym, stack, top, &
                                        capacity, children)
        end select
    end subroutine process_node

    subroutine handle_program_node(builder, arena, node_idx, stack, top, capacity, &
                                    children)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        type(stack_item_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: top
        integer, intent(inout) :: capacity
        integer, allocatable, intent(inout) :: children(:)
        integer :: symbol_id
        integer :: i

        select type (node => arena%entries(node_idx)%node)
        type is (program_node)
            call builder%graph%add_proc(node%name, node_idx, node%line, node%column, &
                                        is_main=.true.)
            call add_symbol_entry(builder, node%name, node%name, 0, node_idx, &
                                  .true., symbol_id)
            if (allocated(node%body_indices)) then
                do i = size(node%body_indices), 1, -1
                    call push_on_stack(arena, node%body_indices(i), symbol_id, stack, &
                                       top, capacity)
                end do
            else
                call push_children_on_stack(arena, node_idx, symbol_id, stack, top, &
                                            capacity, children)
            end if
        end select
    end subroutine handle_program_node

    subroutine handle_function_node(builder, arena, node_idx, scope_sym, stack, top, &
                                     capacity, children)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        integer, intent(in) :: scope_sym
        type(stack_item_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: top
        integer, intent(inout) :: capacity
        integer, allocatable, intent(inout) :: children(:)
        character(len=:), allocatable :: full_name
        integer :: symbol_id
        integer :: i

        select type (node => arena%entries(node_idx)%node)
        type is (function_def_node)
            full_name = compute_full_name(builder, scope_sym, node%name)
            call builder%graph%add_proc(full_name, node_idx, node%line, node%column)
            call add_symbol_entry(builder, node%name, full_name, scope_sym, node_idx, &
                                  .true., symbol_id)
            if (allocated(node%body_indices)) then
                do i = size(node%body_indices), 1, -1
                    call push_on_stack(arena, node%body_indices(i), symbol_id, stack, &
                                       top, capacity)
                end do
            else
                call push_children_on_stack(arena, node_idx, symbol_id, stack, top, &
                                            capacity, children)
            end if
        end select
    end subroutine handle_function_node

    subroutine handle_subroutine_node(builder, arena, node_idx, scope_sym, stack, &
                                       top, capacity, children)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        integer, intent(in) :: scope_sym
        type(stack_item_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: top
        integer, intent(inout) :: capacity
        integer, allocatable, intent(inout) :: children(:)
        character(len=:), allocatable :: full_name
        integer :: symbol_id
        integer :: i

        select type (node => arena%entries(node_idx)%node)
        type is (subroutine_def_node)
            full_name = compute_full_name(builder, scope_sym, node%name)
            call builder%graph%add_proc(full_name, node_idx, node%line, node%column)
            call add_symbol_entry(builder, node%name, full_name, scope_sym, node_idx, &
                                  .true., symbol_id)
            if (allocated(node%body_indices)) then
                do i = size(node%body_indices), 1, -1
                    call push_on_stack(arena, node%body_indices(i), symbol_id, stack, &
                                       top, capacity)
                end do
            else
                call push_children_on_stack(arena, node_idx, symbol_id, stack, top, &
                                            capacity, children)
            end if
        end select
    end subroutine handle_subroutine_node

    subroutine handle_call_node(builder, arena, node_idx, scope_sym)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        integer, intent(in) :: scope_sym
        character(len=256) :: caller_name
        integer :: resolved_symbol

        select type (node => arena%entries(node_idx)%node)
        type is (subroutine_call_node)
            if (scope_sym > 0) then
                caller_name = builder%symbol_table(scope_sym)%full_name
                resolved_symbol = resolve_procedure_symbol(builder, node%name, &
                                                           scope_sym)
                call add_call_with_resolution(builder, scope_sym, trim(caller_name), &
                                              node%name, resolved_symbol, node_idx, &
                                              node%line, node%column)
            end if
        end select
    end subroutine handle_call_node

    subroutine handle_call_or_subscript_node(builder, arena, node_idx, scope_sym)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        integer, intent(in) :: scope_sym
        character(len=256) :: caller_name
        integer :: resolved_symbol

        select type (node => arena%entries(node_idx)%node)
        type is (call_or_subscript_node)
            if (scope_sym > 0 .and. .not. node%is_array_access) then
                caller_name = builder%symbol_table(scope_sym)%full_name
                resolved_symbol = resolve_procedure_symbol(builder, node%name, &
                                                           scope_sym)
                call add_call_with_resolution(builder, scope_sym, trim(caller_name), &
                                              node%name, resolved_symbol, node_idx, &
                                              node%line, node%column)
            end if
        end select
    end subroutine handle_call_or_subscript_node

    subroutine handle_module_node(builder, arena, node_idx, stack, top, capacity, &
                                   children)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        type(stack_item_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: top
        integer, intent(inout) :: capacity
        integer, allocatable, intent(inout) :: children(:)
        integer :: symbol_id
        integer :: i

        select type (node => arena%entries(node_idx)%node)
        type is (module_node)
            call add_symbol_entry(builder, node%name, node%name, 0, node_idx, &
                                  .false., symbol_id)
            if (allocated(node%procedure_indices)) then
                do i = size(node%procedure_indices), 1, -1
                    call push_on_stack(arena, node%procedure_indices(i), symbol_id, &
                                       stack, top, capacity)
                end do
            end if
            if (allocated(node%declaration_indices)) then
                do i = size(node%declaration_indices), 1, -1
                    call push_on_stack(arena, node%declaration_indices(i), symbol_id, &
                                       stack, top, capacity)
                end do
            end if
            call push_children_on_stack(arena, node_idx, symbol_id, stack, top, &
                                        capacity, children)
        end select
    end subroutine handle_module_node

    subroutine push_on_stack(arena, idx, scope_value, stack, top, capacity)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        integer, intent(in) :: scope_value
        type(stack_item_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: top
        integer, intent(inout) :: capacity
        type(stack_item_t), allocatable :: tmp(:)

        if (idx <= 0) return
        if (.not. allocated(arena%entries(idx)%node)) return

        if (top >= capacity) then
            capacity = capacity * 2
            allocate (tmp(capacity))
            if (top > 0) tmp(1:top) = stack(1:top)
            call move_alloc(tmp, stack)
        end if

        top = top + 1
        stack(top)%node_index = idx
        stack(top)%scope_symbol = scope_value
    end subroutine push_on_stack

    subroutine push_children_on_stack(arena, parent_index, scope_value, stack, top, &
                                       capacity, children)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: parent_index
        integer, intent(in) :: scope_value
        type(stack_item_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: top
        integer, intent(inout) :: capacity
        integer, allocatable, intent(inout) :: children(:)
        integer :: j

        children = arena%get_children(parent_index)
        if (allocated(children)) then
            do j = size(children), 1, -1
                call push_on_stack(arena, children(j), scope_value, stack, top, &
                                   capacity)
            end do
        end if
    end subroutine push_children_on_stack

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

    subroutine normalize_program_scopes(builder, arena)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena

        integer, allocatable :: program_symbols(:)
        type(rename_entry_t), allocatable :: renames(:)
        integer :: program_count
        integer :: rename_count
        integer :: agg_symbol
        integer :: agg_proc_index
        integer :: i
        integer :: root_symbol
        integer :: parent_symbol
        integer :: node_index
        character(len=:), allocatable :: old_name
        character(len=:), allocatable :: new_name

        program_count = 0
        rename_count = 0
        agg_symbol = find_symbol_by_full_name(builder, "__MULTI_UNIT__")
        agg_proc_index = builder%graph%find_proc_index("__MULTI_UNIT__")

        do i = 1, builder%symbol_count
            if (.not. builder%symbol_table(i)%is_procedure) cycle
            if (builder%symbol_table(i)%parent_symbol /= 0) cycle
            if (.not. allocated(builder%symbol_table(i)%full_name)) cycle
            if (trim(builder%symbol_table(i)%full_name) == "__MULTI_UNIT__") cycle
            node_index = builder%symbol_table(i)%node_index
            if (node_index <= 0) cycle
            if (node_index > arena%size) cycle
            if (.not. allocated(arena%entries(node_index)%node)) cycle
            select case (trim(arena%entries(node_index)%node_type))
            case ("program", "program_node")
                call append_program_symbol(i)
            end select
        end do

        if (program_count == 0) return

        do i = 1, builder%symbol_count
            if (.not. builder%symbol_table(i)%is_procedure) cycle
            if (.not. allocated(builder%symbol_table(i)%full_name)) cycle
            if (trim(builder%symbol_table(i)%full_name) == "__MULTI_UNIT__") cycle
            if (is_program_symbol(i)) cycle

            root_symbol = i
            do
                parent_symbol = builder%symbol_table(root_symbol)%parent_symbol
                if (parent_symbol <= 0) exit
                root_symbol = parent_symbol
            end do

            if (.not. is_program_symbol(root_symbol)) cycle

            if (agg_symbol <= 0) call ensure_aggregator()
            old_name = trim(builder%symbol_table(i)%full_name)
            new_name = "__MULTI_UNIT__::" // trim(builder%symbol_table(i)%simple_name)
            if (old_name == new_name) cycle

            builder%symbol_table(i)%parent_symbol = agg_symbol
            builder%symbol_table(i)%full_name = new_name
            call append_rename(old_name, new_name)
        end do

        if (rename_count == 0) return

        if (agg_proc_index <= 0) then
            call builder%graph%add_proc("__MULTI_UNIT__", 0, 0, 0, is_main=.true.)
        end if

        do i = 1, builder%graph%proc_count
            call apply_renames(builder%graph%procedures(i)%name)
        end do

        do i = 1, builder%graph%call_count
            call apply_renames(builder%graph%calls(i)%caller)
            call apply_renames(builder%graph%calls(i)%callee)
        end do

    contains

        subroutine append_program_symbol(symbol_id)
            integer, intent(in) :: symbol_id
            integer, allocatable :: temp(:)

            if (.not. allocated(program_symbols)) then
                allocate (program_symbols(1))
            else
                allocate (temp(program_count + 1))
                if (program_count > 0) temp(1:program_count) = program_symbols
                call move_alloc(temp, program_symbols)
            end if
            program_count = program_count + 1
            program_symbols(program_count) = symbol_id
        end subroutine append_program_symbol

        logical function is_program_symbol(symbol_id)
            integer, intent(in) :: symbol_id
            integer :: idx

            is_program_symbol = .false.
            if (.not. allocated(program_symbols)) return
            do idx = 1, program_count
                if (program_symbols(idx) == symbol_id) then
                    is_program_symbol = .true.
                    exit
                end if
            end do
        end function is_program_symbol

        subroutine ensure_aggregator()
            if (agg_symbol <= 0) then
                call add_symbol_entry(builder, "__MULTI_UNIT__", "__MULTI_UNIT__", &
                                      0, 0, .true., agg_symbol)
            end if
            agg_proc_index = builder%graph%find_proc_index("__MULTI_UNIT__")
            if (agg_proc_index <= 0) then
                call builder%graph%add_proc("__MULTI_UNIT__", 0, 0, 0, is_main=.true.)
                agg_proc_index = builder%graph%find_proc_index("__MULTI_UNIT__")
            end if
        end subroutine ensure_aggregator

        subroutine append_rename(old_name_in, new_name_in)
            character(len=*), intent(in) :: old_name_in
            character(len=*), intent(in) :: new_name_in
            type(rename_entry_t), allocatable :: temp(:)

            if (.not. allocated(renames)) then
                allocate (renames(1))
            else
                allocate (temp(rename_count + 1))
                if (rename_count > 0) temp(1:rename_count) = renames
                call move_alloc(temp, renames)
            end if

            rename_count = rename_count + 1
            renames(rename_count)%old_name = trim(old_name_in)
            renames(rename_count)%new_name = trim(new_name_in)
        end subroutine append_rename

        subroutine apply_renames(name)
            character(len=:), allocatable, intent(inout) :: name
            integer :: idx

            if (.not. allocated(renames)) return
            do idx = 1, rename_count
                if (trim(name) == renames(idx)%old_name) then
                    name = renames(idx)%new_name
                    exit
                end if
            end do
        end subroutine apply_renames

    end subroutine normalize_program_scopes

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
                    logical :: declares_recursive
                    integer :: def_node

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
                    def_node = builder%graph%procedures(i)%definition_node

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

                    proc_symbol = find_symbol_by_full_name(builder, proc_name)
                    if (proc_symbol > 0) then
                        if (def_node <= 0) then
                            def_node = builder%symbol_table(proc_symbol)%node_index
                        end if
                    end if
                    declares_recursive = procedure_declares_recursive(arena, def_node)
                    if (declares_recursive) should_be_recursive = .true.

                    if (should_be_recursive) then
                        if (proc_symbol > 0) then
                            call add_call_with_resolution(builder, proc_symbol, &
                                                          proc_name, simple_name, &
                                                          proc_symbol, 0, 0, 0)
                        end if
                    end if
                end block
            end if
        end do
    end subroutine detect_recursive_calls

    pure logical function procedure_declares_recursive(arena, node_index) &
        & result(is_recursive)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: i

        is_recursive = .false.
        if (node_index <= 0) return
        if (node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (proc_node => arena%entries(node_index)%node)
        type is (function_def_node)
            if (proc_node%is_recursive) then
                is_recursive = .true.
                return
            end if
            if (.not. allocated(proc_node%prefix_keywords)) return
            do i = 1, size(proc_node%prefix_keywords)
                if (trim(proc_node%prefix_keywords(i)) == 'recursive') then
                    is_recursive = .true.
                    return
                end if
            end do
        type is (subroutine_def_node)
            if (proc_node%is_recursive) then
                is_recursive = .true.
                return
            end if
            if (.not. allocated(proc_node%prefix_keywords)) return
            do i = 1, size(proc_node%prefix_keywords)
                if (trim(proc_node%prefix_keywords(i)) == 'recursive') then
                    is_recursive = .true.
                    return
                end if
            end do
        class default
        end select
    end function procedure_declares_recursive

end module call_graph_builder_mod
