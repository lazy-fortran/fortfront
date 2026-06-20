module call_graph_builder_mod
    use call_graph_core_mod, only: call_graph_t
    use call_graph_builder_state_mod, only: call_graph_builder_t
    use call_graph_builder_state_mod, only: create_call_graph_builder
    use call_graph_builder_state_mod, only: add_symbol_entry
    use call_graph_builder_state_mod, only: add_call_with_resolution
    use call_graph_builder_state_mod, only: resolve_deferred_calls
    use call_graph_builder_state_mod, only: compute_full_name
    use call_graph_builder_state_mod, only: resolve_procedure_symbol
    use call_graph_builder_postprocess_mod, only: handle_missing_nested_procedures
    use call_graph_builder_postprocess_mod, only: detect_recursive_calls
    use call_graph_builder_postprocess_mod, only: normalize_program_scopes
    use call_graph_constants_mod, only: max_proc_name_len
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

        if (.not. arena%has_node_at(node_index)) return

        capacity = 128
        allocate (stack(capacity))
        top = 1
        stack(top)%node_index = node_index
        stack(top)%scope_symbol = current_scope_symbol

        do while (top > 0)
            item = stack(top)
            top = top - 1

            if (.not. arena%has_node_at(item%node_index)) cycle

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
        case ('function_def', 'function', 'subroutine_def', 'subroutine')
            call handle_procedure_node(builder, arena, node_idx, scope_sym, &
                                       stack, top, &
                                       capacity, children)
        case ('subroutine_call', 'call')
            call handle_call_node(builder, arena, node_idx, scope_sym)
        case ('call_or_subscript')
            call handle_call_or_subscript_node(builder, arena, node_idx, scope_sym)
        case ('assignment', 'assignment_node')
            call process_assignment_node(arena, node_idx, scope_sym, stack, top, &
                                         capacity)
        case ('binary_op', 'binary_op_node')
            call process_binary_op_node(arena, node_idx, scope_sym, stack, top, &
                                        capacity)
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

    subroutine process_assignment_node(arena, node_idx, scope_sym, stack, top, &
                                       capacity)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        integer, intent(in) :: scope_sym
        type(stack_item_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: top
        integer, intent(inout) :: capacity

        select type (node => arena%entries(node_idx)%node)
        type is (assignment_node)
            call push_on_stack(arena, node%value_index, scope_sym, stack, top, &
                               capacity)
            call push_on_stack(arena, node%target_index, scope_sym, stack, top, &
                               capacity)
        end select
    end subroutine process_assignment_node

    subroutine process_binary_op_node(arena, node_idx, scope_sym, stack, top, &
                                      capacity)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        integer, intent(in) :: scope_sym
        type(stack_item_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: top
        integer, intent(inout) :: capacity

        select type (node => arena%entries(node_idx)%node)
        type is (binary_op_node)
            call push_on_stack(arena, node%right_index, scope_sym, stack, top, &
                               capacity)
            call push_on_stack(arena, node%left_index, scope_sym, stack, top, &
                               capacity)
        end select
    end subroutine process_binary_op_node

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
            call push_body_or_children(arena, node_idx, symbol_id, node%body_indices, &
                                       stack, top, capacity, children)
        end select
    end subroutine handle_program_node

    subroutine handle_procedure_node(builder, arena, node_idx, scope_sym, stack, top, &
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

        select type (node => arena%entries(node_idx)%node)
        type is (function_def_node)
            call process_routine(node%name, node%line, node%column, node%body_indices)
        type is (subroutine_def_node)
            call process_routine(node%name, node%line, node%column, node%body_indices)
        end select
    contains
        subroutine process_routine(name, line, column, body_indices)
            character(len=*), intent(in) :: name
            integer, intent(in) :: line, column
            integer, allocatable, intent(in) :: body_indices(:)

            full_name = compute_full_name(builder, scope_sym, name)
            call builder%graph%add_proc(full_name, node_idx, line, column)
            call add_symbol_entry(builder, name, full_name, scope_sym, node_idx, &
                                  .true., symbol_id)
            call push_body_or_children(arena, node_idx, symbol_id, body_indices, &
                                       stack, top, capacity, children)
        end subroutine process_routine
    end subroutine handle_procedure_node

    subroutine handle_call_node(builder, arena, node_idx, scope_sym)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        integer, intent(in) :: scope_sym
        character(len=max_proc_name_len) :: caller_name
        integer :: resolved_symbol

        select type (node => arena%entries(node_idx)%node)
        type is (subroutine_call_node)
            if (scope_sym > 0 .and. allocated(builder%symbol_table)) then
                if (scope_sym <= size(builder%symbol_table)) then
                    caller_name = builder%symbol_table(scope_sym)%full_name
                    resolved_symbol = resolve_procedure_symbol(builder, node%name, &
                                                               scope_sym)
                    call add_call_with_resolution(builder, scope_sym, &
                                                  trim(caller_name), node%name, &
                                                  resolved_symbol, node_idx, &
                                                  node%line, node%column)
                end if
            end if
        end select
    end subroutine handle_call_node

    subroutine handle_call_or_subscript_node(builder, arena, node_idx, scope_sym)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        integer, intent(in) :: scope_sym
        character(len=max_proc_name_len) :: caller_name
        integer :: resolved_symbol

        select type (node => arena%entries(node_idx)%node)
        type is (call_or_subscript_node)
            if (scope_sym > 0 .and. .not. node%is_array_access &
                .and. allocated(builder%symbol_table)) then
                if (scope_sym <= size(builder%symbol_table)) then
                    caller_name = builder%symbol_table(scope_sym)%full_name
                    resolved_symbol = resolve_procedure_symbol(builder, node%name, &
                                                               scope_sym)
                    call add_call_with_resolution(builder, scope_sym, &
                                                  trim(caller_name), node%name, &
                                                  resolved_symbol, node_idx, &
                                                  node%line, node%column)
                end if
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

    subroutine push_body_or_children(arena, node_idx, symbol_id, body_indices, stack, &
                                     top, capacity, children)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_idx
        integer, intent(in) :: symbol_id
        integer, allocatable, intent(in), optional :: body_indices(:)
        type(stack_item_t), allocatable, intent(inout) :: stack(:)
        integer, intent(inout) :: top
        integer, intent(inout) :: capacity
        integer, allocatable, intent(inout) :: children(:)
        integer :: i

        if (present(body_indices)) then
            if (allocated(body_indices)) then
                do i = size(body_indices), 1, -1
                    call push_on_stack(arena, body_indices(i), symbol_id, stack, top, &
                                       capacity)
                end do
                return
            end if
        end if

        call push_children_on_stack(arena, node_idx, symbol_id, stack, top, &
                                    capacity, children)
    end subroutine push_body_or_children

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

end module call_graph_builder_mod
