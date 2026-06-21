module call_graph_builder_postprocess_mod
    use call_graph_builder_state_mod, only: call_graph_builder_t
    use call_graph_builder_state_mod, only: add_symbol_entry
    use call_graph_builder_state_mod, only: add_call_with_resolution
    use call_graph_builder_state_mod, only: compute_full_name
    use call_graph_builder_state_mod, only: find_symbol_any_parent
    use call_graph_builder_state_mod, only: find_symbol_by_full_name
    use call_graph_builder_state_mod, only: extract_simple_name
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_data, only: MULTI_UNIT_NAME
    use call_graph_constants_mod, only: max_proc_name_len
    implicit none
    private

    public :: handle_missing_nested_procedures
    public :: detect_recursive_calls
    public :: normalize_program_scopes
    public :: procedure_declares_recursive

    type :: rename_entry_t
        character(len=:), allocatable :: old_name
        character(len=:), allocatable :: new_name
    end type rename_entry_t

contains

    subroutine handle_missing_nested_procedures(builder, arena)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena

        integer :: i
        integer :: caller_symbol
        integer :: callee_symbol
        integer :: parent_symbol
        integer :: new_symbol
        character(len=max_proc_name_len) :: caller_name
        character(len=max_proc_name_len) :: callee_name
        character(len=:), allocatable :: simple_callee
        character(len=:), allocatable :: inferred_full_name

        do i = 1, builder%graph%call_count
            caller_name = builder%graph%calls(i)%caller
            callee_name = builder%graph%calls(i)%callee

            simple_callee = extract_simple_name(callee_name)
            callee_symbol = find_symbol_any_parent(builder, simple_callee)

            if (callee_symbol > 0) cycle

            caller_symbol = find_symbol_by_full_name(builder, caller_name)
            parent_symbol = 0
            if (caller_symbol > 0 .and. allocated(builder%symbol_table)) then
                if (caller_symbol <= size(builder%symbol_table)) then
                    parent_symbol = builder%symbol_table(caller_symbol)%parent_symbol
                    if (parent_symbol == 0) parent_symbol = caller_symbol
                end if
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

        integer :: proc_index

        do proc_index = 1, builder%graph%proc_count
            call ensure_recursive_edge(builder, arena, proc_index)
        end do
    end subroutine detect_recursive_calls

    subroutine ensure_recursive_edge(builder, arena, proc_index)
        type(call_graph_builder_t), intent(inout) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_index

        character(len=:), allocatable :: proc_name
        character(len=:), allocatable :: simple_name
        integer :: proc_symbol

        proc_name = builder%graph%procedures(proc_index)%name
        simple_name = extract_simple_name(proc_name)
        proc_symbol = find_symbol_by_full_name(builder, proc_name)

        if (has_explicit_recursive_call(builder, proc_name, simple_name)) return
        if (.not. should_infer_recursive(builder, arena, proc_index, proc_symbol, &
                                         proc_name, simple_name)) return

        if (proc_symbol > 0) then
            call add_call_with_resolution(builder, proc_symbol, trim(proc_name), &
                                          trim(simple_name), proc_symbol, 0, 0, 0)
        end if
    end subroutine ensure_recursive_edge

    logical function has_explicit_recursive_call(builder, proc_name, simple_name)
        type(call_graph_builder_t), intent(in) :: builder
        character(len=*), intent(in) :: proc_name
        character(len=*), intent(in) :: simple_name

        integer :: call_index
        character(len=:), allocatable :: caller_trim
        character(len=:), allocatable :: callee_trim
        character(len=:), allocatable :: callee_simple

        has_explicit_recursive_call = .false.
        do call_index = 1, builder%graph%call_count
            caller_trim = trim(builder%graph%calls(call_index)%caller)
            if (caller_trim /= trim(proc_name)) cycle

            callee_trim = trim(builder%graph%calls(call_index)%callee)
            if (callee_trim == trim(proc_name)) then
                has_explicit_recursive_call = .true.
                return
            end if

            callee_simple = extract_simple_name(callee_trim)
            if (.not. allocated(callee_simple)) cycle
            if (trim(callee_simple) == trim(simple_name)) then
                has_explicit_recursive_call = .true.
                return
            end if
        end do
    end function has_explicit_recursive_call

    logical function should_infer_recursive(builder, arena, proc_index, &
                                            proc_symbol, proc_name, simple_name)
        type(call_graph_builder_t), intent(in) :: builder
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_index
        integer, intent(in) :: proc_symbol
        character(len=*), intent(in) :: proc_name
        character(len=*), intent(in) :: simple_name

        integer :: def_node

        def_node = builder%graph%procedures(proc_index)%definition_node
        if (proc_symbol > 0 .and. def_node <= 0) then
            def_node = builder%symbol_table(proc_symbol)%node_index
        end if

        if (invoked_from_parent_scope(builder, proc_name, simple_name)) then
            should_infer_recursive = .true.
            return
        end if

        should_infer_recursive = procedure_declares_recursive(arena, def_node)
    end function should_infer_recursive

    logical function invoked_from_parent_scope(builder, proc_name, simple_name)
        type(call_graph_builder_t), intent(in) :: builder
        character(len=*), intent(in) :: proc_name
        character(len=*), intent(in) :: simple_name

        character(len=:), allocatable :: parent_scope
        character(len=:), allocatable :: caller_trim
        character(len=:), allocatable :: callee_trim
        character(len=:), allocatable :: callee_simple
        integer :: sep
        integer :: call_index
        integer :: parent_sep

        invoked_from_parent_scope = .false.
        parent_scope = trim(proc_name)
        sep = index(parent_scope, '::', back=.true.)
        if (sep > 0) then
            parent_scope = trim(parent_scope(:sep - 1))
        else
            parent_scope = ''
        end if

        parent_sep = index(parent_scope, '::', back=.true.)
        if (parent_sep > 0) then
            invoked_from_parent_scope = .false.
            return
        end if

        do call_index = 1, builder%graph%call_count
            caller_trim = trim(builder%graph%calls(call_index)%caller)
            if (caller_trim /= trim(parent_scope)) cycle

            callee_trim = trim(builder%graph%calls(call_index)%callee)
            callee_simple = extract_simple_name(callee_trim)
            if (.not. allocated(callee_simple)) cycle
            if (trim(callee_simple) == trim(simple_name)) then
                invoked_from_parent_scope = .true.
                return
            end if
        end do
    end function invoked_from_parent_scope

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
        agg_symbol = find_symbol_by_full_name(builder, MULTI_UNIT_NAME)
        agg_proc_index = builder%graph%find_proc_index(MULTI_UNIT_NAME)

        do i = 1, builder%symbol_count
            if (.not. builder%symbol_table(i)%is_procedure) cycle
            if (builder%symbol_table(i)%parent_symbol /= 0) cycle
            if (.not. allocated(builder%symbol_table(i)%full_name)) cycle
            if (trim(builder%symbol_table(i)%full_name) == MULTI_UNIT_NAME) cycle
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
            if (trim(builder%symbol_table(i)%full_name) == MULTI_UNIT_NAME) cycle
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
            new_name = MULTI_UNIT_NAME//"::"// &
                       trim(builder%symbol_table(i)%simple_name)
            if (old_name == new_name) cycle

            builder%symbol_table(i)%parent_symbol = agg_symbol
            builder%symbol_table(i)%full_name = new_name
            call append_rename(old_name, new_name)
        end do

        if (rename_count == 0) return

        if (agg_proc_index <= 0) then
            call builder%graph%add_proc(MULTI_UNIT_NAME, 0, 0, 0, is_main=.true.)
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
                call add_symbol_entry(builder, MULTI_UNIT_NAME, MULTI_UNIT_NAME, &
                                      0, 0, .true., agg_symbol)
            end if
            agg_proc_index = builder%graph%find_proc_index(MULTI_UNIT_NAME)
            if (agg_proc_index <= 0) then
                call builder%graph%add_proc(MULTI_UNIT_NAME, 0, 0, 0, is_main=.true.)
                agg_proc_index = builder%graph%find_proc_index(MULTI_UNIT_NAME)
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
            integer :: old_len
            character(len=:), allocatable :: suffix

            if (.not. allocated(renames)) return
            do idx = 1, rename_count
                if (trim(name) == renames(idx)%old_name) then
                    name = renames(idx)%new_name
                    exit
                end if
                old_len = len_trim(renames(idx)%old_name)
                if (len_trim(name) > old_len + 2) then
                    if (name(1:old_len) == trim(renames(idx)%old_name) .and. &
                        name(old_len + 1:old_len + 2) == '::') then
                        suffix = name(old_len + 1:len_trim(name))
                        name = trim(renames(idx)%new_name)//trim(suffix)
                        exit
                    end if
                end if
            end do
        end subroutine apply_renames

    end subroutine normalize_program_scopes

    pure logical function procedure_declares_recursive(arena, node_index) &
        result(is_recursive)
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

end module call_graph_builder_postprocess_mod
