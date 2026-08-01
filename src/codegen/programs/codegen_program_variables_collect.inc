    function collect_program_variable_decls(arena, prog, header_code) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        character(len=*), intent(in), optional :: header_code
        character(len=:), allocatable :: decl_code
        type(program_decl_state_t) :: state
        decl_code = ""
        if (.not. allocated(prog%body_indices)) return

        call initialize_program_decl_state(state)
        if (present(header_code)) then
            call seed_namelist_groups_from_text(state, header_code)
        end if
        call populate_defined_function_table(arena, state)
        call collect_use_associated_symbols(arena, prog, state)
        call collect_local_module_exports(arena, prog, state)
        call collect_auto_module_exports(arena, state)
        call collect_declared_symbols(arena, prog, state)
        call collect_derived_type_names(arena, prog, state)
        call collect_associate_names(arena, prog, state)
        call collect_namelist_groups(arena, prog, state)
        call collect_assignment_symbols(arena, prog, state)
        call collect_executable_identifier_symbols(arena, prog, state)

        if (state%var_count == 0 .and. state%func_count == 0) return

        decl_code = emit_program_declarations(state)
    end function collect_program_variable_decls

    subroutine populate_defined_function_table(arena, state)
        type(ast_arena_t), intent(in) :: arena
        type(program_decl_state_t), intent(inout) :: state

        call build_function_return_type_table(arena, state%defined_func_names, &
                                              state%defined_func_types, &
                                              state%defined_func_count)
    end subroutine populate_defined_function_table

    ! Record derived type names defined in the program so structure
    ! constructor calls are not emitted as external functions (Issue #2827).
    subroutine collect_derived_type_names(arena, prog, state)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        type(program_decl_state_t), intent(inout) :: state
        integer :: i, idx

        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (.not. arena%has_node_at(idx)) cycle
            select type (dtype => arena%entries(idx)%node)
            type is (derived_type_node)
                if (allocated(dtype%name)) &
                    call record_derived_type_name(state, trim(dtype%name))
            end select
        end do
    end subroutine collect_derived_type_names

    subroutine collect_use_associated_symbols(arena, prog, state)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        type(program_decl_state_t), intent(inout) :: state
        integer :: i, j, idx
        character(len=:), allocatable :: module_name

        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (.not. arena%has_node_at(idx)) cycle
            select type (use_stmt => arena%entries(idx)%node)
            type is (use_statement_node)
                if (allocated(use_stmt%module_name)) then
                    call record_use_module_name(state, trim(use_stmt%module_name))
                end if
                if (use_stmt%has_only .and. allocated(use_stmt%only_list)) then
                    do j = 1, size(use_stmt%only_list)
                        if (.not. allocated(use_stmt%only_list(j)%s)) cycle
                        call record_use_associated_name(state, &
                                                        trim(use_stmt%only_list(j)%s))
                    end do
                else if (allocated(use_stmt%module_name)) then
                    module_name = trim(use_stmt%module_name)
                    call collect_module_symbols(arena, module_name, state)
                end if

                if (allocated(use_stmt%rename_list)) then
                    do j = 1, size(use_stmt%rename_list), 2
                        if (.not. allocated(use_stmt%rename_list(j)%s)) cycle
                        call record_use_associated_name(state, &
                                                        trim(use_stmt%rename_list(j)%s))
                    end do
                end if
            end select
        end do
    end subroutine collect_use_associated_symbols

    subroutine collect_local_module_exports(arena, prog, state)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        type(program_decl_state_t), intent(inout) :: state
        integer :: i, idx
        character(len=:), allocatable :: module_name

        if (.not. allocated(prog%body_indices)) return

        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (.not. arena%has_node_at(idx)) cycle
            select type (mod => arena%entries(idx)%node)
            type is (module_node)
                if (.not. allocated(mod%name)) cycle
                module_name = trim(mod%name)
                if (index(module_name, "auto_") /= 1) then
                    if (.not. module_is_used(state, module_name)) cycle
                end if
                call record_module_exports(arena, mod, state)
            end select
        end do
    end subroutine collect_local_module_exports

    subroutine collect_auto_module_exports(arena, state)
        type(ast_arena_t), intent(in) :: arena
        type(program_decl_state_t), intent(inout) :: state
        integer :: i
        character(len=:), allocatable :: module_name

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (mod => arena%entries(i)%node)
            type is (module_node)
                if (.not. allocated(mod%name)) cycle
                module_name = trim(mod%name)
                if (index(module_name, "auto_") /= 1) cycle
                call record_module_exports(arena, mod, state)
            end select
        end do
    end subroutine collect_auto_module_exports

    subroutine collect_module_symbols(arena, module_name, state)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: module_name
        type(program_decl_state_t), intent(inout) :: state
        integer :: i

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (mod_node => arena%entries(i)%node)
            type is (module_node)
                if (.not. allocated(mod_node%name)) cycle
                if (trim(mod_node%name) /= module_name) cycle
                call record_module_exports(arena, mod_node, state)
            end select
        end do
    end subroutine collect_module_symbols

    subroutine record_module_exports(arena, mod_node, state)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: mod_node
        type(program_decl_state_t), intent(inout) :: state
        integer :: j, decl_idx, proc_idx

        if (allocated(mod_node%declaration_indices)) then
            do j = 1, size(mod_node%declaration_indices)
                decl_idx = mod_node%declaration_indices(j)
                if (.not. arena%has_node_at(decl_idx)) cycle
                call extract_declaration_names(arena, decl_idx, state)
            end do
        end if

        if (allocated(mod_node%procedure_indices)) then
            do j = 1, size(mod_node%procedure_indices)
                proc_idx = mod_node%procedure_indices(j)
                if (.not. arena%has_node_at(proc_idx)) cycle
                call extract_procedure_names(arena, proc_idx, state)
            end do
        end if
    end subroutine record_module_exports

    subroutine extract_declaration_names(arena, decl_idx, state)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_idx
        type(program_decl_state_t), intent(inout) :: state
        integer :: k
        integer :: proc_idx

        select type (decl => arena%entries(decl_idx)%node)
        type is (declaration_node)
            if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                do k = 1, size(decl%var_names)
                    call record_use_associated_name(state, trim(decl%var_names(k)))
                end do
            else if (allocated(decl%var_name)) then
                call record_use_associated_name(state, trim(decl%var_name))
            end if
        type is (derived_type_node)
            if (allocated(decl%name)) then
                call record_use_associated_name(state, trim(decl%name))
            end if
        type is (interface_block_node)
            if (allocated(decl%name)) then
                call record_use_associated_name(state, trim(decl%name))
            end if
            if (allocated(decl%procedure_indices)) then
                do k = 1, size(decl%procedure_indices)
                    proc_idx = decl%procedure_indices(k)
                    if (.not. arena%has_node_at(proc_idx)) cycle
                    select type (iface_item => arena%entries(proc_idx)%node)
                    type is (module_procedure_node)
                        if (.not. allocated(iface_item%procedure_names)) cycle
                        call record_module_procedure_names(state, iface_item)
                    type is (subroutine_def_node)
                        if (.not. allocated(iface_item%name)) cycle
                        call record_use_associated_name(state, &
                                                        trim(iface_item%name))
                    type is (function_def_node)
                        if (.not. allocated(iface_item%name)) cycle
                        call record_use_associated_name(state, &
                                                        trim(iface_item%name))
                    end select
                end do
            end if
        end select
    end subroutine extract_declaration_names

    subroutine record_module_procedure_names(state, node)
        type(program_decl_state_t), intent(inout) :: state
        type(module_procedure_node), intent(in) :: node
        integer :: i

        if (.not. allocated(node%procedure_names)) return
        do i = 1, size(node%procedure_names)
            if (.not. allocated(node%procedure_names(i)%s)) cycle
            call record_use_associated_name(state, &
                                            trim(node%procedure_names(i)%s))
        end do
    end subroutine record_module_procedure_names

    subroutine extract_procedure_names(arena, proc_idx, state)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_idx
        type(program_decl_state_t), intent(inout) :: state

        select type (proc => arena%entries(proc_idx)%node)
        type is (function_def_node)
            if (allocated(proc%name)) then
                call record_use_associated_name(state, trim(proc%name))
            end if
        end select
    end subroutine extract_procedure_names

    logical function module_is_used(state, module_name)
        type(program_decl_state_t), intent(in) :: state
        character(len=*), intent(in) :: module_name

        module_is_used = exists_in_list(state%use_module_names, &
                                        state%use_module_count, module_name)
    end function module_is_used

    subroutine collect_declared_symbols(arena, prog, state)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        type(program_decl_state_t), intent(inout) :: state
        integer :: i, j, idx
        logical :: contains_seen

        contains_seen = .false.

        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (.not. arena%has_node_at(idx)) cycle
            select type (decl => arena%entries(idx)%node)
            type is (contains_node)
                contains_seen = .true.
                cycle
            type is (declaration_node)
                if (contains_seen) cycle
                if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                    do j = 1, size(decl%var_names)
                        call record_declared_name(state, trim(decl%var_names(j)))
                    end do
                else
                    call record_declared_name(state, trim(decl%var_name))
                end if
            type is (function_def_node)
                if (contains_seen .and. allocated(decl%name)) then
                    call try_add_internal_function(state, trim(decl%name))
                    call collect_entry_points_from_function(arena, decl, state)
                end if
            type is (subroutine_def_node)
                ! A contained subroutine name is a procedure, not a variable
                ! (Issue #2950): passing it as an actual argument must not
                ! fabricate a declaration for it.
                if (contains_seen .and. allocated(decl%name)) then
                    call try_add_internal_function(state, trim(decl%name))
                end if
            type is (intrinsic_statement_node)
                ! INTRINSIC declares the name as a procedure (Issue #2950).
                if (contains_seen) cycle
                if (allocated(decl%procedure_names)) then
                    do j = 1, size(decl%procedure_names)
                        if (.not. allocated(decl%procedure_names(j)%s)) cycle
                        call try_add_internal_function(state, &
                                                  trim(decl%procedure_names(j)%s))
                    end do
                end if
            type is (call_or_subscript_node)
                if (contains_seen) cycle
                if (allocated(decl%name)) then
                    if (is_intrinsic_function(trim(decl%name))) then
                        call try_add_internal_function(state, trim(decl%name))
                    end if
                end if
            type is (namelist_statement_node)
                if (contains_seen) cycle
                if (allocated(decl%group_name)) then
                    call record_namelist_group(state, decl%group_name)
                end if
            type is (enum_node)
                if (contains_seen) cycle
                if (allocated(decl%enumerator_names)) then
                    do j = 1, size(decl%enumerator_names)
                        if (.not. allocated(decl%enumerator_names(j)%s)) cycle
                        call record_declared_name(state, &
                                                  trim(decl%enumerator_names(j)%s))
                    end do
                end if
            type is (interface_block_node)
                if (contains_seen) cycle
                call extract_declaration_names(arena, idx, state)
            end select
        end do
    end subroutine collect_declared_symbols

    subroutine collect_namelist_groups(arena, prog, state)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        type(program_decl_state_t), intent(inout) :: state
        integer :: i

        if (.not. allocated(arena%entries)) return

        do i = 1, min(arena%size, size(arena%entries))
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (stmt => arena%entries(i)%node)
            type is (namelist_statement_node)
                if (.not. allocated(stmt%group_name)) cycle
                call record_namelist_group(state, stmt%group_name)
            end select
        end do
    end subroutine collect_namelist_groups

    subroutine collect_associate_names(arena, prog, state)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        type(program_decl_state_t), intent(inout) :: state
        integer :: i, idx
        logical :: contains_seen

        if (.not. allocated(prog%body_indices)) return

        contains_seen = .false.

        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (.not. arena%has_node_at(idx)) cycle
            select type (stmt => arena%entries(idx)%node)
            type is (contains_node)
                contains_seen = .true.
                exit
            end select
            if (.not. contains_seen) then
                call collect_associate_names_recursive(arena, idx, state)
            end if
        end do
    end subroutine collect_associate_names

    recursive subroutine collect_associate_names_recursive(arena, node_index, state)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(program_decl_state_t), intent(inout) :: state
        integer :: i, j, idx

        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (associate_node)
            if (allocated(node%associations)) then
                do i = 1, size(node%associations)
                    if (allocated(node%associations(i)%name)) then
                        call record_declared_name(state, &
                                                  trim(node%associations(i)%name))
                    end if
                end do
            end if
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call collect_associate_names_recursive(arena, &
                                                           node%body_indices(i), state)
                end do
            end if
        type is (if_node)
            if (allocated(node%then_body_indices)) then
                do i = 1, size(node%then_body_indices)
                    call collect_associate_names_recursive(arena, &
                                                           node%then_body_indices(i), &
                                                           state)
                end do
            end if
            if (allocated(node%elseif_blocks)) then
                do i = 1, size(node%elseif_blocks)
                    if (allocated(node%elseif_blocks(i)%body_indices)) then
                        do j = 1, size(node%elseif_blocks(i)%body_indices)
                            idx = node%elseif_blocks(i)%body_indices(j)
                            call collect_associate_names_recursive(arena, idx, state)
                        end do
                    end if
                end do
            end if
            if (allocated(node%else_body_indices)) then
                do i = 1, size(node%else_body_indices)
                    call collect_associate_names_recursive(arena, &
                                                           node%else_body_indices(i), &
                                                           state)
                end do
            end if
        type is (do_loop_node)
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call collect_associate_names_recursive(arena, &
                                                           node%body_indices(i), state)
                end do
            end if
        type is (do_while_node)
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call collect_associate_names_recursive(arena, &
                                                           node%body_indices(i), state)
                end do
            end if
        type is (select_case_node)
            if (allocated(node%case_indices)) then
                do i = 1, size(node%case_indices)
                    idx = node%case_indices(i)
                    if (.not. arena%has_node_at(idx)) cycle
                    select type (case_node => arena%entries(idx)%node)
                    type is (case_block_node)
                        if (allocated(case_node%body_indices)) then
                            do j = 1, size(case_node%body_indices)
                                idx = case_node%body_indices(j)
                                call collect_associate_names_recursive(arena, &
                                                                       idx, state)
                            end do
                        end if
                    end select
                end do
            end if
            if (node%default_index > 0 .and. node%default_index <= arena%size) then
                if (allocated(arena%entries(node%default_index)%node)) then
                    select type (def_node => arena%entries(node%default_index)%node)
                    type is (case_default_node)
                        if (allocated(def_node%body_indices)) then
                            do i = 1, size(def_node%body_indices)
                                idx = def_node%body_indices(i)
                                call collect_associate_names_recursive(arena, &
                                                                       idx, state)
                            end do
                        end if
                    end select
                end if
            end if
        type is (where_node)
            if (allocated(node%where_body_indices)) then
                do i = 1, size(node%where_body_indices)
                    call collect_associate_names_recursive(arena, &
                                                           node%where_body_indices(i), &
                                                           state)
                end do
            end if
            if (allocated(node%elsewhere_clauses)) then
                do i = 1, size(node%elsewhere_clauses)
                    if (allocated(node%elsewhere_clauses(i)%body_indices)) then
                        do j = 1, size(node%elsewhere_clauses(i)%body_indices)
                            idx = node%elsewhere_clauses(i)%body_indices(j)
                            call collect_associate_names_recursive(arena, idx, state)
                        end do
                    end if
                end do
            end if
        type is (forall_node)
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call collect_associate_names_recursive(arena, &
                                                           node%body_indices(i), state)
                end do
            end if
        type is (block_construct_node)
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call collect_associate_names_recursive(arena, &
                                                           node%body_indices(i), state)
                end do
            end if
        type is (select_type_node)
            if (allocated(node%guard_indices)) then
                do i = 1, size(node%guard_indices)
                    idx = node%guard_indices(i)
                    if (.not. arena%has_node_at(idx)) cycle
                    select type (guard_node => arena%entries(idx)%node)
                    type is (type_guard_block_node)
                        if (allocated(guard_node%body_indices)) then
                            do j = 1, size(guard_node%body_indices)
                                idx = guard_node%body_indices(j)
                                call collect_associate_names_recursive(arena, &
                                                                       idx, state)
                            end do
                        end if
                    end select
                end do
            end if
            if (node%default_index > 0 .and. node%default_index <= arena%size) then
                if (allocated(arena%entries(node%default_index)%node)) then
                    select type (def_node => arena%entries(node%default_index)%node)
                    type is (type_guard_block_node)
                        if (allocated(def_node%body_indices)) then
                            do i = 1, size(def_node%body_indices)
                                idx = def_node%body_indices(i)
                                call collect_associate_names_recursive(arena, &
                                                                       idx, state)
                            end do
                        end if
                    end select
                end if
            end if
        type is (select_rank_node)
            if (allocated(node%rank_indices)) then
                do i = 1, size(node%rank_indices)
                    idx = node%rank_indices(i)
                    if (.not. arena%has_node_at(idx)) cycle
                    select type (rank_node => arena%entries(idx)%node)
                    type is (rank_block_node)
                        if (allocated(rank_node%body_indices)) then
                            do j = 1, size(rank_node%body_indices)
                                idx = rank_node%body_indices(j)
                                call collect_associate_names_recursive(arena, &
                                                                       idx, state)
                            end do
                        end if
                    end select
                end do
            end if
            if (node%default_index > 0 .and. node%default_index <= arena%size) then
                if (allocated(arena%entries(node%default_index)%node)) then
                    select type (def_node => arena%entries(node%default_index)%node)
                    type is (rank_block_node)
                        if (allocated(def_node%body_indices)) then
                            do i = 1, size(def_node%body_indices)
                                idx = def_node%body_indices(i)
                                call collect_associate_names_recursive(arena, &
                                                                       idx, state)
                            end do
                        end if
                    end select
                end if
            end if
        end select
    end subroutine collect_associate_names_recursive

    subroutine try_add_internal_function(state, name)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: name
        character(len=64) :: normalized_name

        normalized_name = trim(to_lower(name))
        if (len_trim(normalized_name) == 0) return
        if (state%internal_count >= program_decl_max_vars) return
        if (exists_in_list(state%internal_funcs, state%internal_count, &
                           normalized_name)) return
        state%internal_count = state%internal_count + 1
        state%internal_funcs(state%internal_count) = normalized_name
    end subroutine try_add_internal_function

    subroutine collect_entry_points_from_function(arena, func, state)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        type(program_decl_state_t), intent(inout) :: state
        integer :: i, idx

        if (.not. allocated(func%body_indices)) return

        do i = 1, size(func%body_indices)
            idx = func%body_indices(i)
            if (.not. arena%has_node_at(idx)) cycle
            select type (stmt => arena%entries(idx)%node)
            type is (entry_node)
                if (allocated(stmt%name)) then
                    call try_add_internal_function(state, trim(stmt%name))
                end if
            end select
        end do
    end subroutine collect_entry_points_from_function
