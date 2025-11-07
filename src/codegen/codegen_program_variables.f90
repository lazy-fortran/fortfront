module codegen_program_variables
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: assignment_node, call_or_subscript_node, &
                              identifier_node, program_node, binary_op_node, &
                              array_literal_node, component_access_node, &
                              range_subscript_node
    use ast_nodes_misc, only: contains_node, use_statement_node, &
                              allocate_statement_node, interface_block_node, &
                              module_procedure_node, implicit_statement_node, &
                              comment_node, blank_line_node, namelist_statement_node
    use ast_nodes_data, only: declaration_node, module_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_transfer, only: entry_node
    use codegen_program_decl_utils, only: exists_in_list, &
                                           build_function_return_type_table, &
                                           program_decl_state_t, &
                                           program_decl_max_vars, &
                                           record_declared_name, &
                                           record_namelist_group, &
                                           record_use_associated_name, &
                                           record_use_module_name, &
                                           seed_namelist_groups_from_text
    use codegen_type_utils, only: get_type_standardization
    use codegen_type_inference_utils, only: canonicalize_type, &
                                            deduce_type_from_arguments, &
                                            infer_function_return_type_from_rhs
    use intrinsic_registry, only: is_intrinsic_function
    use string_utils_mod, only: to_lower
    use type_string_utils, only: mono_type_to_string
    use type_system_unified, only: mono_type_t
    use variable_usage_dispatcher_module, only: collect_identifiers_recursive
    use variable_usage_core_module, only: variable_usage_info_t, &
                                          create_variable_usage_info
    implicit none
    private
    public :: collect_program_variable_decls

contains

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
        call collect_namelist_groups(arena, prog, state)
        call collect_assignment_symbols(arena, prog, state)
        call collect_executable_identifier_symbols(arena, prog, state)

        if (state%var_count == 0 .and. state%func_count == 0) return

        decl_code = emit_program_declarations(state)
    end function collect_program_variable_decls

    subroutine initialize_program_decl_state(state)
        type(program_decl_state_t), intent(out) :: state

        state%declared_names = ""
        state%var_names = ""
        state%var_types = ""
        state%func_names = ""
        state%func_types = ""
        state%internal_funcs = ""
        state%defined_func_names = ""
        state%defined_func_types = ""
        state%use_associated_names = ""
        state%use_module_names = ""
        state%namelist_group_names = ""
        state%declared_count = 0
        state%var_count = 0
        state%func_count = 0
        state%internal_count = 0
        state%defined_func_count = 0
        state%use_associated_count = 0
        state%use_module_count = 0
        state%namelist_group_count = 0
    end subroutine initialize_program_decl_state

    subroutine populate_defined_function_table(arena, state)
        type(ast_arena_t), intent(in) :: arena
        type(program_decl_state_t), intent(inout) :: state

        call build_function_return_type_table(arena, state%defined_func_names, &
                                              state%defined_func_types, &
                                              state%defined_func_count)
    end subroutine populate_defined_function_table

    subroutine collect_use_associated_symbols(arena, prog, state)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        type(program_decl_state_t), intent(inout) :: state
        integer :: i, j, idx
        character(len=:), allocatable :: module_name

        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
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
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
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
                if (decl_idx <= 0 .or. decl_idx > arena%size) cycle
                if (.not. allocated(arena%entries(decl_idx)%node)) cycle
                call extract_declaration_names(arena, decl_idx, state)
            end do
        end if

        if (allocated(mod_node%procedure_indices)) then
            do j = 1, size(mod_node%procedure_indices)
                proc_idx = mod_node%procedure_indices(j)
                if (proc_idx <= 0 .or. proc_idx > arena%size) cycle
                if (.not. allocated(arena%entries(proc_idx)%node)) cycle
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
        type is (interface_block_node)
            if (allocated(decl%name)) then
                call record_use_associated_name(state, trim(decl%name))
            end if
            if (allocated(decl%procedure_indices)) then
                do k = 1, size(decl%procedure_indices)
                    proc_idx = decl%procedure_indices(k)
                    if (proc_idx <= 0 .or. proc_idx > arena%size) cycle
                    if (.not. allocated(arena%entries(proc_idx)%node)) cycle
                    select type (iface_item => arena%entries(proc_idx)%node)
                    type is (module_procedure_node)
                        if (.not. allocated(iface_item%procedure_names)) cycle
                        call record_module_procedure_names(state, iface_item)
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
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
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
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (stmt => arena%entries(idx)%node)
            type is (entry_node)
                if (allocated(stmt%name)) then
                    call try_add_internal_function(state, trim(stmt%name))
                end if
            end select
        end do
    end subroutine collect_entry_points_from_function

    subroutine collect_assignment_symbols(arena, prog, state)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        type(program_decl_state_t), intent(inout) :: state
        integer :: i, idx
        logical :: contains_seen

        contains_seen = .false.

        ! First pass: process allocate statements to ensure allocatable
        ! variables are registered before processing assignments
        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (stmt => arena%entries(idx)%node)
            type is (contains_node)
                contains_seen = .true.
            type is (allocate_statement_node)
                if (contains_seen) cycle
                call process_allocate_variables(arena, stmt, state)
            end select
        end do

        contains_seen = .false.

        ! Second pass: process assignments after allocate statements
        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (stmt => arena%entries(idx)%node)
            type is (contains_node)
                contains_seen = .true.
            type is (assignment_node)
                if (contains_seen) cycle
                call process_assignment_target(arena, stmt, state)
                call process_assignment_value(arena, stmt, state)
            end select
        end do
    end subroutine collect_assignment_symbols

    subroutine collect_executable_identifier_symbols(arena, prog, state)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        type(program_decl_state_t), intent(inout) :: state
        integer :: i, idx
        logical :: standardize_types_enabled

        if (.not. allocated(prog%body_indices)) return

        call get_type_standardization(standardize_types_enabled)

        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle

            select type (stmt => arena%entries(idx)%node)
            type is (contains_node)
                exit
            type is (declaration_node)
                cycle
            type is (use_statement_node)
                cycle
            type is (implicit_statement_node)
                cycle
            type is (comment_node)
                cycle
            type is (blank_line_node)
                cycle
            type is (interface_block_node)
                cycle
            type is (module_node)
                cycle
            type is (module_procedure_node)
                cycle
            type is (function_def_node)
                cycle
            type is (subroutine_def_node)
                cycle
            type is (entry_node)
                cycle
            type is (allocate_statement_node)
                cycle
            type is (namelist_statement_node)
                cycle
            class default
                call collect_identifiers_for_node(arena, idx, state, &
                                                  standardize_types_enabled)
            end select
        end do
    end subroutine collect_executable_identifier_symbols

    subroutine collect_identifiers_for_node(arena, node_index, state, &
                                            standardize_types_enabled)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(program_decl_state_t), intent(inout) :: state
        logical, intent(in) :: standardize_types_enabled
        type(variable_usage_info_t) :: usage
        integer :: i, ident_index

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        usage = create_variable_usage_info()
        call collect_identifiers_recursive(arena, node_index, usage)

        if (.not. allocated(usage%variable_names)) return

        do i = 1, size(usage%variable_names)
            ident_index = usage%node_indices(i)
            if (ident_index <= 0 .or. ident_index > arena%size) cycle
            if (.not. allocated(arena%entries(ident_index)%node)) cycle
            select type (id => arena%entries(ident_index)%node)
            type is (identifier_node)
                call register_identifier_reference(state, trim(id%name), &
                                                   id%inferred_type, &
                                                   standardize_types_enabled)
            end select
        end do
    end subroutine collect_identifiers_for_node


    subroutine process_assignment_target(arena, stmt, state)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: stmt
        type(program_decl_state_t), intent(inout) :: state
        integer :: target_idx
        character(len=:), allocatable :: name_buf
        character(len=:), allocatable :: type_buf
        character(len=:), allocatable :: func_return_type
        logical :: standardize_types_enabled
        character(len=:), allocatable :: normalized_target

        call get_type_standardization(standardize_types_enabled)

        target_idx = stmt%target_index
        if (target_idx <= 0 .or. target_idx > arena%size) return
        if (.not. allocated(arena%entries(target_idx)%node)) return

        select type (id => arena%entries(target_idx)%node)
        type is (identifier_node)
            name_buf = trim(id%name)
            if (len_trim(name_buf) == 0) return
            normalized_target = to_lower(trim(name_buf))
            if (len_trim(normalized_target) == 0) return
            if (normalized_target == 'namelist') return
            if (exists_in_list(state%namelist_group_names, &
                               state%namelist_group_count, normalized_target)) return
            if (exists_in_list(state%declared_names, state%declared_count, &
                               name_buf)) return
            if (exists_in_list(state%var_names, state%var_count, name_buf)) return
            if (exists_in_list(state%use_associated_names, &
                               state%use_associated_count, name_buf)) return

            type_buf = mono_type_to_string(id%inferred_type, &
                                           include_shape=.true., &
                                           standardize_real=standardize_types_enabled, &
                                           fallback='')
            type_buf = canonicalize_type(type_buf)
            func_return_type = infer_function_return_type_from_rhs( &
                               arena, stmt, state%defined_func_names, &
                               state%defined_func_types, state%defined_func_count)
            func_return_type = canonicalize_type(func_return_type)

            if (len_trim(func_return_type) > 0) then
                block
                    character(len=:), allocatable :: curr_lower
                    character(len=:), allocatable :: func_lower

                    curr_lower = to_lower(trim(type_buf))
                    func_lower = to_lower(trim(func_return_type))

                    if (len_trim(type_buf) == 0) then
                        type_buf = func_return_type
                    else if (curr_lower == 'real' .and. func_lower /= 'real') then
                        type_buf = func_return_type
                    else if (curr_lower == 'logical' .and. func_lower /= &
                             'logical') then
                        type_buf = func_return_type
                    end if
                end block
            end if

            if (len_trim(type_buf) == 0) type_buf = 'real'
            call try_add_variable(state, name_buf, trim(type_buf))
        end select
    end subroutine process_assignment_target

    subroutine process_assignment_value(arena, stmt, state)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: stmt
        type(program_decl_state_t), intent(inout) :: state
        integer :: value_idx
        character(len=:), allocatable :: type_buf
        character(len=:), allocatable :: func_return_type
        character(len=:), allocatable :: name_buf
        logical :: standardize_types_enabled

        call get_type_standardization(standardize_types_enabled)

        value_idx = stmt%value_index
        if (value_idx <= 0 .or. value_idx > arena%size) return
        if (.not. allocated(arena%entries(value_idx)%node)) return

        select type (val => arena%entries(value_idx)%node)
        type is (call_or_subscript_node)
            name_buf = trim(val%name)
            if (len_trim(name_buf) == 0) return
            type_buf = mono_type_to_string( &
                val%inferred_type, &
                include_shape=.true., &
                standardize_real=standardize_types_enabled, &
                fallback='')
            type_buf = canonicalize_type(type_buf)
            func_return_type = infer_function_return_type_from_rhs( &
                               arena, stmt, state%defined_func_names, &
                               state%defined_func_types, state%defined_func_count)
            func_return_type = canonicalize_type(func_return_type)

            if (len_trim(func_return_type) > 0) then
                block
                    character(len=:), allocatable :: curr_lower
                    character(len=:), allocatable :: func_lower

                    curr_lower = to_lower(trim(type_buf))
                    func_lower = to_lower(trim(func_return_type))

                    if (len_trim(type_buf) == 0) then
                        type_buf = func_return_type
                    else if (curr_lower == 'integer' .and. func_lower /= &
                             'integer') then
                        type_buf = func_return_type
                    else if (curr_lower == 'real' .and. func_lower /= 'real') then
                        type_buf = func_return_type
                    else if (curr_lower == 'logical' .and. func_lower /= &
                             'logical') then
                        type_buf = func_return_type
                    end if
                end block
            end if

            if (len_trim(type_buf) == 0) type_buf = 'real'
            call try_add_function_reference(state, name_buf, trim(type_buf))
        end select

        call collect_expr_identifiers(arena, value_idx, state, &
                                      standardize_types_enabled)
    end subroutine process_assignment_value

    recursive subroutine collect_expr_identifiers(arena, expr_index, state, &
                                                  standardize_types_enabled)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        type(program_decl_state_t), intent(inout) :: state
        logical, intent(in) :: standardize_types_enabled
        integer :: i

        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return

        select type (expr => arena%entries(expr_index)%node)
        type is (identifier_node)
            if (.not. allocated(expr%name)) return
            call register_identifier_reference(state, trim(expr%name), &
                                               expr%inferred_type, &
                                               standardize_types_enabled)
        type is (binary_op_node)
            call collect_expr_identifiers(arena, expr%left_index, state, &
                                          standardize_types_enabled)
            call collect_expr_identifiers(arena, expr%right_index, state, &
                                          standardize_types_enabled)
        type is (call_or_subscript_node)
            if (expr%base_expr_index > 0) then
                call collect_expr_identifiers(arena, expr%base_expr_index, state, &
                                              standardize_types_enabled)
            end if
            if (allocated(expr%arg_indices)) then
                do i = 1, size(expr%arg_indices)
                    call collect_expr_identifiers(arena, expr%arg_indices(i), &
                                                  state, standardize_types_enabled)
                end do
            end if
        type is (array_literal_node)
            if (allocated(expr%element_indices)) then
                do i = 1, size(expr%element_indices)
                    call collect_expr_identifiers(arena, expr%element_indices(i), &
                                                  state, standardize_types_enabled)
                end do
            end if
        type is (component_access_node)
            if (expr%base_expr_index > 0) then
                call collect_expr_identifiers(arena, expr%base_expr_index, state, &
                                              standardize_types_enabled)
            end if
        type is (range_subscript_node)
            if (expr%base_expr_index > 0) then
                call collect_expr_identifiers(arena, expr%base_expr_index, state, &
                                              standardize_types_enabled)
            end if
            if (expr%start_index > 0) then
                call collect_expr_identifiers(arena, expr%start_index, state, &
                                              standardize_types_enabled)
            end if
            if (expr%end_index > 0) then
                call collect_expr_identifiers(arena, expr%end_index, state, &
                                              standardize_types_enabled)
            end if
        end select
    end subroutine collect_expr_identifiers

    subroutine register_identifier_reference(state, name, inferred_type, &
                                             standardize_types_enabled)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: name
        type(mono_type_t), intent(in) :: inferred_type
        logical, intent(in) :: standardize_types_enabled
        character(len=:), allocatable :: type_buf
        character(len=:), allocatable :: normalized_name

        if (len_trim(name) == 0) return
        normalized_name = trim(to_lower(name))
        if (normalized_name == 'namelist') return
        if (exists_in_list(state%namelist_group_names, &
                           state%namelist_group_count, normalized_name)) return
        if (exists_in_list(state%declared_names, state%declared_count, &
                           name)) return
        if (exists_in_list(state%var_names, state%var_count, name)) return
        if (exists_in_list(state%use_associated_names, &
                           state%use_associated_count, name)) return

        type_buf = mono_type_to_string(inferred_type, &
                                       include_shape=.true., &
                                       standardize_real=standardize_types_enabled, &
                                       fallback='')
        if (len_trim(type_buf) == 0) type_buf = 'real'
        call try_add_variable(state, name, trim(type_buf))
    end subroutine register_identifier_reference

    subroutine process_allocate_variables(arena, stmt, state)
        type(ast_arena_t), intent(in) :: arena
        type(allocate_statement_node), intent(in) :: stmt
        type(program_decl_state_t), intent(inout) :: state
        integer :: i, var_index, arg_count
        character(len=:), allocatable :: name_buf
        logical :: standardize_types_enabled

        call get_type_standardization(standardize_types_enabled)

        if (.not. allocated(stmt%var_indices)) return

        do i = 1, size(stmt%var_indices)
            var_index = stmt%var_indices(i)
            if (var_index <= 0 .or. var_index > arena%size) cycle
            if (.not. allocated(arena%entries(var_index)%node)) cycle

            select type (node => arena%entries(var_index)%node)
            type is (identifier_node)
                name_buf = trim(node%name)
                call add_allocate_variable(state, stmt, name_buf, &
                                           node%inferred_type, 0, &
                                           standardize_types_enabled)
            type is (call_or_subscript_node)
                name_buf = trim(node%name)
                if (allocated(node%arg_indices)) then
                    arg_count = size(node%arg_indices)
                else
                    arg_count = 0
                end if
                call add_allocate_variable(state, stmt, name_buf, &
                                           node%inferred_type, arg_count, &
                                           standardize_types_enabled)
            end select
        end do
    end subroutine process_allocate_variables

    subroutine add_allocate_variable(state, stmt, name, inferred_type, rank, &
                                     standardize_types_enabled)
        type(program_decl_state_t), intent(inout) :: state
        type(allocate_statement_node), intent(in) :: stmt
        character(len=*), intent(in) :: name
        type(mono_type_t), intent(in) :: inferred_type
        integer, intent(in) :: rank
        logical, intent(in) :: standardize_types_enabled
        character(len=:), allocatable :: base_type
        character(len=:), allocatable :: type_buf
        character(len=:), allocatable :: dimension_spec
        character(len=:), allocatable :: lowered
        integer :: dim_index

        if (len_trim(name) == 0) return
        if (exists_in_list(state%declared_names, state%declared_count, &
                           name)) return
        if (exists_in_list(state%var_names, state%var_count, name)) return
        if (exists_in_list(state%use_associated_names, &
                           state%use_associated_count, name)) return

        if (allocated(stmt%type_spec)) then
            if (len_trim(stmt%type_spec) > 0) then
                base_type = trim(stmt%type_spec)
            end if
        end if

        if (.not. allocated(base_type)) then
            base_type = mono_type_to_string( &
                        inferred_type, &
                        include_shape=.false., &
                        standardize_real=standardize_types_enabled, &
                        fallback='integer')
        end if

        if (len_trim(base_type) == 0) base_type = 'integer'

        type_buf = trim(base_type)

        if (rank > 0) then
            dimension_spec = ':'
            do dim_index = 2, rank
                dimension_spec = trim(dimension_spec) // ',:'
            end do
            lowered = to_lower(type_buf)
            if (index(lowered, 'dimension(') == 0) then
                type_buf = trim(type_buf) // ', dimension(' // &
                    trim(dimension_spec) // ')'
            end if
        end if

        lowered = to_lower(type_buf)
        if (index(lowered, 'allocatable') == 0) then
            type_buf = trim(type_buf) // ', allocatable'
        end if

        call try_add_variable(state, trim(name), trim(type_buf))
    end subroutine add_allocate_variable

    subroutine try_add_variable(state, name, type_name)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: type_name
        character(len=64) :: normalized_name
        character(len=:), allocatable :: adjusted_type
        character(len=:), allocatable :: lowered

        normalized_name = trim(to_lower(name))
        if (len_trim(normalized_name) == 0) return
        if (state%var_count >= program_decl_max_vars) return
        if (exists_in_list(state%var_names, state%var_count, normalized_name)) return
        if (normalized_name == 'namelist') return
        if (exists_in_list(state%namelist_group_names, &
                           state%namelist_group_count, normalized_name)) return

        adjusted_type = trim(type_name)
        lowered = to_lower(adjusted_type)
        if (index(lowered, 'character') == 1 .and. index(lowered, 'len=:') > 0) then
            if (index(lowered, 'allocatable') == 0) then
                adjusted_type = trim(adjusted_type) // ', allocatable'
            end if
        end if

        state%var_count = state%var_count + 1
        state%var_names(state%var_count) = normalized_name
        state%var_types(state%var_count) = adjusted_type
    end subroutine try_add_variable

    subroutine try_add_function_reference(state, name, type_name)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: type_name
        integer :: i, existing_idx
        character(len=:), allocatable :: normalized_type
        character(len=:), allocatable :: current_lower
        character(len=:), allocatable :: new_lower
        character(len=64) :: normalized_name
        character(len=64) :: normalized_existing

        normalized_name = trim(to_lower(name))
        if (len_trim(normalized_name) == 0) return
        if (state%func_count >= program_decl_max_vars) return
        if (exists_in_list(state%declared_names, state%declared_count, &
                           normalized_name)) return
        normalized_type = canonicalize_type(type_name)

        existing_idx = 0
        do i = 1, state%func_count
            normalized_existing = trim(to_lower(state%func_names(i)))
            if (trim(normalized_existing) == trim(normalized_name)) then
                existing_idx = i
                exit
            end if
        end do

        if (existing_idx > 0) then
            current_lower = to_lower(trim(state%func_types(existing_idx)))
            new_lower = to_lower(trim(normalized_type))
            if (len_trim(new_lower) == 0) return
            if (current_lower == 'integer' .and. new_lower /= 'integer') then
                state%func_types(existing_idx) = normalized_type
            else if (current_lower == 'real' .and. new_lower /= 'real') then
                state%func_types(existing_idx) = normalized_type
            else if (current_lower /= new_lower .and. new_lower == 'real(8)' &
                     .and. current_lower == 'real') then
                state%func_types(existing_idx) = normalized_type
            end if
            return
        end if

        state%func_count = state%func_count + 1
        state%func_names(state%func_count) = normalized_name
        state%func_types(state%func_count) = normalized_type
    end subroutine try_add_function_reference

    function emit_program_declarations(state) result(code)
        type(program_decl_state_t), intent(in) :: state
        character(len=:), allocatable :: code
        integer :: i, j
        character(len=:), allocatable :: current_type, var_list
        logical :: type_used(program_decl_max_vars)
        logical :: first_var

        code = ""
        type_used = .false.

        do i = 1, state%var_count
            if (type_used(i)) cycle

            current_type = trim(state%var_types(i))
            var_list = trim(state%var_names(i))
            type_used(i) = .true.
            first_var = .true.

            do j = i + 1, state%var_count
                if (type_used(j)) cycle
                if (trim(state%var_types(j)) == current_type) then
                    if (first_var) then
                        first_var = .false.
                    end if
                    var_list = var_list // ", " // trim(state%var_names(j))
                    type_used(j) = .true.
                end if
            end do

            code = code // "    " // current_type // " :: " // var_list // &
                   new_line('A')
        end do

        do i = 1, state%func_count
            if (exists_in_list(state%internal_funcs, state%internal_count, &
                               trim(state%func_names(i)))) cycle
            if (is_intrinsic_function(trim(state%func_names(i)))) cycle
            if (module_is_used(state, "auto_"//trim(state%func_names(i)))) cycle
            if (exists_in_list(state%use_associated_names, &
                               state%use_associated_count, &
                               trim(state%func_names(i)))) cycle
            code = code // "    " // trim(state%func_types(i)) // &
                   ", external :: " // trim(state%func_names(i)) // new_line('A')
        end do
    end function emit_program_declarations

end module codegen_program_variables
