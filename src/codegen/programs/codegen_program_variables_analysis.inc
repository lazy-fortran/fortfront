
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
            if (.not. arena%has_node_at(idx)) cycle
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
            if (.not. arena%has_node_at(idx)) cycle
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
        integer, allocatable :: guard_type_indices(:)

        if (.not. allocated(prog%body_indices)) return

        call get_type_standardization(standardize_types_enabled)
        call collect_guard_type_name_indices(arena, guard_type_indices)

        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (.not. arena%has_node_at(idx)) cycle

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
            type is (directive_node)
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
                                                  standardize_types_enabled, &
                                                  guard_type_indices)
            end select
        end do
    end subroutine collect_executable_identifier_symbols

    ! Issue #2821: SELECT TYPE / SELECT RANK guard type-names (the type spec in
    ! "type is (...)"/"class is (...)") are stored as identifier nodes referenced
    ! by type_guard_block_node%type_name_index. They are not program variables,
    ! so collect their indices and exclude them from variable harvesting.
    subroutine collect_guard_type_name_indices(arena, indices)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(out) :: indices(:)
        integer :: i

        allocate (indices(0))
        if (.not. allocated(arena%entries)) return

        do i = 1, min(arena%size, size(arena%entries))
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (guard => arena%entries(i)%node)
            type is (type_guard_block_node)
                if (guard%type_name_index <= 0) cycle
                indices = [indices, guard%type_name_index]
            end select
        end do
    end subroutine collect_guard_type_name_indices

    subroutine collect_identifiers_for_node(arena, node_index, state, &
                                            standardize_types_enabled, &
                                            guard_type_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(program_decl_state_t), intent(inout) :: state
        logical, intent(in) :: standardize_types_enabled
        integer, intent(in) :: guard_type_indices(:)
        type(variable_usage_info_t) :: usage
        integer :: i, ident_index

        if (.not. arena%has_node_at(node_index)) return

        usage = create_variable_usage_info()
        call collect_identifiers_recursive(arena, node_index, usage)

        if (.not. allocated(usage%variable_names)) return

        do i = 1, size(usage%variable_names)
            ident_index = usage%node_indices(i)
            if (.not. arena%has_node_at(ident_index)) cycle
            if (any(guard_type_indices == ident_index)) cycle
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

        ! Skip keyword arguments (e.g., dim=1 in minloc(arr, dim=1))
        if (stmt%is_keyword_argument) return

        call get_type_standardization(standardize_types_enabled)

        target_idx = stmt%target_index
        if (.not. arena%has_node_at(target_idx)) return

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
                        ! Issue #2075: Use function return type if it has allocatable
                    else if (index(func_lower, 'allocatable') > 0 .and. &
                             index(curr_lower, 'allocatable') == 0) then
                        type_buf = func_return_type
                    end if
                end block
            end if

            if (len_trim(type_buf) == 0) type_buf = 'real'

            ! Issue #2075: Deferred-shape arrays in local scope must be allocatable
            ! If dimension(:), dimension(:,:), etc. without allocatable, add it
            block
                use codegen_character_types, only: is_deferred_shape_array
                logical :: needs_alloc
                needs_alloc = is_deferred_shape_array(type_buf)
                if (needs_alloc .and. index(to_lower(trim(type_buf)), &
                                            'allocatable') == 0) then
                    ! Add allocatable to deferred-shape array
                    type_buf = trim(type_buf)//', allocatable'
                end if
            end block

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
        if (.not. arena%has_node_at(value_idx)) return

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

        if (.not. arena%has_node_at(expr_index)) return

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
                    if (expr%arg_indices(i) <= 0) cycle
                    if (.not. allocated(arena%entries(expr%arg_indices(i))%node)) cycle
                    select type (arg => arena%entries(expr%arg_indices(i))%node)
                    type is (assignment_node)
                        call collect_expr_identifiers(arena, arg%value_index, state, &
                                                      standardize_types_enabled)
                    class default
                        call collect_expr_identifiers(arena, expr%arg_indices(i), &
                                                      state, standardize_types_enabled)
                    end select
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
        ! A name known to be a procedure in this scope (contained procedure or
        ! INTRINSIC declared) is a procedure reference, not an implicitly typed
        ! variable (Issue #2950).
        if (exists_in_list(state%internal_funcs, state%internal_count, &
                           normalized_name)) return

        type_buf = mono_type_to_string(inferred_type, &
                                       include_shape=.true., &
                                       standardize_real=standardize_types_enabled, &
                                       fallback='')
        if (len_trim(type_buf) == 0) then
            if (standardize_types_enabled) then
                type_buf = 'real(dp)'
            else
                type_buf = 'real'
            end if
        end if
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
            if (.not. arena%has_node_at(var_index)) cycle

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
                dimension_spec = trim(dimension_spec)//',:'
            end do
            lowered = to_lower(type_buf)
            if (index(lowered, 'dimension(') == 0) then
                type_buf = trim(type_buf)//', dimension('// &
                    trim(dimension_spec)//')'
            end if
        end if

        lowered = to_lower(type_buf)
        if (index(lowered, 'allocatable') == 0) then
            type_buf = trim(type_buf)//', allocatable'
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
                adjusted_type = trim(adjusted_type)//', allocatable'
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
            else if (current_lower /= new_lower .and. &
                     (new_lower == 'real(8)' .or. new_lower == 'real(dp)') &
                     .and. current_lower == 'real') then
                state%func_types(existing_idx) = normalized_type
            end if
            return
        end if

        state%func_count = state%func_count + 1
        state%func_names(state%func_count) = normalized_name
        state%func_types(state%func_count) = normalized_type
    end subroutine try_add_function_reference
