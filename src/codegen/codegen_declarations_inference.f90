module codegen_declarations_inference
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: assignment_node, call_or_subscript_node, &
                              identifier_node, program_node, array_literal_node
    use ast_nodes_misc, only: contains_node, use_statement_node, &
                              allocate_statement_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
                              intent_type_to_string, module_node
    use ast_nodes_procedure, only: function_def_node
    use ast_nodes_transfer, only: entry_node
    use string_utils_mod, only: to_lower
    use type_string_utils, only: mono_type_to_string
    use type_system_unified, only: mono_type_t
    use codegen_utilities, only: parameter_info_t
    use intrinsic_registry, only: is_intrinsic_function
    use codegen_type_utils, only: get_type_standardization
    implicit none
    private
    public :: build_parameter_map
    public :: derive_character_return_type
    public :: character_len_references_params
    public :: is_deferred_character_return
    public :: has_character_len_result_decl
    public :: is_character_len_declaration
    public :: collect_program_variable_decls

    integer, parameter :: program_decl_max_vars = 256

    type :: program_decl_state_t
        character(len=64) :: declared_names(program_decl_max_vars)
        character(len=64) :: var_names(program_decl_max_vars)
        character(len=64) :: var_types(program_decl_max_vars)
        character(len=64) :: func_names(program_decl_max_vars)
        character(len=64) :: func_types(program_decl_max_vars)
        character(len=64) :: internal_funcs(program_decl_max_vars)
        character(len=64) :: defined_func_names(program_decl_max_vars)
        character(len=64) :: defined_func_types(program_decl_max_vars)
        character(len=64) :: use_associated_names(program_decl_max_vars)
        integer :: declared_count
        integer :: var_count
        integer :: func_count
        integer :: internal_count
        integer :: defined_func_count
        integer :: use_associated_count
    end type program_decl_state_t

contains

    subroutine build_parameter_map(arena, param_indices, body_indices, param_map)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_indices(:)
        integer, intent(in) :: body_indices(:)
        type(parameter_info_t), allocatable, intent(out) :: param_map(:)
        integer :: param_count

        param_count = size(param_indices)
        allocate (param_map(param_count))

        call seed_parameter_map_from_params(arena, param_indices, param_map)
        call merge_parameter_details_from_body(arena, body_indices, param_map)
    end subroutine build_parameter_map

    subroutine seed_parameter_map_from_params(arena, param_indices, param_map)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_indices(:)
        type(parameter_info_t), intent(inout) :: param_map(:)
        integer :: i, idx

        do i = 1, size(param_indices)
            param_map(i)%name = ""
            param_map(i)%intent_str = ""
            param_map(i)%is_optional = .false.
            param_map(i)%is_target = .false.

            idx = param_indices(i)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle

            select type (param_node => arena%entries(idx)%node)
            type is (identifier_node)
                param_map(i)%name = param_node%name
            type is (parameter_declaration_node)
                param_map(i)%name = param_node%name
                param_map(i)%intent_str = intent_type_to_string(param_node%intent_type)
                param_map(i)%is_optional = param_node%is_optional
                param_map(i)%is_target = param_node%is_target
            end select
        end do
    end subroutine seed_parameter_map_from_params

    subroutine merge_parameter_details_from_body(arena, body_indices, param_map)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        type(parameter_info_t), intent(inout) :: param_map(:)
        integer :: j, idx
        integer :: name_idx
        character(len=:), allocatable :: intent_str

        do j = 1, size(body_indices)
            idx = body_indices(j)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle

            select type (body_node => arena%entries(idx)%node)
            type is (parameter_declaration_node)
                intent_str = intent_type_to_string(body_node%intent_type)
                call update_parameter_entry(param_map, body_node%name, intent_str, &
                                            .true., body_node%is_optional, &
                                            body_node%is_target)
            type is (declaration_node)
                if (body_node%is_multi_declaration .and. &
                    allocated(body_node%var_names)) then
                    do name_idx = 1, size(body_node%var_names)
                        if (len_trim(body_node%var_names(name_idx)) == 0) cycle
                        call update_parameter_entry(param_map, &
                                                    body_node%var_names(name_idx), &
                                                    body_node%intent, &
                                                    body_node%has_intent, &
                                                    body_node%is_optional, &
                                                    body_node%is_target)
                    end do
                else
                    call update_parameter_entry(param_map, body_node%var_name, &
                                                body_node%intent, &
                                                body_node%has_intent, &
                                                body_node%is_optional, &
                                                body_node%is_target)
                end if
            end select
        end do
    end subroutine merge_parameter_details_from_body

    subroutine update_parameter_entry(param_map, name, intent_value, has_intent, &
                                      is_optional, is_target)
        type(parameter_info_t), intent(inout) :: param_map(:)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: intent_value
        logical, intent(in) :: has_intent
        logical, intent(in) :: is_optional
        logical, intent(in) :: is_target
        integer :: i

        do i = 1, size(param_map)
            if (.not. allocated(param_map(i)%name)) cycle
            if (trim(param_map(i)%name) /= trim(name)) cycle

            if (has_intent) param_map(i)%intent_str = intent_value
            param_map(i)%is_optional = is_optional
            param_map(i)%is_target = is_target
            return
        end do
    end subroutine update_parameter_entry

    subroutine derive_character_return_type(arena, node, override)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable, intent(out) :: override
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: target_name
        integer :: i, decl_index

        override = ""

        if (allocated(node%return_type)) then
            lowered = to_lower(trim(node%return_type))
            if (index(lowered, "character(len=:), allocatable") == 0) return
        else
            return
        end if

        if (allocated(node%result_variable)) then
            if (len_trim(node%result_variable) > 0) then
                target_name = trim(node%result_variable)
            else
                target_name = trim(node%name)
            end if
        else
            target_name = trim(node%name)
        end if

        if (.not. allocated(node%body_indices)) return
        do i = 1, size(node%body_indices)
            decl_index = node%body_indices(i)
            if (decl_index <= 0 .or. decl_index > arena%size) cycle
            if (.not. allocated(arena%entries(decl_index)%node)) cycle
            select type (stmt => arena%entries(decl_index)%node)
            type is (declaration_node)
                if (len_trim(stmt%var_name) == 0) cycle
                if (trim(stmt%var_name) /= target_name) cycle
                if (.not. allocated(stmt%type_name)) cycle
                lowered = to_lower(trim(stmt%type_name))
                if (index(lowered, "len=") > 0) then
                    if (.not. character_len_references_params(arena, node, &
                                                              stmt%type_name)) then
                        override = trim(stmt%type_name)
                        return
                    end if
                end if
            end select
        end do
    end subroutine derive_character_return_type

    logical function character_len_references_params(arena, node, type_spec) &
        result(refs_params)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=*), intent(in) :: type_spec
        integer :: len_pos, paren_pos, i
        character(len=:), allocatable :: len_expr
        character(len=:), allocatable :: param_name

        refs_params = .false.
        len_pos = index(type_spec, 'len=')
        if (len_pos == 0) return

        paren_pos = index(type_spec(len_pos:), ')')
        if (paren_pos == 0) return

        len_expr = type_spec(len_pos + 4:len_pos + paren_pos - 2)
        if (.not. allocated(node%param_indices)) return

        do i = 1, size(node%param_indices)
            if (node%param_indices(i) <= 0 .or. node%param_indices(i) > &
                arena%size) cycle
            if (.not. allocated(arena%entries(node%param_indices(i))%node)) cycle

            select type (param_node => arena%entries(node%param_indices(i))%node)
            type is (identifier_node)
                param_name = trim(param_node%name)
            type is (parameter_declaration_node)
                param_name = trim(param_node%name)
            type is (declaration_node)
                param_name = trim(param_node%var_name)
            class default
                cycle
            end select

            if (index(len_expr, trim(param_name)) > 0) then
                refs_params = .true.
                return
            end if
        end do
    end function character_len_references_params

    pure logical function is_deferred_character_return(text) result(is_deferred)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lowered

        lowered = to_lower(trim(text))
        is_deferred = (index(lowered, 'character') == 1) .and. &
                      (index(lowered, 'len=:') > 0)
        if (is_deferred) then
            if (index(lowered, 'allocatable') == 0) then
                is_deferred = .false.
            end if
        end if
    end function is_deferred_character_return

    logical function has_character_len_result_decl(arena, node) result(has_decl)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: target_name
        integer :: i, decl_index, name_idx
        character(len=:), allocatable :: lowered

        has_decl = .false.

        if (allocated(node%result_variable)) then
            target_name = trim(node%result_variable)
        else if (allocated(node%name)) then
            target_name = trim(node%name)
        else
            target_name = ''
        end if

        if (len_trim(target_name) == 0) return
        if (.not. allocated(node%body_indices)) return

        do i = 1, size(node%body_indices)
            decl_index = node%body_indices(i)
            if (decl_index <= 0 .or. decl_index > arena%size) cycle
            if (.not. allocated(arena%entries(decl_index)%node)) cycle
            select type (stmt => arena%entries(decl_index)%node)
            type is (declaration_node)
                if (is_character_len_declaration(stmt%type_name)) then
                    if (trim(stmt%var_name) == target_name) then
                        has_decl = .true.
                        return
                    end if
                    if (stmt%is_multi_declaration .and. &
                        allocated(stmt%var_names)) then
                        do name_idx = 1, size(stmt%var_names)
                            if (trim(stmt%var_names(name_idx)) == target_name) then
                                has_decl = .true.
                                return
                            end if
                        end do
                    end if
                end if
            end select
        end do
    end function has_character_len_result_decl

    pure logical function is_character_len_declaration(type_name) result(matches)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: lowered

        lowered = to_lower(trim(type_name))
        if (len_trim(lowered) == 0) then
            matches = .false.
            return
        end if

        matches = (index(lowered, 'character') == 1) .and. &
                  (index(lowered, 'len=') > 0) .and. &
                  (index(lowered, 'len=*') == 0) .and. &
                  (index(lowered, 'len=:') == 0)
    end function is_character_len_declaration

    function collect_program_variable_decls(arena, prog) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        character(len=:), allocatable :: decl_code
        type(program_decl_state_t) :: state

        decl_code = ""
        if (.not. allocated(prog%body_indices)) return

        call initialize_program_decl_state(state)
        call populate_defined_function_table(arena, state)
        call collect_use_associated_symbols(arena, prog, state)
        call collect_declared_symbols(arena, prog, state)
        call collect_assignment_symbols(arena, prog, state)

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
        state%declared_count = 0
        state%var_count = 0
        state%func_count = 0
        state%internal_count = 0
        state%defined_func_count = 0
        state%use_associated_count = 0
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

    subroutine collect_module_symbols(arena, module_name, state)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: module_name
        type(program_decl_state_t), intent(inout) :: state
        integer :: i, j, decl_idx, proc_idx

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (mod_node => arena%entries(i)%node)
            type is (module_node)
                if (.not. allocated(mod_node%name)) cycle
                if (trim(mod_node%name) /= module_name) cycle
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
            end select
        end do
    end subroutine collect_module_symbols

    subroutine extract_declaration_names(arena, decl_idx, state)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_idx
        type(program_decl_state_t), intent(inout) :: state
        integer :: k

        select type (decl => arena%entries(decl_idx)%node)
        type is (declaration_node)
            if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                do k = 1, size(decl%var_names)
                    call record_use_associated_name(state, trim(decl%var_names(k)))
                end do
            else if (allocated(decl%var_name)) then
                call record_use_associated_name(state, trim(decl%var_name))
            end if
        end select
    end subroutine extract_declaration_names

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

    subroutine record_use_associated_name(state, name)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: name

        if (len_trim(name) == 0) return
        if (state%use_associated_count >= program_decl_max_vars) return
        if (exists_in_list(state%use_associated_names, &
                           state%use_associated_count, name)) return
        state%use_associated_count = state%use_associated_count + 1
        state%use_associated_names(state%use_associated_count) = name
    end subroutine record_use_associated_name

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
            type is (declaration_node)
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
                if (allocated(decl%name)) then
                    if (is_intrinsic_function(trim(decl%name))) then
                        call try_add_internal_function(state, trim(decl%name))
                    end if
                end if
            end select
        end do
    end subroutine collect_declared_symbols

    subroutine record_declared_name(state, name)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: name

        if (len_trim(name) == 0) return
        if (state%declared_count >= program_decl_max_vars) return
        state%declared_count = state%declared_count + 1
        state%declared_names(state%declared_count) = name
    end subroutine record_declared_name

    subroutine try_add_internal_function(state, name)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: name

        if (len_trim(name) == 0) return
        if (state%internal_count >= program_decl_max_vars) return
        if (exists_in_list(state%internal_funcs, state%internal_count, name)) return
        state%internal_count = state%internal_count + 1
        state%internal_funcs(state%internal_count) = name
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

        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (stmt => arena%entries(idx)%node)
            type is (assignment_node)
                call process_assignment_target(arena, stmt, state)
                call process_assignment_value(arena, stmt, state)
            type is (allocate_statement_node)
                call process_allocate_variables(arena, stmt, state)
            end select
        end do
    end subroutine collect_assignment_symbols

    subroutine process_assignment_target(arena, stmt, state)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: stmt
        type(program_decl_state_t), intent(inout) :: state
        integer :: target_idx
        character(len=:), allocatable :: name_buf
        character(len=:), allocatable :: type_buf
        character(len=:), allocatable :: func_return_type
        logical :: standardize_types_enabled

        call get_type_standardization(standardize_types_enabled)

        target_idx = stmt%target_index
        if (target_idx <= 0 .or. target_idx > arena%size) return
        if (.not. allocated(arena%entries(target_idx)%node)) return

        select type (id => arena%entries(target_idx)%node)
        type is (identifier_node)
            name_buf = trim(id%name)
            if (len_trim(name_buf) == 0) return
            if (exists_in_list(state%declared_names, state%declared_count, &
                               name_buf)) return
            if (exists_in_list(state%var_names, state%var_count, name_buf)) return
            if (exists_in_list(state%use_associated_names, &
                               state%use_associated_count, name_buf)) return

            type_buf = mono_type_to_string(id%inferred_type, &
                                           include_shape=.true., &
                                           standardize_real=standardize_types_enabled, &
                                           fallback='real')
            if (len_trim(type_buf) == 0 .or. trim(type_buf) == 'real') then
                func_return_type = infer_function_return_type_from_rhs(arena, &
                                                                       stmt, state)
                if (len_trim(func_return_type) > 0) type_buf = trim(func_return_type)
            end if
            if (len_trim(type_buf) == 0) type_buf = 'real'

            call try_add_variable(state, name_buf, trim(type_buf))
        end select
    end subroutine process_assignment_target

    function infer_function_return_type_from_rhs(arena, stmt, state) result(type_name)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: stmt
        type(program_decl_state_t), intent(in) :: state
        character(len=:), allocatable :: type_name
        integer :: value_idx
        logical :: standardize_types_enabled

        call get_type_standardization(standardize_types_enabled)

        type_name = ""
        value_idx = stmt%value_index
        if (value_idx <= 0 .or. value_idx > arena%size) return
        if (.not. allocated(arena%entries(value_idx)%node)) return

        select type (rhs => arena%entries(value_idx)%node)
        type is (call_or_subscript_node)
            if (len_trim(rhs%name) == 0) return
            type_name = lookup_function_return_type(state%defined_func_names, &
                                                    state%defined_func_types, &
                                                    state%defined_func_count, rhs%name)
        type is (array_literal_node)
            type_name = mono_type_to_string(rhs%inferred_type, &
                                            include_shape=.true., &
                                           standardize_real=standardize_types_enabled, &
                                            fallback='')
        end select
    end function infer_function_return_type_from_rhs

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
            type_buf = mono_type_to_string(val%inferred_type, &
                                           include_shape=.true., &
                                           standardize_real=standardize_types_enabled, &
                                           fallback='real')
            if (len_trim(type_buf) == 0 .or. trim(type_buf) == 'real') then
                func_return_type = &
                    lookup_function_return_type(state%defined_func_names, &
                                                state%defined_func_types, &
                                                state%defined_func_count, &
                                                name_buf)
                if (len_trim(func_return_type) > 0) type_buf = trim(func_return_type)
            end if
            if (len_trim(type_buf) == 0) type_buf = 'real'
            call try_add_function_reference(state, name_buf, trim(type_buf))
        end select
    end subroutine process_assignment_value

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
            base_type = mono_type_to_string(inferred_type, &
                                            include_shape=.false., &
                                           standardize_real=standardize_types_enabled, &
                                            fallback='integer')
        end if

        if (len_trim(base_type) == 0) base_type = 'integer'

        type_buf = trim(base_type)

        if (.not. allocated(stmt%type_spec)) then
            lowered = to_lower(type_buf)
            if (trim(lowered) == 'real') type_buf = 'integer'
        end if

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

        if (len_trim(name) == 0) return
        if (state%var_count >= program_decl_max_vars) return
        if (exists_in_list(state%var_names, state%var_count, name)) return
        state%var_count = state%var_count + 1
        state%var_names(state%var_count) = name
        state%var_types(state%var_count) = type_name
    end subroutine try_add_variable

    subroutine try_add_function_reference(state, name, type_name)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: type_name

        if (len_trim(name) == 0) return
        if (state%func_count >= program_decl_max_vars) return
        if (exists_in_list(state%declared_names, state%declared_count, name)) return
        if (exists_in_list(state%func_names, state%func_count, name)) return
        state%func_count = state%func_count + 1
        state%func_names(state%func_count) = name
        state%func_types(state%func_count) = type_name
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
            if (exists_in_list(state%use_associated_names, &
                               state%use_associated_count, &
                               trim(state%func_names(i)))) cycle
            code = code // "    " // trim(state%func_types(i)) // &
                   ", external :: " // trim(state%func_names(i)) // new_line('A')
        end do
    end function emit_program_declarations

    ! Helper function to check if a name exists in a list
    logical function exists_in_list(list, count, name)
        character(len=*), intent(in) :: list(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: name
        integer :: i

        exists_in_list = .false.
        do i = 1, count
            if (trim(list(i)) == trim(name)) then
                exists_in_list = .true.
                return
            end if
        end do
    end function exists_in_list

    subroutine build_function_return_type_table(arena, func_names, func_types, count)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(inout) :: func_names(:)
        character(len=*), intent(inout) :: func_types(:)
        integer, intent(out) :: count
        integer :: i
        character(len=64) :: func_name

        count = 0
        func_names = ""
        func_types = ""

        do i = 1, arena%size
            if (count >= size(func_names)) exit
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (func => arena%entries(i)%node)
            type is (function_def_node)
                if (.not. allocated(func%name)) cycle
                func_name = trim(func%name)
                if (len_trim(func_name) == 0) cycle
                if (exists_in_list(func_names, count, func_name)) cycle
                count = count + 1
                func_names(count) = func_name
                if (allocated(func%return_type)) then
                    if (len_trim(func%return_type) > 0) then
                        func_types(count) = trim(func%return_type)
                    end if
                end if
            end select
        end do
    end subroutine build_function_return_type_table

    function lookup_function_return_type(func_names, func_types, count, &
                                         func_name) result(type_name)
        character(len=*), intent(in) :: func_names(:)
        character(len=*), intent(in) :: func_types(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: func_name
        character(len=:), allocatable :: type_name
        integer :: i

        type_name = ""
        if (len_trim(func_name) == 0) return

        do i = 1, count
            if (trim(func_names(i)) == trim(func_name)) then
                if (len_trim(func_types(i)) > 0) then
                    type_name = trim(func_types(i))
                end if
                return
            end if
        end do
    end function lookup_function_return_type

end module codegen_declarations_inference
