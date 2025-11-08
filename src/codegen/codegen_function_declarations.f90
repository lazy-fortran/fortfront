module codegen_function_declarations
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, assignment_node
    use ast_nodes_loops, only: do_loop_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use ast_nodes_io, only: print_statement_node, read_statement_node
    use ast_nodes_procedure, only: function_def_node
    use ast_nodes_transfer, only: entry_node
    use codegen_declarations_core, only: fix_character_len_placeholder
    use codegen_declarations_inference, only: build_parameter_map, &
                                              derive_character_return_type, &
                                              has_character_len_result_decl, &
                                              is_deferred_character_return, &
                                              is_allocatable_array_return
    use codegen_procedure_shared, only: build_parameter_clause, gather_prefix, &
                                        copy_indices, apply_default_intents, &
                                        apply_function_default_intents, &
                                        maybe_add_procedure_implicit_none, &
                                        filter_implicit_statements, &
                                        append_parameter_declaration, &
                                        is_parameter_name, ensure_local_var_capacity, &
                                        is_local_var_collected
    use codegen_type_utils, only: get_type_standardization
    use codegen_grouped_body_params, only: generate_grouped_body_with_params
    use codegen_import_reorder, only: reorder_import_lines
    use codegen_parameter_info, only: parameter_info_t
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_entry_utils, only: collect_entry_param_names, &
                                    is_entry_parameter_decl
    use string_utils_mod, only: to_lower
    use type_string_utils, only: mono_type_to_string
    use type_system_unified, only: mono_type_t, TARRAY
    implicit none
    private
    public :: generate_code_function_def

contains

    function generate_code_function_def(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = compose_function_signature(arena, node)
        code = code // new_line('A')
        code = code // build_function_body_section(arena, node)
        code = code // "end function " // node%name
    end function generate_code_function_def

    function compose_function_signature(arena, node) result(signature)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: signature
        character(len=:), allocatable :: prefix
        character(len=:), allocatable :: return_type_code
        character(len=:), allocatable :: params_clause
        character(len=:), allocatable :: result_clause
        logical :: recursive_in_prefix
        logical :: is_deferred_char

        prefix = gather_function_prefix(node, recursive_in_prefix)
        if (node%is_recursive .and. .not. recursive_in_prefix) then
            if (len_trim(prefix) > 0) then
                prefix = "recursive " // trim(prefix)
            else
                prefix = "recursive"
            end if
        end if

        call derive_function_return_type_and_flags(arena, node, return_type_code, &
                                                    is_deferred_char)

        if (allocated(node%param_indices)) then
            params_clause = build_parameter_clause(arena, node%param_indices)
        else
            params_clause = "()"
        end if

        result_clause = build_function_result_clause(arena, node)

        if (len_trim(prefix) > 0) then
            signature = trim(prefix) // " "
        else
            signature = ""
        end if

        if (len_trim(return_type_code) > 0) then
            signature = signature // trim(return_type_code) // " function " // &
                        node%name
        else
            signature = signature // "function " // node%name
        end if

        signature = signature // params_clause // result_clause
        if (allocated(node%bind_c_clause)) then
            if (len_trim(node%bind_c_clause) > 0) then
                signature = signature // " " // trim(node%bind_c_clause)
            end if
        end if
    end function compose_function_signature

    function gather_function_prefix(node, recursive_in_prefix) result(prefix)
        type(function_def_node), intent(in) :: node
        logical, intent(out) :: recursive_in_prefix
        character(len=:), allocatable :: prefix

        prefix = gather_prefix(node%prefix_keywords, recursive_in_prefix)
    end function gather_function_prefix

    subroutine derive_function_return_type_and_flags(arena, node, &
                                                      return_type_code, &
                                                      is_deferred_char)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable, intent(out) :: return_type_code
        logical, intent(out) :: is_deferred_char
        character(len=:), allocatable :: override
        character(len=:), allocatable :: lowered
        logical :: standardize_types_enabled

        return_type_code = ""
        is_deferred_char = .false.

        if (allocated(node%return_type)) then
            return_type_code = trim(node%return_type)
            call get_type_standardization(standardize_types_enabled)
            if (standardize_types_enabled) then
                lowered = to_lower(trim(return_type_code))
                if (lowered == 'real') then
                    return_type_code = "real(8)"
                end if
            end if
        end if

        call derive_character_return_type(arena, node, override)
        if (len_trim(override) > 0) return_type_code = override

        if (len_trim(return_type_code) == 0) return

        if (should_omit_return_type(arena, node, return_type_code)) then
            return_type_code = ""
            return
        end if

        return_type_code = fix_character_len_placeholder(return_type_code)

        if (is_allocatable_array_return(return_type_code)) then
            is_deferred_char = .true.
            return_type_code = ""
            return
        end if

        if (.not. is_deferred_character_return(return_type_code)) return

        is_deferred_char = .true.
        return_type_code = ""
    end subroutine derive_function_return_type_and_flags

    function build_function_result_clause(arena, node) result(result_clause)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: result_clause
        character(len=:), allocatable :: result_name
        character(len=:), allocatable :: function_name
        logical :: rename_result
        logical :: needs_result_for_recursion
        logical :: needs_result_for_array
        logical :: missing_result_variable
        logical :: has_return_type

        result_clause = ""

        if (allocated(node%name)) then
            function_name = trim(node%name)
        else
            function_name = ""
        end if

        if (allocated(node%result_variable)) then
            missing_result_variable = len_trim(node%result_variable) == 0
            if (.not. missing_result_variable) then
                if (len_trim(function_name) > 0) then
                    missing_result_variable = &
                        (trim(node%result_variable) == function_name)
                end if
            end if
        else
            missing_result_variable = .true.
        end if

        has_return_type = allocated(node%return_type)
        if (has_return_type) has_return_type = len_trim(node%return_type) > 0

        rename_result = should_rename_deferred_char_result(node)
        needs_result_for_recursion = node%is_recursive .and. &
                                     missing_result_variable .and. &
                                     has_return_type
        needs_result_for_array = should_use_result_for_array(arena, node) .and. &
                                missing_result_variable

        if (rename_result .or. needs_result_for_recursion .or. needs_result_for_array) then
            result_name = trim(node%name) // "_result"
        else
            if (.not. allocated(node%result_variable)) return
            result_name = trim(node%result_variable)
        end if

        if (len_trim(result_name) == 0) return

        if (allocated(node%name)) then
            if (.not. rename_result .and. .not. needs_result_for_recursion .and. &
                .not. needs_result_for_array) then
                if (result_name == trim(node%name)) return
            end if
        end if

        result_clause = " result(" // result_name // ")"
    end function build_function_result_clause

    function build_function_body_section(arena, node) result(body)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: body
        type(parameter_info_t), allocatable :: param_map(:)
        integer, allocatable :: param_indices(:)
        integer, allocatable :: body_indices(:)
        integer, allocatable :: filtered_body_indices(:)

        call copy_indices(node%param_indices, param_indices)
        call copy_indices(node%body_indices, body_indices)

        call build_parameter_map(arena, param_indices, body_indices, param_map, node)
        if (allocated(node%prefix_keywords)) then
            call apply_default_intents(node%prefix_keywords, param_map)
        end if
        call apply_function_default_intents(param_map)

        body = maybe_add_procedure_implicit_none(arena, body_indices)
        body = body // collect_function_parameter_decls(arena, node, param_map)
        body = body // collect_deferred_char_result_decl(arena, node)
        body = body // collect_recursive_result_decl(arena, node)
        body = body // collect_entry_parameter_decls(arena, node)
        body = body // collect_local_variable_decls(arena, node, param_map)

        call filter_implicit_statements(arena, body_indices, filtered_body_indices)
        body = body // generate_grouped_body_with_params(arena, &
                                                         filtered_body_indices, 1, &
                                                         param_map, node)
        call reorder_import_lines(body)
        call rename_result_variable_in_body(node, body, arena)
    end function build_function_body_section

    logical function should_omit_return_type(arena, node, return_type_code) &
        result(omit)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=*), intent(in) :: return_type_code
        character(len=:), allocatable :: result_name
        character(len=:), allocatable :: lowered_return
        integer :: i
        integer :: decl_index
        integer :: k
        logical :: has_header_return_type

        omit = .false.

        ! Array functions with result clauses have type in result var declaration
        if (should_use_result_for_array(arena, node)) then
            omit = .true.
            return
        end if

        result_name = ""
        if (allocated(node%result_variable)) then
            result_name = trim(node%result_variable)
        end if
        if (len_trim(result_name) == 0 .and. allocated(node%name)) then
            result_name = trim(node%name)
        end if
        if (len_trim(result_name) == 0) return

        if (.not. allocated(node%name)) return

        has_header_return_type = node%has_return_type_in_header

        if (.not. allocated(node%body_indices)) return

        lowered_return = to_lower(trim(return_type_code))
        if (len_trim(lowered_return) == 0) return

        do i = 1, size(node%body_indices)
            decl_index = node%body_indices(i)
            if (decl_index <= 0 .or. decl_index > arena%size) cycle
            if (.not. allocated(arena%entries(decl_index)%node)) cycle
            select type (decl => arena%entries(decl_index)%node)
            type is (declaration_node)
                if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                    do k = 1, size(decl%var_names)
                        if (trim(decl%var_names(k)) == trim(result_name)) then
                            if (has_header_return_type) cycle
                            omit = .true.
                            return
                        end if
                    end do
                else
                    if (trim(decl%var_name) /= trim(result_name)) cycle
                    if (has_header_return_type) cycle
                    omit = .true.
                    return
                end if
            end select
        end do
    end function should_omit_return_type

    function collect_function_parameter_decls(arena, func, param_map) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=:), allocatable :: decl_code
        integer :: i
        integer :: param_idx
        logical :: has_declaration

        decl_code = ""

        if (.not. allocated(func%param_indices)) return

        do i = 1, size(func%param_indices)
            param_idx = func%param_indices(i)
            if (param_idx <= 0 .or. param_idx > arena%size) cycle
            if (.not. allocated(arena%entries(param_idx)%node)) cycle

            has_declaration = parameter_has_declaration(arena, func, param_map, i)

            if (.not. has_declaration .and. i <= size(param_map)) then
                call append_parameter_declaration(arena, param_idx, param_map(i), &
                                                  decl_code)
            end if
        end do
    end function collect_function_parameter_decls

    logical function parameter_has_declaration(arena, func, param_map, param_idx) &
        result(has_decl)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        type(parameter_info_t), intent(in) :: param_map(:)
        integer, intent(in) :: param_idx
        integer :: body_idx
        integer :: k

        has_decl = .false.
        if (.not. allocated(func%body_indices)) return
        if (param_idx > size(param_map)) return

        do body_idx = 1, size(func%body_indices)
            if (func%body_indices(body_idx) <= 0 .or. &
                func%body_indices(body_idx) > arena%size) cycle
            if (.not. allocated(arena%entries(func%body_indices(body_idx))%node)) cycle

            select type (body_node => arena%entries(func%body_indices(body_idx))%node)
            type is (declaration_node)
                if (len_trim(param_map(param_idx)%name) == 0) cycle
                if (body_node%is_multi_declaration .and. &
                    allocated(body_node%var_names)) then
                    do k = 1, size(body_node%var_names)
                        if (trim(body_node%var_names(k)) == &
                            trim(param_map(param_idx)%name)) then
                            has_decl = .true.
                            return
                        end if
                    end do
                else
                    if (trim(body_node%var_name) == trim(param_map(param_idx)%name)) &
                        then
                        has_decl = .true.
                        return
                    end if
                end if
            type is (parameter_declaration_node)
                if (trim(body_node%name) == trim(param_map(param_idx)%name)) then
                    has_decl = .true.
                    return
                end if
            end select
        end do
    end function parameter_has_declaration

    logical function is_function_result_identifier(func, var_name) result(is_result)
        type(function_def_node), intent(in) :: func
        character(len=*), intent(in) :: var_name
        character(len=:), allocatable :: function_name
        character(len=:), allocatable :: result_name
        character(len=:), allocatable :: renamed_result

        is_result = .false.

        if (allocated(func%name)) then
            function_name = trim(func%name)
        else
            function_name = ''
        end if

        if (allocated(func%result_variable)) then
            result_name = trim(func%result_variable)
        else
            result_name = ''
        end if

        if (len_trim(function_name) > 0) then
            if (trim(var_name) == function_name) then
                is_result = .true.
                return
            end if
        end if

        if (len_trim(result_name) > 0) then
            if (trim(var_name) == result_name) then
                is_result = .true.
                return
            end if
            if (result_name == function_name) then
                renamed_result = trim(function_name) // "_result"
                if (trim(var_name) == renamed_result) then
                    is_result = .true.
                end if
            end if
        end if
    end function is_function_result_identifier

    function get_result_type_string(func, var_name, inferred_type) &
        result(type_str)
        type(function_def_node), intent(in) :: func
        character(len=*), intent(in) :: var_name
        type(mono_type_t), intent(in) :: inferred_type
        character(len=:), allocatable :: type_str
        type(mono_type_t) :: deferred_type
        logical :: is_result_var

        is_result_var = .false.
        if (allocated(func%result_variable)) then
            is_result_var = (trim(var_name) == trim(func%result_variable))
        end if

        if (is_result_var .and. inferred_type%kind == TARRAY .and. &
            (inferred_type%size == 0 .or. inferred_type%alloc_info%is_allocatable)) &
            then
            deferred_type = convert_array_to_deferred_shape(inferred_type)
            type_str = mono_type_to_string(deferred_type, include_shape=.true., &
                                           fallback='integer')
        else
            type_str = mono_type_to_string(inferred_type, include_shape=.true., &
                                           fallback='integer')
        end if
    end function get_result_type_string

    recursive function convert_array_to_deferred_shape(typ) result(deferred)
        use type_system_unified, only: create_mono_type
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t) :: deferred
        type(mono_type_t) :: inner
        type(mono_type_t), allocatable :: args(:)

        deferred = typ
        if (typ%kind /= TARRAY) return

        if (typ%get_args_count() > 0) then
            inner = typ%get_arg(1)
            inner = convert_array_to_deferred_shape(inner)
            allocate (args(1))
            args(1) = inner
            deferred = create_mono_type(TARRAY, args=args)
            deferred%size = 0
            deferred%alloc_info%is_allocatable = .true.
        else
            deferred%size = 0
            deferred%alloc_info%is_allocatable = .true.
        end if
    end function convert_array_to_deferred_shape

    logical function needs_result_declaration(arena, func, result_type) &
        result(needed)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        type(mono_type_t), intent(in) :: result_type
        character(len=:), allocatable :: function_name
        character(len=:), allocatable :: result_name
        logical :: header_has_type
        logical :: requires_deferred_attrs

        needed = .false.
        header_has_type = .false.
        requires_deferred_attrs = .false.

        if (allocated(func%name)) then
            function_name = trim(func%name)
        else
            function_name = ''
        end if

        if (allocated(func%result_variable)) then
            result_name = trim(func%result_variable)
        else
            result_name = ''
        end if

        if (len_trim(result_name) == 0) then
            result_name = function_name
        end if

        if (allocated(func%return_type)) then
            if (len_trim(func%return_type) > 0) header_has_type = .true.
        end if

        if (result_type%kind == TARRAY) then
            if (result_type%size == 0 .or. result_type%alloc_info%is_allocatable) then
                requires_deferred_attrs = .true.
            end if
        end if

        if (len_trim(result_name) == 0) return

        if (has_result_variable_declaration(arena, func, result_name)) return

        if (trim(result_name) /= trim(function_name)) then
            if (.not. header_has_type) then
                needed = .true.
                return
            end if
            if (requires_deferred_attrs) then
                needed = .true.
                return
            end if
            return
        end if

        if (.not. header_has_type) then
            needed = .true.
            return
        end if

        if (requires_deferred_attrs) needed = .true.
    end function needs_result_declaration

    logical function has_result_variable_declaration(arena, func, result_name) &
        result(found)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        character(len=*), intent(in) :: result_name
        integer :: i
        integer :: stmt_idx
        integer :: name_idx

        found = .false.
        if (.not. allocated(func%body_indices)) return
        if (len_trim(result_name) == 0) return

        do i = 1, size(func%body_indices)
            stmt_idx = func%body_indices(i)
            if (stmt_idx <= 0 .or. stmt_idx > arena%size) cycle
            if (.not. allocated(arena%entries(stmt_idx)%node)) cycle

            select type (stmt => arena%entries(stmt_idx)%node)
            type is (declaration_node)
                if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
                    do name_idx = 1, size(stmt%var_names)
                        if (trim(stmt%var_names(name_idx)) == trim(result_name)) then
                            found = .true.
                            return
                        end if
                    end do
                else if (allocated(stmt%var_name)) then
                    if (trim(stmt%var_name) == trim(result_name)) then
                        found = .true.
                        return
                    end if
                end if
            end select
        end do
    end function has_result_variable_declaration

    function collect_local_variable_decls(arena, func, param_map) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=:), allocatable :: decl_code
        character(len=64), allocatable :: local_vars(:)
        integer :: i
        integer :: stmt_idx
        integer :: n_locals
        integer :: capacity
        character(len=64) :: var_name
        character(len=:), allocatable :: result_name

        decl_code = ""
        n_locals = 0
        capacity = 0

        result_name = ""
        if (allocated(func%result_variable)) then
            result_name = trim(func%result_variable)
        else if (allocated(func%name)) then
            result_name = trim(func%name)
        end if

        if (.not. allocated(func%body_indices)) return

        do i = 1, size(func%body_indices)
            stmt_idx = func%body_indices(i)
            if (stmt_idx <= 0 .or. stmt_idx > arena%size) cycle
            if (.not. allocated(arena%entries(stmt_idx)%node)) cycle

            select type (stmt => arena%entries(stmt_idx)%node)
            type is (assignment_node)
                if (stmt%target_index <= 0 .or. stmt%target_index > arena%size) cycle
                if (.not. allocated(arena%entries(stmt%target_index)%node)) cycle

                select type (target => arena%entries(stmt%target_index)%node)
                type is (identifier_node)
                    if (.not. allocated(target%name)) cycle
                    if (target%inferred_type%kind == 0) cycle

                    var_name = trim(target%name)

                    if (is_function_result_identifier(func, var_name)) then
                        if (.not. needs_result_declaration(arena, func, &
                                                           target%inferred_type)) &
                            cycle
                        var_name = trim(result_name)
                    end if

                    if (is_parameter_name(var_name, param_map)) cycle

                    if (.not. is_local_var_collected(var_name, local_vars, n_locals)) &
                        then
                        call ensure_local_var_capacity(local_vars, capacity, &
                                                       n_locals + 1)
                        n_locals = n_locals + 1
                        local_vars(n_locals) = var_name
                        decl_code = decl_code // "    " // &
                                    get_result_type_string(func, var_name, &
                                                           target%inferred_type) // &
                                    " :: " // trim(var_name) // new_line('A')
                    end if
                end select
            type is (print_statement_node)
                call collect_vars_from_print(arena, stmt, param_map, local_vars, &
                                             n_locals, capacity, result_name, &
                                             decl_code)
            type is (read_statement_node)
                call collect_vars_from_read(arena, stmt, param_map, local_vars, &
                                            n_locals, capacity, result_name, &
                                            decl_code)
            type is (do_loop_node)
                call collect_loop_var(arena, func, i, stmt, param_map, &
                                      local_vars, n_locals, capacity, &
                                      result_name, decl_code)
            end select
        end do
    end function collect_local_variable_decls

    subroutine collect_vars_from_print(arena, stmt, param_map, local_vars, &
                                       n_locals, capacity, result_name, decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(print_statement_node), intent(in) :: stmt
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=64), allocatable, intent(inout) :: local_vars(:)
        integer, intent(inout) :: n_locals
        integer, intent(inout) :: capacity
        character(len=*), intent(in) :: result_name
        character(len=:), allocatable, intent(inout) :: decl_code
        integer :: j
        integer :: expr_idx
        character(len=64) :: var_name

        if (.not. allocated(stmt%expression_indices)) return

        do j = 1, size(stmt%expression_indices)
            expr_idx = stmt%expression_indices(j)
            if (expr_idx <= 0 .or. expr_idx > arena%size) cycle
            if (.not. allocated(arena%entries(expr_idx)%node)) cycle

            select type (expr => arena%entries(expr_idx)%node)
            type is (identifier_node)
                if (.not. allocated(expr%name)) cycle

                var_name = trim(expr%name)

                if (len_trim(result_name) > 0 .and. var_name == result_name) cycle
                if (is_parameter_name(var_name, param_map)) cycle

                if (.not. is_local_var_collected(var_name, local_vars, n_locals)) then
                    call ensure_local_var_capacity(local_vars, capacity, n_locals + 1)
                    n_locals = n_locals + 1
                    local_vars(n_locals) = var_name
                    decl_code = decl_code // "    real :: " // trim(var_name) // &
                                new_line('A')
                end if
            end select
        end do
    end subroutine collect_vars_from_print

    subroutine collect_vars_from_read(arena, stmt, param_map, local_vars, &
                                      n_locals, capacity, result_name, decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(read_statement_node), intent(in) :: stmt
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=64), allocatable, intent(inout) :: local_vars(:)
        integer, intent(inout) :: n_locals
        integer, intent(inout) :: capacity
        character(len=*), intent(in) :: result_name
        character(len=:), allocatable, intent(inout) :: decl_code
        integer :: j
        integer :: var_idx
        character(len=64) :: var_name

        if (.not. allocated(stmt%var_indices)) return

        do j = 1, size(stmt%var_indices)
            var_idx = stmt%var_indices(j)
            if (var_idx <= 0 .or. var_idx > arena%size) cycle
            if (.not. allocated(arena%entries(var_idx)%node)) cycle

            select type (var => arena%entries(var_idx)%node)
            type is (identifier_node)
                if (.not. allocated(var%name)) cycle

                var_name = trim(var%name)

                if (len_trim(result_name) > 0 .and. var_name == result_name) cycle
                if (is_parameter_name(var_name, param_map)) cycle

                if (.not. is_local_var_collected(var_name, local_vars, n_locals)) then
                    call ensure_local_var_capacity(local_vars, capacity, n_locals + 1)
                    n_locals = n_locals + 1
                    local_vars(n_locals) = var_name
                    decl_code = decl_code // "    real :: " // trim(var_name) // &
                                new_line('A')
                end if
            end select
        end do
    end subroutine collect_vars_from_read

    recursive subroutine collect_loop_var(arena, func, stmt_position, loop_node, &
                                          param_map, local_vars, n_locals, capacity, &
                                          result_name, decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        integer, intent(in) :: stmt_position
        type(do_loop_node), intent(in) :: loop_node
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=64), allocatable, intent(inout) :: local_vars(:)
        integer, intent(inout) :: n_locals
        integer, intent(inout) :: capacity
        character(len=*), intent(in) :: result_name
        character(len=:), allocatable, intent(inout) :: decl_code
        character(len=64) :: var_name
        integer :: body_idx, nested_idx

        if (.not. allocated(loop_node%var_name)) return

        var_name = trim(loop_node%var_name)
        if (len_trim(var_name) == 0) return

        if (len_trim(result_name) > 0 .and. var_name == result_name) return
        if (is_parameter_name(var_name, param_map)) return
        if (index(decl_code, "integer :: "//trim(var_name)) > 0) return
        if (has_explicit_loop_declaration(arena, func, stmt_position, var_name)) return

        if (.not. is_local_var_collected(var_name, local_vars, n_locals)) then
            call ensure_local_var_capacity(local_vars, capacity, n_locals + 1)
            n_locals = n_locals + 1
            local_vars(n_locals) = var_name
            decl_code = decl_code // "    integer :: " // trim(var_name) // &
                        new_line('A')
        end if

        if (allocated(loop_node%body_indices)) then
            do body_idx = 1, size(loop_node%body_indices)
                nested_idx = loop_node%body_indices(body_idx)
                if (nested_idx <= 0 .or. nested_idx > arena%size) cycle
                if (.not. allocated(arena%entries(nested_idx)%node)) cycle

                select type (nested_stmt => arena%entries(nested_idx)%node)
                type is (do_loop_node)
                    call collect_loop_var(arena, func, stmt_position, nested_stmt, &
                                          param_map, local_vars, n_locals, capacity, &
                                          result_name, decl_code)
                end select
            end do
        end if
    end subroutine collect_loop_var

    logical function has_explicit_loop_declaration(arena, func, stmt_position, &
                                                   var_name) &
        result(has_decl)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        integer, intent(in) :: stmt_position
        character(len=*), intent(in) :: var_name
        integer :: idx
        integer :: stmt_idx
        integer :: name_idx

        has_decl = .false.
        if (.not. allocated(func%body_indices)) return
        if (stmt_position <= 1) return

        do idx = 1, stmt_position - 1
            stmt_idx = func%body_indices(idx)
            if (stmt_idx <= 0 .or. stmt_idx > arena%size) cycle
            if (.not. allocated(arena%entries(stmt_idx)%node)) cycle

            select type (decl => arena%entries(stmt_idx)%node)
            type is (declaration_node)
                if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                    do name_idx = 1, size(decl%var_names)
                        if (trim(decl%var_names(name_idx)) == trim(var_name)) then
                            has_decl = .true.
                            return
                        end if
                    end do
                else if (allocated(decl%var_name)) then
                    if (trim(decl%var_name) == trim(var_name)) then
                        has_decl = .true.
                        return
                    end if
                end if
            end select
        end do
    end function has_explicit_loop_declaration

    function collect_deferred_char_result_decl(arena, node) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: decl_code
        character(len=:), allocatable :: full_type
        character(len=:), allocatable :: base_type
        character(len=:), allocatable :: result_name

        decl_code = ""

        call get_deferred_char_type_info(arena, node, full_type, base_type, &
                                         result_name)

        if (len_trim(base_type) == 0) return

        decl_code = "    " // trim(base_type) // ", allocatable :: " // &
                    trim(result_name) // new_line('A')
    end function collect_deferred_char_result_decl

    function collect_recursive_result_decl(arena, node) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: decl_code
        character(len=:), allocatable :: type_str
        character(len=:), allocatable :: result_name
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: function_name
        logical :: standardize_types_enabled
        logical :: missing_result_variable

        decl_code = ""

        if (.not. node%is_recursive) return
        if (.not. allocated(node%name)) return

        function_name = trim(node%name)

        if (allocated(node%result_variable)) then
            missing_result_variable = len_trim(node%result_variable) == 0
            if (.not. missing_result_variable) then
                if (len_trim(function_name) > 0) then
                    missing_result_variable = &
                        (trim(node%result_variable) == function_name)
                end if
            end if
        else
            missing_result_variable = .true.
        end if
        if (.not. missing_result_variable) return

        if (should_rename_deferred_char_result(node)) return

        if (.not. allocated(node%return_type)) return
        if (len_trim(node%return_type) == 0) return

        type_str = trim(node%return_type)
        call get_type_standardization(standardize_types_enabled)
        if (standardize_types_enabled) then
            lowered = to_lower(trim(type_str))
            if (lowered == 'real') then
                type_str = "real(8)"
            end if
        end if

        result_name = trim(function_name) // "_result"

        decl_code = "    " // trim(type_str) // " :: " // &
                    trim(result_name) // new_line('A')
    end function collect_recursive_result_decl

    function collect_array_result_decl(arena, node) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: decl_code
        character(len=:), allocatable :: type_str
        character(len=:), allocatable :: result_name
        character(len=:), allocatable :: function_name
        logical :: missing_result_variable

        decl_code = ""

        if (.not. should_use_result_for_array(arena, node)) return
        if (.not. allocated(node%name)) return

        function_name = trim(node%name)

        if (allocated(node%result_variable)) then
            missing_result_variable = len_trim(node%result_variable) == 0
            if (.not. missing_result_variable) then
                if (len_trim(function_name) > 0) then
                    missing_result_variable = &
                        (trim(node%result_variable) == function_name)
                end if
            end if
        else
            missing_result_variable = .true.
        end if
        if (.not. missing_result_variable) return

        if (should_rename_deferred_char_result(node)) return

        if (.not. allocated(node%return_type)) return
        if (len_trim(node%return_type) == 0) return

        type_str = trim(node%return_type)
        result_name = trim(function_name) // "_result"

        decl_code = "    " // trim(type_str) // " :: " // &
                    trim(result_name) // new_line('A')
    end function collect_array_result_decl

    subroutine get_deferred_char_type_info(arena, node, full_type, base_type, &
                                           result_name)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable, intent(out) :: full_type
        character(len=:), allocatable, intent(out) :: base_type
        character(len=:), allocatable, intent(out) :: result_name
        character(len=:), allocatable :: override
        character(len=:), allocatable :: lowered
        logical :: standardize_types_enabled

        full_type = ""
        base_type = ""
        result_name = ""

        if (allocated(node%return_type)) then
            full_type = trim(node%return_type)
            call get_type_standardization(standardize_types_enabled)
            if (standardize_types_enabled) then
                lowered = to_lower(trim(full_type))
                if (lowered == 'real') then
                    full_type = "real(8)"
                end if
            end if
        end if

        call derive_character_return_type(arena, node, override)
        if (len_trim(override) > 0) full_type = override

        if (len_trim(full_type) == 0) return

        full_type = fix_character_len_placeholder(full_type)

        if (.not. is_deferred_character_return(full_type)) then
            full_type = ""
            return
        end if

        if (has_character_len_result_decl(arena, node)) then
            full_type = ""
            return
        end if

        base_type = strip_allocatable(full_type)

        if (allocated(node%result_variable)) then
            result_name = trim(node%result_variable)
            if (allocated(node%name)) then
                if (result_name == trim(node%name)) then
                    result_name = trim(node%name) // "_result"
                end if
            end if
        else if (allocated(node%name)) then
            result_name = trim(node%name)
        end if
    end subroutine get_deferred_char_type_info

    pure function strip_allocatable(type_string) result(stripped)
        character(len=*), intent(in) :: type_string
        character(len=:), allocatable :: stripped
        character(len=:), allocatable :: lowered
        integer :: alloc_pos, comma_pos

        stripped = trim(type_string)
        lowered = to_lower(stripped)

        alloc_pos = index(lowered, 'allocatable')
        if (alloc_pos == 0) return

        comma_pos = index(lowered(1:alloc_pos-1), ',', back=.true.)
        if (comma_pos > 0) then
            stripped = stripped(1:comma_pos-1) // stripped(alloc_pos+11:)
        else
            stripped = trim(adjustl(stripped(alloc_pos+11:)))
        end if
    end function strip_allocatable

    function collect_entry_parameter_decls(arena, func) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        character(len=:), allocatable :: decl_code
        integer :: i
        integer :: stmt_idx
        logical :: found_exec_stmt
        character(len=64), allocatable :: entry_params(:)
        integer :: num_entry_params

        decl_code = ""
        found_exec_stmt = .false.
        num_entry_params = 0

        if (.not. allocated(func%body_indices)) return

        do i = 1, size(func%body_indices)
            stmt_idx = func%body_indices(i)
            if (stmt_idx <= 0 .or. stmt_idx > arena%size) cycle
            if (.not. allocated(arena%entries(stmt_idx)%node)) cycle

            select type (stmt => arena%entries(stmt_idx)%node)
            type is (entry_node)
                call collect_entry_param_names(stmt, entry_params, num_entry_params)
                found_exec_stmt = .true.
            type is (assignment_node)
                found_exec_stmt = .true.
            type is (print_statement_node)
                found_exec_stmt = .true.
            type is (declaration_node)
                if (found_exec_stmt) then
                    if (is_entry_parameter_decl(stmt, entry_params, num_entry_params)) &
                        then
                        decl_code = decl_code // "    " // &
                                    generate_code_from_arena(arena, stmt_idx) // &
                                    new_line('A')
                    end if
                end if
            end select
        end do
    end function collect_entry_parameter_decls


    subroutine rename_result_variable_in_body(node, body, arena)
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable, intent(inout) :: body
        type(ast_arena_t), intent(in) :: arena
        character(len=:), allocatable :: old_name, new_name
        character(len=:), allocatable :: search_pattern, replace_pattern
        integer :: pos, start_pos
        character(len=:), allocatable :: function_name
        logical :: needs_rename
        logical :: missing_result_variable
        logical :: has_return_type

        if (.not. allocated(node%name)) return
        function_name = trim(node%name)

        if (allocated(node%result_variable)) then
            missing_result_variable = len_trim(node%result_variable) == 0
            if (.not. missing_result_variable) then
                if (len_trim(function_name) > 0) then
                    missing_result_variable = &
                        (trim(node%result_variable) == function_name)
                end if
            end if
        else
            missing_result_variable = .true.
        end if

        has_return_type = allocated(node%return_type)
        if (has_return_type) has_return_type = len_trim(node%return_type) > 0

        needs_rename = should_rename_deferred_char_result(node) .or. &
                       (node%is_recursive .and. has_return_type .and. &
                        missing_result_variable) .or. &
                       (should_use_result_for_array(arena, node) .and. &
                        missing_result_variable)

        if (.not. needs_rename) return

        if (allocated(node%result_variable)) then
            if (len_trim(node%result_variable) > 0) then
                old_name = trim(node%result_variable)
            else
                old_name = function_name
            end if
        else
            old_name = function_name
        end if
        new_name = trim(function_name) // "_result"

        start_pos = 1
        do
            pos = index(body(start_pos:), old_name)
            if (pos == 0) exit

            pos = pos + start_pos - 1

            if (pos > 1) then
                if (is_identifier_char(body(pos - 1:pos - 1))) then
                    start_pos = pos + len(old_name)
                    cycle
                end if
            end if

            if (pos + len(old_name) <= len(body)) then
                if (is_identifier_char(body(pos + len(old_name):pos + &
                                            len(old_name)))) then
                    start_pos = pos + len(old_name)
                    cycle
                end if
            end if

            body = body(1:pos - 1) // new_name // body(pos + len(old_name):)
            start_pos = pos + len(new_name)
        end do
    end subroutine rename_result_variable_in_body

    logical function should_rename_deferred_char_result(node) result(should_rename)
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: lowered

        should_rename = .false.

        if (.not. allocated(node%name)) return
        if (.not. allocated(node%result_variable)) return
        if (trim(node%result_variable) /= trim(node%name)) return
        if (.not. allocated(node%return_type)) return

        lowered = to_lower(trim(node%return_type))
        should_rename = (index(lowered, 'character') == 1) .and. &
                        (index(lowered, 'len=:') > 0)
    end function should_rename_deferred_char_result

    logical function should_use_result_for_array(arena, node) result(needs_result)
        use ast_nodes_core, only: array_literal_node
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: result_name
        integer :: i, stmt_idx

        needs_result = .false.

        ! Determine result variable name
        if (allocated(node%result_variable)) then
            result_name = trim(node%result_variable)
        else if (allocated(node%name)) then
            result_name = trim(node%name)
        else
            return
        end if

        if (len_trim(result_name) == 0) return

        ! Check if the result variable has an inferred array type
        if (.not. allocated(node%body_indices)) return

        do i = 1, size(node%body_indices)
            stmt_idx = node%body_indices(i)
            if (stmt_idx <= 0 .or. stmt_idx > arena%size) cycle
            if (.not. allocated(arena%entries(stmt_idx)%node)) cycle

            select type (stmt => arena%entries(stmt_idx)%node)
            type is (assignment_node)
                if (stmt%target_index <= 0 .or. stmt%target_index > arena%size) cycle
                if (.not. allocated(arena%entries(stmt%target_index)%node)) cycle

                select type (target => arena%entries(stmt%target_index)%node)
                type is (identifier_node)
                    if (.not. allocated(target%name)) cycle
                    if (trim(target%name) /= trim(result_name)) cycle

                    ! Found assignment to result variable - check inferred type
                    if (target%inferred_type%kind == TARRAY) then
                        needs_result = .true.
                        return
                    end if
                end select
            end select
        end do
    end function should_use_result_for_array

    logical function is_identifier_char(c) result(is_id)
        character(len=1), intent(in) :: c
        is_id = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z') .or. &
                (c >= '0' .and. c <= '9') .or. c == '_'
    end function is_identifier_char

end module codegen_function_declarations
