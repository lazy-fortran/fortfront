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
                                              is_deferred_character_return
    use codegen_procedure_shared, only: build_parameter_clause, gather_prefix, &
                                        copy_indices, apply_default_intents, &
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
    use string_utils_mod, only: to_lower
    use type_string_utils, only: mono_type_to_string
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

        result_clause = build_function_result_clause(arena, node, is_deferred_char)

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

        if (.not. is_deferred_character_return(return_type_code)) return

        is_deferred_char = .true.
        return_type_code = ""
    end subroutine derive_function_return_type_and_flags

    function build_function_result_clause(arena, node, is_deferred_char) &
        result(result_clause)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        logical, intent(in) :: is_deferred_char
        character(len=:), allocatable :: result_clause
        character(len=:), allocatable :: result_name

        result_clause = ""

        if (.not. allocated(node%result_variable)) return
        result_name = trim(node%result_variable)
        if (len_trim(result_name) == 0) return

        if (allocated(node%name)) then
            if (result_name == trim(node%name)) return
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

        body = maybe_add_procedure_implicit_none(arena, body_indices)
        body = body // collect_function_parameter_decls(arena, node, param_map)
        body = body // collect_deferred_char_result_decl(arena, node)
        body = body // collect_entry_parameter_decls(arena, node)
        body = body // collect_local_variable_decls(arena, node, param_map)

        call filter_implicit_statements(arena, body_indices, filtered_body_indices)
        body = body // generate_grouped_body_with_params(arena, &
                                                         filtered_body_indices, 1, &
                                                         param_map, node)
        call reorder_import_lines(body)
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

                    if (len_trim(result_name) > 0 .and. var_name == result_name) cycle

                    if (is_parameter_name(var_name, param_map)) cycle

                    if (.not. is_local_var_collected(var_name, local_vars, n_locals)) &
                        then
                        call ensure_local_var_capacity(local_vars, capacity, &
                                                       n_locals + 1)
                        n_locals = n_locals + 1
                        local_vars(n_locals) = var_name
                        decl_code = decl_code // "    " // &
                                    mono_type_to_string(target%inferred_type, &
                                                        include_shape=.true., &
                                                        fallback='integer') // &
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

    subroutine collect_entry_param_names(entry_stmt, param_list, num_params)
        type(entry_node), intent(in) :: entry_stmt
        character(len=64), allocatable, intent(inout) :: param_list(:)
        integer, intent(inout) :: num_params
        character(len=:), allocatable :: params_text
        character(len=:), allocatable :: clean_params
        integer :: start_pos
        integer :: end_pos
        integer :: comma_pos
        character(len=64) :: param_name

        if (.not. allocated(entry_stmt%params_text)) return

        params_text = entry_stmt%params_text

        start_pos = index(params_text, '(')
        if (start_pos == 0) return
        end_pos = index(params_text, ')')
        if (end_pos == 0) return

        clean_params = params_text(start_pos+1:end_pos-1)
        clean_params = adjustl(clean_params)

        if (len_trim(clean_params) == 0) return

        if (.not. allocated(param_list)) allocate (param_list(10))

        do while (len_trim(clean_params) > 0)
            comma_pos = index(clean_params, ',')
            if (comma_pos > 0) then
                param_name = trim(adjustl(clean_params(1:comma_pos-1)))
                clean_params = adjustl(clean_params(comma_pos+1:))
            else
                param_name = trim(adjustl(clean_params))
                clean_params = ""
            end if

            if (len_trim(param_name) > 0) then
                num_params = num_params + 1
                if (num_params > size(param_list)) then
                    call expand_param_list(param_list, num_params)
                end if
                param_list(num_params) = param_name
            end if
        end do
    end subroutine collect_entry_param_names

    subroutine expand_param_list(param_list, new_size)
        character(len=64), allocatable, intent(inout) :: param_list(:)
        integer, intent(in) :: new_size
        character(len=64), allocatable :: temp(:)
        integer :: old_size

        old_size = size(param_list)
        allocate (temp(old_size))
        temp = param_list
        deallocate (param_list)
        allocate (param_list(new_size * 2))
        param_list(1:old_size) = temp
    end subroutine expand_param_list

    logical function is_entry_parameter_decl(decl, entry_params, num_params) &
        result(is_entry_param)
        type(declaration_node), intent(in) :: decl
        character(len=64), intent(in) :: entry_params(:)
        integer, intent(in) :: num_params
        integer :: i

        is_entry_param = .false.

        if (num_params == 0) return

        if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
            do i = 1, size(decl%var_names)
                if (is_in_entry_params(decl%var_names(i), entry_params, &
                                       num_params)) then
                    is_entry_param = .true.
                    return
                end if
            end do
        else if (allocated(decl%var_name)) then
            is_entry_param = is_in_entry_params(decl%var_name, entry_params, &
                                                num_params)
        end if
    end function is_entry_parameter_decl

    logical function is_in_entry_params(var_name, entry_params, num_params) &
        result(found)
        character(len=*), intent(in) :: var_name
        character(len=64), intent(in) :: entry_params(:)
        integer, intent(in) :: num_params
        integer :: i

        found = .false.
        do i = 1, num_params
            if (trim(var_name) == trim(entry_params(i))) then
                found = .true.
                return
            end if
        end do
    end function is_in_entry_params

end module codegen_function_declarations
