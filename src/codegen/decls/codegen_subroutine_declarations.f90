module codegen_subroutine_declarations
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, assignment_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use ast_nodes_io, only: print_statement_node, read_statement_node
    use ast_nodes_loops, only: do_loop_node
    use ast_nodes_procedure, only: subroutine_def_node
    use ast_nodes_misc, only: use_statement_node
    use codegen_declarations_inference, only: build_parameter_map
    use codegen_procedure_shared, only: build_parameter_clause, gather_prefix, &
        copy_indices, apply_default_intents, &
        maybe_add_procedure_implicit_none, &
        filter_implicit_statements, &
        append_parameter_declaration, &
        is_parameter_name, ensure_local_var_capacity, &
        is_local_var_collected, add_declared_vars, &
        add_single_declared_var
    use codegen_grouped_body_params, only: generate_grouped_body_with_params
    use codegen_import_reorder, only: reorder_import_lines
    use codegen_parameter_info, only: parameter_info_t
    use codegen_arena_interface, only: generate_code_from_arena
    use type_string_utils, only: mono_type_to_string
    implicit none
    private
    public :: generate_code_subroutine_def, extract_use_statements

contains

    function generate_code_subroutine_def(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = compose_subroutine_signature(arena, node)
        code = code // new_line('A')
        code = code // build_subroutine_body_section(arena, node, node_index)
        code = code // "end subroutine " // node%name
    end function generate_code_subroutine_def

    function compose_subroutine_signature(arena, node) result(signature)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: node
        character(len=:), allocatable :: signature
        character(len=:), allocatable :: prefix
        character(len=:), allocatable :: params_clause
        logical :: recursive_in_prefix

        prefix = gather_subroutine_prefix(node, recursive_in_prefix)
        if (node%is_recursive .and. .not. recursive_in_prefix) then
            if (len_trim(prefix) > 0) then
                prefix = "recursive " // trim(prefix)
            else
                prefix = "recursive"
            end if
        end if

        if (allocated(node%param_indices)) then
            params_clause = build_parameter_clause(arena, node%param_indices)
        else
            params_clause = "()"
        end if

        if (len_trim(prefix) > 0) then
            signature = trim(prefix) // " subroutine " // node%name // params_clause
        else
            signature = "subroutine " // node%name // params_clause
        end if
        if (allocated(node%bind_c_clause)) then
            if (len_trim(node%bind_c_clause) > 0) then
                signature = signature // " " // trim(node%bind_c_clause)
            end if
        end if
    end function compose_subroutine_signature

    function gather_subroutine_prefix(node, recursive_in_prefix) result(prefix)
        type(subroutine_def_node), intent(in) :: node
        logical, intent(out) :: recursive_in_prefix
        character(len=:), allocatable :: prefix

        prefix = gather_prefix(node%prefix_keywords, recursive_in_prefix)
    end function gather_subroutine_prefix

    function build_subroutine_body_section(arena, node, node_index) result(body)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: body
        type(parameter_info_t), allocatable :: param_map(:)
        integer, allocatable :: param_indices(:)
        integer, allocatable :: body_indices(:)
        integer, allocatable :: filtered_body_indices(:)
        character(len=:), allocatable :: use_section
        integer, allocatable :: trimmed_indices(:)

        call copy_indices(node%param_indices, param_indices)
        call copy_indices(node%body_indices, body_indices)

        call extract_use_statements(arena, body_indices, trimmed_indices, use_section)
        call move_alloc(trimmed_indices, body_indices)

        call build_parameter_map(arena, param_indices, body_indices, param_map, node)
        if (allocated(node%prefix_keywords)) then
            call apply_default_intents(node%prefix_keywords, param_map)
        end if

        body = ""
        if (len_trim(use_section) > 0) body = body // use_section

        body = body // maybe_add_procedure_implicit_none(arena, body_indices, &
            node_index)
        body = body // collect_subroutine_parameter_decls(arena, node, param_map)
        body = body // collect_subroutine_local_variable_decls(arena, node, param_map)

        call filter_implicit_statements(arena, body_indices, filtered_body_indices)
        body = body // generate_grouped_body_with_params(arena, &
            filtered_body_indices, 1, &
            param_map, node)
        call reorder_import_lines(body)
    end function build_subroutine_body_section

    function collect_subroutine_parameter_decls(arena, sub, param_map) &
            result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: sub
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=:), allocatable :: decl_code
        integer :: i
        integer :: param_idx
        logical :: has_declaration

        decl_code = ""

        if (.not. allocated(sub%param_indices)) return

        do i = 1, size(sub%param_indices)
            param_idx = sub%param_indices(i)
            if (.not. arena%has_node_at(param_idx)) cycle

            has_declaration = subroutine_parameter_has_declaration(arena, sub, &
                param_map, i)

            if (.not. has_declaration .and. i <= size(param_map)) then
                call append_parameter_declaration(arena, param_idx, param_map(i), &
                    decl_code)
            end if
        end do
    end function collect_subroutine_parameter_decls

    logical function subroutine_parameter_has_declaration(arena, sub, param_map, &
            param_idx) result(has_decl)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: sub
        type(parameter_info_t), intent(in) :: param_map(:)
        integer, intent(in) :: param_idx
        integer :: j
        integer :: body_idx
        integer :: k

        has_decl = .false.
        if (.not. allocated(sub%body_indices)) return
        if (param_idx > size(param_map)) return

        do j = 1, size(sub%body_indices)
            body_idx = sub%body_indices(j)
            if (.not. arena%has_node_at(body_idx)) cycle
            select type (body_node => arena%entries(body_idx)%node)
                type is (declaration_node)
                if (len_trim(param_map(param_idx)%name) == 0) cycle
                if (trim(body_node%var_name) == trim(param_map(param_idx)%name)) then
                    has_decl = .true.
                    return
                end if
                if (body_node%is_multi_declaration .and. &
                    allocated(body_node%var_names)) then
                    do k = 1, size(body_node%var_names)
                        if (trim(body_node%var_names(k)) == &
                            trim(param_map(param_idx)%name)) then
                            has_decl = .true.
                            return
                        end if
                    end do
                end if
                type is (parameter_declaration_node)
                if (trim(body_node%name) == trim(param_map(param_idx)%name)) then
                    has_decl = .true.
                    return
                end if
            end select
        end do
    end function subroutine_parameter_has_declaration

    function collect_subroutine_local_variable_decls(arena, sub, param_map) &
            result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: sub
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=:), allocatable :: decl_code
        character(len=64), allocatable :: local_vars(:)
        character(len=64), allocatable :: declared_vars(:)
        integer :: i
        integer :: stmt_idx
        integer :: n_locals
        integer :: capacity
        integer :: n_declared
        integer :: declared_capacity
        character(len=64) :: var_name

        decl_code = ""
        n_locals = 0
        capacity = 0
        n_declared = 0
        declared_capacity = 0

        if (.not. allocated(sub%body_indices)) return

        do i = 1, size(sub%body_indices)
            stmt_idx = sub%body_indices(i)
            if (.not. arena%has_node_at(stmt_idx)) cycle

            select type (stmt => arena%entries(stmt_idx)%node)
                type is (declaration_node)
                if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
                    call add_declared_vars(stmt%var_names, declared_vars, n_declared, &
                        declared_capacity)
                else
                    call add_single_declared_var(stmt%var_name, declared_vars, &
                        n_declared, declared_capacity)
                end if
                type is (parameter_declaration_node)
                call add_single_declared_var(stmt%name, declared_vars, n_declared, &
                    declared_capacity)
                type is (assignment_node)
                call collect_vars_from_assignment_sub(arena, stmt, param_map, &
                    local_vars, n_locals, &
                    capacity, declared_vars, &
                    n_declared, decl_code)
                type is (print_statement_node)
                call collect_vars_from_print_sub(arena, stmt, param_map, &
                    local_vars, n_locals, capacity, &
                    declared_vars, n_declared, &
                    decl_code)
                type is (read_statement_node)
                call collect_vars_from_read_sub(arena, stmt, param_map, &
                    local_vars, n_locals, capacity, &
                    declared_vars, n_declared, &
                    decl_code)
                type is (do_loop_node)
                call collect_loop_var_sub(arena, stmt, param_map, local_vars, &
                    n_locals, capacity, declared_vars, &
                    n_declared, decl_code)
            end select
        end do
    end function collect_subroutine_local_variable_decls

    subroutine collect_vars_from_print_sub(arena, stmt, param_map, local_vars, &
            n_locals, capacity, declared_vars, &
            n_declared, decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(print_statement_node), intent(in) :: stmt
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=64), allocatable, intent(inout) :: local_vars(:)
        integer, intent(inout) :: n_locals
        integer, intent(inout) :: capacity
        character(len=64), allocatable, intent(in) :: declared_vars(:)
        integer, intent(in) :: n_declared
        character(len=:), allocatable, intent(inout) :: decl_code
        integer :: j
        integer :: expr_idx
        character(len=64) :: var_name

        if (.not. allocated(stmt%expression_indices)) return

        do j = 1, size(stmt%expression_indices)
            expr_idx = stmt%expression_indices(j)
            if (.not. arena%has_node_at(expr_idx)) cycle

            select type (expr => arena%entries(expr_idx)%node)
                type is (identifier_node)
                if (.not. allocated(expr%name)) cycle

                var_name = trim(expr%name)

                if (is_parameter_name(var_name, param_map)) cycle
                if (is_local_var_collected(var_name, declared_vars, n_declared)) cycle

                if (.not. is_local_var_collected(var_name, local_vars, n_locals)) then
                    call ensure_local_var_capacity(local_vars, capacity, n_locals + 1)
                    n_locals = n_locals + 1
                    local_vars(n_locals) = var_name
                    decl_code = decl_code // "    real :: " // trim(var_name) // &
                        new_line('A')
                end if
            end select
        end do
    end subroutine collect_vars_from_print_sub

    subroutine collect_vars_from_read_sub(arena, stmt, param_map, local_vars, &
            n_locals, capacity, declared_vars, &
            n_declared, decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(read_statement_node), intent(in) :: stmt
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=64), allocatable, intent(inout) :: local_vars(:)
        integer, intent(inout) :: n_locals
        integer, intent(inout) :: capacity
        character(len=64), allocatable, intent(in) :: declared_vars(:)
        integer, intent(in) :: n_declared
        character(len=:), allocatable, intent(inout) :: decl_code
        integer :: j
        integer :: var_idx
        character(len=64) :: var_name

        if (.not. allocated(stmt%var_indices)) return

        do j = 1, size(stmt%var_indices)
            var_idx = stmt%var_indices(j)
            if (.not. arena%has_node_at(var_idx)) cycle

            select type (var => arena%entries(var_idx)%node)
                type is (identifier_node)
                if (.not. allocated(var%name)) cycle

                var_name = trim(var%name)

                if (is_parameter_name(var_name, param_map)) cycle
                if (is_local_var_collected(var_name, declared_vars, n_declared)) cycle

                if (.not. is_local_var_collected(var_name, local_vars, n_locals)) then
                    call ensure_local_var_capacity(local_vars, capacity, n_locals + 1)
                    n_locals = n_locals + 1
                    local_vars(n_locals) = var_name
                    decl_code = decl_code // "    real :: " // trim(var_name) // &
                        new_line('A')
                end if
            end select
        end do
    end subroutine collect_vars_from_read_sub

    recursive subroutine collect_loop_var_sub(arena, loop_node, param_map, &
            local_vars, n_locals, capacity, &
            declared_vars, n_declared, decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(do_loop_node), intent(in) :: loop_node
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=64), allocatable, intent(inout) :: local_vars(:)
        integer, intent(inout) :: n_locals
        integer, intent(inout) :: capacity
        character(len=64), allocatable, intent(in) :: declared_vars(:)
        integer, intent(in) :: n_declared
        character(len=:), allocatable, intent(inout) :: decl_code
        character(len=64) :: var_name
        integer :: body_idx, nested_idx

        if (.not. allocated(loop_node%var_name)) return

        var_name = trim(loop_node%var_name)
        if (len_trim(var_name) == 0) return

        if (is_parameter_name(var_name, param_map)) return
        if (is_local_var_collected(var_name, declared_vars, n_declared)) return

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
                if (.not. arena%has_node_at(nested_idx)) cycle

                select type (nested_stmt => arena%entries(nested_idx)%node)
                    type is (do_loop_node)
                    call collect_loop_var_sub(arena, nested_stmt, param_map, &
                        local_vars, n_locals, capacity, &
                        declared_vars, n_declared, decl_code)
                end select
            end do
        end if
    end subroutine collect_loop_var_sub

    subroutine collect_vars_from_assignment_sub(arena, stmt, param_map, &
            local_vars, n_locals, capacity, &
            declared_vars, n_declared, decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: stmt
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=64), allocatable, intent(inout) :: local_vars(:)
        integer, intent(inout) :: n_locals
        integer, intent(inout) :: capacity
        character(len=64), allocatable, intent(in) :: declared_vars(:)
        integer, intent(in) :: n_declared
        character(len=:), allocatable, intent(inout) :: decl_code
        character(len=64) :: var_name
        integer :: j

        if (.not. arena%has_node_at(stmt%target_index)) return

        select type (target => arena%entries(stmt%target_index)%node)
            type is (identifier_node)
            if (.not. allocated(target%name)) return
            if (target%inferred_type%kind == 0) return

            var_name = trim(target%name)

            if (is_parameter_name(var_name, param_map)) return

            do j = 1, n_declared
                if (trim(declared_vars(j)) == var_name) return
            end do

            if (.not. is_local_var_collected(var_name, local_vars, n_locals)) then
                call ensure_local_var_capacity(local_vars, capacity, n_locals + 1)
                n_locals = n_locals + 1
                local_vars(n_locals) = var_name
                decl_code = decl_code // "    " // &
                    mono_type_to_string(target%inferred_type, &
                    include_shape=.true., &
                    fallback='integer') // &
                    " :: " // trim(var_name) // new_line('A')
            end if
        end select
    end subroutine collect_vars_from_assignment_sub

    subroutine extract_use_statements(arena, original_indices, remaining_indices, &
            use_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: original_indices(:)
        integer, allocatable, intent(out) :: remaining_indices(:)
        character(len=:), allocatable, intent(out) :: use_code
        integer :: i, count
        logical :: is_use_stmt
        character(len=:), allocatable :: stmt_text

        if (size(original_indices) == 0) then
            allocate (remaining_indices(0))
            use_code = ""
            return
        end if

        allocate (remaining_indices(size(original_indices)))
        count = 0
        use_code = ""

        do i = 1, size(original_indices)
            is_use_stmt = .false.
            if (original_indices(i) > 0 .and. original_indices(i) <= arena%size) then
                if (allocated(arena%entries(original_indices(i))%node)) then
                    select type (node => arena%entries(original_indices(i))%node)
                        type is (use_statement_node)
                        is_use_stmt = .true.
                        stmt_text = generate_code_from_arena(arena, original_indices(i))
                        if (len_trim(stmt_text) > 0) then
                            use_code = use_code // "    " // trim(stmt_text) // &
                                new_line('A')
                        end if
                    end select
                end if
            end if

            if (.not. is_use_stmt) then
                count = count + 1
                remaining_indices(count) = original_indices(i)
            end if
        end do

        if (count == 0) then
            deallocate (remaining_indices)
            allocate (remaining_indices(0))
        else if (count < size(original_indices)) then
            block
                integer, allocatable :: trimmed(:)
                allocate (trimmed(count))
                trimmed = remaining_indices(1:count)
                call move_alloc(trimmed, remaining_indices)
            end block
        end if
    end subroutine extract_use_statements

end module codegen_subroutine_declarations
