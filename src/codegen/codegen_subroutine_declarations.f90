module codegen_subroutine_declarations
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use ast_nodes_io, only: print_statement_node, read_statement_node
    use ast_nodes_procedure, only: subroutine_def_node
    use codegen_declarations_inference, only: build_parameter_map
    use codegen_procedure_shared, only: build_parameter_clause, gather_prefix, &
                                        copy_indices, apply_default_intents, &
                                        maybe_add_procedure_implicit_none, &
                                        filter_implicit_statements, &
                                        append_parameter_declaration, &
                                        is_parameter_name, ensure_local_var_capacity, &
                                        is_local_var_collected, add_declared_vars, &
                                        add_single_declared_var
    use codegen_utilities, only: parameter_info_t, generate_grouped_body_with_params, &
                                 reorder_import_lines
    implicit none
    private
    public :: generate_code_subroutine_def

contains

    function generate_code_subroutine_def(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = compose_subroutine_signature(arena, node)
        code = code // new_line('A')
        code = code // build_subroutine_body_section(arena, node)
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

    function build_subroutine_body_section(arena, node) result(body)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: node
        character(len=:), allocatable :: body
        type(parameter_info_t), allocatable :: param_map(:)
        integer, allocatable :: param_indices(:)
        integer, allocatable :: body_indices(:)
        integer, allocatable :: filtered_body_indices(:)

        call copy_indices(node%param_indices, param_indices)
        call copy_indices(node%body_indices, body_indices)

        call build_parameter_map(arena, param_indices, body_indices, param_map)
        if (allocated(node%prefix_keywords)) then
            call apply_default_intents(node%prefix_keywords, param_map)
        end if

        body = maybe_add_procedure_implicit_none(arena, body_indices)
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
            if (param_idx <= 0 .or. param_idx > arena%size) cycle
            if (.not. allocated(arena%entries(param_idx)%node)) cycle

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
            if (body_idx <= 0 .or. body_idx > arena%size) cycle
            if (.not. allocated(arena%entries(body_idx)%node)) cycle
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
            if (stmt_idx <= 0 .or. stmt_idx > arena%size) cycle
            if (.not. allocated(arena%entries(stmt_idx)%node)) cycle

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
            type is (print_statement_node)
                call collect_vars_from_print_sub(arena, stmt, param_map, local_vars, &
                                                 n_locals, capacity, declared_vars, &
                                                 n_declared, decl_code)
            type is (read_statement_node)
                call collect_vars_from_read_sub(arena, stmt, param_map, local_vars, &
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
            if (expr_idx <= 0 .or. expr_idx > arena%size) cycle
            if (.not. allocated(arena%entries(expr_idx)%node)) cycle

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
            if (var_idx <= 0 .or. var_idx > arena%size) cycle
            if (.not. allocated(arena%entries(var_idx)%node)) cycle

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

end module codegen_subroutine_declarations

