module codegen_grouped_body
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: ast_node
    use ast_nodes_control, only: cycle_node, exit_node, goto_node, return_node, &
                                 continue_node, stop_node, &
                                 error_stop_node
    use ast_nodes_misc, only: blank_line_node, comment_node, directive_node, &
                              contains_node, end_statement_node, &
                              data_statement_node, use_statement_node, &
                              implicit_statement_node, intrinsic_statement_node, &
                              namelist_statement_node, import_statement_node, &
                              include_statement_node
    use ast_nodes_transfer, only: entry_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
                              intent_type_to_string, &
                              INTENT_NONE
    use ast_nodes_io, only: read_statement_node, write_statement_node, &
                            print_statement_node, &
                            format_statement_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_character_normalization, only: normalize_character_type, &
                                               normalize_character_type_param
    use codegen_declaration_grouping, only: can_group_declarations, &
                                            can_group_parameters, &
                                            generate_grouped_declaration, &
                                            is_type_definition_declaration
    use codegen_indent, only: indent_lines
    use string_utils_mod, only: int_to_string
    use type_string_utils, only: is_character_type_string
    implicit none
    private

    public :: generate_grouped_body
    public :: generate_grouped_body_context

contains

    recursive subroutine process_single_statement(arena, idx, indent, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        integer, intent(in) :: indent
        character(len=:), allocatable, intent(inout) :: code
        character(len=:), allocatable :: stmt_code

        stmt_code = generate_code_from_arena(arena, idx)
        if (.not. allocated(stmt_code)) return
        if (len(stmt_code) == 0) return
        code = code // indent_lines(stmt_code, indent) // new_line('A')
    end subroutine process_single_statement

    subroutine process_procedure_def(arena, idx, indent, indent_str, in_contains, &
                                     i, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        integer, intent(in) :: indent
        character(len=*), intent(in) :: indent_str
        logical, intent(in) :: in_contains
        integer, intent(in) :: i
        character(len=:), allocatable, intent(inout) :: code
        character(len=:), allocatable :: stmt_code

        if (in_contains .and. i > 1) code = code // new_line('A')
        stmt_code = generate_code_from_arena(arena, idx)
        if (in_contains) then
            code = code // indent_lines(stmt_code, indent) // new_line('A')
        else
            code = code // indent_str // stmt_code // new_line('A')
        end if
    end subroutine process_procedure_def

    pure logical function is_valid_body_entry(arena, idx) result(valid)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx

        if (idx <= 0) then
            valid = .false.
            return
        end if
        if (idx > arena%size) then
            valid = .false.
            return
        end if
        if (.not. allocated(arena%entries(idx)%node)) then
            valid = .false.
            return
        end if

        valid = .true.
    end function is_valid_body_entry

    subroutine handle_contains_entry(in_contains_section, code, i)
        logical, intent(inout) :: in_contains_section
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(inout) :: i

        in_contains_section = .true.
        code = code // "contains" // new_line('A')
        i = i + 1
    end subroutine handle_contains_entry

    subroutine handle_end_statement(i)
        integer, intent(inout) :: i

        i = i + 1
    end subroutine handle_end_statement

    subroutine handle_procedure_entry(arena, body_indices, i, indent, indent_str, &
                                      in_contains_section, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(inout) :: i
        integer, intent(in) :: indent
        character(len=*), intent(in) :: indent_str
        logical, intent(in) :: in_contains_section
        character(len=:), allocatable, intent(inout) :: code

        call process_procedure_def(arena, body_indices(i), indent, indent_str, &
                                   in_contains_section, i, code)
        i = i + 1
    end subroutine handle_procedure_entry

    subroutine handle_declaration_entry(node, arena, body_indices, i, indent, &
                                        indent_str, in_contains_section, code)
        type(declaration_node), intent(in) :: node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(inout) :: i
        integer, intent(in) :: indent
        character(len=*), intent(in) :: indent_str
        logical, intent(in) :: in_contains_section
        character(len=:), allocatable, intent(inout) :: code

        if (is_type_definition_declaration(node)) then
            i = i + 1
            return
        end if
        ! Removed has_intent skip to prevent dropping declarations (fixes #2140)
        if (.not. in_contains_section .and. node%initializer_index == 0) then
            call process_grouped_declarations(arena, body_indices, i, indent_str, &
                                              code)
        else
            call process_single_statement(arena, body_indices(i), indent, code)
            i = i + 1
        end if
    end subroutine handle_declaration_entry

    subroutine handle_comment_entry(arena, idx, code, i)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(inout) :: i
        character(len=:), allocatable :: stmt_code

        stmt_code = generate_code_from_arena(arena, idx)
        code = code // stmt_code // new_line('A')
        i = i + 1
    end subroutine handle_comment_entry

    subroutine handle_blank_line_entry(code, i)
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(inout) :: i

        code = code // new_line('A')
        i = i + 1
    end subroutine handle_blank_line_entry

    recursive subroutine handle_default_entry(arena, idx, indent, code, i)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        integer, intent(in) :: indent
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(inout) :: i

        call process_single_statement(arena, idx, indent, code)
        i = i + 1
    end subroutine handle_default_entry

    recursive subroutine process_body_entry(arena, body_indices, i, indent, &
                                            indent_str, in_contains_section, &
                                            code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(inout) :: i
        integer, intent(in) :: indent
        character(len=*), intent(in) :: indent_str
        logical, intent(inout) :: in_contains_section
        character(len=:), allocatable, intent(inout) :: code

        select type (node => arena%entries(body_indices(i))%node)
        type is (contains_node)
            call handle_contains_entry(in_contains_section, code, i)

        type is (end_statement_node)
            call handle_end_statement(i)

        type is (function_def_node)
            call handle_procedure_entry(arena, body_indices, i, indent, indent_str, &
                                        in_contains_section, code)

        type is (subroutine_def_node)
            call handle_procedure_entry(arena, body_indices, i, indent, indent_str, &
                                        in_contains_section, code)

        type is (declaration_node)
            call handle_declaration_entry(node, arena, body_indices, i, indent, &
                                          indent_str, in_contains_section, code)

        type is (parameter_declaration_node)
            call process_grouped_parameters(arena, body_indices, i, &
                                            indent_str, code)

        type is (data_statement_node)
            call process_single_statement(arena, body_indices(i), indent, code)
            i = i + 1

        type is (comment_node)
            call handle_comment_entry(arena, body_indices(i), code, i)
        type is (directive_node)
            call handle_comment_entry(arena, body_indices(i), code, i)

        type is (blank_line_node)
            call handle_blank_line_entry(code, i)

        class default
            if (.not. in_contains_section) then
                call handle_default_entry(arena, body_indices(i), indent, code, i)
            else
                i = i + 1
            end if
        end select
    end subroutine process_body_entry

    recursive function generate_grouped_body(arena, body_indices, indent) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: indent
        character(len=:), allocatable :: code
        character(len=:), allocatable :: indent_str
        integer :: i
        logical :: in_contains_section
        integer, allocatable :: ordered_indices(:)

        indent_str = repeat("    ", indent)
        code = ""
        in_contains_section = .false.
        i = 1
        ordered_indices = body_indices
        call reorder_use_before_implicit(arena, ordered_indices)
        call reorder_data_statements(arena, ordered_indices)

        do while (i <= size(ordered_indices))
            if (.not. is_valid_body_entry(arena, ordered_indices(i))) then
                i = i + 1
                cycle
            end if
            call process_body_entry(arena, ordered_indices, i, indent, indent_str, &
                                    in_contains_section, code)
        end do
    end function generate_grouped_body

    function generate_grouped_body_context(arena, body_indices, indent, &
                                           has_exec_before_contains) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: indent
        logical, intent(in) :: has_exec_before_contains
        character(len=:), allocatable :: code
        logical :: unused_flag

        unused_flag = has_exec_before_contains
        code = generate_grouped_body(arena, body_indices, indent)
    end function generate_grouped_body_context

    subroutine reorder_use_before_implicit(arena, body_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(inout) :: body_indices(:)
        integer :: first_implicit
        integer :: i

        if (.not. allocated(body_indices)) return
        if (size(body_indices) == 0) return

        first_implicit = find_first_implicit(arena, body_indices)
        if (first_implicit <= 0) return

        do i = first_implicit + 1, size(body_indices)
            if (.not. is_valid_body_entry(arena, body_indices(i))) cycle
            if (is_contains_entry(arena, body_indices(i))) exit
            if (.not. is_spec_statement(arena, body_indices(i))) exit
            if (is_use_like_statement(arena, body_indices(i))) then
                call move_index(body_indices, i, first_implicit)
                first_implicit = first_implicit + 1
            end if
        end do
    end subroutine reorder_use_before_implicit

    subroutine reorder_data_statements(arena, body_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(inout) :: body_indices(:)
        integer :: first_exec_pos
        integer :: i

        if (.not. allocated(body_indices)) return
        if (size(body_indices) == 0) return

        first_exec_pos = find_first_exec_position(arena, body_indices)
        if (first_exec_pos <= 0) return

        i = first_exec_pos
        do while (i <= size(body_indices))
            if (is_contains_entry(arena, body_indices(i))) exit
            if (is_data_statement(arena, body_indices(i))) then
                call move_index(body_indices, i, first_exec_pos)
                first_exec_pos = first_exec_pos + 1
                i = i + 1
            else
                i = i + 1
            end if
        end do
    end subroutine reorder_data_statements

    integer function find_first_exec_position(arena, body_indices) result(pos)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer :: i

        pos = 0
        do i = 1, size(body_indices)
            if (.not. is_valid_body_entry(arena, body_indices(i))) cycle
            if (is_contains_entry(arena, body_indices(i))) exit
            if (is_spec_statement(arena, body_indices(i))) cycle
            pos = i
            exit
        end do
    end function find_first_exec_position

    integer function find_first_implicit(arena, body_indices) result(pos)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer :: i

        pos = 0
        do i = 1, size(body_indices)
            if (.not. is_valid_body_entry(arena, body_indices(i))) cycle
            if (is_contains_entry(arena, body_indices(i))) exit
            if (.not. is_spec_statement(arena, body_indices(i))) exit
            if (is_implicit_statement(arena, body_indices(i))) then
                pos = i
                return
            end if
        end do
    end function find_first_implicit

    logical function is_spec_statement(arena, idx) result(is_spec)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx

        is_spec = .false.
        if (.not. arena%has_node_at(idx)) return

        select type (node => arena%entries(idx)%node)
        type is (declaration_node)
            is_spec = .true.
        type is (parameter_declaration_node)
            is_spec = .true.
        type is (use_statement_node)
            is_spec = .true.
        type is (implicit_statement_node)
            is_spec = .true.
        type is (intrinsic_statement_node)
            is_spec = .true.
        type is (namelist_statement_node)
            is_spec = .true.
        type is (import_statement_node)
            is_spec = .true.
        type is (include_statement_node)
            is_spec = .true.
        type is (data_statement_node)
            is_spec = .true.
        type is (format_statement_node)
            is_spec = .true.
        type is (comment_node)
            is_spec = .true.
        type is (directive_node)
            is_spec = .true.
        type is (blank_line_node)
            is_spec = .true.
        class default
            is_spec = .false.
        end select
    end function is_spec_statement

    logical function is_implicit_statement(arena, idx) result(is_implicit)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx

        is_implicit = .false.
        if (.not. arena%has_node_at(idx)) return

        select type (node => arena%entries(idx)%node)
        type is (implicit_statement_node)
            is_implicit = .true.
        class default
            is_implicit = .false.
        end select
    end function is_implicit_statement

    logical function is_use_like_statement(arena, idx) result(is_use_like)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx

        is_use_like = .false.
        if (.not. arena%has_node_at(idx)) return

        select type (node => arena%entries(idx)%node)
        type is (use_statement_node)
            is_use_like = .true.
        type is (import_statement_node)
            is_use_like = .true.
        type is (intrinsic_statement_node)
            is_use_like = .true.
        class default
            is_use_like = .false.
        end select
    end function is_use_like_statement

    logical function is_data_statement(arena, idx) result(is_data)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx

        is_data = .false.
        if (.not. arena%has_node_at(idx)) return

        select type (node => arena%entries(idx)%node)
        type is (data_statement_node)
            is_data = .true.
        class default
            is_data = .false.
        end select
    end function is_data_statement

    logical function is_contains_entry(arena, idx) result(is_contains)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx

        is_contains = .false.
        if (.not. arena%has_node_at(idx)) return

        select type (node => arena%entries(idx)%node)
        type is (contains_node)
            is_contains = .true.
        class default
            is_contains = .false.
        end select
    end function is_contains_entry

    subroutine move_index(indices, from_pos, to_pos)
        integer, allocatable, intent(inout) :: indices(:)
        integer, intent(in) :: from_pos
        integer, intent(in) :: to_pos
        integer :: value
        integer :: j

        if (from_pos <= to_pos) return
        if (from_pos > size(indices)) return
        if (to_pos < 1) return

        value = indices(from_pos)
        do j = from_pos, to_pos + 1, -1
            indices(j) = indices(j - 1)
        end do
        indices(to_pos) = value
    end subroutine move_index

    subroutine emit_declaration_statement(arena, idx, indent_str, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        character(len=*), intent(in) :: indent_str
        character(len=:), allocatable, intent(inout) :: code
        character(len=:), allocatable :: stmt_code

        stmt_code = generate_code_from_arena(arena, idx)
        code = code // indent_str // stmt_code // new_line('A')
    end subroutine emit_declaration_statement

    pure logical function is_groupable_declaration(node) result(can_group)
        type(declaration_node), intent(in) :: node

        if (node%disable_grouping) then
            can_group = .false.
            return
        end if
        if (node%is_multi_declaration) then
            can_group = .false.
            return
        end if
        if (node%is_array) then
            can_group = .false.
            return
        end if
        if (node%is_allocatable) then
            can_group = .false.
            return
        end if
        if (node%is_pointer) then
            can_group = .false.
            return
        end if
        if (node%is_target) then
            can_group = .false.
            return
        end if
        if (node%is_external) then
            can_group = .false.
            return
        end if
        if (node%is_parameter) then
            can_group = .false.
            return
        end if
        if (node%initializer_index > 0) then
            can_group = .false.
            return
        end if

        can_group = .true.
    end function is_groupable_declaration

    subroutine extend_declaration_group(arena, body_indices, start_pos, first_node, &
                                        grouped_names, group_count, next_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: start_pos
        type(declaration_node), intent(in) :: first_node
        character(len=64), allocatable, intent(inout) :: grouped_names(:)
        integer, intent(inout) :: group_count
        integer, intent(out) :: next_index
        integer :: j

        j = start_pos
        do while (j <= size(body_indices))
            if (body_indices(j) <= 0) exit
            if (body_indices(j) > arena%size) exit
            if (.not. allocated(arena%entries(body_indices(j))%node)) exit
            select type (next_node => arena%entries(body_indices(j))%node)
            type is (declaration_node)
                if (can_group_declarations(first_node, next_node)) then
                    group_count = group_count + 1
                    call append_name(grouped_names, group_count, &
                                     trim(next_node%var_name))
                    j = j + 1
                else
                    exit
                end if
            class default
                exit
            end select
        end do
        next_index = j
    end subroutine extend_declaration_group

    function build_grouped_statement(first_node, grouped_names, group_count) &
        result(stmt)
        type(declaration_node), intent(in) :: first_node
        character(len=64), allocatable, intent(in) :: grouped_names(:)
        integer, intent(in) :: group_count
        character(len=:), allocatable :: stmt
        character(len=:), allocatable :: intent_text
        character(len=:), allocatable :: var_list
        character(len=64), allocatable :: sorted_names(:)

        sorted_names = grouped_names
        call sort_names(sorted_names, group_count)
        var_list = build_var_list(sorted_names, group_count)

        if (first_node%has_intent) then
            intent_text = first_node%intent
        else
            intent_text = ""
        end if

        stmt = generate_grouped_declaration(first_node%type_name, &
                                            first_node%kind_value, &
                                            first_node%has_kind, &
                                            intent_text, &
                                            var_list, &
                                            first_node%is_optional, &
                                            first_node%is_target)
    end function build_grouped_statement

    subroutine extend_parameter_group(arena, body_indices, start_pos, var_list, &
                                      next_index, first_node)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: start_pos
        character(len=:), allocatable, intent(inout) :: var_list
        integer, intent(out) :: next_index
        type(parameter_declaration_node), intent(in) :: first_node
        integer :: j

        j = start_pos
        do while (j <= size(body_indices))
            if (body_indices(j) <= 0) exit
            if (body_indices(j) > arena%size) exit
            if (.not. allocated(arena%entries(body_indices(j))%node)) exit
            select type (next_node => arena%entries(body_indices(j))%node)
            type is (parameter_declaration_node)
                if (can_group_parameters(first_node, next_node)) then
                    var_list = var_list // ", " // trim(next_node%name)
                    j = j + 1
                else
                    exit
                end if
            class default
                exit
            end select
        end do
        next_index = j
    end subroutine extend_parameter_group

    function build_parameter_statement(first_node, var_list) result(stmt)
        type(parameter_declaration_node), intent(in) :: first_node
        character(len=*), intent(in) :: var_list
        character(len=:), allocatable :: stmt
        character(len=:), allocatable :: base_type

        if (allocated(first_node%type_name)) then
            base_type = first_node%type_name
        else
            base_type = "real"
        end if

        if (is_character_type_string(base_type)) then
            stmt = normalize_character_type_param(base_type, &
                                                  first_node%has_kind, &
                                                  first_node%kind_value)
        else
            stmt = base_type
            if (first_node%has_kind .and. first_node%kind_value > 0) then
                stmt = stmt // "(" // &
                       trim(adjustl(int_to_string(first_node%kind_value))) // ")"
            end if
        end if

        if (first_node%intent_type /= INTENT_NONE) then
            stmt = stmt // ", intent(" // &
                   intent_type_to_string(first_node%intent_type) // ")"
        end if
        if (first_node%is_optional) then
            stmt = stmt // ", optional"
        end if

        stmt = stmt // " :: " // var_list
    end function build_parameter_statement

    subroutine process_grouped_declarations(arena, body_indices, i, indent_str, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(inout) :: i
        character(len=*), intent(in) :: indent_str
        character(len=:), allocatable, intent(inout) :: code

        type(declaration_node) :: first_node
        character(len=:), allocatable :: stmt_code
        character(len=64), allocatable :: grouped_names(:)
        integer :: group_count
        integer :: next_index

        select type (node => arena%entries(body_indices(i))%node)
        type is (declaration_node)
            if (.not. is_groupable_declaration(node)) then
                call emit_declaration_statement(arena, body_indices(i), &
                                                indent_str, code)
                i = i + 1
                return
            end if

            first_node = node
            group_count = 1
            allocate (grouped_names(group_count))
            grouped_names(1) = trim(node%var_name)

            call extend_declaration_group(arena, body_indices, i + 1, first_node, &
                                          grouped_names, group_count, next_index)

            if (group_count == 1) then
                call emit_declaration_statement(arena, body_indices(i), &
                                                indent_str, code)
                i = next_index
                return
            end if

            stmt_code = build_grouped_statement(first_node, grouped_names, group_count)
            code = code // indent_str // stmt_code // new_line('A')
            i = next_index
        end select
    end subroutine process_grouped_declarations

    subroutine process_grouped_parameters(arena, body_indices, i, indent_str, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(inout) :: i
        character(len=*), intent(in) :: indent_str
        character(len=:), allocatable, intent(inout) :: code

        type(parameter_declaration_node) :: first_node
        character(len=:), allocatable :: var_list
        character(len=:), allocatable :: stmt_code
        integer :: next_index

        select type (node => arena%entries(body_indices(i))%node)
        type is (parameter_declaration_node)
            first_node = node
            var_list = trim(node%name)

            call extend_parameter_group(arena, body_indices, i + 1, var_list, &
                                        next_index, first_node)

            stmt_code = build_parameter_statement(first_node, var_list)
            code = code // indent_str // stmt_code // new_line('A')
            i = next_index
        end select
    end subroutine process_grouped_parameters

    subroutine append_name(names, count, new_name)
        character(len=64), allocatable, intent(inout) :: names(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: new_name
        character(len=64), allocatable :: tmp(:)

        if (.not. allocated(names)) then
            allocate (names(1))
            names(1) = new_name
        else
            allocate (tmp(count))
            tmp(1:count - 1) = names
            tmp(count) = new_name
            call move_alloc(tmp, names)
        end if
    end subroutine append_name

    subroutine sort_names(names, count)
        character(len=64), allocatable, intent(inout) :: names(:)
        integer, intent(in) :: count
        character(len=64) :: tmp
        integer :: k
        integer :: m

        if (count <= 1) return

        do k = 1, count - 1
            do m = k + 1, count
                if (names(m) < names(k)) then
                    tmp = names(k)
                    names(k) = names(m)
                    names(m) = tmp
                end if
            end do
        end do
    end subroutine sort_names

    function build_var_list(names, count) result(var_list)
        character(len=64), allocatable, intent(in) :: names(:)
        integer, intent(in) :: count
        character(len=:), allocatable :: var_list
        integer :: idx

        var_list = ""
        do idx = 1, count
            if (idx > 1) var_list = var_list // ", "
            var_list = var_list // trim(names(idx))
        end do
    end function build_var_list

end module codegen_grouped_body
