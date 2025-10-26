module codegen_grouped_body
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: ast_node
    use ast_nodes_control, only: cycle_node, exit_node, goto_node, return_node, &
                                 continue_node, stop_node, &
                                 error_stop_node
    use ast_nodes_misc, only: blank_line_node, comment_node, contains_node, &
                              end_statement_node
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

    function generate_grouped_body(arena, body_indices, indent) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: indent
        character(len=:), allocatable :: code
        character(len=:), allocatable :: stmt_code
        character(len=:), allocatable :: indent_str
        integer :: i
        logical :: in_contains_section

        indent_str = repeat("    ", indent)
        code = ""
        in_contains_section = .false.
        i = 1

        do while (i <= size(body_indices))
            if (body_indices(i) <= 0 .or. body_indices(i) > arena%size) then
                i = i + 1
                cycle
            end if
            if (.not. allocated(arena%entries(body_indices(i))%node)) then
                i = i + 1
                cycle
            end if

            select type (node => arena%entries(body_indices(i))%node)
            type is (contains_node)
                in_contains_section = .true.
                code = code // "contains" // new_line('A')
                i = i + 1

            type is (end_statement_node)
                i = i + 1

            type is (function_def_node)
                if (in_contains_section .and. i > 1) then
                    code = code // new_line('A')
                end if
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_str // stmt_code // new_line('A')
                i = i + 1

            type is (subroutine_def_node)
                if (in_contains_section .and. i > 1) then
                    code = code // new_line('A')
                end if
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_str // stmt_code // new_line('A')
                i = i + 1

            type is (declaration_node)
                if (is_type_definition_declaration(node)) then
                    i = i + 1
                    cycle
                end if
                if (.not. in_contains_section .and. node%initializer_index == 0) then
                    call process_grouped_declarations(arena, body_indices, i, &
                                                      indent_str, code)
                else
                    stmt_code = generate_code_from_arena(arena, body_indices(i))
                    code = code // indent_lines(stmt_code, indent) // new_line('A')
                    i = i + 1
                end if

            type is (parameter_declaration_node)
                call process_grouped_parameters(arena, body_indices, i, &
                                                indent_str, code)

            type is (comment_node)
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // stmt_code // new_line('A')
                i = i + 1

            type is (blank_line_node)
                code = code // new_line('A')
                i = i + 1

            type is (write_statement_node)
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_lines(stmt_code, indent) // new_line('A')
                i = i + 1

            type is (print_statement_node)
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_lines(stmt_code, indent) // new_line('A')
                i = i + 1

            type is (read_statement_node)
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_lines(stmt_code, indent) // new_line('A')
                i = i + 1

            type is (format_statement_node)
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_lines(stmt_code, indent) // new_line('A')
                i = i + 1

            type is (goto_node)
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_lines(stmt_code, indent) // new_line('A')
                i = i + 1

            type is (return_node)
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_lines(stmt_code, indent) // new_line('A')
                i = i + 1

            type is (entry_node)
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_lines(stmt_code, indent) // new_line('A')
                i = i + 1

            type is (continue_node)
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_lines(stmt_code, indent) // new_line('A')
                i = i + 1

            type is (stop_node)
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_lines(stmt_code, indent) // new_line('A')
                i = i + 1

            type is (error_stop_node)
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_lines(stmt_code, indent) // new_line('A')
                i = i + 1

            type is (cycle_node)
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_lines(stmt_code, indent) // new_line('A')
                i = i + 1

            type is (exit_node)
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_lines(stmt_code, indent) // new_line('A')
                i = i + 1

            class default
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_lines(stmt_code, indent) // new_line('A')
                i = i + 1
            end select
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

    subroutine process_grouped_declarations(arena, body_indices, i, indent_str, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(inout) :: i
        character(len=*), intent(in) :: indent_str
        character(len=:), allocatable, intent(inout) :: code

        type(declaration_node) :: first_node
        character(len=:), allocatable :: var_list
        character(len=:), allocatable :: stmt_code
        character(len=64), allocatable :: grouped_names(:)
        integer :: group_count
        integer :: j
        integer :: k

        select type (node => arena%entries(body_indices(i))%node)
        type is (declaration_node)
            if (node%is_multi_declaration) then
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_str // stmt_code // new_line('A')
                i = i + 1
                return
            end if

            first_node = node
            group_count = 1
            allocate (grouped_names(group_count))
            grouped_names(1) = trim(node%var_name)

            if (node%is_array .or. node%is_allocatable .or. node%is_pointer .or. &
                node%is_target .or. node%is_external .or. node%is_parameter .or. &
                node%initializer_index > 0) then
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_str // stmt_code // new_line('A')
                i = i + 1
                return
            end if

            j = i + 1
            do while (j <= size(body_indices))
                if (body_indices(j) <= 0 .or. body_indices(j) > arena%size) exit
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

            if (group_count == 1) then
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_str // stmt_code // new_line('A')
                i = j
            else
                call sort_names(grouped_names, group_count)
                var_list = build_var_list(grouped_names, group_count)
                block
                    character(len=:), allocatable :: intent_text
                    if (first_node%has_intent) then
                        intent_text = first_node%intent
                    else
                        intent_text = ""
                    end if
                    stmt_code = generate_grouped_declaration(first_node%type_name, &
                                                             first_node%kind_value, &
                                                             first_node%has_kind, &
                                                             intent_text, &
                                                             var_list, &
                                                             first_node%is_optional, &
                                                             first_node%is_target)
                end block
                code = code // indent_str // stmt_code // new_line('A')
                i = j
            end if
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
        integer :: j

        select type (node => arena%entries(body_indices(i))%node)
        type is (parameter_declaration_node)
            first_node = node
            var_list = trim(node%name)

            j = i + 1
            do while (j <= size(body_indices))
                if (body_indices(j) <= 0 .or. body_indices(j) > arena%size) exit
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

            if (allocated(first_node%type_name)) then
                stmt_code = first_node%type_name
            else
                stmt_code = "real"
            end if
            if (is_character_type_string(stmt_code)) then
                stmt_code = normalize_character_type_param(stmt_code, &
                                                           first_node%has_kind, &
                                                           first_node%kind_value)
            else if (first_node%has_kind .and. first_node%kind_value > 0) then
                stmt_code = stmt_code // "(" // &
                            trim(adjustl(int_to_string(first_node%kind_value))) // ")"
            end if
            if (first_node%intent_type /= INTENT_NONE) then
                stmt_code = stmt_code // ", intent(" // &
                            intent_type_to_string(first_node%intent_type) // ")"
            end if
            if (first_node%is_optional) then
                stmt_code = stmt_code // ", optional"
            end if
            stmt_code = stmt_code // " :: " // var_list
            code = code // indent_str // stmt_code // new_line('A')
            i = j
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
