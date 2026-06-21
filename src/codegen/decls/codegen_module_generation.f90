module codegen_module_generation
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: literal_node
    use ast_nodes_data, only: module_node, block_data_node, submodule_node
    use ast_nodes_misc, only: implicit_statement_node, interface_block_node, &
                              module_procedure_node, use_statement_node, &
                              import_statement_node, include_statement_node, &
                              comment_node, directive_node, blank_line_node
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_grouped_body, only: generate_grouped_body
    use codegen_program_body, only: maybe_require_dp_kind_use
    use string_utils_mod, only: to_lower
    implicit none
    private
    public :: generate_code_module
    public :: generate_code_submodule
    public :: generate_code_block_data
    public :: generate_code_interface_block
    public :: generate_code_module_procedure

    logical, save :: in_operator_or_assignment_interface = .false.
    logical, save :: in_module_context = .false.

contains

    function generate_code_module(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: header
        character(len=:), allocatable :: use_block
        character(len=:), allocatable :: remainder_block
        integer :: prefix_count
        logical :: needs_implicit
        logical :: has_implicit

        in_module_context = .true.
        header = build_module_header(arena, node)
        prefix_count = count_leading_use_entries(arena, node)
        use_block = collect_module_declarations(arena, node, 1, prefix_count)
        remainder_block = collect_module_declarations(arena, node, &
                                                      prefix_count + 1)
        has_implicit = module_has_implicit_statement(arena, node)
        needs_implicit = .not. has_implicit

        code = header
        if (len(use_block) > 0) code = code//use_block
        if (needs_implicit) then
            code = code//"    implicit none"//new_line('A')
        end if
        if (len(remainder_block) > 0) code = code//remainder_block
        code = code//build_contains_section(arena, node)
        code = code//"end module "//node%name
        call maybe_require_dp_kind_use(code)
        in_module_context = .false.
    end function generate_code_module

    function generate_code_submodule(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(submodule_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: header
        character(len=:), allocatable :: use_block
        character(len=:), allocatable :: remainder_block
        integer :: prefix_count
        logical :: needs_implicit
        logical :: has_implicit

        in_module_context = .true.
        header = build_submodule_header(arena, node)
        prefix_count = count_leading_use_entries_submodule(arena, node)
        use_block = collect_submodule_declarations(arena, node, 1, prefix_count)
        remainder_block = collect_submodule_declarations(arena, node, &
                                                         prefix_count + 1)
        has_implicit = submodule_has_implicit_statement(arena, node)
        needs_implicit = .not. has_implicit

        code = header
        if (len(use_block) > 0) code = code//use_block
        if (needs_implicit) then
            code = code//"    implicit none"//new_line('A')
        end if
        if (len(remainder_block) > 0) code = code//remainder_block
        code = code//build_contains_section_submodule(arena, node)
        code = code//"end submodule "//node%name
        call maybe_require_dp_kind_use(code)
        in_module_context = .false.
    end function generate_code_submodule

    function build_submodule_header(arena, node) result(header)
        type(ast_arena_t), intent(in) :: arena
        type(submodule_node), intent(in) :: node
        character(len=:), allocatable :: header

        header = "submodule ("//node%parent_identifier//") "//node%name// &
                 new_line('A')
    end function build_submodule_header

    logical function submodule_has_implicit_statement(arena, node) &
        result(has_implicit)
        type(ast_arena_t), intent(in) :: arena
        type(submodule_node), intent(in) :: node
        integer :: i
        integer :: decl_index
        character(len=:), allocatable :: lowered_value

        has_implicit = .false.
        if (.not. allocated(node%declaration_indices)) return

        do i = 1, size(node%declaration_indices)
            decl_index = node%declaration_indices(i)
            if (.not. arena%has_node_at(decl_index)) cycle

            select type (decl => arena%entries(decl_index)%node)
            type is (implicit_statement_node)
                has_implicit = .true.
                return
            type is (literal_node)
                if (allocated(decl%value)) then
                    lowered_value = to_lower(adjustl(decl%value))
                    if (index(lowered_value, "implicit") == 1) then
                        has_implicit = .true.
                        return
                    end if
                end if
            end select
        end do
    end function submodule_has_implicit_statement

    function collect_submodule_declarations(arena, node, start_idx, end_idx) &
        result(body_code)
        type(ast_arena_t), intent(in) :: arena
        type(submodule_node), intent(in) :: node
        integer, intent(in), optional :: start_idx
        integer, intent(in), optional :: end_idx
        character(len=:), allocatable :: body_code
        integer :: first, last

        if (.not. allocated(node%declaration_indices)) then
            body_code = ""
            return
        end if
        if (size(node%declaration_indices) == 0) then
            body_code = ""
            return
        end if

        first = 1
        if (present(start_idx)) first = max(1, start_idx)
        last = size(node%declaration_indices)
        if (present(end_idx)) last = min(last, end_idx)
        if (first > last) then
            body_code = ""
            return
        end if

        body_code = generate_grouped_body(arena, &
                                          node%declaration_indices(first:last), &
                                          1)
    end function collect_submodule_declarations

    integer function count_leading_use_entries_submodule(arena, node) result(count)
        type(ast_arena_t), intent(in) :: arena
        type(submodule_node), intent(in) :: node
        integer :: i

        count = 0
        if (.not. allocated(node%declaration_indices)) return

        do i = 1, size(node%declaration_indices)
            if (is_use_prefix_entry(arena, node%declaration_indices(i))) then
                count = count + 1
            else
                exit
            end if
        end do
    end function count_leading_use_entries_submodule

    function build_contains_section_submodule(arena, node) result(section_code)
        type(ast_arena_t), intent(in) :: arena
        type(submodule_node), intent(in) :: node
        character(len=:), allocatable :: section_code
        character(len=:), allocatable :: procedure_code
        integer :: i
        logical :: has_entries
        logical :: has_more

        section_code = ""
        has_entries = .false.

        if (.not. node%has_contains) return
        if (.not. allocated(node%procedure_indices)) return

        section_code = "contains"//new_line('A')

        do i = 1, size(node%procedure_indices)
            procedure_code = collect_contained_procedure(arena, &
                                                         node%procedure_indices(i))
            if (len(procedure_code) == 0) cycle
            has_entries = .true.
            has_more = i < size(node%procedure_indices)
            section_code = section_code//format_contained_procedure( &
                           procedure_code, has_more)
        end do

        if (.not. has_entries) section_code = ""
    end function build_contains_section_submodule

    function generate_code_block_data(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(block_data_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: body_code
        character(len=:), allocatable :: header_line
        character(len=:), allocatable :: end_line
        integer :: i

        header_line = ""
        if (allocated(node%header_label)) then
            if (len_trim(node%header_label) > 0) then
                header_line = trim(node%header_label)//" "
            end if
        end if
        header_line = header_line//"block data"
        if (allocated(node%name)) then
            if (len_trim(node%name) > 0) header_line = header_line//" "// &
                                                       trim(node%name)
        end if

        code = header_line//new_line('A')

        if (allocated(node%statement_indices)) then
            do i = 1, size(node%statement_indices)
                body_code = generate_code_from_arena(arena, node%statement_indices(i))
                if (len(body_code) > 0) then
                    code = code//"    "//body_code//new_line('A')
                end if
            end do
        end if

        end_line = ""
        if (allocated(node%end_label)) then
            if (len_trim(node%end_label) > 0) then
                end_line = trim(node%end_label)//" "
            end if
        end if
        end_line = end_line//"end block data"
        if (allocated(node%name)) then
            if (len_trim(node%name) > 0) end_line = end_line//" "//trim(node%name)
        end if

        code = code//end_line
    end function generate_code_block_data

    function build_module_header(arena, node) result(header)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        character(len=:), allocatable :: header

        header = "module "//node%name//new_line('A')
    end function build_module_header

    logical function module_has_implicit_statement(arena, node) result(has_implicit)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        integer :: i
        integer :: decl_index
        character(len=:), allocatable :: lowered_value

        has_implicit = .false.
        if (.not. allocated(node%declaration_indices)) return

        do i = 1, size(node%declaration_indices)
            decl_index = node%declaration_indices(i)
            if (.not. arena%has_node_at(decl_index)) cycle

            select type (decl => arena%entries(decl_index)%node)
            type is (implicit_statement_node)
                has_implicit = .true.
                return
            type is (literal_node)
                if (allocated(decl%value)) then
                    lowered_value = to_lower(adjustl(decl%value))
                    if (index(lowered_value, "implicit") == 1) then
                        has_implicit = .true.
                        return
                    end if
                end if
            end select
        end do
    end function module_has_implicit_statement

    function collect_module_declarations(arena, node, start_idx, end_idx) &
        result(body_code)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        integer, intent(in), optional :: start_idx
        integer, intent(in), optional :: end_idx
        character(len=:), allocatable :: body_code
        integer :: first, last

        if (.not. allocated(node%declaration_indices)) then
            body_code = ""
            return
        end if
        if (size(node%declaration_indices) == 0) then
            body_code = ""
            return
        end if

        first = 1
        if (present(start_idx)) first = max(1, start_idx)
        last = size(node%declaration_indices)
        if (present(end_idx)) last = min(last, end_idx)
        if (first > last) then
            body_code = ""
            return
        end if

        body_code = generate_grouped_body(arena, &
                                          node%declaration_indices(first:last), &
                                          1)
    end function collect_module_declarations

    integer function count_leading_use_entries(arena, node) result(count)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        integer :: i

        count = 0
        if (.not. allocated(node%declaration_indices)) return

        do i = 1, size(node%declaration_indices)
            if (is_use_prefix_entry(arena, node%declaration_indices(i))) then
                count = count + 1
            else
                exit
            end if
        end do
    end function count_leading_use_entries

    logical function is_use_prefix_entry(arena, decl_index) result(is_prefix)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index

        is_prefix = .false.
        if (decl_index <= 0) return
        if (decl_index > arena%size) return
        if (.not. allocated(arena%entries(decl_index)%node)) return

        select type (decl => arena%entries(decl_index)%node)
        type is (use_statement_node)
            is_prefix = .true.
        type is (import_statement_node)
            is_prefix = .true.
        type is (include_statement_node)
            is_prefix = .true.
        type is (comment_node)
            is_prefix = .true.
        type is (directive_node)
            is_prefix = .true.
        type is (blank_line_node)
            is_prefix = .true.
        class default
            is_prefix = .false.
        end select
    end function is_use_prefix_entry

    function build_contains_section(arena, node) result(section_code)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        character(len=:), allocatable :: section_code
        character(len=:), allocatable :: procedure_code
        integer :: i
        logical :: has_entries
        logical :: has_more

        section_code = ""
        has_entries = .false.

        if (.not. node%has_contains) return
        if (.not. allocated(node%procedure_indices)) return

        section_code = "contains"//new_line('A')

        do i = 1, size(node%procedure_indices)
            procedure_code = collect_contained_procedure(arena, &
                                                         node%procedure_indices(i))
            if (len(procedure_code) == 0) cycle
            has_entries = .true.
            has_more = i < size(node%procedure_indices)
            section_code = section_code//format_contained_procedure( &
                           procedure_code, has_more)
        end do

        if (.not. has_entries) section_code = ""
    end function build_contains_section

    function collect_contained_procedure(arena, procedure_index) result(proc_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: procedure_index
        character(len=:), allocatable :: proc_code

        proc_code = ""
        if (.not. arena%has_node_at(procedure_index)) return

        proc_code = generate_code_from_arena(arena, procedure_index)
    end function collect_contained_procedure

    function format_contained_procedure(proc_code, has_more) result(formatted)
        character(len=*), intent(in) :: proc_code
        logical, intent(in) :: has_more
        character(len=:), allocatable :: formatted

        formatted = "    "//proc_code
        if (has_more) then
            formatted = formatted//new_line('A')//new_line('A')
        else
            formatted = formatted//new_line('A')
        end if
    end function format_contained_procedure

    function generate_code_interface_block(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(interface_block_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: body_code
        logical :: is_op_or_assign

        if (node%is_abstract) then
            code = "abstract interface"
        else
            code = "interface"
        end if
        is_op_or_assign = .false.
        if (allocated(node%kind)) then
            if (trim(node%kind) == "operator" .or. trim(node%kind) == &
                "assignment") then
                is_op_or_assign = .true.
                code = code//" "//trim(node%kind)
                if (allocated(node%operator)) then
                    code = code//"("//trim(node%operator)//")"
                end if
            else if (trim(node%kind) == "read" .or. trim(node%kind) == "write") then
                code = code//" "//trim(node%kind)
                if (allocated(node%operator)) then
                    code = code//"("//trim(node%operator)//")"
                end if
            else if (allocated(node%name)) then
                if (len_trim(node%name) > 0) code = code//" "//trim(node%name)
            end if
        else if (allocated(node%name)) then
            if (len_trim(node%name) > 0) code = code//" "//trim(node%name)
        end if
        code = code//new_line('A')

        if (allocated(node%procedure_indices)) then
            in_operator_or_assignment_interface = is_op_or_assign
            body_code = generate_grouped_body(arena, node%procedure_indices, 1)
            in_operator_or_assignment_interface = .false.
            if (len(body_code) > 0) code = code//body_code
        end if

        code = code//"end interface"
        if (allocated(node%kind)) then
            if (trim(node%kind) == "operator" .or. trim(node%kind) == &
                "assignment") then
                code = code//" "//trim(node%kind)
                if (allocated(node%operator)) then
                    code = code//"("//trim(node%operator)//")"
                end if
            else if (trim(node%kind) == "read" .or. trim(node%kind) == "write") then
                code = code//" "//trim(node%kind)
                if (allocated(node%operator)) then
                    code = code//"("//trim(node%operator)//")"
                end if
            else if (allocated(node%name)) then
                if (len_trim(node%name) > 0) code = code//" "//trim(node%name)
            end if
        else if (allocated(node%name)) then
            if (len_trim(node%name) > 0) code = code//" "//trim(node%name)
        end if
    end function generate_code_interface_block

    function generate_code_module_procedure(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(module_procedure_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        integer :: i
        character(len=:), allocatable :: name_text
        logical :: first_name
        logical :: allow_module_prefix

        allow_module_prefix = node%has_module_prefix .and. &
                              (in_module_context .or. &
                               in_operator_or_assignment_interface)

        if (allow_module_prefix) then
            code = "module procedure"
        else
            code = "procedure"
        end if
        if (node%has_double_colon) then
            code = code//" ::"
        end if

        first_name = .true.
        if (allocated(node%procedure_names)) then
            do i = 1, size(node%procedure_names)
                if (.not. allocated(node%procedure_names(i)%s)) cycle
                name_text = trim(node%procedure_names(i)%s)
                if (len_trim(name_text) == 0) cycle
                if (first_name) then
                    code = code//" "//name_text
                    first_name = .false.
                else
                    code = code//", "//name_text
                end if
            end do
        end if
    end function generate_code_module_procedure

end module codegen_module_generation
