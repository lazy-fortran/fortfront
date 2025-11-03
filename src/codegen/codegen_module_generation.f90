module codegen_module_generation
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: literal_node
    use ast_nodes_data, only: module_node, block_data_node
    use ast_nodes_misc, only: implicit_statement_node, interface_block_node, &
                              module_procedure_node
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_indent, only: indent_lines
    use codegen_grouped_body, only: generate_grouped_body
    use ast_traversal_utils, only: get_ancestor_of_type
    use string_utils_mod, only: to_lower
    implicit none
    private
    public :: generate_code_module
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
        character(len=:), allocatable :: declarations

        in_module_context = .true.
        code = build_module_header(arena, node)
        declarations = collect_module_declarations(arena, node)
        if (.not. module_has_implicit_none(arena, node)) then
            declarations = inject_module_implicit_none(declarations)
        end if
        code = code // declarations
        code = code // build_contains_section(arena, node)
        code = code // "end module " // node%name
        in_module_context = .false.
    end function generate_code_module

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
                header_line = trim(node%header_label) // " "
            end if
        end if
        header_line = header_line // "block data"
        if (allocated(node%name)) then
            if (len_trim(node%name) > 0) header_line = header_line // " " // &
                                                       trim(node%name)
        end if

        code = header_line // new_line('A')

        if (allocated(node%statement_indices)) then
            do i = 1, size(node%statement_indices)
                body_code = generate_code_from_arena(arena, node%statement_indices(i))
                if (len(body_code) > 0) then
                    code = code // "    " // body_code // new_line('A')
                end if
            end do
        end if

        end_line = ""
        if (allocated(node%end_label)) then
            if (len_trim(node%end_label) > 0) then
                end_line = trim(node%end_label) // " "
            end if
        end if
        end_line = end_line // "end block data"
        if (allocated(node%name)) then
            if (len_trim(node%name) > 0) end_line = end_line // " " // trim(node%name)
        end if

        code = code // end_line
    end function generate_code_block_data

    function build_module_header(arena, node) result(header)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        character(len=:), allocatable :: header

        header = "module " // node%name // new_line('A')
    end function build_module_header

    logical function module_has_implicit_none(arena, node) result(has_implicit)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        integer :: i
        integer :: decl_index

        has_implicit = .false.
        if (.not. allocated(node%declaration_indices)) return

        do i = 1, size(node%declaration_indices)
            decl_index = node%declaration_indices(i)
            if (decl_index <= 0 .or. decl_index > arena%size) cycle
            if (.not. allocated(arena%entries(decl_index)%node)) cycle

            select type (decl => arena%entries(decl_index)%node)
            type is (implicit_statement_node)
                if (decl%is_none) then
                    has_implicit = .true.
                    return
                end if
            type is (literal_node)
                if (allocated(decl%value)) then
                    if (index(decl%value, "implicit none") > 0) then
                        has_implicit = .true.
                        return
                    end if
                end if
            end select
        end do
    end function module_has_implicit_none

    function collect_module_declarations(arena, node) result(body_code)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        character(len=:), allocatable :: body_code

        if (.not. allocated(node%declaration_indices)) then
            body_code = ""
            return
        end if

        body_code = generate_grouped_body(arena, node%declaration_indices, 1)
    end function collect_module_declarations

    function inject_module_implicit_none(body_code) result(result_code)
        character(len=:), allocatable, intent(in) :: body_code
        character(len=:), allocatable :: result_code
        integer :: header_end
        integer :: len_body
        character(len=1) :: nl

        nl = new_line('A')

        if (.not. allocated(body_code)) then
            result_code = "    implicit none" // nl
            return
        end if

        len_body = len(body_code)
        if (len_body == 0) then
            result_code = "    implicit none" // nl
            return
        end if

        header_end = find_module_header_end(body_code)

        if (header_end <= 0) then
            result_code = "    implicit none" // nl // body_code
        else if (header_end >= len_body) then
            if (body_code(len_body:len_body) == nl) then
                result_code = body_code // "    implicit none" // nl
            else
                result_code = body_code // nl // "    implicit none" // nl
            end if
        else
            result_code = body_code(1:header_end) // "    implicit none" // nl // &
                          body_code(header_end + 1:)
        end if
    end function inject_module_implicit_none

    integer function find_module_header_end(body_code) result(pos)
        character(len=*), intent(in) :: body_code
        integer :: len_body
        integer :: line_start
        integer :: next_break
        integer :: line_end
        integer :: next_start
        character(len=:), allocatable :: line_text
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: normalized
        character(len=1) :: nl
        logical :: is_header_line

        nl = new_line('A')
        len_body = len(body_code)
        pos = 0
        line_start = 1

        do
            if (line_start > len_body) exit
            next_break = index(body_code(line_start:), nl)
            if (next_break == 0) then
                line_end = len_body
                next_start = len_body + 1
            else
                line_end = line_start + next_break - 2
                next_start = line_end + 2
            end if

            if (line_end >= line_start) then
                line_text = body_code(line_start:line_end)
            else
                line_text = ""
            end if

            if (len_trim(line_text) > 0) then
                lowered = to_lower(trim(line_text))
            else
                lowered = ""
            end if
            normalized = adjustl(lowered)

            is_header_line = .false.
            if (len_trim(normalized) == 0) then
                is_header_line = .true.
            else if (normalized(1:1) == "!") then
                is_header_line = .true.
            else if (len(normalized) >= 3) then
                if (normalized(1:3) == "use") is_header_line = .true.
            end if
            if (.not. is_header_line) then
                if (len(normalized) >= 6) then
                    if (normalized(1:6) == "import") is_header_line = .true.
                end if
            end if
            if (.not. is_header_line) then
                if (len(normalized) >= 9) then
                    if (normalized(1:9) == "intrinsic") is_header_line = .true.
                end if
            end if

            if (.not. is_header_line) exit

            pos = next_start - 1
            if (next_break == 0) exit
            line_start = next_start
        end do
    end function find_module_header_end

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

        section_code = "contains" // new_line('A')

        do i = 1, size(node%procedure_indices)
            procedure_code = collect_contained_procedure(arena, &
                                                         node%procedure_indices(i))
            if (len(procedure_code) == 0) cycle
            has_entries = .true.
            has_more = i < size(node%procedure_indices)
            section_code = section_code // format_contained_procedure( &
                           procedure_code, has_more)
        end do

        if (.not. has_entries) section_code = ""
    end function build_contains_section

    function collect_contained_procedure(arena, procedure_index) result(proc_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: procedure_index
        character(len=:), allocatable :: proc_code

        proc_code = ""
        if (procedure_index <= 0 .or. procedure_index > arena%size) return
        if (.not. allocated(arena%entries(procedure_index)%node)) return

        proc_code = generate_code_from_arena(arena, procedure_index)
    end function collect_contained_procedure

    function format_contained_procedure(proc_code, has_more) result(formatted)
        character(len=*), intent(in) :: proc_code
        logical, intent(in) :: has_more
        character(len=:), allocatable :: formatted

        formatted = "    " // proc_code
        if (has_more) then
            formatted = formatted // new_line('A') // new_line('A')
        else
            formatted = formatted // new_line('A')
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
                code = code // " " // trim(node%kind)
                if (allocated(node%operator)) then
                    code = code // "(" // trim(node%operator) // ")"
                end if
            else if (allocated(node%name)) then
                if (len_trim(node%name) > 0) code = code // " " // trim(node%name)
            end if
        else if (allocated(node%name)) then
            if (len_trim(node%name) > 0) code = code // " " // trim(node%name)
        end if
        code = code // new_line('A')

        if (allocated(node%procedure_indices)) then
            in_operator_or_assignment_interface = is_op_or_assign
            body_code = generate_grouped_body(arena, node%procedure_indices, 1)
            in_operator_or_assignment_interface = .false.
            if (len(body_code) > 0) code = code // body_code
        end if

        code = code // "end interface"
        if (allocated(node%kind)) then
            if (trim(node%kind) == "operator" .or. trim(node%kind) == &
                "assignment") then
                code = code // " " // trim(node%kind)
                if (allocated(node%operator)) then
                    code = code // "(" // trim(node%operator) // ")"
                end if
            else if (allocated(node%name)) then
                if (len_trim(node%name) > 0) code = code // " " // trim(node%name)
            end if
        else if (allocated(node%name)) then
            if (len_trim(node%name) > 0) code = code // " " // trim(node%name)
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

        if (in_module_context .or. in_operator_or_assignment_interface) then
            code = "module procedure"
        else
            code = "procedure"
        end if

        first_name = .true.
        if (allocated(node%procedure_names)) then
            do i = 1, size(node%procedure_names)
                if (.not. allocated(node%procedure_names(i)%s)) cycle
                name_text = trim(node%procedure_names(i)%s)
                if (len_trim(name_text) == 0) cycle
                if (first_name) then
                    code = code // " " // name_text
                    first_name = .false.
                else
                    code = code // ", " // name_text
                end if
            end do
        end if
    end function generate_code_module_procedure

end module codegen_module_generation
