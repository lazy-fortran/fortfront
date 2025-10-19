module codegen_declarations_module_mod
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: module_node
    use ast_nodes_core, only: literal_node
    use ast_nodes_misc, only: implicit_statement_node
    use codegen_utilities, only: generate_grouped_body
    use codegen_arena_interface, only: generate_code_from_arena
    implicit none
    private
    public :: generate_code_module

contains

    function generate_code_module(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = build_module_header(arena, node)
        code = code // collect_module_declarations(arena, node)
        code = code // build_contains_section(arena, node)
        code = code // "end module " // node%name
    end function generate_code_module

    function build_module_header(arena, node) result(header)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        character(len=:), allocatable :: header

        header = "module " // node%name // new_line('A')
        if (.not. module_has_implicit_none(arena, node)) then
            header = header // "    implicit none" // new_line('A')
        end if
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

end module codegen_declarations_module_mod
