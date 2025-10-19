module codegen_declarations_type_mod
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: derived_type_node
    use codegen_arena_interface, only: generate_code_from_arena
    implicit none
    private
    public :: generate_code_derived_type

contains

    function generate_code_derived_type(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(derived_type_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = build_derived_type_header(node)
        code = code // collect_derived_components(arena, node)
        code = code // "end type " // node%name
    end function generate_code_derived_type

    function build_derived_type_header(node) result(header)
        type(derived_type_node), intent(in) :: node
        character(len=:), allocatable :: header
        character(len=:), allocatable :: clause

        clause = derived_type_attribute_clause(node)
        if (len_trim(clause) == 0) then
            header = "type :: " // node%name // new_line('A')
        else if (clause(1:1) == ",") then
            header = "type" // clause // " :: " // node%name // new_line('A')
        else
            header = "type " // trim(clause) // " :: " // node%name // new_line('A')
        end if
    end function build_derived_type_header

    function derived_type_attribute_clause(node) result(clause)
        type(derived_type_node), intent(in) :: node
        character(len=:), allocatable :: clause
        integer :: i
        integer :: trimmed_length

        clause = ""
        if (.not. node%has_attributes) return
        if (.not. allocated(node%attribute_clause)) return

        trimmed_length = len_trim(node%attribute_clause)
        if (trimmed_length == 0) return

        clause = ""
        do i = 1, trimmed_length
            clause = clause // node%attribute_clause(i:i)
            if (node%attribute_clause(i:i) == "," .and. i < trimmed_length) then
                if (node%attribute_clause(i + 1:i + 1) /= " " .and. &
                    node%attribute_clause(i + 1:i + 1) /= new_line('A')) then
                    clause = clause // " "
                end if
            end if
        end do
    end function derived_type_attribute_clause

    function collect_derived_components(arena, node) result(component_block)
        type(ast_arena_t), intent(in) :: arena
        type(derived_type_node), intent(in) :: node
        character(len=:), allocatable :: component_block
        character(len=:), allocatable :: component_code
        integer :: i
        integer :: component_index

        component_block = ""
        if (.not. allocated(node%component_indices)) return

        do i = 1, size(node%component_indices)
            component_index = node%component_indices(i)
            if (component_index <= 0 .or. component_index > arena%size) cycle
            if (.not. allocated(arena%entries(component_index)%node)) cycle

            select type (child => arena%entries(component_index)%node)
            type is (derived_type_node)
                cycle
            class default
                component_code = generate_code_from_arena(arena, component_index)
            end select

            if (len_trim(component_code) == 0) cycle
            component_block = component_block // "    " // component_code // &
                              new_line('A')
        end do
    end function collect_derived_components

end module codegen_declarations_type_mod
