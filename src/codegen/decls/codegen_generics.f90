module codegen_generics
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_generics, only: template_block_node, instantiate_statement_node, &
                                  trait_block_node, requirement_block_node, &
                                  implements_block_node
    use codegen_arena_interface, only: generate_code_from_arena
    implicit none
    private

    public :: generate_code_template_block
    public :: generate_code_instantiate_statement
    public :: generate_code_trait_block
    public :: generate_code_requirement_block
    public :: generate_code_implements_block

contains

    function generate_code_template_block(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(template_block_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: header
        character(len=:), allocatable :: decl_code
        character(len=:), allocatable :: proc_code
        character(len=:), allocatable :: params
        integer :: i

        code = ""
        params = ""

        if (allocated(node%parameter_names)) then
            do i = 1, size(node%parameter_names)
                if (i == 1) then
                    params = node%parameter_names(i)
                else
                    params = params // ", " // node%parameter_names(i)
                end if
            end do
        end if

        header = "template " // node%name
        if (len(params) > 0) header = header // "(" // params // ")"
        header = header // new_line('A')
        code = header

        if (allocated(node%declaration_indices)) then
            do i = 1, size(node%declaration_indices)
                if (.not. arena%has_node_at(node%declaration_indices(i))) cycle
                decl_code = generate_code_from_arena(arena, node%declaration_indices(i))
                if (len(decl_code) == 0) cycle
                code = code // "    " // decl_code // new_line('A')
            end do
        end if

        if (node%has_contains) then
            code = code // "contains" // new_line('A')
            if (allocated(node%procedure_indices)) then
                do i = 1, size(node%procedure_indices)
                    if (.not. arena%has_node_at(node%procedure_indices(i))) cycle
                    proc_code = generate_code_from_arena(arena, &
                                                         node%procedure_indices(i))
                    if (len(proc_code) == 0) cycle
                    code = code // "    " // proc_code // new_line('A')
                    if (i < size(node%procedure_indices)) then
                        code = code // new_line('A')
                    end if
                end do
            end if
        end if

        code = code // "end template " // node%name
    end function generate_code_template_block

    function generate_code_instantiate_statement(node) result(code)
        type(instantiate_statement_node), intent(in) :: node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: spec

        spec = ""
        if (allocated(node%spec_text)) spec = node%spec_text
        code = "instantiate"
        if (len(spec) > 0) then
            code = code // " " // spec
        end if
    end function generate_code_instantiate_statement

    function generate_code_trait_block(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(trait_block_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: header
        character(len=:), allocatable :: decl_code
        character(len=:), allocatable :: proc_code
        character(len=:), allocatable :: params
        integer :: i

        code = ""
        params = ""

        if (allocated(node%parameter_names)) then
            do i = 1, size(node%parameter_names)
                if (i == 1) then
                    params = node%parameter_names(i)
                else
                    params = params // ", " // node%parameter_names(i)
                end if
            end do
        end if

        header = "trait " // node%name
        if (len(params) > 0) header = header // "(" // params // ")"
        header = header // new_line('A')
        code = header

        if (allocated(node%declaration_indices)) then
            do i = 1, size(node%declaration_indices)
                if (.not. arena%has_node_at(node%declaration_indices(i))) cycle
                decl_code = generate_code_from_arena(arena, node%declaration_indices(i))
                if (len(decl_code) == 0) cycle
                code = code // "    " // decl_code // new_line('A')
            end do
        end if

        if (node%has_contains) then
            code = code // "contains" // new_line('A')
            if (allocated(node%procedure_indices)) then
                do i = 1, size(node%procedure_indices)
                    if (.not. arena%has_node_at(node%procedure_indices(i))) cycle
                    proc_code = generate_code_from_arena(arena, &
                                                         node%procedure_indices(i))
                    if (len(proc_code) == 0) cycle
                    code = code // "    " // proc_code // new_line('A')
                    if (i < size(node%procedure_indices)) then
                        code = code // new_line('A')
                    end if
                end do
            end if
        end if

        code = code // "end trait " // node%name
    end function generate_code_trait_block

    function generate_code_requirement_block(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(requirement_block_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: header
        character(len=:), allocatable :: decl_code
        character(len=:), allocatable :: proc_code
        character(len=:), allocatable :: params
        integer :: i

        code = ""
        params = ""

        if (allocated(node%parameter_names)) then
            do i = 1, size(node%parameter_names)
                if (i == 1) then
                    params = node%parameter_names(i)
                else
                    params = params // ", " // node%parameter_names(i)
                end if
            end do
        end if

        header = "requirement " // node%name
        if (len(params) > 0) header = header // "(" // params // ")"
        header = header // new_line('A')
        code = header

        if (allocated(node%declaration_indices)) then
            do i = 1, size(node%declaration_indices)
                if (.not. arena%has_node_at(node%declaration_indices(i))) cycle
                decl_code = generate_code_from_arena(arena, node%declaration_indices(i))
                if (len(decl_code) == 0) cycle
                code = code // "    " // decl_code // new_line('A')
            end do
        end if

        if (node%has_contains) then
            code = code // "contains" // new_line('A')
            if (allocated(node%procedure_indices)) then
                do i = 1, size(node%procedure_indices)
                    if (.not. arena%has_node_at(node%procedure_indices(i))) cycle
                    proc_code = generate_code_from_arena(arena, &
                                                         node%procedure_indices(i))
                    if (len(proc_code) == 0) cycle
                    code = code // "    " // proc_code // new_line('A')
                    if (i < size(node%procedure_indices)) then
                        code = code // new_line('A')
                    end if
                end do
            end if
        end if

        code = code // "end requirement " // node%name
    end function generate_code_requirement_block

    function generate_code_implements_block(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(implements_block_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: header
        character(len=:), allocatable :: decl_code
        character(len=:), allocatable :: proc_code
        character(len=:), allocatable :: params
        integer :: i

        code = ""
        params = ""

        if (allocated(node%parameter_names)) then
            do i = 1, size(node%parameter_names)
                if (i == 1) then
                    params = node%parameter_names(i)
                else
                    params = params // ", " // node%parameter_names(i)
                end if
            end do
        end if

        header = "implements " // node%name
        if (len(params) > 0) header = header // "(" // params // ")"
        header = header // new_line('A')
        code = header

        if (allocated(node%declaration_indices)) then
            do i = 1, size(node%declaration_indices)
                if (.not. arena%has_node_at(node%declaration_indices(i))) cycle
                decl_code = generate_code_from_arena(arena, node%declaration_indices(i))
                if (len(decl_code) == 0) cycle
                code = code // "    " // decl_code // new_line('A')
            end do
        end if

        if (node%has_contains) then
            code = code // "contains" // new_line('A')
            if (allocated(node%procedure_indices)) then
                do i = 1, size(node%procedure_indices)
                    if (.not. arena%has_node_at(node%procedure_indices(i))) cycle
                    proc_code = generate_code_from_arena(arena, &
                                                         node%procedure_indices(i))
                    if (len(proc_code) == 0) cycle
                    code = code // "    " // proc_code // new_line('A')
                    if (i < size(node%procedure_indices)) then
                        code = code // new_line('A')
                    end if
                end do
            end if
        end if

        code = code // "end implements " // node%name
    end function generate_code_implements_block

end module codegen_generics
