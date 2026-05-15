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

    subroutine build_parameter_list(parameter_names, params)
        character(len=:), allocatable, intent(in), optional :: parameter_names(:)
        character(len=:), allocatable, intent(out) :: params
        integer :: i

        params = ""
        if (.not. present(parameter_names)) return
        if (.not. allocated(parameter_names)) return

        do i = 1, size(parameter_names)
            if (i == 1) then
                params = parameter_names(i)
            else
                params = params // ", " // parameter_names(i)
            end if
        end do
    end subroutine build_parameter_list

    subroutine append_node_code_lines(arena, indices, code, add_blank_lines)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in), optional :: indices(:)
        character(len=:), allocatable, intent(inout) :: code
        logical, intent(in) :: add_blank_lines
        character(len=:), allocatable :: node_code
        integer :: i

        if (.not. present(indices)) return
        if (.not. allocated(indices)) return

        do i = 1, size(indices)
            if (.not. arena%has_node_at(indices(i))) cycle
            node_code = generate_code_from_arena(arena, indices(i))
            if (len(node_code) == 0) cycle
            code = code // "    " // node_code // new_line('A')
            if (add_blank_lines) then
                if (i < size(indices)) code = code // new_line('A')
            end if
        end do
    end subroutine append_node_code_lines

    subroutine generate_code_named_block(arena, keyword, name, parameter_names, &
                                         declaration_indices, procedure_indices, &
                                         has_contains, code)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: keyword
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(in), optional :: parameter_names(:)
        integer, allocatable, intent(in), optional :: declaration_indices(:)
        integer, allocatable, intent(in), optional :: procedure_indices(:)
        logical, intent(in) :: has_contains
        character(len=:), allocatable, intent(out) :: code
        character(len=:), allocatable :: header
        character(len=:), allocatable :: params

        code = ""

        call build_parameter_list(parameter_names, params)
        header = keyword // " " // name
        if (len(params) > 0) header = header // "(" // params // ")"
        header = header // new_line('A')
        code = header

        call append_node_code_lines(arena, declaration_indices, code, &
                                    add_blank_lines=.false.)

        if (has_contains) then
            code = code // "contains" // new_line('A')
            call append_node_code_lines(arena, procedure_indices, code, &
                                        add_blank_lines=.true.)
        end if

        code = code // "end " // keyword // " " // name
    end subroutine generate_code_named_block

    function generate_code_template_block(arena, node) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(template_block_node), intent(in) :: node
        character(len=:), allocatable :: code
        call generate_code_named_block(arena, "template", node%name, &
                                       parameter_names=node%parameter_names, &
                                       declaration_indices=node%declaration_indices, &
                                       procedure_indices=node%procedure_indices, &
                                       has_contains=node%has_contains, &
                                       code=code)
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

    function generate_code_trait_block(arena, node) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(trait_block_node), intent(in) :: node
        character(len=:), allocatable :: code
        call generate_code_named_block(arena, "trait", node%name, &
                                       parameter_names=node%parameter_names, &
                                       declaration_indices=node%declaration_indices, &
                                       procedure_indices=node%procedure_indices, &
                                       has_contains=node%has_contains, &
                                       code=code)
    end function generate_code_trait_block

    function generate_code_requirement_block(arena, node) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(requirement_block_node), intent(in) :: node
        character(len=:), allocatable :: code
        call generate_code_named_block(arena, "requirement", node%name, &
                                       parameter_names=node%parameter_names, &
                                       declaration_indices=node%declaration_indices, &
                                       procedure_indices=node%procedure_indices, &
                                       has_contains=node%has_contains, &
                                       code=code)
    end function generate_code_requirement_block

    function generate_code_implements_block(arena, node) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(implements_block_node), intent(in) :: node
        character(len=:), allocatable :: code
        call generate_code_named_block(arena, "implements", node%name, &
                                       parameter_names=node%parameter_names, &
                                       declaration_indices=node%declaration_indices, &
                                       procedure_indices=node%procedure_indices, &
                                       has_contains=node%has_contains, &
                                       code=code)
    end function generate_code_implements_block

end module codegen_generics
