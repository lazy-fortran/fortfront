module standardizer_declarations_parsing
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_data, only: declaration_node
    use lexer_core, only: to_lower
    use standardizer_declarations_array, only: parse_dimension_attribute, &
                                               set_array_properties_from_type
    implicit none
    private

    public :: apply_type_string_to_decl
    public :: update_existing_declaration_type

contains

    subroutine apply_type_string_to_decl(arena, prog_index, var_name, &
                                         var_type, decl_node)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        character(len=*), intent(in) :: var_name, var_type
        type(declaration_node), intent(inout) :: decl_node
        integer :: dim_pos
        logical :: has_dimension_attr
        character(len=:), allocatable :: lowered_type
        character(len=:), allocatable :: base_part
        character(len=:), allocatable :: attr_part
        character(len=:), allocatable :: filtered_attr
        character(len=:), allocatable :: attr_trim
        character(len=:), allocatable :: component
        character(len=:), allocatable :: lowered_component
        integer :: comma_pos, paren_pos, close_pos
        integer :: kind_val, ios
        integer :: comp_start, comp_end, local_comma

        has_dimension_attr = .false.
        lowered_type = to_lower(var_type)

        comma_pos = index(var_type, ',')
        if (comma_pos > 0) then
            base_part = trim(var_type(1:comma_pos - 1))
            attr_part = trim(var_type(comma_pos + 1:))
        else
            base_part = trim(var_type)
            attr_part = ''
        end if

        decl_node%has_kind = .false.
        decl_node%kind_value = 0

        paren_pos = index(base_part, '(')
        if (paren_pos > 0) then
            close_pos = index(base_part(paren_pos:), ')')
            if (close_pos > 0) then
                close_pos = paren_pos + close_pos - 1
                read (base_part(paren_pos + 1:close_pos - 1), *, iostat=ios) &
                    kind_val
                if (ios == 0) then
                    if (index(to_lower(base_part(1:paren_pos - 1)), 'character') &
                        == 0) then
                        decl_node%has_kind = .true.
                        decl_node%kind_value = kind_val
                        base_part = trim(base_part(1:paren_pos - 1))
                    end if
                end if
            end if
        end if

        decl_node%type_name = trim(base_part)
        filtered_attr = ""
        if (len_trim(attr_part) > 0) then
            attr_trim = trim(attr_part)
            if (len_trim(attr_trim) > 0) then
                lowered_component = to_lower(attr_trim)
                do
                    dim_pos = index(lowered_component, 'dimension(')
                    if (dim_pos == 0) exit
                    comp_start = dim_pos
                    comp_end = comp_start + len('dimension(')
                    local_comma = 1
                    do while (comp_end <= len(lowered_component) .and. &
                              local_comma > 0)
                        select case (lowered_component(comp_end:comp_end))
                        case ('(')
                            local_comma = local_comma + 1
                        case (')')
                            local_comma = local_comma - 1
                        end select
                        comp_end = comp_end + 1
                    end do
                    comp_end = comp_end - 1
                    if (comp_end < comp_start) exit
                    attr_trim = attr_trim(:comp_start - 1) // &
                                attr_trim(comp_end + 1:)
                    lowered_component = to_lower(attr_trim)
                end do

                attr_trim = trim(attr_trim)
                if (len_trim(attr_trim) > 0) then
                    comp_start = 1
                    do
                        if (comp_start > len_trim(attr_trim)) exit
                        local_comma = index(attr_trim(comp_start:), ',')
                        if (local_comma > 0) then
                            comp_end = comp_start + local_comma - 2
                        else
                            comp_end = len_trim(attr_trim)
                        end if
                        if (comp_end >= comp_start) then
                            component = trim(attr_trim(comp_start:comp_end))
                            if (len_trim(component) > 0) then
                                if (len_trim(filtered_attr) > 0) then
                                    filtered_attr = filtered_attr // ', '
                                end if
                                filtered_attr = filtered_attr // component
                            end if
                        end if
                        if (local_comma == 0) exit
                        comp_start = comp_end + 2
                    end do
                end if
            end if
        end if
        if (len_trim(filtered_attr) > 0) then
            decl_node%type_name = trim(decl_node%type_name) // ', ' // &
                                  trim(filtered_attr)
        end if

        dim_pos = index(lowered_type, 'dimension(')
        if (dim_pos > 0) then
            has_dimension_attr = .true.
            call parse_dimension_attribute(arena, prog_index, var_type, &
                                           dim_pos, decl_node)
        else
            if (allocated(decl_node%dimension_indices)) then
                deallocate (decl_node%dimension_indices)
            end if
            decl_node%is_array = .false.
        end if

        if (index(lowered_type, 'allocatable') > 0) then
            decl_node%is_allocatable = .true.
        else if (.not. has_dimension_attr) then
            decl_node%is_allocatable = .false.
        end if

        if (.not. has_dimension_attr) then
            call set_array_properties_from_type(arena, var_name, prog_index, &
                                                decl_node)
        end if

        if (decl_node%is_array .and. allocated(decl_node%dimension_indices)) then
            if (size(decl_node%dimension_indices) > 0) then
                if (decl_node%dimension_indices(1) == 0) then
                    decl_node%is_allocatable = .true.
                end if
            end if
        end if
    end subroutine apply_type_string_to_decl

    subroutine update_existing_declaration_type(arena, prog_index, var_name, &
                                                var_type)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        character(len=*), intent(in) :: var_name, var_type
        type(program_node) :: prog
        integer :: i, j, node_idx

        if (prog_index <= 0 .or. prog_index > arena%size) return
        if (.not. allocated(arena%entries(prog_index)%node)) return

        select type (prog => arena%entries(prog_index)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do i = 1, size(prog%body_indices)
                node_idx = prog%body_indices(i)
                if (node_idx <= 0 .or. node_idx > arena%size) cycle
                if (.not. allocated(arena%entries(node_idx)%node)) cycle
                select type (decl => arena%entries(node_idx)%node)
                type is (declaration_node)
                    if (.not. decl%is_multi_declaration) then
                        if (trim(decl%var_name) == trim(var_name)) then
                            call apply_type_string_to_decl(arena, prog_index, &
                                                           var_name, var_type, &
                                                           decl)
                            arena%entries(node_idx)%node = decl
                            return
                        end if
                    else if (allocated(decl%var_names)) then
                        do j = 1, size(decl%var_names)
                            if (trim(decl%var_names(j)) == trim(var_name)) then
                                if (index(to_lower(var_type), 'dimension(') == 0) then
                                    call apply_type_string_to_decl(arena, &
                                                                   prog_index, &
                                                                   var_name, &
                                                                   var_type, decl)
                                    arena%entries(node_idx)%node = decl
                                end if
                                return
                            end if
                        end do
                    end if
                end select
            end do
        end select
    end subroutine update_existing_declaration_type

end module standardizer_declarations_parsing
