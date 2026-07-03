module standardizer_declarations_parsing
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_data, only: declaration_node
    use lexer_core, only: to_lower
    use standardizer_declarations_array, only: parse_dimension_attribute, &
                                               set_array_properties_from_type, &
                                               check_has_explicit_bounds
    use semantic_input_mode, only: INPUT_MODE_STANDARD
    use standardizer_parameter, only: get_standardizer_input_mode
    implicit none
    private

    public :: apply_type_string_to_decl
    public :: update_existing_declaration_type

contains

    ! Check if declaration has explicit array bounds (not deferred shape)
    ! Returns true only if all dimension_indices > 0
    pure logical function has_explicit_array_bounds_decl(decl) result(has_bounds)
        type(declaration_node), intent(in) :: decl
        integer :: i

        has_bounds = .false.
        if (.not. decl%is_array) return
        if (.not. allocated(decl%dimension_indices)) return
        if (size(decl%dimension_indices) == 0) return

        ! Check all dimensions have explicit bounds (index > 0)
        ! dimension_indices(i) == 0 means deferred shape (:)
        do i = 1, size(decl%dimension_indices)
            if (decl%dimension_indices(i) <= 0) return
        end do

        has_bounds = .true.
    end function has_explicit_array_bounds_decl

    subroutine parse_base_and_attributes(var_type, base_part, attr_part)
        character(len=*), intent(in) :: var_type
        character(len=:), allocatable, intent(out) :: base_part, attr_part
        integer :: comma_pos

        comma_pos = index(var_type, ',')
        if (comma_pos > 0) then
            base_part = trim(var_type(1:comma_pos - 1))
            attr_part = trim(var_type(comma_pos + 1:))
        else
            base_part = trim(var_type)
            attr_part = ''
        end if
    end subroutine parse_base_and_attributes

    pure function normalize_base_name(text) result(base_name)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: base_name
        integer :: paren_pos

        base_name = to_lower(trim(text))
        if (len_trim(base_name) == 0) return

        paren_pos = index(base_name, '(')
        if (paren_pos > 0) then
            if (paren_pos > 1) then
                base_name = trim(base_name(:paren_pos - 1))
            else
                base_name = ''
            end if
        end if
    end function normalize_base_name

    subroutine extract_kind_from_base(base_part, decl_node)
        character(len=:), allocatable, intent(inout) :: base_part
        type(declaration_node), intent(inout) :: decl_node
        integer :: paren_pos, close_pos, kind_val, ios

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
    end subroutine extract_kind_from_base

    subroutine remove_dimension_attrs(attr_part, filtered_attr)
        character(len=:), allocatable, intent(inout) :: attr_part
        character(len=:), allocatable, intent(out) :: filtered_attr
        character(len=:), allocatable :: attr_trim, lowered_component
        integer :: dim_pos, comp_start, comp_end, local_comma

        filtered_attr = ""
        if (len_trim(attr_part) == 0) return

        attr_trim = trim(attr_part)
        if (len_trim(attr_trim) == 0) return

        lowered_component = to_lower(attr_trim)
        do
            dim_pos = index(lowered_component, 'dimension(')
            if (dim_pos == 0) exit
            comp_start = dim_pos
            comp_end = comp_start + len('dimension(')
            local_comma = 1
            do while (comp_end <= len(lowered_component) .and. local_comma > 0)
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
            attr_trim = attr_trim(:comp_start - 1) // attr_trim(comp_end + 1:)
            lowered_component = to_lower(attr_trim)
        end do

        attr_part = attr_trim
    end subroutine remove_dimension_attrs

    subroutine build_filtered_attrs(attr_part, filtered_attr)
        character(len=:), allocatable, intent(in) :: attr_part
        character(len=:), allocatable, intent(inout) :: filtered_attr
        character(len=:), allocatable :: attr_trim, component
        integer :: comp_start, comp_end, local_comma

        attr_trim = trim(attr_part)
        if (len_trim(attr_trim) == 0) return

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
    end subroutine build_filtered_attrs

    subroutine apply_type_string_to_decl(arena, prog_index, var_name, &
                                         var_type, decl_node)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        character(len=*), intent(in) :: var_name, var_type
        type(declaration_node), intent(inout) :: decl_node
        integer :: dim_pos
        logical :: has_dimension_attr
        logical :: keep_base_type
        character(len=:), allocatable :: lowered_type
        character(len=:), allocatable :: base_part
        character(len=:), allocatable :: attr_part
        character(len=:), allocatable :: filtered_attr
        character(len=:), allocatable :: existing_base
        character(len=:), allocatable :: existing_attr
        character(len=:), allocatable :: base_name_existing
        character(len=:), allocatable :: base_name_new

        has_dimension_attr = .false.
        lowered_type = to_lower(var_type)
        keep_base_type = .false.

        call parse_base_and_attributes(var_type, base_part, attr_part)
        base_name_new = normalize_base_name(base_part)

        if (allocated(decl_node%type_name)) then
            call parse_base_and_attributes(decl_node%type_name, existing_base, &
                                           existing_attr)
            base_name_existing = normalize_base_name(existing_base)
            if (len_trim(base_name_existing) > 0 .and. &
                len_trim(base_name_new) > 0) then
                if (trim(base_name_new) == 'integer' .and. &
                    trim(base_name_existing) /= 'integer') then
                    keep_base_type = .true.
                end if
                ! Preserve existing type if it has a kind specifier and new doesn't
                ! e.g., existing "integer(kind=1)" vs new "integer"
                if (trim(base_name_new) == trim(base_name_existing)) then
                    if (index(existing_base, '(') > 0 .and. &
                        index(base_part, '(') == 0) then
                        keep_base_type = .true.
                    end if
                end if
            end if
        end if

        if (.not. keep_base_type) then
            call extract_kind_from_base(base_part, decl_node)
            decl_node%type_name = trim(base_part)
        end if
        call remove_dimension_attrs(attr_part, filtered_attr)
        call build_filtered_attrs(attr_part, filtered_attr)

        if (.not. keep_base_type) then
            if (len_trim(filtered_attr) > 0) then
                ! Known attributes become declaration flags so type_name stays
                ! a pure type spec; only unrecognized text is kept in it.
                call absorb_attribute_flags(filtered_attr, decl_node)
                if (len_trim(filtered_attr) > 0) then
                    decl_node%type_name = trim(decl_node%type_name) // ', ' // &
                                          trim(adjustl(filtered_attr))
                end if
            end if
        end if

        dim_pos = index(lowered_type, 'dimension(')
        if (dim_pos > 0) then
            has_dimension_attr = .true.
            call parse_dimension_attribute(arena, prog_index, var_type, &
                                           dim_pos, decl_node)
        else
            ! Preserve existing explicit array bounds (standard Fortran declarations)
            ! ISO/IEC 1539-1:2018 Section 8.5.8.2: explicit-shape arrays have
            ! declared dimensions that should not be overwritten by inference
            if (.not. check_has_explicit_bounds(arena, decl_node)) then
                if (allocated(decl_node%dimension_indices)) then
                    deallocate (decl_node%dimension_indices)
                end if
                decl_node%is_array = .false.
            end if
        end if

        if (index(lowered_type, 'allocatable') > 0) then
            decl_node%is_allocatable = .true.
        else if (.not. has_dimension_attr .and. .not. keep_base_type) then
            decl_node%is_allocatable = .false.
        end if

        if (index(lowered_type, 'pointer') > 0) then
            decl_node%is_pointer = .true.
        else if (.not. keep_base_type) then
            decl_node%is_pointer = .false.
        end if

        if (.not. has_dimension_attr) then
            call set_array_properties_from_type(arena, var_name, prog_index, &
                                                decl_node)
        end if

        if (decl_node%is_array .and. allocated(decl_node%dimension_indices)) then
            if (size(decl_node%dimension_indices) > 0) then
                if (decl_node%dimension_indices(1) == 0) then
                    ! Deferred shape array - needs allocatable UNLESS its a pointer
                    ! ISO/IEC 1539-1:2018 Section 8.5.3: pointer arrays can have
                    ! deferred shape without allocatable attribute
                    if (.not. decl_node%is_pointer) then
                        decl_node%is_allocatable = .true.
                    end if
                end if
            end if
        end if
    end subroutine apply_type_string_to_decl

    subroutine absorb_attribute_flags(filtered_attr, decl_node)
        character(len=:), allocatable, intent(inout) :: filtered_attr
        type(declaration_node), intent(inout) :: decl_node
        character(len=:), allocatable :: residual, component
        integer :: comp_start, comma_pos, comp_end

        residual = ''
        comp_start = 1
        do
            if (comp_start > len_trim(filtered_attr)) exit
            comma_pos = index(filtered_attr(comp_start:), ',')
            if (comma_pos > 0) then
                comp_end = comp_start + comma_pos - 2
            else
                comp_end = len_trim(filtered_attr)
            end if
            if (comp_end >= comp_start) then
                component = trim(adjustl(filtered_attr(comp_start:comp_end)))
                if (len_trim(component) > 0) then
                    call set_attribute_flag(component, decl_node, residual)
                end if
            end if
            if (comma_pos == 0) exit
            comp_start = comp_end + 2
        end do
        filtered_attr = residual
    end subroutine absorb_attribute_flags

    subroutine set_attribute_flag(component, decl_node, residual)
        character(len=*), intent(in) :: component
        type(declaration_node), intent(inout) :: decl_node
        character(len=:), allocatable, intent(inout) :: residual
        character(len=:), allocatable :: lowered

        lowered = to_lower(trim(component))
        select case (lowered)
        case ('allocatable')
            decl_node%is_allocatable = .true.
        case ('pointer')
            decl_node%is_pointer = .true.
        case ('target')
            decl_node%is_target = .true.
        case ('parameter')
            decl_node%is_parameter = .true.
        case ('optional')
            decl_node%is_optional = .true.
        case ('save')
            decl_node%is_save = .true.
        case ('external')
            decl_node%is_external = .true.
        case ('volatile')
            decl_node%is_volatile = .true.
        case ('protected')
            decl_node%is_protected = .true.
        case ('asynchronous')
            decl_node%is_asynchronous = .true.
        case ('contiguous')
            decl_node%is_contiguous = .true.
        case ('value')
            decl_node%is_value = .true.
        case default
            if (index(lowered, 'intent(') == 1 .and. &
                index(lowered, ')') == len_trim(lowered)) then
                decl_node%has_intent = .true.
                decl_node%intent = lowered(8:len_trim(lowered) - 1)
            else
                if (len_trim(residual) > 0) residual = residual // ', '
                residual = residual // trim(component)
            end if
        end select
    end subroutine set_attribute_flag

    subroutine update_existing_declaration_type(arena, prog_index, var_name, &
                                                var_type)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        character(len=*), intent(in) :: var_name, var_type
        type(program_node) :: prog
        integer :: i, j, node_idx
        character(len=64) :: target_name
        character(len=64) :: candidate_name

        target_name = to_lower(trim(var_name))

        if (.not. arena%has_node_at(prog_index)) return

        select type (prog => arena%entries(prog_index)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do i = 1, size(prog%body_indices)
                node_idx = prog%body_indices(i)
                if (.not. arena%has_node_at(node_idx)) cycle
                select type (decl => arena%entries(node_idx)%node)
                type is (declaration_node)
                    if (.not. decl%is_multi_declaration) then
                        candidate_name = to_lower(trim(decl%var_name))
                        if (trim(candidate_name) == trim(target_name)) then
                            ! Preserve existing explicit array bounds ONLY for standard
                            ! Fortran (.f90) files - for lazy Fortran (.lf), dimensions
                            ! may need correction based on actual usage
                            ! ISO/IEC 1539-1:2018 Section 8.5.8.2: explicit-shape arrays
                            ! Note: dimension_indices(i)==0 means deferred shape (:)
                            !       dimension_indices(i)>0 means explicit size
                            if (get_standardizer_input_mode() == &
                                INPUT_MODE_STANDARD) then
                                if (has_explicit_array_bounds_decl(decl)) then
                                    ! Skip type update - preserve original bounds
                                    return
                                end if
                            end if
                            call apply_type_string_to_decl(arena, prog_index, &
                                                           var_name, var_type, &
                                                           decl)
                            arena%entries(node_idx)%node = decl
                            return
                        end if
                    else if (allocated(decl%var_names)) then
                        do j = 1, size(decl%var_names)
                            candidate_name = to_lower(trim(decl%var_names(j)))
                            if (trim(candidate_name) == trim(target_name)) then
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
