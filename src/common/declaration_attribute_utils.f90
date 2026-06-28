module declaration_attribute_utils
    use string_utils_mod, only: to_lower
    use ast_arena_modern, only: ast_arena_t
    use codegen_arena_interface, only: generate_code_from_arena
    implicit none
    private

    public :: declaration_attribute_info_t
    public :: reset_declaration_attributes
    public :: set_declaration_intent
    public :: append_declaration_attributes

    type :: declaration_attribute_info_t
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_target = .false.
        logical :: is_parameter = .false.
        logical :: is_external = .false.
        logical :: is_unsigned = .false.
        logical :: is_optional = .false.
        logical :: is_save = .false.
        logical :: is_volatile = .false.
        logical :: is_protected = .false.
        logical :: is_asynchronous = .false.
        logical :: is_contiguous = .false.
        logical :: is_value = .false.
        logical :: has_intent = .false.
        logical :: has_global_dimensions = .false.
        character(len=:), allocatable :: intent
        character(len=:), allocatable :: accessibility ! 'public'/'private'
        integer, allocatable :: global_dimension_indices(:)
    end type declaration_attribute_info_t

contains

    subroutine reset_declaration_attributes(attr)
        type(declaration_attribute_info_t), intent(inout) :: attr

        attr%is_allocatable = .false.
        attr%is_pointer = .false.
        attr%is_target = .false.
        attr%is_parameter = .false.
        attr%is_external = .false.
        attr%is_unsigned = .false.
        attr%is_optional = .false.
        attr%is_save = .false.
        attr%is_volatile = .false.
        attr%is_protected = .false.
        attr%is_asynchronous = .false.
        attr%is_contiguous = .false.
        attr%is_value = .false.
        attr%has_intent = .false.
        if (allocated(attr%intent)) deallocate (attr%intent)
        if (allocated(attr%accessibility)) deallocate (attr%accessibility)
        attr%has_global_dimensions = .false.
        if (allocated(attr%global_dimension_indices)) then
            deallocate (attr%global_dimension_indices)
        end if
    end subroutine reset_declaration_attributes

    subroutine set_declaration_intent(attr, value)
        type(declaration_attribute_info_t), intent(inout) :: attr
        character(len=*), intent(in) :: value

        attr%has_intent = .true.
        if (allocated(attr%intent)) deallocate (attr%intent)
        attr%intent = trim(adjustl(value))
    end subroutine set_declaration_intent

    subroutine append_declaration_attributes(code, attr, arena)
        character(len=:), allocatable, intent(inout) :: code
        type(declaration_attribute_info_t), intent(in) :: attr
        type(ast_arena_t), intent(in), optional :: arena
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: dim_clause

        if (.not. allocated(code)) then
            code = ""
        end if

        lowered = to_lower(trim(code))

        if (attr%has_global_dimensions .and. present(arena)) then
            if (allocated(attr%global_dimension_indices)) then
                if (index(lowered, 'dimension(') == 0) then
                    call build_dimension_attribute(arena, &
                        attr%global_dimension_indices, &
                        attr%is_allocatable, dim_clause)
                    code = trim(code) // ", " // dim_clause
                    lowered = to_lower(trim(code))
                end if
            end if
        end if

        if (attr%has_intent) then
            if (allocated(attr%intent)) then
                if (index(lowered, 'intent(') == 0) then
                    code = trim(code) // ", intent(" // trim(attr%intent) // ")"
                    lowered = to_lower(trim(code))
                end if
            end if
        end if

        if (attr%is_allocatable) then
            if (index(lowered, 'allocatable') == 0) then
                code = trim(code) // ", allocatable"
                lowered = to_lower(trim(code))
            end if
        end if

        if (attr%is_optional) then
            if (index(lowered, 'optional') == 0) then
                code = trim(code) // ", optional"
                lowered = to_lower(trim(code))
            end if
        end if

        if (attr%is_pointer) then
            if (index(lowered, 'pointer') == 0) then
                code = trim(code) // ", pointer"
                lowered = to_lower(trim(code))
            end if
        end if

        if (attr%is_target) then
            if (index(lowered, 'target') == 0) then
                code = trim(code) // ", target"
                lowered = to_lower(trim(code))
            end if
        end if

        if (attr%is_external) then
            if (index(lowered, 'external') == 0) then
                code = trim(code) // ", external"
                lowered = to_lower(trim(code))
            end if
        end if

        if (attr%is_parameter) then
            if (index(lowered, 'parameter') == 0) then
                code = trim(code) // ", parameter"
                lowered = to_lower(trim(code))
            end if
        end if

        if (attr%is_unsigned) then
            if (index(lowered, 'unsigned') == 0) then
                code = trim(code) // ", unsigned"
                lowered = to_lower(trim(code))
            end if
        end if

        if (attr%is_save) then
            if (index(lowered, 'save') == 0) then
                code = trim(code) // ", save"
                lowered = to_lower(trim(code))
            end if
        end if

        if (attr%is_volatile) then
            if (index(lowered, 'volatile') == 0) then
                code = trim(code) // ", volatile"
                lowered = to_lower(trim(code))
            end if
        end if

        if (attr%is_protected) then
            if (index(lowered, 'protected') == 0) then
                code = trim(code) // ", protected"
                lowered = to_lower(trim(code))
            end if
        end if

        if (attr%is_asynchronous) then
            if (index(lowered, 'asynchronous') == 0) then
                code = trim(code) // ", asynchronous"
                lowered = to_lower(trim(code))
            end if
        end if

        if (attr%is_contiguous) then
            if (index(lowered, 'contiguous') == 0) then
                code = trim(code) // ", contiguous"
                lowered = to_lower(trim(code))
            end if
        end if

        if (attr%is_value) then
            if (index(lowered, 'value') == 0) then
                code = trim(code) // ", value"
            end if
        end if
    end subroutine append_declaration_attributes

    subroutine build_dimension_attribute(arena, dimension_indices, &
            is_allocatable, clause)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: dimension_indices(:)
        logical, intent(in) :: is_allocatable
        character(len=:), allocatable, intent(out) :: clause
        character(len=:), allocatable :: dim_spec
        integer :: i, dim_index

        if (size(dimension_indices) == 0) then
            clause = ""
            return
        end if

        clause = "dimension("
        do i = 1, size(dimension_indices)
            if (i > 1) clause = clause // ", "
            dim_index = dimension_indices(i)
            if (dim_index == 0 .or. is_allocatable) then
                clause = clause // ":"
            else if (dim_index > 0 .and. dim_index <= arena%size) then
                dim_spec = generate_code_from_arena(arena, dim_index)
                if (len_trim(dim_spec) > 0) then
                    clause = clause // trim(dim_spec)
                else
                    clause = clause // ":"
                end if
            else
                clause = clause // ":"
            end if
        end do
        clause = clause // ")"
    end subroutine build_dimension_attribute

end module declaration_attribute_utils
