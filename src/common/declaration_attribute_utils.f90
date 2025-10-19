module declaration_attribute_utils
    use string_utils_mod, only: to_lower
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
        logical :: is_optional = .false.
        logical :: has_intent = .false.
        logical :: has_global_dimensions = .false.
        character(len=:), allocatable :: intent
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
        attr%is_optional = .false.
        attr%has_intent = .false.
        if (allocated(attr%intent)) deallocate (attr%intent)
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

    subroutine append_declaration_attributes(code, attr)
        character(len=:), allocatable, intent(inout) :: code
        type(declaration_attribute_info_t), intent(in) :: attr
        character(len=:), allocatable :: lowered

        if (.not. allocated(code)) then
            code = ""
        end if

        lowered = to_lower(trim(code))

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
            end if
        end if
    end subroutine append_declaration_attributes

end module declaration_attribute_utils
