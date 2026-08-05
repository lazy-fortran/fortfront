module declaration_attribute_utils
    use string_utils_mod, only: to_lower
    use ast_arena_modern, only: ast_arena_t
    use codegen_arena_interface, only: generate_code_from_arena
    implicit none
    private

    public :: declaration_attribute_info_t
    public :: attribute_validation_t
    public :: reset_declaration_attributes
    public :: set_declaration_intent
    public :: append_declaration_attributes
    public :: validate_attribute_addition

    ! Result of checking one attr-spec against the attributes already seen in
    ! the same attribute list. Invalid means the source must be rejected.
    type :: attribute_validation_t
        logical :: valid = .true.
        character(len=:), allocatable :: message
    end type attribute_validation_t

    ! Attributes that may not appear together on the same entity. Each entry
    ! is "|<attr>|" separated so a whole-word lookup is a plain index() test.
    ! Sources: Fortran 2023 C862 (PARAMETER), C860 (POINTER/ALLOCATABLE),
    ! C861 (TARGET), C868 (VALUE), C858 (PROTECTED).
    character(len=*), parameter :: CONFLICTS_PARAMETER = &
        "|save|pointer|allocatable|external|target|optional|value|volatile|" // &
        "protected|asynchronous|contiguous|intent|"
    character(len=*), parameter :: CONFLICTS_POINTER = &
        "|allocatable|target|value|"
    character(len=*), parameter :: CONFLICTS_ALLOCATABLE = "|value|external|"
    character(len=*), parameter :: CONFLICTS_VALUE = "|volatile|external|"
    character(len=*), parameter :: CONFLICTS_PROTECTED = "|external|"
    character(len=*), parameter :: CONFLICTS_SAVE = "|intent|"
    character(len=*), parameter :: CONFLICTS_PUBLIC = "|private|"

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
        logical :: is_bind_c = .false.
        logical :: has_intent = .false.
        logical :: has_global_dimensions = .false.
        character(len=:), allocatable :: intent
        character(len=:), allocatable :: accessibility ! 'public'/'private'
        character(len=:), allocatable :: bind_name ! bind(c, name="...") value
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
        attr%is_bind_c = .false.
        attr%has_intent = .false.
        if (allocated(attr%intent)) deallocate (attr%intent)
        if (allocated(attr%accessibility)) deallocate (attr%accessibility)
        if (allocated(attr%bind_name)) deallocate (attr%bind_name)
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

    ! Check one attr-spec against the attributes already recorded for the same
    ! entity. `name` is lowercase and carries the intent direction when it has
    ! one, for example "intent(out)".
    function validate_attribute_addition(attr, name) result(validation)
        type(declaration_attribute_info_t), intent(in) :: attr
        character(len=*), intent(in) :: name
        type(attribute_validation_t) :: validation

        character(len=16) :: seen(20)
        character(len=:), allocatable :: added
        integer :: seen_count, i

        validation%valid = .true.
        added = attribute_base_name(name)
        call collect_present_attributes(attr, seen, seen_count)

        do i = 1, seen_count
            if (attribute_base_name(seen(i)) == added) then
                validation%valid = .false.
                validation%message = "Duplicate " // to_upper(added) // &
                    " attribute specified"
                return
            end if
        end do

        do i = 1, seen_count
            if (attributes_conflict(seen(i), name)) then
                validation%valid = .false.
                validation%message = to_upper(attribute_display_name(name)) // &
                    " attribute conflicts with " // &
                    to_upper(attribute_display_name(seen(i))) // " attribute"
                return
            end if
        end do
    end function validate_attribute_addition

    subroutine collect_present_attributes(attr, names, name_count)
        type(declaration_attribute_info_t), intent(in) :: attr
        character(len=*), intent(out) :: names(:)
        integer, intent(out) :: name_count

        name_count = 0
        call add_present_attribute(attr%is_allocatable, "allocatable", &
            names, name_count)
        call add_present_attribute(attr%is_pointer, "pointer", names, name_count)
        call add_present_attribute(attr%is_target, "target", names, name_count)
        call add_present_attribute(attr%is_parameter, "parameter", &
            names, name_count)
        call add_present_attribute(attr%is_external, "external", names, name_count)
        call add_present_attribute(attr%is_unsigned, "unsigned", names, name_count)
        call add_present_attribute(attr%is_optional, "optional", names, name_count)
        call add_present_attribute(attr%is_save, "save", names, name_count)
        call add_present_attribute(attr%is_volatile, "volatile", names, name_count)
        call add_present_attribute(attr%is_protected, "protected", &
            names, name_count)
        call add_present_attribute(attr%is_asynchronous, "asynchronous", &
            names, name_count)
        call add_present_attribute(attr%is_contiguous, "contiguous", &
            names, name_count)
        call add_present_attribute(attr%is_value, "value", names, name_count)
        call add_present_attribute(attr%is_bind_c, "bind", names, name_count)
        call add_present_attribute(attr%has_global_dimensions, "dimension", &
            names, name_count)
        if (attr%has_intent) then
            if (allocated(attr%intent)) then
                call add_present_attribute(.true., "intent(" // &
                    to_lower(trim(attr%intent)) // ")", names, name_count)
            else
                call add_present_attribute(.true., "intent", names, name_count)
            end if
        end if
        if (allocated(attr%accessibility)) then
            call add_present_attribute(.true., to_lower(trim(attr%accessibility)), &
                names, name_count)
        end if
    end subroutine collect_present_attributes

    subroutine add_present_attribute(is_present, name, names, name_count)
        logical, intent(in) :: is_present
        character(len=*), intent(in) :: name
        character(len=*), intent(inout) :: names(:)
        integer, intent(inout) :: name_count

        if (.not. is_present) return
        if (name_count >= size(names)) return
        name_count = name_count + 1
        names(name_count) = name
    end subroutine add_present_attribute

    function attribute_base_name(name) result(base)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: base
        integer :: paren

        base = to_lower(trim(adjustl(name)))
        paren = index(base, "(")
        if (paren > 1) base = base(1:paren - 1)
    end function attribute_base_name

    function attribute_display_name(name) result(display)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: display
        character(len=:), allocatable :: direction

        if (attribute_base_name(name) /= "intent") then
            display = attribute_base_name(name)
            return
        end if

        direction = intent_direction(name)
        if (len_trim(direction) > 0) then
            display = "intent(" // trim(direction) // ")"
        else
            display = "intent"
        end if
    end function attribute_display_name

    logical function attributes_conflict(existing, added) result(conflict)
        character(len=*), intent(in) :: existing
        character(len=*), intent(in) :: added

        conflict = pair_conflicts(attribute_base_name(existing), &
            attribute_base_name(added))
        if (.not. conflict) then
            conflict = pair_conflicts(attribute_base_name(added), &
                attribute_base_name(existing))
        end if
        if (.not. conflict) conflict = value_intent_conflict(existing, added)
        if (.not. conflict) conflict = value_intent_conflict(added, existing)
    end function attributes_conflict

    logical function pair_conflicts(first, second) result(conflict)
        character(len=*), intent(in) :: first
        character(len=*), intent(in) :: second
        character(len=:), allocatable :: probe

        probe = "|" // second // "|"
        select case (first)
        case ("parameter")
            conflict = index(CONFLICTS_PARAMETER, probe) > 0
        case ("pointer")
            conflict = index(CONFLICTS_POINTER, probe) > 0
        case ("allocatable")
            conflict = index(CONFLICTS_ALLOCATABLE, probe) > 0
        case ("value")
            conflict = index(CONFLICTS_VALUE, probe) > 0
        case ("protected")
            conflict = index(CONFLICTS_PROTECTED, probe) > 0
        case ("save")
            conflict = index(CONFLICTS_SAVE, probe) > 0
        case ("public")
            conflict = index(CONFLICTS_PUBLIC, probe) > 0
        case default
            conflict = .false.
        end select
    end function pair_conflicts

    ! VALUE is compatible with INTENT(IN) only; the other two directions are
    ! forbidden, so the direction has to be inspected rather than the bare name.
    logical function value_intent_conflict(value_name, intent_name) result(conflict)
        character(len=*), intent(in) :: value_name
        character(len=*), intent(in) :: intent_name
        character(len=:), allocatable :: direction

        conflict = .false.
        if (attribute_base_name(value_name) /= "value") return
        if (attribute_base_name(intent_name) /= "intent") return
        direction = intent_direction(intent_name)
        conflict = direction == "out" .or. direction == "inout"
    end function value_intent_conflict

    function intent_direction(name) result(direction)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: direction
        character(len=:), allocatable :: text
        integer :: lparen, rparen

        text = trim(adjustl(name))
        direction = ""
        lparen = index(text, "(")
        rparen = index(text, ")", back=.true.)
        if (lparen < 1) return
        if (rparen < lparen + 2) return
        direction = to_lower(text(lparen + 1:rparen - 1))
    end function intent_direction

    function to_upper(text) result(upper)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: upper
        integer :: i, code

        upper = text
        do i = 1, len(upper)
            code = iachar(upper(i:i))
            if (code >= iachar('a') .and. code <= iachar('z')) then
                upper(i:i) = achar(code - 32)
            end if
        end do
    end function to_upper

end module declaration_attribute_utils
