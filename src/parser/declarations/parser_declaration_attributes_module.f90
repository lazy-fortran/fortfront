module parser_declaration_attributes_module
    use lexer_core, only: token_t, to_lower
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use parser_expressions_module, only: parse_range
    use declaration_attribute_utils, only: declaration_attribute_info_t, &
        reset_declaration_attributes, &
        set_declaration_intent, &
        attribute_validation_t, &
        validate_attribute_addition
    implicit none
    private

    public :: parse_declaration_attributes
    public :: parse_array_dimensions

contains

    subroutine parse_declaration_attributes(parser, arena, attr_info)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(declaration_attribute_info_t), intent(out) :: attr_info

        logical :: handled_attribute
        type(token_t) :: token
        call reset_declaration_attributes(attr_info)

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%text /= ",") then
                exit
            end if

            token = parser%consume()
            handled_attribute = parse_single_declaration_attribute(parser, &
                arena, attr_info)
            if (.not. handled_attribute) then
                exit
            end if
        end do
    end subroutine parse_declaration_attributes

    logical function parse_single_declaration_attribute(parser, arena, attr_info) &
            result(handled)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(declaration_attribute_info_t), intent(inout) :: attr_info

        type(token_t) :: token, attribute_token
        character(len=:), allocatable :: lowered

        handled = .false.

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        attribute_token = token
        lowered = to_lower(trim(token%text))

        ! INTENT is checked inside its own handler because the conflict rules
        ! depend on the direction, which is not known until it is parsed.
        if (is_declaration_attribute_keyword(lowered)) then
            if (lowered /= "intent") then
                call check_attribute_addition(parser, attr_info, lowered, &
                    attribute_token)
            end if
        end if

        select case (lowered)
        case ("allocatable")
            attr_info%is_allocatable = .true.
            token = parser%consume()
            handled = .true.
        case ("pointer")
            attr_info%is_pointer = .true.
            token = parser%consume()
            handled = .true.
        case ("parameter")
            attr_info%is_parameter = .true.
            token = parser%consume()
            handled = .true.
        case ("external")
            attr_info%is_external = .true.
            token = parser%consume()
            handled = .true.
        case ("unsigned")
            attr_info%is_unsigned = .true.
            token = parser%consume()
            handled = .true.
        case ("dimension")
            token = parser%consume()
            call handle_dimension_attribute(parser, arena, attr_info, handled)
        case ("intent")
            token = parser%consume()
            call handle_intent_attribute(parser, attr_info, handled, &
                attribute_token)
        case ("bind")
            token = parser%consume()
            call handle_bind_attribute(parser, attr_info, handled)
        case ("optional")
            attr_info%is_optional = .true.
            token = parser%consume()
            handled = .true.
        case ("save")
            attr_info%is_save = .true.
            token = parser%consume()
            handled = .true.
        case ("target")
            attr_info%is_target = .true.
            token = parser%consume()
            handled = .true.
        case ("volatile")
            attr_info%is_volatile = .true.
            token = parser%consume()
            handled = .true.
        case ("protected")
            attr_info%is_protected = .true.
            token = parser%consume()
            handled = .true.
        case ("asynchronous")
            attr_info%is_asynchronous = .true.
            token = parser%consume()
            handled = .true.
        case ("contiguous")
            attr_info%is_contiguous = .true.
            token = parser%consume()
            handled = .true.
        case ("value")
            attr_info%is_value = .true.
            token = parser%consume()
            handled = .true.
        case ("public", "private")
            attr_info%accessibility = lowered
            token = parser%consume()
            handled = .true.
        case default
            handled = .false.
        end select
    end function parse_single_declaration_attribute

    logical function is_declaration_attribute_keyword(name) result(is_attribute)
        character(len=*), intent(in) :: name

        select case (name)
        case ("allocatable", "pointer", "parameter", "external", "unsigned", &
                "dimension", "intent", "bind", "optional", "save", "target", &
                "volatile", "protected", "asynchronous", "contiguous", "value", &
                "public", "private")
            is_attribute = .true.
        case default
            is_attribute = .false.
        end select
    end function is_declaration_attribute_keyword

    subroutine check_attribute_addition(parser, attr_info, name, token)
        type(parser_state_t), intent(inout) :: parser
        type(declaration_attribute_info_t), intent(in) :: attr_info
        character(len=*), intent(in) :: name
        type(token_t), intent(in) :: token

        type(attribute_validation_t) :: validation

        validation = validate_attribute_addition(attr_info, name)
        if (validation%valid) return
        if (.not. allocated(validation%message)) return
        call parser%error_at_token(validation%message, token)
    end subroutine check_attribute_addition

    subroutine handle_dimension_attribute(parser, arena, attr_info, handled)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(declaration_attribute_info_t), intent(inout) :: attr_info
        logical, intent(out) :: handled

        type(token_t) :: token

        handled = .true.

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%text /= "(") then
            return
        end if

        token = parser%consume()
        call parse_array_dimensions(parser, arena, attr_info%global_dimension_indices)
        attr_info%has_global_dimensions = .true.
    end subroutine handle_dimension_attribute

    subroutine handle_intent_attribute(parser, attr_info, handled, attribute_token)
        type(parser_state_t), intent(inout) :: parser
        type(declaration_attribute_info_t), intent(inout) :: attr_info
        logical, intent(out) :: handled
        type(token_t), intent(in) :: attribute_token

        type(token_t) :: token
        character(len=:), allocatable :: lowered

        handled = .true.

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%text /= "(") then
            return
        end if

        token = parser%consume()
        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        lowered = to_lower(trim(token%text))
        select case (lowered)
        case ("in", "out", "inout")
            call check_attribute_addition(parser, attr_info, &
                "intent(" // lowered // ")", attribute_token)
            call set_declaration_intent(attr_info, lowered)
            token = parser%consume()
        case default
            return
        end select

        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%text == ")") then
                token = parser%consume()
            end if
        end if
    end subroutine handle_intent_attribute

    subroutine handle_bind_attribute(parser, attr_info, handled)
        type(parser_state_t), intent(inout) :: parser
        type(declaration_attribute_info_t), intent(inout) :: attr_info
        logical, intent(out) :: handled

        type(token_t) :: token

        handled = .true.
        attr_info%is_bind_c = .true.

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (token%text /= "(") then
            return
        end if
        token = parser%consume()

        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (to_lower(trim(token%text)) == "c") then
                token = parser%consume()
            end if
        end if

        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%text == ",") then
                token = parser%consume()
                call parse_bind_name_clause(parser, attr_info)
            end if
        end if

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%text == ")") then
                token = parser%consume()
                exit
            end if
            token = parser%consume()
        end do
    end subroutine handle_bind_attribute

    subroutine parse_bind_name_clause(parser, attr_info)
        type(parser_state_t), intent(inout) :: parser
        type(declaration_attribute_info_t), intent(inout) :: attr_info

        type(token_t) :: token

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if (to_lower(trim(token%text)) /= "name") then
            return
        end if
        token = parser%consume()

        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%text == "=") then
                token = parser%consume()
            end if
        end if

        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%text /= ")" .and. token%text /= ",") then
                attr_info%bind_name = trim(token%text)
                token = parser%consume()
            end if
        end if
    end subroutine parse_bind_name_clause

    subroutine parse_array_dimensions(parser, arena, dimension_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: dimension_indices(:)

        integer, parameter :: max_dims = 10
        integer :: temp_indices(max_dims)
        integer :: dim_count
        integer :: range_index
        type(token_t) :: token

        dim_count = 0

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%text == ")") then
                token = parser%consume()
                exit
            end if

            range_index = parse_range(parser, arena)
            if (range_index > 0 .and. dim_count < max_dims) then
                dim_count = dim_count + 1
                temp_indices(dim_count) = range_index
            end if

            token = parser%peek()
            if (token%text == ",") then
                token = parser%consume()
            else if (token%text /= ")") then
                exit
            end if
        end do

        if (dim_count > 0) then
            allocate (dimension_indices(dim_count))
            dimension_indices = temp_indices(1:dim_count)
        else
            allocate (dimension_indices(0))
        end if
    end subroutine parse_array_dimensions

end module parser_declaration_attributes_module
