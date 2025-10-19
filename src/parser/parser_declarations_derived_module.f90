module parser_declarations_derived_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD, TK_NEWLINE, &
                          TK_WHITESPACE, TK_COMMENT, TK_OPERATOR
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_derived_type
    use parser_declarations_type_spec_support_module, only: &
        skip_type_definition_attributes
    use parser_declarations_core_module, only: parse_declaration
    implicit none
    private

    public :: parse_derived_type_def
    public :: parse_derived_type_component

contains

    function parse_derived_type_def(parser, arena) result(type_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: type_index

        character(len=100) :: type_name
        character(len=:), allocatable :: header_attributes
        logical :: has_header_attrs
        logical :: invalid_type_spec
        integer, allocatable :: component_indices(:)
        integer :: component_count

        type_index = 0

        call parse_type_definition_header(parser, type_name, header_attributes, &
                                          has_header_attrs, invalid_type_spec)
        if (invalid_type_spec) then
            return
        end if

        call collect_derived_type_components(parser, arena, component_indices, &
                                             component_count)
        type_index = finalize_derived_type(arena, type_name, header_attributes, &
                                           has_header_attrs, component_indices, &
                                           component_count)
    end function parse_derived_type_def

    subroutine parse_type_definition_header(parser, type_name, &
                                            header_attributes, &
                                            has_header_attrs, invalid_type_spec)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(out) :: type_name
        character(len=:), allocatable, intent(out) :: header_attributes
        logical, intent(out) :: has_header_attrs
        logical, intent(out) :: invalid_type_spec
        type(token_t) :: token

        type_name = ""
        has_header_attrs = .false.
        invalid_type_spec = .false.

        token = parser%consume()
        call skip_type_definition_attributes(parser, invalid_type_spec, &
                                             header_attributes)
        if (invalid_type_spec) then
            return
        end if

        if (allocated(header_attributes)) then
            if (len_trim(header_attributes) > 0) then
                has_header_attrs = .true.
            else
                block
                    character(len=:), allocatable :: temp
                    call move_alloc(header_attributes, temp)
                end block
            end if
        end if

        token = parser%peek()
        if (token%kind /= TK_IDENTIFIER) then
            invalid_type_spec = .true.
            return
        end if

        token = parser%consume()
        type_name = trim(token%text)

        call skip_type_header_trivia(parser)
    end subroutine parse_type_definition_header

    subroutine skip_type_header_trivia(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ";") then
                token = parser%consume()
            else if (token%kind == TK_NEWLINE .or. token%kind == TK_WHITESPACE .or. &
                     token%kind == TK_COMMENT) then
                token = parser%consume()
            else
                exit
            end if
        end do
    end subroutine skip_type_header_trivia

    subroutine collect_derived_type_components(parser, arena, component_indices, &
                                               component_count)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: component_indices(:)
        integer, intent(out) :: component_count
        integer, allocatable :: indices(:)
        integer :: capacity
        integer :: comp_index

        capacity = 8
        allocate (indices(capacity))
        component_count = 0

        do while (.not. parser%is_at_end())
            if (end_type_ahead(parser)) then
                call consume_end_type_sequence(parser)
                exit
            end if

            comp_index = parse_derived_type_component(parser, arena)
            if (comp_index > 0) then
                component_count = component_count + 1
                if (component_count > capacity) then
                    call expand_component_storage(indices, capacity)
                end if
                indices(component_count) = comp_index
                call skip_component_trivia(parser)
            else
                call skip_failed_component(parser)
            end if
        end do

        call finalize_component_storage(indices, component_count, &
                                        component_indices)
    end subroutine collect_derived_type_components

    logical function end_type_ahead(parser) result(is_end)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        is_end = .false.
        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if ((token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) .and. &
            token%text == "end") then
            is_end = .true.
        end if
    end function end_type_ahead

    subroutine consume_end_type_sequence(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        if (parser%is_at_end()) then
            return
        end if

        token = parser%consume()
        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if ((token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) .and. &
            token%text == "type") then
            token = parser%consume()
            if (.not. parser%is_at_end()) then
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER) then
                    token = parser%consume()
                end if
            end if
        end if
    end subroutine consume_end_type_sequence

    subroutine expand_component_storage(indices, capacity)
        integer, allocatable, intent(inout) :: indices(:)
        integer, intent(inout) :: capacity
        integer, allocatable :: temp(:)
        integer :: new_capacity

        new_capacity = capacity * 2
        allocate (temp(new_capacity))
        temp = 0
        temp(1:capacity) = indices(1:capacity)
        call move_alloc(temp, indices)
        capacity = new_capacity
    end subroutine expand_component_storage

    subroutine skip_component_trivia(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE) then
                token = parser%consume()
            else if (token%kind == TK_WHITESPACE .or. token%kind == TK_COMMENT) then
                token = parser%consume()
            else
                exit
            end if
        end do
    end subroutine skip_component_trivia

    subroutine skip_failed_component(parser)
        type(parser_state_t), intent(inout) :: parser
        type(token_t) :: token

        if (parser%is_at_end()) then
            return
        end if

        token = parser%peek()
        if ((token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) .and. &
            token%text == "end") then
            return
        end if

        if (token%kind == TK_NEWLINE) then
            token = parser%consume()
        else if (token%kind == TK_WHITESPACE .or. token%kind == TK_COMMENT) then
            token = parser%consume()
        else
            token = parser%consume()
        end if
    end subroutine skip_failed_component

    subroutine finalize_component_storage(indices, component_count, &
                                          component_indices)
        integer, allocatable, intent(inout) :: indices(:)
        integer, intent(in) :: component_count
        integer, allocatable, intent(out) :: component_indices(:)
        integer, allocatable :: temp(:)

        if (component_count <= 0) then
            allocate (temp(0))
            call move_alloc(temp, component_indices)
        else
            allocate (temp(component_count))
            temp = indices(1:component_count)
            call move_alloc(temp, component_indices)
        end if

        block
            integer, allocatable :: discard(:)
            call move_alloc(indices, discard)
        end block
    end subroutine finalize_component_storage

    integer function finalize_derived_type(arena, type_name, header_attributes, &
                                           has_header_attrs, component_indices, &
                                           component_count) result(type_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable, intent(in) :: header_attributes
        logical, intent(in) :: has_header_attrs
        integer, intent(in) :: component_indices(:)
        integer, intent(in) :: component_count

        if (component_count > 0) then
            if (has_header_attrs) then
                type_index = push_derived_type( &
                    arena, type_name, component_indices, &
                    attribute_clause=header_attributes)
            else
                type_index = push_derived_type(arena, type_name, component_indices)
            end if
        else
            if (has_header_attrs) then
                type_index = push_derived_type( &
                    arena, type_name, [integer ::], &
                                       attribute_clause=header_attributes)
            else
                type_index = push_derived_type(arena, type_name, [integer ::])
            end if
        end if
    end function finalize_derived_type

    ! Parse derived type component with robust error handling and loop prevention
    function parse_derived_type_component(parser, arena) result(comp_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: comp_index

        type(token_t) :: token

        comp_index = 0

        ! Skip any leading newlines
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE) then
                token = parser%consume()
            else if (token%kind == TK_WHITESPACE .or. &
                     token%kind == TK_COMMENT) then
                token = parser%consume()
            else
                exit
            end if
        end do

        token = parser%peek()

        ! Handle end of type definition
        if ((token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) .and. &
            token%text &
            == "end") then
            return
        end if

        ! Check for type declaration keywords
        if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
            select case (trim(adjustl(token%text)))
            case ("integer", "real", "complex", "logical", "character", &
                  "type", "double")
                comp_index = parse_declaration(parser, arena)
            case default
                ! Not a component declaration, return 0
                comp_index = 0
            end select
        else
            ! Not a component declaration
            comp_index = 0
        end if
    end function parse_derived_type_component

end module parser_declarations_derived_module
