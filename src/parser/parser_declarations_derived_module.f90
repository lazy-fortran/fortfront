module parser_declarations_derived_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD, TK_NEWLINE, &
                          TK_WHITESPACE, TK_COMMENT, TK_OPERATOR
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use parser_declarations_type_spec_support_module, only: &
        skip_type_definition_attributes
    use parser_declarations_core_module, only: parse_declaration
    implicit none
    private

    public :: parse_derived_type_def
    public :: parse_derived_type_component

contains

    function parse_derived_type_def(parser, arena) result(type_index)
        use ast_factory, only: push_derived_type
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: type_index

        type(token_t) :: token
        character(len=100) :: type_name
        character(len=:), allocatable :: header_attributes
        logical :: has_header_attrs
        integer :: comp_index
        integer, parameter :: max_components = 100
        integer :: component_indices(max_components)
        integer :: component_count
        logical :: invalid_type_spec

        type_index = 0
        component_count = 0
        token = parser%peek()
        ! Consume 'type'
        token = parser%consume()

        call skip_type_definition_attributes(parser, invalid_type_spec, &
                                             header_attributes)
        if (invalid_type_spec) then
            return
        end if

        has_header_attrs = .false.
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
            return
        end if
        token = parser%consume()
        type_name = trim(token%text)

        ! Skip any semicolons or newlines
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

        ! Parse components
        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for end type
            if ((token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) .and. &
                token%text == "end") then
                token = parser%consume()
                token = parser%peek()
                if ((token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) .and. &
                    token%text == "type") then
                    token = parser%consume()
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER) then
                        token = parser%consume()
                    end if
                end if
                exit
            end if

            ! Parse component
            comp_index = parse_derived_type_component(parser, arena)
            if (comp_index > 0 .and. component_count < max_components) then
                component_count = component_count + 1
                component_indices(component_count) = comp_index
                ! Skip any trailing newlines after parsing a component
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
            else if (comp_index == 0) then
                ! If we couldn't parse a component, skip to next line or token
                token = parser%peek()
                if (.not. ((token%kind == TK_IDENTIFIER .or. token%kind == &
                            TK_KEYWORD) .and. &
                           token%text == "end")) then
                    if (token%kind == TK_NEWLINE) then
                        token = parser%consume()
                    else if (token%kind == TK_WHITESPACE .or. &
                             token%kind == TK_COMMENT) then
                        token = parser%consume()
                    else
                        ! Skip unknown token to avoid infinite loop
                        token = parser%consume()
                    end if
                end if
            end if
        end do

        ! Create derived type node
        if (component_count > 0) then
            if (has_header_attrs) then
                type_index = push_derived_type(arena, type_name, &
                                               component_indices(1:component_count), &
                                               attribute_clause=header_attributes)
            else
                type_index = push_derived_type(arena, type_name, &
                                               component_indices(1:component_count))
            end if
        else
            if (has_header_attrs) then
                type_index = push_derived_type(arena, type_name, &
                                               [integer ::], &
                                                attribute_clause=header_attributes)
            else
                type_index = push_derived_type(arena, type_name, &
                                               [integer ::])
            end if
        end if
    end function parse_derived_type_def

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
            case ("integer", "real", "complex", "logical", "character", "type", "double")
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
