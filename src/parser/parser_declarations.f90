module parser_declarations
    use iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER, TK_EOF, TK_KEYWORD
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_types, only: LITERAL_STRING
    use ast_nodes_data, only: INTENT_IN, INTENT_OUT, INTENT_INOUT
    use parser_expressions_module, only: parse_comparison, parse_range
    use parser_result_types, only: parse_result_t, success_parse_result, error_parse_result
    use error_handling, only: ERROR_PARSER
    implicit none
    private

    public :: parse_declaration, parse_multi_declaration, parse_declaration_with_result
    public :: parse_derived_type_def, parse_derived_type_component
    public :: parse_array_dimensions

    ! Type specifier result type for structured type information
    type, public :: type_specifier_t
        character(len=:), allocatable :: type_name
        logical :: has_kind = .false.
        integer :: kind_value = 0
        integer :: line = 0
        integer :: column = 0
    end type type_specifier_t

    ! Declaration attributes result type for structured attribute information
    type, public :: declaration_attributes_t
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_target = .false.
        logical :: is_parameter = .false.
        logical :: is_optional = .false.
        logical :: has_intent = .false.
        logical :: has_global_dimensions = .false.
        character(len=:), allocatable :: intent
        integer, allocatable :: global_dimension_indices(:)
    end type declaration_attributes_t

contains

    ! Parse type specifier (e.g., "integer(kind=8)", "character(len=*)")
    function parse_type_specifier(parser) result(type_spec)
        type(parser_state_t), intent(inout) :: parser
        type(type_specifier_t) :: type_spec

        type(token_t) :: token

        token = parser%consume()
        type_spec%type_name = token%text
        type_spec%line = token%line
        type_spec%column = token%column
        type_spec%has_kind = .false.
        type_spec%kind_value = 0

        ! Check for kind specification
        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%text == "(") then
                ! Skip kind specifications for simplicity
                do while (.not. parser%is_at_end())
                    token = parser%consume()
                    if (token%text == ")") exit
                end do
            end if
        end if
    end function parse_type_specifier

    ! Parse declaration attributes like allocatable, pointer, intent, etc.
    subroutine parse_declaration_attributes(parser, arena, attr_info)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(declaration_attributes_t), intent(out) :: attr_info

        type(token_t) :: token

        ! Initialize attributes
        attr_info%is_allocatable = .false.
        attr_info%is_pointer = .false.
        attr_info%is_target = .false.
        attr_info%is_parameter = .false.
        attr_info%is_optional = .false.
        attr_info%has_intent = .false.
        attr_info%has_global_dimensions = .false.

        ! Parse basic attributes (simplified)
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%text == ",") then
                token = parser%consume()
                token = parser%peek()
                
                select case (token%text)
                case ("allocatable")
                    attr_info%is_allocatable = .true.
                    token = parser%consume()
                case ("pointer")
                    attr_info%is_pointer = .true.
                    token = parser%consume()
                case ("parameter")
                    attr_info%is_parameter = .true.
                    token = parser%consume()
                case default
                    exit
                end select
            else
                exit
            end if
        end do
    end subroutine parse_declaration_attributes

    ! Parse single-variable declaration (e.g., real :: x)
    function parse_declaration(parser, arena) result(decl_index)
        use ast_factory, only: push_declaration
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: decl_index

        type(token_t) :: token
        type(type_specifier_t) :: type_spec
        type(declaration_attributes_t) :: attr_info
        integer :: initializer_index, intent_code

        decl_index = 0
        initializer_index = 0

        ! Parse type specifier
        type_spec = parse_type_specifier(parser)
        if (.not. allocated(type_spec%type_name)) then
            return
        end if

        ! Parse declaration attributes
        call parse_declaration_attributes(parser, arena, attr_info)

        ! Check for :: separator
        token = parser%peek()
        if (token%text == "::") then
            token = parser%consume()
        end if

        ! Get variable name
        if (parser%is_at_end()) then
            return
        end if

        token = parser%consume()
        if (token%kind /= TK_IDENTIFIER) then
            return
        end if

        ! Check for initialization
        if (.not. parser%is_at_end()) then
            token = parser%peek()
            if (token%text == "=" .or. token%text == "=>") then
                token = parser%consume()
                initializer_index = parse_comparison(parser, arena)
            end if
        end if

        ! Convert intent to code (simplified)
        intent_code = 0

        ! Create declaration node
        decl_index = push_declaration( &
            arena, &
            type_spec%type_name, &
            token%text, &
            initializer_index=initializer_index, &
            is_allocatable=attr_info%is_allocatable, &
            is_pointer=attr_info%is_pointer, &
            is_parameter=attr_info%is_parameter &
        )
    end function parse_declaration

    ! Result-based declaration parser with structured error handling
    function parse_declaration_with_result(parser, arena) result(parse_res)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(parse_result_t) :: parse_res

        integer :: decl_index

        decl_index = parse_declaration(parser, arena)

        if (decl_index > 0) then
            parse_res = success_parse_result(decl_index)
        else
            parse_res = error_parse_result("Failed to parse declaration", ERROR_PARSER)
        end if
    end function parse_declaration_with_result

    ! Parse array dimensions (e.g., (:), (10), (1:n))
    subroutine parse_array_dimensions(parser, arena, dimension_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: dimension_indices(:)

        integer, parameter :: max_dims = 10
        integer :: temp_indices(max_dims)
        integer :: dim_count, range_index
        type(token_t) :: token

        dim_count = 0

        ! Parse dimension list until closing parenthesis
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%text == ")") then
                token = parser%consume()
                exit
            end if

            ! Parse dimension specification
            range_index = parse_range(parser, arena)
            if (range_index > 0 .and. dim_count < max_dims) then
                dim_count = dim_count + 1
                temp_indices(dim_count) = range_index
            end if

            ! Check for comma
            token = parser%peek()
            if (token%text == ",") then
                token = parser%consume()
            else if (token%text /= ")") then
                exit
            end if
        end do

        ! Allocate exact size needed
        if (dim_count > 0) then
            allocate(dimension_indices(dim_count))
            dimension_indices = temp_indices(1:dim_count)
        else
            allocate(dimension_indices(0))
        end if
    end subroutine parse_array_dimensions

    ! Parse multi-variable declaration (e.g., real :: x, y, z = 1.0)
    function parse_multi_declaration(parser, arena) result(decl_indices)
        use ast_factory, only: push_multi_declaration
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable :: decl_indices(:)

        integer :: first_decl_index

        ! For simplicity, just parse as single declaration
        first_decl_index = parse_declaration(parser, arena)

        if (first_decl_index > 0) then
            allocate(decl_indices(1))
            decl_indices(1) = first_decl_index
        else
            allocate(decl_indices(0))
        end if
    end function parse_multi_declaration

    ! Parse derived type definition with robust error handling
    function parse_derived_type_def(parser, arena) result(type_index)
        use ast_factory, only: push_derived_type
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: type_index

        type(token_t) :: token
        character(len=100) :: type_name
        integer :: comp_index
        integer, parameter :: max_components = 100
        integer :: component_indices(max_components)
        integer :: component_count

        type_index = 0
        component_count = 0

        ! Consume 'type'
        token = parser%consume()

        ! Check for optional '::'
        token = parser%peek()
        if (token%text == "::") then
            token = parser%consume()
        end if

        ! Get type name
        token = parser%consume()
        if (token%kind /= TK_IDENTIFIER) then
            return
        end if
        type_name = token%text

        ! Parse components
        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for end type
            if (token%kind == TK_IDENTIFIER .and. token%text == "end") then
                token = parser%consume()
                token = parser%peek()
                if (token%text == "type") then
                    token = parser%consume()
                    exit
                end if
            end if

            ! Parse component
            comp_index = parse_derived_type_component(parser, arena)
            if (comp_index > 0 .and. component_count < max_components) then
                component_count = component_count + 1
                component_indices(component_count) = comp_index
            end if
        end do

        ! Create derived type node
        if (component_count > 0) then
            type_index = push_derived_type(arena, type_name, &
                component_indices(1:component_count))
        else
            type_index = push_derived_type(arena, type_name, &
                [integer ::])
        end if
    end function parse_derived_type_def

    ! Parse derived type component with robust error handling and loop prevention
    function parse_derived_type_component(parser, arena) result(comp_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: comp_index

        type(token_t) :: token
        integer :: safety_counter

        comp_index = 0
        safety_counter = 0

        ! Safety mechanism to prevent infinite loops
        do
            safety_counter = safety_counter + 1
            if (safety_counter > 5 .or. parser%is_at_end()) then
                exit
            end if

            token = parser%peek()
            
            ! Handle end of type definition
            if (token%kind == TK_IDENTIFIER .and. token%text == "end") then
                exit
            end if

            ! Check for type declaration keywords
            if (token%kind == TK_IDENTIFIER) then
                select case (token%text)
                case ("integer", "real", "complex", "logical", "character", "type")
                    comp_index = parse_declaration(parser, arena)
                    exit
                case default
                    ! Skip unknown token safely
                    token = parser%consume()
                end select
            else
                ! Skip non-identifier token
                token = parser%consume()
            end if
        end do
    end function parse_derived_type_component

end module parser_declarations