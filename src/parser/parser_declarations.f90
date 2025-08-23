module parser_declarations
    use iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER, TK_EOF, TK_KEYWORD
    use parser_state_module, only: parser_state_t
    use ast_arena, only: ast_arena_t
    use ast_types, only: LITERAL_STRING
    use ast_nodes_data, only: INTENT_IN, INTENT_OUT, INTENT_INOUT
    use parser_expressions_module, only: parse_expression, parse_comparison, parse_range
    implicit none
    private

    public :: parse_declaration, parse_multi_declaration
    public :: parse_derived_type_def
    public :: parse_derived_type_component
    public :: parse_array_dimensions

    ! Variable name collection result type
    type :: var_collection_t
        character(len=:), allocatable :: names(:)
        integer :: count
    end type var_collection_t

    ! Declaration parameters for unified creation
    type :: declaration_params_t
        character(len=:), allocatable :: type_name
        logical :: has_kind = .false.
        integer :: kind_value = 0
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_target = .false.
        logical :: is_parameter = .false.
        logical :: is_optional = .false.
        logical :: has_intent = .false.
        character(len=:), allocatable :: intent
        integer :: line = 1
        integer :: column = 1
    end type declaration_params_t

    ! Type specifier result from parse_type_specifier
    type :: type_specifier_t
        character(len=:), allocatable :: type_name
        logical :: has_kind = .false.
        integer :: kind_value = 0
        integer :: line = 1
        integer :: column = 1
    end type type_specifier_t

    ! Declaration attributes result from parse_declaration_attributes
    type :: declaration_attributes_t
        logical :: is_allocatable = .false.
        logical :: is_pointer = .false.
        logical :: is_target = .false.
        logical :: is_parameter = .false.
        logical :: is_optional = .false.
        logical :: has_intent = .false.
        character(len=:), allocatable :: intent
        logical :: has_global_dimensions = .false.
        integer, allocatable :: global_dimension_indices(:)
    end type declaration_attributes_t

contains

    ! Helper function to collect variable names from multi-variable declarations
    function collect_variable_names(parser, first_var_name) result(var_collection)
        type(parser_state_t), intent(inout) :: parser
        character(len=*), intent(in) :: first_var_name
        type(var_collection_t) :: var_collection
        
        type(token_t) :: var_token
        character(len=100) :: temp_names(50)
        integer :: name_count
        
        ! Start with the first variable we already parsed
        name_count = 1
        temp_names(1) = trim(first_var_name)
        
        ! Collect additional variables
        do while (.not. parser%is_at_end())
            var_token = parser%peek()
            if (var_token%kind == TK_EOF) exit

            ! Check if we have a comma (indicating another variable)
            if (var_token%kind == TK_OPERATOR .and. var_token%text == ",") then
                ! Consume the comma
                var_token = parser%consume()

                ! Get the next variable name
                var_token = parser%peek()
                if (var_token%kind == TK_IDENTIFIER) then
                    var_token = parser%consume()
                    if (name_count < size(temp_names)) then
                        name_count = name_count + 1
                        temp_names(name_count) = trim(var_token%text)
                    end if
                else
                    ! Error: expected identifier after comma - exit collection
                    exit
                end if
            else
                ! No more variables, exit without consuming
                exit
            end if
        end do
        
        ! Copy to properly sized array
        var_collection%count = name_count
        if (name_count > 0) then
            allocate(character(len=100) :: var_collection%names(name_count))
            var_collection%names(1:name_count) = temp_names(1:name_count)
        end if
    end function collect_variable_names

    ! Helper function to determine final dimensions based on precedence rules
    function determine_final_dimensions(has_per_var_dims, per_var_dims, has_global_dims, global_dims) result(final_dims)
        logical, intent(in) :: has_per_var_dims, has_global_dims
        integer, allocatable, intent(in) :: per_var_dims(:), global_dims(:)
        integer, allocatable :: final_dims(:)
        
        ! Per-variable dimensions take precedence over global dimensions
        if (has_per_var_dims) then
            if (allocated(per_var_dims)) then
                final_dims = per_var_dims
            end if
        else if (has_global_dims) then
            if (allocated(global_dims)) then
                final_dims = global_dims
            end if
        end if
    end function determine_final_dimensions

    ! Helper subroutine to create declaration nodes with unified logic
    subroutine create_declaration_nodes(arena, params, var_names, final_dims, has_final_dims, first_index)
        use ast_factory, only: push_declaration
        type(ast_arena_t), intent(inout) :: arena
        type(declaration_params_t), intent(in) :: params
        character(len=*), intent(in) :: var_names(:)
        integer, allocatable, intent(in) :: final_dims(:)
        logical, intent(in) :: has_final_dims
        integer, intent(out) :: first_index
        
        integer :: i, decl_index
        
        first_index = 0
        
        do i = 1, size(var_names)
            ! Create declaration based on attribute combination
            if (params%has_kind .and. has_final_dims) then
                if (params%has_intent) then
                    decl_index = push_declaration(arena, params%type_name, &
                        trim(var_names(i)), kind_value=params%kind_value, &
                        dimension_indices=final_dims, &
                        is_allocatable=params%is_allocatable, is_pointer=params%is_pointer, &
                        is_target=params%is_target, intent_value=params%intent, &
                        is_optional=params%is_optional, is_parameter=params%is_parameter, &
                        line=params%line, column=params%column)
                else
                    decl_index = push_declaration(arena, params%type_name, &
                        trim(var_names(i)), kind_value=params%kind_value, &
                        dimension_indices=final_dims, &
                        is_allocatable=params%is_allocatable, is_pointer=params%is_pointer, &
                        is_target=params%is_target, is_optional=params%is_optional, &
                        is_parameter=params%is_parameter, &
                        line=params%line, column=params%column)
                end if
            else if (params%has_kind) then
                if (params%has_intent) then
                    decl_index = push_declaration(arena, params%type_name, &
                        trim(var_names(i)), kind_value=params%kind_value, &
                        is_allocatable=params%is_allocatable, is_pointer=params%is_pointer, &
                        is_target=params%is_target, intent_value=params%intent, &
                        is_optional=params%is_optional, is_parameter=params%is_parameter, &
                        line=params%line, column=params%column)
                else
                    decl_index = push_declaration(arena, params%type_name, &
                        trim(var_names(i)), kind_value=params%kind_value, &
                        is_allocatable=params%is_allocatable, is_pointer=params%is_pointer, &
                        is_target=params%is_target, is_optional=params%is_optional, &
                        is_parameter=params%is_parameter, &
                        line=params%line, column=params%column)
                end if
            else if (has_final_dims) then
                if (params%has_intent) then
                    decl_index = push_declaration(arena, params%type_name, &
                        trim(var_names(i)), &
                        dimension_indices=final_dims, &
                        is_allocatable=params%is_allocatable, is_pointer=params%is_pointer, &
                        is_target=params%is_target, intent_value=params%intent, &
                        is_optional=params%is_optional, is_parameter=params%is_parameter, &
                        line=params%line, column=params%column)
                else
                    decl_index = push_declaration(arena, params%type_name, &
                        trim(var_names(i)), &
                        dimension_indices=final_dims, &
                        is_allocatable=params%is_allocatable, is_pointer=params%is_pointer, &
                        is_target=params%is_target, is_optional=params%is_optional, &
                        is_parameter=params%is_parameter, &
                        line=params%line, column=params%column)
                end if
            else
                if (params%has_intent) then
                    decl_index = push_declaration(arena, params%type_name, &
                        trim(var_names(i)), &
                        is_allocatable=params%is_allocatable, is_pointer=params%is_pointer, &
                        is_target=params%is_target, intent_value=params%intent, &
                        is_optional=params%is_optional, is_parameter=params%is_parameter, &
                        line=params%line, column=params%column)
                else
                    decl_index = push_declaration(arena, params%type_name, &
                        trim(var_names(i)), &
                        is_allocatable=params%is_allocatable, is_pointer=params%is_pointer, &
                        is_target=params%is_target, is_optional=params%is_optional, &
                        is_parameter=params%is_parameter, &
                        line=params%line, column=params%column)
                end if
            end if
            
            ! Remember first declaration index to return
            if (i == 1) first_index = decl_index
        end do
    end subroutine create_declaration_nodes

    ! Parse type specifier (e.g., integer, real(8), character(len=*))
    function parse_type_specifier(parser) result(type_spec)
        type(parser_state_t), intent(inout) :: parser
        type(type_specifier_t) :: type_spec
        
        type(token_t) :: token
        
        ! Get type name
        token = parser%peek()
        if (token%kind == TK_KEYWORD .or. token%kind == TK_IDENTIFIER) then
            select case (token%text)
            case ("integer", "real", "complex", "logical", "character", "type")
                token = parser%consume()
                type_spec%type_name = token%text
                type_spec%line = token%line
                type_spec%column = token%column
                
                ! Check for kind specification (e.g., real(8))
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == "(") then
                    token = parser%consume()  ! consume '('
                    
                    ! Get kind value
                    token = parser%peek()
                    if (token%kind == TK_NUMBER) then
                        token = parser%consume()
                        read (token%text, *) type_spec%kind_value
                        type_spec%has_kind = .true.
                        
                        ! Consume ')'
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == ")") then
                            token = parser%consume()
                        else
                            type_spec%type_name = "ERROR: Expected ) after kind value"
                        end if
                    else if (token%kind == TK_IDENTIFIER) then
                        ! Handle character(len=*) or similar
                        token = parser%consume()
                        
                        ! Simplified: just consume until ')'
                        do while (.not. parser%is_at_end())
                            token = parser%peek()
                            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                                token = parser%consume()
                                exit
                            end if
                            token = parser%consume()
                        end do
                    else
                        ! Consume ')' for empty parentheses
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == ")") then
                            token = parser%consume()
                        end if
                    end if
                end if
            case default
                type_spec%type_name = "ERROR: Unknown type " // token%text
                type_spec%line = token%line
                type_spec%column = token%column
            end select
        else
            type_spec%type_name = "ERROR: Expected type specifier"
            type_spec%line = token%line
            type_spec%column = token%column
        end if
    end function parse_type_specifier

    ! Parse declaration attributes (allocatable, pointer, intent, etc.)
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
        
        ! Parse attributes separated by commas
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()  ! consume ','
                
                ! Parse attribute
                token = parser%peek()
                if ((token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD)) then
                    select case (token%text)
                    case ("allocatable")
                        attr_info%is_allocatable = .true.
                        token = parser%consume()
                    case ("pointer")
                        attr_info%is_pointer = .true.
                        token = parser%consume()
                    case ("target")
                        attr_info%is_target = .true.
                        token = parser%consume()
                    case ("parameter")
                        attr_info%is_parameter = .true.
                        token = parser%consume()
                    case ("optional")
                        attr_info%is_optional = .true.
                        token = parser%consume()
                    case ("dimension")
                        attr_info%has_global_dimensions = .true.
                        token = parser%consume()
                        
                        ! Parse dimension specification
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "(") then
                            token = parser%consume()  ! consume '('
                            
                            ! Parse dimensions
                            call parse_array_dimensions(parser, arena, attr_info%global_dimension_indices)
                            
                            ! Consume ')'
                            token = parser%peek()
                            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                                token = parser%consume()
                            end if
                        end if
                    case ("intent")
                        attr_info%has_intent = .true.
                        token = parser%consume()  ! consume 'intent'
                        
                        ! Parse intent specification
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "(") then
                            token = parser%consume()  ! consume '('
                            
                            token = parser%peek()
                            if (token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) then
                                attr_info%intent = token%text
                                token = parser%consume()  ! consume intent value
                            end if
                            
                            token = parser%peek()
                            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                                token = parser%consume()  ! consume ')'
                            end if
                        end if
                    case default
                        ! Unknown attribute - consume it
                        token = parser%consume()
                        
                        ! If next token is '(', consume until matching ')'
                        token = parser%peek()
                        if (token%kind == TK_OPERATOR .and. token%text == "(") then
                            block
                                integer :: paren_count
                                paren_count = 1
                                token = parser%consume()  ! consume '('
                                
                                do while (paren_count > 0 .and. .not. parser%is_at_end())
                                    token = parser%peek()
                                    if (token%kind == TK_OPERATOR) then
                                        if (token%text == "(") then
                                            paren_count = paren_count + 1
                                        else if (token%text == ")") then
                                            paren_count = paren_count - 1
                                        end if
                                    end if
                                    token = parser%consume()
                                end do
                            end block
                        end if
                    end select
                end if
            else
                ! No more attributes
                exit
            end if
        end do
    end subroutine parse_declaration_attributes

    ! Parse single-variable declaration (e.g., real :: x) - REFACTORED with helper functions  
    function parse_declaration(parser, arena) result(decl_index)
        use ast_factory, only: push_declaration, push_literal
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: decl_index

        type(type_specifier_t) :: type_spec
        type(declaration_attributes_t) :: attr_info  
        type(declaration_params_t) :: params
        type(token_t) :: var_token
        character(len=:), allocatable :: var_name
        integer, allocatable :: dimension_indices(:), final_dims(:)
        logical :: is_array, has_initializer
        integer :: initializer_index
        type(var_collection_t) :: variables
        
        ! Parse type specifier and attributes using extracted helper functions
        type_spec = parse_type_specifier(parser)
        if (index(type_spec%type_name, "ERROR:") == 1) then
            decl_index = push_literal(arena, type_spec%type_name, LITERAL_STRING, type_spec%line, type_spec%column)
            return
        end if
        
        call parse_declaration_attributes(parser, arena, attr_info)
        
        ! Consume '::' and get variable name
        var_token = parser%peek()
        if (.not. (var_token%kind == TK_OPERATOR .and. var_token%text == "::")) then
            decl_index = push_literal(arena, "ERROR: Expected :: after type specification", LITERAL_STRING, type_spec%line, type_spec%column)
            return
        end if
        var_token = parser%consume()
        
        var_token = parser%peek()
        if (var_token%kind /= TK_IDENTIFIER) then
            decl_index = push_literal(arena, "ERROR: Expected variable name", LITERAL_STRING, type_spec%line, type_spec%column)
            return
        end if
        var_token = parser%consume()
        var_name = var_token%text
        
        ! Check for per-variable array dimensions
        var_token = parser%peek()
        is_array = (var_token%kind == TK_OPERATOR .and. var_token%text == "(")
        if (is_array) then
            var_token = parser%consume()
            call parse_array_dimensions(parser, arena, dimension_indices)
            var_token = parser%peek()
            if (var_token%kind == TK_OPERATOR .and. var_token%text == ")") then
                var_token = parser%consume()
            end if
        end if
        
        ! Check for initialization and collect variables
        var_token = parser%peek()
        has_initializer = (var_token%kind == TK_OPERATOR .and. var_token%text == "=")
        if (has_initializer) then
            var_token = parser%consume()
            initializer_index = parse_comparison(parser, arena)
        end if
        
        ! Check if multi-variable declaration
        var_token = parser%peek()
        if (var_token%kind == TK_OPERATOR .and. var_token%text == ",") then
            ! Multi-variable: collect all names and create nodes
            variables = collect_variable_names(parser, var_name)
            
            ! Set up declaration parameters
            params%type_name = type_spec%type_name
            params%has_kind = type_spec%has_kind
            params%kind_value = type_spec%kind_value
            params%is_allocatable = attr_info%is_allocatable
            params%is_pointer = attr_info%is_pointer
            params%is_target = attr_info%is_target
            params%is_parameter = attr_info%is_parameter
            params%is_optional = attr_info%is_optional
            params%has_intent = attr_info%has_intent
            if (allocated(attr_info%intent)) params%intent = attr_info%intent
            params%line = type_spec%line
            params%column = type_spec%column
            
            ! Determine final dimensions using helper function
            final_dims = determine_final_dimensions(is_array, dimension_indices, attr_info%has_global_dimensions, attr_info%global_dimension_indices)
            
            ! Create declaration nodes using helper function
            call create_declaration_nodes(arena, params, variables%names, final_dims, allocated(final_dims), decl_index)
        else
            ! Single variable: create single node with all attributes
            final_dims = determine_final_dimensions(is_array, dimension_indices, attr_info%has_global_dimensions, attr_info%global_dimension_indices)
            
            if (type_spec%has_kind .and. allocated(final_dims)) then
                if (attr_info%has_intent) then
                    decl_index = push_declaration(arena, type_spec%type_name, var_name, kind_value=type_spec%kind_value, &
                        initializer_index=initializer_index, dimension_indices=final_dims, &
                        is_allocatable=attr_info%is_allocatable, is_pointer=attr_info%is_pointer, &
                        is_target=attr_info%is_target, intent_value=attr_info%intent, &
                        is_optional=attr_info%is_optional, is_parameter=attr_info%is_parameter, &
                        line=type_spec%line, column=type_spec%column)
                else
                    decl_index = push_declaration(arena, type_spec%type_name, var_name, kind_value=type_spec%kind_value, &
                        initializer_index=initializer_index, dimension_indices=final_dims, &
                        is_allocatable=attr_info%is_allocatable, is_pointer=attr_info%is_pointer, &
                        is_target=attr_info%is_target, is_optional=attr_info%is_optional, &
                        is_parameter=attr_info%is_parameter, line=type_spec%line, column=type_spec%column)
                end if
            else if (type_spec%has_kind) then
                if (attr_info%has_intent) then
                    decl_index = push_declaration(arena, type_spec%type_name, var_name, kind_value=type_spec%kind_value, &
                        initializer_index=initializer_index, is_allocatable=attr_info%is_allocatable, &
                        is_pointer=attr_info%is_pointer, is_target=attr_info%is_target, &
                        intent_value=attr_info%intent, is_optional=attr_info%is_optional, &
                        is_parameter=attr_info%is_parameter, line=type_spec%line, column=type_spec%column)
                else
                    decl_index = push_declaration(arena, type_spec%type_name, var_name, kind_value=type_spec%kind_value, &
                        initializer_index=initializer_index, is_allocatable=attr_info%is_allocatable, &
                        is_pointer=attr_info%is_pointer, is_target=attr_info%is_target, &
                        is_optional=attr_info%is_optional, is_parameter=attr_info%is_parameter, &
                        line=type_spec%line, column=type_spec%column)
                end if
            else if (allocated(final_dims)) then
                if (attr_info%has_intent) then
                    decl_index = push_declaration(arena, type_spec%type_name, var_name, &
                        initializer_index=initializer_index, dimension_indices=final_dims, &
                        is_allocatable=attr_info%is_allocatable, is_pointer=attr_info%is_pointer, &
                        is_target=attr_info%is_target, intent_value=attr_info%intent, &
                        is_optional=attr_info%is_optional, is_parameter=attr_info%is_parameter, &
                        line=type_spec%line, column=type_spec%column)
                else
                    decl_index = push_declaration(arena, type_spec%type_name, var_name, &
                        initializer_index=initializer_index, dimension_indices=final_dims, &
                        is_allocatable=attr_info%is_allocatable, is_pointer=attr_info%is_pointer, &
                        is_target=attr_info%is_target, is_optional=attr_info%is_optional, &
                        is_parameter=attr_info%is_parameter, line=type_spec%line, column=type_spec%column)
                end if
            else
                if (attr_info%has_intent) then
                    decl_index = push_declaration(arena, type_spec%type_name, var_name, &
                        initializer_index=initializer_index, is_allocatable=attr_info%is_allocatable, &
                        is_pointer=attr_info%is_pointer, is_target=attr_info%is_target, &
                        intent_value=attr_info%intent, is_optional=attr_info%is_optional, &
                        is_parameter=attr_info%is_parameter, line=type_spec%line, column=type_spec%column)
                else
                    decl_index = push_declaration(arena, type_spec%type_name, var_name, &
                        initializer_index=initializer_index, is_allocatable=attr_info%is_allocatable, &
                        is_pointer=attr_info%is_pointer, is_target=attr_info%is_target, &
                        is_optional=attr_info%is_optional, is_parameter=attr_info%is_parameter, &
                        line=type_spec%line, column=type_spec%column)
                end if
            end if
        end if

    end function parse_declaration

    ! Parse array dimensions helper
    subroutine parse_array_dimensions(parser, arena, dimension_indices)
        use ast_factory, only: push_literal
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: dimension_indices(:)

        type(token_t) :: token
        integer :: dim_count
        character(len=:), allocatable :: dim_spec
        integer :: lower_bound_index, upper_bound_index
        integer :: line, column

        dim_count = 0
        allocate (dimension_indices(0))

        do
            token = parser%peek()
            line = token%line
            column = token%column

            ! Check for : (assumed shape)
            if (token%kind == TK_OPERATOR .and. token%text == ":") then
                token = parser%consume()

                ! Create assumed shape dimension node
                block
                    integer :: dim_index
                    dim_index = push_literal(arena, ":", LITERAL_STRING, line, column)
                    dimension_indices = [dimension_indices, dim_index]
                end block
            else if (token%kind == TK_OPERATOR .and. token%text == "*") then
                ! Assumed size (*)
                token = parser%consume()

                block
                    integer :: dim_index
                    dim_index = push_literal(arena, "*", LITERAL_STRING, line, column)
                    dimension_indices = [dimension_indices, dim_index]
                end block
            else
                ! Parse lower bound (if any) and upper bound

                ! Try to parse a dimension expression
                lower_bound_index = parse_range(parser, arena)

                ! Check if we have : for range specification
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ":") then
                    token = parser%consume()

                    ! Parse upper bound
                    upper_bound_index = parse_range(parser, arena)

                    ! For now, store the upper bound as the dimension
                    ! In a full implementation, we'd create a range node
                    dimension_indices = [dimension_indices, upper_bound_index]
                else
                    ! Single dimension
                    dimension_indices = [dimension_indices, lower_bound_index]
                end if
            end if

            ! Check for comma (more dimensions)
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ",") then
                token = parser%consume()
            else
                exit
            end if
        end do
    end subroutine parse_array_dimensions

    ! Parse derived type definition
    function parse_derived_type_def(parser, arena) result(type_index)
        use ast_factory, only: push_derived_type, push_literal, &
            push_identifier, push_declaration
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: type_index

        type(token_t) :: token
        character(len=:), allocatable :: type_name
        integer :: line, column
        integer, allocatable :: component_indices(:)
        integer :: component_count
        integer, allocatable :: param_indices(:)
        integer :: param_count
        logical :: has_parameters

        ! Consume "type" keyword
        token = parser%peek()
        if (token%kind == TK_KEYWORD .and. token%text == "type") then
            token = parser%consume()
        else
            ! Error: expected type keyword
            type_index = push_literal(arena, &
                "ERROR: Expected 'type' keyword", LITERAL_STRING, &
                token%line, token%column)
            return
        end if
        
        ! Consume "::" operator
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "::") then
            token = parser%consume()
        else
            ! Error: expected ::
            type_index = push_literal(arena, &
                "ERROR: Expected '::' after type", LITERAL_STRING, &
                token%line, token%column)
            return
        end if

        ! Get type name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            type_name = token%text
            line = token%line
            column = token%column
        else
            ! Error: expected type name
            type_index = push_literal(arena, &
                "ERROR: Expected type name", LITERAL_STRING, &
                token%line, token%column)
            return
        end if

        ! Check for parameters (e.g., type :: mytype(kind))
        has_parameters = .false.
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "(") then
            ! Consume '('
            token = parser%consume()
            has_parameters = .true.

            ! Parse parameters
            call parse_derived_type_parameters(parser, arena, param_indices, param_count)

            ! Consume ')'
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
            else
                ! Error: expected )
                type_index = push_literal(arena, &
                    "ERROR: Expected ) after type parameters", LITERAL_STRING, &
                    line, column)
                return
            end if
        end if

        ! Parse components
        component_count = 0
        allocate (component_indices(0))

        ! Parse until we find "end type"
        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for "end type"
            if (token%kind == TK_IDENTIFIER .and. token%text == "end") then
                ! Don't consume "end" yet, check if next is "type"
                block
                    type(parser_state_t) :: saved_state
                    saved_state = parser
                    token = parser%consume()  ! consume "end"
                    token = parser%peek()
                    if (token%kind == TK_IDENTIFIER .and. token%text == "type") then
                        token = parser%consume()  ! consume "type"
                        exit
                    else
                        ! Restore state before "end" was consumed
                        parser = saved_state
                    end if
                end block
            end if

            ! Parse component declaration
            block
                integer :: comp_index
                comp_index = parse_derived_type_component(parser, arena)
                if (comp_index > 0) then
                    component_indices = [component_indices, comp_index]
                    component_count = component_count + 1
                end if
            end block
        end do

        ! Create derived type node
        if (has_parameters) then
            type_index = push_derived_type(arena, type_name, &
                component_indices=component_indices, &
                param_indices=param_indices, &
                line=line, column=column)
        else
            type_index = push_derived_type(arena, type_name, &
                component_indices=component_indices, &
                line=line, column=column)
        end if

    end function parse_derived_type_def

    ! Parse derived type component
    function parse_derived_type_component(parser, arena) result(comp_index)
        use ast_factory, only: push_declaration
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: comp_index

        type(token_t) :: token

        ! Check if we have a type declaration
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            select case (token%text)
            case ("integer", "real", "complex", "logical", "character", "type")
                comp_index = parse_declaration(parser, arena)
            case default
                ! Skip unknown lines
                comp_index = 0
                do while (.not. parser%is_at_end())
                    token = parser%consume()
                    if (token%kind == TK_EOF) exit
                    ! Simple line ending detection
                    if (index(token%text, new_line('a')) > 0) exit
                end do
            end select
        else
            comp_index = 0
        end if

    end function parse_derived_type_component

    ! Parse derived type parameters helper
    subroutine parse_derived_type_parameters(parser, arena, param_indices, param_count)
        use ast_factory, only: push_identifier
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: param_indices(:)
        integer, intent(out) :: param_count

        type(token_t) :: token

        param_count = 0
        allocate (param_indices(0))

        do
            token = parser%peek()

            if (token%kind == TK_IDENTIFIER) then
                ! Parse parameter name
                token = parser%consume()

                ! Create parameter node
                block
                    integer :: param_index
              param_index = push_identifier(arena, token%text, token%line, token%column)
                    param_indices = [param_indices, param_index]
                    param_count = param_count + 1
                end block

                ! Check for comma
                token = parser%peek()
                if (token%kind == TK_OPERATOR .and. token%text == ",") then
                    token = parser%consume()
                else
                    exit
                end if
            else
                exit
            end if
        end do

    end subroutine parse_derived_type_parameters

    ! Parse multi-variable declaration (e.g., real :: a, b, c)
    function parse_multi_declaration(parser, arena) result(decl_indices)
        use ast_factory, only: push_declaration, push_multi_declaration, push_literal
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable :: decl_indices(:)

        type(token_t) :: type_token, var_token, token
        character(len=:), allocatable :: type_name
        integer :: line, column, kind_value
        logical :: has_kind, is_allocatable, is_pointer, is_target
        logical :: has_intent, is_optional, is_parameter
        character(len=:), allocatable :: intent
        integer :: decl_count, decl_index
        character(len=:), allocatable :: var_name
        logical :: is_array, has_global_dimensions
        integer, allocatable :: dimension_indices(:), global_dimension_indices(:)

        allocate (decl_indices(0))
        decl_count = 0

        ! Get type name (real, integer, etc.)
        type_token = parser%consume()
        type_name = type_token%text
        line = type_token%line
        column = type_token%column
        has_kind = .false.
        kind_value = 0
        is_allocatable = .false.
        is_pointer = .false.
        is_target = .false.
        has_intent = .false.
        is_optional = .false.
        has_global_dimensions = .false.
        is_parameter = .false.

        ! Check for kind specification (e.g., real(8))
        var_token = parser%peek()
        if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
            ! Consume '('
            type_token = parser%consume()

            ! Get kind value
            var_token = parser%peek()
            if (var_token%kind == TK_NUMBER) then
                var_token = parser%consume()
                read (var_token%text, *) kind_value
                has_kind = .true.

                ! Consume ')'
                var_token = parser%peek()
                if (var_token%kind == TK_OPERATOR .and. var_token%text == ")") then
                    type_token = parser%consume()
                else
                    ! Error: expected )
     decl_index = push_literal(arena, "ERROR: Expected )", LITERAL_STRING, line, column)
                    decl_indices = [decl_index]
                    return
                end if
            end if
        end if

        ! Check for attributes like allocatable
        ! Parse multiple attributes separated by commas
        do while (.not. parser%is_at_end())
            var_token = parser%peek()
            if (var_token%kind == TK_OPERATOR .and. var_token%text == ",") then
                ! Consume ','
                var_token = parser%consume()

                ! Parse attribute
                var_token = parser%peek()
                if ((var_token%kind == TK_IDENTIFIER .or. var_token%kind == TK_KEYWORD) .and. &
                    var_token%text == "allocatable") then
                    is_allocatable = .true.
                    var_token = parser%consume()
                else if ((var_token%kind == TK_IDENTIFIER .or. var_token%kind == TK_KEYWORD) .and. &
                         var_token%text == "pointer") then
                    is_pointer = .true.
                    var_token = parser%consume()
                else if (var_token%kind == TK_IDENTIFIER .and. &
                         var_token%text == "target") then
                    is_target = .true.
                    var_token = parser%consume()
                else if ((var_token%kind == TK_IDENTIFIER .or. var_token%kind == TK_KEYWORD) .and. &
                         var_token%text == "parameter") then
                    is_parameter = .true.
                    var_token = parser%consume()
                else if (var_token%kind == TK_IDENTIFIER .and. &
                         var_token%text == "dimension") then
                    has_global_dimensions = .true.
                    var_token = parser%consume()
                    
                    ! Parse dimension specification
                    var_token = parser%peek()
                    if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
                        var_token = parser%consume()  ! consume '('
                        
                        ! Parse dimensions
                        call parse_array_dimensions(parser, arena, global_dimension_indices)
                        
                        ! Consume ')'
                        var_token = parser%peek()
                        if (var_token%kind == TK_OPERATOR .and. var_token%text == ")") then
                            var_token = parser%consume()
                        end if
                    end if
                else if ((var_token%kind == TK_IDENTIFIER .or. var_token%kind == TK_KEYWORD) .and. &
                         var_token%text == "dimension") then
                    has_global_dimensions = .true.
                    var_token = parser%consume()
                    
                    ! Parse dimension specification
                    var_token = parser%peek()
                    if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
                        var_token = parser%consume()  ! consume '('
                        
                        ! Parse dimensions
                        call parse_array_dimensions(parser, arena, global_dimension_indices)
                        
                        ! Consume ')'
                        var_token = parser%peek()
                        if (var_token%kind == TK_OPERATOR .and. var_token%text == ")") then
                            var_token = parser%consume()
                        end if
                    end if
                else if ((var_token%kind == TK_IDENTIFIER .or. var_token%kind == TK_KEYWORD) .and. &
                         var_token%text == "intent") then
                    has_intent = .true.
                    var_token = parser%consume()  ! consume 'intent'
                    
                    ! Parse intent specification (in|out|inout) - simplified version  
                    var_token = parser%peek()
                    if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
                        var_token = parser%consume()  ! consume '('
                        
                        var_token = parser%peek()
                        if (var_token%kind == TK_IDENTIFIER .or. var_token%kind == TK_KEYWORD) then
                            intent = var_token%text
                            var_token = parser%consume()  ! consume intent value
                        end if
                        
                        var_token = parser%peek()
                        if (var_token%kind == TK_OPERATOR .and. &
                            var_token%text == ")") then
                            var_token = parser%consume()  ! consume ')'
                        end if
                    end if
                else if ((var_token%kind == TK_IDENTIFIER .or. var_token%kind == TK_KEYWORD) .and. &
                         var_token%text == "optional") then
                    is_optional = .true.
                    var_token = parser%consume()
                else
                    ! Unknown attribute - consume it and handle complex &
                    ! attributes like intent(out)
                    var_token = parser%consume()
                    
                    ! If next token is '(', consume until we find matching ')'
                    var_token = parser%peek()
                    if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
                        block
                            integer :: paren_count
                            paren_count = 1
                            var_token = parser%consume()  ! consume '('
                            
                            do while (paren_count > 0 .and. .not. parser%is_at_end())
                                var_token = parser%peek()
                                if (var_token%kind == TK_OPERATOR) then
                                    if (var_token%text == "(") then
                                        paren_count = paren_count + 1
                                    else if (var_token%text == ")") then
                                        paren_count = paren_count - 1
                                    end if
                                end if
                                var_token = parser%consume()
                            end do
                        end block
                    end if
                end if
            else
                ! No more attributes
                exit
            end if
        end do

        ! Consume '::'
        var_token = parser%peek()
        if (var_token%kind == TK_OPERATOR .and. var_token%text == "::") then
            type_token = parser%consume()
        else
            ! Error: expected ::
            decl_index = push_literal(arena, "ERROR: Expected ::", LITERAL_STRING, line, column)
            decl_indices = [decl_index]
            return
        end if

        ! Collect all variable names first
        block
            character(len=:), allocatable :: var_names_array(:)
            integer :: name_count
            character(len=100) :: temp_var_names(50)  ! Temporary storage
            
            name_count = 0
            
            ! Parse variable names
            do while (.not. parser%is_at_end())
                ! Get variable name
                var_token = parser%peek()
                if (var_token%kind == TK_IDENTIFIER) then
                    var_token = parser%consume()
                    name_count = name_count + 1
                    if (name_count <= 50) then
                        temp_var_names(name_count) = trim(var_token%text)
                    end if

                    ! For now, skip per-variable array dimensions in multi-declarations
                    ! This simplifies the implementation and matches common usage
                    var_token = parser%peek()
                    if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
                        ! Skip array dimensions for multi-var declarations
                        block
                            integer :: paren_count
                            paren_count = 1
                            var_token = parser%consume()  ! consume '('
                            
                            do while (paren_count > 0 .and. .not. parser%is_at_end())
                                var_token = parser%peek()
                                if (var_token%kind == TK_OPERATOR) then
                                    if (var_token%text == "(") then
                                        paren_count = paren_count + 1
                                    else if (var_token%text == ")") then
                                        paren_count = paren_count - 1
                                    end if
                                end if
                                var_token = parser%consume()
                            end do
                        end block
                    end if

                    ! Check for comma (more variables)
                    var_token = parser%peek()
                    if (var_token%kind == TK_OPERATOR .and. var_token%text == ",") then
                        var_token = parser%consume()
                    else
                        exit
                    end if
                else
                    ! Error: expected identifier
                    decl_index = push_literal(arena, "ERROR: Expected variable name", LITERAL_STRING, line, column)
                    decl_indices = [decl_index]
                    return
                end if
            end do
            
            ! Copy to properly sized array
            if (name_count > 0) then
                allocate(character(len=100) :: var_names_array(name_count))
                var_names_array(1:name_count) = temp_var_names(1:name_count)
                
                ! Create multiple individual declaration nodes
                deallocate(decl_indices)
                allocate(decl_indices(name_count))
                
                block
                    integer :: i
                    do i = 1, name_count
                    ! Determine final array status and dimensions for each variable
                    block
                        logical :: final_is_array
                        integer, allocatable :: final_dimension_indices(:)
                        
                        if (has_global_dimensions) then
                            final_is_array = .true.
                            final_dimension_indices = global_dimension_indices
                        else
                            final_is_array = .false.
                        end if
                    
                        ! Create individual declaration node for this variable
                        if (has_kind .and. final_is_array) then
                            if (has_intent) then
                                decl_indices(i) = push_declaration(arena, type_name, &
                                    trim(var_names_array(i)), kind_value=kind_value, &
                                    dimension_indices=final_dimension_indices, &
                                    is_allocatable=is_allocatable, is_pointer=is_pointer, &
                                    is_target=is_target, intent_value=intent, &
                                    is_optional=is_optional, is_parameter=is_parameter, &
                                    line=line, column=column)
                            else
                                decl_indices(i) = push_declaration(arena, type_name, &
                                    trim(var_names_array(i)), kind_value=kind_value, &
                                    dimension_indices=final_dimension_indices, &
                                    is_allocatable=is_allocatable, is_pointer=is_pointer, &
                                    is_target=is_target, is_optional=is_optional, &
                                    is_parameter=is_parameter, line=line, column=column)
                            end if
                        else if (has_kind) then
                            if (has_intent) then
                                decl_indices(i) = push_declaration(arena, type_name, &
                                    trim(var_names_array(i)), kind_value=kind_value, &
                                    is_allocatable=is_allocatable, is_pointer=is_pointer, &
                                    is_target=is_target, intent_value=intent, &
                                    is_optional=is_optional, is_parameter=is_parameter, &
                                    line=line, column=column)
                            else
                                decl_indices(i) = push_declaration(arena, type_name, &
                                    trim(var_names_array(i)), kind_value=kind_value, &
                                    is_allocatable=is_allocatable, is_pointer=is_pointer, &
                                    is_target=is_target, is_optional=is_optional, &
                                    is_parameter=is_parameter, line=line, column=column)
                            end if
                        else if (final_is_array) then
                            if (has_intent) then
                                decl_indices(i) = push_declaration(arena, type_name, &
                                    trim(var_names_array(i)), &
                                    dimension_indices=final_dimension_indices, &
                                    is_allocatable=is_allocatable, is_pointer=is_pointer, &
                                    is_target=is_target, intent_value=intent, &
                                    is_optional=is_optional, is_parameter=is_parameter, &
                                    line=line, column=column)
                            else
                                decl_indices(i) = push_declaration(arena, type_name, &
                                    trim(var_names_array(i)), &
                                    dimension_indices=final_dimension_indices, &
                                    is_allocatable=is_allocatable, is_pointer=is_pointer, &
                                    is_target=is_target, is_optional=is_optional, &
                                    is_parameter=is_parameter, line=line, column=column)
                            end if
                        else
                            if (has_intent) then
                                decl_indices(i) = push_declaration(arena, type_name, &
                                    trim(var_names_array(i)), &
                                    is_allocatable=is_allocatable, is_pointer=is_pointer, &
                                    is_target=is_target, intent_value=intent, &
                                    is_optional=is_optional, is_parameter=is_parameter, &
                                    line=line, column=column)
                            else
                                decl_indices(i) = push_declaration(arena, type_name, &
                                    trim(var_names_array(i)), &
                                    is_allocatable=is_allocatable, is_pointer=is_pointer, &
                                    is_target=is_target, is_optional=is_optional, &
                                    is_parameter=is_parameter, line=line, column=column)
                            end if
                        end if
                    end block
                    end do
                end block
            else
                ! No variables found
                decl_index = push_literal(arena, "ERROR: No variables in declaration", LITERAL_STRING, line, column)
                decl_indices = [decl_index]
            end if
        end block

    end function parse_multi_declaration

end module parser_declarations