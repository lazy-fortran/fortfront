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

contains

    ! Parse single-variable declaration (e.g., real :: x)
    function parse_declaration(parser, arena) result(decl_index)
        use ast_factory, only: push_declaration, push_literal, push_identifier
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: decl_index

        type(token_t) :: type_token, var_token
        character(len=:), allocatable :: type_name, var_name
        integer :: line, column, kind_value
        logical :: has_kind, is_array, is_allocatable, is_pointer, is_target
        logical :: has_intent, is_optional, has_global_dimensions
        character(len=:), allocatable :: intent
        integer, allocatable :: dimension_indices(:), global_dimension_indices(:)

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

        ! Check for kind specification (e.g., real(8)) or derived type (e.g., &
        ! type(point))
        var_token = parser%peek()
        if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
            ! Consume '('
            type_token = parser%consume()

            ! Get what's inside the parentheses
            var_token = parser%peek()
            if (var_token%kind == TK_NUMBER) then
                ! This is a kind specification like real(8)
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
                    return
                end if
            else if (var_token%kind == TK_IDENTIFIER) then
                ! This could be a derived type like type(point) or character(len=20)
                ! For now, we'll capture the full type specification as text
                block
                    character(len=:), allocatable :: type_spec
                    integer :: paren_count

                    type_spec = var_token%text
                    var_token = parser%consume()

                    ! Check for more complex specifications like character(len=*)
                    paren_count = 1
                    do while (paren_count > 0 .and. .not. parser%is_at_end())
                        var_token = parser%peek()
                        if (var_token%kind == TK_OPERATOR) then
                            if (var_token%text == "(") then
                                paren_count = paren_count + 1
                            else if (var_token%text == ")") then
                                paren_count = paren_count - 1
                            end if
                        end if

                        ! Don't consume the final closing parenthesis
                        if (paren_count > 0) then
                            var_token = parser%consume()
                            type_spec = type_spec//var_token%text
                        end if
                    end do

                    type_name = type_name//"("//type_spec//")"
                end block

                ! Consume ')'
                var_token = parser%peek()
                if (var_token%kind == TK_OPERATOR .and. var_token%text == ")") then
                    type_token = parser%consume()
                else
                    ! Error: expected )
     decl_index = push_literal(arena, "ERROR: Expected )", LITERAL_STRING, line, column)
                    return
                end if
            else
                ! Error: expected number or identifier
                decl_index = push_literal(arena, &
                    "ERROR: Expected kind value or type name", LITERAL_STRING, &
                    line, column)
                return
            end if
        end if

        ! Check for attributes like allocatable (e.g., "real, allocatable :: arr")
        ! Parse multiple attributes separated by commas
        do while (.not. parser%is_at_end())
            var_token = parser%peek()
            if (var_token%kind == TK_OPERATOR .and. var_token%text == ",") then
                ! Consume ','
                var_token = parser%consume()

                ! Parse attribute (handle allocatable, pointer, target, and intent)
                var_token = parser%peek()
                if (var_token%kind == TK_IDENTIFIER .and. &
                    var_token%text == "allocatable") then
                    is_allocatable = .true.
                    var_token = parser%consume()
                else if (var_token%kind == TK_IDENTIFIER .and. &
                         var_token%text == "pointer") then
                    is_pointer = .true.
                    var_token = parser%consume()
                else if (var_token%kind == TK_IDENTIFIER .and. &
                         var_token%text == "target") then
                    is_target = .true.
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
            decl_index = push_literal(arena, &
                "ERROR: Expected :: after type specification", LITERAL_STRING, &
                line, column)
            return
        end if

        ! Get variable name
        var_token = parser%peek()
        if (var_token%kind == TK_IDENTIFIER) then
            var_token = parser%consume()
            var_name = var_token%text
        else
            ! Error: expected identifier
            decl_index = push_literal(arena, &
                "ERROR: Expected variable name", LITERAL_STRING, &
                line, column)
            return
        end if

        ! Check for array dimensions (e.g., real :: arr(10))
        var_token = parser%peek()
        is_array = .false.
        if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
            ! Consume '('
            var_token = parser%consume()
            is_array = .true.

            ! Parse dimensions
            call parse_array_dimensions(parser, arena, dimension_indices)

            ! Consume ')'
            var_token = parser%peek()
            if (var_token%kind == TK_OPERATOR .and. var_token%text == ")") then
                var_token = parser%consume()
            else
                ! Error: expected )
                decl_index = push_literal(arena, &
                    "ERROR: Expected ) after array dimensions", LITERAL_STRING, &
                    line, column)
                return
            end if
        end if

        ! Create the first declaration node
        ! Check for initialization (= expression)
        block
            integer :: initializer_index
            var_token = parser%peek()
            initializer_index = 0

            if (var_token%kind == TK_OPERATOR .and. var_token%text == "=") then
                ! Consume '='
                var_token = parser%consume()
                ! Parse the initializer expression
                initializer_index = parse_comparison(parser, arena)
                ! Debug output
                ! print *, "DEBUG: Parsed initializer, index =", initializer_index
            else
                ! print *, "DEBUG: No initializer found"
            end if

            ! Create declaration node with or without initializer
            if (has_kind .and. is_array) then
                if (has_intent) then
                    decl_index = push_declaration(arena, type_name, var_name, &
                        kind_value=kind_value, &
                        initializer_index=initializer_index, &
                        dimension_indices=dimension_indices, &
                        is_allocatable=is_allocatable, is_pointer=is_pointer, &
                        is_target=is_target, intent_value=intent, &
                        is_optional=is_optional, line=line, column=column)
                else
                    decl_index = push_declaration(arena, type_name, var_name, &
                        kind_value=kind_value, &
                        initializer_index=initializer_index, &
                        dimension_indices=dimension_indices, &
                        is_allocatable=is_allocatable, is_pointer=is_pointer, &
                        is_target=is_target, is_optional=is_optional, &
                        line=line, column=column)
                end if
            else if (has_kind) then
                if (has_intent) then
                    decl_index = push_declaration(arena, type_name, var_name, &
                        kind_value=kind_value, initializer_index=initializer_index, &
                        is_allocatable=is_allocatable, is_pointer=is_pointer, &
                        is_target=is_target, intent_value=intent, &
                        is_optional=is_optional, line=line, column=column)
                else
                    decl_index = push_declaration(arena, type_name, var_name, &
                        kind_value=kind_value, initializer_index=initializer_index, &
                        is_allocatable=is_allocatable, is_pointer=is_pointer, &
                        is_target=is_target, is_optional=is_optional, &
                        line=line, column=column)
                end if
            else if (is_array) then
                if (has_intent) then
                    decl_index = push_declaration(arena, type_name, var_name, &
                        initializer_index=initializer_index, &
                        dimension_indices=dimension_indices, &
                        is_allocatable=is_allocatable, is_pointer=is_pointer, &
                        is_target=is_target, intent_value=intent, &
                        is_optional=is_optional, line=line, column=column)
                else
                    decl_index = push_declaration(arena, type_name, var_name, &
                        initializer_index=initializer_index, &
                        dimension_indices=dimension_indices, &
                        is_allocatable=is_allocatable, is_pointer=is_pointer, &
                        is_target=is_target, is_optional=is_optional, &
                        line=line, column=column)
                end if
            else
                if (has_intent) then
                    decl_index = push_declaration(arena, type_name, var_name, &
                        initializer_index=initializer_index, &
                        is_allocatable=is_allocatable, is_pointer=is_pointer, &
                        is_target=is_target, intent_value=intent, &
                        is_optional=is_optional, line=line, column=column)
                else
                    decl_index = push_declaration(arena, type_name, var_name, &
                        initializer_index=initializer_index, &
                        is_allocatable=is_allocatable, is_pointer=is_pointer, &
                        is_target=is_target, is_optional=is_optional, &
                        line=line, column=column)
                end if
            end if
        end block

        ! Handle additional variables in multi-variable declarations like &
        ! "real :: a, b, c"
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
                    var_name = var_token%text

                    ! Create additional declaration node for this variable
                    ! Use same type information as the first variable
                    block
                        integer :: additional_decl_index
                        if (has_kind .and. is_array) then
                            if (has_intent) then
                                additional_decl_index = push_declaration( &
                                    arena, type_name, var_name, &
                                    kind_value=kind_value, &
                                    dimension_indices=dimension_indices, &
                                    is_allocatable=is_allocatable, &
                                    is_pointer=is_pointer, &
                                    is_target=is_target, intent_value=intent, &
                                    is_optional=is_optional, &
                                    line=line, column=column)
                            else
                                additional_decl_index = push_declaration( &
                                    arena, type_name, var_name, &
                                    kind_value=kind_value, &
                                    dimension_indices=dimension_indices, &
                                    is_allocatable=is_allocatable, &
                                    is_pointer=is_pointer, &
                                    is_target=is_target, &
                                    is_optional=is_optional, &
                                    line=line, column=column)
                            end if
                        else if (has_kind) then
                            if (has_intent) then
                                additional_decl_index = push_declaration( &
                                    arena, type_name, var_name, &
                                    kind_value=kind_value, &
                                    is_allocatable=is_allocatable, &
                                    is_pointer=is_pointer, &
                                    is_target=is_target, &
                                    intent_value=intent, &
                                    is_optional=is_optional, &
                                    line=line, column=column)
                            else
                                additional_decl_index = push_declaration( &
                                    arena, type_name, var_name, &
                                    kind_value=kind_value, &
                                    is_allocatable=is_allocatable, &
                                    is_pointer=is_pointer, &
                                    is_target=is_target, &
                                    is_optional=is_optional, &
                                    line=line, column=column)
                            end if
                        else if (is_array) then
                            if (has_intent) then
                                additional_decl_index = push_declaration( &
                                    arena, type_name, var_name, &
                                    dimension_indices=dimension_indices, &
                                    is_allocatable=is_allocatable, &
                                    is_pointer=is_pointer, &
                                    is_target=is_target, &
                                    intent_value=intent, &
                                    is_optional=is_optional, &
                                    line=line, column=column)
                            else
                                additional_decl_index = push_declaration( &
                                    arena, type_name, var_name, &
                                    dimension_indices=dimension_indices, &
                                    is_allocatable=is_allocatable, &
                                    is_pointer=is_pointer, &
                                    is_target=is_target, &
                                    is_optional=is_optional, &
                                    line=line, column=column)
                            end if
                        else
                            if (has_intent) then
                                additional_decl_index = push_declaration( &
                                    arena, type_name, var_name, &
                                    is_allocatable=is_allocatable, &
                                    is_pointer=is_pointer, &
                                    is_target=is_target, &
                                    intent_value=intent, &
                                    is_optional=is_optional, &
                                    line=line, column=column)
                            else
                                additional_decl_index = push_declaration( &
                                    arena, type_name, var_name, &
                                    is_allocatable=is_allocatable, &
                                    is_pointer=is_pointer, &
                                    is_target=is_target, &
                                    is_optional=is_optional, &
                                    line=line, column=column)
                            end if
                        end if

                        ! Note: Currently the function only returns the first &
                        ! declaration index
                        ! This means the additional declarations are created in &
                        ! the arena but
                        ! the caller only gets the first one. This is a limitation &
                        ! of the current
                        ! interface, but the declarations are still created and &
                        ! available in the arena.
                    end block
                else
                    ! Error: expected identifier after comma
                    decl_index = push_literal(arena, "ERROR: Expected variable name after comma", LITERAL_STRING, line, column)
                    exit
                end if
            else
                ! No more variables, exit without consuming
                exit
            end if
        end do

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
        logical :: has_intent, is_optional
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
        has_global_dimensions = .false.

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
                if (var_token%kind == TK_IDENTIFIER .and. &
                    var_token%text == "allocatable") then
                    is_allocatable = .true.
                    var_token = parser%consume()
                else if (var_token%kind == TK_IDENTIFIER .and. &
                         var_token%text == "pointer") then
                    is_pointer = .true.
                    var_token = parser%consume()
                else if (var_token%kind == TK_IDENTIFIER .and. &
                         var_token%text == "target") then
                    is_target = .true.
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

        ! Parse variable names
        do while (.not. parser%is_at_end())
            ! Get variable name
            var_token = parser%peek()
            if (var_token%kind == TK_IDENTIFIER) then
                var_token = parser%consume()
                var_name = var_token%text

                ! Check for array dimensions (per-variable) or use global dimensions
                var_token = parser%peek()
                is_array = has_global_dimensions  ! Start with global dimensions
                
                if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
                    ! Consume '('
                    var_token = parser%consume()
                    is_array = .true.

                    ! Parse per-variable dimensions (overrides global)
                    call parse_array_dimensions(parser, arena, dimension_indices)

                    ! Consume ')'
                    var_token = parser%peek()
                    if (var_token%kind == TK_OPERATOR .and. var_token%text == ")") then
                        var_token = parser%consume()
                    else
                        ! Error: expected )
                        decl_index = push_literal(arena, "ERROR: Expected )", LITERAL_STRING, line, column)
                        decl_indices = [decl_index]
                        return
                    end if
                else if (has_global_dimensions) then
                    ! Use global dimensions
                    dimension_indices = global_dimension_indices
                end if

                ! Create declaration node - use push_declaration for individual variables
                if (has_kind .and. is_array) then
                    if (has_intent) then
                        decl_index = push_declaration(arena, type_name, var_name, &
                            kind_value=kind_value, dimension_indices=dimension_indices, &
                            is_allocatable=is_allocatable, is_pointer=is_pointer, &
                            is_target=is_target, intent_value=intent, &
                            is_optional=is_optional, line=line, column=column)
                    else
                        decl_index = push_declaration(arena, type_name, var_name, &
                            kind_value=kind_value, dimension_indices=dimension_indices, &
                            is_allocatable=is_allocatable, is_pointer=is_pointer, &
                            is_target=is_target, is_optional=is_optional, &
                            line=line, column=column)
                    end if
                else if (has_kind) then
                    if (has_intent) then
                        decl_index = push_declaration(arena, type_name, var_name, &
                            kind_value=kind_value, is_allocatable=is_allocatable, &
                            is_pointer=is_pointer, is_target=is_target, &
                            intent_value=intent, is_optional=is_optional, &
                            line=line, column=column)
                    else
                        decl_index = push_declaration(arena, type_name, var_name, &
                            kind_value=kind_value, is_allocatable=is_allocatable, &
                            is_pointer=is_pointer, is_target=is_target, &
                            is_optional=is_optional, line=line, column=column)
                    end if
                else if (is_array) then
                    if (has_intent) then
                        decl_index = push_declaration(arena, type_name, var_name, &
                            dimension_indices=dimension_indices, is_allocatable=is_allocatable, &
                            is_pointer=is_pointer, is_target=is_target, &
                            intent_value=intent, is_optional=is_optional, &
                            line=line, column=column)
                    else
                        decl_index = push_declaration(arena, type_name, var_name, &
                            dimension_indices=dimension_indices, is_allocatable=is_allocatable, &
                            is_pointer=is_pointer, is_target=is_target, &
                            is_optional=is_optional, line=line, column=column)
                    end if
                else
                    if (has_intent) then
                        decl_index = push_declaration(arena, type_name, var_name, &
                            is_allocatable=is_allocatable, is_pointer=is_pointer, &
                            is_target=is_target, intent_value=intent, &
                            is_optional=is_optional, line=line, column=column)
                    else
                        decl_index = push_declaration(arena, type_name, var_name, &
                            is_allocatable=is_allocatable, is_pointer=is_pointer, &
                            is_target=is_target, is_optional=is_optional, &
                            line=line, column=column)
                    end if
                end if

                decl_indices = [decl_indices, decl_index]
                decl_count = decl_count + 1

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

    end function parse_multi_declaration

end module parser_declarations