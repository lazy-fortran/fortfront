module parser_declarations_module
    use iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF, TK_NUMBER, TK_STRING, TK_IDENTIFIER, &
                          TK_OPERATOR, TK_KEYWORD
    use ast_core
    use ast_factory, only: push_literal, push_identifier, push_binary_op, &
                           push_derived_type, push_declaration, push_multi_declaration
    use parser_state_module, only: parser_state_t
    use parser_expressions_module, only: parse_comparison
    implicit none
    private

    ! Public declaration parsing interface
    public :: parse_declaration, parse_derived_type, parse_multi_declaration

contains

    ! Parse a variable declaration
    function parse_declaration(parser, arena) result(decl_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: decl_index

        type(token_t) :: type_token, var_token
        character(len=:), allocatable :: type_name, var_name
        integer :: line, column, kind_value
        logical :: has_kind, is_array, is_allocatable, has_intent
        character(len=:), allocatable :: intent
        integer, allocatable :: dimension_indices(:)

        ! Get type name (real, integer, etc.)
        type_token = parser%consume()
        type_name = type_token%text
        line = type_token%line
        column = type_token%column
        has_kind = .false.
        kind_value = 0
        is_allocatable = .false.
        has_intent = .false.

        ! Check for kind specification (e.g., real(8)) or derived type (e.g., type(point))
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
                block
                    character(len=:), allocatable :: type_spec
                    type_spec = var_token%text
                    var_token = parser%consume()

                    ! Check for more complex type specifications like len=20
                    var_token = parser%peek()
                    if (var_token%kind == TK_OPERATOR .and. var_token%text == "=") then
                        ! Handle len=20 syntax
                        var_token = parser%consume()  ! consume '='
                        type_spec = type_spec//"="

                        var_token = parser%peek()
                        if (var_token%kind == TK_NUMBER) then
                            var_token = parser%consume()
                            type_spec = type_spec//var_token%text
                        else if (var_token%kind == TK_IDENTIFIER) then
                            ! Handle kind=real64, kind=int32, etc.
                            var_token = parser%consume()
                            type_spec = type_spec//var_token%text
                        else if (var_token%kind == TK_OPERATOR .and. &
                                 var_token%text == ":") then
                            ! Handle len=: for deferred length
                            var_token = parser%consume()
                            type_spec = type_spec//":"
                        else if (var_token%kind == TK_OPERATOR .and. &
                                 var_token%text == "*") then
                            ! Handle len=* for assumed length
                            var_token = parser%consume()
                            type_spec = type_spec//"*"
                        end if
                    end if

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
                    "ERROR: Expected kind value or type name", LITERAL_STRING, line, column)
                return
            end if
        end if

        ! Check for attributes like allocatable (e.g., "real, allocatable :: arr")
        var_token = parser%peek()
        if (var_token%kind == TK_OPERATOR .and. var_token%text == ",") then
            ! Consume ','
            var_token = parser%consume()

            ! Parse attribute (handle allocatable and intent)
            var_token = parser%peek()
            if (var_token%kind == TK_IDENTIFIER .and. var_token%text == "allocatable") then
                is_allocatable = .true.
                var_token = parser%consume()
            else if (var_token%kind == TK_IDENTIFIER .and. var_token%text == "intent") then
                has_intent = .true.
                var_token = parser%consume()  ! consume 'intent'
                
                ! Parse intent specification (in|out|inout) - simplified version  
                var_token = parser%peek()
                if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
                    var_token = parser%consume()  ! consume '('
                    
                    var_token = parser%peek()
                    if (var_token%kind == TK_IDENTIFIER) then
                        intent = var_token%text
                        var_token = parser%consume()  ! consume intent value
                    end if
                    
                    var_token = parser%peek()
                    if (var_token%kind == TK_OPERATOR .and. var_token%text == ")") then
                        var_token = parser%consume()  ! consume ')'
                    end if
                end if
            else
                ! Unknown attribute - consume it and handle complex attributes like intent(out)
                var_token = parser%consume()
                
                ! If next token is '(', consume until we find matching ')'
                var_token = parser%peek()
                if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
                    var_token = parser%consume()  ! consume '('
                    
                    ! Consume tokens until we find ')'
                    do while (.not. parser%is_at_end())
                        var_token = parser%peek()
                        if (var_token%kind == TK_OPERATOR .and. var_token%text == ")") then
                            var_token = parser%consume()  ! consume ')'
                            exit
                        end if
                        var_token = parser%consume()  ! consume whatever token
                    end do
                end if
            end if
        end if

        ! Consume ::
        var_token = parser%peek()
        if (var_token%kind == TK_OPERATOR .and. var_token%text == "::") then
            type_token = parser%consume()
        else
            ! Error: expected ::
    decl_index = push_literal(arena, "ERROR: Expected ::", LITERAL_STRING, line, column)
            return
        end if

        ! Get variable name and create identifier node
        var_token = parser%peek()
        if (var_token%kind == TK_IDENTIFIER) then
            var_token = parser%consume()
            var_name = var_token%text
            ! Create identifier node for the variable name
            block
                integer :: var_id_index
                var_id_index = push_identifier(arena, var_name, var_token%line, var_token%column)
            end block
        else
            ! Error: expected identifier
            decl_index = push_literal(arena, "ERROR: Expected identifier", LITERAL_STRING, line, column)
            return
        end if

        ! Check for array dimensions (e.g., (10), (:), (1:10))
        is_array = .false.
        var_token = parser%peek()
        if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
            is_array = .true.
            ! Consume '('
            var_token = parser%consume()

            ! Parse dimensions - for now, just handle simple cases
            call parse_array_dimensions(parser, arena, dimension_indices)

            ! Consume ')'
            var_token = parser%peek()
            if (var_token%kind == TK_OPERATOR .and. var_token%text == ")") then
                var_token = parser%consume()
            else
                ! Error: expected )
                decl_index = push_literal(arena, "ERROR: Expected ) after array dimensions", LITERAL_STRING, line, column)
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
            end if

            ! Create declaration node with or without initializer
            if (has_kind .and. is_array) then
                if (has_intent) then
                    decl_index = push_declaration(arena, type_name, var_name, &
                               kind_value=kind_value, initializer_index=initializer_index, &
                       dimension_indices=dimension_indices, is_allocatable=is_allocatable, &
                                                  intent_value=intent, line=line, column=column)
                else
                    decl_index = push_declaration(arena, type_name, var_name, &
                               kind_value=kind_value, initializer_index=initializer_index, &
                       dimension_indices=dimension_indices, is_allocatable=is_allocatable, &
                                                  line=line, column=column)
                end if
            else if (has_kind) then
                if (has_intent) then
                    decl_index = push_declaration(arena, type_name, var_name, &
                               kind_value=kind_value, initializer_index=initializer_index, &
                               is_allocatable=is_allocatable, intent_value=intent, line=line, column=column)
                else
                    decl_index = push_declaration(arena, type_name, var_name, &
                               kind_value=kind_value, initializer_index=initializer_index, &
                                    is_allocatable=is_allocatable, line=line, column=column)
                end if
            else if (is_array) then
                if (has_intent) then
                    decl_index = push_declaration(arena, type_name, var_name, &
                 initializer_index=initializer_index, dimension_indices=dimension_indices, &
                                    is_allocatable=is_allocatable, intent_value=intent, line=line, column=column)
                else
                    decl_index = push_declaration(arena, type_name, var_name, &
                 initializer_index=initializer_index, dimension_indices=dimension_indices, &
                                    is_allocatable=is_allocatable, line=line, column=column)
                end if
            else
                if (has_intent) then
                    decl_index = push_declaration(arena, type_name, var_name, &
                       initializer_index=initializer_index, is_allocatable=is_allocatable, &
                                                  intent_value=intent, line=line, column=column)
                else
                    decl_index = push_declaration(arena, type_name, var_name, &
                       initializer_index=initializer_index, is_allocatable=is_allocatable, &
                                                  line=line, column=column)
                end if
            end if
        end block

        ! Handle additional variables in multi-variable declarations like "real :: a, b, c"
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
                  additional_decl_index = push_declaration(arena, type_name, var_name, &
                           kind_value=kind_value, dimension_indices=dimension_indices, &
                                is_allocatable=is_allocatable, line=line, column=column)
                        else if (has_kind) then
                  additional_decl_index = push_declaration(arena, type_name, var_name, &
                                 kind_value=kind_value, is_allocatable=is_allocatable, &
                                                               line=line, column=column)
                        else if (is_array) then
                  additional_decl_index = push_declaration(arena, type_name, var_name, &
                   dimension_indices=dimension_indices, is_allocatable=is_allocatable, &
                                                               line=line, column=column)
                        else
                  additional_decl_index = push_declaration(arena, type_name, var_name, &
                                is_allocatable=is_allocatable, line=line, column=column)
                        end if

                        ! Note: Currently the function only returns the first declaration index
                        ! This means the additional declarations are created in the arena but
                        ! the caller only gets the first one. This is a limitation of the current
                        ! interface, but the declarations are still created and available in the arena.
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
                    dim_count = dim_count + 1
                end block

            else
                ! Parse lower bound expression
                lower_bound_index = parse_comparison(parser, arena)

                if (lower_bound_index > 0) then
                    token = parser%peek()

                    ! Check for : (range)
                    if (token%kind == TK_OPERATOR .and. token%text == ":") then
                        token = parser%consume()

                        ! Parse upper bound
                        upper_bound_index = parse_comparison(parser, arena)

                        if (upper_bound_index > 0) then
                            ! Create range dimension (lower:upper)
                            block
                                integer :: dim_index
                                dim_index = push_binary_op(arena, lower_bound_index, upper_bound_index, ":", line, column)
                                dimension_indices = [dimension_indices, dim_index]
                                dim_count = dim_count + 1
                            end block
                        end if
                    else
                        ! Single dimension (just the size)
                        dimension_indices = [dimension_indices, lower_bound_index]
                        dim_count = dim_count + 1
                    end if
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

    ! Parse derived type definition: type :: type_name or type(params) :: type_name
    function parse_derived_type(parser, arena) result(type_index)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: type_index

        type(token_t) :: token
        character(len=:), allocatable :: type_name
        integer, allocatable :: component_indices(:), param_indices(:)
        integer :: line, column

        ! Already consumed 'type' keyword
        token = parser%peek()
        line = token%line
        column = token%column

        ! Check for optional :: or (
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == "::") then
            ! Simple form: type :: name
            token = parser%consume()
        else if (token%kind == TK_OPERATOR .and. token%text == "(") then
            ! Parameterized form: type(params) :: name
            token = parser%consume()  ! consume '('

            ! Parse parameters
            call parse_derived_type_parameters(parser, arena, param_indices)

            ! Consume ')'
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
            end if

            ! Consume '::'
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == "::") then
                token = parser%consume()
            end if
        else if (token%kind == TK_IDENTIFIER) then
            ! Alternative syntax without :: (type point)
            ! Do nothing, just proceed to get the type name
        end if

        ! Get type name
        token = parser%peek()
        if (token%kind == TK_IDENTIFIER) then
            token = parser%consume()
            type_name = token%text
        else
            ! Error: expected type name
            type_index = push_literal(arena, "ERROR: Expected type name", LITERAL_STRING, line, column)
            return
        end if

        ! Parse type components until 'end type'
        ! For now, just create the type node
        if (allocated(param_indices)) then
            type_index = push_derived_type(arena, type_name, component_indices, param_indices, line, column)
        else
            allocate (component_indices(0))
            type_index = push_derived_type(arena, type_name, component_indices, line=line, column=column)
        end if

    end function parse_derived_type

    ! Parse derived type parameters
    subroutine parse_derived_type_parameters(parser, arena, param_indices)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable, intent(out) :: param_indices(:)

        type(token_t) :: token
        integer :: param_count

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
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable :: decl_indices(:)

        type(token_t) :: type_token, var_token, token
        character(len=:), allocatable :: type_name
        integer :: line, column, kind_value
        logical :: has_kind, is_allocatable, has_intent
        character(len=:), allocatable :: intent
        integer :: decl_count, decl_index
        character(len=:), allocatable :: var_name
        logical :: is_array
        integer, allocatable :: dimension_indices(:)

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
        has_intent = .false.

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
        var_token = parser%peek()
        if (var_token%kind == TK_OPERATOR .and. var_token%text == ",") then
            ! Consume ','
            var_token = parser%consume()

            ! Parse attribute
            var_token = parser%peek()
            if (var_token%kind == TK_IDENTIFIER .and. var_token%text == "allocatable") then
                is_allocatable = .true.
                var_token = parser%consume()
            else if (var_token%kind == TK_IDENTIFIER .and. var_token%text == "intent") then
                var_token = parser%consume()  ! consume 'intent'
                
                ! Parse intent specification (in|out|inout) - simplified version
                var_token = parser%peek()
                if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
                    var_token = parser%consume()  ! consume '('
                    
                    var_token = parser%peek()
                    if (var_token%kind == TK_IDENTIFIER) then
                        var_token = parser%consume()  ! consume intent value
                    end if
                    
                    var_token = parser%peek()
                    if (var_token%kind == TK_OPERATOR .and. var_token%text == ")") then
                        var_token = parser%consume()  ! consume ')'
                    end if
                end if
            else
                ! Unknown attribute - consume it and handle complex attributes like intent(out)
                var_token = parser%consume()
                
                ! If next token is '(', consume until we find matching ')'
                var_token = parser%peek()
                if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
                    var_token = parser%consume()  ! consume '('
                    
                    ! Consume tokens until we find ')'
                    do while (.not. parser%is_at_end())
                        var_token = parser%peek()
                        if (var_token%kind == TK_OPERATOR .and. var_token%text == ")") then
                            var_token = parser%consume()  ! consume ')'
                            exit
                        end if
                        var_token = parser%consume()  ! consume whatever token
                    end do
                end if
            end if
        end if

        ! Consume ::
        var_token = parser%peek()
        if (var_token%kind == TK_OPERATOR .and. var_token%text == "::") then
            type_token = parser%consume()
        else
            ! Error: expected ::
    decl_index = push_literal(arena, "ERROR: Expected ::", LITERAL_STRING, line, column)
            decl_indices = [decl_index]
            return
        end if

        ! Parse variable list - collect all variable names first
        block
            character(len=100) :: temp_var_names(20)  ! Fixed size array
            integer :: var_count, i, j
            logical :: has_complex_attributes
            type :: var_dims_t
                integer, allocatable :: dimension_indices(:)
            end type var_dims_t
            type(var_dims_t) :: var_dimensions(20)
            
            ! Initialize
            var_count = 0
            has_complex_attributes = .false.
            
            do
                ! Get variable name
                var_token = parser%peek()
                if (var_token%kind == TK_IDENTIFIER) then
                    var_token = parser%consume()
                    var_name = var_token%text
                    
                    ! Add to variable names list
                    var_count = var_count + 1
                    if (var_count <= 20) then
                        temp_var_names(var_count) = var_name
                    else
                        ! Too many variables - error
                        decl_indices = [push_literal(arena, &
                            "ERROR: Too many variables in declaration", &
                            LITERAL_STRING, line, column)]
                        return
                    end if
                else
                    exit  ! No more variables
                end if

                ! Check for complex attributes (array dimensions or initializers)
                var_token = parser%peek()
                if (var_token%kind == TK_OPERATOR .and. (var_token%text == "(" .or. var_token%text == "=")) then
                    has_complex_attributes = .true.
                end if

                ! Handle array dimensions for this variable
                if (var_token%kind == TK_OPERATOR .and. var_token%text == "(") then
                    var_token = parser%consume()  ! consume '('
                    
                    ! Parse array dimensions for this variable
                    call parse_array_dimensions(parser, arena, var_dimensions(var_count)%dimension_indices)
                    
                    ! Consume ')' if not already consumed
                    var_token = parser%peek()
                    if (var_token%kind == TK_OPERATOR .and. var_token%text == ")") then
                        var_token = parser%consume()  ! consume ')'
                    end if
                end if

                if (var_token%kind == TK_OPERATOR .and. var_token%text == "=") then
                    ! Skip initializer for now
                    var_token = parser%consume()  ! consume '='
                    ! Skip until comma or end
                    do while (.not. parser%is_at_end())
                        var_token = parser%peek()
                        if (var_token%kind == TK_OPERATOR .and. var_token%text == ",") then
                            exit
                        end if
                        if (var_token%kind == TK_EOF) exit
                        var_token = parser%consume()
                    end do
                end if

                ! Check for comma (more variables)
                var_token = parser%peek()
                if (var_token%kind == TK_OPERATOR .and. var_token%text == ",") then
                    ! Consume ','
                    var_token = parser%consume()
                else
                    exit  ! No more variables
                end if
            end do
            
            ! Create appropriate declaration node
            if (var_count == 1) then
                ! Single variable - use regular declaration
                if (has_kind) then
                    decl_index = push_declaration(arena, type_name, trim(temp_var_names(1)), &
                               kind_value=kind_value, is_allocatable=is_allocatable, &
                                                  line=line, column=column)
                else
                    decl_index = push_declaration(arena, type_name, trim(temp_var_names(1)), &
                                                  is_allocatable=is_allocatable, &
                                                  line=line, column=column)
                end if
                decl_indices = [decl_index]
            else if (var_count > 1 .and. .not. has_complex_attributes) then
                ! Multiple simple variables - use multi-declaration
                if (has_kind) then
                    decl_index = push_multi_declaration(arena, type_name, temp_var_names(1:var_count), &
                               kind_value=kind_value, is_allocatable=is_allocatable, &
                                                        line=line, column=column)
                else
                    decl_index = push_multi_declaration(arena, type_name, temp_var_names(1:var_count), &
                                                        is_allocatable=is_allocatable, &
                                                        line=line, column=column)
                end if
                decl_indices = [decl_index]
            else
                ! Complex multi-variable declarations - create separate declarations for each variable
                if (allocated(decl_indices)) deallocate(decl_indices)
                allocate(decl_indices(var_count))
                do i = 1, var_count
                    if (allocated(var_dimensions(i)%dimension_indices)) then
                        ! Variable has array dimensions
                        if (has_kind) then
                            decl_indices(i) = push_declaration(arena, type_name, trim(temp_var_names(i)), &
                                       kind_value=kind_value, &
                                       dimension_indices=var_dimensions(i)%dimension_indices, &
                                       is_allocatable=is_allocatable, &
                                                              line=line, column=column)
                        else
                            decl_indices(i) = push_declaration(arena, type_name, trim(temp_var_names(i)), &
                                              dimension_indices=var_dimensions(i)%dimension_indices, &
                                              is_allocatable=is_allocatable, &
                                                              line=line, column=column)
                        end if
                    else
                        ! Simple variable without dimensions
                        if (has_kind) then
                            decl_indices(i) = push_declaration(arena, type_name, trim(temp_var_names(i)), &
                                       kind_value=kind_value, is_allocatable=is_allocatable, &
                                                              line=line, column=column)
                        else
                            decl_indices(i) = push_declaration(arena, type_name, trim(temp_var_names(i)), &
                                                              is_allocatable=is_allocatable, &
                                                              line=line, column=column)
                        end if
                    end if
                end do
            end if
            
            decl_count = var_count
        end block

    end function parse_multi_declaration

end module parser_declarations_module
