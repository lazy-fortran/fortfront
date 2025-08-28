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
    use ast_factory, only: push_multi_declaration
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
    ! Also handles multi-variable declarations by creating separate nodes when needed
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

        ! Get variable name(s) - handle both single and multiple variables
        if (parser%is_at_end()) then
            return
        end if

        token = parser%consume()
        if (token%kind /= TK_IDENTIFIER) then
            return
        end if
        
        ! Parse potentially multi-variable declaration
        ! For variables with array dimensions, we create separate declaration nodes
        ! since the AST can't handle per-variable dimensions in multi-declarations
        block
            type :: var_info
                character(len=64) :: name
                integer, allocatable :: dimension_indices(:)
            end type var_info
            
            type(var_info), allocatable :: variables(:)
            integer :: var_count, i, temp_index
            character(len=64) :: first_var_name
            type(token_t) :: next_token
            logical :: has_arrays, needs_separate_decls
            integer, allocatable :: first_dims(:)
            
            ! Allocate initial variables array
            allocate(variables(10))
            var_count = 1
            first_var_name = trim(token%text)
            variables(1)%name = first_var_name
            has_arrays = .false.
            needs_separate_decls = .false.
            
            ! Check for array dimensions after first variable
            if (.not. parser%is_at_end()) then
                next_token = parser%peek()
                if (next_token%text == "(") then
                    has_arrays = .true.
                    next_token = parser%consume()  ! consume '('
                    call parse_array_dimensions(parser, arena, variables(1)%dimension_indices)
                end if
            end if
            
            ! Check for comma to detect multi-variable declaration
            if (.not. parser%is_at_end()) then
                next_token = parser%peek()
                if (next_token%text == ",") then
                    ! This is a multi-variable declaration
                    do while (.not. parser%is_at_end())
                        next_token = parser%peek()
                        if (next_token%text == ",") then
                            ! Consume comma
                            next_token = parser%consume()
                            
                            ! Get next variable name
                            if (.not. parser%is_at_end()) then
                                next_token = parser%consume()
                                if (next_token%kind == TK_IDENTIFIER) then
                                    var_count = var_count + 1
                                    if (var_count > size(variables)) then
                                        ! Extend array if needed
                                        block
                                            type(var_info), allocatable :: temp_vars(:)
                                            integer :: old_size
                                            old_size = size(variables)
                                            allocate(temp_vars(old_size * 2))
                                            temp_vars(1:old_size) = variables(1:old_size)
                                            deallocate(variables)
                                            call move_alloc(temp_vars, variables)
                                        end block
                                    end if
                                    variables(var_count)%name = trim(next_token%text)
                                    
                                    ! Check for array dimensions after this variable
                                    if (.not. parser%is_at_end()) then
                                        next_token = parser%peek()
                                        if (next_token%text == "(") then
                                            has_arrays = .true.
                                            next_token = parser%consume()  ! consume '('
                                            call parse_array_dimensions(parser, arena, variables(var_count)%dimension_indices)
                                            ! If dimensions are different, we need separate declarations
                                            if (allocated(variables(1)%dimension_indices)) then
                                                if (.not. allocated(variables(var_count)%dimension_indices) .or. &
                                                    size(variables(1)%dimension_indices) /= size(variables(var_count)%dimension_indices)) then
                                                    needs_separate_decls = .true.
                                                end if
                                            else if (allocated(variables(var_count)%dimension_indices)) then
                                                needs_separate_decls = .true.
                                            end if
                                        end if
                                    end if
                                else
                                    exit
                                end if
                            else
                                exit
                            end if
                        else
                            exit
                        end if
                    end do
                end if
            end if
            
            ! Check for initialization
            initializer_index = 0
            if (.not. parser%is_at_end()) then
                next_token = parser%peek()
                if (next_token%text == "=" .or. next_token%text == "=>") then
                    next_token = parser%consume()
                    initializer_index = parse_comparison(parser, arena)
                end if
            end if
            
            if (var_count == 1) then
                ! Single variable declaration
                decl_index = push_declaration( &
                    arena, &
                    type_spec%type_name, &
                    variables(1)%name, &
                    dimension_indices=variables(1)%dimension_indices, &
                    initializer_index=initializer_index, &
                    is_allocatable=attr_info%is_allocatable, &
                    is_pointer=attr_info%is_pointer, &
                    is_parameter=attr_info%is_parameter &
                )
            else if (has_arrays .or. needs_separate_decls) then
                ! Multi-variable with arrays - create single multi-declaration with shared dimensions
                ! KNOWN LIMITATION: The current AST structure doesn't support per-variable dimensions
                ! For declarations like "real :: arr1(10), arr2(20)", we preserve variable names
                ! but can only store one set of dimensions (from the first variable).
                ! This is a fundamental AST limitation that requires restructuring to fix properly.
                ! See issue #706 for tracking this enhancement.
                block
                    character(len=64), allocatable :: all_names(:)
                    allocate(all_names(var_count))
                    do i = 1, var_count
                        all_names(i) = variables(i)%name
                    end do
                    
                    ! Create multi-declaration with dimensions from first variable
                    if (allocated(variables(1)%dimension_indices)) then
                        decl_index = push_multi_declaration( &
                            arena, &
                            type_spec%type_name, &
                            all_names, &
                            dimension_indices=variables(1)%dimension_indices, &
                            initializer_index=initializer_index, &
                            is_allocatable=attr_info%is_allocatable, &
                            is_pointer=attr_info%is_pointer, &
                            is_parameter=attr_info%is_parameter &
                        )
                    else
                        decl_index = push_multi_declaration( &
                            arena, &
                            type_spec%type_name, &
                            all_names, &
                            initializer_index=initializer_index, &
                            is_allocatable=attr_info%is_allocatable, &
                            is_pointer=attr_info%is_pointer, &
                            is_parameter=attr_info%is_parameter &
                        )
                    end if
                end block
            else
                ! Multi-variable without arrays - can use multi-declaration
                block
                    character(len=64), allocatable :: simple_names(:)
                    allocate(simple_names(var_count))
                    do i = 1, var_count
                        simple_names(i) = variables(i)%name
                    end do
                    decl_index = push_multi_declaration( &
                        arena, &
                        type_spec%type_name, &
                        simple_names, &
                        initializer_index=initializer_index, &
                        is_allocatable=attr_info%is_allocatable, &
                        is_pointer=attr_info%is_pointer, &
                        is_parameter=attr_info%is_parameter &
                    )
                end block
            end if
        end block
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
    ! Note: This function is called when the type and attributes have been consumed
    ! and we need to parse the variable list after ::
    function parse_multi_declaration(parser, arena) result(decl_indices)
        use iso_fortran_env, only: error_unit
        use ast_factory, only: push_multi_declaration
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable :: decl_indices(:)
        
        type(token_t) :: token, next_token
        type(type_specifier_t) :: type_spec
        type(declaration_attributes_t) :: attr_info
        character(len=64), allocatable :: var_names(:)
        integer :: var_count, initializer_index, decl_index
        
        
        ! Parse type specifier
        type_spec = parse_type_specifier(parser)
        if (.not. allocated(type_spec%type_name)) then
            allocate(decl_indices(0))
            return
        end if

        ! Parse declaration attributes
        call parse_declaration_attributes(parser, arena, attr_info)

        ! Check for :: separator
        token = parser%peek()
        if (token%text == "::") then
            token = parser%consume()
        end if

        ! Collect all variable names
        allocate(var_names(10))  ! Start with reasonable size
        var_count = 0
        initializer_index = 0
        
        do while (.not. parser%is_at_end())
            ! Get variable name
            token = parser%consume()
            if (token%kind /= TK_IDENTIFIER) exit
            
            var_count = var_count + 1
            if (var_count > size(var_names)) then
                ! Extend array if needed
                block
                    character(len=64), allocatable :: temp_names(:)
                    integer :: old_size
                    old_size = size(var_names)
                    allocate(temp_names(old_size * 2))
                    temp_names(1:old_size) = var_names(1:old_size)
                    deallocate(var_names)
                    call move_alloc(temp_names, var_names)
                end block
            end if
            var_names(var_count) = token%text
            
            ! Check for array dimensions after variable name
            if (.not. parser%is_at_end()) then
                next_token = parser%peek()
                if (next_token%text == "(") then
                    ! Skip array dimensions for this variable
                    ! Note: We currently don't store per-variable dimensions
                    next_token = parser%consume()  ! consume '('
                    block
                        integer, allocatable :: temp_dims(:)
                        call parse_array_dimensions(parser, arena, temp_dims)
                        ! If this is the first variable with dimensions, save them
                        if (var_count == 1 .and. allocated(temp_dims)) then
                            if (.not. allocated(attr_info%global_dimension_indices)) then
                                attr_info%global_dimension_indices = temp_dims
                                attr_info%has_global_dimensions = .true.
                            end if
                        end if
                    end block
                end if
            end if
            
            ! Check for comma or end of variables
            if (.not. parser%is_at_end()) then
                next_token = parser%peek()
                if (next_token%text == ",") then
                    ! Consume comma and continue
                    next_token = parser%consume()
                    cycle
                else if (next_token%text == "=" .or. next_token%text == "=>") then
                    ! Initialization found - consume and parse
                    next_token = parser%consume()
                    initializer_index = parse_comparison(parser, arena)
                    exit
                else
                    ! End of variable list
                    exit
                end if
            else
                exit
            end if
        end do
        
        if (var_count == 0) then
            allocate(decl_indices(0))
            return
        end if
        
        ! Create multi-variable declaration node
        ! Include array dimensions if present
        if (type_spec%has_kind) then
            if (attr_info%has_global_dimensions .and. allocated(attr_info%global_dimension_indices)) then
                decl_index = push_multi_declaration( &
                    arena, &
                    type_spec%type_name, &
                    var_names(1:var_count), &
                    kind_value=type_spec%kind_value, &
                    initializer_index=initializer_index, &
                    dimension_indices=attr_info%global_dimension_indices, &
                    is_allocatable=attr_info%is_allocatable, &
                    is_pointer=attr_info%is_pointer, &
                    is_parameter=attr_info%is_parameter &
                )
            else
                decl_index = push_multi_declaration( &
                    arena, &
                    type_spec%type_name, &
                    var_names(1:var_count), &
                    kind_value=type_spec%kind_value, &
                    initializer_index=initializer_index, &
                    is_allocatable=attr_info%is_allocatable, &
                    is_pointer=attr_info%is_pointer, &
                    is_parameter=attr_info%is_parameter &
                )
            end if
        else
            if (attr_info%has_global_dimensions .and. allocated(attr_info%global_dimension_indices)) then
                decl_index = push_multi_declaration( &
                    arena, &
                    type_spec%type_name, &
                    var_names(1:var_count), &
                    initializer_index=initializer_index, &
                    dimension_indices=attr_info%global_dimension_indices, &
                    is_allocatable=attr_info%is_allocatable, &
                    is_pointer=attr_info%is_pointer, &
                    is_parameter=attr_info%is_parameter &
                )
            else
                decl_index = push_multi_declaration( &
                    arena, &
                    type_spec%type_name, &
                    var_names(1:var_count), &
                    initializer_index=initializer_index, &
                    is_allocatable=attr_info%is_allocatable, &
                    is_pointer=attr_info%is_pointer, &
                    is_parameter=attr_info%is_parameter &
                )
            end if
        end if
        
        if (decl_index > 0) then
            allocate(decl_indices(1))
            decl_indices(1) = decl_index
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