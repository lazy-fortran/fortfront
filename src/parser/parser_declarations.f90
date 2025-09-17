module parser_declarations
    use iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER, TK_EOF, TK_KEYWORD, TK_NEWLINE
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use ast_types, only: LITERAL_STRING
    use ast_nodes_data, only: INTENT_IN, INTENT_OUT, INTENT_INOUT
    use parser_expressions_module, only: parse_comparison, parse_range
    use parser_result_types, only: parse_result_t, success_parse_result, error_parse_result
    use error_handling, only: ERROR_PARSER
    use ast_factory, only: push_multi_declaration, push_declaration
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
    recursive function parse_type_specifier(parser) result(type_spec)
        type(parser_state_t), intent(inout) :: parser
        type(type_specifier_t) :: type_spec

        type(token_t) :: token, next_token

        token = parser%consume()
        type_spec%type_name = trim(token%text)  ! Explicit trim for clean allocation
        type_spec%line = token%line
        type_spec%column = token%column
        type_spec%has_kind = .false.
        type_spec%kind_value = 0

        ! Handle "double precision" as a two-word type name
        if (trim(token%text) == "double" .and. .not. parser%is_at_end()) then
            next_token = parser%peek()
            if (trim(next_token%text) == "precision") then
                next_token = parser%consume()  ! consume "precision"
                type_spec%type_name = "double precision"
            end if
        end if

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
    recursive subroutine parse_declaration_attributes(parser, arena, attr_info)
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
                case ("dimension")
                    token = parser%consume()
                    if (.not. parser%is_at_end()) then
                        token = parser%peek()
                        if (token%text == "(") then
                            token = parser%consume()  ! consume '('
                            call parse_array_dimensions(parser, arena, attr_info%global_dimension_indices)
                            attr_info%has_global_dimensions = .true.
                        end if
                    end if
                case ("intent")
                    token = parser%consume()  ! consume 'intent'
                    if (.not. parser%is_at_end()) then
                        token = parser%peek()
                        if (token%text == "(") then
                            token = parser%consume()  ! consume '('
                            if (.not. parser%is_at_end()) then
                                token = parser%peek()
                                select case (token%text)
                                case ("in")
                                    attr_info%intent = "in"
                                    attr_info%has_intent = .true.
                                    token = parser%consume()
                                case ("out")
                                    attr_info%intent = "out"
                                    attr_info%has_intent = .true.
                                    token = parser%consume()
                                case ("inout")
                                    attr_info%intent = "inout"
                                    attr_info%has_intent = .true.
                                    token = parser%consume()
                                end select
                                ! consume closing paren
                                if (.not. parser%is_at_end()) then
                                    token = parser%peek()
                                    if (token%text == ")") then
                                        token = parser%consume()
                                    end if
                                end if
                            end if
                        end if
                    end if
                case ("optional")
                    attr_info%is_optional = .true.
                    token = parser%consume()
                case ("target")
                    attr_info%is_target = .true.
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
    recursive function parse_declaration(parser, arena) result(decl_index)
        use ast_factory, only: push_declaration
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: decl_index
        
        type(token_t) :: token
        type(type_specifier_t) :: type_spec
        type(declaration_attributes_t) :: attr_info
        integer :: initializer_index

        
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

        ! Skip any newlines after ::
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if (token%kind == TK_NEWLINE) then
                token = parser%consume()
            else
                exit
            end if
        end do

        ! Get variable name(s) - handle both single and multiple variables
        ! Removed is_at_end check - might prevent parsing after newlines

        token = parser%consume()
        if (token%kind /= TK_IDENTIFIER) then
            return
        end if
        
        ! Check if this is a multi-variable declaration by looking ahead for commas
        block
            character(len=64), allocatable :: var_names(:)
            integer :: var_count, i, temp_index
            character(len=64) :: first_var_name
            type(token_t) :: next_token
            logical :: is_multi_var
            
            first_var_name = trim(token%text)
            var_count = 1
            is_multi_var = .false.
            
            ! Look ahead for commas to detect multi-variable declaration
            if (.not. parser%is_at_end()) then
                next_token = parser%peek()
                if (next_token%text == ",") then
                    is_multi_var = .true.
                    
                    ! Collect all variable names
                    allocate(var_names(10))  ! Start with reasonable size
                    var_names(1) = first_var_name
                    
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
                                    var_names(var_count) = trim(next_token%text)
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
            
            if (is_multi_var) then
                ! Create multi-variable declaration preserving attributes
                if (type_spec%has_kind) then
                    if (attr_info%has_global_dimensions) then
                        temp_index = push_multi_declaration( &
                            arena, &
                            type_spec%type_name, &
                            var_names(1:var_count), &
                            kind_value=type_spec%kind_value, &
                            dimension_indices=attr_info%global_dimension_indices, &
                            is_allocatable=attr_info%is_allocatable, &
                            is_pointer=attr_info%is_pointer, &
                            is_parameter=attr_info%is_parameter &
                        )
                    else
                        temp_index = push_multi_declaration( &
                            arena, &
                            type_spec%type_name, &
                            var_names(1:var_count), &
                            kind_value=type_spec%kind_value, &
                            is_allocatable=attr_info%is_allocatable, &
                            is_pointer=attr_info%is_pointer, &
                            is_parameter=attr_info%is_parameter &
                        )
                    end if
                else
                    if (attr_info%has_global_dimensions) then
                        temp_index = push_multi_declaration( &
                            arena, &
                            type_spec%type_name, &
                            var_names(1:var_count), &
                            dimension_indices=attr_info%global_dimension_indices, &
                            is_allocatable=attr_info%is_allocatable, &
                            is_pointer=attr_info%is_pointer, &
                            is_parameter=attr_info%is_parameter &
                        )
                    else
                        temp_index = push_multi_declaration( &
                            arena, &
                            type_spec%type_name, &
                            var_names(1:var_count), &
                            is_allocatable=attr_info%is_allocatable, &
                            is_pointer=attr_info%is_pointer, &
                            is_parameter=attr_info%is_parameter &
                        )
                    end if
                end if
                decl_index = temp_index
                return
            end if
        end block

        block
            character(len=:), allocatable :: var_name
            integer, allocatable :: local_dimension_indices(:)
            logical :: has_local_dimensions
            var_name = token%text
            has_local_dimensions = .false.

            ! Per-variable dimensions: e.g., "integer :: arr(10)"
            if (.not. parser%is_at_end()) then
                token = parser%peek()
                if (token%text == "(") then
                    token = parser%consume()  ! consume '('
                    call parse_array_dimensions(parser, arena, local_dimension_indices)
                    has_local_dimensions = .true.
                end if
            end if

            ! Check for initialization
            if (.not. parser%is_at_end()) then
                token = parser%peek()
                if (token%text == "=" .or. token%text == "=>") then
                    token = parser%consume()
                    ! Special handling for complex type initializers
                    if (type_spec%type_name == "complex") then
                        initializer_index = handle_complex_initializer(parser, arena, type_spec%type_name)
                    else
                        initializer_index = parse_comparison(parser, arena)
                    end if
                end if
            end if

            ! Create declaration node
            if (attr_info%has_global_dimensions) then
                decl_index = push_declaration( &
                    arena, &
                    type_spec%type_name, &
                    var_name, &
                    dimension_indices=attr_info%global_dimension_indices, &
                    initializer_index=initializer_index, &
                    is_allocatable=attr_info%is_allocatable, &
                    is_pointer=attr_info%is_pointer, &
                    is_target=attr_info%is_target, &
                    intent_value=attr_info%intent, &
                    is_optional=attr_info%is_optional, &
                    is_parameter=attr_info%is_parameter &
                )
            else if (has_local_dimensions) then
                decl_index = push_declaration( &
                    arena, &
                    type_spec%type_name, &
                    var_name, &
                    dimension_indices=local_dimension_indices, &
                    initializer_index=initializer_index, &
                    is_allocatable=attr_info%is_allocatable, &
                    is_pointer=attr_info%is_pointer, &
                    is_target=attr_info%is_target, &
                    intent_value=attr_info%intent, &
                    is_optional=attr_info%is_optional, &
                    is_parameter=attr_info%is_parameter &
                )
            else
                decl_index = push_declaration( &
                arena, &
                type_spec%type_name, &
                var_name, &
                initializer_index=initializer_index, &
                is_allocatable=attr_info%is_allocatable, &
                is_pointer=attr_info%is_pointer, &
                is_target=attr_info%is_target, &
                intent_value=attr_info%intent, &
                is_optional=attr_info%is_optional, &
                is_parameter=attr_info%is_parameter &
            )
            end if
        end block
    end function parse_declaration

    ! Result-based declaration parser with structured error handling
    recursive function parse_declaration_with_result(parser, arena) result(parse_res)
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
    recursive subroutine parse_array_dimensions(parser, arena, dimension_indices)
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
    recursive function parse_multi_declaration(parser, arena) result(decl_indices)
        use iso_fortran_env, only: error_unit
        use ast_factory, only: push_multi_declaration, push_declaration
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable :: decl_indices(:)
        
        type(token_t) :: token, next_token
        type(type_specifier_t) :: type_spec
        type(declaration_attributes_t) :: attr_info
        character(len=64), allocatable :: var_names(:)
        integer, allocatable :: per_var_dims(:,:)  ! Store dimensions per variable
        logical, allocatable :: has_dims(:)  ! Track which vars have dimensions
        integer, allocatable :: init_indices(:)
        integer :: var_count, decl_index, i
        logical :: has_any_initializer
        
        
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

        ! Collect all variable names and their dimensions
        allocate(var_names(10))  ! Start with reasonable size
        allocate(per_var_dims(10, 10))  ! Max 10 vars, max 10 dims each
        allocate(has_dims(10))
        allocate(init_indices(10))
        var_count = 0
        has_any_initializer = .false.
        per_var_dims = 0
        has_dims = .false.
        init_indices = 0
        
        do while (.not. parser%is_at_end())
            ! Get variable name
            token = parser%consume()
            if (token%kind /= TK_IDENTIFIER) exit
            
            var_count = var_count + 1
            if (var_count > size(var_names)) then
                ! Extend arrays if needed
                block
                    character(len=64), allocatable :: temp_names(:)
                    integer, allocatable :: temp_dims(:,:)
                    logical, allocatable :: temp_has(:)
                    integer, allocatable :: temp_init(:)
                    integer :: old_size, new_size
                    old_size = size(var_names)
                    new_size = old_size * 2
                    allocate(temp_names(new_size))
                    allocate(temp_dims(new_size, 10))
                    allocate(temp_has(new_size))
                    allocate(temp_init(new_size))
                    temp_names = ''
                    temp_dims = 0
                    temp_has = .false.
                    temp_init = 0
                    temp_names(1:old_size) = var_names(1:old_size)
                    temp_dims(1:old_size, :) = per_var_dims(1:old_size, :)
                    temp_has(1:old_size) = has_dims(1:old_size)
                    temp_init(1:old_size) = init_indices(1:old_size)
                    deallocate(var_names, per_var_dims, has_dims, init_indices)
                    call move_alloc(temp_names, var_names)
                    call move_alloc(temp_dims, per_var_dims)
                    call move_alloc(temp_has, has_dims)
                    call move_alloc(temp_init, init_indices)
                end block
            end if
            var_names(var_count) = token%text
            init_indices(var_count) = 0
            has_dims(var_count) = .false.
            per_var_dims(var_count, :) = 0

            ! Check for array dimensions for this variable
            if (.not. parser%is_at_end()) then
                next_token = parser%peek()
                if (next_token%text == "(") then
                    ! This variable has dimensions
                    token = parser%consume()  ! consume '('
                    block
                        integer, allocatable :: local_dims(:)
                        integer :: j
                        call parse_array_dimensions(parser, arena, local_dims)
                        if (allocated(local_dims) .and. size(local_dims) > 0) then
                            has_dims(var_count) = .true.
                            do j = 1, min(size(local_dims), 10)
                                per_var_dims(var_count, j) = local_dims(j)
                            end do
                        end if
                    end block
                end if
            end if
            
            ! Check for initializer for this variable
            if (.not. parser%is_at_end()) then
                next_token = parser%peek()
                if (next_token%text == "=" .or. next_token%text == "=>") then
                    next_token = parser%consume()
                    if (type_spec%type_name == "complex") then
                        init_indices(var_count) = handle_complex_initializer(parser, arena, type_spec%type_name)
                    else
                        init_indices(var_count) = parse_comparison(parser, arena)
                    end if
                    if (init_indices(var_count) > 0) has_any_initializer = .true.
                end if
            end if

            ! Check for comma or end of variables
            if (.not. parser%is_at_end()) then
                next_token = parser%peek()
                if (next_token%text == ",") then
                    next_token = parser%consume()
                    cycle
                end if
            end if
            exit
        end do
        
        if (var_count == 0) then
            allocate(decl_indices(0))
            return
        end if
        
        ! Check if we have per-variable dimensions
        block
            logical :: needs_separate_decls
            integer :: num_with_dims
            
            num_with_dims = 0
            do i = 1, var_count
                if (has_dims(i)) num_with_dims = num_with_dims + 1
            end do
            
            ! If we have per-variable dimensions, create separate declarations
            needs_separate_decls = (num_with_dims > 0) .or. has_any_initializer
            
            if (needs_separate_decls) then
                ! Create separate declaration for each variable
                allocate(decl_indices(var_count))
                do i = 1, var_count
                    if (has_dims(i)) then
                        ! Variable with dimensions
                        block
                            integer, allocatable :: var_dims(:)
                            integer :: j, dim_count
                            
                            ! Count dimensions for this variable
                            dim_count = 0
                            do j = 1, 10
                                if (per_var_dims(i, j) > 0) then
                                    dim_count = dim_count + 1
                                else
                                    exit
                                end if
                            end do
                            
                            if (dim_count > 0) then
                                allocate(var_dims(dim_count))
                                var_dims = per_var_dims(i, 1:dim_count)
                                
                                decl_indices(i) = push_declaration( &
                                    arena, &
                                    type_spec%type_name, &
                                    var_names(i), &
                                    dimension_indices=var_dims, &
                                    initializer_index=init_indices(i), &
                                    is_allocatable=attr_info%is_allocatable, &
                                    is_pointer=attr_info%is_pointer, &
                                    is_target=attr_info%is_target, &
                                    intent_value=attr_info%intent, &
                                    is_optional=attr_info%is_optional, &
                                    is_parameter=attr_info%is_parameter &
                                )
                            end if
                        end block
                    else if (attr_info%has_global_dimensions) then
                        ! Variable without per-var dims but with global dims
                        decl_indices(i) = push_declaration( &
                            arena, &
                            type_spec%type_name, &
                            var_names(i), &
                            dimension_indices=attr_info%global_dimension_indices, &
                            initializer_index=init_indices(i), &
                            is_allocatable=attr_info%is_allocatable, &
                            is_pointer=attr_info%is_pointer, &
                            is_target=attr_info%is_target, &
                            intent_value=attr_info%intent, &
                            is_optional=attr_info%is_optional, &
                            is_parameter=attr_info%is_parameter &
                        )
                    else
                        ! Variable without dimensions
                        decl_indices(i) = push_declaration( &
                            arena, &
                            type_spec%type_name, &
                            var_names(i), &
                            initializer_index=init_indices(i), &
                            is_allocatable=attr_info%is_allocatable, &
                            is_pointer=attr_info%is_pointer, &
                            is_target=attr_info%is_target, &
                            intent_value=attr_info%intent, &
                            is_optional=attr_info%is_optional, &
                            is_parameter=attr_info%is_parameter &
                        )
                    end if
                end do
            else
                ! Use original multi-declaration approach when no per-var dims
                if (type_spec%has_kind) then
                    if (attr_info%has_global_dimensions) then
                        decl_index = push_multi_declaration( &
                            arena, &
                            type_spec%type_name, &
                            var_names(1:var_count), &
                            kind_value=type_spec%kind_value, &
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
                            is_allocatable=attr_info%is_allocatable, &
                            is_pointer=attr_info%is_pointer, &
                            is_parameter=attr_info%is_parameter &
                        )
                    end if
                else
                    if (attr_info%has_global_dimensions) then
                        decl_index = push_multi_declaration( &
                            arena, &
                            type_spec%type_name, &
                            var_names(1:var_count), &
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
            end if
        end block
    end function parse_multi_declaration

    ! Parse derived type definition with robust error handling
    recursive function parse_derived_type_def(parser, arena) result(type_index)
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

        ! Skip any semicolons or newlines  
        do while (.not. parser%is_at_end())
            token = parser%peek()
            if ((token%kind == TK_OPERATOR .and. token%text == ";") .or. &
                token%kind == TK_NEWLINE) then
                token = parser%consume()
            else
                exit
            end if
        end do

        ! Parse components
        do while (.not. parser%is_at_end())
            token = parser%peek()

            ! Check for end type
            if (token%kind == TK_IDENTIFIER .and. token%text == "end") then
                token = parser%consume()
                token = parser%peek()
                if (token%kind == TK_IDENTIFIER .and. token%text == "type") then
                    token = parser%consume()
                    exit
                else
                    ! Not "end type", we need to reprocess this
                    ! This is a problem - we can't push tokens back!
                    ! For now, exit anyway
                    exit
                end if
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
                    else
                        exit
                    end if
                end do
            else if (comp_index == 0) then
                ! If we couldn't parse a component, skip to next line or token
                token = parser%peek()
                if (.not. (token%kind == TK_IDENTIFIER .and. token%text == "end")) then
                    ! Skip unknown token to avoid infinite loop
                    token = parser%consume()
                end if
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
    recursive function parse_derived_type_component(parser, arena) result(comp_index)
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
            else
                exit
            end if
        end do

        token = parser%peek()
        
        ! Handle end of type definition
        if ((token%kind == TK_IDENTIFIER .or. token%kind == TK_KEYWORD) .and. token%text == "end") then
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

    ! Helper function to detect and convert complex literals
    ! When we have a complex type declaration with initializer like (1.0, 2.0),
    ! we need to parse it as a complex literal, not just take the first value
    recursive function handle_complex_initializer(parser, arena, type_name) result(complex_index)
        use ast_factory, only: push_complex_literal
        use parser_expressions_module, only: parse_comparison
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: type_name
        integer :: complex_index
        
        type(token_t) :: token
        integer :: real_index, imag_index
        
        complex_index = 0
        
        ! Only handle if type is complex
        if (type_name /= "complex") then
            ! Not a complex type, parse normally
            complex_index = parse_comparison(parser, arena)
            return
        end if
        
        ! Check for opening parenthesis
        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            ! Not a parenthesized expression, parse normally
            complex_index = parse_comparison(parser, arena)
            return
        end if
        
        ! Consume opening parenthesis
        token = parser%consume()
        
        ! Parse real part
        real_index = parse_comparison(parser, arena)
        
        ! Check for comma
        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            ! This looks like a complex literal
            token = parser%consume()  ! consume comma
            
            ! Parse imaginary part
            imag_index = parse_comparison(parser, arena)
            
            ! Check for closing parenthesis
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()  ! consume closing paren
                
                ! Create complex literal node
                complex_index = push_complex_literal(arena, real_index, imag_index, &
                                                     token%line, token%column)
            else
                ! Malformed, return what we have
                complex_index = real_index
            end if
        else
            ! Not a complex literal, just a parenthesized expression
            ! Check for closing parenthesis
            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
            end if
            complex_index = real_index
        end if
        
    end function handle_complex_initializer

end module parser_declarations
