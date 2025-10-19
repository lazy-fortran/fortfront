module parser_declarations_core_module
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_OPERATOR, TK_NUMBER, TK_EOF, &
                          TK_KEYWORD, TK_NEWLINE, TK_WHITESPACE, TK_COMMENT
    use parser_state_module, only: parser_state_t
    use ast_arena_modern, only: ast_arena_t
    use parser_declarations_type_spec_support_module, only: type_specifier_t
    use parser_declarations_type_spec_module, only: parse_type_specifier
    use parser_result_types, only: parse_result_t, success_parse_result, &
                                   error_parse_result
    use error_handling, only: ERROR_PARSER
    use parser_expressions_module, only: parse_comparison, parse_range
    use parser_type_hooks_module, only: register_type_annotation
    use declaration_attribute_utils, only: declaration_attribute_info_t, &
                                           reset_declaration_attributes, &
                                           set_declaration_intent
    implicit none
    private

    public :: parse_declaration
    public :: parse_multi_declaration
    public :: parse_declaration_with_result
    public :: parse_array_dimensions

contains

    subroutine parse_declaration_attributes(parser, arena, attr_info)
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        type(declaration_attribute_info_t), intent(out) :: attr_info

        type(token_t) :: token

        call reset_declaration_attributes(attr_info)

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
                case ("external")
                    attr_info%is_external = .true.
                    token = parser%consume()
                case ("dimension")
                    token = parser%consume()
                    if (.not. parser%is_at_end()) then
                        token = parser%peek()
                        if (token%text == "(") then
                            token = parser%consume()  ! consume '('
                            call parse_array_dimensions( &
                                parser, arena, attr_info%global_dimension_indices)
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
                                    call set_declaration_intent(attr_info, "in")
                                    token = parser%consume()
                                case ("out")
                                    call set_declaration_intent(attr_info, "out")
                                    token = parser%consume()
                                case ("inout")
                                    call set_declaration_intent(attr_info, "inout")
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
    function parse_declaration(parser, arena) result(decl_index)
        use ast_factory, only: push_declaration, push_multi_declaration
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer :: decl_index

        type(token_t) :: token
        type(type_specifier_t) :: type_spec
        type(declaration_attribute_info_t) :: attr_info
        integer :: initializer_index
        character(len=:), allocatable :: var_name
        integer, allocatable :: local_dimension_indices(:)
        logical :: has_local_dimensions

        decl_index = 0
        initializer_index = 0

        ! Parse type specifier
        type_spec = parse_type_specifier(parser, arena)
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
                    allocate (var_names(10))  ! Start with reasonable size
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
                                            character(len=64), allocatable :: &
                                                temp_names(:)
                                            integer :: old_size
                                            old_size = size(var_names)
                                            allocate (temp_names(old_size * 2))
                                            temp_names(1:old_size) = &
                                                var_names(1:old_size)
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
                                     dimension_indices= &
                                     attr_info%global_dimension_indices, &
                                     is_allocatable=attr_info%is_allocatable, &
                                     is_pointer=attr_info%is_pointer, &
                                     is_parameter=attr_info%is_parameter)
                    else
                        temp_index = push_multi_declaration( &
                                     arena, &
                                     type_spec%type_name, &
                                     var_names(1:var_count), &
                                     kind_value=type_spec%kind_value, &
                                     is_allocatable=attr_info%is_allocatable, &
                                     is_pointer=attr_info%is_pointer, &
                                     is_parameter=attr_info%is_parameter)
                    end if
                else
                    if (attr_info%has_global_dimensions) then
                        temp_index = push_multi_declaration( &
                                     arena, &
                                     type_spec%type_name, &
                                     var_names(1:var_count), &
                                     dimension_indices= &
                                     attr_info%global_dimension_indices, &
                                     is_allocatable=attr_info%is_allocatable, &
                                     is_pointer=attr_info%is_pointer, &
                                     is_parameter=attr_info%is_parameter)
                    else
                        temp_index = push_multi_declaration( &
                                     arena, &
                                     type_spec%type_name, &
                                     var_names(1:var_count), &
                                     is_allocatable=attr_info%is_allocatable, &
                                     is_pointer=attr_info%is_pointer, &
                                     is_parameter=attr_info%is_parameter)
                    end if
                end if
                decl_index = temp_index
                if (temp_index > 0) then
                    if (attr_info%has_global_dimensions) then
                        call register_type_annotation( &
                            temp_index, &
                            type_spec%type_name, &
                            var_names(1:var_count), &
                            has_kind=type_spec%has_kind, &
                            kind_value=type_spec%kind_value, &
                            is_parameter=attr_info%is_parameter, &
                            is_allocatable=attr_info%is_allocatable, &
                            is_pointer=attr_info%is_pointer, &
                            dimension_indices= &
                            attr_info%global_dimension_indices)
                    else
                        call register_type_annotation( &
                            temp_index, &
                            type_spec%type_name, &
                            var_names(1:var_count), &
                            has_kind=type_spec%has_kind, &
                            kind_value=type_spec%kind_value, &
                            is_parameter=attr_info%is_parameter, &
                            is_allocatable=attr_info%is_allocatable, &
                            is_pointer=attr_info%is_pointer)
                    end if
                end if
                return
            end if

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
                        initializer_index = handle_complex_initializer( &
                                            parser, arena, type_spec%type_name)
                    else
                        initializer_index = parse_comparison(parser, arena)
                    end if
                end if
            end if

            ! Create declaration node
            if (attr_info%has_global_dimensions) then
                if (type_spec%has_kind) then
                    decl_index = push_declaration( &
                                 arena, &
                                 type_spec%type_name, &
                                 var_name, &
                                 kind_value=type_spec%kind_value, &
                                 dimension_indices= &
                                 attr_info%global_dimension_indices, &
                                 initializer_index=initializer_index, &
                                 is_allocatable=attr_info%is_allocatable, &
                                 is_pointer=attr_info%is_pointer, &
                                 is_target=attr_info%is_target, &
                                 intent_value=attr_info%intent, &
                                 is_optional=attr_info%is_optional, &
                                 is_parameter=attr_info%is_parameter)
                else
                    decl_index = push_declaration( &
                                 arena, &
                                 type_spec%type_name, &
                                 var_name, &
                                 dimension_indices= &
                                 attr_info%global_dimension_indices, &
                                 initializer_index=initializer_index, &
                                 is_allocatable=attr_info%is_allocatable, &
                                 is_pointer=attr_info%is_pointer, &
                                 is_target=attr_info%is_target, &
                                 intent_value=attr_info%intent, &
                                 is_optional=attr_info%is_optional, &
                                 is_parameter=attr_info%is_parameter)
                end if
            else if (has_local_dimensions) then
                if (type_spec%has_kind) then
                    decl_index = push_declaration( &
                                 arena, &
                                 type_spec%type_name, &
                                 var_name, &
                                 kind_value=type_spec%kind_value, &
                                 dimension_indices=local_dimension_indices, &
                                 initializer_index=initializer_index, &
                                 is_allocatable=attr_info%is_allocatable, &
                                 is_pointer=attr_info%is_pointer, &
                                 is_target=attr_info%is_target, &
                                 intent_value=attr_info%intent, &
                                 is_optional=attr_info%is_optional, &
                                 is_parameter=attr_info%is_parameter)
                else
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
                                 is_parameter=attr_info%is_parameter)
                end if
            else
                if (type_spec%has_kind) then
                    decl_index = push_declaration( &
                                 arena, &
                                 type_spec%type_name, &
                                 var_name, &
                                 kind_value=type_spec%kind_value, &
                                 initializer_index=initializer_index, &
                                 is_allocatable=attr_info%is_allocatable, &
                                 is_pointer=attr_info%is_pointer, &
                                 is_target=attr_info%is_target, &
                                 intent_value=attr_info%intent, &
                                 is_optional=attr_info%is_optional, &
                                 is_parameter=attr_info%is_parameter)
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
                                 is_parameter=attr_info%is_parameter)
                end if
            end if

            if (decl_index > 0) then
                if (attr_info%has_global_dimensions) then
                    call register_type_annotation( &
                        decl_index, &
                        type_spec%type_name, &
                        [adjustl(trim(var_name))], &
                        has_kind=type_spec%has_kind, &
                        kind_value=type_spec%kind_value, &
                        is_parameter=attr_info%is_parameter, &
                        is_allocatable=attr_info%is_allocatable, &
                        is_pointer=attr_info%is_pointer, &
                        dimension_indices= &
                        attr_info%global_dimension_indices)
                else if (has_local_dimensions) then
                    call register_type_annotation( &
                        decl_index, &
                        type_spec%type_name, &
                        [adjustl(trim(var_name))], &
                        has_kind=type_spec%has_kind, &
                        kind_value=type_spec%kind_value, &
                        is_parameter=attr_info%is_parameter, &
                        is_allocatable=attr_info%is_allocatable, &
                        is_pointer=attr_info%is_pointer, &
                        dimension_indices=local_dimension_indices)
                else
                    call register_type_annotation( &
                        decl_index, &
                        type_spec%type_name, &
                        [adjustl(trim(var_name))], &
                        has_kind=type_spec%has_kind, &
                        kind_value=type_spec%kind_value, &
                        is_parameter=attr_info%is_parameter, &
                        is_allocatable=attr_info%is_allocatable, &
                        is_pointer=attr_info%is_pointer)
                end if
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
            allocate (dimension_indices(dim_count))
            dimension_indices = temp_indices(1:dim_count)
        else
            allocate (dimension_indices(0))
        end if
    end subroutine parse_array_dimensions

    ! Helper function to detect and convert complex literals
    function handle_complex_initializer(parser, arena, type_name) result(complex_index)
        use ast_factory, only: push_complex_literal
        use parser_expressions_module, only: parse_comparison
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: type_name
        integer :: complex_index

        type(token_t) :: token
        integer :: real_index
        integer :: imag_index

        complex_index = 0

        if (type_name /= "complex") then
            complex_index = parse_comparison(parser, arena)
            return
        end if

        token = parser%peek()
        if (token%kind /= TK_OPERATOR .or. token%text /= "(") then
            complex_index = parse_comparison(parser, arena)
            return
        end if

        token = parser%consume()
        real_index = parse_comparison(parser, arena)

        token = parser%peek()
        if (token%kind == TK_OPERATOR .and. token%text == ",") then
            token = parser%consume()
            imag_index = parse_comparison(parser, arena)

            token = parser%peek()
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
                complex_index = push_complex_literal(arena, real_index, imag_index, &
                                                     token%line, token%column)
            else
                complex_index = real_index
            end if
        else
            if (token%kind == TK_OPERATOR .and. token%text == ")") then
                token = parser%consume()
            end if
            complex_index = real_index
        end if
    end function handle_complex_initializer

    ! Parse multi-variable declaration (e.g., real :: x, y, z = 1.0)
    function parse_multi_declaration(parser, arena) result(decl_indices)
        use, intrinsic :: iso_fortran_env, only: error_unit
        use ast_factory, only: push_multi_declaration, push_declaration
        type(parser_state_t), intent(inout) :: parser
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable :: decl_indices(:)

        type(token_t) :: token, next_token
        type(type_specifier_t) :: type_spec
        type(declaration_attribute_info_t) :: attr_info
        character(len=64), allocatable :: var_names(:)
        integer, allocatable :: per_var_dims(:, :)  ! Store dimensions per variable
        logical, allocatable :: has_dims(:)  ! Track which vars have dimensions
        integer, allocatable :: init_indices(:)
        integer :: var_count, decl_index, i
        logical :: has_any_initializer

        ! Parse type specifier
        type_spec = parse_type_specifier(parser, arena)
        if (.not. allocated(type_spec%type_name)) then
            allocate (decl_indices(0))
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
        allocate (var_names(10))  ! Start with reasonable size
        allocate (per_var_dims(10, 10))  ! Max 10 vars, max 10 dims each
        allocate (has_dims(10))
        allocate (init_indices(10))
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
                    integer, allocatable :: temp_dims(:, :)
                    logical, allocatable :: temp_has(:)
                    integer, allocatable :: temp_init(:)
                    integer :: old_size, new_size
                    old_size = size(var_names)
                    new_size = old_size * 2
                    allocate (temp_names(new_size))
                    allocate (temp_dims(new_size, 10))
                    allocate (temp_has(new_size))
                    allocate (temp_init(new_size))
                    temp_names = ''
                    temp_dims = 0
                    temp_has = .false.
                    temp_init = 0
                    temp_names(1:old_size) = var_names(1:old_size)
                    temp_dims(1:old_size, :) = per_var_dims(1:old_size, :)
                    temp_has(1:old_size) = has_dims(1:old_size)
                    temp_init(1:old_size) = init_indices(1:old_size)
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
                        init_indices(var_count) = &
                            handle_complex_initializer(parser, arena, &
                                                       type_spec%type_name)
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
            allocate (decl_indices(0))
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
                allocate (decl_indices(var_count))
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
                                allocate (var_dims(dim_count))
                                var_dims = per_var_dims(i, 1:dim_count)

                                decl_indices(i) = push_declaration( &
                                                  arena, &
                                                  type_spec%type_name, &
                                                  var_names(i), &
                                                  dimension_indices=var_dims, &
                                                  initializer_index= &
                                                  init_indices(i), &
                                                  is_allocatable= &
                                                  attr_info%is_allocatable, &
                                                  is_pointer=attr_info%is_pointer, &
                                                  is_target=attr_info%is_target, &
                                                  intent_value=attr_info%intent, &
                                                  is_optional=attr_info%is_optional, &
                                                  is_parameter=attr_info%is_parameter)
                                if (decl_indices(i) > 0) then
                                    call register_type_annotation( &
                                        decl_indices(i), &
                                        type_spec%type_name, &
                                        [adjustl(trim(var_names(i)))], &
                                        has_kind=type_spec%has_kind, &
                                        kind_value=type_spec%kind_value, &
                                        is_parameter=attr_info%is_parameter, &
                                        is_allocatable=attr_info%is_allocatable, &
                                        is_pointer=attr_info%is_pointer, &
                                        dimension_indices=var_dims)
                                end if
                            end if
                        end block
                    else if (attr_info%has_global_dimensions) then
                        ! Variable without per-var dims but with global dims
                        decl_indices(i) = push_declaration( &
                                          arena, &
                                          type_spec%type_name, &
                                          var_names(i), &
                                          dimension_indices= &
                                          attr_info%global_dimension_indices, &
                                          initializer_index=init_indices(i), &
                                          is_allocatable=attr_info%is_allocatable, &
                                          is_pointer=attr_info%is_pointer, &
                                          is_target=attr_info%is_target, &
                                          intent_value=attr_info%intent, &
                                          is_optional=attr_info%is_optional, &
                                          is_parameter=attr_info%is_parameter)
                        if (decl_indices(i) > 0) then
                            call register_type_annotation( &
                                decl_indices(i), &
                                type_spec%type_name, &
                                [adjustl(trim(var_names(i)))], &
                                has_kind=type_spec%has_kind, &
                                kind_value=type_spec%kind_value, &
                                is_parameter=attr_info%is_parameter, &
                                is_allocatable=attr_info%is_allocatable, &
                                is_pointer=attr_info%is_pointer, &
                                dimension_indices= &
                                attr_info%global_dimension_indices)
                        end if
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
                                          is_parameter=attr_info%is_parameter)
                        if (decl_indices(i) > 0) then
                            call register_type_annotation( &
                                decl_indices(i), &
                                type_spec%type_name, &
                                [adjustl(trim(var_names(i)))], &
                                has_kind=type_spec%has_kind, &
                                kind_value=type_spec%kind_value, &
                                is_parameter=attr_info%is_parameter, &
                                is_allocatable=attr_info%is_allocatable, &
                                is_pointer=attr_info%is_pointer)
                        end if
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
                                     dimension_indices= &
                                     attr_info%global_dimension_indices, &
                                     is_allocatable=attr_info%is_allocatable, &
                                     is_pointer=attr_info%is_pointer, &
                                     is_parameter=attr_info%is_parameter)
                    else
                        decl_index = push_multi_declaration( &
                                     arena, &
                                     type_spec%type_name, &
                                     var_names(1:var_count), &
                                     kind_value=type_spec%kind_value, &
                                     is_allocatable=attr_info%is_allocatable, &
                                     is_pointer=attr_info%is_pointer, &
                                     is_parameter=attr_info%is_parameter)
                    end if
                else
                    if (attr_info%has_global_dimensions) then
                        decl_index = push_multi_declaration( &
                                     arena, &
                                     type_spec%type_name, &
                                     var_names(1:var_count), &
                                     dimension_indices= &
                                     attr_info%global_dimension_indices, &
                                     is_allocatable=attr_info%is_allocatable, &
                                     is_pointer=attr_info%is_pointer, &
                                     is_parameter=attr_info%is_parameter)
                    else
                        decl_index = push_multi_declaration( &
                                     arena, &
                                     type_spec%type_name, &
                                     var_names(1:var_count), &
                                     is_allocatable=attr_info%is_allocatable, &
                                     is_pointer=attr_info%is_pointer, &
                                     is_parameter=attr_info%is_parameter)
                    end if
                end if

                if (decl_index > 0) then
                    allocate (decl_indices(1))
                    decl_indices(1) = decl_index
                    if (attr_info%has_global_dimensions) then
                        call register_type_annotation( &
                            decl_index, &
                            type_spec%type_name, &
                            var_names(1:var_count), &
                            has_kind=type_spec%has_kind, &
                            kind_value=type_spec%kind_value, &
                            is_parameter=attr_info%is_parameter, &
                            is_allocatable=attr_info%is_allocatable, &
                            is_pointer=attr_info%is_pointer, &
                            dimension_indices= &
                            attr_info%global_dimension_indices)
                    else
                        call register_type_annotation( &
                            decl_index, &
                            type_spec%type_name, &
                            var_names(1:var_count), &
                            has_kind=type_spec%has_kind, &
                            kind_value=type_spec%kind_value, &
                            is_parameter=attr_info%is_parameter, &
                            is_allocatable=attr_info%is_allocatable, &
                            is_pointer=attr_info%is_pointer)
                    end if
                else
                    allocate (decl_indices(0))
                end if
            end if
        end block
    end function parse_multi_declaration

end module parser_declarations_core_module
