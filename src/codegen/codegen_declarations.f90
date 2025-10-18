module codegen_declarations
    use iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
                              derived_type_node, intent_type_to_string, module_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_core, only: program_node, identifier_node, literal_node, &
                              assignment_node, &
                              array_literal_node, call_or_subscript_node
    use ast_nodes_misc, only: implicit_statement_node, contains_node, comment_node, &
                              blank_line_node, &
                              use_statement_node, interface_block_node, &
                              module_procedure_node
    use ast_nodes_loops, only: do_loop_node
    use type_system_unified
    use string_types, only: string_t
    use string_utils_mod, only: int_to_string
    use codegen_indent
    use codegen_utilities, only: parameter_info_t, &
                                 generate_grouped_body, &
                                 generate_grouped_body_with_params, &
                                 generate_grouped_body_context, find_parameter_info, &
                                 is_character_type_string, normalize_character_type, &
                                 normalize_character_type_param
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_type_utils, only: get_type_standardization
    implicit none
    private

    public :: generate_code_function_def
    public :: generate_code_subroutine_def
    public :: generate_code_declaration
    public :: generate_code_parameter_declaration
    public :: generate_code_module
    public :: generate_code_interface_block
    public :: generate_code_module_procedure
    public :: generate_code_derived_type
    public :: generate_code_program

contains

    ! Generate code for function definitions
    function generate_code_function_def(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: return_type_code, params_code, body_code
        character(len=:), allocatable :: return_type_override
        integer :: i
        logical :: recursive_in_prefix

        ! Start function definition with optional recursive keyword and return type
        code = ""
        recursive_in_prefix = .false.
        return_type_code = ""
        if (allocated(node%return_type)) then
            return_type_code = trim(node%return_type)
        end if

        call derive_character_return_type(arena, node, return_type_override)
        if (len_trim(return_type_override) > 0) then
            return_type_code = return_type_override
        end if

        if (len_trim(return_type_code) > 0) then
            if (should_omit_return_type(arena, node, return_type_code)) then
                return_type_code = ""
            else
                return_type_code = fix_character_len_placeholder(return_type_code)
            end if
        end if

        if (len_trim(return_type_code) > 0) then
            if (is_deferred_character_return(return_type_code)) then
                if (has_character_len_result_decl(arena, node)) then
                    return_type_code = ""
                end if
            end if
        end if

        if (allocated(node%prefix_keywords)) then
            do i = 1, size(node%prefix_keywords)
                if (len_trim(node%prefix_keywords(i)) > 0) then
                    code = code // trim(node%prefix_keywords(i)) // " "
                    if (trim(node%prefix_keywords(i)) == "recursive") then
                        recursive_in_prefix = .true.
                    end if
                end if
            end do
        end if
        if (node%is_recursive .and. .not. recursive_in_prefix) then
            code = "recursive " // code
        end if

        if (len_trim(return_type_code) > 0) then
            if (len(code) > 0) then
                code = code // return_type_code // " function " // node%name
            else
                code = return_type_code // " function " // node%name
            end if
        else
            if (len(code) > 0) then
                code = code // "function " // node%name
            else
                code = "function " // node%name
            end if
        end if

        ! Generate parameters (names only)
        if (allocated(node%param_indices) .and. size(node%param_indices) > 0) then
            code = code // "("
            do i = 1, size(node%param_indices)
                if (i > 1) code = code // ", "
                if (node%param_indices(i) > 0 .and. node%param_indices(i) <= &
                    arena%size) then
                    if (allocated(arena%entries(node%param_indices(i))%node)) then
                        select type (p => arena%entries(node%param_indices(i))%node)
                        type is (identifier_node)
                            code = code // p%name
                        type is (parameter_declaration_node)
                            code = code // p%name
                        type is (declaration_node)
                            code = code // p%var_name
                        class default
                            code = code // "param" // trim(adjustl(int_to_string(i)))
                        end select
                    end if
                end if
            end do
            code = code // ")"
        else
            code = code // "()"
        end if

        ! Add result clause if present (but NOT if result name equals function name)
        if (allocated(node%result_variable) .and. &
            len_trim(node%result_variable) > 0) then
            ! Don't add result() clause if result variable name equals function name
            ! (Fortran doesn't allow result(foo) for function foo - just use typed function signature)
            if (.not. (allocated(node%name) .and. trim(node%result_variable) == &
                       trim(node%name))) then
                code = code // " result(" // node%result_variable // ")"
            end if
        end if

        code = code // new_line('A')

        ! Build parameter map by matching parameter names to body declarations
        block
            type(parameter_info_t), allocatable :: param_map(:)
            integer :: param_count, j
            logical :: has_implicit

            param_count = 0
            if (allocated(node%param_indices)) param_count = size(node%param_indices)

            allocate (param_map(param_count))

            ! Initialize parameter map from parameter names
            do i = 1, param_count
                ! Initialize entry
                param_map(i)%name = ""
                param_map(i)%intent_str = ""
                param_map(i)%is_optional = .false.

                if (node%param_indices(i) > 0 .and. &
                    node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(node%param_indices(i))%node)) then
                        select type (param_node => &
                                     arena%entries(node%param_indices(i))%node)
                        type is (identifier_node)
                            param_map(i)%name = param_node%name
                        type is (parameter_declaration_node)
                            ! Get attributes directly from parameter node
                            param_map(i)%name = param_node%name
                            param_map(i)%intent_str = &
                                intent_type_to_string(param_node%intent_type)
                            param_map(i)%is_optional = param_node%is_optional
                        end select
                    end if
                end if
            end do

            ! Find parameter attributes in body declarations
            if (allocated(node%body_indices)) then
                do j = 1, size(node%body_indices)
                    if (node%body_indices(j) > 0 .and. &
                        node%body_indices(j) <= arena%size) then
                        if (allocated(arena%entries(node%body_indices(j))%node)) then
                            select type (body_node => &
                                         arena%entries(node%body_indices(j))%node)
                            type is (parameter_declaration_node)
                                ! Find matching parameter in param_map
                                do i = 1, param_count
                                    if (allocated(param_map(i)%name) .and. &
                                        param_map(i)%name == body_node%name) then
                                        param_map(i)%intent_str = &
                                            intent_type_to_string(body_node%intent_type)
                                        param_map(i)%is_optional = body_node%is_optional
                                    end if
                                end do
                            type is (declaration_node)
                                ! Check if this declaration matches a parameter
                                do i = 1, param_count
                                    if (allocated(param_map(i)%name) .and. &
                                        param_map(i)%name == body_node%var_name) then
                                        ! Update intent if present
                                        if (body_node%has_intent) then
                                            param_map(i)%intent_str = body_node%intent
                                        end if
                                        ! Always update optional flag
                                        param_map(i)%is_optional = body_node%is_optional
                                    end if
                                end do
                            end select
                        end if
                    end if
                end do
            end if

            ! Ensure pure/elemental functions default parameters to intent(in)
            if (allocated(node%prefix_keywords)) then
                do j = 1, size(node%prefix_keywords)
                    select case (trim(node%prefix_keywords(j)))
                    case ("pure", "elemental")
                        do i = 1, param_count
                            if (allocated(param_map(i)%name)) then
                                if (len_trim(param_map(i)%intent_str) == 0) then
                                    param_map(i)%intent_str = "in"
                                end if
                            end if
                        end do
                    end select
                end do
            end if

            ! Add implicit none to function (quality requirement for lazy Fortran)
            has_implicit = .false.
            if (allocated(node%body_indices)) then
                do j = 1, size(node%body_indices)
                    if (node%body_indices(j) > 0 .and. node%body_indices(j) <= &
                        arena%size) then
                        if (allocated(arena%entries(node%body_indices(j))%node)) then
                            select type (body_node => &
                                         arena%entries(node%body_indices(j))%node)
                            type is (implicit_statement_node)
                                if (body_node%is_none) has_implicit = .true.
                            end select
                        end if
                    end if
                end do
            end if
            if (.not. has_implicit) then
                code = code // "    implicit none" // new_line('A')
            end if

            ! Add declarations for any undeclared parameters
            block
                character(len=:), allocatable :: param_decls
                param_decls = collect_function_parameter_decls(arena, node, param_map)
                if (len_trim(param_decls) > 0) then
                    code = code // param_decls
                end if
            end block

            ! Generate body with indentation, declaration grouping, and parameter mapping
            if (allocated(node%body_indices)) then
                code = code // generate_grouped_body_with_params(arena, &
                                                                 node%body_indices, 1, &
                                                                 param_map, node)
            end if
        end block

        ! End function
        code = code // "end function " // node%name
    end function generate_code_function_def

    logical function should_omit_return_type(arena, node, return_type_code) result(omit)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=*), intent(in) :: return_type_code
        character(len=:), allocatable :: result_name
        character(len=:), allocatable :: lowered_return
        integer :: i, decl_index

        omit = .false.
        result_name = ""
        if (allocated(node%result_variable)) then
            result_name = trim(node%result_variable)
        end if
        if (len_trim(result_name) == 0 .and. allocated(node%name)) then
            result_name = trim(node%name)
        end if
        if (len_trim(result_name) == 0) return

        if (.not. allocated(node%name)) return

        if (.not. allocated(node%body_indices)) return

        lowered_return = to_lower_ascii_str(trim(return_type_code))
        if (len_trim(lowered_return) == 0) return

        do i = 1, size(node%body_indices)
            decl_index = node%body_indices(i)
            if (decl_index <= 0 .or. decl_index > arena%size) cycle
            if (.not. allocated(arena%entries(decl_index)%node)) cycle
            select type (decl => arena%entries(decl_index)%node)
            type is (declaration_node)
                if (trim(decl%var_name) /= trim(result_name)) cycle
                if (.not. decl%is_array) then
                    if (.not. allocated(decl%dimension_indices)) cycle
                    if (size(decl%dimension_indices) == 0) cycle
                end if
                omit = .true.
                return
            end select
        end do
    end function should_omit_return_type

    ! Generate code for subroutine definitions
    function generate_code_subroutine_def(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: params_code
        integer :: i

        ! Start subroutine definition
        code = "subroutine " // node%name

        ! Generate parameters (names only)
        if (allocated(node%param_indices) .and. size(node%param_indices) > 0) then
            code = code // "("
            do i = 1, size(node%param_indices)
                if (i > 1) code = code // ", "
                if (node%param_indices(i) > 0 .and. node%param_indices(i) <= &
                    arena%size) then
                    if (allocated(arena%entries(node%param_indices(i))%node)) then
                        select type (p => arena%entries(node%param_indices(i))%node)
                        type is (identifier_node)
                            code = code // p%name
                        type is (parameter_declaration_node)
                            code = code // p%name
                        type is (declaration_node)
                            code = code // p%var_name
                        class default
                            code = code // "param" // trim(adjustl(int_to_string(i)))
                        end select
                    end if
                end if
            end do
            code = code // ")"
        else
            code = code // "()"
        end if
        code = code // new_line('A')

        ! Build parameter map by matching parameter names to body declarations
        block
            type(parameter_info_t), allocatable :: param_map(:)
            integer :: param_count, j

            param_count = 0
            if (allocated(node%param_indices)) param_count = size(node%param_indices)

            allocate (param_map(param_count))

            ! Initialize parameter map from parameter names
            do i = 1, param_count
                ! Initialize entry
                param_map(i)%name = ""
                param_map(i)%intent_str = ""
                param_map(i)%is_optional = .false.

                if (node%param_indices(i) > 0 .and. &
                    node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(node%param_indices(i))%node)) then
                        select type (param_node => &
                                     arena%entries(node%param_indices(i))%node)
                        type is (identifier_node)
                            param_map(i)%name = param_node%name
                        type is (parameter_declaration_node)
                            ! Get attributes directly from parameter node
                            param_map(i)%name = param_node%name
                            param_map(i)%intent_str = &
                                intent_type_to_string(param_node%intent_type)
                            param_map(i)%is_optional = param_node%is_optional
                        end select
                    end if
                end if
            end do

            ! Find parameter attributes in body declarations
            if (allocated(node%body_indices)) then
                do j = 1, size(node%body_indices)
                    if (node%body_indices(j) > 0 .and. &
                        node%body_indices(j) <= arena%size) then
                        if (allocated(arena%entries(node%body_indices(j))%node)) then
                            select type (body_node => &
                                         arena%entries(node%body_indices(j))%node)
                            type is (parameter_declaration_node)
                                ! Find matching parameter in param_map
                                do i = 1, param_count
                                    if (allocated(param_map(i)%name) .and. &
                                        param_map(i)%name == body_node%name) then
                                        param_map(i)%intent_str = &
                                            intent_type_to_string(body_node%intent_type)
                                        param_map(i)%is_optional = body_node%is_optional
                                    end if
                                end do
                            type is (declaration_node)
                                ! Check if this declaration matches a parameter
                                do i = 1, param_count
                                    if (allocated(param_map(i)%name) .and. &
                                        param_map(i)%name == body_node%var_name) then
                                        ! Update intent if present
                                        if (body_node%has_intent) then
                                            param_map(i)%intent_str = body_node%intent
                                        end if
                                        ! Always update optional flag
                                        param_map(i)%is_optional = body_node%is_optional
                                    end if
                                end do
                            end select
                        end if
                    end if
                end do
            end if

            ! Generate body with indentation, declaration grouping, and parameter mapping
            if (allocated(node%body_indices)) then
                code = code // generate_grouped_body_with_params(arena, &
                                                                 node%body_indices, 1, &
                                                                 param_map, node)
            end if
        end block

        ! End subroutine
        code = code // "end subroutine " // node%name
    end function generate_code_subroutine_def

    ! Generate code for declarations
    function generate_code_declaration(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: init_code, type_str
        integer :: i, j
        logical :: standardize_types_enabled
        logical :: has_dimension_attr

        ! Get type standardization setting
        call get_type_standardization(standardize_types_enabled)

        ! Determine the type string
        if (len_trim(node%type_name) > 0) then
            type_str = node%type_name
        else if (node%inferred_type%kind > 0) then
            ! Handle type inference
            select case (node%inferred_type%kind)
            case (TINT)
                type_str = "integer"
            case (TREAL)
                if (standardize_types_enabled) then
                    type_str = "real(8)"
                else
                    type_str = "real"
                end if
            case (TCHAR)
                if (node%inferred_type%alloc_info%needs_allocatable_string) then
                    type_str = "character(len=:)"
                else if (node%inferred_type%size > 0) then
                    type_str = "character(len=" // &
                        trim(adjustl(int_to_string(node%inferred_type%size))) // ")"
                else
                    ! For zero-length or unknown strings, use explicit length 0
                    ! character(*) is only valid in parameter declarations
                    type_str = "character(len=0)"
                end if
            case (TLOGICAL)
                type_str = "logical"
            case (TCOMPLEX)
                type_str = "complex"
            case (TDOUBLE)
                type_str = "double precision"
            case (TDERIVED)
                ! For derived types, use the type name from the node
                if (len_trim(node%type_name) > 0) then
                    type_str = node%type_name
                else
                    type_str = "type(unknown_t)"  ! Fallback for derived types
                end if
            case default
                type_str = "real"  ! Default to real
            end select
        else
            type_str = "real"  ! Default fallback
        end if

        if (is_character_type_string(type_str)) then
            if (index(to_lower_ascii_str(trim(type_str)), "len=)") > 0) then
                if (node%has_kind) then
                    select case (node%kind_value)
                    case (-1)
                        type_str = "character(len=*)"
                    case default
                        if (node%kind_value > 0) then
                            type_str = "character(len=" // &
                                trim(adjustl(int_to_string(node%kind_value))) // ")"
                        end if
                    end select
                end if
            end if
        end if

        ! Normalize character type representations to preserve length specifiers
        if (is_character_type_string(type_str) .or. node%inferred_type%kind == &
            TCHAR) then
            type_str = normalize_character_type(node, type_str)
        end if

        select case (trim(type_str))
        case ("character(len=))", "character(len=)")
            type_str = "character(len=*)"
        end select

        ! Generate basic declaration
        code = type_str
        code = fix_character_len_placeholder(code)

        ! Add kind if present and valid (>0) for non-character types
        if (node%has_kind .and. node%kind_value > 0) then
            if (.not. is_character_type_string(code)) then
                code = code // "(" // &
                       trim(adjustl(int_to_string(node%kind_value))) // ")"
            end if
        end if

        ! Add intent if present
        if (node%has_intent .and. allocated(node%intent)) then
            code = code // ", intent(" // node%intent // ")"
        end if

        ! Add allocatable if present or if string needs allocatable
        if (node%is_allocatable) then
            if (index(to_lower_ascii_decl(trim(code)), 'allocatable') == 0) then
                code = code // ", allocatable"
            end if
        else if (node%inferred_type%kind > 0) then
            if (node%inferred_type%alloc_info%needs_allocatable_string) then
                code = code // ", allocatable"
            end if
        end if

        ! Add optional if present
        if (node%is_optional) then
            code = code // ", optional"
        end if

        ! Add pointer if present
        if (node%is_pointer) then
            if (index(to_lower_ascii_decl(trim(code)), 'pointer') == 0) then
                code = code // ", pointer"
            end if
        end if

        ! Add target if present
        if (node%is_target) then
            if (index(to_lower_ascii_decl(trim(code)), 'target') == 0) then
                code = code // ", target"
            end if
        end if

        if (node%is_external) then
            if (index(to_lower_ascii_decl(trim(code)), 'external') == 0) then
                code = code // ", external"
            end if
        end if

        ! Add parameter if present
        if (node%is_parameter) then
            if (index(to_lower_ascii_decl(trim(code)), 'parameter') == 0) then
                code = code // ", parameter"
            end if
        end if

        has_dimension_attr = index(to_lower_ascii_decl(trim(type_str)), "dimension(") > 0

        ! Add variable names - handle both single and multi declarations
        code = code // " :: "
        if (node%is_multi_declaration .and. allocated(node%var_names)) then
            ! Multi-variable declaration
            do i = 1, size(node%var_names)
                if (i > 1) code = code // ", "
                code = code // trim(node%var_names(i))
                ! Add dimensions per variable if needed
                if (node%is_array .and. allocated(node%dimension_indices) .and. &
                    .not. has_dimension_attr) then
                    code = trim(code)
                    code = code // "("
                    do j = 1, size(node%dimension_indices)
                        if (j > 1) code = code // ","
                        if (node%dimension_indices(j) > 0 .and. &
                            node%dimension_indices(j) <= arena%size) then
                            code = code // generate_code_from_arena(arena, &
                                                                node%dimension_indices(j))
                        else
                            code = code // ":"  ! Default for unspecified dimensions
                        end if
                    end do
                    code = code // ")"
                end if
            end do
        else
            ! Single variable declaration
            code = code // node%var_name

            ! Add array dimensions if present
            if (node%is_array .and. allocated(node%dimension_indices) .and. &
                .not. has_dimension_attr) then
                ! Generate dimension expressions
                code = trim(code)
                code = code // "("
                do i = 1, size(node%dimension_indices)
                    if (i > 1) code = code // ","
                    if (node%dimension_indices(i) > 0 .and. &
                        node%dimension_indices(i) <= arena%size) then
                        ! Valid arena index
                        code = code // generate_code_from_arena(arena, &
                                                                node%dimension_indices(i))
                    else if (node%dimension_indices(i) > arena%size) then
                        ! Direct integer value (for inferred dimensions)
                        code = code // int_to_string(node%dimension_indices(i))
                    else
                        code = code // ":"  ! Default for unspecified dimensions (allocatable)
                    end if
                end do
                code = code // ")"
            end if
        end if

        ! Add initializer if present
        if (node%initializer_index > 0 .and. node%initializer_index <= arena%size) then
            init_code = generate_code_from_arena(arena, node%initializer_index)
            if (node%is_pointer) then
                if (to_lower_ascii_decl(trim(init_code)) == "null") then
                    init_code = "null()"
                end if
                code = code // " => " // init_code
            else
                code = code // " = " // init_code
            end if
        end if

        code = fix_character_len_placeholder(code)
    contains

        pure function to_lower_ascii_decl(text) result(lower_text)
            character(len=*), intent(in) :: text
            character(len=len(text)) :: lower_text
            integer :: idx
            integer :: char_code

            lower_text = text
            do idx = 1, len(text)
                char_code = iachar(lower_text(idx:idx))
                if (char_code >= iachar('A') .and. char_code <= iachar('Z')) then
                    lower_text(idx:idx) = achar(char_code + iachar('a') - iachar('A'))
                end if
            end do
        end function to_lower_ascii_decl
    end function generate_code_declaration

    ! Generate code for parameter declarations
    function generate_code_parameter_declaration(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(parameter_declaration_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: intent_str
        integer :: j

        ! Check if this node has a parent that needs just the name (parameter list)
        ! vs full declaration (in body). For now, generate full declaration when
        ! the node has type and attributes.
        if (len_trim(node%type_name) > 0) then
            ! Generate full declaration (when in body)
            code = node%type_name

            if (is_character_type_string(code)) then
                code = normalize_character_type_param(code, node%has_kind, &
                                                      node%kind_value)
            else if (node%has_kind .and. node%kind_value > 0) then
                code = code // "(" // &
                       trim(adjustl(int_to_string(node%kind_value))) // ")"
            end if

            ! Add intent attribute
            intent_str = intent_type_to_string(node%intent_type)
            if (len_trim(intent_str) > 0) then
                code = code // ", intent(" // intent_str // ")"
            end if

            ! Add optional attribute
            if (node%is_optional) then
                code = code // ", optional"
            end if

            code = code // " :: " // node%name

            ! Add dimensions if present
            if (allocated(node%dimension_indices) .and. &
                size(node%dimension_indices) > 0) then
                code = code // "("
                do j = 1, size(node%dimension_indices)
                    if (j > 1) code = code // ", "
                    code = code // generate_code_from_arena(arena, &
                                                            node%dimension_indices(j))
                end do
                code = code // ")"
            end if
        else
            ! Just emit the name (when in parameter list)
            code = node%name
        end if

        code = fix_character_len_placeholder(code)
    end function generate_code_parameter_declaration

    ! Generate code for modules
    function generate_code_module(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: body_code
        integer :: i
        logical :: has_implicit

        ! Module header
        code = "module " // node%name // new_line('A')

        ! Ensure module includes implicit none (quality requirement for lazy Fortran)
        has_implicit = .false.
        if (allocated(node%declaration_indices)) then
            do i = 1, size(node%declaration_indices)
                if (node%declaration_indices(i) > 0 .and. &
                    node%declaration_indices(i) <= &
                    arena%size) then
                    if (allocated(arena%entries(node%declaration_indices(i))%node)) then
                        select type (decl => &
                                     arena%entries(node%declaration_indices(i))%node)
                        type is (implicit_statement_node)
                            if (decl%is_none) then
                                has_implicit = .true.
                                exit
                            end if
                        type is (literal_node)
                            if (allocated(decl%value)) then
                                if (index(decl%value, 'implicit none') > 0) then
                                    has_implicit = .true.
                                    exit
                                end if
                            end if
                        end select
                    end if
                end if
            end do
        end if
        if (.not. has_implicit) then
            code = code // "    implicit none" // new_line('A')
        end if

        ! Generate module declarations
        if (allocated(node%declaration_indices)) then
            body_code = generate_grouped_body(arena, node%declaration_indices, 1)
            if (len(body_code) > 0) then
                code = code // body_code
            end if
        end if

        ! Check for contains section
        if (node%has_contains .and. allocated(node%procedure_indices)) then
            code = code // "contains" // new_line('A')

            ! Generate contained procedures
            do i = 1, size(node%procedure_indices)
                if (node%procedure_indices(i) > 0 .and. &
                    node%procedure_indices(i) <= arena%size) then
                    body_code = generate_code_from_arena(arena, &
                                                         node%procedure_indices(i))
                    if (len(body_code) > 0) then
                        ! Add proper indentation for contained procedures
                        code = code // "    " // body_code
                        if (i < size(node%procedure_indices)) then
                            code = code // new_line('A') // new_line('A')
                        else
                            code = code // new_line('A')
                        end if
                    end if
                end if
            end do
        end if

        ! Module end
        code = code // "end module " // node%name
    end function generate_code_module

    function generate_code_interface_block(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(interface_block_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: body_code

        code = "interface"
        if (allocated(node%name)) then
            if (len_trim(node%name) > 0) code = code // " " // trim(node%name)
        end if
        code = code // new_line('A')

        if (allocated(node%procedure_indices)) then
            body_code = generate_grouped_body(arena, node%procedure_indices, 1)
            if (len(body_code) > 0) code = code // body_code
        end if

        code = code // "end interface"
        if (allocated(node%name)) then
            if (len_trim(node%name) > 0) code = code // " " // trim(node%name)
        end if
    end function generate_code_interface_block

    function generate_code_module_procedure(node) result(code)
        type(module_procedure_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i
        character(len=:), allocatable :: name_text
        logical :: first_name

        code = "module procedure"
        first_name = .true.
        if (allocated(node%procedure_names)) then
            do i = 1, size(node%procedure_names)
                if (.not. allocated(node%procedure_names(i)%s)) cycle
                name_text = trim(node%procedure_names(i)%s)
                if (len_trim(name_text) == 0) cycle
                if (first_name) then
                    code = code // " " // name_text
                    first_name = .false.
                else
                    code = code // ", " // name_text
                end if
            end do
        end if
    end function generate_code_module_procedure

    ! Generate code for derived types
    function generate_code_derived_type(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(derived_type_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: component_code
        character(len=:), allocatable :: header_clause
        integer :: i

        ! Type definition header
        if (node%has_attributes .and. allocated(node%attribute_clause) .and. &
            len_trim(node%attribute_clause) > 0) then
            header_clause = ""
            do i = 1, len_trim(node%attribute_clause)
                header_clause = header_clause // node%attribute_clause(i:i)
                if (node%attribute_clause(i:i) == "," .and. i < &
                    len_trim(node%attribute_clause)) then
                    if (node%attribute_clause(i + 1:i + 1) /= " " .and. &
                        node%attribute_clause(i + 1:i + 1) /= new_line('A')) then
                        header_clause = header_clause // " "
                    end if
                end if
            end do

            if (header_clause(1:1) == ",") then
                code = "type" // header_clause // " :: " // node%name // &
                       new_line('A')
            else
                code = "type " // trim(header_clause) // " :: " // node%name // &
                       new_line('A')
            end if
        else
            code = "type :: " // node%name // new_line('A')
        end if

        ! Generate components
        if (allocated(node%component_indices)) then
            do i = 1, size(node%component_indices)
                if (node%component_indices(i) > 0 .and. &
                    node%component_indices(i) <= arena%size) then
                    if (.not. &
                        allocated(arena%entries(node%component_indices(i))%node)) cycle
                    select type (child => arena%entries(node%component_indices(i))%node)
                    type is (derived_type_node)
                        cycle
                    class default
                        component_code = generate_code_from_arena(arena, &
                                                                node%component_indices(i))
                    end select
                    if (len_trim(component_code) == 0) cycle
                    code = code // "    " // component_code // new_line('A')
                end if
            end do
        end if

        ! Type definition end
        code = code // "end type " // node%name
    end function generate_code_derived_type

    ! Generate code for program nodes
    function generate_code_program(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: body_code
        integer :: i, j
        logical :: in_contains_section
        logical :: found_contains
        logical :: has_non_trivial_body
        logical :: context_has_executable_before_contains
        integer, allocatable :: non_use_indices(:)
        integer :: non_use_count

        context_has_executable_before_contains = .false.
        non_use_count = 0

        ! Check if there's a non-trivial body before contains
        has_non_trivial_body = .false.
        found_contains = .false.
        if (allocated(node%body_indices)) then
            do i = 1, size(node%body_indices)
                if (node%body_indices(i) > 0 .and. node%body_indices(i) <= &
                    arena%size) then
                    if (allocated(arena%entries(node%body_indices(i))%node)) then
                        select type (body_node => &
                                     arena%entries(node%body_indices(i))%node)
                        type is (contains_node)
                            found_contains = .true.
                            exit
                        type is (comment_node)
                            ! Comments don't count as non-trivial
                        type is (blank_line_node)
                            ! Blank lines don't count as non-trivial
                        class default
                            has_non_trivial_body = .true.
                        end select
                    end if
                end if
            end do
        end if

        context_has_executable_before_contains = has_non_trivial_body .and. &
                                                 found_contains

        ! Handle special multi-unit container
        if (node%name == "__MULTI_UNIT__") then
            ! Generate code for each unit as siblings without program wrapper
            code = ""
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    if (node%body_indices(i) > 0 .and. node%body_indices(i) <= &
                        arena%size) then
                        if (allocated(arena%entries(node%body_indices(i))%node)) then
                            select type (child => &
                                         arena%entries(node%body_indices(i))%node)
                            type is (program_node)
                                ! Skip trivial implicit main wrappers that only contain comments/blank lines
                                if (program_is_trivial_wrapper(arena, &
                                                               node%body_indices(i), &
                                                               child%name)) then
                                    block
                                        character(len=:), allocatable :: trivia_code
                                        trivia_code = &
                                            collect_trivial_program_trivia(arena, &
                                                                     node%body_indices(i))
                                        if (len_trim(trivia_code) > 0) then
                                            if (len(code) > 0) code = code // &
                                                                      new_line('A') // &
                                                                      new_line('A')
                                            code = code // trivia_code
                                        end if
                                    end block
                                    cycle
                                end if
                            type is (subroutine_def_node)
                                ! Skip duplicate empty subroutines (defensive check)
                                if (.not. allocated(child%body_indices) .or. &
                                    size(child%body_indices) == 0) then
                                    if (.not. allocated(child%param_indices) .or. &
                                        size(child%param_indices) == 0) then
                                        ! Check if this is a duplicate of a previous subroutine
                                        block
                                            integer :: j
                                            logical :: is_duplicate
                                            is_duplicate = .false.
                                            do j = 1, i - 1
                                                if (node%body_indices(j) > 0 .and. &
                                                    node%body_indices(j) <= &
                                                    arena%size) then
                             if (allocated(arena%entries(node%body_indices(j))%node)) then
                                                        select type (prev => &
                                                 arena%entries(node%body_indices(j))%node)
                                                        type is (subroutine_def_node)
                                                            if (prev%name == &
                                                                child%name) then
                                                                is_duplicate = .true.
                                                                exit
                                                            end if
                                                        end select
                                                    end if
                                                end if
                                            end do
                                            if (is_duplicate) cycle
                                        end block
                                    end if
                                end if
                            end select
                        end if
                        if (len(code) > 0) then
                            code = code // new_line('A') // new_line('A')
                        end if
                        code = code // generate_code_from_arena(arena, &
                                                                node%body_indices(i))
                    end if
                end do
            end if
            return
        end if

        ! Program header
        code = "program " // node%name // new_line('A')

        ! Process use statements first, then add implicit none, then rest of body
        block
            logical :: has_implicit
            logical :: is_use_stmt
            character(len=:), allocatable :: use_statements_code
            character(len=:), allocatable :: loop_var_declarations
            character(len=:), allocatable :: extra_decls

            has_implicit = .false.
            use_statements_code = ""
            loop_var_declarations = ""

            ! First pass: collect use statements and check for implicit none
            if (allocated(node%body_indices)) then
                allocate (non_use_indices(size(node%body_indices)))
                non_use_count = 0

                do i = 1, size(node%body_indices)
                    if (node%body_indices(i) > 0 .and. node%body_indices(i) <= &
                        arena%size) then
                        if (allocated(arena%entries(node%body_indices(i))%node)) then
                            is_use_stmt = .false.

                            select type (ib => arena%entries(node%body_indices(i))%node)
                            type is (use_statement_node)
                                ! Generate use statement code
                                is_use_stmt = .true.
                                use_statements_code = use_statements_code // "    " // &
                                                      generate_code_from_arena(arena, &
                                                                node%body_indices(i)) // &
                                                      new_line('A')

                            type is (implicit_statement_node)
                                if (ib%is_none) has_implicit = .true.
                                non_use_count = non_use_count + 1
                                non_use_indices(non_use_count) = node%body_indices(i)

                            type is (literal_node)
                                if (allocated(ib%value)) then
                                    if (index(ib%value, 'implicit none') > 0) &
                                        has_implicit = .true.
                                end if
                                non_use_count = non_use_count + 1
                                non_use_indices(non_use_count) = node%body_indices(i)

                            class default
                                non_use_count = non_use_count + 1
                                non_use_indices(non_use_count) = node%body_indices(i)
                            end select

                            ! Don't add use statements to non_use_indices
                            if (is_use_stmt) then
                                ! Use statement already processed, skip
                            end if
                        end if
                    end if
                end do
            end if

            ! Add use statements first
            if (len(use_statements_code) > 0) then
                code = code // use_statements_code
            end if

            ! Then add implicit none if not present
            if (.not. has_implicit) then
                code = code // "    implicit none" // new_line('A')
            end if

            ! Collect and add variable declarations for undeclared identifiers
            extra_decls = collect_program_variable_decls(arena, node)
            if (len_trim(extra_decls) > 0) then
                code = code // extra_decls
            end if
        end block

        ! Generate rest of body (non-use statements) with proper grouping
        if (allocated(node%body_indices) .and. non_use_count > 0) then
            body_code = generate_grouped_body_with_context(arena, &
                                                       non_use_indices(1:non_use_count), &
                                                           1, &
                                                   context_has_executable_before_contains)

            if (index(body_code, 'output_unit') > 0) then
                block
                    integer :: search_pos
                    integer :: iso_pos
                    integer :: line_start
                    integer :: line_end
                    integer :: header_end
                    integer :: comment_pos
                    logical :: has_iso_line
                    logical :: iso_has_only
                    logical :: iso_has_output
                    character(len=:), allocatable :: prefix
                    character(len=:), allocatable :: suffix
                    character(len=:), allocatable :: iso_line
                    character(len=:), allocatable :: iso_comment
                    character(len=:), allocatable :: trimmed_line

                    has_iso_line = .false.
                    search_pos = 1

                    do
                        iso_pos = index(code(search_pos:), 'iso_fortran_env')
                        if (iso_pos == 0) exit
                        iso_pos = search_pos + iso_pos - 1

                        line_start = iso_pos
                        do while (line_start > 1 .and. code(line_start - &
                                                            1:line_start - 1) &
                                  /= new_line('A'))
                            line_start = line_start - 1
                        end do

                        line_end = iso_pos
                        do while (line_end <= len(code) .and. code(line_end:line_end) &
                                  /= new_line('A'))
                            line_end = line_end + 1
                        end do

                        has_iso_line = .true.

                        if (line_end > len(code)) then
                            iso_line = code(line_start:)
                        else
                            iso_line = code(line_start:line_end - 1)
                        end if

                        iso_has_only = index(to_lower_ascii_local(iso_line), 'only:') > 0
                        iso_has_output = index(to_lower_ascii_local(iso_line), &
                                               'output_unit') > 0

                        if (iso_has_only .and. .not. iso_has_output) then
                            if (line_start > 1) then
                                prefix = code(1:line_start - 1)
                            else
                                prefix = ''
                            end if

                            if (line_end <= len(code)) then
                                if (line_end < len(code)) then
                                    suffix = code(line_end + 1:)
                                else
                                    suffix = ''
                                end if
                            else
                                suffix = ''
                            end if

                            comment_pos = scan(iso_line, '!')
                            if (comment_pos > 0) then
                                if (comment_pos > 1) then
                                    trimmed_line = iso_line(1:comment_pos - 1)
                                else
                                    trimmed_line = ''
                                end if
                                iso_comment = iso_line(comment_pos:)
                            else
                                trimmed_line = iso_line
                                iso_comment = ''
                            end if

                            if (len_trim(trimmed_line) > 0) then
                                trimmed_line = trimmed_line(1:len_trim(trimmed_line))
                            end if

                            iso_line = trimmed_line // ', output_unit'
                            if (len_trim(iso_comment) > 0) then
                                iso_line = iso_line // ' ' // iso_comment
                            end if

                            code = prefix // iso_line // new_line('A') // suffix
                            iso_has_output = .true.
                        end if

                        if (.not. iso_has_only .or. iso_has_output) exit

                        if (line_end <= len(code)) then
                            search_pos = line_end + 1
                        else
                            exit
                        end if
                    end do

                    if (.not. has_iso_line) then
                        header_end = index(code, new_line('A'))
                        if (header_end <= 0) header_end = len(code)

                        if (header_end > 0) then
                            prefix = code(1:header_end)
                        else
                            prefix = ''
                        end if

                        if (header_end < len(code)) then
                            suffix = code(header_end + 1:)
                        else
                            suffix = ''
                        end if

                        code = prefix // &
                           '    use, intrinsic :: iso_fortran_env, only: output_unit' // &
                               new_line('A') // suffix
                    end if
                end block
            end if

            ! Check if body contains implied do loops and add loop variables after implicit none
            if (len(body_code) > 0) then
                block
                    integer :: pos, start_pos, end_pos, impl_pos, insert_pos
                    character(len=:), allocatable :: before_code, after_code, var_name
                    character(len=:), allocatable :: loop_vars(:)
                    integer :: n_vars, i, j
                    logical :: already_declared

                    ! Find all implied do loop variables
                    allocate (character(len=32) :: loop_vars(20))  ! Support up to 20 loop variables
                    n_vars = 0

                    ! Search for patterns like "(var=" in implied do loops (both old and new syntax)
                    pos = 1
                    do while (pos <= len(body_code))
                        ! Find next occurrence of either "= (/(" or "= [(", with or without spaces
                        start_pos = index(body_code(pos:), "= (/(")
                        if (start_pos == 0) then
                            start_pos = index(body_code(pos:), "= (/ (")
                        end if
                        if (start_pos == 0) then
                            ! Try new syntax
                            start_pos = index(body_code(pos:), "= [(")
                            if (start_pos > 0) then
                                start_pos = pos + start_pos - 1
                                ! Find the end with "]" for new syntax
                                end_pos = index(body_code(start_pos:), ")]")
                                if (end_pos > 0) then
                                    end_pos = start_pos + end_pos - 1
                                    ! Extract variables from this implied do section
                       call extract_loop_vars_from_section(body_code(start_pos:end_pos), &
                                                                        loop_vars, &
                                                                        n_vars)
                                end if
                                pos = start_pos + 3  ! Move past "= [("
                            else
                                exit  ! No more patterns found
                            end if
                        else
                            start_pos = pos + start_pos - 1
                            ! Find the loop variable patterns for old syntax
                            end_pos = index(body_code(start_pos:), "/)")
                            if (end_pos > 0) then
                                end_pos = start_pos + end_pos - 1
                                ! Extract variables from this implied do section
                       call extract_loop_vars_from_section(body_code(start_pos:end_pos), &
                                                                    loop_vars, n_vars)
                            end if
                            pos = start_pos + 5  ! Move past "= (/("
                        end if
                    end do

                    ! If we found loop variables, add declarations
                    if (n_vars > 0 .or. (index(body_code, "[(") > 0 .and. &
                                         index(body_code, ")]") > 0)) then
                        ! Check if implicit none is in body_code
                        impl_pos = index(body_code, "implicit none")
                        if (impl_pos > 0) then
                            ! Find the end of the implicit none line
                            insert_pos = impl_pos + 13  ! Length of "implicit none"
                            do while (insert_pos <= len(body_code))
                                if (body_code(insert_pos:insert_pos) == &
                                    new_line('A')) then
                                    insert_pos = insert_pos + 1
                                    exit
                                end if
                                insert_pos = insert_pos + 1
                            end do

                            ! Build declarations for loop variables
                            before_code = body_code(1:insert_pos - 1)
                            after_code = body_code(insert_pos:)

                            if (n_vars > 0) then
                                do i = 1, n_vars
                                    ! Skip if already declared
                                    already_declared = .false.
                                    if (index(body_code, &
                                              "integer :: "//trim(loop_vars(i))) > 0) then
                                        already_declared = .true.
                                    end if

                                    if (.not. already_declared) then
                                        before_code = before_code // &
                                                      "    integer :: " // &
                                                      trim(loop_vars(i)) // new_line('A')
                                    end if
                                end do
                            else
                                ! Check for implied do with default i
                                if (index(body_code, "[(") > 0 .and. index(body_code, &
                                                                           ")]") > 0) then
                                    if (index(body_code, "integer :: i") == 0) then
                                        before_code = before_code // "    integer :: i" &
                                                      // new_line('A')
                                    end if
                                end if
                            end if

                            body_code = before_code // after_code
                        else
                            ! No implicit none in body, add to code as before
                            if (n_vars > 0) then
                                do i = 1, n_vars
                                    already_declared = .false.
                                    if (index(body_code, &
                                              "integer :: "//trim(loop_vars(i))) > 0) then
                                        already_declared = .true.
                                    end if
                                    if (index(code, &
                                              "integer :: "//trim(loop_vars(i))) > 0) then
                                        already_declared = .true.
                                    end if

                                    if (.not. already_declared) then
                                        code = code // "    integer :: " // &
                                               trim(loop_vars(i)) // new_line('A')
                                    end if
                                end do
                            else
                                if (index(body_code, "[(") > 0 .and. index(body_code, &
                                                                           ")]") > 0) then
                                    if (index(body_code, "integer :: i") == 0 .and. &
                                        index(code, "integer :: i") == 0) then
                                        code = code // "    integer :: i" // &
                                               new_line('A')
                                    end if
                                end if
                            end if
                        end if
                    end if
                end block
            end if

            code = code // body_code
        end if

        if (allocated(non_use_indices)) then
            deallocate (non_use_indices)
        end if

        ! Program end
        code = code // "end program " // node%name
    contains

        pure function to_lower_ascii_local(text) result(lower_text)
            character(len=*), intent(in) :: text
            character(len=len(text)) :: lower_text
            integer :: i
            integer :: char_code

            lower_text = text
            do i = 1, len(text)
                char_code = iachar(lower_text(i:i))
                if (char_code >= iachar('A') .and. char_code <= iachar('Z')) then
                    lower_text(i:i) = achar(char_code + 32)
                end if
            end do
        end function to_lower_ascii_local

    end function generate_code_program

    subroutine derive_character_return_type(arena, node, override)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable, intent(out) :: override
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: target_name
        integer :: i, decl_index

        override = ""

        if (allocated(node%return_type)) then
            lowered = to_lower_ascii_str(trim(node%return_type))
            if (index(lowered, "character(len=:), allocatable") == 0) return
        else
            return
        end if

        if (allocated(node%result_variable)) then
            if (len_trim(node%result_variable) > 0) then
                target_name = trim(node%result_variable)
            else
                target_name = trim(node%name)
            end if
        else
            target_name = trim(node%name)
        end if

        if (.not. allocated(node%body_indices)) return
        do i = 1, size(node%body_indices)
            decl_index = node%body_indices(i)
            if (decl_index <= 0 .or. decl_index > arena%size) cycle
            if (.not. allocated(arena%entries(decl_index)%node)) cycle
            select type (stmt => arena%entries(decl_index)%node)
            type is (declaration_node)
                if (len_trim(stmt%var_name) == 0) cycle
                if (trim(stmt%var_name) /= target_name) cycle
                if (.not. allocated(stmt%type_name)) cycle
                lowered = to_lower_ascii_str(trim(stmt%type_name))
                if (index(lowered, "len=") > 0) then
                    if (.not. character_len_references_params(arena, node, stmt%type_name)) then
                        override = trim(stmt%type_name)
                        return
                    end if
                end if
            end select
        end do
    end subroutine derive_character_return_type

    logical function character_len_references_params(arena, node, type_spec) result(refs_params)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=*), intent(in) :: type_spec
        integer :: len_pos, paren_pos, i
        character(len=:), allocatable :: len_expr
        character(len=:), allocatable :: param_name

        refs_params = .false.
        len_pos = index(type_spec, 'len=')
        if (len_pos == 0) return

        paren_pos = index(type_spec(len_pos:), ')')
        if (paren_pos == 0) return

        len_expr = type_spec(len_pos + 4:len_pos + paren_pos - 2)
        if (.not. allocated(node%param_indices)) return

        do i = 1, size(node%param_indices)
            if (node%param_indices(i) <= 0 .or. node%param_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(node%param_indices(i))%node)) cycle

            select type (param_node => arena%entries(node%param_indices(i))%node)
            type is (identifier_node)
                param_name = trim(param_node%name)
            type is (parameter_declaration_node)
                param_name = trim(param_node%name)
            type is (declaration_node)
                param_name = trim(param_node%var_name)
            class default
                cycle
            end select

            if (index(len_expr, trim(param_name)) > 0) then
                refs_params = .true.
                return
            end if
        end do
    end function character_len_references_params

    pure logical function is_deferred_character_return(text) result(is_deferred)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lowered

        lowered = to_lower_ascii_str(trim(text))
        is_deferred = (index(lowered, 'character') == 1) .and. &
                      (index(lowered, 'len=:') > 0)
        if (is_deferred) then
            if (index(lowered, 'allocatable') == 0) then
                is_deferred = .false.
            end if
        end if
    end function is_deferred_character_return

    logical function has_character_len_result_decl(arena, node) result(has_decl)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: target_name
        integer :: i, decl_index, name_idx
        character(len=:), allocatable :: lowered

        has_decl = .false.

        if (allocated(node%result_variable)) then
            target_name = trim(node%result_variable)
        else if (allocated(node%name)) then
            target_name = trim(node%name)
        else
            target_name = ''
        end if

        if (len_trim(target_name) == 0) return
        if (.not. allocated(node%body_indices)) return

        do i = 1, size(node%body_indices)
            decl_index = node%body_indices(i)
            if (decl_index <= 0 .or. decl_index > arena%size) cycle
            if (.not. allocated(arena%entries(decl_index)%node)) cycle
            select type (stmt => arena%entries(decl_index)%node)
            type is (declaration_node)
                if (is_character_len_declaration(stmt%type_name)) then
                    if (trim(stmt%var_name) == target_name) then
                        has_decl = .true.
                        return
                    end if
                    if (stmt%is_multi_declaration .and. &
                        allocated(stmt%var_names)) then
                        do name_idx = 1, size(stmt%var_names)
                            if (trim(stmt%var_names(name_idx)) == target_name) then
                                has_decl = .true.
                                return
                            end if
                        end do
                    end if
                end if
            end select
        end do
    end function has_character_len_result_decl

    pure logical function is_character_len_declaration(type_name) result(matches)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: lowered

        lowered = to_lower_ascii_str(trim(type_name))
        if (len_trim(lowered) == 0) then
            matches = .false.
            return
        end if

        matches = (index(lowered, 'character') == 1) .and. &
                  (index(lowered, 'len=') > 0) .and. &
                  (index(lowered, 'len=*') == 0) .and. &
                  (index(lowered, 'len=:') == 0)
    end function is_character_len_declaration

    pure function to_lower_ascii_str(text) result(lower_text)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lower_text
        integer :: i, code

        if (len(text) == 0) then
            allocate (character(len=0) :: lower_text)
            return
        end if

        allocate (character(len=len(text)) :: lower_text)
        do i = 1, len(text)
            code = iachar(text(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) then
                lower_text(i:i) = achar(code + 32)
            else
                lower_text(i:i) = text(i:i)
            end if
        end do
    end function to_lower_ascii_str

    pure function fix_character_len_placeholder(text) result(out)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: pos

        out = text

        pos = index(out, "len=))")
        do while (pos > 0)
            out = out(:pos - 1) // "len=*" // out(pos + 5:)
            pos = index(out, "len=))")
        end do

        pos = index(out, "len=)")
        do while (pos > 0)
            out = out(:pos - 1) // "len=*" // out(pos + 4:)
            pos = index(out, "len=)")
        end do
    end function fix_character_len_placeholder

    logical function program_is_trivial_wrapper(arena, prog_index, name) &
        result(is_trivial)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=*), intent(in) :: name
        integer :: j, child_idx

        is_trivial = .false.
        if (prog_index <= 0 .or. prog_index > arena%size) return
        if (.not. allocated(arena%entries(prog_index)%node)) return

        select type (prog => arena%entries(prog_index)%node)
        type is (program_node)
            if (.not. (trim(name) == 'main' .or. trim(name) == &
                       '__IMPLICIT_MAIN__')) return
            if (.not. allocated(prog%body_indices) .or. &
                size(prog%body_indices) == 0) then
                is_trivial = .true.
                return
            end if

            is_trivial = .true.
            do j = 1, size(prog%body_indices)
                child_idx = prog%body_indices(j)
                if (child_idx <= 0 .or. child_idx > arena%size) cycle
                if (.not. allocated(arena%entries(child_idx)%node)) cycle
                select type (body => arena%entries(child_idx)%node)
                type is (comment_node)
                    cycle
                type is (blank_line_node)
                    cycle
                type is (implicit_statement_node)
                    if (body%is_none) cycle
                    is_trivial = .false.
                    return
                class default
                    is_trivial = .false.
                    return
                end select
            end do
        class default
            return
        end select
    end function program_is_trivial_wrapper

    function collect_trivial_program_trivia(arena, prog_index) result(trivia_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=:), allocatable :: trivia_code
        integer :: j, child_idx
        character(len=:), allocatable :: snippet

        trivia_code = ""
        if (prog_index <= 0 .or. prog_index > arena%size) return
        if (.not. allocated(arena%entries(prog_index)%node)) return

        select type (prog => arena%entries(prog_index)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do j = 1, size(prog%body_indices)
                child_idx = prog%body_indices(j)
                if (child_idx <= 0 .or. child_idx > arena%size) cycle
                if (.not. allocated(arena%entries(child_idx)%node)) cycle
                select type (body => arena%entries(child_idx)%node)
                type is (comment_node)
                    snippet = generate_code_from_arena(arena, child_idx)
                type is (blank_line_node)
                    snippet = generate_code_from_arena(arena, child_idx)
                class default
                    cycle
                end select

                if (len(snippet) > 0) then
                    if (len(trivia_code) > 0) trivia_code = trivia_code // new_line('A')
                    trivia_code = trivia_code // snippet
                end if
            end do
        end select
    end function collect_trivial_program_trivia

    ! Generate grouped body with context
    function generate_grouped_body_with_context(arena, body_indices, indent, &
                                                has_exec_before_contains) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: indent
        logical, intent(in) :: has_exec_before_contains
        character(len=:), allocatable :: code

        ! Pass context to utilities module
        code = generate_grouped_body_context(arena, body_indices, indent, &
                                             has_exec_before_contains)
    end function generate_grouped_body_with_context

    ! Helper subroutine to extract loop variables from an implied do section
    subroutine extract_loop_vars_from_section(section, loop_vars, n_vars)
        character(len=*), intent(in) :: section
        character(len=*), intent(inout) :: loop_vars(:)
        integer, intent(inout) :: n_vars
        integer :: pos, eq_pos, comma_pos
        character(len=32) :: var_name
        logical :: already_added
        integer :: i

        ! Look for patterns like "i=1," or "j=1," or "k=1,"
        pos = 1
        do while (pos < len_trim(section))
            eq_pos = index(section(pos:), "=")
            if (eq_pos == 0) exit
            eq_pos = pos + eq_pos - 1

            ! Look backwards from = to find variable name
            if (eq_pos > 1) then
                ! Find the start of the variable name
                i = eq_pos - 1
                do while (i > 0)
                    if (section(i:i) == ' ' .or. section(i:i) == ',' .or. &
                        section(i:i) == '(') then
                        exit
                    end if
                    i = i - 1
                end do

                ! Extract variable name
                var_name = adjustl(trim(section(i + 1:eq_pos - 1)))

                ! Check if it looks like a loop variable (single letter or simple name)
                if (len_trim(var_name) > 0 .and. len_trim(var_name) <= 8) then
                    ! Check if it's a number after =
                    comma_pos = index(section(eq_pos + 1:), ",")
                    if (comma_pos > 0) then
                        ! This looks like a loop variable
                        ! Check if already in list
                        already_added = .false.
                        do i = 1, n_vars
                            if (trim(loop_vars(i)) == trim(var_name)) then
                                already_added = .true.
                                exit
                            end if
                        end do

                        if (.not. already_added .and. n_vars < size(loop_vars)) then
                            n_vars = n_vars + 1
                            loop_vars(n_vars) = trim(var_name)
                        end if
                    end if
                end if
            end if

            pos = eq_pos + 1
        end do
    end subroutine extract_loop_vars_from_section

    ! Collect variable declarations for undeclared identifiers in programs
    function collect_program_variable_decls(arena, prog) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        character(len=:), allocatable :: decl_code
        integer, parameter :: MAX_VARS = 256
        character(len=64) :: declared_names(MAX_VARS)
        character(len=64) :: var_names(MAX_VARS)
        character(len=64) :: var_types(MAX_VARS)
        character(len=64) :: func_names(MAX_VARS)
        character(len=64) :: func_types(MAX_VARS)
        character(len=64) :: internal_funcs(MAX_VARS)
        character(len=64) :: func_return_type
        character(len=64) :: defined_func_names(MAX_VARS)
        character(len=64) :: defined_func_types(MAX_VARS)
        logical :: name_declared
        integer :: declared_count, var_count, func_count, internal_count
        integer :: i, j, idx, target_idx
        integer :: defined_func_count
        character(len=64) :: name_buf
        character(len=:), allocatable :: type_buf

        decl_code = ""
        declared_count = 0
        var_count = 0
        func_count = 0
        internal_count = 0
        defined_func_count = 0

        declared_names = ""
        var_names = ""
        var_types = ""
        func_names = ""
        func_types = ""
        internal_funcs = ""
        func_return_type = ""
        defined_func_names = ""
        defined_func_types = ""

        if (.not. allocated(prog%body_indices)) return

        call build_function_return_type_table(arena, defined_func_names, &
                                              defined_func_types, defined_func_count)

        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (decl => arena%entries(idx)%node)
            type is (declaration_node)
                if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                    do j = 1, size(decl%var_names)
                        if (declared_count < MAX_VARS) then
                            declared_count = declared_count + 1
                            declared_names(declared_count) = trim(decl%var_names(j))
                        end if
                    end do
                else
                    if (declared_count < MAX_VARS) then
                        declared_count = declared_count + 1
                        declared_names(declared_count) = trim(decl%var_name)
                    end if
                end if
            type is (function_def_node)
                if (internal_count < MAX_VARS) then
                    internal_count = internal_count + 1
                    internal_funcs(internal_count) = trim(decl%name)
                end if
            end select
        end do

        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (stmt => arena%entries(idx)%node)
            type is (assignment_node)
                target_idx = stmt%target_index
                if (target_idx > 0 .and. target_idx <= arena%size) then
                    if (allocated(arena%entries(target_idx)%node)) then
                        select type (id => arena%entries(target_idx)%node)
                        type is (identifier_node)
                            name_buf = trim(id%name)
                            if (len_trim(name_buf) == 0) cycle
                            name_declared = exists_in_list(declared_names, &
                                                           declared_count, name_buf)
                            if (.not. name_declared) then
                                if (.not. exists_in_list(var_names, var_count, &
                                                         name_buf)) then
                                    type_buf = mono_type_to_string( &
                                        id%inferred_type)
                                    if (len_trim(type_buf) == 0 .or. &
                                        trim(type_buf) == 'real') then
                                        func_return_type = ''
                                        if (stmt%value_index > 0 .and. &
                                            stmt%value_index <= arena%size) then
                                            if (allocated(arena%entries( &
                                                          stmt%value_index)%node)) then
                                                select type (rhs => arena%entries( &
                                                             stmt%value_index)%node)
                                                type is (call_or_subscript_node)
                                                    if (len_trim(rhs%name) > 0) then
                                                        func_return_type = &
                                                            lookup_function_return_type( &
                                                            defined_func_names, &
                                                            defined_func_types, &
                                                            defined_func_count, &
                                                            rhs%name)
                                                  if (len_trim(func_return_type) > 0) then
                                                            type_buf = trim( &
                                                                func_return_type)
                                                        end if
                                                    end if
                                                end select
                                            end if
                                        end if
                                    end if
                                    if (len_trim(type_buf) == 0) type_buf = 'real'
                                    if (var_count < MAX_VARS) then
                                        var_count = var_count + 1
                                        var_names(var_count) = ""
                                        var_types(var_count) = ""
                                        var_names(var_count) = name_buf
                                        var_types(var_count) = trim(type_buf)
                                    end if
                                end if
                            end if
                        end select
                    end if
                end if

                if (stmt%value_index > 0 .and. stmt%value_index <= arena%size) then
                    if (allocated(arena%entries(stmt%value_index)%node)) then
                        select type (val => arena%entries(stmt%value_index)%node)
                        type is (call_or_subscript_node)
                            if (len_trim(val%name) > 0) then
                                type_buf = mono_type_to_string(val%inferred_type)
                                if (len_trim(type_buf) == 0 .or. &
                                    trim(type_buf) == 'real') then
                                    func_return_type = &
                                        lookup_function_return_type( &
                                        defined_func_names, defined_func_types, &
                                        defined_func_count, val%name)
                                    if (len_trim(func_return_type) > 0) then
                                        type_buf = trim(func_return_type)
                                    end if
                                end if
                                if (len_trim(type_buf) == 0) type_buf = 'real'
                                if (.not. exists_in_list(func_names, func_count, &
                                                         trim(val%name))) then
                                    if (func_count < MAX_VARS) then
                                        func_count = func_count + 1
                                        func_names(func_count) = ""
                                        func_types(func_count) = ""
                                        func_names(func_count) = trim(val%name)
                                        func_types(func_count) = trim(type_buf)
                                    end if
                                end if
                            end if
                        end select
                    end if
                end if
            end select
        end do

        if (var_count == 0 .and. func_count == 0) return

        do i = 1, var_count
            decl_code = decl_code // "    " // trim(var_types(i)) // " :: " // &
                        trim(var_names(i)) // new_line('A')
        end do

        do i = 1, func_count
            if (.not. exists_in_list(internal_funcs, internal_count, &
                                     trim(func_names(i)))) then
                decl_code = decl_code // "    " // trim(func_types(i)) // &
                            ", external :: " // trim(func_names(i)) // new_line('A')
            end if
        end do
    end function collect_program_variable_decls

    ! Helper function to check if a name exists in a list
    logical function exists_in_list(list, count, name)
        character(len=*), intent(in) :: list(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: name
        integer :: i

        exists_in_list = .false.
        do i = 1, count
            if (trim(list(i)) == trim(name)) then
                exists_in_list = .true.
                return
            end if
        end do
    end function exists_in_list

    subroutine build_function_return_type_table(arena, func_names, func_types, count)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(inout) :: func_names(:)
        character(len=*), intent(inout) :: func_types(:)
        integer, intent(out) :: count
        integer :: i
        character(len=64) :: func_name

        count = 0
        func_names = ""
        func_types = ""

        do i = 1, arena%size
            if (count >= size(func_names)) exit
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (func => arena%entries(i)%node)
            type is (function_def_node)
                if (.not. allocated(func%name)) cycle
                func_name = trim(func%name)
                if (len_trim(func_name) == 0) cycle
                if (exists_in_list(func_names, count, func_name)) cycle
                count = count + 1
                func_names(count) = func_name
                if (allocated(func%return_type)) then
                    if (len_trim(func%return_type) > 0) then
                        func_types(count) = trim(func%return_type)
                    end if
                end if
            end select
        end do
    end subroutine build_function_return_type_table

    function lookup_function_return_type(func_names, func_types, count, &
                                         func_name) result(type_name)
        character(len=*), intent(in) :: func_names(:)
        character(len=*), intent(in) :: func_types(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: func_name
        character(len=:), allocatable :: type_name
        integer :: i

        type_name = ""
        if (len_trim(func_name) == 0) return

        do i = 1, count
            if (trim(func_names(i)) == trim(func_name)) then
                if (len_trim(func_types(i)) > 0) then
                    type_name = trim(func_types(i))
                end if
                return
            end if
        end do
    end function lookup_function_return_type

    ! Convert mono_type_t to Fortran type string
    recursive function mono_type_to_string(mono) result(type_name)
        type(mono_type_t), intent(in) :: mono
        character(len=:), allocatable :: type_name

        if (mono%kind <= 0) then
            type_name = ""
            return
        end if

        select case (mono%kind)
        case (TINT)
            type_name = "integer"
        case (TREAL)
            type_name = "real"
        case (TARRAY)
            block
                type(mono_type_t) :: elem_mono
                character(len=:), allocatable :: elem_str

                if (mono%get_args_count() > 0) then
                    elem_mono = mono%get_arg(1)
                    elem_str = mono_type_to_string(elem_mono)
                else
                    elem_str = ""
                end if

                if (.not. allocated(elem_str) .or. len_trim(elem_str) == 0) then
                    elem_str = "real"
                end if

                if (mono%size > 0) then
                    type_name = trim(elem_str) // ", dimension(" // &
                        trim(int_to_string(mono%size)) // ")"
                else if (mono%alloc_info%is_allocatable .or. &
                         mono%alloc_info%needs_allocatable_string) then
                    type_name = trim(elem_str) // ", dimension(:), allocatable"
                else
                    type_name = trim(elem_str) // ", dimension(:)"
                end if
            end block
        case (TCHAR)
            if (mono%size > 0) then
                type_name = "character(len=" // &
                    trim(adjustl(int_to_string(mono%size))) // ")"
            else
                type_name = "character(len=:)"
            end if
        case (TLOGICAL)
            type_name = "logical"
        case (TCOMPLEX)
            type_name = "complex"
        case (TDOUBLE)
            type_name = "double precision"
        case default
            type_name = "real"
        end select
    end function mono_type_to_string

    ! Collect parameter declarations for undeclared function parameters
    function collect_function_parameter_decls(arena, func, param_map) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=:), allocatable :: decl_code
        integer :: i, param_idx, j, k
        character(len=:), allocatable :: param_type
        character(len=:), allocatable :: decl_line
        logical :: has_declaration

        decl_code = ""

        if (.not. allocated(func%param_indices)) return

        ! Check each parameter
        do i = 1, size(func%param_indices)
            param_idx = func%param_indices(i)
            if (param_idx <= 0 .or. param_idx > arena%size) cycle
            if (.not. allocated(arena%entries(param_idx)%node)) cycle

            ! Check if parameter already has a declaration in body
            has_declaration = .false.
            if (allocated(func%body_indices)) then
                block
                    integer :: j, body_idx
                    do j = 1, size(func%body_indices)
                        body_idx = func%body_indices(j)
                        if (body_idx <= 0 .or. body_idx > arena%size) cycle
                        if (.not. allocated(arena%entries(body_idx)%node)) cycle
                        select type (body_node => arena%entries(body_idx)%node)
                        type is (declaration_node)
                            if (i <= size(param_map)) then
                                if (len_trim(param_map(i)%name) > 0) then
                                    if (trim(body_node%var_name) == &
                                        trim(param_map(i)%name)) then
                                        has_declaration = .true.
                                        exit
                                    end if
                                end if
                                if (body_node%is_multi_declaration .and. &
                                    allocated(body_node%var_names)) then
                                    do k = 1, size(body_node%var_names)
                                        if (trim(body_node%var_names(k)) == &
                                            trim(param_map(i)%name)) then
                                            has_declaration = .true.
                                            exit
                                        end if
                                    end do
                                    if (has_declaration) exit
                                end if
                            end if
                        type is (parameter_declaration_node)
                            if (i <= size(param_map)) then
                                if (trim(body_node%name) == trim(param_map(i)%name)) then
                                    has_declaration = .true.
                                    exit
                                end if
                            end if
                        end select
                    end do
                end block
            end if

            ! If no declaration, generate one from inferred type or parameter map
            if (.not. has_declaration .and. i <= size(param_map)) then
                if (len_trim(param_map(i)%name) > 0) then
                    select type (param_node => arena%entries(param_idx)%node)
                    type is (identifier_node)
                        param_type = mono_type_to_string(param_node%inferred_type)
                        if (len_trim(param_type) == 0) param_type = 'real'
                        decl_line = "    " // trim(param_type) // " :: " // &
                                    trim(param_map(i)%name)
                        decl_line = fix_character_len_placeholder(decl_line)
                        decl_code = decl_code // decl_line // new_line('A')
                    type is (parameter_declaration_node)
                        ! Try type_name first, then inferred_type
                        if (allocated(param_node%type_name) .and. &
                            len_trim(param_node%type_name) > 0) then
                            param_type = param_node%type_name
                        else
                            param_type = mono_type_to_string(param_node%inferred_type)
                            if (len_trim(param_type) == 0) param_type = 'real'
                        end if
                        decl_line = "    " // trim(param_type) // " :: " // &
                                    trim(param_map(i)%name)
                        decl_line = fix_character_len_placeholder(decl_line)
                        decl_code = decl_code // decl_line // new_line('A')
                    class default
                        ! For any other node type, try using the base inferred_type field
                        param_type = mono_type_to_string(param_node%inferred_type)
                        if (len_trim(param_type) == 0) param_type = 'real'
                        decl_line = "    " // trim(param_type) // " :: " // &
                                    trim(param_map(i)%name)
                        decl_line = fix_character_len_placeholder(decl_line)
                        decl_code = decl_code // decl_line // new_line('A')
                    end select
                end if
            end if
        end do
    end function collect_function_parameter_decls

end module codegen_declarations
