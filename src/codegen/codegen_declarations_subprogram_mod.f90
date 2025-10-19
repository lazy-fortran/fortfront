module codegen_declarations_subprogram_mod
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
                              intent_type_to_string
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_core, only: identifier_node
    use ast_nodes_misc, only: implicit_statement_node
    use string_utils_mod, only: int_to_string, to_lower
    use type_string_utils, only: mono_type_to_string
    use codegen_utilities, only: parameter_info_t, generate_grouped_body_with_params
    use codegen_declarations_shared_mod, only: fix_character_len_placeholder
    implicit none
    private
    public :: generate_code_function_def
    public :: generate_code_subroutine_def

contains

    ! Generate code for function definitions
    function generate_code_function_def(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = compose_function_signature(arena, node)
        code = code // new_line('A')
        code = code // build_function_body_section(arena, node)
        code = code // "end function " // node%name
    end function generate_code_function_def

    function compose_function_signature(arena, node) result(signature)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: signature
        character(len=:), allocatable :: prefix
        character(len=:), allocatable :: return_type_code
        character(len=:), allocatable :: params_clause
        character(len=:), allocatable :: result_clause
        logical :: recursive_in_prefix

        prefix = gather_function_prefix(node, recursive_in_prefix)
        if (node%is_recursive .and. .not. recursive_in_prefix) then
            if (len_trim(prefix) > 0) then
                prefix = "recursive " // trim(prefix)
            else
                prefix = "recursive"
            end if
        end if

        return_type_code = derive_function_return_type(arena, node)

        if (allocated(node%param_indices)) then
            params_clause = build_parameter_clause(arena, node%param_indices)
        else
            params_clause = "()"
        end if

        result_clause = build_function_result_clause(node)

        if (len_trim(prefix) > 0) then
            signature = trim(prefix) // " "
        else
            signature = ""
        end if

        if (len_trim(return_type_code) > 0) then
            signature = signature // trim(return_type_code) // " function " // node%name
        else
            signature = signature // "function " // node%name
        end if

        signature = signature // params_clause // result_clause
    end function compose_function_signature

    function gather_function_prefix(node, recursive_in_prefix) result(prefix)
        type(function_def_node), intent(in) :: node
        logical, intent(out) :: recursive_in_prefix
        character(len=:), allocatable :: prefix
        integer :: i
        character(len=:), allocatable :: term

        prefix = ""
        recursive_in_prefix = .false.

        if (.not. allocated(node%prefix_keywords)) return

        do i = 1, size(node%prefix_keywords)
            term = node%prefix_keywords(i)
            if (len_trim(term) == 0) cycle
            if (len(prefix) > 0) prefix = prefix // " "
            prefix = prefix // trim(term)
            if (trim(term) == "recursive") recursive_in_prefix = .true.
        end do
    end function gather_function_prefix

    function derive_function_return_type(arena, node) result(return_type_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: return_type_code
        character(len=:), allocatable :: override

        return_type_code = ""

        if (allocated(node%return_type)) then
            return_type_code = trim(node%return_type)
        end if

        call derive_character_return_type(arena, node, override)
        if (len_trim(override) > 0) return_type_code = override

        if (len_trim(return_type_code) == 0) return

        if (should_omit_return_type(arena, node, return_type_code)) then
            return_type_code = ""
            return
        end if

        return_type_code = fix_character_len_placeholder(return_type_code)

        if (.not. is_deferred_character_return(return_type_code)) return
        if (has_character_len_result_decl(arena, node)) return_type_code = ""
    end function derive_function_return_type

    function build_parameter_clause(arena, param_indices) result(clause)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_indices(:)
        character(len=:), allocatable :: clause
        integer :: i

        if (size(param_indices) == 0) then
            clause = "()"
            return
        end if

        clause = "("
        do i = 1, size(param_indices)
            if (i > 1) clause = clause // ", "
            clause = clause // derive_parameter_name(arena, param_indices(i), i)
        end do
        clause = clause // ")"
    end function build_parameter_clause

    function derive_parameter_name(arena, param_index, fallback_index) result(name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        integer, intent(in) :: fallback_index
        character(len=:), allocatable :: name

        name = "param" // trim(adjustl(int_to_string(fallback_index)))

        if (param_index <= 0 .or. param_index > arena%size) return
        if (.not. allocated(arena%entries(param_index)%node)) return

        select type (param_node => arena%entries(param_index)%node)
        type is (identifier_node)
            name = param_node%name
        type is (parameter_declaration_node)
            name = param_node%name
        type is (declaration_node)
            name = param_node%var_name
        end select
    end function derive_parameter_name

    function build_function_result_clause(node) result(result_clause)
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: result_clause
        character(len=:), allocatable :: result_name

        result_clause = ""
        if (.not. allocated(node%result_variable)) return
        if (len_trim(node%result_variable) == 0) return

        result_name = node%result_variable
        if (allocated(node%name)) then
            if (trim(result_name) == trim(node%name)) return
        end if

        result_clause = " result(" // result_name // ")"
    end function build_function_result_clause

    function build_function_body_section(arena, node) result(body)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: body
        type(parameter_info_t), allocatable :: param_map(:)
        integer, allocatable :: param_indices(:)
        integer, allocatable :: body_indices(:)

        if (allocated(node%param_indices)) then
            param_indices = node%param_indices
        else
            allocate (param_indices(0))
        end if

        if (allocated(node%body_indices)) then
            body_indices = node%body_indices
        else
            allocate (body_indices(0))
        end if

        call build_parameter_map(arena, param_indices, body_indices, param_map)
        if (allocated(node%prefix_keywords)) then
            call apply_default_intents(node%prefix_keywords, param_map)
        end if

        body = maybe_add_function_implicit_none(arena, body_indices)
        body = body // collect_function_parameter_decls(arena, node, param_map)
        body = body // generate_grouped_body_with_params(arena, body_indices, 1, &
                                                         param_map, node)
    end function build_function_body_section

    function maybe_add_function_implicit_none(arena, body_indices) result(prolog)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=:), allocatable :: prolog
        integer :: j

        prolog = ""
        do j = 1, size(body_indices)
            if (body_indices(j) <= 0 .or. body_indices(j) > arena%size) cycle
            if (.not. allocated(arena%entries(body_indices(j))%node)) cycle
            select type (body_node => arena%entries(body_indices(j))%node)
            type is (implicit_statement_node)
                if (body_node%is_none) return
            end select
        end do
        prolog = "    implicit none" // new_line('A')
    end function maybe_add_function_implicit_none

    subroutine apply_default_intents(prefix_keywords, param_map)
        character(len=*), intent(in) :: prefix_keywords(:)
        type(parameter_info_t), intent(inout) :: param_map(:)
        integer :: i, j

        do j = 1, size(prefix_keywords)
            select case (trim(prefix_keywords(j)))
            case ("pure", "elemental")
                do i = 1, size(param_map)
                    if (.not. allocated(param_map(i)%name)) cycle
                    if (len_trim(param_map(i)%intent_str) == 0) then
                        param_map(i)%intent_str = "in"
                    end if
                end do
            end select
        end do
    end subroutine apply_default_intents

    ! Generate code for subroutine definitions
    function generate_code_subroutine_def(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = compose_subroutine_signature(arena, node)
        code = code // new_line('A')
        code = code // build_subroutine_body_section(arena, node)
        code = code // "end subroutine " // node%name
    end function generate_code_subroutine_def

    function compose_subroutine_signature(arena, node) result(signature)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: node
        character(len=:), allocatable :: signature
        character(len=:), allocatable :: prefix
        character(len=:), allocatable :: params_clause
        logical :: recursive_in_prefix

        prefix = gather_subroutine_prefix(node, recursive_in_prefix)
        if (node%is_recursive .and. .not. recursive_in_prefix) then
            if (len_trim(prefix) > 0) then
                prefix = "recursive " // trim(prefix)
            else
                prefix = "recursive"
            end if
        end if

        if (allocated(node%param_indices)) then
            params_clause = build_parameter_clause(arena, node%param_indices)
        else
            params_clause = "()"
        end if

        if (len_trim(prefix) > 0) then
            signature = trim(prefix) // " subroutine " // node%name // params_clause
        else
            signature = "subroutine " // node%name // params_clause
        end if
    end function compose_subroutine_signature

    function gather_subroutine_prefix(node, recursive_in_prefix) result(prefix)
        type(subroutine_def_node), intent(in) :: node
        logical, intent(out) :: recursive_in_prefix
        character(len=:), allocatable :: prefix
        integer :: i
        character(len=:), allocatable :: term

        prefix = ""
        recursive_in_prefix = .false.

        if (.not. allocated(node%prefix_keywords)) return

        do i = 1, size(node%prefix_keywords)
            term = node%prefix_keywords(i)
            if (len_trim(term) == 0) cycle
            if (len(prefix) > 0) prefix = prefix // " "
            prefix = prefix // trim(term)
            if (trim(term) == "recursive") recursive_in_prefix = .true.
        end do
    end function gather_subroutine_prefix

    function build_subroutine_body_section(arena, node) result(body)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: node
        character(len=:), allocatable :: body
        type(parameter_info_t), allocatable :: param_map(:)
        integer, allocatable :: param_indices(:)
        integer, allocatable :: body_indices(:)

        if (allocated(node%param_indices)) then
            param_indices = node%param_indices
        else
            allocate (param_indices(0))
        end if

        if (allocated(node%body_indices)) then
            body_indices = node%body_indices
        else
            allocate (body_indices(0))
        end if

        call build_parameter_map(arena, param_indices, body_indices, param_map)
        if (allocated(node%prefix_keywords)) then
            call apply_default_intents(node%prefix_keywords, param_map)
        end if

        body = generate_grouped_body_with_params(arena, body_indices, 1, param_map, node)
    end function build_subroutine_body_section

    logical function should_omit_return_type(arena, node, return_type_code) &
        result(omit)
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

        lowered_return = to_lower(trim(return_type_code))
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

    subroutine build_parameter_map(arena, param_indices, body_indices, param_map)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_indices(:)
        integer, intent(in) :: body_indices(:)
        type(parameter_info_t), allocatable, intent(out) :: param_map(:)
        integer :: param_count, i, j

        param_count = size(param_indices)
        allocate (param_map(param_count))

        do i = 1, param_count
            param_map(i)%name = ""
            param_map(i)%intent_str = ""
            param_map(i)%is_optional = .false.

            if (param_indices(i) <= 0 .or. param_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(param_indices(i))%node)) cycle

            select type (param_node => arena%entries(param_indices(i))%node)
            type is (identifier_node)
                param_map(i)%name = param_node%name
            type is (parameter_declaration_node)
                param_map(i)%name = param_node%name
                param_map(i)%intent_str = intent_type_to_string(param_node%intent_type)
                param_map(i)%is_optional = param_node%is_optional
            end select
        end do

        do j = 1, size(body_indices)
            if (body_indices(j) <= 0 .or. body_indices(j) > arena%size) cycle
            if (.not. allocated(arena%entries(body_indices(j))%node)) cycle

            select type (body_node => arena%entries(body_indices(j))%node)
            type is (parameter_declaration_node)
                do i = 1, param_count
                    if (.not. allocated(param_map(i)%name)) cycle
                    if (param_map(i)%name == body_node%name) then
                        param_map(i)%intent_str = &
                            intent_type_to_string(body_node%intent_type)
                        param_map(i)%is_optional = body_node%is_optional
                    end if
                end do
            type is (declaration_node)
                do i = 1, param_count
                    if (.not. allocated(param_map(i)%name)) cycle
                    if (param_map(i)%name == body_node%var_name) then
                        if (body_node%has_intent) then
                            param_map(i)%intent_str = body_node%intent
                        end if
                        param_map(i)%is_optional = body_node%is_optional
                    end if
                end do
            end select
        end do
    end subroutine build_parameter_map

    subroutine derive_character_return_type(arena, node, override)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable, intent(out) :: override
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: target_name
        integer :: i, decl_index

        override = ""

        if (allocated(node%return_type)) then
            lowered = to_lower(trim(node%return_type))
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
                lowered = to_lower(trim(stmt%type_name))
                if (index(lowered, "len=") > 0) then
                    if (.not. character_len_references_params(arena, node, &
                                                              stmt%type_name)) then
                        override = trim(stmt%type_name)
                        return
                    end if
                end if
            end select
        end do
    end subroutine derive_character_return_type

    logical function character_len_references_params(arena, node, type_spec) &
        result(refs_params)
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
            if (node%param_indices(i) <= 0 .or. node%param_indices(i) > &
                arena%size) cycle
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

        lowered = to_lower(trim(text))
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

        lowered = to_lower(trim(type_name))
        if (len_trim(lowered) == 0) then
            matches = .false.
            return
        end if

        matches = (index(lowered, 'character') == 1) .and. &
                  (index(lowered, 'len=') > 0) .and. &
                  (index(lowered, 'len=*') == 0) .and. &
                  (index(lowered, 'len=:') == 0)
    end function is_character_len_declaration

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
                                if (trim(body_node%name) == &
                                    trim(param_map(i)%name)) then
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
                        param_type = mono_type_to_string( &
                                     param_node%inferred_type, include_shape=.true., &
                                     fallback='real')
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
                            param_type = mono_type_to_string( &
                                         param_node%inferred_type, &
                                         include_shape=.true., &
                                         fallback='real')
                            if (len_trim(param_type) == 0) param_type = 'real'
                        end if
                        decl_line = "    " // trim(param_type) // " :: " // &
                                    trim(param_map(i)%name)
                        decl_line = fix_character_len_placeholder(decl_line)
                        decl_code = decl_code // decl_line // new_line('A')
                    class default
                        ! For any other node type, try using the base inferred_type field
                        param_type = mono_type_to_string( &
                                     param_node%inferred_type, include_shape=.true., &
                                     fallback='real')
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
end module codegen_declarations_subprogram_mod
