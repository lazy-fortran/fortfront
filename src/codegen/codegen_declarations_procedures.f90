module codegen_declarations_procedures
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
                              intent_type_to_string
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_core, only: identifier_node
    use ast_nodes_misc, only: implicit_statement_node
    use string_utils_mod, only: int_to_string, to_lower
    use type_string_utils, only: mono_type_to_string
    use codegen_utilities, only: parameter_info_t, generate_grouped_body_with_params, &
                                 reorder_import_lines
    use codegen_declarations_core, only: fix_character_len_placeholder
    use codegen_declarations_inference, only: build_parameter_map, &
                                              derive_character_return_type, &
                                              character_len_references_params, &
                                              has_character_len_result_decl, &
                                              is_character_len_declaration, &
                                              is_deferred_character_return
    use codegen_type_utils, only: get_type_standardization
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
            signature = signature // trim(return_type_code) // " function " &
                        // node%name
        else
            signature = signature // "function " // node%name
        end if

        signature = signature // params_clause // result_clause
    end function compose_function_signature

    function gather_function_prefix(node, recursive_in_prefix) result(prefix)
        type(function_def_node), intent(in) :: node
        logical, intent(out) :: recursive_in_prefix
        character(len=:), allocatable :: prefix

        prefix = gather_prefix(node%prefix_keywords, recursive_in_prefix)
    end function gather_function_prefix

    function derive_function_return_type(arena, node) result(return_type_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: return_type_code
        character(len=:), allocatable :: override
        character(len=:), allocatable :: lowered
        logical :: standardize_types_enabled

        return_type_code = ""

        if (allocated(node%return_type)) then
            return_type_code = trim(node%return_type)
            call get_type_standardization(standardize_types_enabled)
            if (standardize_types_enabled) then
                lowered = to_lower(trim(return_type_code))
                if (lowered == 'real') then
                    return_type_code = "real(8)"
                end if
            end if
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
        integer, allocatable :: filtered_body_indices(:)

        call copy_indices(node%param_indices, param_indices)
        call copy_indices(node%body_indices, body_indices)

        call build_parameter_map(arena, param_indices, body_indices, param_map)
        if (allocated(node%prefix_keywords)) then
            call apply_default_intents(node%prefix_keywords, param_map)
        end if

        body = maybe_add_function_implicit_none(arena, body_indices)
        body = body // collect_function_parameter_decls(arena, node, param_map)

        call filter_implicit_statements(arena, body_indices, filtered_body_indices)
        body = body // generate_grouped_body_with_params(arena, &
            filtered_body_indices, 1, param_map, node)
        call reorder_import_lines(body)
    end function build_function_body_section

    subroutine filter_implicit_statements(arena, body_indices, filtered_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, allocatable, intent(out) :: filtered_indices(:)
        integer :: j, count
        logical :: is_implicit_none

        count = 0
        do j = 1, size(body_indices)
            is_implicit_none = .false.
            if (body_indices(j) > 0 .and. body_indices(j) <= arena%size) then
                if (allocated(arena%entries(body_indices(j))%node)) then
                    select type (body_node => arena%entries(body_indices(j))%node)
                    type is (implicit_statement_node)
                        is_implicit_none = body_node%is_none
                    end select
                end if
            end if
            if (.not. is_implicit_none) count = count + 1
        end do

        allocate (filtered_indices(count))
        count = 0
        do j = 1, size(body_indices)
            is_implicit_none = .false.
            if (body_indices(j) > 0 .and. body_indices(j) <= arena%size) then
                if (allocated(arena%entries(body_indices(j))%node)) then
                    select type (body_node => arena%entries(body_indices(j))%node)
                    type is (implicit_statement_node)
                        is_implicit_none = body_node%is_none
                    end select
                end if
            end if
            if (.not. is_implicit_none) then
                count = count + 1
                filtered_indices(count) = body_indices(j)
            end if
        end do
    end subroutine filter_implicit_statements

    function maybe_add_function_implicit_none(arena, body_indices) result(prolog)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=:), allocatable :: prolog
        integer :: j
        logical :: has_implicit_none
        logical :: has_other_implicit

        prolog = ""
        has_implicit_none = .false.
        has_other_implicit = .false.

        do j = 1, size(body_indices)
            if (body_indices(j) <= 0 .or. body_indices(j) > arena%size) cycle
            if (.not. allocated(arena%entries(body_indices(j))%node)) cycle
            select type (body_node => arena%entries(body_indices(j))%node)
            type is (implicit_statement_node)
                if (body_node%is_none) then
                    has_implicit_none = .true.
                else
                    has_other_implicit = .true.
                end if
            end select
        end do

        if (has_other_implicit) return
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

        prefix = gather_prefix(node%prefix_keywords, recursive_in_prefix)
    end function gather_subroutine_prefix

    function gather_prefix(prefix_keywords, recursive_in_prefix) result(prefix)
        character(len=*), allocatable, intent(in) :: prefix_keywords(:)
        logical, intent(out) :: recursive_in_prefix
        character(len=:), allocatable :: prefix
        integer :: i
        character(len=:), allocatable :: term

        prefix = ""
        recursive_in_prefix = .false.

        if (.not. allocated(prefix_keywords)) return

        do i = 1, size(prefix_keywords)
            term = prefix_keywords(i)
            if (len_trim(term) == 0) cycle
            if (len(prefix) > 0) prefix = prefix // " "
            prefix = prefix // trim(term)
            if (trim(term) == "recursive") recursive_in_prefix = .true.
        end do
    end function gather_prefix

    function build_subroutine_body_section(arena, node) result(body)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: node
        character(len=:), allocatable :: body
        type(parameter_info_t), allocatable :: param_map(:)
        integer, allocatable :: param_indices(:)
        integer, allocatable :: body_indices(:)
        integer, allocatable :: filtered_body_indices(:)

        call copy_indices(node%param_indices, param_indices)
        call copy_indices(node%body_indices, body_indices)

        call build_parameter_map(arena, param_indices, body_indices, param_map)
        if (allocated(node%prefix_keywords)) then
            call apply_default_intents(node%prefix_keywords, param_map)
        end if

        body = maybe_add_function_implicit_none(arena, body_indices)
        body = body // collect_subroutine_parameter_decls(arena, node, param_map)

        call filter_implicit_statements(arena, body_indices, filtered_body_indices)
        body = body // generate_grouped_body_with_params(arena, &
            filtered_body_indices, 1, param_map, node)
        call reorder_import_lines(body)
    end function build_subroutine_body_section

    subroutine copy_indices(source, target)
        integer, allocatable, intent(in) :: source(:)
        integer, allocatable, intent(out) :: target(:)

        if (allocated(source)) then
            allocate (target(size(source)))
            target = source
        else
            allocate (target(0))
        end if
    end subroutine copy_indices

    logical function should_omit_return_type(arena, node, return_type_code) &
        result(omit)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=*), intent(in) :: return_type_code
        character(len=:), allocatable :: result_name
        character(len=:), allocatable :: lowered_return
        integer :: i, decl_index
        logical :: has_explicit_result_clause

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

        has_explicit_result_clause = .false.
        if (allocated(node%result_variable)) then
            if (len_trim(node%result_variable) > 0) then
                if (trim(node%result_variable) /= trim(node%name)) then
                    has_explicit_result_clause = .true.
                end if
            end if
        end if

        if (has_explicit_result_clause) return

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

    ! Collect parameter declarations for undeclared function parameters
    function collect_function_parameter_decls(arena, func, param_map) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=:), allocatable :: decl_code
        integer :: i, param_idx
        logical :: has_declaration

        decl_code = ""

        if (.not. allocated(func%param_indices)) return

        do i = 1, size(func%param_indices)
            param_idx = func%param_indices(i)
            if (param_idx <= 0 .or. param_idx > arena%size) cycle
            if (.not. allocated(arena%entries(param_idx)%node)) cycle

            has_declaration = parameter_has_declaration(arena, func, param_map, i)

            if (.not. has_declaration .and. i <= size(param_map)) then
                call append_parameter_declaration(arena, param_idx, param_map(i), &
                                                  decl_code)
            end if
        end do
    end function collect_function_parameter_decls

    logical function parameter_has_declaration(arena, func, param_map, param_idx) &
        result(has_decl)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        type(parameter_info_t), intent(in) :: param_map(:)
        integer, intent(in) :: param_idx
        integer :: j, body_idx, k

        has_decl = .false.
        if (.not. allocated(func%body_indices)) return
        if (param_idx > size(param_map)) return

        do j = 1, size(func%body_indices)
            body_idx = func%body_indices(j)
            if (body_idx <= 0 .or. body_idx > arena%size) cycle
            if (.not. allocated(arena%entries(body_idx)%node)) cycle
            select type (body_node => arena%entries(body_idx)%node)
            type is (declaration_node)
                if (len_trim(param_map(param_idx)%name) == 0) cycle
                if (trim(body_node%var_name) == trim(param_map(param_idx)%name)) then
                    has_decl = .true.
                    return
                end if
                if (body_node%is_multi_declaration .and. &
                    allocated(body_node%var_names)) then
                    do k = 1, size(body_node%var_names)
                        if (trim(body_node%var_names(k)) == &
                            trim(param_map(param_idx)%name)) then
                            has_decl = .true.
                            return
                        end if
                    end do
                end if
            type is (parameter_declaration_node)
                if (trim(body_node%name) == trim(param_map(param_idx)%name)) then
                    has_decl = .true.
                    return
                end if
            end select
        end do
    end function parameter_has_declaration

    ! Collect parameter declarations for undeclared subroutine parameters
    function collect_subroutine_parameter_decls(arena, sub, param_map) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: sub
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=:), allocatable :: decl_code
        integer :: i, param_idx
        logical :: has_declaration

        decl_code = ""

        if (.not. allocated(sub%param_indices)) return

        do i = 1, size(sub%param_indices)
            param_idx = sub%param_indices(i)
            if (param_idx <= 0 .or. param_idx > arena%size) cycle
            if (.not. allocated(arena%entries(param_idx)%node)) cycle

            has_declaration = subroutine_parameter_has_declaration(arena, sub, &
                                                                    param_map, i)

            if (.not. has_declaration .and. i <= size(param_map)) then
                call append_parameter_declaration(arena, param_idx, param_map(i), &
                                                  decl_code)
            end if
        end do
    end function collect_subroutine_parameter_decls

    logical function subroutine_parameter_has_declaration(arena, sub, param_map, &
                                                          param_idx) result(has_decl)
        type(ast_arena_t), intent(in) :: arena
        type(subroutine_def_node), intent(in) :: sub
        type(parameter_info_t), intent(in) :: param_map(:)
        integer, intent(in) :: param_idx
        integer :: j, body_idx, k

        has_decl = .false.
        if (.not. allocated(sub%body_indices)) return
        if (param_idx > size(param_map)) return

        do j = 1, size(sub%body_indices)
            body_idx = sub%body_indices(j)
            if (body_idx <= 0 .or. body_idx > arena%size) cycle
            if (.not. allocated(arena%entries(body_idx)%node)) cycle
            select type (body_node => arena%entries(body_idx)%node)
            type is (declaration_node)
                if (len_trim(param_map(param_idx)%name) == 0) cycle
                if (trim(body_node%var_name) == trim(param_map(param_idx)%name)) then
                    has_decl = .true.
                    return
                end if
                if (body_node%is_multi_declaration .and. &
                    allocated(body_node%var_names)) then
                    do k = 1, size(body_node%var_names)
                        if (trim(body_node%var_names(k)) == &
                            trim(param_map(param_idx)%name)) then
                            has_decl = .true.
                            return
                        end if
                    end do
                end if
            type is (parameter_declaration_node)
                if (trim(body_node%name) == trim(param_map(param_idx)%name)) then
                    has_decl = .true.
                    return
                end if
            end select
        end do
    end function subroutine_parameter_has_declaration

    subroutine append_parameter_declaration(arena, param_idx, param_info, decl_code)
        use codegen_declarations_core, only: build_parameter_dimensions
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_idx
        type(parameter_info_t), intent(in) :: param_info
        character(len=:), allocatable, intent(inout) :: decl_code
        character(len=:), allocatable :: param_type, decl_line, intent_str, dim_clause

        if (len_trim(param_info%name) == 0) return

        select type (param_node => arena%entries(param_idx)%node)
        type is (identifier_node)
            param_type = get_param_type_from_identifier(param_node)
        type is (parameter_declaration_node)
            param_type = get_param_type_from_param_decl(param_node)
            intent_str = intent_type_to_string(param_node%intent_type)
            if (len_trim(intent_str) > 0) then
                param_type = trim(param_type) // ", intent(" // trim(intent_str) // ")"
            end if
            if (param_node%is_optional) then
                param_type = trim(param_type) // ", optional"
            end if
            if (param_node%is_target) then
                param_type = trim(param_type) // ", target"
            end if
        class default
            param_type = get_param_type_fallback(param_node)
        end select

        decl_line = "    " // trim(param_type) // " :: " // trim(param_info%name)

        select type (param_node => arena%entries(param_idx)%node)
        type is (parameter_declaration_node)
            if (param_node%is_array) then
                dim_clause = build_parameter_dimensions(arena, param_node)
                decl_line = trim(decl_line) // trim(dim_clause)
            end if
        end select

        decl_line = fix_character_len_placeholder(decl_line)
        ! For parameters, convert character(len=:) to character(len=*)
        block
            integer :: pos_char
            pos_char = index(decl_line, 'character(len=:)')
            if (pos_char > 0) then
                decl_line = decl_line(1:pos_char+13) // '*)' // decl_line(pos_char+17:)
            end if
        end block
        decl_code = decl_code // decl_line // new_line('A')
    end subroutine append_parameter_declaration

    function get_param_type_from_identifier(param_node) result(param_type)
        type(identifier_node), intent(in) :: param_node
        character(len=:), allocatable :: param_type
        integer :: pos

        param_type = mono_type_to_string(param_node%inferred_type, &
                                         include_shape=.true., fallback='real')
        if (len_trim(param_type) == 0) param_type = 'real'

        ! For parameters, convert character(len=:) to character(len=*)
        pos = index(param_type, 'character(len=:)')
        if (pos > 0) then
            param_type = param_type(1:pos+13) // '*' // param_type(pos+16:)
        end if
    end function get_param_type_from_identifier

    function get_param_type_from_param_decl(param_node) result(param_type)
        type(parameter_declaration_node), intent(in) :: param_node
        character(len=:), allocatable :: param_type

        if (allocated(param_node%type_name) .and. &
            len_trim(param_node%type_name) > 0) then
            param_type = param_node%type_name
        else
            param_type = mono_type_to_string(param_node%inferred_type, &
                                             include_shape=.true., fallback='real')
            if (len_trim(param_type) == 0) param_type = 'real'
        end if
    end function get_param_type_from_param_decl

    function get_param_type_fallback(param_node) result(param_type)
        class(*), intent(in) :: param_node
        character(len=:), allocatable :: param_type

        param_type = 'real'
    end function get_param_type_fallback
end module codegen_declarations_procedures
