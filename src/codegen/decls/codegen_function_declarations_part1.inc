
    function generate_code_function_def(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        logical :: in_interface_context

        in_interface_context = function_in_interface(arena, node_index) .or. &
                               function_has_only_specs(arena, node)

        code = compose_function_signature(arena, node)
        code = code // new_line('A')
        if (in_interface_context) then
            code = code // build_interface_prototype_body(arena, node)
        else
            code = code // build_function_body_section(arena, node, node_index)
        end if
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
        logical :: is_deferred_char

        prefix = gather_function_prefix(node, recursive_in_prefix)
        if (node%is_recursive .and. .not. recursive_in_prefix) then
            if (len_trim(prefix) > 0) then
                prefix = "recursive " // trim(prefix)
            else
                prefix = "recursive"
            end if
        end if

        call derive_function_return_type_and_flags(arena, node, return_type_code, &
                                                   is_deferred_char)

        if (allocated(node%param_indices)) then
            params_clause = build_parameter_clause(arena, node%param_indices)
        else
            params_clause = "()"
        end if

        result_clause = build_function_result_clause(arena, node)

        if (len_trim(prefix) > 0) then
            signature = trim(prefix) // " "
        else
            signature = ""
        end if

        if (len_trim(return_type_code) > 0) then
            signature = signature // trim(return_type_code) // " function " // &
                        node%name
        else
            signature = signature // "function " // node%name
        end if

        signature = signature // params_clause // result_clause
        if (allocated(node%bind_c_clause)) then
            if (len_trim(node%bind_c_clause) > 0) then
                signature = signature // " " // trim(node%bind_c_clause)
            end if
        end if
    end function compose_function_signature

    function gather_function_prefix(node, recursive_in_prefix) result(prefix)
        type(function_def_node), intent(in) :: node
        logical, intent(out) :: recursive_in_prefix
        character(len=:), allocatable :: prefix

        prefix = gather_prefix(node%prefix_keywords, recursive_in_prefix)
    end function gather_function_prefix

    subroutine derive_function_return_type_and_flags(arena, node, &
                                                     return_type_code, &
                                                     is_deferred_char)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable, intent(out) :: return_type_code
        logical, intent(out) :: is_deferred_char
        character(len=:), allocatable :: override
        character(len=:), allocatable :: lowered
        logical :: standardize_types_enabled
        type(mono_type_t) :: inferred_return_type

        return_type_code = ""
        is_deferred_char = .false.

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

        if (len_trim(return_type_code) == 0) then
            inferred_return_type%kind = 0
            inferred_return_type = node%inferred_type
            if (inferred_return_type%kind == TFUN) then
                if (type_args_allocated(inferred_return_type)) then
                    if (type_args_size(inferred_return_type) >= 2) then
                        inferred_return_type = type_args_element(inferred_return_type, &
                                                                 2)
                    end if
                end if
            end if

            if (inferred_return_type%kind /= 0 .and. &
                inferred_return_type%kind /= TARRAY .and. &
                inferred_return_type%kind /= TFUN) then
                call get_type_standardization(standardize_types_enabled)
                return_type_code = mono_type_to_string( &
                                   inferred_return_type, include_shape=.false., &
                                   standardize_real=standardize_types_enabled, &
                                   fallback="")
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

        if (is_allocatable_array_return(return_type_code)) then
            is_deferred_char = .true.
            return_type_code = ""
            return
        end if

        if (.not. is_deferred_character_return(return_type_code)) return

        is_deferred_char = .true.
        return_type_code = ""
    end subroutine derive_function_return_type_and_flags

    function build_function_result_clause(arena, node) result(result_clause)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: result_clause
        character(len=:), allocatable :: result_name
        character(len=:), allocatable :: function_name
        logical :: rename_result
        logical :: needs_result_for_recursion
        logical :: needs_result_for_array
        logical :: missing_result_variable
        logical :: has_return_type

        result_clause = ""

        if (allocated(node%name)) then
            function_name = trim(node%name)
        else
            function_name = ""
        end if

        if (allocated(node%result_variable)) then
            missing_result_variable = len_trim(node%result_variable) == 0
            if (.not. missing_result_variable) then
                if (len_trim(function_name) > 0) then
                    missing_result_variable = &
                        (trim(node%result_variable) == function_name)
                end if
            end if
        else
            missing_result_variable = .true.
        end if

        has_return_type = allocated(node%return_type)
        if (has_return_type) has_return_type = len_trim(node%return_type) > 0

        rename_result = should_rename_deferred_char_result(node)
        needs_result_for_recursion = node%is_recursive .and. &
                                     missing_result_variable .and. &
                                     has_return_type
        needs_result_for_array = should_use_result_for_array(arena, node) .and. &
                                 missing_result_variable

        if (rename_result .or. needs_result_for_recursion .or. &
            needs_result_for_array) then
            result_name = trim(node%name) // "_result"
        else
            if (.not. allocated(node%result_variable)) return
            result_name = trim(node%result_variable)
        end if

        if (len_trim(result_name) == 0) return

        if (allocated(node%name)) then
            if (.not. rename_result .and. .not. needs_result_for_recursion .and. &
                .not. needs_result_for_array) then
                if (result_name == trim(node%name)) return
            end if
        end if

        result_clause = " result(" // result_name // ")"
    end function build_function_result_clause

    function build_function_body_section(arena, node, node_index) result(body)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: body
        type(parameter_info_t), allocatable :: param_map(:)
        integer, allocatable :: param_indices(:)
        integer, allocatable :: body_indices(:)
        integer, allocatable :: filtered_body_indices(:)
        character(len=:), allocatable :: use_section
        integer, allocatable :: trimmed_indices(:)
        character(len=:), allocatable :: array_result_decl
        character(len=:), allocatable :: array_result_name
        logical :: array_result_declared

        call copy_indices(node%param_indices, param_indices)
        call copy_indices(node%body_indices, body_indices)

        call extract_use_statements_function(arena, body_indices, trimmed_indices, &
                                             use_section)
        call move_alloc(trimmed_indices, body_indices)

        call build_parameter_map(arena, param_indices, body_indices, param_map, node)
        if (allocated(node%prefix_keywords)) then
            call apply_default_intents(node%prefix_keywords, param_map)
        end if
        call apply_function_default_intents(param_map, arena, body_indices)

        body = ""
        if (len_trim(use_section) > 0) body = body // use_section
        body = body // maybe_add_procedure_implicit_none(arena, body_indices, &
                                                         node_index)
        body = body // collect_function_parameter_decls(arena, node, param_map)
        body = body // collect_deferred_char_result_decl(arena, node)
        body = body // collect_recursive_result_decl(arena, node)
        array_result_decl = collect_array_result_decl(arena, node, &
                                                      array_result_name)
        array_result_declared = allocated(array_result_decl)
        if (array_result_declared) &
            array_result_declared = len_trim(array_result_decl) > 0
        if (array_result_declared) body = body // array_result_decl
        body = body // collect_entry_parameter_decls(arena, node)
        body = body // collect_local_variable_decls(arena, node, param_map, &
                                                    array_result_declared)

        call filter_implicit_statements(arena, body_indices, filtered_body_indices)
        body = body // generate_grouped_body_with_params(arena, &
                                                         filtered_body_indices, 1, &
                                                         param_map, node)
        if (allocated(node%result_variable)) then
            body = dedupe_result_declaration(body, trim(node%result_variable))
        end if
        if (allocated(node%name)) then
            body = dedupe_result_declaration(body, trim(node%name))
            body = dedupe_result_declaration(body, trim(node%name)//"_result")
        end if
        call reorder_import_lines(body)
        call rename_result_variable_in_body(node, body, arena)
    end function build_function_body_section

    function build_interface_prototype_body(arena, node) result(body)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: body
        integer, allocatable :: body_indices(:)
        integer, allocatable :: remaining_indices(:)
        character(len=:), allocatable :: use_section

        if (.not. allocated(node%body_indices)) then
            body = ""
            return
        end if

        call copy_indices(node%body_indices, body_indices)

        call extract_use_statements_function(arena, body_indices, remaining_indices, &
                                             use_section)
        call move_alloc(remaining_indices, body_indices)

        body = ""
        if (len_trim(use_section) > 0) body = body // use_section

        if (allocated(body_indices)) then
            if (size(body_indices) > 0) then
                body = body // generate_grouped_body(arena, body_indices, 1)
            end if
        end if

        if (.not. allocated(body)) body = ""
    end function build_interface_prototype_body

    subroutine extract_use_statements_function(arena, original_indices, &
                                               remaining_indices, use_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: original_indices(:)
        integer, allocatable, intent(out) :: remaining_indices(:)
        character(len=:), allocatable, intent(out) :: use_code
        integer :: i, count
        logical :: is_use_stmt
        character(len=:), allocatable :: stmt_text

        if (size(original_indices) == 0) then
            allocate (remaining_indices(0))
            use_code = ""
            return
        end if

        allocate (remaining_indices(size(original_indices)))
        count = 0
        use_code = ""

        do i = 1, size(original_indices)
            is_use_stmt = .false.
            if (original_indices(i) > 0 .and. original_indices(i) <= arena%size) then
                if (allocated(arena%entries(original_indices(i))%node)) then
                    select type (node => arena%entries(original_indices(i))%node)
                    type is (use_statement_node)
                        is_use_stmt = .true.
                        stmt_text = generate_code_from_arena(arena, &
                                                             original_indices(i))
                        if (len_trim(stmt_text) > 0) then
                            use_code = use_code // "    " // trim(stmt_text) // &
                                       new_line('A')
                        end if
                    end select
                end if
            end if

            if (.not. is_use_stmt) then
                count = count + 1
                remaining_indices(count) = original_indices(i)
            end if
        end do

        if (count == 0) then
            deallocate (remaining_indices)
            allocate (remaining_indices(0))
        else if (count < size(original_indices)) then
            block
                integer, allocatable :: trimmed(:)
                allocate (trimmed(count))
                trimmed = remaining_indices(1:count)
                call move_alloc(trimmed, remaining_indices)
            end block
        end if
    end subroutine extract_use_statements_function

    logical function should_omit_return_type(arena, node, return_type_code) &
        result(omit)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=*), intent(in) :: return_type_code
        character(len=:), allocatable :: result_name
        character(len=:), allocatable :: lowered_return
        integer :: i
        integer :: decl_index
        integer :: k
        logical :: has_header_return_type

        omit = .false.

        ! Array functions with result clauses have type in result var declaration
        if (should_use_result_for_array(arena, node)) then
            omit = .true.
            return
        end if

        result_name = ""
        if (allocated(node%result_variable)) then
            result_name = trim(node%result_variable)
        end if
        if (len_trim(result_name) == 0 .and. allocated(node%name)) then
            result_name = trim(node%name)
        end if
        if (len_trim(result_name) == 0) return

        if (.not. allocated(node%name)) return

        has_header_return_type = node%has_return_type_in_header

        if (.not. allocated(node%body_indices)) return

        lowered_return = to_lower(trim(return_type_code))
        if (len_trim(lowered_return) == 0) return

        do i = 1, size(node%body_indices)
            decl_index = node%body_indices(i)
            if (.not. arena%has_node_at(decl_index)) cycle
            select type (decl => arena%entries(decl_index)%node)
            type is (declaration_node)
                if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                    do k = 1, size(decl%var_names)
                        if (trim(decl%var_names(k)) == trim(result_name)) then
                            if (has_header_return_type) cycle
                            omit = .true.
                            return
                        end if
                    end do
                else
                    if (trim(decl%var_name) /= trim(result_name)) cycle
                    if (has_header_return_type) cycle
                    omit = .true.
                    return
                end if
            end select
        end do
    end function should_omit_return_type

    function collect_function_parameter_decls(arena, func, param_map) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=:), allocatable :: decl_code
        integer :: i
        integer :: param_idx
        logical :: has_declaration

        decl_code = ""

        if (.not. allocated(func%param_indices)) return

        do i = 1, size(func%param_indices)
            param_idx = func%param_indices(i)
            if (.not. arena%has_node_at(param_idx)) cycle

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
        integer :: body_idx
        integer :: k

        has_decl = .false.
        if (.not. allocated(func%body_indices)) return
        if (param_idx > size(param_map)) return

        do body_idx = 1, size(func%body_indices)
            if (func%body_indices(body_idx) <= 0 .or. &
                func%body_indices(body_idx) > arena%size) cycle
            if (.not. allocated(arena%entries(func%body_indices(body_idx))%node)) cycle

            select type (body_node => arena%entries(func%body_indices(body_idx))%node)
            type is (declaration_node)
                if (len_trim(param_map(param_idx)%name) == 0) cycle
                if (body_node%is_multi_declaration .and. &
                    allocated(body_node%var_names)) then
                    do k = 1, size(body_node%var_names)
                        if (trim(body_node%var_names(k)) == &
                            trim(param_map(param_idx)%name)) then
                            has_decl = .true.
                            return
                        end if
                    end do
                else
                    if (trim(body_node%var_name) == trim(param_map(param_idx)%name)) &
                        then
                        has_decl = .true.
                        return
                    end if
                end if
            type is (parameter_declaration_node)
                if (trim(body_node%name) == trim(param_map(param_idx)%name)) then
                    has_decl = .true.
                    return
                end if
            end select
        end do
    end function parameter_has_declaration

    logical function is_function_result_identifier(func, var_name) result(is_result)
        type(function_def_node), intent(in) :: func
        character(len=*), intent(in) :: var_name
        character(len=:), allocatable :: function_name
        character(len=:), allocatable :: result_name
        character(len=:), allocatable :: renamed_result

        is_result = .false.
        renamed_result = ''

        if (allocated(func%name)) then
            function_name = trim(func%name)
        else
            function_name = ''
        end if
        if (len_trim(function_name) > 0) then
            renamed_result = trim(function_name) // "_result"
        end if

        if (allocated(func%result_variable)) then
            result_name = trim(func%result_variable)
        else
            result_name = ''
        end if

        if (len_trim(function_name) > 0) then
            if (trim(var_name) == function_name) then
                is_result = .true.
                return
            end if
        end if

        if (len_trim(result_name) > 0) then
            if (trim(var_name) == result_name) then
                is_result = .true.
                return
            end if
            if (result_name == function_name) then
                if (trim(var_name) == renamed_result) then
                    is_result = .true.
                    return
                end if
            end if
        end if

        if (len_trim(renamed_result) > 0) then
            if (trim(var_name) == renamed_result) then
                is_result = .true.
            end if
        end if
    end function is_function_result_identifier

    function get_result_type_string(arena, func, var_name, inferred_type) &
        result(type_str)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        character(len=*), intent(in) :: var_name
        type(mono_type_t), intent(in) :: inferred_type
        character(len=:), allocatable :: type_str
        type(mono_type_t) :: deferred_type
        type(mono_type_t) :: type_choice
        logical :: is_result_var
        logical :: has_header_type
        character(len=:), allocatable :: header_type
        logical :: is_array_result
        logical :: is_deferred_char
        character(len=:), allocatable :: signature
        integer :: func_pos

        is_result_var = is_function_result_identifier(func, var_name)
        is_array_result = is_result_var .and. should_use_result_for_array(arena, &
                                                                          func)

        has_header_type = .false.
        if (is_result_var) then
            if (allocated(func%return_type)) then
                header_type = trim(func%return_type)
                if (len_trim(header_type) > 0) has_header_type = .true.
            end if
        end if

        if (is_array_result) has_header_type = .false.

        if (has_header_type) then
            type_str = trim(header_type)
            return
        end if

        if (is_result_var) then
            call derive_function_return_type_and_flags(arena, func, &
                                                       header_type, &
                                                       is_deferred_char)
            if (len_trim(header_type) > 0) then
                type_str = trim(header_type)
                return
            end if

            signature = compose_function_signature(arena, func)
            func_pos = index(to_lower(signature), 'function')
            if (func_pos > 1) then
                header_type = trim(signature(:func_pos - 1))
                if (len_trim(header_type) > 0) then
                    type_str = trim(header_type)
                    return
                end if
            end if
        end if

        type_choice = inferred_type
        if (is_result_var) then
            if (should_use_result_for_array(arena, func)) then
                if (type_choice%kind /= TARRAY) then
                    type_choice = wrap_scalar_as_deferred_array(type_choice)
                end if
            end if
            if (type_choice%kind == 0 .and. func%inferred_type%kind /= 0) then
                type_choice = func%inferred_type
            end if
        end if

        if (is_result_var .and. type_choice%kind == TARRAY .and. &
            (type_choice%size == 0 .or. type_choice%alloc_info%is_allocatable)) &
            then
            deferred_type = convert_array_to_deferred_shape(type_choice)
            type_str = mono_type_to_string(deferred_type, include_shape=.true., &
                                           fallback='integer')
        else
            type_str = mono_type_to_string(type_choice, include_shape=.true., &
                                           fallback='integer')
        end if
    end function get_result_type_string

    logical function function_in_interface(arena, node_index) result(in_iface)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: parent_index

        in_iface = .false.
        if (.not. arena%has_node_at(node_index)) return

        parent_index = arena%entries(node_index)%parent_index
        do while (parent_index > 0 .and. parent_index <= arena%size)
            if (.not. allocated(arena%entries(parent_index)%node)) then
                parent_index = arena%entries(parent_index)%parent_index
                cycle
            end if
            select type (parent_node => arena%entries(parent_index)%node)
            type is (interface_block_node)
                in_iface = .true.
                return
            class default
                parent_index = arena%entries(parent_index)%parent_index
            end select
        end do
    end function function_in_interface

    logical function function_has_only_specs(arena, node) result(only_specs)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        integer :: i, idx

        only_specs = .true.
        if (.not. allocated(node%body_indices)) return

        do i = 1, size(node%body_indices)
            idx = node%body_indices(i)
            if (.not. arena%has_node_at(idx)) cycle
            select type (body_node => arena%entries(idx)%node)
            type is (declaration_node)
                cycle
            type is (parameter_declaration_node)
                cycle
            type is (implicit_statement_node)
                cycle
            type is (comment_node)
                cycle
            type is (directive_node)
                cycle
            type is (blank_line_node)
                cycle
            type is (end_statement_node)
                cycle
            class default
                only_specs = .false.
                return
            end select
        end do
    end function function_has_only_specs

    recursive function convert_array_to_deferred_shape(typ) result(deferred)
        use type_system_unified, only: create_mono_type
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t) :: deferred
        type(mono_type_t) :: inner
        type(mono_type_t), allocatable :: args(:)

        deferred = typ
        if (typ%kind /= TARRAY) return

        if (typ%get_args_count() > 0) then
            inner = typ%get_arg(1)
            inner = convert_array_to_deferred_shape(inner)
            allocate (args(1))
            args(1) = inner
            deferred = create_mono_type(TARRAY, args=args)
            deferred%size = 0
            deferred%alloc_info%is_allocatable = .true.
        else
            deferred%size = 0
            deferred%alloc_info%is_allocatable = .true.
        end if
    end function convert_array_to_deferred_shape

    function wrap_scalar_as_deferred_array(base_type) result(array_type)
        use type_system_unified, only: create_mono_type
        type(mono_type_t), intent(in) :: base_type
        type(mono_type_t) :: array_type
        type(mono_type_t), allocatable :: args(:)

        allocate (args(1))
        args(1) = base_type
        array_type = create_mono_type(TARRAY, args=args)
        array_type%size = 0
        array_type%alloc_info%is_allocatable = .true.
    end function wrap_scalar_as_deferred_array

    logical function assignment_value_is_array(arena, stmt) result(is_array)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: stmt
        integer :: value_idx
        character(len=:), allocatable :: value_code
        character(len=:), allocatable :: lowered

        is_array = stmt%inferred_type%kind == TARRAY
        if (is_array) return
        value_idx = stmt%value_index
        if (.not. arena%has_node_at(value_idx)) return

        select type (value => arena%entries(value_idx)%node)
        type is (array_literal_node)
            is_array = .true.
        class default
            if (value%inferred_type%kind == TARRAY) is_array = .true.
        end select

        if (.not. is_array) then
            value_code = generate_code_from_arena(arena, value_idx)
            if (allocated(value_code)) then
                lowered = to_lower(adjustl(value_code))
                if (index(lowered, '[') > 0 .or. index(lowered, '(/') > 0) then
                    is_array = .true.
                end if
            end if
        end if
    end function assignment_value_is_array

    logical function needs_result_declaration(arena, func, result_type) &
        result(needed)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        type(mono_type_t), intent(in) :: result_type
        character(len=:), allocatable :: function_name
        character(len=:), allocatable :: result_name
        logical :: header_has_type
        logical :: requires_deferred_attrs
        logical :: has_fixed_shape_result
        character(len=:), allocatable :: signature
        integer :: func_pos

        needed = .false.
        header_has_type = .false.
        requires_deferred_attrs = .false.
        has_fixed_shape_result = .false.

        if (allocated(func%name)) then
            function_name = trim(func%name)
        else
            function_name = ''
        end if

        if (allocated(func%result_variable)) then
            result_name = trim(func%result_variable)
        else
            result_name = ''
        end if

        if (len_trim(result_name) == 0) then
            result_name = function_name
        end if

        if (allocated(func%return_type)) then
            if (len_trim(func%return_type) > 0) header_has_type = .true.
            if (len_trim(func%return_type) > 0) then
                has_fixed_shape_result = .not. is_allocatable_array_return( &
                                         func%return_type) .and. &
                                         .not. is_deferred_shape_array( &
                                         func%return_type)
            end if
        end if
        if (.not. header_has_type) header_has_type = func%has_return_type_in_header
        if (.not. header_has_type) then
            signature = compose_function_signature(arena, func)
            func_pos = index(to_lower(signature), 'function')
            if (func_pos > 1) then
                header_has_type = len_trim(signature(:func_pos - 1)) > 0
            end if
        end if

        if (result_type%kind == TARRAY) then
            if (result_type%size == 0 .or. result_type%alloc_info%is_allocatable) then
                requires_deferred_attrs = .true.
            end if
        end if

        if (len_trim(result_name) == 0) return

        if (has_result_variable_declaration(arena, func, result_name)) return

        if (should_use_result_for_array(arena, func) .and. has_fixed_shape_result) then
            needed = .false.
            return
        end if

        if (trim(result_name) /= trim(function_name)) then
            if (.not. header_has_type) then
                needed = .true.
                return
            end if
            if (requires_deferred_attrs) then
                needed = .true.
                return
            end if
            return
        end if

        if (.not. header_has_type) then
            needed = .true.
            return
        end if

        if (requires_deferred_attrs) needed = .true.
    end function needs_result_declaration

    logical function has_result_variable_declaration(arena, func, result_name) &
        result(found)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        character(len=*), intent(in) :: result_name
        integer :: i
        integer :: stmt_idx
        integer :: name_idx

        found = .false.
        if (.not. allocated(func%body_indices)) return
        if (len_trim(result_name) == 0) return

        do i = 1, size(func%body_indices)
            stmt_idx = func%body_indices(i)
            if (.not. arena%has_node_at(stmt_idx)) cycle

            select type (stmt => arena%entries(stmt_idx)%node)
            type is (declaration_node)
                if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
                    do name_idx = 1, size(stmt%var_names)
                        if (trim(stmt%var_names(name_idx)) == trim(result_name)) then
                            found = .true.
                            return
                        end if
                    end do
                else if (allocated(stmt%var_name)) then
                    if (trim(stmt%var_name) == trim(result_name)) then
                        found = .true.
                        return
                    end if
                end if
            end select
        end do
    end function has_result_variable_declaration

    function collect_local_variable_decls(arena, func, param_map, result_declared) &
        result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        type(parameter_info_t), intent(in) :: param_map(:)
        logical, intent(in), optional :: result_declared
        character(len=:), allocatable :: decl_code
        character(len=64), allocatable :: local_vars(:)
        integer :: i
        integer :: stmt_idx
        integer :: n_locals
        integer :: capacity
        character(len=:), allocatable :: result_name
        logical :: skip_result_decl

        decl_code = ""
        n_locals = 0
        capacity = 0
        skip_result_decl = .false.
        if (present(result_declared)) skip_result_decl = result_declared

        result_name = ""
        if (allocated(func%result_variable)) then
            result_name = trim(func%result_variable)
        else if (allocated(func%name)) then
            result_name = trim(func%name)
        end if

        if (.not. allocated(func%body_indices)) return

        do i = 1, size(func%body_indices)
            stmt_idx = func%body_indices(i)
            if (.not. arena%has_node_at(stmt_idx)) cycle

            select type (stmt => arena%entries(stmt_idx)%node)
            type is (assignment_node)
                call collect_local_var_from_assignment(arena, func, stmt, &
                                                       param_map, result_name, &
                                                       skip_result_decl, &
                                                       local_vars, n_locals, &
                                                       capacity, decl_code)
            type is (print_statement_node)
                call collect_vars_from_print(arena, stmt, param_map, local_vars, &
                                             n_locals, capacity, result_name, &
                                             decl_code)
            type is (read_statement_node)
                call collect_vars_from_read(arena, stmt, param_map, local_vars, &
                                            n_locals, capacity, result_name, &
                                            decl_code)
            type is (do_loop_node)
                call collect_loop_var(arena, func, i, stmt, param_map, &
                                      local_vars, n_locals, capacity, &
                                      result_name, decl_code)
            end select
        end do
    end function collect_local_variable_decls

    subroutine collect_local_var_from_assignment(arena, func, stmt, param_map, &
                                                 result_name, skip_result_decl, &
                                                 local_vars, n_locals, capacity, &
                                                 decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        type(assignment_node), intent(in) :: stmt
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=*), intent(in) :: result_name
        logical, intent(in) :: skip_result_decl
        character(len=64), allocatable, intent(inout) :: local_vars(:)
        integer, intent(inout) :: n_locals
        integer, intent(inout) :: capacity
        character(len=:), allocatable, intent(inout) :: decl_code

        character(len=64) :: var_name
        type(mono_type_t) :: decl_type
        logical :: is_array_value
        logical :: ok

        call get_assignment_target_identifier(arena, stmt, var_name, decl_type, &
                                              is_array_value, ok)
        if (.not. ok) return

        call apply_result_identifier_rules(arena, func, stmt, result_name, &
                                           skip_result_decl, var_name, decl_type, &
                                           is_array_value, ok)
        if (.not. ok) return

        if (is_parameter_name(var_name, param_map)) return
        if (has_explicit_variable_declaration(arena, func, var_name)) return

        call add_local_var_decl(arena, func, var_name, decl_type, local_vars, &
                                n_locals, capacity, decl_code)
    end subroutine collect_local_var_from_assignment

    subroutine get_assignment_target_identifier(arena, stmt, var_name, decl_type, &
                                                is_array_value, ok)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: stmt
        character(len=64), intent(out) :: var_name
        type(mono_type_t), intent(out) :: decl_type
        logical, intent(out) :: is_array_value
        logical, intent(out) :: ok

        ok = .false.
        var_name = ""
        is_array_value = .false.
        decl_type%kind = 0

        if (.not. arena%has_node_at(stmt%target_index)) return

        select type (target => arena%entries(stmt%target_index)%node)
        type is (identifier_node)
            if (.not. allocated(target%name)) return
            if (target%inferred_type%kind == 0) return
            decl_type = target%inferred_type
            is_array_value = assignment_value_is_array(arena, stmt)
            var_name = trim(target%name)
            ok = .true.
        class default
            return
        end select
    end subroutine get_assignment_target_identifier

    subroutine apply_result_identifier_rules(arena, func, stmt, result_name, &
                                             skip_result_decl, var_name, decl_type, &
                                             is_array_value, ok)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        type(assignment_node), intent(in) :: stmt
        character(len=*), intent(in) :: result_name
        logical, intent(in) :: skip_result_decl
        character(len=64), intent(inout) :: var_name
        type(mono_type_t), intent(inout) :: decl_type
        logical, intent(inout) :: is_array_value
        logical, intent(out) :: ok

        character(len=:), allocatable :: lowered_value
        character(len=:), allocatable :: value_code

        ok = .true.

        if (.not. is_function_result_identifier(func, var_name)) return
        if (skip_result_decl) then
            ok = .false.
            return
        end if

        if (result_has_fixed_dimension_return(func)) then
            ok = .false.
            return
        end if

        if (array_result_decl_is_handled(arena, func)) then
            ok = .false.
            return
        end if

        if (.not. is_array_value) then
            value_code = generate_code_from_arena(arena, stmt%value_index)
            if (allocated(value_code)) then
                lowered_value = to_lower(adjustl(value_code))
                if (index(lowered_value, '[') > 0 .or. &
                    index(lowered_value, '(/') > 0) then
                    is_array_value = .true.
                end if
            end if
        end if

        if (should_use_result_for_array(arena, func) .or. &
            (is_array_value .and. decl_type%kind /= TARRAY)) then
            if (decl_type%kind /= TARRAY) then
                decl_type = wrap_scalar_as_deferred_array(decl_type)
            end if
        end if

        if (.not. needs_result_declaration(arena, func, decl_type)) then
            ok = .false.
            return
        end if
        var_name = trim(result_name)
    end subroutine apply_result_identifier_rules

    logical function result_has_fixed_dimension_return(func) result(has_fixed)
        type(function_def_node), intent(in) :: func
        character(len=:), allocatable :: lowered_return_type

        has_fixed = .false.
        if (.not. allocated(func%return_type)) return

        lowered_return_type = to_lower(adjustl(func%return_type))
        if (len_trim(lowered_return_type) == 0) return
        if (index(lowered_return_type, 'dimension') == 0) return

        if (.not. is_allocatable_array_return(func%return_type) .and. &
            .not. is_deferred_shape_array(func%return_type)) then
            has_fixed = .true.
        end if
    end function result_has_fixed_dimension_return

    logical function array_result_decl_is_handled(arena, func) result(handled)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func

        handled = .false.
        if (.not. should_use_result_for_array(arena, func)) return
        if (.not. allocated(func%name)) return
        if (.not. allocated(func%return_type)) return
        if (len_trim(func%return_type) == 0) return
        if (is_allocatable_array_return(func%return_type)) return
        if (is_deferred_shape_array(func%return_type)) return

        handled = .true.
    end function array_result_decl_is_handled

    subroutine add_local_var_decl(arena, func, var_name, decl_type, local_vars, &
                                  n_locals, capacity, decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        character(len=*), intent(in) :: var_name
        type(mono_type_t), intent(in) :: decl_type
        character(len=64), allocatable, intent(inout) :: local_vars(:)
        integer, intent(inout) :: n_locals
        integer, intent(inout) :: capacity
        character(len=:), allocatable, intent(inout) :: decl_code

        if (is_local_var_collected(var_name, local_vars, n_locals)) return

        call ensure_local_var_capacity(local_vars, capacity, n_locals + 1)
        n_locals = n_locals + 1
        local_vars(n_locals) = var_name
        decl_code = decl_code // "    " // &
                    get_result_type_string(arena, func, var_name, decl_type) // &
                    " :: " // trim(var_name) // new_line('A')
    end subroutine add_local_var_decl

    function dedupe_result_declaration(body, result_name) result(clean_body)
        character(len=:), allocatable, intent(in) :: body
        character(len=*), intent(in) :: result_name
        character(len=:), allocatable :: clean_body
        character(len=:), allocatable :: pattern
        integer :: first_pos
        integer :: search_pos
        integer :: line_start
        integer :: line_end

        clean_body = body
        if (.not. allocated(clean_body)) return
        if (len_trim(result_name) == 0) return

        pattern = ":: " // trim(result_name)
        first_pos = index(clean_body, pattern)
        if (first_pos == 0) return

        search_pos = index(clean_body(first_pos + len(pattern):), pattern)
        do while (search_pos > 0)
            search_pos = search_pos + first_pos + len(pattern) - 1
            line_start = search_pos
            do while (line_start > 1 .and. clean_body(line_start - 1: &
                                                      line_start - 1) /= &
                      new_line('A'))
                line_start = line_start - 1
            end do
            line_end = index(clean_body(search_pos:), new_line('A'))
            if (line_end == 0) then
                clean_body = clean_body(:line_start - 1)
            else
                line_end = line_end + search_pos - 1
                clean_body = clean_body(:line_start - 1) // &
                             clean_body(line_end + 1:)
            end if
            search_pos = index(clean_body(first_pos + len(pattern):), pattern)
        end do
    end function dedupe_result_declaration

    subroutine collect_vars_from_print(arena, stmt, param_map, local_vars, &
                                       n_locals, capacity, result_name, decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(print_statement_node), intent(in) :: stmt
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=64), allocatable, intent(inout) :: local_vars(:)
        integer, intent(inout) :: n_locals
        integer, intent(inout) :: capacity
        character(len=*), intent(in) :: result_name
        character(len=:), allocatable, intent(inout) :: decl_code
        integer :: j
        integer :: expr_idx
        character(len=64) :: var_name

        if (.not. allocated(stmt%expression_indices)) return

        do j = 1, size(stmt%expression_indices)
            expr_idx = stmt%expression_indices(j)
            if (.not. arena%has_node_at(expr_idx)) cycle

            select type (expr => arena%entries(expr_idx)%node)
            type is (identifier_node)
                if (.not. allocated(expr%name)) cycle

                var_name = trim(expr%name)

                if (len_trim(result_name) > 0 .and. var_name == result_name) cycle
                if (is_parameter_name(var_name, param_map)) cycle

                if (.not. is_local_var_collected(var_name, local_vars, n_locals)) then
                    call ensure_local_var_capacity(local_vars, capacity, n_locals + 1)
                    n_locals = n_locals + 1
                    local_vars(n_locals) = var_name
                    decl_code = decl_code // "    real :: " // trim(var_name) // &
                                new_line('A')
                end if
            end select
        end do
    end subroutine collect_vars_from_print

    subroutine collect_vars_from_read(arena, stmt, param_map, local_vars, &
                                      n_locals, capacity, result_name, decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(read_statement_node), intent(in) :: stmt
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=64), allocatable, intent(inout) :: local_vars(:)
        integer, intent(inout) :: n_locals
        integer, intent(inout) :: capacity
        character(len=*), intent(in) :: result_name
        character(len=:), allocatable, intent(inout) :: decl_code
        integer :: j
        integer :: var_idx
        character(len=64) :: var_name

        if (.not. allocated(stmt%var_indices)) return

        do j = 1, size(stmt%var_indices)
            var_idx = stmt%var_indices(j)
            if (.not. arena%has_node_at(var_idx)) cycle

            select type (var => arena%entries(var_idx)%node)
            type is (identifier_node)
                if (.not. allocated(var%name)) cycle

                var_name = trim(var%name)

                if (len_trim(result_name) > 0 .and. var_name == result_name) cycle
                if (is_parameter_name(var_name, param_map)) cycle

                if (.not. is_local_var_collected(var_name, local_vars, n_locals)) then
                    call ensure_local_var_capacity(local_vars, capacity, n_locals + 1)
                    n_locals = n_locals + 1
                    local_vars(n_locals) = var_name
                    decl_code = decl_code // "    real :: " // trim(var_name) // &
                                new_line('A')
                end if
            end select
        end do
    end subroutine collect_vars_from_read

    recursive subroutine collect_loop_var(arena, func, stmt_position, loop_node, &
                                          param_map, local_vars, n_locals, capacity, &
                                          result_name, decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        integer, intent(in) :: stmt_position
        type(do_loop_node), intent(in) :: loop_node
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=64), allocatable, intent(inout) :: local_vars(:)
        integer, intent(inout) :: n_locals
        integer, intent(inout) :: capacity
        character(len=*), intent(in) :: result_name
        character(len=:), allocatable, intent(inout) :: decl_code
        character(len=64) :: var_name
        integer :: body_idx, nested_idx

        if (.not. allocated(loop_node%var_name)) return

        var_name = trim(loop_node%var_name)
        if (len_trim(var_name) == 0) return

        if (len_trim(result_name) > 0 .and. var_name == result_name) return
        if (is_parameter_name(var_name, param_map)) return
        if (index(decl_code, "integer :: "//trim(var_name)) > 0) return
        if (has_explicit_loop_declaration(arena, func, stmt_position, var_name)) return

        if (.not. is_local_var_collected(var_name, local_vars, n_locals)) then
            call ensure_local_var_capacity(local_vars, capacity, n_locals + 1)
            n_locals = n_locals + 1
            local_vars(n_locals) = var_name
            decl_code = decl_code // "    integer :: " // trim(var_name) // &
                        new_line('A')
        end if

        if (allocated(loop_node%body_indices)) then
            do body_idx = 1, size(loop_node%body_indices)
                nested_idx = loop_node%body_indices(body_idx)
                if (.not. arena%has_node_at(nested_idx)) cycle

                select type (nested_stmt => arena%entries(nested_idx)%node)
                type is (do_loop_node)
                    call collect_loop_var(arena, func, stmt_position, nested_stmt, &
                                          param_map, local_vars, n_locals, capacity, &
                                          result_name, decl_code)
                end select
            end do
        end if
    end subroutine collect_loop_var

    logical function has_explicit_loop_declaration(arena, func, stmt_position, &
                                                   var_name) &
        result(has_decl)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        integer, intent(in) :: stmt_position
        character(len=*), intent(in) :: var_name
        integer :: idx
        integer :: stmt_idx
        integer :: name_idx

        has_decl = .false.
        if (.not. allocated(func%body_indices)) return
        if (stmt_position <= 1) return

        do idx = 1, stmt_position - 1
            stmt_idx = func%body_indices(idx)
            if (.not. arena%has_node_at(stmt_idx)) cycle

            select type (decl => arena%entries(stmt_idx)%node)
            type is (declaration_node)
                if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                    do name_idx = 1, size(decl%var_names)
                        if (trim(decl%var_names(name_idx)) == trim(var_name)) then
                            has_decl = .true.
                            return
                        end if
                    end do
                else if (allocated(decl%var_name)) then
                    if (trim(decl%var_name) == trim(var_name)) then
                        has_decl = .true.
                        return
                    end if
                end if
            end select
        end do
    end function has_explicit_loop_declaration

    logical function has_explicit_variable_declaration(arena, func, var_name) &
        result(has_decl)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        character(len=*), intent(in) :: var_name
        integer :: idx
        integer :: stmt_idx
        integer :: name_idx

        has_decl = .false.
        if (.not. allocated(func%body_indices)) return

        do idx = 1, size(func%body_indices)
            stmt_idx = func%body_indices(idx)
            if (.not. arena%has_node_at(stmt_idx)) cycle

            select type (decl => arena%entries(stmt_idx)%node)
            type is (declaration_node)
                if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                    do name_idx = 1, size(decl%var_names)
                        if (trim(decl%var_names(name_idx)) == trim(var_name)) then
                            has_decl = .true.
                            return
                        end if
                    end do
                else if (allocated(decl%var_name)) then
                    if (trim(decl%var_name) == trim(var_name)) then
                        has_decl = .true.
                        return
                    end if
                end if
            end select
        end do
    end function has_explicit_variable_declaration

    function collect_deferred_char_result_decl(arena, node) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: decl_code
        character(len=:), allocatable :: full_type
        character(len=:), allocatable :: base_type
        character(len=:), allocatable :: result_name

        decl_code = ""

        call get_deferred_char_type_info(arena, node, full_type, base_type, &
                                         result_name)

        if (len_trim(base_type) == 0) return

        decl_code = "    " // trim(base_type) // ", allocatable :: " // &
                    trim(result_name) // new_line('A')
    end function collect_deferred_char_result_decl
