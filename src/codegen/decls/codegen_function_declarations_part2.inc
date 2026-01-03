
    function collect_recursive_result_decl(arena, node) result(decl_code)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: decl_code
        character(len=:), allocatable :: type_str
        character(len=:), allocatable :: result_name
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: function_name
        logical :: standardize_types_enabled
        logical :: missing_result_variable
        logical :: will_emit_result_clause
        logical :: has_existing_decl
        integer :: i

        decl_code = ""

        ! ISO/IEC 1539-1:2018 Section 15.6.2.1 result variable declaration rules:
        ! 1. Typed prefix (e.g., integer function f() result(r)):
        !    - Type prefix implicitly declares result variable
        !    - Explicit declaration is invalid (gfortran error already has basic type)
        ! 2. Untyped function (e.g., function f() result(r)):
        !    - Result variable must be explicitly declared in body
        !    - result() clause does not create implicit declaration

        if (.not. allocated(node%name)) return
        function_name = trim(node%name)

        ! If function has type prefix, result is implicitly declared - do not add
        ! explicit decl. Allocatable arrays are not emitted as type prefix (handled
        ! in local declarations).
        if (allocated(node%return_type)) then
            if (len_trim(node%return_type) > 0) then
                if (.not. is_allocatable_array_return(node%return_type)) then
                    ! Function has type prefix - result variable implicitly declared
                    return
                end if
            end if
        end if

        ! Determine if result() clause will be generated (same logic as
        ! build_function_result_clause)
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

        ! Check if a result() clause will be generated
        will_emit_result_clause = .false.
        if (should_rename_deferred_char_result(node)) then
            will_emit_result_clause = .true.
        else if (node%is_recursive .and. missing_result_variable) then
            will_emit_result_clause = .true.
        else if (should_use_result_for_array(arena, node) .and. &
                 missing_result_variable) then
            will_emit_result_clause = .true.
        else if (allocated(node%result_variable) .and. &
                 .not. missing_result_variable) then
            will_emit_result_clause = .true.
        end if

        ! If no result() clause, no declaration needed (function name is the result)
        if (.not. will_emit_result_clause) return

        ! Determine result variable name
        if (should_rename_deferred_char_result(node)) then
            result_name = trim(function_name) // "_result"
        else if (missing_result_variable) then
            result_name = trim(function_name) // "_result"
        else
            result_name = trim(node%result_variable)
        end if

        ! Check if result variable already has a declaration in body
        has_existing_decl = .false.
        if (allocated(node%body_indices)) then
            do i = 1, size(node%body_indices)
                if (node%body_indices(i) <= 0 .or. node%body_indices(i) > &
                    arena%size) cycle
                if (.not. allocated(arena%entries(node%body_indices(i))%node)) cycle
                select type (body_node => arena%entries(node%body_indices(i))%node)
                type is (declaration_node)
                    if (allocated(body_node%var_name)) then
                        if (trim(body_node%var_name) == trim(result_name)) then
                            has_existing_decl = .true.
                            exit
                        end if
                    end if
                end select
            end do
        end if

        ! If declaration already exists, don't add another
        if (has_existing_decl) return

        ! Allocatable and deferred-shape arrays are declared in local variable
        ! declarations - skip here
        if (allocated(node%return_type)) then
            if (is_allocatable_array_return(node%return_type)) return
            ! Also skip deferred-shape arrays (dimension(:), dimension(:,:), etc.)
            ! even without allocatable keyword because collect_local_variable_decls
            ! will add the allocatable attribute
            if (is_deferred_shape_array(node%return_type)) return
        end if

        ! Build type declaration
        if (.not. allocated(node%return_type)) return
        type_str = trim(node%return_type)
        if (len_trim(type_str) == 0) return

        call get_type_standardization(standardize_types_enabled)
        if (standardize_types_enabled) then
            lowered = to_lower(trim(type_str))
            if (lowered == 'real') then
                type_str = "real(8)"
            end if
        end if

        decl_code = "    " // trim(type_str) // " :: " // &
                    trim(result_name) // new_line('A')
    end function collect_recursive_result_decl

    function collect_array_result_decl(arena, node, result_name_out) &
        result(decl_code)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable, intent(out), optional :: result_name_out
        character(len=:), allocatable :: decl_code
        character(len=:), allocatable :: type_str
        character(len=:), allocatable :: result_name
        character(len=:), allocatable :: function_name
        character(len=:), allocatable :: lowered
        logical :: has_existing_decl
        logical :: standardize_types_enabled
        integer :: i
        integer :: k

        decl_code = ""
        if (present(result_name_out)) result_name_out = ""

        if (.not. should_use_result_for_array(arena, node)) return
        if (.not. allocated(node%name)) return

        function_name = trim(node%name)

        if (should_rename_deferred_char_result(node)) return

        if (.not. allocated(node%return_type)) return
        if (len_trim(node%return_type) == 0) return

        if (node%inferred_type%alloc_info%is_allocatable) return

        type_str = trim(node%return_type)

        if (allocated(node%result_variable) .and. &
            len_trim(node%result_variable) > 0 .and. &
            trim(node%result_variable) /= function_name) then
            result_name = trim(node%result_variable)
        else
            result_name = trim(function_name) // "_result"
        end if

        has_existing_decl = .false.
        if (allocated(node%body_indices)) then
            do i = 1, size(node%body_indices)
                if (node%body_indices(i) <= 0 .or. node%body_indices(i) > &
                    arena%size) cycle
                if (.not. allocated(arena%entries(node%body_indices(i))%node)) cycle
                select type (body_node => arena%entries(node%body_indices(i))%node)
                type is (declaration_node)
                    if (body_node%is_multi_declaration .and. &
                        allocated(body_node%var_names)) then
                        do k = 1, size(body_node%var_names)
                            if (trim(body_node%var_names(k)) == trim(result_name)) then
                                has_existing_decl = .true.
                                exit
                            end if
                            if (trim(body_node%var_names(k)) == &
                                trim(function_name)) then
                                has_existing_decl = .true.
                                exit
                            end if
                        end do
                        if (has_existing_decl) exit
                    else if (allocated(body_node%var_name)) then
                        if (trim(body_node%var_name) == trim(result_name) .or. &
                            trim(body_node%var_name) == trim(function_name)) then
                            has_existing_decl = .true.
                            exit
                        end if
                    end if
                end select
                if (has_existing_decl) exit
            end do
        end if

        if (has_existing_decl) return

        if (is_allocatable_array_return(type_str)) return
        if (is_deferred_shape_array(type_str)) return

        call get_type_standardization(standardize_types_enabled)
        if (standardize_types_enabled) then
            lowered = to_lower(trim(type_str))
            if (lowered == 'real') type_str = "real(8)"
        end if

        decl_code = "    " // trim(type_str) // " :: " // &
                    trim(result_name) // new_line('A')

        if (present(result_name_out)) result_name_out = trim(result_name)
    end function collect_array_result_decl

    subroutine get_deferred_char_type_info(arena, node, full_type, base_type, &
                                           result_name)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable, intent(out) :: full_type
        character(len=:), allocatable, intent(out) :: base_type
        character(len=:), allocatable, intent(out) :: result_name
        character(len=:), allocatable :: override
        character(len=:), allocatable :: lowered
        logical :: standardize_types_enabled

        full_type = ""
        base_type = ""
        result_name = ""

        if (allocated(node%return_type)) then
            full_type = trim(node%return_type)
            call get_type_standardization(standardize_types_enabled)
            if (standardize_types_enabled) then
                lowered = to_lower(trim(full_type))
                if (lowered == 'real') then
                    full_type = "real(8)"
                end if
            end if
        end if

        call derive_character_return_type(arena, node, override)
        if (len_trim(override) > 0) full_type = override

        if (len_trim(full_type) == 0) return

        full_type = fix_character_len_placeholder(full_type)

        if (.not. is_deferred_character_return(full_type)) then
            full_type = ""
            return
        end if

        if (has_character_len_result_decl(arena, node)) then
            full_type = ""
            return
        end if

        base_type = strip_allocatable(full_type)

        if (allocated(node%result_variable)) then
            result_name = trim(node%result_variable)
            if (allocated(node%name)) then
                if (result_name == trim(node%name)) then
                    result_name = trim(node%name) // "_result"
                end if
            end if
        else if (allocated(node%name)) then
            result_name = trim(node%name)
        end if
    end subroutine get_deferred_char_type_info

    pure function strip_allocatable(type_string) result(stripped)
        character(len=*), intent(in) :: type_string
        character(len=:), allocatable :: stripped
        character(len=:), allocatable :: lowered
        integer :: alloc_pos, comma_pos

        stripped = trim(type_string)
        lowered = to_lower(stripped)

        alloc_pos = index(lowered, 'allocatable')
        if (alloc_pos == 0) return

        comma_pos = index(lowered(1:alloc_pos-1), ',', back=.true.)
        if (comma_pos > 0) then
            stripped = stripped(1:comma_pos-1) // stripped(alloc_pos+11:)
        else
            stripped = trim(adjustl(stripped(alloc_pos+11:)))
        end if
    end function strip_allocatable

    function collect_entry_parameter_decls(arena, func) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func
        character(len=:), allocatable :: decl_code
        integer :: i
        integer :: stmt_idx
        logical :: found_exec_stmt
        character(len=64), allocatable :: entry_params(:)
        integer :: num_entry_params

        decl_code = ""
        found_exec_stmt = .false.
        num_entry_params = 0

        if (.not. allocated(func%body_indices)) return

        do i = 1, size(func%body_indices)
            stmt_idx = func%body_indices(i)
            if (.not. arena%has_node_at(stmt_idx)) cycle

            select type (stmt => arena%entries(stmt_idx)%node)
            type is (entry_node)
                call collect_entry_param_names(stmt, entry_params, num_entry_params)
                found_exec_stmt = .true.
            type is (assignment_node)
                found_exec_stmt = .true.
            type is (print_statement_node)
                found_exec_stmt = .true.
            type is (declaration_node)
                if (found_exec_stmt) then
                    if (is_entry_parameter_decl(stmt, entry_params, num_entry_params)) &
                        then
                        decl_code = decl_code // "    " // &
                                    generate_code_from_arena(arena, stmt_idx) // &
                                    new_line('A')
                    end if
                end if
            end select
        end do
    end function collect_entry_parameter_decls


    subroutine rename_result_variable_in_body(node, body, arena)
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable, intent(inout) :: body
        type(ast_arena_t), intent(in) :: arena
        character(len=:), allocatable :: old_name, new_name
        character(len=:), allocatable :: search_pattern, replace_pattern
        integer :: pos, start_pos
        character(len=:), allocatable :: function_name
        logical :: needs_rename
        logical :: missing_result_variable
        logical :: has_return_type

        if (.not. allocated(node%name)) return
        function_name = trim(node%name)

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

        needs_rename = should_rename_deferred_char_result(node) .or. &
                       (node%is_recursive .and. has_return_type .and. &
                        missing_result_variable) .or. &
                       should_use_result_for_array(arena, node)

        if (.not. needs_rename) return

        ! For array functions, rename from function name to result variable
        if (should_use_result_for_array(arena, node)) then
            old_name = function_name
            ! Always use generated name for array functions
            new_name = trim(function_name) // "_result"
        else if (allocated(node%result_variable)) then
            if (len_trim(node%result_variable) > 0) then
                old_name = trim(node%result_variable)
            else
                old_name = function_name
            end if
            new_name = trim(function_name) // "_result"
        else
            old_name = function_name
            new_name = trim(function_name) // "_result"
        end if

        start_pos = 1
        do
            pos = index(body(start_pos:), old_name)
            if (pos == 0) exit

            pos = pos + start_pos - 1

            ! Skip if inside a string literal (quoted text)
            if (is_inside_string_literal(body, pos)) then
                start_pos = pos + len(old_name)
                cycle
            end if

            if (pos > 1) then
                if (is_identifier_char(body(pos - 1:pos - 1))) then
                    start_pos = pos + len(old_name)
                    cycle
                end if
            end if

            if (pos + len(old_name) <= len(body)) then
                ! Skip if followed by '(' AND not preceded by '::'
                ! This distinguishes function calls from array declarations
                ! - foo(n) in assignment/expression = function call (skip)
                ! - :: foo(3) in declaration = array declaration (rename)
                if (body(pos + len(old_name):pos + len(old_name)) == '(') then
                    ! Check if preceded by '::' (declaration context)
                    block
                        integer :: check_pos
                        logical :: is_declaration
                        is_declaration = .false.
                        ! Look backward for '::'
                        do check_pos = pos - 1, max(1, pos - 50), -1
                            if (body(check_pos:check_pos+1) == '::') then
                                is_declaration = .true.
                                exit
                            end if
                            ! Stop at newline or other delimiters
                            if (body(check_pos:check_pos) == new_line('A') .or. &
                                body(check_pos:check_pos) == ';') exit
                        end do
                        ! If not a declaration, skip (it's a function call)
                        if (.not. is_declaration) then
                            start_pos = pos + len(old_name)
                            cycle
                        end if
                    end block
                end if
                ! Skip if followed by another identifier character
                if (is_identifier_char(body(pos + len(old_name):pos + &
                                            len(old_name)))) then
                    start_pos = pos + len(old_name)
                    cycle
                end if
            end if

            body = body(1:pos - 1) // new_name // body(pos + len(old_name):)
            start_pos = pos + len(new_name)
        end do
    end subroutine rename_result_variable_in_body

    logical function should_rename_deferred_char_result(node) result(should_rename)
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: lowered

        should_rename = .false.

        if (.not. allocated(node%name)) return
        if (.not. allocated(node%result_variable)) return
        if (trim(node%result_variable) /= trim(node%name)) return
        if (.not. allocated(node%return_type)) return

        lowered = to_lower(trim(node%return_type))
        should_rename = (index(lowered, 'character') == 1) .and. &
                        (index(lowered, 'len=:') > 0)
    end function should_rename_deferred_char_result

    logical function should_use_result_for_array(arena, node) result(needs_result)
        use ast_nodes_core, only: array_literal_node
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: function_name
        character(len=:), allocatable :: result_name
        integer :: i, stmt_idx, k
        logical :: has_array_declaration
        logical :: value_is_array
        logical :: is_result_target

        needs_result = .false.

        ! Get function name to look for assignments
        if (.not. allocated(node%name)) return
        function_name = trim(node%name)
        if (len_trim(function_name) == 0) return
        if (allocated(node%result_variable)) then
            result_name = trim(node%result_variable)
        else
            result_name = function_name
        end if

        if (.not. allocated(node%body_indices)) return

        ! Check for explicit array declaration of function name
        has_array_declaration = .false.
        do i = 1, size(node%body_indices)
            stmt_idx = node%body_indices(i)
            if (.not. arena%has_node_at(stmt_idx)) cycle

                select type (decl => arena%entries(stmt_idx)%node)
                type is (declaration_node)
                    if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                        do k = 1, size(decl%var_names)
                            if (trim(decl%var_names(k)) == trim(function_name)) then
                                if (decl%is_array .or. &
                                    decl%inferred_type%kind == TARRAY) then
                                    has_array_declaration = .true.
                                    exit
                                end if
                            else if (len_trim(result_name) > 0) then
                                if (trim(decl%var_names(k)) == trim(result_name)) then
                                    if (decl%is_array .or. &
                                        decl%inferred_type%kind == TARRAY) then
                                        has_array_declaration = .true.
                                        exit
                                    end if
                                end if
                            end if
                        end do
                else if (allocated(decl%var_name)) then
                    if (trim(decl%var_name) == trim(function_name)) then
                        if (decl%is_array .or. decl%inferred_type%kind == TARRAY) then
                            has_array_declaration = .true.
                            exit
                        end if
                    end if
                end if
            end select
            if (has_array_declaration) exit
        end do

        ! If we found an explicit array declaration, we need a result clause
        if (has_array_declaration) then
            needs_result = .true.
            return
        end if

        ! Otherwise, check for inferred array type from assignments
        do i = 1, size(node%body_indices)
            stmt_idx = node%body_indices(i)
            if (.not. arena%has_node_at(stmt_idx)) cycle

            select type (stmt => arena%entries(stmt_idx)%node)
                type is (assignment_node)
                    if (stmt%target_index <= 0 .or. stmt%target_index > &
                        arena%size) cycle
                    if (.not. allocated(arena%entries(stmt%target_index)%node)) cycle

                    select type (target => arena%entries(stmt%target_index)%node)
                    type is (identifier_node)
                        if (.not. allocated(target%name)) cycle
                        ! Look for assignment to function result (before rename)
                        is_result_target = (trim(target%name) == trim(function_name))
                        if (.not. is_result_target) then
                            if (len_trim(result_name) > 0) then
                                is_result_target = &
                                    (trim(target%name) == trim(result_name))
                            end if
                        end if
                        if (.not. is_result_target) cycle

                        value_is_array = assignment_value_is_array(arena, stmt)

                        ! Found assignment to function name - check inferred type
                        if (target%inferred_type%kind == TARRAY .or. &
                            value_is_array) then
                            needs_result = .true.
                            return
                        end if
                    end select
                end select
        end do
    end function should_use_result_for_array

    logical function is_identifier_char(c) result(is_id)
        character(len=1), intent(in) :: c
        is_id = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z') .or. &
                (c >= '0' .and. c <= '9') .or. c == '_'
    end function is_identifier_char

    logical function is_inside_string_literal(text, pos) result(inside)
        ! Check if position pos is inside a string literal in text.
        ! Counts quotes from line start to position, odd count means inside string.
        ! Handles both single and double quotes, and escaped quotes.
        character(len=*), intent(in) :: text
        integer, intent(in) :: pos
        integer :: i, line_start, single_count, double_count
        character(len=1) :: c, prev_c

        inside = .false.
        if (pos < 1 .or. pos > len(text)) return

        ! Find start of current line (after last newline before pos)
        line_start = 1
        do i = pos - 1, 1, -1
            if (text(i:i) == new_line('A')) then
                line_start = i + 1
                exit
            end if
        end do

        ! Count unescaped quotes from line start to position
        single_count = 0
        double_count = 0
        prev_c = ' '
        do i = line_start, pos - 1
            c = text(i:i)
            ! In Fortran, doubled quotes inside strings represent literal quote
            ! So we need to track quote pairs properly
            if (c == "'") then
                if (double_count == 0) then
                    ! Not inside double-quoted string
                    if (prev_c == "'") then
                        ! Escaped single quote - decrement to cancel previous
                        single_count = single_count - 1
                    else
                        single_count = single_count + 1
                    end if
                end if
            else if (c == '"') then
                if (single_count == 0) then
                    ! Not inside single-quoted string
                    if (prev_c == '"') then
                        ! Escaped double quote - decrement to cancel previous
                        double_count = double_count - 1
                    else
                        double_count = double_count + 1
                    end if
                end if
            end if
            prev_c = c
        end do

        ! Odd count means we are inside a string
        inside = (mod(single_count, 2) == 1) .or. (mod(double_count, 2) == 1)
    end function is_inside_string_literal
