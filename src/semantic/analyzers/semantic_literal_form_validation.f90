module semantic_literal_form_validation
    ! Rejects malformed or disallowed literal forms (issue #2894).
    !
    ! One constraint family: a literal constant must denote a value that the
    ! standard actually allows at that point in the program.
    !
    !   * A kind parameter written as a name (`0.0_dp`) must be a named integer
    !     constant that is available in the compilation unit (F2018 R764,
    !     C716). An undefined name leaves the literal without a kind.
    !   * A named constant imported from the intrinsic module ISO_FORTRAN_ENV
    !     must be an entity that module actually defines. The unsigned kind
    !     names (`uint8` ... `uint64`) are a vendor extension, not standard.
    !   * A procedure name is not a constant and not a variable, so it is not a
    !     valid value in an output list (`print *, (erfc)`).
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL
    use ast_nodes_core, only: literal_node, identifier_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use ast_nodes_io, only: print_statement_node
    use ast_nodes_misc, only: use_statement_node
    use ast_nodes_procedure, only: function_def_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use string_utils_mod, only: int_to_string, to_lower
    use string_types, only: string_t
    implicit none
    private

    public :: validate_literal_forms

contains

    ! Walk the arena once, gathering the names a literal kind parameter could
    ! refer to together with the nodes each rule has to inspect, then report.
    subroutine validate_literal_forms(arena, errors, standard_mode)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        logical, intent(in) :: standard_mode
        type(string_t), allocatable :: available(:)
        type(string_t), allocatable :: procedures(:)
        integer, allocatable :: kind_literals(:)
        integer, allocatable :: output_items(:)
        integer, allocatable :: imports(:)
        integer :: available_count, procedure_count
        integer :: kind_count, output_count, import_count
        logical :: unrestricted_use

        call scan_arena(arena, available, available_count, procedures, &
                        procedure_count, kind_literals, kind_count, &
                        output_items, output_count, imports, import_count, &
                        unrestricted_use)

        ! Lazy Fortran supplies kind names such as `dp` implicitly, so a named
        ! kind parameter is only required to be declared in standard input.
        if (standard_mode .and. .not. unrestricted_use) then
            call check_kind_parameters(arena, kind_literals, kind_count, &
                                       available, available_count, errors)
        end if
        call check_intrinsic_module_imports(arena, imports, import_count, errors)
        call check_output_list_items(arena, output_items, output_count, &
                                     available, available_count, procedures, &
                                     procedure_count, errors)
    end subroutine validate_literal_forms

    ! Single arena traversal feeding every rule below.
    subroutine scan_arena(arena, available, available_count, procedures, &
                          procedure_count, kind_literals, kind_count, &
                          output_items, output_count, imports, import_count, &
                          unrestricted_use)
        type(ast_arena_t), intent(in) :: arena
        type(string_t), allocatable, intent(out) :: available(:)
        integer, intent(out) :: available_count
        type(string_t), allocatable, intent(out) :: procedures(:)
        integer, intent(out) :: procedure_count
        integer, allocatable, intent(out) :: kind_literals(:)
        integer, intent(out) :: kind_count
        integer, allocatable, intent(out) :: output_items(:)
        integer, intent(out) :: output_count
        integer, allocatable, intent(out) :: imports(:)
        integer, intent(out) :: import_count
        logical, intent(out) :: unrestricted_use
        character(len=:), allocatable :: suffix
        integer :: i, j

        allocate (available(0))
        allocate (procedures(0))
        allocate (kind_literals(0))
        allocate (output_items(0))
        allocate (imports(0))
        available_count = 0
        procedure_count = 0
        kind_count = 0
        output_count = 0
        import_count = 0
        unrestricted_use = .false.

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (literal_node)
                if (.not. allocated(node%value)) cycle
                if (.not. is_numeric_literal(node%literal_kind)) cycle
                suffix = kind_suffix(node%value)
                if (len(suffix) == 0) cycle
                if (is_digit_string(suffix)) cycle
                call append_index(kind_literals, kind_count, i)
                type is (print_statement_node)
                if (.not. allocated(node%expression_indices)) cycle
                do j = 1, size(node%expression_indices)
                    call append_index(output_items, output_count, &
                                      node%expression_indices(j))
                end do
                type is (declaration_node)
                if (allocated(node%var_name)) then
                    call append(available, available_count, node%var_name)
                end if
                if (allocated(node%var_names)) then
                    do j = 1, size(node%var_names)
                        call append(available, available_count, node%var_names(j))
                    end do
                end if
                type is (parameter_declaration_node)
                if (allocated(node%name)) then
                    call append(available, available_count, node%name)
                end if
                type is (function_def_node)
                if (allocated(node%name)) then
                    call append(procedures, procedure_count, node%name)
                end if
                type is (use_statement_node)
                if (.not. node%has_only) unrestricted_use = .true.
                call append_index(imports, import_count, i)
                if (allocated(node%only_list)) then
                    do j = 1, size(node%only_list)
                        if (.not. allocated(node%only_list(j)%s)) cycle
                        call append(available, available_count, &
                                    local_name(node%only_list(j)%s))
                    end do
                end if
                if (allocated(node%rename_list)) then
                    do j = 1, size(node%rename_list)
                        if (.not. allocated(node%rename_list(j)%s)) cycle
                        call append(available, available_count, &
                                    local_name(node%rename_list(j)%s))
                    end do
                end if
            end select
        end do
    end subroutine scan_arena

    ! A named kind parameter must denote a named integer constant in scope.
    subroutine check_kind_parameters(arena, kind_literals, kind_count, &
                                     available, available_count, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: kind_literals(:)
        integer, intent(in) :: kind_count
        type(string_t), intent(in) :: available(:)
        integer, intent(in) :: available_count
        type(error_collection_t), intent(inout) :: errors
        character(len=:), allocatable :: suffix
        integer :: i

        do i = 1, kind_count
            select type (node => arena%entries(kind_literals(i))%node)
                type is (literal_node)
                suffix = kind_suffix(node%value)
                if (name_present(available, available_count, suffix)) cycle
                call report(errors, 'Missing kind-parameter: '//suffix// &
                            ' in literal constant '//node%value// &
                            ' is not a named integer constant', &
                            node%line, node%column, &
                            'declare the kind as an integer parameter before use')
            end select
        end do
    end subroutine check_kind_parameters

    ! ONLY-list names taken from ISO_FORTRAN_ENV must exist in that module.
    subroutine check_intrinsic_module_imports(arena, imports, import_count, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: imports(:)
        integer, intent(in) :: import_count
        type(error_collection_t), intent(inout) :: errors
        character(len=:), allocatable :: imported
        integer :: i, j

        do i = 1, import_count
            select type (node => arena%entries(imports(i))%node)
                type is (use_statement_node)
                if (.not. allocated(node%module_name)) cycle
                if (to_lower(node%module_name) /= 'iso_fortran_env') cycle
                if (.not. allocated(node%only_list)) cycle
                do j = 1, size(node%only_list)
                    if (.not. allocated(node%only_list(j)%s)) cycle
                    imported = remote_name(node%only_list(j)%s)
                    if (len(imported) == 0) cycle
                    if (is_iso_fortran_env_entity(imported)) cycle
                    call report(errors, 'Invalid literal kind import: '// &
                                imported//' is not an entity of the intrinsic '// &
                                'module ISO_FORTRAN_ENV in the selected standard', &
                                node%line, node%column, &
                                'use a standard kind such as int32 or real64')
                end do
            end select
        end do
    end subroutine check_intrinsic_module_imports

    ! A bare procedure name is neither a constant nor a variable, so it cannot
    ! appear as an output list item.
    subroutine check_output_list_items(arena, output_items, output_count, &
                                       available, available_count, procedures, &
                                       procedure_count, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: output_items(:)
        integer, intent(in) :: output_count
        type(string_t), intent(in) :: available(:)
        integer, intent(in) :: available_count
        type(string_t), intent(in) :: procedures(:)
        integer, intent(in) :: procedure_count
        type(error_collection_t), intent(inout) :: errors
        integer :: i, item_index

        if (procedure_count == 0) return

        do i = 1, output_count
            item_index = output_items(i)
            if (item_index <= 0) cycle
            if (.not. arena%has_node_at(item_index)) cycle
            select type (item => arena%entries(item_index)%node)
                type is (identifier_node)
                if (.not. allocated(item%name)) cycle
                if (name_present(available, available_count, item%name)) cycle
                if (.not. name_present(procedures, procedure_count, &
                                       item%name)) cycle
                call report(errors, 'Invalid constant in output list: '// &
                            item%name//' is a procedure name, not a floating '// &
                            'constant or variable', item%line, item%column, &
                            'call the procedure with an argument list, for '// &
                            'example '//item%name//'()')
            end select
        end do
    end subroutine check_output_list_items

    ! Append an arena index with geometric growth.
    subroutine append_index(indices, count, value)
        integer, allocatable, intent(inout) :: indices(:)
        integer, intent(inout) :: count
        integer, intent(in) :: value
        integer, allocatable :: grown(:)
        integer :: capacity

        capacity = size(indices)
        if (count >= capacity) then
            allocate (grown(max(16, 2*capacity)))
            if (count > 0) grown(1:count) = indices(1:count)
            call move_alloc(grown, indices)
        end if
        count = count + 1
        indices(count) = value
    end subroutine append_index

    ! Add one diagnostic of this rule family.
    subroutine report(errors, message, line, column, suggestion)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: message
        integer, intent(in) :: line, column
        character(len=*), intent(in) :: suggestion

        call errors%add_error( &
            message=message, &
            code=ERROR_SEMANTIC, &
            component='semantic_literal_form_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), &
            suggestion=suggestion, line=line, column=column, &
            end_line=line, end_column=column + 1)
    end subroutine report

    ! The kind suffix of a numeric literal, empty when there is none.
    function kind_suffix(value) result(suffix)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: suffix
        integer :: i

        suffix = ''
        do i = 1, len(value)
            if (value(i:i) == '_') then
                if (i < len(value)) suffix = value(i + 1:)
                return
            end if
        end do
    end function kind_suffix

    ! Whether a literal carries a numeric type that admits a kind parameter.
    function is_numeric_literal(literal_kind) result(is_numeric)
        integer, intent(in) :: literal_kind
        logical :: is_numeric

        is_numeric = literal_kind == LITERAL_REAL .or. &
            literal_kind == LITERAL_INTEGER
    end function is_numeric_literal

    function is_digit_string(text) result(all_digits)
        character(len=*), intent(in) :: text
        logical :: all_digits
        integer :: i

        all_digits = len(text) > 0
        do i = 1, len(text)
            if (text(i:i) < '0' .or. text(i:i) > '9') then
                all_digits = .false.
                return
            end if
        end do
    end function is_digit_string

    ! Local name of an ONLY item: `new => old` binds `new` locally.
    function local_name(item) result(name)
        character(len=*), intent(in) :: item
        character(len=:), allocatable :: name
        integer :: arrow

        arrow = index(item, '=>')
        if (arrow > 0) then
            name = trim(adjustl(item(1:arrow - 1)))
        else
            name = trim(adjustl(item))
        end if
    end function local_name

    ! Name that an ONLY item requests from the module.
    function remote_name(item) result(name)
        character(len=*), intent(in) :: item
        character(len=:), allocatable :: name
        integer :: arrow

        arrow = index(item, '=>')
        if (arrow > 0) then
            name = trim(adjustl(item(arrow + 2:)))
        else
            name = trim(adjustl(item))
        end if
    end function remote_name

    function name_present(names, count, name) result(found)
        type(string_t), intent(in) :: names(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: name
        logical :: found
        character(len=:), allocatable :: target_name
        integer :: i

        found = .false.
        target_name = to_lower(trim(name))
        do i = 1, count
            if (.not. allocated(names(i)%s)) cycle
            if (to_lower(trim(names(i)%s)) == target_name) then
                found = .true.
                return
            end if
        end do
    end function name_present

    ! Append with geometric growth. Entries past `count` are unused padding,
    ! so every lookup must respect the count carried alongside the array.
    subroutine append(names, count, name)
        type(string_t), allocatable, intent(inout) :: names(:)
        integer, intent(inout) :: count
        character(len=*), intent(in) :: name
        type(string_t), allocatable :: grown(:)
        integer :: capacity

        if (len_trim(name) == 0) return
        capacity = size(names)
        if (count >= capacity) then
            allocate (grown(max(16, 2*capacity)))
            if (count > 0) grown(1:count) = names(1:count)
            call move_alloc(grown, names)
        end if
        count = count + 1
        names(count)%s = trim(name)
    end subroutine append

    ! Public entities of the intrinsic module ISO_FORTRAN_ENV (F2018 16.10.2).
    function is_iso_fortran_env_entity(name) result(is_entity)
        character(len=*), intent(in) :: name
        logical :: is_entity
        character(len=:), allocatable :: lowered

        lowered = to_lower(trim(name))
        select case (lowered)
        case ('atomic_int_kind', 'atomic_logical_kind', 'character_kinds', &
              'character_storage_size', 'compiler_options', 'compiler_version', &
              'current_team', 'error_unit', 'event_type', 'file_storage_size', &
              'initial_team', 'input_unit', 'int8', 'int16', 'int32', 'int64', &
              'integer_kinds', 'iostat_end', 'iostat_eor', &
              'iostat_inquire_internal_unit', 'lock_type', 'logical_kinds', &
              'notify_type', 'numeric_storage_size', 'output_unit', &
              'parent_team', 'real16', 'real32', 'real64', 'real128', &
              'real_kinds', 'stat_failed_image', 'stat_locked', &
              'stat_locked_other_image', 'stat_stopped_image', 'stat_unlocked', &
              'stat_unlocked_failed_image', 'team_type')
            is_entity = .true.
        case default
            is_entity = .false.
        end select
    end function is_iso_fortran_env_entity

end module semantic_literal_form_validation
