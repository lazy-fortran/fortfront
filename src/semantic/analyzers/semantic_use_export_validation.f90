module semantic_use_export_validation
    ! Issue #2887 (reject-use-01). F2023 14.2.2: every name and generic spec in
    ! the ONLY list of a USE statement shall be a public entity of the module
    ! being accessed. gfortran.dg/use_9.f90, use_19.f90, operator_6.f90 and
    ! interface_operator_3.f90 are the reference cases.
    !
    ! The rule is applied only when the module is defined in the same source
    ! and its export list is fully understood by this analyzer: a module that
    ! itself accesses another module, or that contains a construct this
    ! analyzer does not model, is skipped. Missing knowledge therefore means
    ! silence, never a rejection.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_misc, only: use_statement_node, interface_block_node, &
        comment_node, implicit_statement_node, module_procedure_node, &
        contains_node, visibility_statement_node
    use ast_nodes_data, only: declaration_node, derived_type_node, &
        parameter_declaration_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use frontend_compiler_resolution, only: is_scope_node, &
        get_scope_statement_indices, find_module_index
    use generic_spec_names, only: normalize_generic_operator, is_generic_spec
    use string_types, only: string_t
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: validate_use_only_exports

contains

    ! Check every USE ... ONLY list in the arena against the exports of the
    ! module it names, when that module is available and fully understood.
    subroutine validate_use_only_exports(arena, errors)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (use_statement_node)
                call check_use_statement(arena, node, errors)
            end select
        end do
    end subroutine validate_use_only_exports

    subroutine check_use_statement(arena, node, errors)
        type(ast_arena_t), intent(in) :: arena
        type(use_statement_node), intent(in) :: node
        type(error_collection_t), intent(inout) :: errors
        type(string_t), allocatable :: exports(:)
        character(len=:), allocatable :: module_name
        integer :: module_index
        integer :: i

        if (.not. node%has_only) return
        if (node%is_intrinsic) return
        if (.not. allocated(node%module_name)) return
        module_name = trim(node%module_name)
        if (len_trim(module_name) == 0) return
        module_index = find_module_index(arena, module_name)
        if (module_index <= 0) return
        if (.not. collect_exports(arena, module_index, exports)) return

        if (allocated(node%only_list)) then
            do i = 1, size(node%only_list)
                call report_missing(node, exports, node%only_list(i)%s, &
                    module_name, errors)
            end do
        end if
        if (allocated(node%rename_list)) then
            i = 2
            do while (i <= size(node%rename_list))
                call report_missing(node, exports, node%rename_list(i)%s, &
                    module_name, errors)
                i = i + 2
            end do
        end if
    end subroutine check_use_statement

    ! Emit the diagnostic when an accessed generic spec is absent from the
    ! module. Plain names are left alone: their export set depends on module
    ! constructs (common blocks, enumerators, ...) this analyzer does not
    ! model, and silence is the only safe answer there.
    subroutine report_missing(node, exports, entity, module_name, errors)
        type(use_statement_node), intent(in) :: node
        type(string_t), intent(in) :: exports(:)
        character(len=*), intent(in) :: entity
        character(len=*), intent(in) :: module_name
        type(error_collection_t), intent(inout) :: errors
        character(len=:), allocatable :: wanted

        if (len_trim(entity) == 0) return
        if (.not. is_generic_spec(entity)) return
        wanted = canonical(entity)
        if (list_contains(exports, wanted)) return

        call errors%add_error( &
            "Generic spec '"//wanted//"' referenced in USE ONLY is not "// &
            "found in module '"//trim(module_name)//"'", &
            severity=ERROR_SEMANTIC, component="semantic_use_export", &
            line=node%line, column=node%column)
    end subroutine report_missing

    ! Gather the names a module makes available. Returns .false. when the
    ! module's export list cannot be determined exactly, in which case no
    ! diagnostic may be derived from it.
    logical function collect_exports(arena, module_index, exports) result(known)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: module_index
        type(string_t), allocatable, intent(out) :: exports(:)
        integer, allocatable :: indices(:)
        integer :: count
        integer :: i

        known = .true.
        count = 0
        allocate (exports(0))
        call get_scope_statement_indices(arena, module_index, indices)
        do i = 1, size(indices)
            if (.not. known) exit
            call add_statement_exports(arena, indices(i), exports, count, known)
        end do
        if (.not. known) return
        exports = exports(1:count)
    end function collect_exports

    subroutine add_statement_exports(arena, node_index, exports, count, known)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(string_t), allocatable, intent(inout) :: exports(:)
        integer, intent(inout) :: count
        logical, intent(inout) :: known
        integer :: i

        if (.not. arena%has_node_at(node_index)) then
            known = .false.
            return
        end if
        select type (node => arena%entries(node_index)%node)
            type is (use_statement_node)
            ! The module re-exports entities this analyzer cannot see.
            known = .false.
            type is (declaration_node)
            if (node%is_multi_declaration) then
                if (allocated(node%var_names)) then
                    do i = 1, size(node%var_names)
                        call append(exports, count, node%var_names(i))
                    end do
                end if
            else if (allocated(node%var_name)) then
                call append(exports, count, node%var_name)
            end if
            type is (parameter_declaration_node)
            if (allocated(node%name)) call append(exports, count, node%name)
            type is (derived_type_node)
            if (allocated(node%name)) call append(exports, count, node%name)
            type is (function_def_node)
            if (allocated(node%name)) call append(exports, count, node%name)
            type is (subroutine_def_node)
            if (allocated(node%name)) call append(exports, count, node%name)
            type is (module_procedure_node)
            if (allocated(node%procedure_names)) then
                do i = 1, size(node%procedure_names)
                    call append(exports, count, node%procedure_names(i)%s)
                end do
            end if
            type is (interface_block_node)
            call add_interface_exports(arena, node, exports, count, known)
            type is (visibility_statement_node)
            ! Visibility narrows exports; a private entity is still present,
            ! so nothing is added and nothing is unknown here.
            type is (implicit_statement_node)
            type is (comment_node)
            type is (contains_node)
        class default
            known = .false.
        end select
    end subroutine add_statement_exports

    ! An interface block exports its generic name or generic spec, and the
    ! names of the specific procedures declared inside a plain interface.
    subroutine add_interface_exports(arena, node, exports, count, known)
        type(ast_arena_t), intent(in) :: arena
        type(interface_block_node), intent(in) :: node
        type(string_t), allocatable, intent(inout) :: exports(:)
        integer, intent(inout) :: count
        logical, intent(inout) :: known
        character(len=:), allocatable :: kind_text
        integer :: i

        kind_text = ''
        if (allocated(node%kind)) kind_text = to_lower(trim(node%kind))
        if (kind_text == "operator" .or. kind_text == "assignment") then
            if (allocated(node%operator)) then
                call append(exports, count, kind_text//"("// &
                    normalize_generic_operator(node%operator)//")")
            end if
        else if (kind_text == "read" .or. kind_text == "write") then
            ! Defined input/output generic specs are not named in ONLY lists
            ! that this rule inspects; treat them as opaque.
            known = .false.
            return
        else if (allocated(node%name)) then
            if (len_trim(node%name) > 0) call append(exports, count, node%name)
        end if

        if (.not. allocated(node%procedure_indices)) return
        do i = 1, size(node%procedure_indices)
            call add_interface_procedure(arena, node%procedure_indices(i), &
                exports, count)
        end do
    end subroutine add_interface_exports

    subroutine add_interface_procedure(arena, node_index, exports, count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(string_t), allocatable, intent(inout) :: exports(:)
        integer, intent(inout) :: count
        integer :: i

        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (function_def_node)
            if (allocated(node%name)) call append(exports, count, node%name)
            type is (subroutine_def_node)
            if (allocated(node%name)) call append(exports, count, node%name)
            type is (module_procedure_node)
            if (allocated(node%procedure_names)) then
                do i = 1, size(node%procedure_names)
                    call append(exports, count, node%procedure_names(i)%s)
                end do
            end if
        end select
    end subroutine add_interface_procedure

    subroutine append(exports, count, name)
        type(string_t), allocatable, intent(inout) :: exports(:)
        integer, intent(inout) :: count
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: canonical_name
        type(string_t), allocatable :: bigger(:)

        if (len_trim(name) == 0) return
        if (count >= size(exports)) then
            allocate (bigger(max(8, 2*size(exports))))
            if (count > 0) bigger(1:count) = exports(1:count)
            call move_alloc(bigger, exports)
        end if
        count = count + 1
        canonical_name = canonical(name)
        exports(count) = string_t(canonical_name)
    end subroutine append

    logical function list_contains(exports, wanted) result(found)
        type(string_t), intent(in) :: exports(:)
        character(len=*), intent(in) :: wanted
        integer :: i

        found = .false.
        do i = 1, size(exports)
            if (.not. allocated(exports(i)%s)) cycle
            if (exports(i)%s == wanted) then
                found = .true.
                return
            end if
        end do
    end function list_contains

    ! Case-insensitive form of a name, with the two spellings of a relational
    ! operator folded onto one generic spec.
    function canonical(name) result(text)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: text
        integer :: open_paren

        text = to_lower(trim(name))
        if (.not. is_generic_spec(text)) return
        open_paren = index(text, "(")
        if (open_paren <= 1) return
        text = text(1:open_paren)// &
            normalize_generic_operator(text(open_paren + 1:len(text) - 1))//")"
    end function canonical

end module semantic_use_export_validation
