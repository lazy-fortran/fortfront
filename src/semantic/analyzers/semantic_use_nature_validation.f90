module semantic_use_nature_validation
    ! Issue #2887 (reject-use-01). F2023 14.2.2 lets a USE statement state the
    ! nature of the module it accesses, but all USE statements of one scoping
    ! unit that name the same module must agree: a module is either intrinsic
    ! or non-intrinsic, never both. gfortran.dg/iso_fortran_env_4.f90 is the
    ! reference case. The rule is checked per scoping unit, so two different
    ! scoping units are free to disagree, and repeating the same nature is
    ! accepted.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_misc, only: use_statement_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use frontend_compiler_resolution, only: is_scope_node, &
        get_scope_statement_indices
    implicit none
    private

    public :: validate_use_module_nature

contains

    ! Report every scoping unit that accesses one module under both the
    ! INTRINSIC and the NON_INTRINSIC nature.
    subroutine validate_use_module_nature(arena, errors)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (.not. is_scope_node(arena, i)) cycle
            call check_scope(arena, i, errors)
        end do
    end subroutine validate_use_module_nature

    ! Compare the module natures of the USE statements directly owned by one
    ! scoping unit. Only the first conflict per USE statement is reported.
    subroutine check_scope(arena, scope_index, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        type(error_collection_t), intent(inout) :: errors
        integer, allocatable :: indices(:)
        integer :: i, j

        call get_scope_statement_indices(arena, scope_index, indices)
        do j = 2, size(indices)
            do i = 1, j - 1
                if (report_if_conflicting(arena, indices(i), indices(j), &
                    errors)) exit
            end do
        end do
    end subroutine check_scope

    ! Whether the two arena entries are USE statements of the same module with
    ! opposite stated natures. Emits the diagnostic when they are.
    logical function report_if_conflicting(arena, first_index, second_index, &
            errors) result(conflicting)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: first_index
        integer, intent(in) :: second_index
        type(error_collection_t), intent(inout) :: errors
        character(len=:), allocatable :: first_name, second_name
        logical :: first_intrinsic, first_non_intrinsic
        logical :: second_intrinsic, second_non_intrinsic
        integer :: ignored_line, ignored_column
        integer :: line, column

        conflicting = .false.
        call use_facts(arena, first_index, first_name, first_intrinsic, &
            first_non_intrinsic, ignored_line, ignored_column)
        if (len_trim(first_name) == 0) return
        call use_facts(arena, second_index, second_name, second_intrinsic, &
            second_non_intrinsic, line, column)
        if (len_trim(second_name) == 0) return
        if (.not. same_name(first_name, second_name)) return

        if (first_intrinsic .and. second_non_intrinsic) then
            conflicting = .true.
        else if (first_non_intrinsic .and. second_intrinsic) then
            conflicting = .true.
        end if
        if (.not. conflicting) return

        call errors%add_error( &
            "Conflicting module nature for module '"//trim(second_name)// &
            "': accessed as INTRINSIC and as NON_INTRINSIC in the same "// &
            "scoping unit", &
            severity=ERROR_SEMANTIC, component="semantic_use_nature", &
            line=line, column=column)
    end function report_if_conflicting

    ! Extract module name, stated nature, and source position from an arena
    ! entry. A non-USE entry yields an empty name.
    subroutine use_facts(arena, node_index, module_name, is_intrinsic, &
            is_non_intrinsic, line, column)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: module_name
        logical, intent(out) :: is_intrinsic
        logical, intent(out) :: is_non_intrinsic
        integer, intent(out) :: line
        integer, intent(out) :: column

        module_name = ''
        is_intrinsic = .false.
        is_non_intrinsic = .false.
        line = 0
        column = 0
        if (.not. arena%has_node_at(node_index)) return
        select type (node => arena%entries(node_index)%node)
            type is (use_statement_node)
            if (.not. allocated(node%module_name)) return
            module_name = trim(node%module_name)
            is_intrinsic = node%is_intrinsic
            is_non_intrinsic = node%is_non_intrinsic
            line = node%line
            column = node%column
        end select
    end subroutine use_facts

    ! Case-insensitive comparison of Fortran names.
    logical function same_name(lhs, rhs) result(equal)
        character(len=*), intent(in) :: lhs
        character(len=*), intent(in) :: rhs

        equal = lowered(trim(lhs)) == lowered(trim(rhs))
    end function same_name

    function lowered(text) result(out)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: out
        integer :: i, code

        do i = 1, len(text)
            code = iachar(text(i:i))
            if (code < iachar('A')) then
                out(i:i) = text(i:i)
            else if (code > iachar('Z')) then
                out(i:i) = text(i:i)
            else
                out(i:i) = achar(code + 32)
            end if
        end do
    end function lowered

end module semantic_use_nature_validation
