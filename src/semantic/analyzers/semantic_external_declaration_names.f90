module semantic_external_declaration_names
    ! Collects the procedure names a scoping unit declares explicitly, for
    ! the IMPLICIT NONE (EXTERNAL) check in
    ! semantic_undefined_variable_checker.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node
    use ast_nodes_misc, only: interface_block_node, use_statement_node, &
        module_procedure_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: collect_declared_procedures

contains

    ! Gather every procedure name the unit declares explicitly: EXTERNAL and
    ! PROCEDURE declarations, interface bodies, MODULE PROCEDURE lists,
    ! contained procedures, and USE ... ONLY items.
    subroutine collect_declared_procedures(arena, body_indices, declared, &
            declared_count, usable)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=64), intent(out) :: declared(:)
        integer, intent(out) :: declared_count
        logical, intent(out) :: usable
        integer :: i, j

        declared_count = 0
        usable = .true.

        do i = 1, size(body_indices)
            if (body_indices(i) <= 0) cycle
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (stmt => arena%entries(body_indices(i))%node)
                type is (use_statement_node)
                if (.not. stmt%has_only) then
                    usable = .false.
                    return
                end if
                if (allocated(stmt%only_list)) then
                    do j = 1, size(stmt%only_list)
                        if (.not. allocated(stmt%only_list(j)%s)) cycle
                        call add_declared(declared, declared_count, &
                            stmt%only_list(j)%s, usable)
                        if (.not. usable) return
                    end do
                end if
                if (allocated(stmt%rename_list)) then
                    do j = 1, size(stmt%rename_list)
                        if (.not. allocated(stmt%rename_list(j)%s)) cycle
                        call add_declared(declared, declared_count, &
                            stmt%rename_list(j)%s, usable)
                        if (.not. usable) return
                    end do
                end if
                type is (declaration_node)
                call add_declaration_names(arena, stmt, declared, declared_count, &
                    usable)
                if (.not. usable) return
                type is (interface_block_node)
                call add_interface_names(arena, stmt, declared, declared_count, &
                    usable)
                if (.not. usable) return
                type is (module_procedure_node)
                if (allocated(stmt%procedure_names)) then
                    do j = 1, size(stmt%procedure_names)
                        if (.not. allocated(stmt%procedure_names(j)%s)) cycle
                        call add_declared(declared, declared_count, &
                            stmt%procedure_names(j)%s, usable)
                        if (.not. usable) return
                    end do
                end if
                type is (function_def_node)
                call add_declared(declared, declared_count, stmt%name, usable)
                if (.not. usable) return
                type is (subroutine_def_node)
                call add_declared(declared, declared_count, stmt%name, usable)
                if (.not. usable) return
            end select
        end do
    end subroutine collect_declared_procedures

    subroutine add_declaration_names(arena, decl, declared, declared_count, usable)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: decl
        character(len=64), intent(inout) :: declared(:)
        integer, intent(inout) :: declared_count
        logical, intent(inout) :: usable
        logical :: names_a_procedure
        integer :: j

        names_a_procedure = decl%is_external
        if (allocated(decl%type_name)) then
            if (len(decl%type_name) >= 9) then
                if (to_lower(decl%type_name(1:9)) == 'procedure') then
                    names_a_procedure = .true.
                end if
            end if
        end if
        if (.not. names_a_procedure) return

        if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
            do j = 1, size(decl%var_names)
                call add_declared(declared, declared_count, decl%var_names(j), usable)
                if (.not. usable) return
            end do
            return
        end if
        if (allocated(decl%var_name)) then
            call add_declared(declared, declared_count, decl%var_name, usable)
        end if
    end subroutine add_declaration_names

    subroutine add_interface_names(arena, iface, declared, declared_count, usable)
        type(ast_arena_t), intent(in) :: arena
        type(interface_block_node), intent(in) :: iface
        character(len=64), intent(inout) :: declared(:)
        integer, intent(inout) :: declared_count
        logical, intent(inout) :: usable
        integer :: j

        if (allocated(iface%name)) then
            call add_declared(declared, declared_count, iface%name, usable)
            if (.not. usable) return
        end if
        if (.not. allocated(iface%procedure_indices)) return
        do j = 1, size(iface%procedure_indices)
            if (iface%procedure_indices(j) <= 0) cycle
            if (.not. arena%has_node_at(iface%procedure_indices(j))) cycle
            select type (proc => arena%entries(iface%procedure_indices(j))%node)
                type is (function_def_node)
                call add_declared(declared, declared_count, proc%name, usable)
                type is (subroutine_def_node)
                call add_declared(declared, declared_count, proc%name, usable)
                type is (module_procedure_node)
                block
                    integer :: k
                    if (allocated(proc%procedure_names)) then
                        do k = 1, size(proc%procedure_names)
                            if (.not. allocated(proc%procedure_names(k)%s)) cycle
                            call add_declared(declared, declared_count, &
                                proc%procedure_names(k)%s, usable)
                        end do
                    end if
                end block
            end select
            if (.not. usable) return
        end do
    end subroutine add_interface_names

    subroutine add_declared(declared, declared_count, name, usable)
        character(len=64), intent(inout) :: declared(:)
        integer, intent(inout) :: declared_count
        character(len=*), intent(in) :: name
        logical, intent(inout) :: usable

        if (len_trim(name) == 0) return
        if (len_trim(name) > len(declared)) return
        if (declared_count >= size(declared)) then
            usable = .false.
            return
        end if
        declared_count = declared_count + 1
        declared(declared_count) = to_lower(trim(name))
    end subroutine add_declared

end module semantic_external_declaration_names
