module semantic_interface_declaration_validation
    ! Issue #2883 (reject-interface-01). Explicit-interface declaration rules.
    !
    ! An interface body declares a name that is external and has an explicit
    ! interface (F2018 15.4.3.2). Within the scoping unit that contains the
    ! interface block, that name shall not be given the EXTERNAL or INTRINSIC
    ! attribute again, shall not be given attributes outside the interface body,
    ! and shall not also be defined as a contained procedure. A name declared
    ! INTRINSIC likewise cannot be listed by a module-procedure-stmt.
    !
    ! Reference cases: gfortran.dg/interface_23.f90, interface_24.f90,
    ! derived_function_interface_1.f90, module_procedure_2.f90.
    !
    ! Interface bodies carrying the MODULE prefix declare separate module
    ! procedures, which are legitimately defined in the CONTAINS part of the
    ! same module, so they are excluded from every rule below.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_data, only: declaration_node, module_node, &
        multi_unit_container_node
    use ast_nodes_misc, only: contains_node, interface_block_node, &
        intrinsic_statement_node, module_procedure_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: validate_interface_declarations

    integer, parameter :: MAX_NAMES = 256

    ! Names collected from one scoping unit.
    type :: name_set_t
        character(len=64) :: names(MAX_NAMES) = ""
        integer :: count = 0
    end type name_set_t

contains

    ! Check every scoping unit that can hold an interface block.
    subroutine validate_interface_declarations(arena, errors)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors

        integer :: i

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
            type is (module_node)
                call check_scope(arena, errors, node%declaration_indices, &
                    node%procedure_indices)
                call check_assumed_length_results(arena, errors, &
                    node%procedure_indices)
            type is (multi_unit_container_node)
                call check_container(arena, errors, node%body_indices)
            type is (program_node)
                call check_scope(arena, errors, node%body_indices)
                call check_internal_assumed_length(arena, errors, &
                    node%body_indices)
            type is (function_def_node)
                call check_scope(arena, errors, node%body_indices)
                call check_internal_assumed_length(arena, errors, &
                    node%body_indices)
            type is (subroutine_def_node)
                call check_scope(arena, errors, node%body_indices)
                call check_internal_assumed_length(arena, errors, &
                    node%body_indices)
            class default
                cycle
            end select
        end do
    end subroutine validate_interface_declarations

    ! F2018 C721: a function whose result has assumed character length shall be
    ! an external function or a dummy procedure, so it may not be declared by
    ! an interface body's host as a module or internal procedure.
    ! gfortran.dg/assumed_charlen_function_6.f90 is the reference case.
    subroutine check_assumed_length_results(arena, errors, procedure_indices)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer, allocatable, intent(in) :: procedure_indices(:)

        integer :: i

        if (.not. allocated(procedure_indices)) return
        do i = 1, size(procedure_indices)
            call check_one_assumed_length(arena, errors, procedure_indices(i))
        end do
    end subroutine check_assumed_length_results

    ! Same rule for procedures after CONTAINS in a program or procedure body.
    subroutine check_internal_assumed_length(arena, errors, body_indices)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer, allocatable, intent(in) :: body_indices(:)

        integer :: i
        logical :: after_contains

        if (.not. allocated(body_indices)) return
        after_contains = .false.
        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (node => arena%entries(body_indices(i))%node)
            type is (contains_node)
                after_contains = .true.
            class default
                if (after_contains) call check_one_assumed_length(arena, errors, &
                    body_indices(i))
            end select
        end do
    end subroutine check_internal_assumed_length

    subroutine check_one_assumed_length(arena, errors, node_index)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer, intent(in) :: node_index

        integer :: i
        character(len=:), allocatable :: result_name

        if (.not. arena%has_node_at(node_index)) return
        select type (func => arena%entries(node_index)%node)
        type is (function_def_node)
            if (.not. allocated(func%name)) return
            result_name = to_lower(trim(func%name))
            if (allocated(func%result_variable)) then
                if (len_trim(func%result_variable) > 0) then
                    result_name = to_lower(trim(func%result_variable))
                end if
            end if
            if (.not. allocated(func%body_indices)) return
            do i = 1, size(func%body_indices)
                if (.not. arena%has_node_at(func%body_indices(i))) cycle
                select type (decl => arena%entries(func%body_indices(i))%node)
                type is (declaration_node)
                    if (.not. decl%has_character_length) cycle
                    if (.not. allocated(decl%character_length_expr)) cycle
                    if (trim(decl%character_length_expr) /= "*") cycle
                    if (.not. allocated(decl%var_name)) cycle
                    if (to_lower(trim(decl%var_name)) /= result_name) cycle
                    call errors%add_error("Character-valued result of '"// &
                        trim(func%name)//"' has assumed length, which is "// &
                        "allowed only for an external function or a dummy "// &
                        "procedure", severity=ERROR_SEMANTIC, &
                        component="semantic_interface_declaration", &
                        line=decl%line, column=decl%column)
                    return
                class default
                    cycle
                end select
            end do
        class default
            return
        end select
    end subroutine check_one_assumed_length

    ! Apply every rule of the family to one scoping unit. `spec_indices` holds
    ! the specification (and, for a program or procedure, execution) part;
    ! `contained_indices` holds the procedures after CONTAINS when the node
    ! keeps them separately, as module_node does.
    subroutine check_scope(arena, errors, spec_indices, contained_indices)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer, allocatable, intent(in) :: spec_indices(:)
        integer, allocatable, intent(in), optional :: contained_indices(:)

        type(name_set_t) :: interface_names
        type(name_set_t) :: intrinsic_names
        integer :: i
        logical :: after_contains

        if (.not. allocated(spec_indices)) return

        call collect_names(arena, spec_indices, interface_names, intrinsic_names)
        if (interface_names%count == 0 .and. intrinsic_names%count == 0) return

        after_contains = .false.
        do i = 1, size(spec_indices)
            if (.not. arena%has_node_at(spec_indices(i))) cycle
            select type (node => arena%entries(spec_indices(i))%node)
            type is (contains_node)
                after_contains = .true.
            type is (declaration_node)
                call check_declaration(errors, node, interface_names)
            type is (intrinsic_statement_node)
                call check_intrinsic_statement(errors, node, interface_names)
            type is (interface_block_node)
                call check_module_procedures(arena, errors, node, intrinsic_names)
            type is (function_def_node)
                if (after_contains) call check_contained_name(errors, node%name, &
                    node%line, node%column, interface_names)
            type is (subroutine_def_node)
                if (after_contains) call check_contained_name(errors, node%name, &
                    node%line, node%column, interface_names)
            class default
                cycle
            end select
        end do

        if (.not. present(contained_indices)) return
        if (.not. allocated(contained_indices)) return
        do i = 1, size(contained_indices)
            if (.not. arena%has_node_at(contained_indices(i))) cycle
            select type (node => arena%entries(contained_indices(i))%node)
            type is (function_def_node)
                call check_contained_name(errors, node%name, node%line, &
                    node%column, interface_names)
            type is (subroutine_def_node)
                call check_contained_name(errors, node%name, node%line, &
                    node%column, interface_names)
            class default
                cycle
            end select
        end do
    end subroutine check_scope

    ! An implicit main program (one with no PROGRAM statement) leaves its
    ! leading interface blocks as direct children of the multi-unit container.
    ! No valid source has an interface block outside a program unit, so such a
    ! block belongs to the main program of the same file and its names are
    ! checked against that program's contained procedures.
    subroutine check_container(arena, errors, body_indices)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer, allocatable, intent(in) :: body_indices(:)

        type(name_set_t) :: interface_names
        type(name_set_t) :: intrinsic_names
        integer :: i

        if (.not. allocated(body_indices)) return
        call collect_names(arena, body_indices, interface_names, intrinsic_names)
        if (interface_names%count == 0) return

        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (node => arena%entries(body_indices(i))%node)
            type is (program_node)
                call check_contained_procedures(arena, errors, &
                    node%body_indices, interface_names)
            class default
                cycle
            end select
        end do
    end subroutine check_container

    ! Report contained procedures of one program unit whose names an interface
    ! body already declared.
    subroutine check_contained_procedures(arena, errors, body_indices, &
            interface_names)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer, allocatable, intent(in) :: body_indices(:)
        type(name_set_t), intent(in) :: interface_names

        integer :: i
        logical :: after_contains

        if (.not. allocated(body_indices)) return
        after_contains = .false.
        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (node => arena%entries(body_indices(i))%node)
            type is (contains_node)
                after_contains = .true.
            type is (function_def_node)
                if (after_contains) call check_contained_name(errors, node%name, &
                    node%line, node%column, interface_names)
            type is (subroutine_def_node)
                if (after_contains) call check_contained_name(errors, node%name, &
                    node%line, node%column, interface_names)
            class default
                cycle
            end select
        end do
    end subroutine check_contained_procedures

    ! Names declared by interface bodies, and names declared INTRINSIC, in one
    ! scoping unit.
    subroutine collect_names(arena, indices, interface_names, intrinsic_names)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: indices(:)
        type(name_set_t), intent(inout) :: interface_names
        type(name_set_t), intent(inout) :: intrinsic_names

        integer :: i, j

        do i = 1, size(indices)
            if (.not. arena%has_node_at(indices(i))) cycle
            select type (node => arena%entries(indices(i))%node)
            type is (interface_block_node)
                if (node%is_abstract) cycle
                if (.not. allocated(node%procedure_indices)) cycle
                do j = 1, size(node%procedure_indices)
                    call collect_interface_body_name(arena, &
                        node%procedure_indices(j), interface_names)
                end do
            type is (intrinsic_statement_node)
                if (.not. allocated(node%procedure_names)) cycle
                do j = 1, size(node%procedure_names)
                    call add_name(intrinsic_names, node%procedure_names(j)%s)
                end do
            class default
                cycle
            end select
        end do
    end subroutine collect_names

    ! Record the name an interface body declares, unless the body carries the
    ! MODULE prefix (a separate module procedure interface).
    subroutine collect_interface_body_name(arena, node_index, interface_names)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(name_set_t), intent(inout) :: interface_names

        if (.not. arena%has_node_at(node_index)) return
        select type (proc => arena%entries(node_index)%node)
        type is (function_def_node)
            if (has_module_prefix(proc%prefix_keywords)) return
            if (.not. allocated(proc%name)) return
            call add_name(interface_names, proc%name)
        type is (subroutine_def_node)
            if (has_module_prefix(proc%prefix_keywords)) return
            if (.not. allocated(proc%name)) return
            call add_name(interface_names, proc%name)
        class default
            return
        end select
    end subroutine collect_interface_body_name

    logical function has_module_prefix(prefix_keywords) result(is_module)
        character(len=16), allocatable, intent(in) :: prefix_keywords(:)

        integer :: i

        is_module = .false.
        if (.not. allocated(prefix_keywords)) return
        do i = 1, size(prefix_keywords)
            if (to_lower(trim(prefix_keywords(i))) == "module") then
                is_module = .true.
                return
            end if
        end do
    end function has_module_prefix

    ! F2018 C1519 and 8.5.9: attributes of a name declared by an interface body
    ! are specified inside that body. EXTERNAL is implied by the interface body
    ! itself, so restating it duplicates the attribute.
    subroutine check_declaration(errors, decl, interface_names)
        type(error_collection_t), intent(inout) :: errors
        type(declaration_node), intent(in) :: decl
        type(name_set_t), intent(in) :: interface_names

        integer :: i

        if (decl%is_multi_declaration) then
            if (.not. allocated(decl%var_names)) return
            do i = 1, size(decl%var_names)
                call report_declaration(errors, decl, decl%var_names(i), &
                    interface_names)
            end do
        else
            if (.not. allocated(decl%var_name)) return
            call report_declaration(errors, decl, decl%var_name, interface_names)
        end if
    end subroutine check_declaration

    subroutine report_declaration(errors, decl, name, interface_names)
        type(error_collection_t), intent(inout) :: errors
        type(declaration_node), intent(in) :: decl
        character(len=*), intent(in) :: name
        type(name_set_t), intent(in) :: interface_names

        if (.not. contains_name(interface_names, name)) return

        if (decl%is_external) then
            call errors%add_error("Duplicate EXTERNAL attribute for '"// &
                trim(name)//"': its INTERFACE body already makes it external", &
                severity=ERROR_SEMANTIC, &
                component="semantic_interface_declaration", &
                line=decl%line, column=decl%column)
        else
            call errors%add_error("Attribute of '"//trim(name)// &
                "' is declared outside its INTERFACE body", &
                severity=ERROR_SEMANTIC, &
                component="semantic_interface_declaration", &
                line=decl%line, column=decl%column)
        end if
    end subroutine report_declaration

    ! F2018 C846: INTRINSIC and an explicit interface for an external procedure
    ! cannot both apply to one name.
    subroutine check_intrinsic_statement(errors, stmt, interface_names)
        type(error_collection_t), intent(inout) :: errors
        type(intrinsic_statement_node), intent(in) :: stmt
        type(name_set_t), intent(in) :: interface_names

        integer :: i

        if (.not. allocated(stmt%procedure_names)) return
        do i = 1, size(stmt%procedure_names)
            if (.not. contains_name(interface_names, stmt%procedure_names(i)%s)) &
                cycle
            call errors%add_error("INTRINSIC attribute of '"// &
                trim(stmt%procedure_names(i)%s)// &
                "' conflicts with the EXTERNAL attribute implied by its "// &
                "INTERFACE body", &
                severity=ERROR_SEMANTIC, &
                component="semantic_interface_declaration", &
                line=stmt%line, column=stmt%column)
        end do
    end subroutine check_intrinsic_statement

    ! F2018 C1514: a name in a module-procedure-stmt shall be accessible as a
    ! module procedure. A name declared INTRINSIC is not one.
    subroutine check_module_procedures(arena, errors, block, intrinsic_names)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        type(interface_block_node), intent(in) :: block
        type(name_set_t), intent(in) :: intrinsic_names

        integer :: i, j

        if (intrinsic_names%count == 0) return
        if (.not. allocated(block%procedure_indices)) return
        do i = 1, size(block%procedure_indices)
            if (.not. arena%has_node_at(block%procedure_indices(i))) cycle
            select type (proc => arena%entries(block%procedure_indices(i))%node)
            type is (module_procedure_node)
                ! Only MODULE PROCEDURE is restricted to module procedures; a
                ! plain PROCEDURE statement may name an intrinsic (F2018
                ! C1512), as gfortran.dg/pr95500.f90 does.
                if (.not. proc%has_module_prefix) cycle
                if (.not. allocated(proc%procedure_names)) cycle
                do j = 1, size(proc%procedure_names)
                    if (.not. contains_name(intrinsic_names, &
                        proc%procedure_names(j)%s)) cycle
                    call errors%add_error("'"// &
                        trim(proc%procedure_names(j)%s)// &
                        "' is declared INTRINSIC and cannot be a MODULE "// &
                        "PROCEDURE", severity=ERROR_SEMANTIC, &
                        component="semantic_interface_declaration", &
                        line=proc%line, column=proc%column)
                end do
            class default
                cycle
            end select
        end do
    end subroutine check_module_procedures

    ! A contained procedure already has an explicit interface, so an interface
    ! body for the same name in the host is invalid.
    subroutine check_contained_name(errors, name, line, column, interface_names)
        type(error_collection_t), intent(inout) :: errors
        character(len=:), allocatable, intent(in) :: name
        integer, intent(in) :: line
        integer, intent(in) :: column
        type(name_set_t), intent(in) :: interface_names

        if (.not. allocated(name)) return
        if (.not. contains_name(interface_names, name)) return

        call errors%add_error("'"//trim(name)// &
            "' already has an explicit interface: an INTERFACE body in the "// &
            "host cannot declare a contained procedure", &
            severity=ERROR_SEMANTIC, &
            component="semantic_interface_declaration", &
            line=line, column=column)
    end subroutine check_contained_name

    subroutine add_name(set, name)
        type(name_set_t), intent(inout) :: set
        character(len=*), intent(in) :: name

        if (len_trim(name) == 0) return
        if (set%count >= MAX_NAMES) return
        set%count = set%count + 1
        set%names(set%count) = to_lower(trim(name))
    end subroutine add_name

    logical function contains_name(set, name) result(found)
        type(name_set_t), intent(in) :: set
        character(len=*), intent(in) :: name

        integer :: i
        character(len=:), allocatable :: lowered

        found = .false.
        if (len_trim(name) == 0) return
        lowered = to_lower(trim(name))
        do i = 1, set%count
            if (trim(set%names(i)) == lowered) then
                found = .true.
                return
            end if
        end do
    end function contains_name

end module semantic_interface_declaration_validation
