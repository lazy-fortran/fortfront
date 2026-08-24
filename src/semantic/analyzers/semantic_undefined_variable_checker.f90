module semantic_undefined_variable_checker
    ! Undefined variable detection and checking utilities
    ! Extracted from semantic_analyzer.f90 for architectural compliance (Issue #1067)
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, binary_op_node, assignment_node, &
        call_or_subscript_node, array_literal_node, program_node, &
        component_access_node, pointer_assignment_node
    use ast_nodes_conditional, only: if_node, select_case_node, case_block_node, &
        select_type_node, type_guard_block_node
    use ast_nodes_control, only: do_loop_node, do_while_node, case_default_node, &
        forall_node
    use ast_nodes_associate, only: associate_node, block_construct_node
    use ast_nodes_data, only: declaration_node, multi_unit_container_node
    use ast_nodes_misc, only: implicit_statement_node, use_statement_node, &
        statement_function_node, namelist_statement_node, interface_block_node
    use ast_nodes_transfer, only: entry_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
        subroutine_call_node
    use intrinsic_registry, only: is_intrinsic_function, is_intrinsic_subroutine
    use semantic_explicit_interface_checker, only: is_part_reference, &
        interface_declares_subroutine
    use semantic_external_declaration_names, only: collect_declared_procedures
    use frontend_compiler_resolution, only: declaration_binding_t, &
        find_enclosing_scope, find_host_scope, get_scope_statement_indices, &
        is_scope_node, resolve_name_at_node
    use string_utils_mod, only: to_lower
    use ast_nodes_io, only: print_statement_node, read_statement_node
    use type_system_unified, only: poly_type_t, mono_type_t, &
        create_poly_type, empty_type_vars, type_var_t
    use semantic_declaration_utils, only: fetch_declaration_type
    use scope_manager, only: scope_stack_t
    use error_handling, only: create_error_result, ERROR_SEMANTIC, result_t, &
        error_collection_t
    use semantic_input_mode, only: INPUT_MODE_STANDARD
    implicit none
    private

    public :: check_undefined_variables_generic
    public :: check_external_implicit_none
    public :: check_implicit_none_references

    ! Upper bound on explicitly declared procedure names tracked per scoping
    ! unit. A unit that exceeds it is skipped so the check never guesses.
    integer, parameter :: MAX_DECLARED_NAMES = 512

contains

    ! Check references in scoping units whose effective implicit mapping is
    ! IMPLICIT NONE.  The generic lazy checker below intentionally skips
    ! standard input because ordinary IMPLICIT typing is valid there.  This
    ! structural pass is the corresponding standard-Fortran check: it uses
    ! the public resolver so USE, host, ASSOCIATE, procedure and declaration
    ! bindings are accepted without guessing from source order.
    subroutine check_implicit_none_references(arena, errors)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer :: i
        logical, allocatable :: keyword_targets(:)
        logical, allocatable :: construct_targets(:)

        allocate (keyword_targets(arena%size), source=.false.)
        allocate (construct_targets(arena%size), source=.false.)
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (assignment_node)
                if (node%is_keyword_argument .or. &
                    is_call_keyword_assignment(arena, i)) then
                    if (node%target_index > 0 .and. &
                        node%target_index <= arena%size) then
                        keyword_targets(node%target_index) = .true.
                    end if
                end if
                type is (pointer_assignment_node)
                if (node%pointer_index > 0 .and. &
                    node%pointer_index <= arena%size) then
                    if (node%target_index > 0 .and. &
                        node%target_index <= arena%size) then
                        if (is_select_type_selector(arena, i)) then
                            construct_targets(node%pointer_index) = .true.
                        end if
                    end if
                end if
            end select
        end do

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (identifier_node)
                if (allocated(node%name)) then
                    call check_reference(arena, i, node%name, .false., .false., &
                        node%line, node%column, errors)
                end if
                type is (call_or_subscript_node)
                if (allocated(node%name)) then
                    call check_reference(arena, i, node%name, .true., &
                        node%is_array_access, node%line, node%column, errors)
                end if
            end select
        end do
    contains
        subroutine check_reference(arena, reference_index, name, is_call, &
                is_array_access, line, column, errors)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: reference_index
            character(len=*), intent(in) :: name
            logical, intent(in) :: is_call, is_array_access
            integer, intent(in) :: line, column
            type(error_collection_t), intent(inout) :: errors
            type(declaration_binding_t) :: binding
            character(len=:), allocatable :: resolver_error
            integer :: scope_index

            if (len_trim(name) == 0) return
            ! Component/coindexed names are resolved through their base
            ! designator.  The component itself is not a declaration in the
            ! enclosing scope and therefore must not be diagnosed here.
            if (is_part_reference(trim(name))) return
            if (is_component_name_reference(arena, reference_index, trim(name))) return
            if (is_component_designator_reference(arena, reference_index, &
                trim(name))) return
            if (is_keyword_argument_name(reference_index)) return
            if (reference_index > 0 .and. reference_index <= &
                size(construct_targets)) then
                if (construct_targets(reference_index)) return
            end if

            scope_index = find_enclosing_scope(arena, reference_index)
            if (scope_index <= 0) return
            if (.not. effective_implicit_none(arena, scope_index)) return

            ! A function reference is checked by the function/interface
            ! validators, which know intrinsic and external procedure rules.
            ! This pass is deliberately limited to data names (identifiers
            ! and array designators); CALL statements are handled by the
            ! IMPLICIT NONE (EXTERNAL) checker below.
            if (is_call .and. .not. is_array_access) return

            if (is_construct_entity(arena, reference_index, trim(name))) return
            if (is_intrinsic_function(trim(name)) .and. &
                is_actual_argument_reference(arena, reference_index)) return
            if (is_select_type_associate_name(arena, scope_index, trim(name))) return
            if (is_use_associated_name(arena, scope_index, trim(name))) return
            if (is_namelist_name(arena, scope_index, trim(name))) return
            if (is_interface_procedure_name(arena, scope_index, trim(name))) return
            if (is_entry_procedure_name(arena, scope_index, trim(name))) return

            call resolve_name_at_node(arena, reference_index, trim(name), &
                binding, resolver_error)
            if (binding%found) return

            call errors%add_result(create_error_result( &
                "Name '"//trim(name)//"' is not declared under IMPLICIT NONE", &
                ERROR_SEMANTIC, &
                component="semantic_undefined_variable_checker", &
                context="check_implicit_none_references", &
                suggestion="Declare the name before using it, or provide an "// &
                "explicit interface for the procedure", &
                line=line, column=column, end_line=line, end_column=column + 1))
        end subroutine check_reference

        ! Resolve the effective implicit policy by walking from a nested
        ! ASSOCIATE/BLOCK scope through its host scopes.  An explicit IMPLICIT
        ! statement in the nearest scoping unit overrides the host policy.
        logical function effective_implicit_none(arena, scope_index) result(active)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: scope_index
            integer :: current
            logical :: has_statement, is_none

            active = .false.
            current = scope_index
            do while (current > 0)
                if (.not. is_scope_node(arena, current)) exit
                call implicit_policy(arena, current, has_statement, is_none)
                if (has_statement) then
                    active = is_none
                    return
                end if
                current = find_host_scope(arena, current)
            end do
        end function effective_implicit_none

        subroutine implicit_policy(arena, scope_index, has_statement, is_none)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: scope_index
            logical, intent(out) :: has_statement, is_none
            integer, allocatable :: indices(:)
            integer :: i

            has_statement = .false.
            is_none = .false.
            call get_scope_statement_indices(arena, scope_index, indices)
            do i = 1, size(indices)
                if (.not. arena%has_node_at(indices(i))) cycle
                select type (stmt => arena%entries(indices(i))%node)
                    type is (implicit_statement_node)
                    has_statement = .true.
                    is_none = stmt%is_none
                    return
                end select
            end do
        end subroutine implicit_policy

        ! Names introduced by constructs whose bindings are represented as
        ! strings rather than identifier declaration nodes.  The public
        ! resolver already handles ASSOCIATE names; these cases are the
        ! remaining construct entities that can otherwise look undeclared.
        logical function is_construct_entity(arena, reference_index, name) &
                result(found)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: reference_index
            character(len=*), intent(in) :: name
            integer :: current, i

            found = .false.
            current = reference_index
            do while (current > 0)
                current = arena%entries(current)%parent_index
                if (current <= 0) exit
                if (.not. arena%has_node_at(current)) exit
                select type (parent => arena%entries(current)%node)
                    type is (do_loop_node)
                    if (allocated(parent%var_name)) then
                        if (same_name(parent%var_name, name)) then
                            found = .true.
                            return
                        end if
                    end if
                    type is (forall_node)
                    if (allocated(parent%index_names)) then
                        do i = 1, size(parent%index_names)
                            if (same_name(parent%index_names(i), name)) then
                                found = .true.
                                return
                            end if
                        end do
                    end if
                    type is (statement_function_node)
                    if (allocated(parent%arg_names)) then
                        do i = 1, size(parent%arg_names)
                            if (same_name(parent%arg_names(i), name)) then
                                found = .true.
                                return
                            end if
                        end do
                    end if
                    type is (type_guard_block_node)
                    if (parent%type_name_index == reference_index) then
                        found = .true.
                        return
                    end if
                end select
            end do
        end function is_construct_entity

        ! Component names are stored on COMPONENT_ACCESS nodes, but some
        ! parser paths also retain an identifier child for the spelling.
        ! That child is not a declaration in the enclosing scope.
        logical function is_component_name_reference(arena, reference_index, &
                name) result(found)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: reference_index
            character(len=*), intent(in) :: name
            integer :: current

            found = .false.
            current = reference_index
            do while (current > 0)
                current = arena%entries(current)%parent_index
                if (current <= 0) exit
                if (.not. arena%has_node_at(current)) exit
                select type (parent => arena%entries(current)%node)
                    type is (component_access_node)
                    if (allocated(parent%component_name)) then
                        if (same_name(parent%component_name, name)) then
                            found = .true.
                            return
                        end if
                    end if
                end select
            end do
        end function is_component_name_reference

        ! Array element references to a component retain the component access
        ! as the call/subscript base (for example V%VALUES(I)).
        logical function is_component_designator_reference(arena, &
                reference_index, name) result(found)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: reference_index
            character(len=*), intent(in) :: name
            integer :: base_index

            found = .false.
            if (.not. arena%has_node_at(reference_index)) return
            select type (reference => arena%entries(reference_index)%node)
                type is (call_or_subscript_node)
                base_index = reference%base_expr_index
                if (base_index <= 0) return
                if (.not. arena%has_node_at(base_index)) return
                select type (base => arena%entries(base_index)%node)
                    type is (component_access_node)
                    if (allocated(base%component_name)) then
                        found = same_name(base%component_name, name)
                    end if
                end select
            end select
        end function is_component_designator_reference

        ! A keyword argument is represented as an assignment node. Its left
        ! hand side is a label, not a variable reference (e.g. KIND=4).
        logical function is_keyword_argument_name(reference_index) result(found)
            integer, intent(in) :: reference_index

            found = .false.
            if (reference_index > 0 .and. reference_index <= size(keyword_targets)) then
                found = keyword_targets(reference_index)
            end if
        end function is_keyword_argument_name

        logical function is_select_type_selector(arena, assignment_index) &
                result(found)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: assignment_index
            integer :: parent_index

            found = .false.
            parent_index = arena%entries(assignment_index)%parent_index
            if (parent_index <= 0) return
            if (.not. arena%has_node_at(parent_index)) return
            select type (parent => arena%entries(parent_index)%node)
                type is (select_type_node)
                found = .true.
            end select
        end function is_select_type_selector

        logical function is_select_type_associate_name(arena, scope_index, &
                name) result(found)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: scope_index
            character(len=*), intent(in) :: name
            integer :: i, pointer_index, current

            found = .false.
            do i = 1, arena%size
                if (.not. arena%has_node_at(i)) cycle
                select type (selector => arena%entries(i)%node)
                    type is (pointer_assignment_node)
                    if (.not. is_select_type_selector(arena, i)) cycle
                    pointer_index = selector%pointer_index
                    if (pointer_index <= 0 .or. &
                        pointer_index > arena%size) cycle
                    if (.not. arena%has_node_at(pointer_index)) cycle
                    select type (pointer => arena%entries(pointer_index)%node)
                        type is (identifier_node)
                        if (.not. allocated(pointer%name)) cycle
                        if (.not. same_name(pointer%name, name)) cycle
                        current = find_enclosing_scope(arena, i)
                        if (current == scope_index) then
                            found = .true.
                            return
                        end if
                        if (current > 0) then
                            if (find_host_scope(arena, current) == scope_index) then
                                found = .true.
                                return
                            end if
                        end if
                    end select
                end select
            end do
        end function is_select_type_associate_name

        ! Function-expression parsers do not always set the assignment flag
        ! used by CALL parsing. An assignment directly below a call node is
        ! necessarily a keyword actual (NAME=VALUE), so classify it here.
        logical function is_call_keyword_assignment(arena, assignment_index) &
                result(found)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: assignment_index
            integer :: parent_index

            found = .false.
            parent_index = arena%entries(assignment_index)%parent_index
            if (parent_index <= 0) return
            if (.not. arena%has_node_at(parent_index)) return
            select type (parent => arena%entries(parent_index)%node)
                type is (call_or_subscript_node)
                found = .true.
                type is (subroutine_call_node)
                found = .true.
            end select
        end function is_call_keyword_assignment

        ! A USE of an external module cannot be resolved without loading that
        ! module's AST.  Preserve the standard contract by accepting names
        ! explicitly listed by USE ... ONLY and all names from an unrestricted
        ! USE; same-arena module exports are still checked by the resolver.
        logical function is_use_associated_name(arena, scope_index, name) &
                result(found)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: scope_index
            character(len=*), intent(in) :: name
            integer :: current, i, j
            integer, allocatable :: indices(:)

            found = .false.
            current = scope_index
            do while (current > 0)
                if (.not. is_scope_node(arena, current)) exit
                call get_scope_statement_indices(arena, current, indices)
                do i = 1, size(indices)
                    if (.not. arena%has_node_at(indices(i))) cycle
                    select type (stmt => arena%entries(indices(i))%node)
                        type is (use_statement_node)
                        if (.not. stmt%has_only) then
                            found = .true.
                            return
                        end if
                        if (allocated(stmt%only_list)) then
                            do j = 1, size(stmt%only_list)
                                if (same_name(stmt%only_list(j)%s, name)) then
                                    found = .true.
                                    return
                                end if
                            end do
                        end if
                        if (allocated(stmt%rename_list)) then
                            do j = 1, size(stmt%rename_list), 2
                                if (same_name(stmt%rename_list(j)%s, name)) then
                                    found = .true.
                                    return
                                end if
                            end do
                        end if
                    end select
                end do
                current = find_host_scope(arena, current)
            end do
        end function is_use_associated_name

        ! NAMELIST group names are namespace labels, not variables. The
        ! group may appear later as NML=GROUP in READ/WRITE control lists.
        logical function is_namelist_name(arena, scope_index, name) result(found)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: scope_index
            character(len=*), intent(in) :: name
            integer :: current, i, j
            integer, allocatable :: indices(:)

            found = .false.
            current = scope_index
            do while (current > 0)
                if (.not. is_scope_node(arena, current)) exit
                call get_scope_statement_indices(arena, current, indices)
                do i = 1, size(indices)
                    if (.not. arena%has_node_at(indices(i))) cycle
                    select type (stmt => arena%entries(indices(i))%node)
                        type is (namelist_statement_node)
                        if (allocated(stmt%group_name)) then
                            if (same_name(stmt%group_name, name)) then
                                found = .true.
                                return
                            end if
                        end if
                        if (allocated(stmt%variable_names)) then
                            do j = 1, size(stmt%variable_names)
                                if (same_name(stmt%variable_names(j)%s, name)) then
                                    found = .true.
                                    return
                                end if
                            end do
                        end if
                    end select
                end do
                current = find_host_scope(arena, current)
            end do
        end function is_namelist_name

        ! Specific procedure names declared inside an explicit interface body
        ! are procedure names in the enclosing scoping unit.  They are not
        ! data objects subject to implicit typing (Fortran 2018 19.6.7).
        logical function is_interface_procedure_name(arena, scope_index, name) &
                result(found)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: scope_index
            character(len=*), intent(in) :: name
            integer :: current, i
            integer, allocatable :: indices(:)

            found = .false.
            current = scope_index
            do while (current > 0)
                if (.not. is_scope_node(arena, current)) exit
                call get_scope_statement_indices(arena, current, indices)
                do i = 1, size(indices)
                    if (.not. arena%has_node_at(indices(i))) cycle
                    select type (block => arena%entries(indices(i))%node)
                        type is (interface_block_node)
                        if (allocated(block%name)) then
                            if (same_name(block%name, name)) then
                                found = .true.
                                return
                            end if
                        end if
                        if (interface_declares_subroutine(arena, block, name)) then
                            found = .true.
                            return
                        end if
                    end select
                end do
                current = find_host_scope(arena, current)
            end do
        end function is_interface_procedure_name

        ! ENTRY introduces another procedure name in the enclosing procedure
        ! body.  It must not be diagnosed as an undeclared implicitly typed
        ! variable; the result/assignment validator owns its legality.
        logical function is_entry_procedure_name(arena, scope_index, name) &
                result(found)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: scope_index
            character(len=*), intent(in) :: name
            integer :: i
            integer, allocatable :: indices(:)

            found = .false.
            call get_scope_statement_indices(arena, scope_index, indices)
            do i = 1, size(indices)
                if (.not. arena%has_node_at(indices(i))) cycle
                select type (entry => arena%entries(indices(i))%node)
                    type is (entry_node)
                    if (allocated(entry%name)) then
                        if (same_name(entry%name, name)) then
                            found = .true.
                            return
                        end if
                    end if
                end select
            end do
        end function is_entry_procedure_name

        logical function is_actual_argument_reference(arena, reference_index) &
                result(found)
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: reference_index
            integer :: current, i

            found = .false.
            current = reference_index
            do while (current > 0)
                current = arena%entries(current)%parent_index
                if (current <= 0) exit
                if (.not. arena%has_node_at(current)) exit
                select type (parent => arena%entries(current)%node)
                    type is (call_or_subscript_node)
                    if (.not. allocated(parent%arg_indices)) cycle
                    do i = 1, size(parent%arg_indices)
                        if (parent%arg_indices(i) == reference_index) then
                            found = .true.
                            return
                        end if
                    end do
                    type is (subroutine_call_node)
                    if (.not. allocated(parent%arg_indices)) cycle
                    do i = 1, size(parent%arg_indices)
                        if (parent%arg_indices(i) == reference_index) then
                            found = .true.
                            return
                        end if
                    end do
                end select
            end do
        end function is_actual_argument_reference

        logical function same_name(left, right) result(equal)
            character(len=*), intent(in) :: left, right
            equal = to_lower(trim(left)) == to_lower(trim(right))
        end function same_name
    end subroutine check_implicit_none_references

    ! Reject a procedure reference that IMPLICIT NONE (EXTERNAL) requires to
    ! be explicitly declared (Fortran 2018 8.7: with an EXTERNAL implicit
    ! none specifier, a procedure referenced in the scoping unit shall have
    ! an explicit interface or be explicitly declared to have the EXTERNAL
    ! attribute). Only CALL statements are judged, because only they are
    ! unambiguously procedure references without resolved types.
    subroutine check_external_implicit_none(arena, errors)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (unit => arena%entries(i)%node)
                type is (program_node)
                call check_external_unit(arena, unit%body_indices, errors)
                type is (function_def_node)
                call check_external_unit(arena, unit%body_indices, errors)
                type is (subroutine_def_node)
                call check_external_unit(arena, unit%body_indices, errors)
            end select
        end do
    end subroutine check_external_implicit_none

    subroutine check_external_unit(arena, body_indices, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        type(error_collection_t), intent(inout) :: errors
        character(len=64) :: declared(MAX_DECLARED_NAMES)
        integer :: declared_count
        logical :: usable

        if (.not. allocated(body_indices)) return
        if (.not. has_external_implicit_none(arena, body_indices)) return

        call collect_declared_procedures(arena, body_indices, declared, &
            declared_count, usable)
        ! A USE without an ONLY list can supply any name, so the unit is
        ! left alone rather than risking a false rejection.
        if (.not. usable) return

        call report_undeclared_calls(arena, body_indices, declared, &
            declared_count, errors)
    end subroutine check_external_unit

    ! True when the unit's specification part carries IMPLICIT NONE with an
    ! EXTERNAL specifier, e.g. "implicit none (type, external)".
    function has_external_implicit_none(arena, body_indices) result(has_spec)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        logical :: has_spec
        integer :: i

        has_spec = .false.
        if (.not. allocated(body_indices)) return
        do i = 1, size(body_indices)
            if (body_indices(i) <= 0) cycle
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (stmt => arena%entries(body_indices(i))%node)
                type is (implicit_statement_node)
                if (.not. stmt%is_none) cycle
                if (.not. allocated(stmt%none_spec)) cycle
                if (index(to_lower(stmt%none_spec), 'external') > 0) has_spec = .true.
            end select
        end do
    end function has_external_implicit_none

    ! Walk the unit's executable statements and report every CALL naming a
    ! procedure that is neither explicitly declared nor an intrinsic
    ! subroutine. Contained procedures and interface bodies are not entered;
    ! they are separate scoping units.
    subroutine report_undeclared_calls(arena, body_indices, declared, &
            declared_count, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=64), intent(in) :: declared(:)
        integer, intent(in) :: declared_count
        type(error_collection_t), intent(inout) :: errors
        integer, allocatable :: pending(:)
        integer :: count, current, i

        allocate (pending(64))
        count = 0
        do i = 1, size(body_indices)
            call enqueue(body_indices(i))
        end do

        do while (count > 0)
            current = pending(count)
            count = count - 1
            if (.not. arena%has_node_at(current)) cycle
            select type (stmt => arena%entries(current)%node)
                type is (subroutine_call_node)
                call report_call(stmt)
                type is (if_node)
                call enqueue_all(stmt%then_body_indices)
                call enqueue_all(stmt%else_body_indices)
                type is (do_loop_node)
                call enqueue_all(stmt%body_indices)
                type is (do_while_node)
                call enqueue_all(stmt%body_indices)
                type is (associate_node)
                call enqueue_all(stmt%body_indices)
                type is (block_construct_node)
                call enqueue_all(stmt%body_indices)
                type is (select_case_node)
                call enqueue_all(stmt%case_indices)
                call enqueue(stmt%default_index)
                type is (case_block_node)
                call enqueue_all(stmt%body_indices)
                type is (case_default_node)
                call enqueue_all(stmt%body_indices)
            end select
        end do

    contains

        subroutine enqueue(idx)
            integer, intent(in) :: idx
            integer, allocatable :: grown(:)

            if (idx <= 0) return
            if (count >= size(pending)) then
                allocate (grown(2*size(pending)))
                grown(1:count) = pending(1:count)
                call move_alloc(grown, pending)
            end if
            count = count + 1
            pending(count) = idx
        end subroutine enqueue

        subroutine enqueue_all(indices)
            integer, allocatable, intent(in) :: indices(:)
            integer :: k

            if (.not. allocated(indices)) return
            do k = 1, size(indices)
                call enqueue(indices(k))
            end do
        end subroutine enqueue_all

        subroutine report_call(stmt)
            type(subroutine_call_node), intent(in) :: stmt
            character(len=:), allocatable :: lowered
            integer :: k

            if (.not. allocated(stmt%name)) return
            if (len_trim(stmt%name) == 0) return
            lowered = to_lower(trim(stmt%name))
            ! Type-bound and coindexed calls name a binding of a declared
            ! type, not an external procedure, so IMPLICIT NONE (EXTERNAL)
            ! never requires a separate declaration for them.
            if (is_part_reference(lowered)) return
            do k = 1, declared_count
                if (trim(declared(k)) == lowered) return
            end do
            if (is_intrinsic_subroutine(lowered)) return

            call errors%add_result(create_error_result( &
                "Procedure '"//trim(stmt%name)//"' is not explicitly "// &
                "declared under IMPLICIT NONE (EXTERNAL)", &
                ERROR_SEMANTIC, &
                component="semantic_undefined_variable_checker", &
                context="check_external_implicit_none", &
                suggestion="declare an explicit interface for the procedure "// &
                "or give it the EXTERNAL attribute", &
                line=stmt%line, column=stmt%column, end_line=stmt%line, &
                end_column=stmt%column + 1))
        end subroutine report_call

    end subroutine report_undeclared_calls

    ! Generic implementation that works with any context type
    subroutine check_undefined_variables_generic(scopes, errors, input_mode, arena, &
            prog_index)
        type(scope_stack_t), intent(inout) :: scopes
        type(error_collection_t), intent(inout) :: errors
        integer, intent(in) :: input_mode
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index

        ! Only perform undefined variable checking in lazy mode (for now)
        ! Standard mode files have explicit declarations and use statements
        ! which the checker doesn't fully understand yet
        if (input_mode == INPUT_MODE_STANDARD) return

        ! Recursively traverse the AST to find identifier nodes and check if they are
        ! defined.
        call traverse_for_undefined_variables(scopes, errors, arena, prog_index)
    end subroutine check_undefined_variables_generic

    ! Recursive helper to traverse AST and detect undefined variables
    subroutine traverse_for_undefined_variables(scopes, errors, arena, node_index)
        type(scope_stack_t), intent(inout) :: scopes
        type(error_collection_t), intent(inout) :: errors
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index

        type node_stack_entry
        integer :: idx = 0
    end type node_stack_entry

    type(node_stack_entry), allocatable :: stack(:)
    integer :: top, capacity
    type(poly_type_t), allocatable :: scheme
    type(result_t) :: error_result
    integer :: current_index, i

    capacity = 64
    allocate (stack(capacity))
    top = 0

    call push(node_index)

    do while (top > 0)
        current_index = pop()
        if (.not. arena%has_node_at(current_index)) cycle

        select type (node => arena%entries(current_index)%node)
            type is (identifier_node)
            if (.not. allocated(node%name) .or. len_trim(node%name) == 0) cycle
            call scopes%lookup(node%name, scheme)
            if (.not. allocated(scheme)) then
                if (is_declared_in_arena(arena, node%name)) then
                    call define_from_arena(scopes, arena, node%name)
                    cycle
                end if
                error_result = create_error_result( &
                    "Undefined variable '"//node%name//"'", &
                    ERROR_SEMANTIC, &
                    component="semantic_analyzer", &
                    context="check_undefined_variables", &
                    suggestion="Declare the variable before using it"// &
                    " or remove 'implicit none'", &
                    line=node%line, column=node%column, end_line=node%line, &
                    end_column=node%column + 1 &
                    )
                call errors%add_result(error_result)
            end if

            type is (program_node)
            if (allocated(node%body_indices)) then
                do i = size(node%body_indices), 1, -1
                    call push(node%body_indices(i))
                end do
            end if

            type is (multi_unit_container_node)
            if (allocated(node%body_indices)) then
                do i = size(node%body_indices), 1, -1
                    call push(node%body_indices(i))
                end do
            end if

            type is (binary_op_node)
            call push(node%right_index)
            call push(node%left_index)

            type is (assignment_node)
            call push(node%value_index)
            call push(node%target_index)

            type is (call_or_subscript_node)
            if (allocated(node%arg_indices)) then
                do i = size(node%arg_indices), 1, -1
                    call push(node%arg_indices(i))
                end do
            end if

            type is (array_literal_node)
            if (allocated(node%element_indices)) then
                do i = size(node%element_indices), 1, -1
                    call push(node%element_indices(i))
                end do
            end if

            type is (if_node)
            if (allocated(node%else_body_indices)) then
                do i = size(node%else_body_indices), 1, -1
                    call push(node%else_body_indices(i))
                end do
            end if
            if (allocated(node%then_body_indices)) then
                do i = size(node%then_body_indices), 1, -1
                    call push(node%then_body_indices(i))
                end do
            end if
            call push(node%condition_index)

            type is (print_statement_node)
            if (allocated(node%expression_indices)) then
                do i = size(node%expression_indices), 1, -1
                    call push(node%expression_indices(i))
                end do
            end if

            type is (read_statement_node)
            if (allocated(node%var_indices)) then
                do i = size(node%var_indices), 1, -1
                    call push(node%var_indices(i))
                end do
            end if

        class default
            cycle
        end select
    end do

contains

    subroutine push(idx)
        integer, intent(in) :: idx
        type(node_stack_entry), allocatable :: temp(:)
        if (idx <= 0) return
        if (top >= capacity) then
            allocate (temp(capacity * 2))
            if (capacity > 0) temp(1:capacity) = stack(1:capacity)
            call move_alloc(temp, stack)
            capacity = size(stack)
        end if
        top = top + 1
        stack(top)%idx = idx
    end subroutine push

    integer function pop()
        if (top <= 0) then
            pop = 0
        else
            pop = stack(top)%idx
            top = top - 1
        end if
    end function pop

end subroutine traverse_for_undefined_variables

! Helper: check if a name is declared by any declaration_node in the arena
logical function is_declared_in_arena(arena, name) result(found)
    type(ast_arena_t), intent(inout) :: arena
    character(len=*), intent(in) :: name
    type(mono_type_t) :: decl_type

    found = fetch_declaration_type(arena, name, decl_type)
end function is_declared_in_arena

! Helper: define a symbol in scope using declaration type from arena
subroutine define_from_arena(scopes, arena, name)
    type(scope_stack_t), intent(inout) :: scopes
    type(ast_arena_t), intent(inout) :: arena
    character(len=*), intent(in) :: name
    type(mono_type_t) :: decl_type
    type(poly_type_t) :: scheme

    if (fetch_declaration_type(arena, name, decl_type)) then
        scheme = create_poly_type(forall_vars=empty_type_vars(), mono=decl_type)
        call scopes%define(name, scheme)
    end if
end subroutine define_from_arena

end module semantic_undefined_variable_checker
