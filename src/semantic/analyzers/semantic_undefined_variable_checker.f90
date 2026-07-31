module semantic_undefined_variable_checker
    ! Undefined variable detection and checking utilities
    ! Extracted from semantic_analyzer.f90 for architectural compliance (Issue #1067)
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, binary_op_node, assignment_node, &
        call_or_subscript_node, array_literal_node, program_node
    use ast_nodes_conditional, only: if_node, select_case_node, case_block_node
    use ast_nodes_control, only: do_loop_node, do_while_node, case_default_node
    use ast_nodes_associate, only: associate_node, block_construct_node
    use ast_nodes_data, only: declaration_node, multi_unit_container_node
    use ast_nodes_misc, only: implicit_statement_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
        subroutine_call_node
    use intrinsic_registry, only: is_intrinsic_subroutine
    use semantic_external_declaration_names, only: collect_declared_procedures
    use string_utils_mod, only: to_lower
    use ast_nodes_io, only: print_statement_node, read_statement_node
    use type_system_unified, only: poly_type_t, mono_type_t, &
        create_poly_type, type_var_t
    use semantic_declaration_utils, only: fetch_declaration_type
    use scope_manager, only: scope_stack_t
    use error_handling, only: create_error_result, ERROR_SEMANTIC, result_t, &
        error_collection_t
    use semantic_input_mode, only: INPUT_MODE_STANDARD
    implicit none
    private

    public :: check_undefined_variables_generic
    public :: check_external_implicit_none

    ! Upper bound on explicitly declared procedure names tracked per scoping
    ! unit. A unit that exceeds it is skipped so the check never guesses.
    integer, parameter :: MAX_DECLARED_NAMES = 512

contains

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
        scheme = create_poly_type(forall_vars=[type_var_t ::], mono=decl_type)
        call scopes%define(name, scheme)
    end if
end subroutine define_from_arena

end module semantic_undefined_variable_checker
