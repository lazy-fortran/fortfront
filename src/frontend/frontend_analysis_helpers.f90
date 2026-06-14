module frontend_analysis_helpers
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node, identifier_node, &
                              call_or_subscript_node, assignment_node, &
                              component_access_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node, &
                                   subroutine_call_node
    use ast_nodes_data, only: module_node, submodule_node
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: build_procedure_membership
    public :: analyze_ast_content
    public :: analyze_single_unit
    public :: has_existing_module_in_ast
    public :: requires_lazy_internalization
    public :: collect_host_assignment_names
    public :: collect_program_assignment_names
    public :: collect_procedure_assignment_names
    public :: collect_assignment_from_node
    public :: record_identifier_name
    public :: append_unique_name

contains

    recursive subroutine mark_procedure_subtree(arena, node_index, membership)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        logical, intent(inout) :: membership(:)
        integer :: child_idx, child_count, member_idx

        if (node_index <= 0) return
        if (node_index > size(membership)) return
        if (membership(node_index)) return

        membership(node_index) = .true.

        child_count = valid_child_count(arena, node_index)
        if (child_count == 0) return
        do child_idx = 1, child_count
            member_idx = arena%entries(node_index)%child_indices(child_idx)
            call mark_procedure_subtree(arena, member_idx, membership)
        end do
    end subroutine mark_procedure_subtree

    subroutine build_procedure_membership(arena, membership)
        type(ast_arena_t), intent(in) :: arena
        logical, allocatable, intent(out) :: membership(:)
        integer :: i, j, body_idx

        if (arena%size <= 0) then
            allocate (membership(0))
            return
        end if

        allocate (membership(arena%size))
        membership = .false.

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (proc => arena%entries(i)%node)
            type is (function_def_node)
                if (allocated(proc%body_indices)) then
                    do j = 1, size(proc%body_indices)
                        body_idx = proc%body_indices(j)
                        call mark_procedure_subtree(arena, body_idx, membership)
                    end do
                end if
            type is (subroutine_def_node)
                if (allocated(proc%body_indices)) then
                    do j = 1, size(proc%body_indices)
                        body_idx = proc%body_indices(j)
                        call mark_procedure_subtree(arena, body_idx, membership)
                    end do
                end if
            end select
        end do
    end subroutine build_procedure_membership

    subroutine analyze_ast_content(arena, root_index, has_functions, &
                                   has_subroutines, has_main_code)
        use ast_nodes_io, only: print_statement_node
        use ast_nodes_control, only: if_node, do_loop_node
        use ast_nodes_procedure, only: subroutine_call_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        logical, intent(out) :: has_functions, has_subroutines, has_main_code
        integer :: i, j
        logical, allocatable :: in_procedure(:)

        has_functions = .false.
        has_subroutines = .false.
        has_main_code = .false.

        if (root_index > 0 .and. root_index <= arena%size) then
            if (allocated(arena%entries(root_index)%node)) then
                select type (root => arena%entries(root_index)%node)
                type is (module_node)
                    return
                type is (program_node)
                    if (root%name == "__MULTI_UNIT__" .and. &
                        allocated(root%body_indices)) then
                        do j = 1, size(root%body_indices)
                            call analyze_single_unit(arena, root%body_indices(j), &
                                                     has_functions, has_subroutines, &
                                                     has_main_code)
                        end do
                        return
                    end if
                end select
            end if
        end if

        call build_procedure_membership(arena, in_procedure)

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle

            select type (node => arena%entries(i)%node)
            type is (function_def_node)
                has_functions = .true.
            type is (subroutine_def_node)
                has_subroutines = .true.
            type is (assignment_node)
                if (.not. in_procedure(i)) then
                    has_main_code = .true.
                end if
            type is (print_statement_node)
                if (.not. in_procedure(i)) then
                    has_main_code = .true.
                end if
            type is (if_node)
                if (.not. in_procedure(i)) then
                    has_main_code = .true.
                end if
            type is (do_loop_node)
                if (.not. in_procedure(i)) then
                    has_main_code = .true.
                end if
            type is (subroutine_call_node)
                if (.not. in_procedure(i)) then
                    has_main_code = .true.
                end if
            end select
        end do
    end subroutine analyze_ast_content

    subroutine analyze_single_unit(arena, unit_index, has_functions, &
                                   has_subroutines, has_main_code)
        use ast_nodes_io, only: print_statement_node
        use ast_nodes_control, only: if_node, do_loop_node
        use ast_nodes_procedure, only: subroutine_call_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: unit_index
        logical, intent(inout) :: has_functions, has_subroutines, has_main_code
        integer :: i

        if (.not. arena%has_node_at(unit_index)) return

        select type (unit => arena%entries(unit_index)%node)
        type is (function_def_node)
            has_functions = .true.
        type is (subroutine_def_node)
            has_subroutines = .true.
        type is (program_node)
            if (allocated(unit%body_indices)) then
                do i = 1, size(unit%body_indices)
                    if (unit%body_indices(i) <= 0 .or. &
                        unit%body_indices(i) > arena%size) cycle
                    if (.not. allocated(arena%entries(unit%body_indices(i))%node)) &
                        cycle

                    select type (stmt => arena%entries(unit%body_indices(i))%node)
                    type is (assignment_node)
                        has_main_code = .true.
                    type is (print_statement_node)
                        has_main_code = .true.
                    type is (if_node)
                        has_main_code = .true.
                    type is (do_loop_node)
                        has_main_code = .true.
                    type is (subroutine_call_node)
                        has_main_code = .true.
                    type is (function_def_node)
                        has_functions = .true.
                    type is (subroutine_def_node)
                        has_subroutines = .true.
                    end select
                end do
            end if
        end select
    end subroutine analyze_single_unit

    function has_existing_module_in_ast(arena) result(has_module)
        type(ast_arena_t), intent(in) :: arena
        logical :: has_module
        integer :: i

        has_module = .false.

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (module_node)
                has_module = .true.
                exit
            type is (submodule_node)
                has_module = .true.
                exit
            end select
        end do
    end function has_existing_module_in_ast

    logical function requires_lazy_internalization(arena, prog_index) &
        result(needs_wrapping)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        integer :: i, idx
        character(len=64), allocatable :: host_names(:)
        character(len=64), allocatable :: proc_names(:)

        needs_wrapping = .false.
        if (.not. arena%has_node_at(prog_index)) return

        select type (root => arena%entries(prog_index)%node)
        type is (program_node)
            if (trim(root%name) /= "__MULTI_UNIT__") return
            if (.not. allocated(root%body_indices)) return

            needs_wrapping = .true.
            call collect_host_assignment_names(arena, root, host_names)
            do i = 1, size(root%body_indices)
                idx = root%body_indices(i)
                if (.not. arena%has_node_at(idx)) cycle
                select type (child => arena%entries(idx)%node)
                type is (program_node)
                    if (.not. is_implicit_program_name(child%name)) then
                        needs_wrapping = .false.
                        return
                    end if
                type is (function_def_node)
                    call collect_procedure_assignment_names(arena, idx, proc_names)
                    if (has_name_intersection(host_names, proc_names)) return
                type is (subroutine_def_node)
                    call collect_procedure_assignment_names(arena, idx, proc_names)
                    if (has_name_intersection(host_names, proc_names)) return
                class default
                    needs_wrapping = .false.
                    return
                end select
            end do
            needs_wrapping = .false.
        class default
            needs_wrapping = .false.
        end select
    end function requires_lazy_internalization

    logical function is_implicit_program_name(name) result(is_implicit)
        character(len=*), intent(in) :: name

        select case (trim(name))
        case ("main", "__IMPLICIT_MAIN__")
            is_implicit = .true.
        case default
            is_implicit = .false.
        end select
    end function is_implicit_program_name

    subroutine collect_host_assignment_names(arena, root_prog, host_names)
        type(ast_arena_t), intent(in) :: arena
        class(program_node), intent(in) :: root_prog
        character(len=64), allocatable, intent(inout) :: host_names(:)
        integer :: i, child_idx

        if (.not. allocated(root_prog%body_indices)) return

        do i = 1, size(root_prog%body_indices)
            child_idx = root_prog%body_indices(i)
            if (.not. arena%has_node_at(child_idx)) cycle
            select type (child => arena%entries(child_idx)%node)
            type is (program_node)
                if (is_implicit_program_name(child%name)) then
                    call collect_program_assignment_names(arena, child_idx, &
                                                          host_names)
                end if
            end select
        end do
    end subroutine collect_host_assignment_names

    subroutine collect_program_assignment_names(arena, prog_idx, names)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_idx
        character(len=64), allocatable, intent(inout) :: names(:)
        integer :: i, stmt_idx

        if (.not. arena%has_node_at(prog_idx)) return
        select type (prog => arena%entries(prog_idx)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do i = 1, size(prog%body_indices)
                stmt_idx = prog%body_indices(i)
                call collect_assignment_from_node(arena, stmt_idx, names, &
                                                  skip_procedures=.true.)
            end do
        end select
    end subroutine collect_program_assignment_names

    subroutine collect_procedure_assignment_names(arena, proc_idx, names)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_idx
        character(len=64), allocatable, intent(inout) :: names(:)
        integer :: i, stmt_idx

        if (.not. arena%has_node_at(proc_idx)) return

        select type (proc => arena%entries(proc_idx)%node)
        type is (function_def_node)
            if (.not. allocated(proc%body_indices)) return
            do i = 1, size(proc%body_indices)
                stmt_idx = proc%body_indices(i)
                call collect_assignment_from_node(arena, stmt_idx, names, &
                                                  skip_procedures=.false.)
            end do
        type is (subroutine_def_node)
            if (.not. allocated(proc%body_indices)) return
            do i = 1, size(proc%body_indices)
                stmt_idx = proc%body_indices(i)
                call collect_assignment_from_node(arena, stmt_idx, names, &
                                                  skip_procedures=.false.)
            end do
        end select
    end subroutine collect_procedure_assignment_names

    subroutine collect_assignment_from_node(arena, node_index, names, &
                                            skip_procedures)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=64), allocatable, intent(inout) :: names(:)
        logical, intent(in) :: skip_procedures
        logical, allocatable :: visited(:)

        call allocate_visit_mask(arena, visited)
        call collect_assignment_from_node_impl(arena, node_index, names, &
                                               skip_procedures, visited)
    end subroutine collect_assignment_from_node

    recursive subroutine collect_assignment_from_node_impl(arena, node_index, names, &
                                                           skip_procedures, visited)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=64), allocatable, intent(inout) :: names(:)
        logical, intent(in) :: skip_procedures
        logical, intent(inout) :: visited(:)
        integer :: child_i, child_target
        integer :: child_count

        if (.not. arena%has_node_at(node_index)) return
        if (node_index <= 0 .or. node_index > size(visited)) return
        if (visited(node_index)) return
        visited(node_index) = .true.

        select type (node => arena%entries(node_index)%node)
        type is (assignment_node)
            call record_identifier_name(arena, node%target_index, names)
            return
        type is (function_def_node)
            if (skip_procedures) return
        type is (subroutine_def_node)
            if (skip_procedures) return
        type is (program_node)
            if (.not. allocated(node%body_indices)) return
            do child_i = 1, size(node%body_indices)
                child_target = node%body_indices(child_i)
                call collect_assignment_from_node_impl(arena, child_target, names, &
                                                       skip_procedures, visited)
            end do
            return
        end select

        child_count = valid_child_count(arena, node_index)
        if (child_count > 0) then
            do child_i = 1, child_count
                child_target = arena%entries(node_index)%child_indices(child_i)
                call collect_assignment_from_node_impl(arena, child_target, names, &
                                                       skip_procedures, visited)
            end do
        end if
    end subroutine collect_assignment_from_node_impl

    subroutine record_identifier_name(arena, node_index, names)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=64), allocatable, intent(inout) :: names(:)
        logical, allocatable :: visited(:)

        call allocate_visit_mask(arena, visited)
        call record_identifier_name_impl(arena, node_index, names, visited)
    end subroutine record_identifier_name

    recursive subroutine record_identifier_name_impl(arena, node_index, names, &
                                                     visited)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=64), allocatable, intent(inout) :: names(:)
        logical, intent(inout) :: visited(:)

        if (.not. arena%has_node_at(node_index)) return
        if (node_index <= 0 .or. node_index > size(visited)) return
        if (visited(node_index)) return
        visited(node_index) = .true.

        select type (id => arena%entries(node_index)%node)
        type is (identifier_node)
            call append_unique_name(names, trim(to_lower(id%name)))
        type is (call_or_subscript_node)
            if (id%base_expr_index > 0) then
                call record_identifier_name_impl(arena, id%base_expr_index, names, &
                                                 visited)
            end if
        type is (component_access_node)
            if (id%base_expr_index > 0) then
                call record_identifier_name_impl(arena, id%base_expr_index, names, &
                                                 visited)
            end if
        end select
    end subroutine record_identifier_name_impl

    subroutine allocate_visit_mask(arena, visited)
        type(ast_arena_t), intent(in) :: arena
        logical, allocatable, intent(out) :: visited(:)
        integer :: count

        count = max(0, arena%size)
        allocate (visited(count))
        visited = .false.
    end subroutine allocate_visit_mask

    integer function valid_child_count(arena, node_index) result(count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        count = 0
        if (.not. arena%has_node_at(node_index)) return
        if (.not. allocated(arena%entries(node_index)%child_indices)) return

        count = arena%entries(node_index)%child_count
        if (count <= 0) then
            count = 0
            return
        end if
        count = min(count, size(arena%entries(node_index)%child_indices))
    end function valid_child_count

    subroutine append_unique_name(names, candidate)
        character(len=64), allocatable, intent(inout) :: names(:)
        character(len=*), intent(in) :: candidate
        character(len=64) :: lowered
        integer :: n

        lowered = adjustl(candidate)
        if (len_trim(lowered) == 0) return

        lowered = to_lower(lowered)

        if (.not. allocated(names)) then
            allocate (names(1))
            names(1) = lowered
            return
        end if

        do n = 1, size(names)
            if (names(n) == lowered) return
        end do

        names = [names, lowered]
    end subroutine append_unique_name

    logical function has_name_intersection(left, right) result(has_common)
        character(len=64), allocatable, intent(in) :: left(:)
        character(len=64), allocatable, intent(in) :: right(:)
        integer :: i, j

        has_common = .false.
        if (.not. allocated(left)) return
        if (.not. allocated(right)) return

        do i = 1, size(left)
            do j = 1, size(right)
                if (left(i) == right(j)) then
                    has_common = .true.
                    return
                end if
            end do
        end do
    end function has_name_intersection

end module frontend_analysis_helpers
