module standardizer_driver
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_data, only: module_node, multi_unit_container_node
    use ast_nodes_transfer, only: entry_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use standardizer_program, only: standardize_program
    use standardizer_module, only: standardize_module
    use standardizer_function, only: standardize_function_def
    use standardizer_subroutine, only: standardize_subroutine_def
    use standardizer_subprograms, only: wrap_function_in_program, &
                                        wrap_subroutine_in_program
    implicit none
    private

    public :: standardize_ast_iter

contains

    subroutine standardize_ast_iter(arena, root_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: root_index

        integer, allocatable :: idx_stack(:)
        logical, allocatable :: inmod_stack(:)
        logical, allocatable :: visited(:)
        integer :: top, cap
        logical :: current_inmod
        logical :: tmp_inmod
        integer :: current_index
        integer :: root_tracker
        integer :: original_index

        if (.not. arena%has_node_at(root_index)) return

        root_tracker = root_index
        allocate (visited(arena%size))
        visited = .false.
        cap = 64
        allocate (idx_stack(cap), inmod_stack(cap))
        top = 0

        call push(root_tracker, .false.)

        do while (top > 0)
            call pop(current_index, tmp_inmod)
            if (.not. arena%has_node_at(current_index)) cycle
            call ensure_visited_capacity(current_index)
            if (visited(current_index)) cycle
            visited(current_index) = .true.

            select type (node => arena%entries(current_index)%node)
            type is (multi_unit_container_node)
                if (allocated(node%body_indices)) then
                    call push_many(node%body_indices, .false.)
                end if
            type is (program_node)
                original_index = current_index
                call standardize_program(arena, node, current_index)
                if (original_index == root_tracker) root_tracker = current_index
            type is (module_node)
                call standardize_module(arena, node, current_index)
                if (allocated(node%declaration_indices)) then
                    call push_many(node%declaration_indices, .true.)
                end if
                if (allocated(node%procedure_indices)) then
                    call push_many(node%procedure_indices, .true.)
                end if
            type is (function_def_node)
                if (.not. get_in_module()) then
                    if (procedure_contains_entry(arena, current_index)) then
                        call standardize_function_def(arena, node, current_index)
                    else
                        original_index = current_index
                        call wrap_function_in_program(arena, current_index)
                        if (original_index == root_tracker) then
                            root_tracker = current_index
                        end if
                    end if
                end if
            type is (subroutine_def_node)
                if (.not. get_in_module()) then
                    if (procedure_contains_entry(arena, current_index)) then
                        call standardize_subroutine_def(arena, node, current_index)
                    else
                        original_index = current_index
                        call wrap_subroutine_in_program(arena, current_index)
                        if (original_index == root_tracker) then
                            root_tracker = current_index
                        end if
                    end if
                end if
            class default
                ! no-op
            end select
        end do

        root_index = root_tracker

    contains

        subroutine push(i, in_mod)
            integer, intent(in) :: i
            logical, intent(in) :: in_mod
            if (top >= cap) then
                call grow()
            end if
            top = top + 1
            idx_stack(top) = i
            inmod_stack(top) = in_mod
        end subroutine push

        subroutine push_many(arr, in_mod)
            integer, intent(in) :: arr(:)
            logical, intent(in) :: in_mod
            integer :: k
            do k = 1, size(arr)
                if (arr(k) > 0 .and. arr(k) <= arena%size) then
                    call push(arr(k), in_mod)
                end if
            end do
        end subroutine push_many

        subroutine pop(i, in_module)
            integer, intent(out) :: i
            logical, intent(out) :: in_module
            i = idx_stack(top)
            in_module = inmod_stack(top)
            current_inmod = inmod_stack(top)
            top = top - 1
        end subroutine pop

        logical function get_in_module()
            get_in_module = current_inmod
        end function get_in_module

        subroutine grow()
            integer, allocatable :: tmp_i(:)
            logical, allocatable :: tmp_b(:)
            allocate (tmp_i(cap * 2), tmp_b(cap * 2))
            tmp_i(1:cap) = idx_stack(1:cap)
            tmp_b(1:cap) = inmod_stack(1:cap)
            call move_alloc(tmp_i, idx_stack)
            call move_alloc(tmp_b, inmod_stack)
            cap = cap * 2
        end subroutine grow

        ! Grow the visited array when standardization pushes new arena nodes
        ! whose indices exceed the original allocation.
        subroutine ensure_visited_capacity(idx)
            integer, intent(in) :: idx
            logical, allocatable :: tmp_v(:)
            integer :: old_size, new_size

            if (idx <= size(visited)) return
            old_size = size(visited)
            new_size = max(idx, old_size * 2)
            allocate (tmp_v(new_size))
            tmp_v = .false.
            tmp_v(1:old_size) = visited(1:old_size)
            call move_alloc(tmp_v, visited)
        end subroutine ensure_visited_capacity

    end subroutine standardize_ast_iter

    logical function procedure_contains_entry(arena, proc_index) &
        result(has_entry)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_index

        has_entry = .false.
        if (proc_index <= 0) return
        if (proc_index > arena%size) return
        if (.not. allocated(arena%entries(proc_index)%node)) return

        select type (proc => arena%entries(proc_index)%node)
        type is (function_def_node)
            if (.not. allocated(proc%body_indices)) return
            has_entry = body_contains_entry(arena, proc%body_indices)
        type is (subroutine_def_node)
            if (.not. allocated(proc%body_indices)) return
            has_entry = body_contains_entry(arena, proc%body_indices)
        class default
            has_entry = .false.
        end select
    end function procedure_contains_entry

    logical function body_contains_entry(arena, body_indices) result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer :: i, idx

        found = .false.

        do i = 1, size(body_indices)
            idx = body_indices(i)
            if (idx <= 0) cycle
            if (idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (stmt => arena%entries(idx)%node)
            type is (entry_node)
                found = .true.
                return
            end select
        end do
    end function body_contains_entry

end module standardizer_driver
