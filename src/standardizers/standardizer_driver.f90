module standardizer_driver
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_data, only: module_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use standardizer_program, only: standardize_program
    use standardizer_subprograms, only: wrap_function_in_program, wrap_subroutine_in_program
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

        if (root_index <= 0 .or. root_index > arena%size) return
        if (.not. allocated(arena%entries(root_index)%node)) return

        root_tracker = root_index
        allocate(visited(arena%size))
        visited = .false.
        cap = 64
        allocate(idx_stack(cap), inmod_stack(cap))
        top = 0

        call push(root_tracker, .false.)

        do while (top > 0)
            call pop(current_index, tmp_inmod)
            if (current_index <= 0 .or. current_index > arena%size) cycle
            if (.not. allocated(arena%entries(current_index)%node)) cycle
            if (visited(current_index)) cycle
            visited(current_index) = .true.

            select type (node => arena%entries(current_index)%node)
            type is (program_node)
                if (node%name == "__MULTI_UNIT__") then
                    if (allocated(node%body_indices)) then
                        call push_many(node%body_indices, .false.)
                    end if
                else
                    original_index = current_index
                    call standardize_program(arena, node, current_index)
                    if (original_index == root_tracker) root_tracker = current_index
                end if
            type is (module_node)
                if (allocated(node%declaration_indices)) then
                    call push_many(node%declaration_indices, .true.)
                end if
                if (allocated(node%procedure_indices)) then
                    call push_many(node%procedure_indices, .true.)
                end if
            type is (function_def_node)
                if (.not. get_in_module()) then
                    original_index = current_index
                    call wrap_function_in_program(arena, current_index)
                    if (original_index == root_tracker) root_tracker = current_index
                end if
            type is (subroutine_def_node)
                if (.not. get_in_module()) then
                    original_index = current_index
                    call wrap_subroutine_in_program(arena, current_index)
                    if (original_index == root_tracker) root_tracker = current_index
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
            allocate(tmp_i(cap*2), tmp_b(cap*2))
            tmp_i(1:cap) = idx_stack(1:cap)
            tmp_b(1:cap) = inmod_stack(1:cap)
            call move_alloc(tmp_i, idx_stack)
            call move_alloc(tmp_b, inmod_stack)
            cap = cap*2
        end subroutine grow

    end subroutine standardize_ast_iter



end module standardizer_driver
