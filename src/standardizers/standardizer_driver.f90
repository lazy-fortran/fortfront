module standardizer_driver
    use ast_core
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

        if (root_index <= 0 .or. root_index > arena%size) return
        if (.not. allocated(arena%entries(root_index)%node)) return

        allocate(visited(arena%size)); visited = .false.
        cap = 64
        allocate(idx_stack(cap), inmod_stack(cap))
        top = 0

        call push(root_index, .false.)

        do while (top > 0)
            call pop(root_index, tmp_inmod)
            if (root_index <= 0 .or. root_index > arena%size) cycle
            if (.not. allocated(arena%entries(root_index)%node)) cycle
            if (visited(root_index)) cycle
            visited(root_index) = .true.

            select type (node => arena%entries(root_index)%node)
            type is (program_node)
                if (node%name == "__MULTI_UNIT__") then
                    if (allocated(node%body_indices)) then
                        call push_many(node%body_indices, .false.)
                    end if
                else
                    call standardize_program(arena, node, root_index)
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
                    call wrap_function_in_program(arena, root_index)
                end if
            type is (subroutine_def_node)
                if (.not. get_in_module()) then
                    call wrap_subroutine_in_program(arena, root_index)
                end if
            class default
                ! no-op
            end select
        end do

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
