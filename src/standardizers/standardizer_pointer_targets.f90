module standardizer_pointer_targets
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: pointer_assignment_node, call_or_subscript_node, &
        component_access_node, range_subscript_node, &
        identifier_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: mark_pointer_targets

contains

    subroutine mark_pointer_targets(arena)
        type(ast_arena_t), intent(inout) :: arena
        integer :: i

        if (arena%size <= 0) return

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle

            select type (node => arena%entries(i)%node)
                type is (pointer_assignment_node)
                call handle_pointer_target(arena, node%target_index)
            class default
            end select
        end do
    end subroutine mark_pointer_targets

    subroutine handle_pointer_target(arena, target_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: target_index
        character(len=64), allocatable :: names(:)
        integer :: count
        integer :: i

        call init_name_buffer(names, count)
        call collect_target_names(arena, target_index, names, count)
        if (count == 0) then
            call finalize_name_buffer(names)
            return
        end if

        do i = 1, count
            call mark_name_as_target(arena, names(i))
        end do

        call finalize_name_buffer(names)
    end subroutine handle_pointer_target

    recursive subroutine collect_target_names(arena, expr_index, names, count)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: expr_index
        character(len=64), allocatable, intent(inout) :: names(:)
        integer, intent(inout) :: count

        if (expr_index <= 0) return
        if (expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return

        select type (expr => arena%entries(expr_index)%node)
            type is (identifier_node)
            call append_name(names, count, expr%name)
            type is (call_or_subscript_node)
            if (allocated(expr%name)) then
                call append_name(names, count, expr%name)
            end if
            if (expr%base_expr_index > 0) then
                call collect_target_names(arena, expr%base_expr_index, names, count)
            end if
            type is (range_subscript_node)
            call collect_target_names(arena, expr%base_expr_index, names, count)
            type is (component_access_node)
            if (allocated(expr%component_name)) then
                call append_name(names, count, expr%component_name)
            end if
            if (expr%base_expr_index > 0) then
                call collect_target_names(arena, expr%base_expr_index, names, count)
            end if
        class default
            ! No additional handling required for other node types
        end select
    end subroutine collect_target_names

    subroutine mark_name_as_target(arena, lowered_name)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: lowered_name
        integer :: i, j
        logical :: updated
        character(len=:), allocatable :: candidate

        if (len_trim(lowered_name) == 0) return

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            updated = .false.

            select type (decl => arena%entries(i)%node)
                type is (declaration_node)
                if (allocated(decl%var_name)) then
                    candidate = to_lower(trim(decl%var_name))
                    if (candidate == lowered_name) then
                        if (.not. decl%is_target) then
                            decl%is_target = .true.
                            updated = .true.
                        end if
                    end if
                end if

                if (.not. updated) then
                    if (decl%is_multi_declaration .and. &
                        allocated(decl%var_names)) then
                        do j = 1, size(decl%var_names)
                            candidate = to_lower(trim(decl%var_names(j)))
                            if (candidate == lowered_name) then
                                if (.not. decl%is_target) then
                                    decl%is_target = .true.
                                    updated = .true.
                                end if
                                exit
                            end if
                        end do
                    end if
                end if

                if (updated) arena%entries(i)%node = decl
                type is (parameter_declaration_node)
                if (allocated(decl%name)) then
                    candidate = to_lower(trim(decl%name))
                    if (candidate == lowered_name) then
                        if (.not. decl%is_target) then
                            decl%is_target = .true.
                            arena%entries(i)%node = decl
                        end if
                    end if
                end if
            class default
            end select
        end do
    end subroutine mark_name_as_target

    subroutine init_name_buffer(names, count)
        character(len=64), allocatable, intent(inout) :: names(:)
        integer, intent(out) :: count

        allocate (names(4))
        names = ""
        count = 0
    end subroutine init_name_buffer

    subroutine finalize_name_buffer(names)
        character(len=64), allocatable, intent(inout) :: names(:)

        if (allocated(names)) then
            block
                character(len=64), allocatable :: temp(:)
                call move_alloc(names, temp)
            end block
        end if
    end subroutine finalize_name_buffer

    subroutine append_name(names, count, raw_name)
        character(len=64), allocatable, intent(inout) :: names(:)
        integer, intent(inout) :: count
        character(len=*), intent(in) :: raw_name
        character(len=:), allocatable :: lowered
        character(len=64), allocatable :: temp(:)
        integer :: i, new_capacity

        lowered = to_lower(trim(adjustl(raw_name)))
        if (len_trim(lowered) == 0) return

        do i = 1, count
            if (trim(names(i)) == trimmed(lowered)) return
        end do

        if (count >= size(names)) then
            new_capacity = max(4, size(names) * 2)
            allocate (temp(new_capacity))
            temp = ""
            temp(1:size(names)) = names
            call move_alloc(temp, names)
        end if

        count = count + 1
        names(count) = trimmed(lowered)
    contains
        pure function trimmed(text) result(value)
            character(len=*), intent(in) :: text
            character(len=64) :: value
            value = ""
            value(1:len_trim(text)) = text
        end function trimmed
    end subroutine append_name

end module standardizer_pointer_targets
