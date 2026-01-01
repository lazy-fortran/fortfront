module semantic_strict_argument_type_checker_resolution
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_data, only: module_node, submodule_node
    use ast_nodes_misc, only: contains_node, use_statement_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use semantic_strict_argument_type_checker_scope_utils, only: split_rename
    use semantic_strict_argument_type_checker_scope_utils, only: find_module_node_index
    use semantic_strict_argument_type_checker_scope_utils, only: node_name_matches
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: find_function_interface_index
    public :: find_subroutine_interface_index

contains

    subroutine find_function_interface_index(arena, name, call_index, iface_index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: call_index
        integer, intent(out) :: iface_index
        character(len=:), allocatable :: lowered

        iface_index = 0
        lowered = to_lower(trim(name))
        if (len_trim(lowered) == 0) return

        call find_procedure_interface_index(arena, lowered, call_index, &
                                            want_function=.true., &
                                            iface_index=iface_index)
    end subroutine find_function_interface_index

    subroutine find_subroutine_interface_index(arena, name, call_index, iface_index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: call_index
        integer, intent(out) :: iface_index
        character(len=:), allocatable :: lowered

        iface_index = 0
        lowered = to_lower(trim(name))
        if (len_trim(lowered) == 0) return

        call find_procedure_interface_index(arena, lowered, call_index, &
                                            want_function=.false., &
                                            iface_index=iface_index)
    end subroutine find_subroutine_interface_index

    subroutine find_procedure_interface_index(arena, lowered_name, call_index, &
                                              want_function, iface_index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: lowered_name
        integer, intent(in) :: call_index
        logical, intent(in) :: want_function
        integer, intent(out) :: iface_index

        integer :: current

        iface_index = 0
        if (len_trim(lowered_name) == 0) return
        if (call_index <= 0 .or. call_index > arena%size) return

        current = call_index
        do while (current > 0 .and. current <= arena%size)
            if (allocated(arena%entries(current)%node)) then
                select type (scope_node => arena%entries(current)%node)
                type is (function_def_node)
                    call find_internal_procedure_in_body(arena, &
                                                         scope_node%body_indices, &
                                                         lowered_name, want_function, &
                                                         iface_index)
                    if (iface_index > 0) return
                    call find_used_procedure_in_body(arena, scope_node%body_indices, &
                                                     lowered_name, want_function, &
                                                     iface_index)
                    if (iface_index > 0) return
                type is (subroutine_def_node)
                    call find_internal_procedure_in_body(arena, &
                                                         scope_node%body_indices, &
                                                         lowered_name, want_function, &
                                                         iface_index)
                    if (iface_index > 0) return
                    call find_used_procedure_in_body(arena, scope_node%body_indices, &
                                                     lowered_name, want_function, &
                                                     iface_index)
                    if (iface_index > 0) return
                type is (program_node)
                    call find_internal_procedure_in_body(arena, &
                                                         scope_node%body_indices, &
                                                         lowered_name, want_function, &
                                                         iface_index)
                    if (iface_index > 0) return
                    call find_used_procedure_in_body(arena, scope_node%body_indices, &
                                                     lowered_name, want_function, &
                                                     iface_index)
                    if (iface_index > 0) return
                type is (module_node)
                    call find_module_procedure_in_indices( &
                        arena, scope_node%procedure_indices, lowered_name, &
                        want_function, iface_index)
                    if (iface_index > 0) return
                    call find_used_procedure_in_indices( &
                        arena, scope_node%declaration_indices, lowered_name, &
                        want_function, iface_index)
                    if (iface_index > 0) return
                type is (submodule_node)
                    call find_module_procedure_in_indices( &
                        arena, scope_node%procedure_indices, lowered_name, &
                        want_function, iface_index)
                    if (iface_index > 0) return
                    call find_used_procedure_in_indices( &
                        arena, scope_node%declaration_indices, lowered_name, &
                        want_function, iface_index)
                    if (iface_index > 0) return
                class default
                    continue
                end select
            end if
            current = arena%entries(current)%parent_index
        end do
    end subroutine find_procedure_interface_index

    subroutine find_internal_procedure_in_body(arena, body_indices, lowered_name, &
                                               want_function, iface_index)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: lowered_name
        logical, intent(in) :: want_function
        integer, intent(out) :: iface_index

        integer :: i
        integer :: node_index
        logical :: in_contains

        iface_index = 0
        if (len_trim(lowered_name) == 0) return
        if (.not. allocated(body_indices)) return
        if (size(body_indices) == 0) return

        in_contains = .false.
        do i = 1, size(body_indices)
            node_index = body_indices(i)
            if (node_index <= 0 .or. node_index > arena%size) cycle
            if (.not. allocated(arena%entries(node_index)%node)) cycle

            select type (node => arena%entries(node_index)%node)
            type is (contains_node)
                in_contains = .true.
            type is (function_def_node)
                if (.not. in_contains) cycle
                if (.not. want_function) cycle
                if (node_name_matches(node%name, lowered_name)) then
                    iface_index = node_index
                    return
                end if
            type is (subroutine_def_node)
                if (.not. in_contains) cycle
                if (want_function) cycle
                if (node_name_matches(node%name, lowered_name)) then
                    iface_index = node_index
                    return
                end if
            class default
                cycle
            end select
        end do
    end subroutine find_internal_procedure_in_body

    subroutine find_module_procedure_in_indices(arena, proc_indices, lowered_name, &
                                                want_function, iface_index)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: proc_indices(:)
        character(len=*), intent(in) :: lowered_name
        logical, intent(in) :: want_function
        integer, intent(out) :: iface_index

        integer :: i
        integer :: node_index

        iface_index = 0
        if (len_trim(lowered_name) == 0) return
        if (.not. allocated(proc_indices)) return
        if (size(proc_indices) == 0) return

        do i = 1, size(proc_indices)
            node_index = proc_indices(i)
            if (node_index <= 0 .or. node_index > arena%size) cycle
            if (.not. allocated(arena%entries(node_index)%node)) cycle

            select type (node => arena%entries(node_index)%node)
            type is (function_def_node)
                if (.not. want_function) cycle
                if (node_name_matches(node%name, lowered_name)) then
                    iface_index = node_index
                    return
                end if
            type is (subroutine_def_node)
                if (want_function) cycle
                if (node_name_matches(node%name, lowered_name)) then
                    iface_index = node_index
                    return
                end if
            class default
                cycle
            end select
        end do
    end subroutine find_module_procedure_in_indices

    subroutine find_used_procedure_in_body(arena, body_indices, lowered_name, &
                                           want_function, iface_index)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: lowered_name
        logical, intent(in) :: want_function
        integer, intent(out) :: iface_index

        integer :: i
        integer :: node_index

        iface_index = 0
        if (len_trim(lowered_name) == 0) return
        if (.not. allocated(body_indices)) return
        if (size(body_indices) == 0) return

        do i = 1, size(body_indices)
            node_index = body_indices(i)
            if (node_index <= 0 .or. node_index > arena%size) cycle
            if (.not. allocated(arena%entries(node_index)%node)) cycle

            select type (node => arena%entries(node_index)%node)
            type is (contains_node)
                return
            type is (use_statement_node)
                call resolve_use_associated_procedure(arena, node, lowered_name, &
                                                      want_function, iface_index)
                if (iface_index > 0) return
            class default
                cycle
            end select
        end do
    end subroutine find_used_procedure_in_body

    subroutine find_used_procedure_in_indices(arena, spec_indices, lowered_name, &
                                              want_function, iface_index)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: spec_indices(:)
        character(len=*), intent(in) :: lowered_name
        logical, intent(in) :: want_function
        integer, intent(out) :: iface_index

        integer :: i
        integer :: node_index

        iface_index = 0
        if (len_trim(lowered_name) == 0) return
        if (.not. allocated(spec_indices)) return
        if (size(spec_indices) == 0) return

        do i = 1, size(spec_indices)
            node_index = spec_indices(i)
            if (node_index <= 0 .or. node_index > arena%size) cycle
            if (.not. allocated(arena%entries(node_index)%node)) cycle

            select type (node => arena%entries(node_index)%node)
            type is (use_statement_node)
                call resolve_use_associated_procedure(arena, node, lowered_name, &
                                                      want_function, iface_index)
                if (iface_index > 0) return
            class default
                cycle
            end select
        end do
    end subroutine find_used_procedure_in_indices

    subroutine resolve_use_associated_procedure(arena, use_node, lowered_name, &
                                                want_function, iface_index)
        type(ast_arena_t), intent(in) :: arena
        type(use_statement_node), intent(in) :: use_node
        character(len=*), intent(in) :: lowered_name
        logical, intent(in) :: want_function
        integer, intent(out) :: iface_index

        character(len=:), allocatable :: module_lowered
        character(len=:), allocatable :: remote_lowered
        integer :: module_index

        iface_index = 0
        if (len_trim(lowered_name) == 0) return
        if (.not. allocated(use_node%module_name)) return

        call map_use_name(use_node, lowered_name, remote_lowered)
        if (.not. allocated(remote_lowered)) return
        if (len_trim(remote_lowered) == 0) return

        module_lowered = to_lower(trim(use_node%module_name))
        if (len_trim(module_lowered) == 0) return

        call find_module_node_index(arena, module_lowered, module_index)
        if (module_index <= 0) return
        if (.not. allocated(arena%entries(module_index)%node)) return

        select type (mod_node => arena%entries(module_index)%node)
        type is (module_node)
            call find_module_procedure_in_indices(arena, mod_node%procedure_indices, &
                                                  remote_lowered, want_function, &
                                                  iface_index)
        type is (submodule_node)
            call find_module_procedure_in_indices(arena, mod_node%procedure_indices, &
                                                  remote_lowered, want_function, &
                                                  iface_index)
        class default
            return
        end select
    end subroutine resolve_use_associated_procedure

    subroutine map_use_name(use_node, local_lowered, remote_lowered)
        type(use_statement_node), intent(in) :: use_node
        character(len=*), intent(in) :: local_lowered
        character(len=:), allocatable, intent(out) :: remote_lowered

        integer :: i
        character(len=:), allocatable :: mapping
        character(len=:), allocatable :: local_name
        character(len=:), allocatable :: remote_name
        logical :: renamed_away

        if (allocated(remote_lowered)) deallocate (remote_lowered)
        if (len_trim(local_lowered) == 0) return

        renamed_away = .false.
        if (allocated(use_node%rename_list)) then
            do i = 1, size(use_node%rename_list)
                if (.not. allocated(use_node%rename_list(i)%s)) cycle
                mapping = trim(use_node%rename_list(i)%s)
                call split_rename(mapping, local_name, remote_name)
                if (.not. allocated(local_name)) cycle
                if (.not. allocated(remote_name)) cycle

                if (to_lower(trim(remote_name)) == local_lowered .and. &
                    to_lower(trim(local_name)) /= local_lowered) then
                    renamed_away = .true.
                end if

                if (to_lower(trim(local_name)) == local_lowered) then
                    remote_lowered = to_lower(trim(remote_name))
                    return
                end if
            end do
        end if

        if (allocated(use_node%only_list)) then
            do i = 1, size(use_node%only_list)
                if (.not. allocated(use_node%only_list(i)%s)) cycle
                mapping = trim(use_node%only_list(i)%s)
                call split_rename(mapping, local_name, remote_name)
                if (allocated(local_name) .and. allocated(remote_name)) then
                    if (to_lower(trim(remote_name)) == local_lowered .and. &
                        to_lower(trim(local_name)) /= local_lowered) then
                        renamed_away = .true.
                    end if

                    if (to_lower(trim(local_name)) == local_lowered) then
                        remote_lowered = to_lower(trim(remote_name))
                        return
                    end if
                end if
            end do
        end if

        if (use_node%has_only) then
            if (allocated(use_node%only_list)) then
                do i = 1, size(use_node%only_list)
                    if (.not. allocated(use_node%only_list(i)%s)) cycle
                    if (to_lower(trim(use_node%only_list(i)%s)) == local_lowered) then
                        remote_lowered = local_lowered
                        return
                    end if
                end do
            end if
        else
            if (.not. renamed_away) remote_lowered = local_lowered
        end if
    end subroutine map_use_name

end module semantic_strict_argument_type_checker_resolution
