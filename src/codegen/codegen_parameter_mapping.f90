module codegen_parameter_mapping
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: ast_node
    use ast_nodes_core, only: assignment_node, identifier_node, pointer_assignment_node
    use ast_nodes_data, only: parameter_declaration_node, declaration_node, &
                              intent_type_to_string
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_control, only: if_node, select_case_node, case_block_node, &
                                 case_default_node, do_loop_node, do_while_node
    use codegen_parameter_info, only: parameter_info_t
    use string_utils_mod, only: to_lower
    implicit none
    private
    public :: build_parameter_map

contains

    subroutine build_parameter_map(arena, param_indices, body_indices, &
                                   param_map, owner)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_indices(:)
        integer, intent(in) :: body_indices(:)
        type(parameter_info_t), allocatable, intent(out) :: param_map(:)
        class(ast_node), intent(in), optional :: owner
        integer :: param_count
        integer :: i
        character(len=:), allocatable :: owner_intent

        param_count = size(param_indices)
        allocate (param_map(param_count))

        call seed_parameter_map_from_params(arena, param_indices, param_map, owner)
        call merge_parameter_details_from_body(arena, body_indices, param_map)
        call detect_parameter_mutations(arena, body_indices, param_map)

        if (present(owner)) then
            do i = 1, size(param_map)
                if (len_trim(param_map(i)%intent_str) == 0) then
                    owner_intent = get_owner_intent(owner, i)
                    if (len_trim(owner_intent) > 0) then
                        param_map(i)%intent_str = trim(owner_intent)
                    end if
                end if
            end do
        end if
    end subroutine build_parameter_map

    subroutine seed_parameter_map_from_params(arena, param_indices, param_map, owner)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_indices(:)
        type(parameter_info_t), intent(inout) :: param_map(:)
        class(ast_node), intent(in), optional :: owner
        integer :: i, idx
        character(len=:), allocatable :: owner_intent

        do i = 1, size(param_indices)
            param_map(i)%name = ""
            param_map(i)%intent_str = ""
            param_map(i)%is_optional = .false.
            param_map(i)%is_target = .false.
            param_map(i)%is_mutated = .false.

            idx = param_indices(i)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle

            select type (param_node => arena%entries(idx)%node)
            type is (identifier_node)
                param_map(i)%name = param_node%name
            type is (parameter_declaration_node)
                param_map(i)%name = param_node%name
                param_map(i)%intent_str = intent_type_to_string(param_node%intent_type)
                param_map(i)%is_optional = param_node%is_optional
                param_map(i)%is_target = param_node%is_target
            end select

            if (len_trim(param_map(i)%intent_str) == 0) then
                owner_intent = get_owner_intent(owner, i)
                if (len_trim(owner_intent) > 0) then
                    param_map(i)%intent_str = trim(owner_intent)
                end if
            end if
        end do
    end subroutine seed_parameter_map_from_params

    subroutine merge_parameter_details_from_body(arena, body_indices, param_map)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        type(parameter_info_t), intent(inout) :: param_map(:)
        integer :: j, idx
        integer :: name_idx
        character(len=:), allocatable :: intent_str

        do j = 1, size(body_indices)
            idx = body_indices(j)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle

            select type (body_node => arena%entries(idx)%node)
            type is (parameter_declaration_node)
                intent_str = intent_type_to_string(body_node%intent_type)
                call update_parameter_entry(param_map, body_node%name, intent_str, &
                                            .true., body_node%is_optional, &
                                            body_node%is_target)
            type is (declaration_node)
                if (body_node%is_multi_declaration .and. &
                    allocated(body_node%var_names)) then
                    if (body_node%has_intent) then
                        intent_str = body_node%intent
                    else
                        intent_str = ""
                    end if
                    do name_idx = 1, size(body_node%var_names)
                        if (len_trim(body_node%var_names(name_idx)) == 0) cycle
                        call update_parameter_entry(param_map, &
                                                    body_node%var_names(name_idx), &
                                                    intent_str, &
                                                    body_node%has_intent, &
                                                    body_node%is_optional, &
                                                    body_node%is_target)
                    end do
                else
                    if (body_node%has_intent) then
                        intent_str = body_node%intent
                    else
                        intent_str = ""
                    end if
                    call update_parameter_entry(param_map, body_node%var_name, &
                                                intent_str, &
                                                body_node%has_intent, &
                                                body_node%is_optional, &
                                                body_node%is_target)
                end if
            end select
        end do
    end subroutine merge_parameter_details_from_body

    subroutine detect_parameter_mutations(arena, body_indices, param_map)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        type(parameter_info_t), intent(inout) :: param_map(:)
        integer :: j

        if (size(param_map) == 0) return

        do j = 1, size(body_indices)
            call traverse_mutation_nodes(arena, body_indices(j), param_map)
        end do
    end subroutine detect_parameter_mutations

    recursive subroutine traverse_mutation_nodes(arena, node_index, param_map)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(parameter_info_t), intent(inout) :: param_map(:)
        integer :: i

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (assignment_node)
            call mark_target_mutation(arena, node%target_index, param_map)
        type is (pointer_assignment_node)
            call mark_target_mutation(arena, node%target_index, param_map)
        type is (function_def_node)
            if (allocated(node%body_indices)) then
                call traverse_index_array(arena, node%body_indices, param_map)
            end if
        type is (subroutine_def_node)
            if (allocated(node%body_indices)) then
                call traverse_index_array(arena, node%body_indices, param_map)
            end if
        type is (if_node)
            call traverse_mutation_nodes(arena, node%condition_index, param_map)
            if (allocated(node%then_body_indices)) then
                call traverse_index_array(arena, node%then_body_indices, param_map)
            end if
            if (allocated(node%elseif_blocks)) then
                do i = 1, size(node%elseif_blocks)
                    call traverse_mutation_nodes(arena, &
                         node%elseif_blocks(i)%condition_index, param_map)
                    if (allocated(node%elseif_blocks(i)%body_indices)) then
                        call traverse_index_array(arena, &
                             node%elseif_blocks(i)%body_indices, param_map)
                    end if
                end do
            end if
            if (allocated(node%else_body_indices)) then
                call traverse_index_array(arena, node%else_body_indices, param_map)
            end if
        type is (do_loop_node)
            call traverse_mutation_nodes(arena, node%start_expr_index, param_map)
            call traverse_mutation_nodes(arena, node%end_expr_index, param_map)
            call traverse_mutation_nodes(arena, node%step_expr_index, param_map)
            if (allocated(node%body_indices)) then
                call traverse_index_array(arena, node%body_indices, param_map)
            end if
        type is (do_while_node)
            call traverse_mutation_nodes(arena, node%condition_index, param_map)
            if (allocated(node%body_indices)) then
                call traverse_index_array(arena, node%body_indices, param_map)
            end if
        type is (select_case_node)
            call traverse_mutation_nodes(arena, node%selector_index, param_map)
            if (allocated(node%case_indices)) then
                call traverse_index_array(arena, node%case_indices, param_map)
            end if
            call traverse_mutation_nodes(arena, node%default_index, param_map)
        type is (case_block_node)
            if (allocated(node%body_indices)) then
                call traverse_index_array(arena, node%body_indices, param_map)
            end if
        type is (case_default_node)
            if (allocated(node%body_indices)) then
                call traverse_index_array(arena, node%body_indices, param_map)
            end if
        class default
            ! No additional traversal required
        end select
    end subroutine traverse_mutation_nodes

    recursive subroutine traverse_index_array(arena, indices, param_map)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: indices(:)
        type(parameter_info_t), intent(inout) :: param_map(:)
        integer :: k

        if (.not. allocated(indices)) return
        do k = 1, size(indices)
            call traverse_mutation_nodes(arena, indices(k), param_map)
        end do
    end subroutine traverse_index_array

    subroutine mark_target_mutation(arena, target_index, param_map)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: target_index
        type(parameter_info_t), intent(inout) :: param_map(:)

        if (target_index <= 0 .or. target_index > arena%size) return
        if (.not. allocated(arena%entries(target_index)%node)) return

        select type (target => arena%entries(target_index)%node)
        type is (identifier_node)
            call set_parameter_mutated(param_map, target%name)
        end select
    end subroutine mark_target_mutation

    subroutine set_parameter_mutated(param_map, name)
        type(parameter_info_t), intent(inout) :: param_map(:)
        character(len=*), intent(in) :: name
        integer :: i
        character(len=:), allocatable :: normalized_target
        character(len=:), allocatable :: normalized_param

        if (len_trim(name) == 0) return
        normalized_target = to_lower(trim(name))

        do i = 1, size(param_map)
            if (.not. allocated(param_map(i)%name)) cycle
            normalized_param = to_lower(trim(param_map(i)%name))
            if (normalized_param == normalized_target) then
                param_map(i)%is_mutated = .true.
                return
            end if
        end do
    end subroutine set_parameter_mutated

    subroutine update_parameter_entry(param_map, name, intent_value, has_intent, &
                                      is_optional, is_target)
        type(parameter_info_t), intent(inout) :: param_map(:)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: intent_value
        logical, intent(in) :: has_intent
        logical, intent(in) :: is_optional
        logical, intent(in) :: is_target
        integer :: i

        do i = 1, size(param_map)
            if (.not. allocated(param_map(i)%name)) cycle
            if (trim(param_map(i)%name) /= trim(name)) cycle

            if (has_intent .and. len_trim(intent_value) > 0) then
                param_map(i)%intent_str = trim(intent_value)
            end if
            param_map(i)%is_optional = is_optional
            param_map(i)%is_target = is_target
            return
        end do
    end subroutine update_parameter_entry

    function get_owner_intent(owner, idx) result(intent_value)
        class(ast_node), intent(in), optional :: owner
        integer, intent(in) :: idx
        character(len=:), allocatable :: intent_value

        intent_value = ""
        if (.not. present(owner)) return

        select type (owner)
        type is (function_def_node)
            if (allocated(owner%param_intents)) then
                if (idx > 0 .and. idx <= size(owner%param_intents)) then
                    intent_value = trim(owner%param_intents(idx))
                end if
            end if
        type is (subroutine_def_node)
            if (allocated(owner%param_intents)) then
                if (idx > 0 .and. idx <= size(owner%param_intents)) then
                    intent_value = trim(owner%param_intents(idx))
                end if
            end if
        class default
            ! No additional intent metadata available
        end select
    end function get_owner_intent

end module codegen_parameter_mapping
