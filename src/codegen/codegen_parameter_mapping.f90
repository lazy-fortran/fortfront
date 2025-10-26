module codegen_parameter_mapping
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node
    use ast_nodes_data, only: parameter_declaration_node, declaration_node, &
                              intent_type_to_string
    use codegen_parameter_info, only: parameter_info_t
    implicit none
    private
    public :: build_parameter_map

contains

    subroutine build_parameter_map(arena, param_indices, body_indices, param_map)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_indices(:)
        integer, intent(in) :: body_indices(:)
        type(parameter_info_t), allocatable, intent(out) :: param_map(:)
        integer :: param_count

        param_count = size(param_indices)
        allocate (param_map(param_count))

        call seed_parameter_map_from_params(arena, param_indices, param_map)
        call merge_parameter_details_from_body(arena, body_indices, param_map)
    end subroutine build_parameter_map

    subroutine seed_parameter_map_from_params(arena, param_indices, param_map)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_indices(:)
        type(parameter_info_t), intent(inout) :: param_map(:)
        integer :: i, idx

        do i = 1, size(param_indices)
            param_map(i)%name = ""
            param_map(i)%intent_str = ""
            param_map(i)%is_optional = .false.
            param_map(i)%is_target = .false.

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
                    do name_idx = 1, size(body_node%var_names)
                        if (len_trim(body_node%var_names(name_idx)) == 0) cycle
                        call update_parameter_entry(param_map, &
                                                    body_node%var_names(name_idx), &
                                                    body_node%intent, &
                                                    body_node%has_intent, &
                                                    body_node%is_optional, &
                                                    body_node%is_target)
                    end do
                else
                    call update_parameter_entry(param_map, body_node%var_name, &
                                                body_node%intent, &
                                                body_node%has_intent, &
                                                body_node%is_optional, &
                                                body_node%is_target)
                end if
            end select
        end do
    end subroutine merge_parameter_details_from_body

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

            if (has_intent) param_map(i)%intent_str = intent_value
            param_map(i)%is_optional = is_optional
            param_map(i)%is_target = is_target
            return
        end do
    end subroutine update_parameter_entry
end module codegen_parameter_mapping
