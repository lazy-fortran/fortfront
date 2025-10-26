module codegen_grouped_body_params
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: ast_node
    use ast_nodes_misc, only: contains_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_arena_utils, only: check_result_is_simple_scalar
    use codegen_character_normalization, only: normalize_character_type, &
                                               normalize_character_type_param
    use codegen_declaration_grouping, only: can_group_declarations_with_params, &
                                            generate_grouped_declaration
    use codegen_grouped_body, only: generate_grouped_body
    use codegen_import_reorder, only: reorder_import_lines
    use codegen_parameter_info, only: parameter_info_t, find_parameter_info
    use codegen_type_utils, only: get_type_standardization
    use string_utils_mod, only: int_to_string, to_lower
    use type_string_utils, only: is_character_type_string
    implicit none
    private

    logical, save :: standardize_types_enabled = .false.

    public :: generate_grouped_body_with_params

contains

    function generate_grouped_body_with_params(arena, body_indices, indent, &
                                               param_map, &
                                               proc_node) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: indent
        type(parameter_info_t), intent(in) :: param_map(:)
        class(ast_node), intent(in) :: proc_node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: indent_str
        character(len=:), allocatable :: stmt_code
        character(len=:), allocatable :: type_name
        integer :: i
        integer :: j
        integer :: param_idx
        logical :: should_skip
        integer, allocatable :: filtered_indices(:)
        integer :: filtered_count
        logical :: in_contains_section
        integer :: var_idx
        logical :: append_kind
        logical :: append_kind_single
        logical :: append_kind_param
        character(len=:), allocatable :: result_var_name
        logical :: has_return_type_in_signature
        logical :: keep_result_decl
        logical :: force_keep_result_decl
        logical :: has_header_return_type
        character(len=:), allocatable :: lowered_return
        logical :: has_dimensions

        indent_str = repeat("    ", indent)
        code = ""
        force_keep_result_decl = .false.
        has_header_return_type = .false.
        call get_type_standardization(standardize_types_enabled)

        has_return_type_in_signature = .false.
        result_var_name = ""
        select type (proc_node)
        type is (function_def_node)
            if (allocated(proc_node%result_variable)) then
                result_var_name = trim(proc_node%result_variable)
            else if (allocated(proc_node%name)) then
                result_var_name = trim(proc_node%name)
            end if
            has_header_return_type = proc_node%has_return_type_in_header
            if (has_header_return_type) then
                lowered_return = to_lower(trim(proc_node%return_type))
                if (index(lowered_return, "len=") > 0) then
                    force_keep_result_decl = .true.
                end if
                call check_result_is_simple_scalar(arena, proc_node, result_var_name, &
                                                   has_return_type_in_signature)
            end if
        end select

        if (len_trim(result_var_name) > 0 .and. .not. has_header_return_type) then
            force_keep_result_decl = .true.
        end if

        if (size(param_map) > 0) then
            do i = 1, size(body_indices)
                if (body_indices(i) <= 0 .or. body_indices(i) > arena%size) cycle
                if (.not. allocated(arena%entries(body_indices(i))%node)) cycle
                select type (node => arena%entries(body_indices(i))%node)
                type is (declaration_node)
                    if (node%is_multi_declaration .and. allocated(node%var_names)) then
                        block
                            logical :: found_params
                            logical, allocatable :: is_param(:)
                            integer :: first_param_idx

                            allocate (is_param(size(node%var_names)))
                            found_params = .false.
                            first_param_idx = 0

                            do j = 1, size(node%var_names)
                                param_idx = find_parameter_info(param_map, &
                                                                trim(node%var_names(j)))
                                is_param(j) = (param_idx > 0)
                                if (param_idx > 0) then
                                    found_params = .true.
                                    if (first_param_idx == 0) first_param_idx &
                                        = param_idx
                                end if
                            end do

                            if (found_params) then
                                type_name = trim(node%type_name)
                                if (is_character_type_string(type_name)) then
                                    type_name = &
                                        normalize_character_type(node, type_name)
                                else if (standardize_types_enabled) then
                                    if (to_lower(trim(type_name)) == "real") then
                                        if (.not. node%has_kind) type_name = "real(8)"
                                    end if
                                end if

                                append_kind = node%has_kind .and. .not. &
                                              is_character_type_string(type_name)
                                code = code // indent_str // type_name
                                if (append_kind) then
                                    code = code // "("
                                    code = code // &
                                           trim(adjustl(int_to_string(node%kind_value)))
                                    code = code // ")"
                                end if

                                if (node%has_intent) then
                                    code = code // ", intent(" // &
                                           node%intent // ")"
                                else
                                    block
                                        character(len=:), allocatable :: param_intent
                                        logical :: has_param_intent

                                        associate (entry => param_map(first_param_idx))
                                            has_param_intent = &
                                                allocated(entry%intent_str)
                                            if (has_param_intent) then
                                                param_intent = trim(entry%intent_str)
                                            else
                                                param_intent = ""
                                            end if
                                        end associate
                                        if (len_trim(param_intent) > 0) then
                                            code = code // ", intent("
                                            code = code // param_intent
                                            code = code // ")"
                                        end if
                                    end block
                                end if
                                if (node%is_optional .or. &
                                    param_map(first_param_idx)%is_optional) then
                                    code = code // ", optional"
                                end if
                                if (node%is_target .or. &
                                    param_map(first_param_idx)%is_target) then
                                    code = code // ", target"
                                end if
                                if (node%is_pointer) then
                                    code = code // ", pointer"
                                end if

                                code = code // " :: "
                                do j = 1, size(node%var_names)
                                    if (.not. is_param(j)) cycle
                                    if (j > 1) code = code // ", "
                                    code = code // trim(node%var_names(j))
                                end do
                                code = code // new_line('A')

                                block
                                    character(len=:), allocatable :: nonparam_list
                                    logical :: have_nonparam
                                    character(len=:), allocatable :: local_type
                                    logical :: local_append_kind
                                    logical :: type_is_character
                                    character(len=:), allocatable :: kind_text

                                    nonparam_list = ""
                                    have_nonparam = .false.
                                    do j = 1, size(node%var_names)
                                        if (.not. is_param(j)) then
                                            if (have_nonparam) then
                                                nonparam_list = nonparam_list // ", "
                                                nonparam_list = nonparam_list // &
                                                                trim(node%var_names(j))
                                            else
                                                nonparam_list = trim(node%var_names(j))
                                            end if
                                            have_nonparam = .true.
                                        end if
                                    end do

                                    if (have_nonparam) then
                                        local_type = trim(node%type_name)
                                        if (is_character_type_string(local_type)) then
                                            local_type = normalize_character_type( &
                                                         node, local_type)
                                        end if
                                        type_is_character = &
                                            is_character_type_string(local_type)
                                        local_append_kind = node%has_kind .and. .not. &
                                                            type_is_character
                                        if (local_type == "real" .and. .not. &
                                            local_append_kind) then
                                            local_type = "real(8)"
                                        end if
                                        code = code // indent_str // local_type
                                        if (local_append_kind) then
                                            kind_text = trim(adjustl(int_to_string( &
                                                                     node%kind_value)))
                                            code = code // "("
                                            code = code // kind_text
                                            code = code // ")"
                                        end if
                                        code = code // " :: " // nonparam_list
                                        code = code // new_line('A')
                                    end if
                                end block
                            end if

                            deallocate (is_param)
                        end block
                    else
                        param_idx = find_parameter_info(param_map, node%var_name)
                        if (param_idx > 0) then
                            type_name = trim(node%type_name)
                            if (is_character_type_string(type_name)) then
                                type_name = normalize_character_type(node, type_name)
                            else if (standardize_types_enabled .and. &
                                     to_lower(trim(type_name)) == "real" .and. &
                                     .not. node%has_kind) then
                                type_name = "real(8)"
                            end if
                            append_kind_single = node%has_kind .and. .not. &
                                                 is_character_type_string(type_name)
                            code = code // indent_str // type_name
                            if (append_kind_single) then
                                code = code // "("
                                code = code // &
                                       trim(adjustl(int_to_string(node%kind_value)))
                                code = code // ")"
                            end if
                            if (node%has_intent) then
                                code = code // ", intent(" // node%intent // ")"
                            else if (allocated(param_map(param_idx)%intent_str)) then
                                if (len_trim(param_map(param_idx)%intent_str) > 0) then
                                    code = code // ", intent("
                                    code = code // param_map(param_idx)%intent_str
                                    code = code // ")"
                                end if
                            end if
                            if (node%is_optional .or. &
                                param_map(param_idx)%is_optional) then
                                code = code // ", optional"
                            end if
                            if (node%is_target .or. &
                                param_map(param_idx)%is_target) then
                                code = code // ", target"
                            end if
                            if (node%is_pointer) then
                                code = code // ", pointer"
                            end if
                            code = code // " :: " // trim(node%var_name)
                            if (allocated(node%dimension_indices)) then
                                if (size(node%dimension_indices) > 0) then
                                    code = code // "("
                                    do j = 1, size(node%dimension_indices)
                                        if (j > 1) code = code // ", "
                                        stmt_code = generate_code_from_arena( &
                                                    arena, node%dimension_indices(j))
                                        code = code // stmt_code
                                    end do
                                    code = code // ")"
                                end if
                            end if
                            code = code // new_line('A')
                        end if
                    end if
                type is (parameter_declaration_node)
                    param_idx = find_parameter_info(param_map, node%name)
                    if (param_idx > 0) then
                        type_name = trim(node%type_name)
                        if (is_character_type_string(type_name)) then
                            type_name = normalize_character_type_param(type_name, &
                                                                       node%has_kind, &
                                                                       node%kind_value)
                        end if
                        append_kind_param = node%has_kind .and. .not. &
                                            is_character_type_string(type_name)
                        if (len_trim(type_name) == 0) cycle
                        code = code // indent_str // type_name
                        if (append_kind_param) then
                            code = code // "("
                            code = code // &
                                   trim(adjustl(int_to_string(node%kind_value)))
                            code = code // ")"
                        end if
                        if (len_trim(param_map(param_idx)%intent_str) > 0) then
                            code = code // ", intent(" // &
                                   param_map(param_idx)%intent_str // ")"
                        end if
                        if (param_map(param_idx)%is_optional) then
                            code = code // ", optional"
                        end if
                        if (param_map(param_idx)%is_target) then
                            code = code // ", target"
                        end if
                        code = code // " :: " // param_map(param_idx)%name
                        if (allocated(node%dimension_indices)) then
                            if (size(node%dimension_indices) > 0) then
                                code = code // "("
                                do j = 1, size(node%dimension_indices)
                                    if (j > 1) code = code // ", "
                                    stmt_code = generate_code_from_arena( &
                                                arena, node%dimension_indices(j))
                                    code = code // stmt_code
                                end do
                                code = code // ")"
                            end if
                        end if
                        code = code // new_line('A')
                    end if
                end select
            end do
        end if

        allocate (filtered_indices(size(body_indices)))
        filtered_count = 0
        in_contains_section = .false.

        do i = 1, size(body_indices)
            should_skip = .false.
            if (body_indices(i) <= 0 .or. body_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(body_indices(i))%node)) cycle

            select type (node => arena%entries(body_indices(i))%node)
            type is (contains_node)
                in_contains_section = .true.
            type is (declaration_node)
                if (node%is_multi_declaration .and. allocated(node%var_names)) then
                    do var_idx = 1, size(node%var_names)
                        param_idx = find_parameter_info(param_map, &
                                                        trim(node%var_names(var_idx)))
                        if (param_idx > 0) then
                            should_skip = .true.
                            exit
                        end if
                    end do
                else
                    param_idx = find_parameter_info(param_map, node%var_name)
                    if (param_idx > 0) should_skip = .true.
                end if

                if (.not. should_skip .and. has_return_type_in_signature) then
                    if (len_trim(result_var_name) > 0) then
                        has_dimensions = allocated(node%dimension_indices)
                        if (has_dimensions) has_dimensions = &
                            size(node%dimension_indices) > 0
                        keep_result_decl = node%is_multi_declaration
                        keep_result_decl = keep_result_decl .or. node%is_array
                        keep_result_decl = keep_result_decl .or. has_dimensions
                        keep_result_decl = keep_result_decl .or. node%is_allocatable
                        keep_result_decl = keep_result_decl .or. node%is_pointer
                        keep_result_decl = keep_result_decl .or. node%is_target
                        keep_result_decl = keep_result_decl .or. node%is_parameter
                        keep_result_decl = keep_result_decl .or. node%has_initializer
                        if (force_keep_result_decl) keep_result_decl = .true.
                        if (.not. keep_result_decl) then
                            if (node%is_multi_declaration .and. &
                                allocated(node%var_names)) then
                                do var_idx = 1, size(node%var_names)
                                    if (trim(node%var_names(var_idx)) == &
                                        result_var_name) then
                                        should_skip = .true.
                                        exit
                                    end if
                                end do
                            else
                                if (trim(node%var_name) == result_var_name) then
                                    should_skip = .true.
                                end if
                            end if
                        end if
                    end if
                end if
            type is (parameter_declaration_node)
                param_idx = find_parameter_info(param_map, node%name)
                if (param_idx > 0) should_skip = .true.
            end select

            if (.not. should_skip) then
                filtered_count = filtered_count + 1
                filtered_indices(filtered_count) = body_indices(i)
            end if
        end do

        if (filtered_count > 0) then
            code = code // generate_grouped_body( &
                   arena, filtered_indices(1:filtered_count), indent)
        end if

        call reorder_import_lines(code)

        deallocate (filtered_indices)
    end function generate_grouped_body_with_params

end module codegen_grouped_body_params
