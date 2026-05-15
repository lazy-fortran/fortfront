module codegen_grouped_body_params_helpers
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_character_normalization, only: normalize_character_type, &
                                               normalize_character_type_param
    use codegen_parameter_info, only: parameter_info_t, find_parameter_info
    use string_utils_mod, only: int_to_string, to_lower
    use type_string_utils, only: is_character_type_string
    implicit none
    private

    public :: process_multi_decl_params
    public :: process_single_decl_param
    public :: process_param_decl_node
    public :: should_skip_result_decl

contains

    subroutine process_multi_decl_params(arena, node, param_map, indent_str, &
                                         standardize_types, preserve_intent, &
                                         code)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: node
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=*), intent(in) :: indent_str
        logical, intent(in) :: standardize_types
        logical, intent(in) :: preserve_intent
        character(len=:), allocatable, intent(inout) :: code
        logical :: found_params
        logical, allocatable :: is_param(:)
        integer :: first_param_idx
        integer :: param_idx
        integer :: j
        character(len=:), allocatable :: type_name
        logical :: append_kind
        character(len=:), allocatable :: dimension_suffix
        logical :: has_dimensions

        allocate (is_param(size(node%var_names)))
        found_params = .false.
        first_param_idx = 0

        do j = 1, size(node%var_names)
            param_idx = find_parameter_info(param_map, trim(node%var_names(j)))
            is_param(j) = (param_idx > 0)
            if (param_idx > 0) then
                found_params = .true.
                if (first_param_idx == 0) first_param_idx = param_idx
            end if
        end do

        if (.not. found_params) then
            deallocate (is_param)
            return
        end if

        type_name = trim(node%type_name)
        if (is_character_type_string(type_name)) then
            type_name = normalize_character_type(node, type_name)
        else if (standardize_types .and. to_lower(trim(type_name)) == "real") then
            if (.not. node%has_kind) type_name = "real(dp)"
        end if

        append_kind = node%has_kind .and. .not. is_character_type_string(type_name)
        code = code // indent_str // type_name
        if (append_kind) then
            code = code // "(" // trim(adjustl(int_to_string(node%kind_value))) &
                   // ")"
        end if

        call add_intent_optional_target(node, param_map, first_param_idx, &
                                        preserve_intent, code)

        dimension_suffix = build_inline_dimension_suffix(arena, node)
        has_dimensions = len(dimension_suffix) > 0

        code = code // " :: "
        do j = 1, size(node%var_names)
            if (.not. is_param(j)) cycle
            if (j > 1) code = code // ", "
            code = code // trim(node%var_names(j))
            if (has_dimensions) code = code // dimension_suffix
        end do
        code = code // new_line('A')

        call process_nonparam_vars(arena, node, is_param, indent_str, &
                                   standardize_types, code)
        deallocate (is_param)
    end subroutine process_multi_decl_params

    subroutine add_intent_optional_target(node, param_map, first_param_idx, &
                                          preserve_intent, code)
        type(declaration_node), intent(in) :: node
        type(parameter_info_t), intent(in) :: param_map(:)
        integer, intent(in) :: first_param_idx
        logical, intent(in) :: preserve_intent
        character(len=:), allocatable, intent(inout) :: code
        character(len=:), allocatable :: param_intent

        if (node%has_intent) then
            code = code // ", intent(" // node%intent // ")"
        else if (preserve_intent) then
            if (first_param_idx > 0) then
                if (allocated(param_map(first_param_idx)%intent_str)) then
                    param_intent = trim(param_map(first_param_idx)%intent_str)
                    if (len_trim(param_intent) > 0) then
                        code = code // ", intent(" // param_intent // ")"
                    end if
                end if
            end if
        end if

        if (node%is_optional .or. param_map(first_param_idx)%is_optional) then
            code = code // ", optional"
        end if
        if (node%is_target .or. param_map(first_param_idx)%is_target) then
            code = code // ", target"
        end if
        if (node%is_allocatable) code = code // ", allocatable"
        if (node%is_pointer) code = code // ", pointer"
    end subroutine add_intent_optional_target

    subroutine process_nonparam_vars(arena, node, is_param, indent_str, &
                                     standardize_types, code)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: node
        logical, intent(in) :: is_param(:)
        character(len=*), intent(in) :: indent_str
        logical, intent(in) :: standardize_types
        character(len=:), allocatable, intent(inout) :: code
        character(len=:), allocatable :: nonparam_list
        character(len=:), allocatable :: local_type
        character(len=:), allocatable :: kind_text
        character(len=:), allocatable :: dimension_suffix
        logical :: have_nonparam
        logical :: local_append_kind
        logical :: type_is_character
        logical :: has_dimensions
        integer :: j

        nonparam_list = ""
        dimension_suffix = build_inline_dimension_suffix(arena, node)
        has_dimensions = len(dimension_suffix) > 0
        have_nonparam = .false.
        do j = 1, size(node%var_names)
            if (.not. is_param(j)) then
                if (have_nonparam) nonparam_list = nonparam_list // ", "
                nonparam_list = nonparam_list // trim(node%var_names(j))
                if (has_dimensions) nonparam_list = nonparam_list // dimension_suffix
                have_nonparam = .true.
            end if
        end do

        if (.not. have_nonparam) return

        local_type = trim(node%type_name)
        if (is_character_type_string(local_type)) then
            local_type = normalize_character_type(node, local_type)
        end if
        type_is_character = is_character_type_string(local_type)
        local_append_kind = node%has_kind .and. .not. type_is_character
        if (local_type == "real" .and. .not. local_append_kind .and. &
            standardize_types) then
            local_type = "real(dp)"
        end if
        code = code // indent_str // local_type
        if (local_append_kind) then
            kind_text = trim(adjustl(int_to_string(node%kind_value)))
            code = code // "(" // kind_text // ")"
        end if
        code = code // " :: " // nonparam_list // new_line('A')
    end subroutine process_nonparam_vars

    subroutine process_single_decl_param(arena, node, param_map, indent_str, &
                                         standardize_types, code)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: node
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=*), intent(in) :: indent_str
        logical, intent(in) :: standardize_types
        character(len=:), allocatable, intent(inout) :: code
        integer :: param_idx
        character(len=:), allocatable :: type_name
        logical :: append_kind_single
        character(len=:), allocatable :: dimension_suffix
        logical :: has_dimensions

        param_idx = find_parameter_info(param_map, node%var_name)
        if (param_idx <= 0) return

        type_name = trim(node%type_name)
        if (is_character_type_string(type_name)) then
            type_name = normalize_character_type(node, type_name)
        else if (standardize_types .and. to_lower(trim(type_name)) == "real" &
                 .and. .not. node%has_kind) then
            type_name = "real(dp)"
        end if
        append_kind_single = node%has_kind .and. .not. &
                             is_character_type_string(type_name)
        code = code // indent_str // type_name
        if (append_kind_single) then
            code = code // "(" // trim(adjustl(int_to_string(node%kind_value))) &
                   // ")"
        end if
        if (node%has_intent) then
            code = code // ", intent(" // node%intent // ")"
        else if (allocated(param_map(param_idx)%intent_str)) then
            if (len_trim(param_map(param_idx)%intent_str) > 0) then
                code = code // ", intent(" // param_map(param_idx)%intent_str &
                       // ")"
            end if
        end if
        if (node%is_optional .or. param_map(param_idx)%is_optional) then
            code = code // ", optional"
        end if
        if (node%is_target .or. param_map(param_idx)%is_target) then
            code = code // ", target"
        end if
        if (node%is_allocatable) code = code // ", allocatable"
        if (node%is_pointer) code = code // ", pointer"
        dimension_suffix = build_inline_dimension_suffix(arena, node)
        has_dimensions = len(dimension_suffix) > 0
        code = code // " :: " // trim(node%var_name)
        if (has_dimensions) code = code // dimension_suffix
        code = code // new_line('A')
    end subroutine process_single_decl_param

    subroutine process_param_decl_node(arena, node, param_map, indent_str, code)
        type(ast_arena_t), intent(in) :: arena
        type(parameter_declaration_node), intent(in) :: node
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=*), intent(in) :: indent_str
        character(len=:), allocatable, intent(inout) :: code
        integer :: param_idx
        character(len=:), allocatable :: type_name
        logical :: append_kind_param
        integer :: j

        param_idx = find_parameter_info(param_map, node%name)
        if (param_idx <= 0) return

        type_name = trim(node%type_name)
        if (is_character_type_string(type_name)) then
            type_name = normalize_character_type_param(type_name, node%has_kind, &
                                                       node%kind_value)
        end if
        append_kind_param = node%has_kind .and. .not. &
                            is_character_type_string(type_name)
        if (len_trim(type_name) == 0) return
        code = code // indent_str // type_name
        if (append_kind_param) then
            code = code // "(" // trim(adjustl(int_to_string(node%kind_value))) &
                   // ")"
        end if
        if (len_trim(param_map(param_idx)%intent_str) > 0) then
            code = code // ", intent(" // param_map(param_idx)%intent_str // ")"
        end if
        if (param_map(param_idx)%is_optional) code = code // ", optional"
        if (param_map(param_idx)%is_target) code = code // ", target"
        code = code // " :: " // param_map(param_idx)%name
        ! Add inline dimension suffix (e.g., x(:) or x(3,5))
        if (allocated(node%dimension_indices)) then
            if (size(node%dimension_indices) > 0) then
                code = code // "("
                do j = 1, size(node%dimension_indices)
                    if (j > 1) code = code // ","
                    code = code // ":"
                end do
                code = code // ")"
            end if
        end if
        code = code // new_line('A')
    end subroutine process_param_decl_node

    function build_inline_dimension_suffix(arena, node) result(suffix)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: node
        character(len=:), allocatable :: suffix
        character(len=:), allocatable :: dim_code
        integer :: j
        integer :: dim_index

        suffix = ""
        if (.not. node%is_array) return
        if (.not. allocated(node%dimension_indices)) return
        if (size(node%dimension_indices) == 0) return

        suffix = "("
        do j = 1, size(node%dimension_indices)
            if (j > 1) suffix = suffix // ", "
            dim_index = node%dimension_indices(j)
            if (dim_index <= 0) then
                suffix = suffix // ":"
            else
                dim_code = generate_code_from_arena(arena, dim_index)
                if (len_trim(dim_code) == 0) then
                    suffix = suffix // ":"
                else
                    suffix = suffix // dim_code
                end if
            end if
        end do
        suffix = suffix // ")"
    end function build_inline_dimension_suffix

    function should_skip_result_decl(node, result_var_name, &
                                     has_return_type_in_signature, &
                                     force_keep_result_decl) result(should_skip)
        type(declaration_node), intent(in) :: node
        character(len=*), intent(in) :: result_var_name
        logical, intent(in) :: has_return_type_in_signature
        logical, intent(in) :: force_keep_result_decl
        logical :: should_skip
        logical :: keep_result_decl
        logical :: has_dimensions
        integer :: j

        should_skip = .false.
        if (.not. has_return_type_in_signature) return
        if (len_trim(result_var_name) == 0) return

        has_dimensions = allocated(node%dimension_indices)
        if (has_dimensions) has_dimensions = size(node%dimension_indices) > 0
        keep_result_decl = node%is_multi_declaration .or. node%is_array .or. &
                           has_dimensions .or. node%is_allocatable .or. &
                           node%is_pointer .or. node%is_target .or. &
                           node%is_parameter .or. node%has_initializer
        if (force_keep_result_decl) keep_result_decl = .true.
        if (keep_result_decl) return

        if (node%is_multi_declaration .and. allocated(node%var_names)) then
            do j = 1, size(node%var_names)
                if (trim(node%var_names(j)) == result_var_name) then
                    should_skip = .true.
                    return
                end if
            end do
        else
            if (trim(node%var_name) == result_var_name) should_skip = .true.
        end if
    end function should_skip_result_decl

end module codegen_grouped_body_params_helpers
