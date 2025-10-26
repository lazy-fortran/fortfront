module codegen_grouped_body_params
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: ast_node
    use ast_nodes_misc, only: contains_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use ast_nodes_procedure, only: function_def_node
    use codegen_arena_utils, only: check_result_is_simple_scalar
    use codegen_grouped_body, only: generate_grouped_body
    use codegen_grouped_body_params_helpers, only: process_multi_decl_params, &
                                                    process_single_decl_param, &
                                                    process_param_decl_node, &
                                                    should_skip_result_decl
    use codegen_import_reorder, only: reorder_import_lines
    use codegen_parameter_info, only: parameter_info_t, find_parameter_info
    use codegen_type_utils, only: get_type_standardization
    use string_utils_mod, only: to_lower
    implicit none
    private

    logical, save :: standardize_types_enabled = .false.

    public :: generate_grouped_body_with_params

contains

    subroutine init_result_var_info(proc_node, arena, result_var_name, &
                                    has_return_type_in_signature, &
                                    force_keep_result_decl)
        class(ast_node), intent(in) :: proc_node
        type(ast_arena_t), intent(in) :: arena
        character(len=:), allocatable, intent(out) :: result_var_name
        logical, intent(out) :: has_return_type_in_signature
        logical, intent(out) :: force_keep_result_decl
        logical :: has_header_return_type
        character(len=:), allocatable :: lowered_return

        force_keep_result_decl = .false.
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
                call check_result_is_simple_scalar(arena, proc_node, &
                                                   result_var_name, &
                                                   has_return_type_in_signature)
            end if
            if (len_trim(result_var_name) > 0 .and. .not. &
                has_header_return_type) then
                force_keep_result_decl = .true.
            end if
        end select
    end subroutine init_result_var_info

    subroutine gen_param_declarations(arena, body_indices, param_map, &
                                      indent_str, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=*), intent(in) :: indent_str
        character(len=:), allocatable, intent(inout) :: code
        integer :: i

        if (size(param_map) == 0) return

        do i = 1, size(body_indices)
            if (body_indices(i) <= 0 .or. body_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(body_indices(i))%node)) cycle
            select type (node => arena%entries(body_indices(i))%node)
            type is (declaration_node)
                if (node%is_multi_declaration .and. allocated(node%var_names)) then
                    call process_multi_decl_params(arena, node, param_map, &
                                                   indent_str, &
                                                   standardize_types_enabled, code)
                else
                    call process_single_decl_param(arena, node, param_map, &
                                                   indent_str, &
                                                   standardize_types_enabled, code)
                end if
            type is (parameter_declaration_node)
                call process_param_decl_node(arena, node, param_map, indent_str, &
                                             code)
            end select
        end do
    end subroutine gen_param_declarations

    subroutine build_filtered_indices(arena, body_indices, param_map, &
                                      result_var_name, &
                                      has_return_type_in_signature, &
                                      force_keep_result_decl, filtered_indices, &
                                      filtered_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=*), intent(in) :: result_var_name
        logical, intent(in) :: has_return_type_in_signature
        logical, intent(in) :: force_keep_result_decl
        integer, allocatable, intent(out) :: filtered_indices(:)
        integer, intent(out) :: filtered_count
        logical :: should_skip
        integer :: i
        integer :: param_idx
        integer :: var_idx

        allocate (filtered_indices(size(body_indices)))
        filtered_count = 0

        do i = 1, size(body_indices)
            should_skip = .false.
            if (body_indices(i) <= 0 .or. body_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(body_indices(i))%node)) cycle

            select type (node => arena%entries(body_indices(i))%node)
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

                if (.not. should_skip) then
                    should_skip = should_skip_result_decl(node, result_var_name, &
                                                          has_return_type_in_signature, &
                                                          force_keep_result_decl)
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
    end subroutine build_filtered_indices

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
        character(len=:), allocatable :: result_var_name
        logical :: has_return_type_in_signature
        logical :: force_keep_result_decl
        integer, allocatable :: filtered_indices(:)
        integer :: filtered_count

        indent_str = repeat("    ", indent)
        code = ""
        call get_type_standardization(standardize_types_enabled)

        call init_result_var_info(proc_node, arena, result_var_name, &
                                  has_return_type_in_signature, &
                                  force_keep_result_decl)

        call gen_param_declarations(arena, body_indices, param_map, indent_str, &
                                    code)

        call build_filtered_indices(arena, body_indices, param_map, &
                                    result_var_name, &
                                    has_return_type_in_signature, &
                                    force_keep_result_decl, filtered_indices, &
                                    filtered_count)

        if (filtered_count > 0) then
            code = code // generate_grouped_body( &
                   arena, filtered_indices(1:filtered_count), indent)
        end if

        call reorder_import_lines(code)

        deallocate (filtered_indices)
    end function generate_grouped_body_with_params

end module codegen_grouped_body_params
