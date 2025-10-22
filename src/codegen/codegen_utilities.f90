module codegen_utilities
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: ast_node
    use ast_nodes_core
    use ast_nodes_data
    use ast_nodes_procedure
    use ast_nodes_control
    use ast_nodes_loops
    use ast_nodes_io
    use ast_nodes_misc
    use ast_nodes_transfer
    use ast_nodes_data, only: intent_type_to_string, INTENT_NONE
    use type_system_unified
    use string_types, only: string_t
    use codegen_indent
    use type_string_utils, only: is_character_type_string
    use codegen_arena_interface, only: generate_code_from_arena
    use string_utils_mod, only: int_to_string, to_lower
    implicit none
    private

    ! Type standardization configuration
    logical, save :: standardize_types_enabled = .true.

    ! Context for executable code before contains
    logical, save :: context_has_executable_before_contains = .false.

    public :: find_node_index_in_arena
    public :: same_node
    public :: can_group_declarations
    public :: can_group_parameters
    public :: can_group_declarations_with_params
    public :: build_param_name_with_dims
    public :: generate_grouped_declaration
    public :: generate_grouped_body
    public :: generate_grouped_body_with_params
    public :: generate_grouped_body_context
    public :: find_parameter_info
    public :: is_function_parameter
    public :: is_parameter_name
    public :: is_character_type_string
    public :: normalize_character_type
    public :: normalize_character_type_param

    ! Type for storing parameter information during codegen
    type, public :: parameter_info_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: intent_str
        logical :: is_optional
        logical :: is_target
    end type parameter_info_t

contains

    ! Find node index in arena
    function find_node_index_in_arena(arena, target_node) result(index)
        type(ast_arena_t), intent(in) :: arena
        class(ast_node), intent(in) :: target_node
        integer :: index
        integer :: i

        index = 0
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                if (same_node(arena%entries(i)%node, target_node)) then
                    index = i
                    return
                end if
            end if
        end do
    end function find_node_index_in_arena

    ! Check if two nodes are the same
    function same_node(node1, node2) result(is_same)
        class(ast_node), intent(in) :: node1, node2
        logical :: is_same

        is_same = .false.

        select type (n1 => node1)
        type is (assignment_node)
            select type (n2 => node2)
            type is (assignment_node)
                ! Assignment nodes don't have direct left/right members
                ! in the current implementation. They have indices instead
                is_same = .false.
            end select
        type is (identifier_node)
            select type (n2 => node2)
            type is (identifier_node)
                is_same = (n1%name == n2%name)
            end select
        type is (literal_node)
            select type (n2 => node2)
            type is (literal_node)
                is_same = (n1%literal_type == n2%literal_type)
            end select
        end select
    end function same_node

    ! Check if two declarations can be grouped
    function can_group_declarations(node1, node2) result(can_group)
        type(declaration_node), intent(in) :: node1, node2
        logical :: can_group
        logical :: types_match

        ! Don't group declarations that have initializers
        if (node1%initializer_index > 0 .or. node2%initializer_index > 0) then
            can_group = .false.
            return
        end if

        ! Avoid grouping if array shapes or key attributes differ
        if (node1%is_array .or. node2%is_array) then
            can_group = .false.
            return
        end if

        if (node1%is_allocatable .neqv. node2%is_allocatable) then
            can_group = .false.
            return
        end if
        if (node1%is_pointer .neqv. node2%is_pointer) then
            can_group = .false.
            return
        end if
        if (node1%is_target .neqv. node2%is_target) then
            can_group = .false.
            return
        end if
        if (node1%is_external .neqv. node2%is_external) then
            can_group = .false.
            return
        end if
        if (node1%is_parameter .neqv. node2%is_parameter) then
            can_group = .false.
            return
        end if

        ! Check if types match (explicit or inferred)
        if (len_trim(node1%type_name) > 0 .and. len_trim(node2%type_name) > 0) then
            types_match = trim(node1%type_name) == trim(node2%type_name)
        else if (node1%inferred_type%kind > 0 .and. node2%inferred_type%kind > 0) then
            types_match = node1%inferred_type%kind == node2%inferred_type%kind
        else
            types_match = .false.
        end if

        ! Combine all matching criteria
        can_group = types_match .and. &
                    (node1%kind_value == node2%kind_value) .and. &
                    (node1%has_kind .eqv. node2%has_kind) .and. &
                    ((node1%has_intent .and. node2%has_intent .and. &
                      trim(node1%intent) == trim(node2%intent)) .or. &
                     (.not. node1%has_intent .and. .not. node2%has_intent)) .and. &
                    (node1%is_optional .eqv. node2%is_optional)
    end function can_group_declarations

    ! Check if two parameter declarations can be grouped
    function can_group_parameters(node1, node2) result(can_group)
        type(parameter_declaration_node), intent(in) :: node1, node2
        logical :: can_group

        ! Use type_name instead of type_spec for parameter_declaration_node
        can_group = .true.
        if (allocated(node1%type_name) .and. allocated(node2%type_name)) then
            can_group = (trim(node1%type_name) == trim(node2%type_name))
        end if
        can_group = can_group .and. &
                    (node1%intent_type == node2%intent_type) .and. &
                    (node1%is_optional .eqv. node2%is_optional) .and. &
                    (node1%is_target .eqv. node2%is_target)
    end function can_group_parameters

    ! Check if declarations can be grouped considering parameter mapping
    function can_group_declarations_with_params(node1, node2, param_map) &
        result(can_group)
        type(declaration_node), intent(in) :: node1, node2
        type(parameter_info_t), intent(in) :: param_map(:)
        logical :: can_group
        integer :: idx1, idx2
        character(len=:), allocatable :: intent1, intent2
        logical :: optional1, optional2, target1, target2

        ! Don't group declarations that have initializers
        if (node1%initializer_index > 0 .or. node2%initializer_index > 0) then
            can_group = .false.
            return
        end if

        ! Get intent and optional from param_map if these are parameters
        idx1 = find_parameter_info(param_map, node1%var_name)
        idx2 = find_parameter_info(param_map, node2%var_name)

        if (idx1 > 0) then
            intent1 = param_map(idx1)%intent_str
            optional1 = param_map(idx1)%is_optional
            target1 = param_map(idx1)%is_target
        else
            if (node1%has_intent) then
                intent1 = node1%intent
            else
                intent1 = ""
            end if
            optional1 = node1%is_optional
            target1 = node1%is_target
        end if

        if (idx2 > 0) then
            intent2 = param_map(idx2)%intent_str
            optional2 = param_map(idx2)%is_optional
            target2 = param_map(idx2)%is_target
        else
            if (node2%has_intent) then
                intent2 = node2%intent
            else
                intent2 = ""
            end if
            optional2 = node2%is_optional
            target2 = node2%is_target
        end if

        can_group = trim(node1%type_name) == trim(node2%type_name) .and. &
                    node1%kind_value == node2%kind_value .and. &
                    node1%has_kind .eqv. node2%has_kind .and. &
                    trim(intent1) == trim(intent2) .and. &
                    optional1 .eqv. optional2 .and. &
                    target1 .eqv. target2
    end function can_group_declarations_with_params

    ! Build parameter name with dimensions
    function build_param_name_with_dims(arena, param_node) result(name_with_dims)
        type(ast_arena_t), intent(in) :: arena
        type(parameter_declaration_node), intent(in) :: param_node
        character(len=:), allocatable :: name_with_dims
        integer :: d
        character(len=:), allocatable :: dim_code

        name_with_dims = param_node%name
        ! parameter_declaration_node currently lacks dimension_indices
        ! so this remains a placeholder for future enhancement
    end function build_param_name_with_dims

    ! Generate grouped declaration statement
    function generate_grouped_declaration(type_name, kind_value, has_kind, &
                                          intent, var_list, is_optional, &
                                          is_target) result(stmt)
        character(len=*), intent(in) :: type_name
        integer, intent(in) :: kind_value
        logical, intent(in) :: has_kind
        character(len=*), intent(in) :: intent
        character(len=*), intent(in) :: var_list
        logical, intent(in), optional :: is_optional, is_target
        character(len=:), allocatable :: stmt
        logical :: opt_flag, target_flag

        opt_flag = .false.
        if (present(is_optional)) opt_flag = is_optional
        target_flag = .false.
        if (present(is_target)) target_flag = is_target

        stmt = type_name
        if (is_character_type_string(stmt)) then
            stmt = normalize_character_type_param(stmt, has_kind, kind_value)
        else if (has_kind) then
            stmt = stmt // "(" // trim(adjustl(int_to_string(kind_value))) // ")"
        end if
        if (len_trim(intent) > 0) then
            stmt = stmt // ", intent(" // intent // ")"
        end if
        if (opt_flag) then
            stmt = stmt // ", optional"
        end if
        if (target_flag) then
            stmt = stmt // ", target"
        end if
        stmt = stmt // " :: " // var_list
    end function generate_grouped_declaration

    ! Generate grouped body statements
    function generate_grouped_body(arena, body_indices, indent) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: indent
        character(len=:), allocatable :: code
        character(len=:), allocatable :: stmt_code
        character(len=:), allocatable :: indent_str
        integer :: i, j
        logical :: in_contains_section
        logical :: found_contains

        ! Build indent string based on indent level
        indent_str = repeat("    ", indent)

        code = ""
        in_contains_section = .false.
        i = 1

        do while (i <= size(body_indices))
            if (body_indices(i) > 0 .and. body_indices(i) <= arena%size) then
                if (allocated(arena%entries(body_indices(i))%node)) then
                    select type (node => arena%entries(body_indices(i))%node)
                    type is (contains_node)
                        in_contains_section = .true.
                        code = code // "contains" // new_line('A')
                        i = i + 1

                    type is (end_statement_node)
                        ! Skip end statements, they're handled by parent
                        i = i + 1

                    type is (function_def_node)
                        if (in_contains_section .and. i > 1) then
                            ! Insert blank line between contains procedures
                            code = code // new_line('A')
                        end if
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_str // stmt_code // new_line('A')
                        i = i + 1

                    type is (subroutine_def_node)
                        if (in_contains_section .and. i > 1) then
                            ! Insert blank line between contains procedures
                            code = code // new_line('A')
                        end if
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_str // stmt_code // new_line('A')
                        i = i + 1

                    type is (declaration_node)
                        if (is_type_definition_declaration(node)) then
                            i = i + 1
                            cycle
                        end if
                        ! Group consecutive declarations of the same type
                        if (.not. in_contains_section .and. &
                            node%initializer_index == 0) then
                            call process_grouped_declarations(arena, body_indices, i, &
                                                              indent_str, code)
                        else
                            stmt_code = generate_code_from_arena(arena, &
                                                                 body_indices(i))
                            code = code // indent_lines(stmt_code, indent) // &
                                   new_line('A')
                            i = i + 1
                        end if

                    type is (parameter_declaration_node)
                        ! Group consecutive parameter declarations
                        call process_grouped_parameters(arena, body_indices, i, &
                                                        indent_str, code)

                    type is (comment_node)
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        ! Comments preserve their own indentation
                        code = code // stmt_code // new_line('A')
                        i = i + 1

                    type is (blank_line_node)
                        code = code // new_line('A')
                        i = i + 1

                    type is (write_statement_node)
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_lines(stmt_code, indent) // new_line('A')
                        i = i + 1

                    type is (print_statement_node)
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_lines(stmt_code, indent) // new_line('A')
                        i = i + 1

                    type is (read_statement_node)
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_lines(stmt_code, indent) // new_line('A')
                        i = i + 1

                    type is (format_statement_node)
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_lines(stmt_code, indent) // new_line('A')
                        i = i + 1

                    type is (goto_node)
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_lines(stmt_code, indent) // new_line('A')
                        i = i + 1

                    type is (return_node)
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_lines(stmt_code, indent) // new_line('A')
                        i = i + 1

                    type is (continue_node)
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_lines(stmt_code, indent) // new_line('A')
                        i = i + 1

                    type is (stop_node)
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_lines(stmt_code, indent) // new_line('A')
                        i = i + 1

                    type is (error_stop_node)
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_lines(stmt_code, indent) // new_line('A')
                        i = i + 1

                    type is (cycle_node)
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_lines(stmt_code, indent) // new_line('A')
                        i = i + 1

                    type is (exit_node)
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_lines(stmt_code, indent) // new_line('A')
                        i = i + 1

                    class default
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_lines(stmt_code, indent) // new_line('A')
                        i = i + 1
                    end select
                else
                    i = i + 1
                end if
            else
                i = i + 1
            end if
        end do
    end function generate_grouped_body

    ! Generate grouped body with parameter mapping
    function generate_grouped_body_with_params(arena, body_indices, indent, &
                                               param_map, &
                                               proc_node) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: indent
        type(parameter_info_t), intent(in) :: param_map(:)
        class(ast_node), intent(in) :: proc_node
        character(len=:), allocatable :: code
        character(len=:), allocatable :: indent_str, stmt_code
        character(len=:), allocatable :: type_name
        integer :: i, j, param_idx
        logical :: has_params_with_attrs
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
        character(len=:), allocatable :: lowered_return
        logical :: has_dimensions

        ! Build indent string
        indent_str = repeat("    ", indent)
        code = ""
        force_keep_result_decl = .false.

        ! Determine if we should skip result variable declarations
        ! Skip when: function has return type in signature AND has result variable
        has_return_type_in_signature = .false.
        result_var_name = ""
        select type (proc_node)
        type is (function_def_node)
            if (allocated(proc_node%return_type) .and. &
                len_trim(proc_node%return_type) > 0) then
                has_return_type_in_signature = .true.
                if (allocated(proc_node%result_variable)) then
                    result_var_name = trim(proc_node%result_variable)
                else if (allocated(proc_node%name)) then
                    result_var_name = trim(proc_node%name)
                end if
                lowered_return = to_lower(trim(proc_node%return_type))
                if (index(lowered_return, "len=") > 0) then
                    force_keep_result_decl = .true.
                end if
            end if
        end select

        ! First pass: collect parameter declarations from the body to capture
        ! types and attributes. Attributes may appear inside the body rather
        ! than the signature.
        if (size(param_map) > 0) then
            do i = 1, size(body_indices)
                if (body_indices(i) > 0 .and. body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(body_indices(i))%node)) then
                        select type (node => arena%entries(body_indices(i))%node)
                        type is (declaration_node)
                            ! Check if this declaration is for parameter(s)
                            ! Handle both single and multi-declarations for parameters
                            if (node%is_multi_declaration .and. &
                                allocated(node%var_names)) then
                                ! Multi-variable declaration - check each variable
                                block
                                    logical :: found_params, first_var
                                    logical, allocatable :: is_param(:)
                                    integer :: first_param_idx
                                    logical :: append_kind

                                    allocate (is_param(size(node%var_names)))
                                    found_params = .false.
                                    first_param_idx = 0

                                    ! Check which variables are parameters
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
                                        ! Parameter declarations
                                        ! generated for this block
                                        type_name = trim(node%type_name)
                                        if (is_character_type_string(type_name)) then
                                            type_name = &
                                                normalize_character_type(node, &
                                                                         type_name)
                                        end if
                                        append_kind = node%has_kind .and. .not. &
                                                     is_character_type_string(type_name)
                                        code = code // indent_str // type_name

                                        if (append_kind) then
                                            code = code // "(" // &
                                    trim(adjustl(int_to_string(node%kind_value))) // ")"
                                        end if

                                        ! Use attributes from the declaration node
                                        if (node%has_intent) then
                                            code = code // ", intent(" // &
                                                   node%intent // ")"
                                        else if &
                               (allocated(param_map(first_param_idx)%intent_str) .and. &
                               len_trim(param_map(first_param_idx)%intent_str) > 0) then
                                            code = code // ", intent(" // &
                                            param_map(first_param_idx)%intent_str // ")"
                                        end if

                                        if (node%is_optional) then
                                            code = code // ", optional"
                                        else if &
                                           (param_map(first_param_idx)%is_optional) then
                                            code = code // ", optional"
                                        end if

                                        if (node%is_target) then
                                            code = code // ", target"
                                        else if &
                                            (param_map(first_param_idx)%is_target) then
                                            code = code // ", target"
                                        end if

                                        if (node%is_pointer) then
                                            code = code // ", pointer"
                                        end if

                                        code = code // " :: "

                                        ! Add all parameter names
                                        first_var = .true.
                                        do j = 1, size(node%var_names)
                                            if (is_param(j)) then
                                                if (.not. first_var) code = &
                                                    code // ", "
                                                code = code // trim(node%var_names(j))
                                                first_var = .false.
                                            end if
                                        end do

                                        code = code // new_line('A')

                                        ! Emit declarations for
                                        ! non-parameter local variables
                                        block
                                            character(len=:), allocatable :: &
                                                nonparam_list
                                            logical :: have_nonparam
                                            character(len=:), allocatable :: local_type
                                            logical :: local_append_kind

                                            nonparam_list = ""
                                            have_nonparam = .false.
                                            do j = 1, size(node%var_names)
                                                if (.not. is_param(j)) then
                                                    if (have_nonparam) then
                                                        nonparam_list = nonparam_list &
                                                                        // ", " // &
                                                                 trim(node%var_names(j))
                                                    else
                                                        nonparam_list = &
                                                            trim(node%var_names(j))
                                                    end if
                                                    have_nonparam = .true.
                                                end if
                                            end do

                                            if (have_nonparam) then
                                                local_type = trim(node%type_name)
                                          if (is_character_type_string(local_type)) then
                                                    local_type = &
                                                        normalize_character_type(node, &
                                                                             local_type)
                                                end if
                                                local_append_kind = &
                                                    node%has_kind .and. &
                                                    .not. &
                                                    is_character_type_string(local_type)
                                                if (local_type == 'real' .and. .not. &
                                                    local_append_kind) then
                                                    local_type = 'real(8)'
                                                end if
                                                code = code // indent_str // local_type
                                                if (local_append_kind) then
                                                    code = code // "(" // &
                                    trim(adjustl(int_to_string(node%kind_value))) // ")"
                                                end if
                                                ! Skip intent/optional for
                                                ! locals that are not parameters
                                                code = code // " :: " // &
                                                       nonparam_list &
                                                       // new_line('A')
                                            end if
                                        end block
                                    end if

                                    deallocate (is_param)
                                end block
                            else
                                ! Single variable declaration
                                param_idx = find_parameter_info(param_map, &
                                                                node%var_name)
                                if (param_idx > 0) then
                                    ! Use declaration node
                                    ! attributes when emitting
                                    type_name = trim(node%type_name)
                                    if (is_character_type_string(type_name)) then
                                        type_name = &
                                            normalize_character_type(node, type_name)
                                    end if
                                    append_kind_single = node%has_kind .and. .not. &
                                                     is_character_type_string(type_name)
                                    code = code // indent_str // type_name

                                    if (append_kind_single) then
                                        code = code // "(" // &
                                    trim(adjustl(int_to_string(node%kind_value))) // ")"
                                    end if

                                    ! Use attributes from the declaration node itself
                                    if (node%has_intent) then
                                        code = code // ", intent(" // &
                                               node%intent // ")"
                                    else if &
                                     (allocated(param_map(param_idx)%intent_str) .and. &
                                     len_trim(param_map(param_idx)%intent_str) > 0) then
                                        code = code // ", intent(" // &
                                               param_map(param_idx)%intent_str // ")"
                                    end if

                                    if (node%is_optional) then
                                        code = code // ", optional"
                                    else if (param_map(param_idx)%is_optional) then
                                        code = code // ", optional"
                                    end if

                                    if (node%is_target) then
                                        code = code // ", target"
                                    else if (param_map(param_idx)%is_target) then
                                        code = code // ", target"
                                    end if

                                    if (node%is_pointer) then
                                        code = code // ", pointer"
                                    end if

                                    code = code // " :: " // param_map(param_idx)%name

                                    ! Add dimensions if present
                                    if (allocated(node%dimension_indices) .and. &
                                        size(node%dimension_indices) > 0) then
                                        code = code // "("
                                        do j = 1, size(node%dimension_indices)
                                            if (j > 1) code = code // ", "
                                            stmt_code = &
                                                generate_code_from_arena(arena, &
                                                              node%dimension_indices(j))
                                            code = code // stmt_code
                                        end do
                                        code = code // ")"
                                    end if

                                    code = code // new_line('A')
                                end if
                            end if
                        type is (parameter_declaration_node)
                            ! Determine if this node is
                            ! a parameter declaration
                            param_idx = find_parameter_info(param_map, node%name)
                            if (param_idx > 0) then
                                ! Emit declaration using
                                ! parameter_declaration_node data
                                type_name = trim(node%type_name)
                                if (is_character_type_string(type_name)) then
                                    type_name = &
                                        normalize_character_type_param(type_name, &
                                                                       node%has_kind, &
                                                                       node%kind_value)
                                end if
                                append_kind_param = node%has_kind .and. .not. &
                                                    is_character_type_string(type_name)
                                ! Debug: print if type_name is empty
                                if (len_trim(type_name) == 0) then
                                    ! Skip if no type name - will be handled elsewhere
                                    cycle
                                end if
                                code = code // indent_str // type_name

                                if (append_kind_param) then
                                    code = code // "(" // &
                                    trim(adjustl(int_to_string(node%kind_value))) // ")"
                                end if

                                ! Apply attributes stored
                                ! on parameter_declaration_node
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

                                ! Add dimensions if present
                                if (allocated(node%dimension_indices) .and. &
                                    size(node%dimension_indices) > 0) then
                                    code = code // "("
                                    do j = 1, size(node%dimension_indices)
                                        if (j > 1) code = code // ", "
                                        stmt_code = generate_code_from_arena(arena, &
                                                              node%dimension_indices(j))
                                        code = code // stmt_code
                                    end do
                                    code = code // ")"
                                end if

                                code = code // new_line('A')
                            end if
                        end select
                    end if
                end if
            end do
        end if

        ! Second pass emits body without
        ! parameter and result declarations
        allocate (filtered_indices(size(body_indices)))
        filtered_count = 0

        do i = 1, size(body_indices)
            should_skip = .false.
            if (body_indices(i) > 0 .and. body_indices(i) <= arena%size) then
                if (allocated(arena%entries(body_indices(i))%node)) then
                    select type (node => arena%entries(body_indices(i))%node)
                    type is (declaration_node)
                        ! Skip parameter declarations if we're handling them separately
                        if (size(param_map) > 0) then
                            if (node%is_multi_declaration .and. &
                                allocated(node%var_names)) then
                                ! Determine if any variable is
                                ! a parameter in this list
                                do var_idx = 1, size(node%var_names)
                                    param_idx = find_parameter_info(param_map, &
                                                          trim(node%var_names(var_idx)))
                                    if (param_idx > 0) then
                                        should_skip = .true.
                                        exit
                                    end if
                                end do
                            else
                                ! Single variable declaration
                                param_idx = find_parameter_info(param_map, &
                                                                node%var_name)
                                if (param_idx > 0) then
                                    should_skip = .true.
                                end if
                            end if
                        end if

                        ! Skip result declaration when
                        ! signature covers it unless
                        ! extra attributes demand one
                        if (.not. should_skip .and. has_return_type_in_signature) then
                            if (len_trim(result_var_name) > 0) then
                                has_dimensions = &
                                    allocated(node%dimension_indices) .and. &
                                    size(node%dimension_indices) > 0
                                keep_result_decl = node%is_multi_declaration .or. &
                                                   node%is_array .or. &
                                                   has_dimensions .or. &
                                                   node%is_allocatable .or. &
                                                   node%is_pointer .or. &
                                                   node%is_target .or. &
                                                   node%is_parameter .or. &
                                                   node%has_initializer

                                if (force_keep_result_decl) keep_result_decl = .true.

                                if (.not. keep_result_decl) then
                                    if (node%is_multi_declaration .and. &
                                        allocated(node%var_names)) then
                                        ! Determine if any variable
                                        ! is the result symbol here
                                        do var_idx = 1, size(node%var_names)
                                            if (trim(node%var_names(var_idx)) == &
                                                result_var_name) then
                                                should_skip = .true.
                                                exit
                                            end if
                                        end do
                                    else
                                        ! Single variable declaration
                                        if (trim(node%var_name) == &
                                            result_var_name) then
                                            should_skip = .true.
                                        end if
                                    end if
                                end if
                            end if
                        end if
                    type is (parameter_declaration_node)
                        ! Also skip parameter_declaration_node entries for parameters
                        if (size(param_map) > 0) then
                            param_idx = find_parameter_info(param_map, node%name)
                            if (param_idx > 0) then
                                should_skip = .true.
                            end if
                        end if
                    end select
                end if
            end if

            if (.not. should_skip) then
                filtered_count = filtered_count + 1
                filtered_indices(filtered_count) = body_indices(i)
            end if
        end do

        ! Generate the rest of the body with filtered indices
        if (filtered_count > 0) then
            code = code // generate_grouped_body(arena, &
                                                 filtered_indices(1:filtered_count), &
                                                 indent)
        end if

        call reorder_import_lines(code)

        deallocate (filtered_indices)
    end function generate_grouped_body_with_params

    ! Generate grouped body with context about executable statements
    function generate_grouped_body_context(arena, body_indices, indent, &
                                           has_exec_before_contains) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: indent
        logical, intent(in) :: has_exec_before_contains
        character(len=:), allocatable :: code

        ! Store context
        context_has_executable_before_contains = has_exec_before_contains

        ! Generate body
        code = generate_grouped_body(arena, body_indices, indent)

        ! Reset context
        context_has_executable_before_contains = .false.
    end function generate_grouped_body_context

    ! Process grouped declarations
    subroutine process_grouped_declarations(arena, body_indices, i, indent_str, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(inout) :: i
        character(len=*), intent(in) :: indent_str
        character(len=:), allocatable, intent(inout) :: code

        type(declaration_node) :: first_node
        character(len=:), allocatable :: var_list, stmt_code
        character(len=64), allocatable :: grouped_names(:)
        integer :: j, group_count, k, m

        select type (node => arena%entries(body_indices(i))%node)
        type is (declaration_node)
            if (is_type_definition_declaration(node)) then
                i = i + 1
                return
            end if
            ! If this is a multi-variable declaration, emit it as-is to preserve
            ! per-variable dimensions/attributes
            if (node%is_multi_declaration) then
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_str // stmt_code // new_line('A')
                i = i + 1
                return
            end if

            first_node = node
            group_count = 1
            allocate (grouped_names(group_count))
            grouped_names(1) = trim(node%var_name)

            ! For arrays or other non-groupable declarations, emit them individually
            if (node%is_array .or. node%is_allocatable .or. node%is_pointer .or. &
                node%is_target .or. node%is_external .or. node%is_parameter .or. &
                node%initializer_index > 0) then
                ! Use the full declaration generator for complex declarations
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_str // stmt_code // new_line('A')
                i = i + 1
                return
            end if

            ! Look ahead for groupable declarations
            j = i + 1
            do while (j <= size(body_indices))
                if (body_indices(j) > 0 .and. body_indices(j) <= arena%size) then
                    if (allocated(arena%entries(body_indices(j))%node)) then
                        select type (next_node => arena%entries(body_indices(j))%node)
                        type is (declaration_node)
                            if (can_group_declarations(first_node, next_node)) then
                                group_count = group_count + 1
                                call append_name(grouped_names, group_count, &
                                                 trim(next_node%var_name))
                                j = j + 1
                            else
                                exit
                            end if
                        class default
                            exit
                        end select
                    else
                        exit
                    end if
                else
                    exit
                end if
            end do

            if (group_count == 1) then
                stmt_code = generate_code_from_arena(arena, body_indices(i))
                code = code // indent_str // stmt_code // new_line('A')
                i = j
            else
                call sort_names(grouped_names, group_count)

                var_list = ""
                do k = 1, group_count
                    if (k > 1) var_list = var_list // ", "
                    var_list = var_list // trim(grouped_names(k))
                end do

                ! Generate grouped declaration
                ! Avoid MERGE for unequal character lengths; build intent manually
                block
                    character(len=:), allocatable :: intent_str
                    if (first_node%has_intent) then
                        intent_str = first_node%intent
                    else
                        intent_str = ""
                    end if
                    stmt_code = generate_grouped_declaration(first_node%type_name, &
                                                             first_node%kind_value, &
                                                             first_node%has_kind, &
                                                             intent_str, &
                                                             var_list, &
                                                             first_node%is_optional, &
                                                             first_node%is_target)
                end block
                code = code // indent_str // stmt_code // new_line('A')
                i = j
            end if
        end select
    contains
        subroutine append_name(names, count, new_name)
            character(len=64), allocatable, intent(inout) :: names(:)
            integer, intent(in) :: count
            character(len=*), intent(in) :: new_name
            character(len=64), allocatable :: tmp(:)

            if (.not. allocated(names)) then
                allocate (names(1))
                names(1) = new_name
            else
                allocate (tmp(count))
                tmp(1:count - 1) = names
                tmp(count) = new_name
                call move_alloc(tmp, names)
            end if
        end subroutine append_name

        subroutine sort_names(names, count)
            character(len=64), allocatable, intent(inout) :: names(:)
            integer, intent(in) :: count
            character(len=64) :: tmp

            if (count <= 1) return

            do k = 1, count - 1
                do m = k + 1, count
                    if (names(m) < names(k)) then
                        tmp = names(k)
                        names(k) = names(m)
                        names(m) = tmp
                    end if
                end do
            end do
        end subroutine sort_names
    end subroutine process_grouped_declarations

    pure logical function is_type_definition_declaration(node) result(is_header)
        type(declaration_node), intent(in) :: node
        character(len=:), allocatable :: normalized

        if (.not. allocated(node%type_name)) then
            is_header = .false.
            return
        end if

        normalized = to_lower(trim(node%type_name))
        if (normalized /= "type") then
            is_header = .false.
            return
        end if

        if (node%is_multi_declaration) then
            is_header = .false.
            return
        end if

        if (node%initializer_index /= 0) then
            is_header = .false.
            return
        end if

        if (node%is_array .or. node%is_allocatable .or. node%is_pointer .or. &
            node%is_target .or. node%is_external .or. node%is_parameter) then
            is_header = .false.
            return
        end if

        if (.not. allocated(node%var_name)) then
            is_header = .false.
            return
        end if

        is_header = (len_trim(node%var_name) > 0)
    end function is_type_definition_declaration

    ! Process grouped parameter declarations
    subroutine process_grouped_parameters(arena, body_indices, i, indent_str, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(inout) :: i
        character(len=*), intent(in) :: indent_str
        character(len=:), allocatable, intent(inout) :: code

        type(parameter_declaration_node) :: first_node
        character(len=:), allocatable :: var_list, stmt_code
        integer :: j

        select type (node => arena%entries(body_indices(i))%node)
        type is (parameter_declaration_node)
            first_node = node
            var_list = trim(node%name)

            ! Look ahead for groupable parameter declarations
            j = i + 1
            do while (j <= size(body_indices))
                if (body_indices(j) > 0 .and. body_indices(j) <= arena%size) then
                    if (allocated(arena%entries(body_indices(j))%node)) then
                        select type (next_node => arena%entries(body_indices(j))%node)
                        type is (parameter_declaration_node)
                            if (can_group_parameters(first_node, next_node)) then
                                var_list = var_list // ", " // trim(next_node%name)
                                j = j + 1
                            else
                                exit
                            end if
                        class default
                            exit
                        end select
                    else
                        exit
                    end if
                else
                    exit
                end if
            end do

            ! Generate grouped parameter declaration
            if (allocated(first_node%type_name)) then
                stmt_code = first_node%type_name
            else
                stmt_code = "real"
            end if
            if (is_character_type_string(stmt_code)) then
                stmt_code = normalize_character_type_param(stmt_code, &
                                                           first_node%has_kind, &
                                                           first_node%kind_value)
            else if (first_node%has_kind .and. first_node%kind_value > 0) then
                stmt_code = stmt_code // "(" // &
                            trim(adjustl(int_to_string(first_node%kind_value))) // ")"
            end if
            if (first_node%intent_type /= INTENT_NONE) then
                stmt_code = stmt_code // ", intent(" // &
                            intent_type_to_string(first_node%intent_type) // ")"
            end if
            if (first_node%is_optional) then
                stmt_code = stmt_code // ", optional"
            end if
            stmt_code = stmt_code // " :: " // var_list
            code = code // indent_str // stmt_code // new_line('A')
            i = j
        end select
    end subroutine process_grouped_parameters

    ! Find parameter information by name
    function find_parameter_info(param_map, var_name) result(param_idx)
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=*), intent(in) :: var_name
        integer :: param_idx
        integer :: i

        param_idx = 0
        do i = 1, size(param_map)
            if (allocated(param_map(i)%name)) then
                if (trim(param_map(i)%name) == trim(var_name)) then
                    param_idx = i
                    return
                end if
            end if
        end do
    end function find_parameter_info

    logical function check_param_indices_for_name(arena, param_indices, var_name) &
        result(found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_indices(:)
        character(len=*), intent(in) :: var_name
        integer :: i

        found = .false.
        do i = 1, size(param_indices)
            if (param_indices(i) > 0 .and. param_indices(i) <= arena%size) then
                if (allocated(arena%entries(param_indices(i))%node)) then
                    select type (param_node => arena%entries(param_indices(i))%node)
                    type is (identifier_node)
                        if (param_node%name == var_name) then
                            found = .true.
                            return
                        end if
                    end select
                end if
            end if
        end do
    end function check_param_indices_for_name

    ! Check if a variable name is a function parameter
    function is_function_parameter(var_name, arena, proc_node) result(is_param)
        character(len=*), intent(in) :: var_name
        type(ast_arena_t), intent(in) :: arena
        class(ast_node), intent(in) :: proc_node
        logical :: is_param

        is_param = .false.

        select type (proc_node)
        type is (function_def_node)
            if (.not. allocated(proc_node%param_indices)) return
            is_param = check_param_indices_for_name(arena, proc_node%param_indices, &
                                                    var_name)
        type is (subroutine_def_node)
            if (.not. allocated(proc_node%param_indices)) return
            is_param = check_param_indices_for_name(arena, proc_node%param_indices, &
                                                    var_name)
        end select
    end function is_function_parameter

    ! Check if a name is in parameter names array
    function is_parameter_name(var_name, param_names) result(is_param)
        character(len=*), intent(in) :: var_name
        character(len=*), intent(in) :: param_names(:)
        logical :: is_param
        integer :: i

        is_param = .false.
        do i = 1, size(param_names)
            if (trim(param_names(i)) == trim(var_name)) then
                is_param = .true.
                return
            end if
        end do
    end function is_parameter_name

    ! Extract character length specification (len or old-style *) from type text
    subroutine extract_character_length(type_str, has_length, length_spec)
        character(len=*), intent(in) :: type_str
        logical, intent(out) :: has_length
        character(len=:), allocatable, intent(out) :: length_spec
        integer :: star_pos, open_paren, close_paren
        integer :: depth, i, last_char
        character(len=:), allocatable :: trimmed_str

        has_length = .false.
        length_spec = ""

        trimmed_str = trim(type_str)

        open_paren = index(trimmed_str, "(")

        star_pos = index(trimmed_str, "*")
        if (star_pos > 0) then
            if (open_paren == 0 .or. star_pos < open_paren) then
                if (star_pos < len_trim(trimmed_str)) then
                    length_spec = trim(trimmed_str(star_pos + 1:))
                    if (len_trim(length_spec) > 0) then
                        has_length = .true.
                        return
                    end if
                end if
            end if
        end if

        if (open_paren > 0) then
            depth = 0
            close_paren = 0
            last_char = len_trim(trimmed_str)
            do i = open_paren + 1, last_char
                select case (trimmed_str(i:i))
                case ("(")
                    depth = depth + 1
                case (")")
                    if (depth == 0) then
                        close_paren = i
                        exit
                    else
                        depth = depth - 1
                    end if
                end select
            end do
            if (close_paren > open_paren + 1) then
                length_spec = trim(trimmed_str(open_paren + 1:close_paren - 1))
                if (len_trim(length_spec) > 0) has_length = .true.
            end if
        end if
    end subroutine extract_character_length

    ! Normalize character declarations to consistently emit LEN specifications
    function normalize_character_type(node, raw_type) result(type_str)
        type(declaration_node), intent(in) :: node
        character(len=*), intent(in) :: raw_type
        character(len=:), allocatable :: type_str
        character(len=:), allocatable :: trimmed
        character(len=:), allocatable :: length_spec
        logical :: has_length
        logical :: needs_post_process

        call preprocess_character_type(raw_type, trimmed, has_length, &
                                       length_spec, needs_post_process, type_str)
        if (.not. needs_post_process) return

        call ensure_character_length_from_node(node, has_length, length_spec)

        call finalize_character_type(has_length, length_spec, type_str)
    end function normalize_character_type

    ! Simpler normalization helper for parameter declarations (no inference data)
    function normalize_character_type_param(raw_type, has_kind, kind_value, &
                                            character_length_expr) &
        result(type_str)
        character(len=*), intent(in) :: raw_type
        logical, intent(in) :: has_kind
        integer, intent(in) :: kind_value
        character(len=*), intent(in), optional :: character_length_expr
        character(len=:), allocatable :: type_str
        character(len=:), allocatable :: trimmed
        character(len=:), allocatable :: length_spec
        logical :: has_length
        logical :: needs_post_process

        call preprocess_character_type(raw_type, trimmed, has_length, &
                                       length_spec, needs_post_process, type_str)
        if (.not. needs_post_process) return

        if (present(character_length_expr)) then
            if (len_trim(character_length_expr) > 0) then
                has_length = .true.
                length_spec = character_length_expr
            end if
        end if

        call ensure_character_length_from_kind(has_kind, kind_value, has_length, &
                                               length_spec)

        call finalize_character_type(has_length, length_spec, type_str)
    end function normalize_character_type_param

    subroutine preprocess_character_type(raw_type, trimmed, has_length, length_spec, &
                                         needs_post_process, type_str)
        character(len=*), intent(in) :: raw_type
        character(len=:), allocatable, intent(out) :: trimmed
        logical, intent(out) :: has_length
        character(len=:), allocatable, intent(out) :: length_spec
        logical, intent(out) :: needs_post_process
        character(len=:), allocatable, intent(out) :: type_str
        integer :: comma_pos
        character(len=:), allocatable :: lowered_trim
        character(len=:), allocatable :: lowered_len

        trimmed = trim(raw_type)
        has_length = .false.
        needs_post_process = .false.

        if (.not. is_character_type_string(trimmed)) then
            type_str = trimmed
            return
        end if

        comma_pos = index(trimmed, ",")
        if (comma_pos > 0) then
            trimmed = trim(trimmed(:comma_pos - 1))
        end if

        lowered_trim = to_lower(trimmed)
        if (index(lowered_trim, "kind=") > 0 .and. index(lowered_trim, &
                                                         "len") == 0) then
            type_str = trimmed
            return
        end if

        call extract_character_length(trimmed, has_length, length_spec)

        if (has_length) then
            lowered_len = to_lower(length_spec)
            if (index(lowered_len, "kind=") > 0 .and. index(lowered_len, &
                                                            "len=") == 0) then
                type_str = trimmed
                return
            end if
        end if

        needs_post_process = .true.
    end subroutine preprocess_character_type

    subroutine ensure_character_length_from_node(node, has_length, length_spec)
        type(declaration_node), intent(in) :: node
        logical, intent(inout) :: has_length
        character(len=:), allocatable, intent(inout) :: length_spec

        if (.not. has_length) then
            if (node%has_kind) then
                if (node%kind_value > 0) then
                    length_spec = trim(adjustl(int_to_string(node%kind_value)))
                    has_length = .true.
                else if (node%kind_value == -1) then
                    length_spec = "*"
                    has_length = .true.
                end if
            end if
        end if

        if (.not. has_length) then
            if (node%inferred_type%kind == TCHAR) then
                if (node%inferred_type%alloc_info%needs_allocatable_string) then
                    length_spec = ":"
                    has_length = .true.
                else if (node%inferred_type%size > 0) then
                    length_spec = trim(adjustl(int_to_string(node%inferred_type%size)))
                    has_length = .true.
                else if (node%inferred_type%size == -1) then
                    length_spec = "*"
                    has_length = .true.
                end if
            end if
        end if
    end subroutine ensure_character_length_from_node

    subroutine ensure_character_length_from_kind(has_kind, kind_value, has_length, &
                                                 length_spec)
        logical, intent(in) :: has_kind
        integer, intent(in) :: kind_value
        logical, intent(inout) :: has_length
        character(len=:), allocatable, intent(inout) :: length_spec

        if (.not. has_length) then
            if (has_kind) then
                if (kind_value > 0) then
                    length_spec = trim(adjustl(int_to_string(kind_value)))
                    has_length = .true.
                else if (kind_value == -1) then
                    length_spec = "*"
                    has_length = .true.
                end if
            end if
        end if
    end subroutine ensure_character_length_from_kind

    subroutine finalize_character_type(has_length, length_spec, type_str)
        logical, intent(inout) :: has_length
        character(len=:), allocatable, intent(inout) :: length_spec
        character(len=:), allocatable, intent(out) :: type_str
        character(len=:), allocatable :: lowered_len

        if (has_length) then
            if (.not. allocated(length_spec)) then
                has_length = .false.
            else if (len_trim(length_spec) == 0) then
                has_length = .false.
            end if
        end if

        if (has_length) then
            lowered_len = to_lower(trim(length_spec))
            select case (trim(lowered_len))
            case ("-1")
                length_spec = "*"
            case ("len=-1")
                length_spec = "len=*"
            end select
        end if

        if (.not. has_length) then
            type_str = "character"
        else
            lowered_len = to_lower(length_spec)
            if (index(lowered_len, "len=") == 0) then
                length_spec = "len=" // trim(length_spec)
            end if
            type_str = "character(" // trim(length_spec) // ")"
        end if
    end subroutine finalize_character_type

    subroutine reorder_import_lines(text)
        character(len=:), allocatable, intent(inout) :: text
        type(string_t), allocatable :: lines(:)
        type(string_t), allocatable :: imports(:)
        type(string_t), allocatable :: others(:)
        integer :: total_lines
        integer :: import_count
        integer :: other_count
        logical :: has_trailing_newline

        if (.not. allocated(text)) return
        if (len(text) == 0) return

        call split_text_lines(text, lines, total_lines, has_trailing_newline)
        if (total_lines == 0) return

        call partition_import_lines(lines, total_lines, imports, import_count, &
                                    others, other_count)
        if (import_count == 0) return

        call rebuild_lines_with_imports(text, imports, import_count, others, &
                                        other_count, has_trailing_newline)
    end subroutine reorder_import_lines

    subroutine split_text_lines(text, lines, total_lines, has_trailing_newline)
        character(len=:), allocatable, intent(in) :: text
        type(string_t), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: total_lines
        logical, intent(out) :: has_trailing_newline
        integer :: len_text
        integer :: idx_line
        integer :: start_pos
        integer :: line_idx
        character(len=1) :: nl

        nl = new_line('A')
        total_lines = 0
        has_trailing_newline = .false.
        len_text = len(text)
        if (len_text == 0) return

        has_trailing_newline = (text(len_text:len_text) == nl)
        start_pos = 1
        do idx_line = 1, len_text
            if (text(idx_line:idx_line) == nl) then
                total_lines = total_lines + 1
                start_pos = idx_line + 1
            end if
        end do
        if (start_pos <= len_text) total_lines = total_lines + 1
        if (total_lines == 0) return

        allocate (lines(total_lines))
        start_pos = 1
        line_idx = 0
        do idx_line = 1, len_text
            if (text(idx_line:idx_line) == nl) then
                line_idx = line_idx + 1
                call assign_slice(lines(line_idx), text, start_pos, idx_line - 1)
                start_pos = idx_line + 1
            end if
        end do
        if (start_pos <= len_text) then
            line_idx = line_idx + 1
            call assign_slice(lines(line_idx), text, start_pos, len_text)
        end if
    end subroutine split_text_lines

    subroutine partition_import_lines(lines, total_lines, imports, import_count, &
                                      others, other_count)
        type(string_t), intent(in) :: lines(:)
        integer, intent(in) :: total_lines
        type(string_t), allocatable, intent(out) :: imports(:)
        type(string_t), allocatable, intent(out) :: others(:)
        integer, intent(out) :: import_count
        integer, intent(out) :: other_count
        integer :: line_idx
        character(len=:), allocatable :: line_text
        character(len=:), allocatable :: trimmed

        allocate (imports(total_lines))
        allocate (others(total_lines))
        import_count = 0
        other_count = 0

        do line_idx = 1, total_lines
            if (allocated(lines(line_idx)%s)) then
                line_text = lines(line_idx)%s
            else
                line_text = ""
            end if
            trimmed = adjustl(line_text)
            if (len_trim(trimmed) == 0) then
                other_count = other_count + 1
                others(other_count) = lines(line_idx)
            else
                trimmed = to_lower(trim(trimmed))
                if (is_import_statement_line(trimmed)) then
                    import_count = import_count + 1
                    imports(import_count) = lines(line_idx)
                    cycle
                end if
                other_count = other_count + 1
                others(other_count) = lines(line_idx)
            end if
        end do
    end subroutine partition_import_lines

    logical function is_import_statement_line(text) result(is_import)
        character(len=*), intent(in) :: text
        integer :: len_line
        character(len=1) :: next_char

        is_import = .false.
        len_line = len(text)
        if (len_line < 6) return
        if (text(1:6) /= "import") return
        if (len_line == 6) then
            is_import = .true.
            return
        end if

        next_char = text(7:7)
        select case (next_char)
        case (" ", achar(9), ",")
            is_import = .true.
        case (":")
            if (len_line >= 8) then
                if (text(8:8) == ":") is_import = .true.
            end if
        end select
    end function is_import_statement_line

    subroutine rebuild_lines_with_imports(text, imports, import_count, others, &
                                          other_count, has_trailing_newline)
        character(len=:), allocatable, intent(inout) :: text
        type(string_t), intent(in) :: imports(:)
        type(string_t), intent(in) :: others(:)
        integer, intent(in) :: import_count
        integer, intent(in) :: other_count
        logical, intent(in) :: has_trailing_newline
        integer :: line_idx
        integer :: total
        character(len=:), allocatable :: line_text
        character(len=1) :: nl

        nl = new_line('A')
        total = import_count + other_count
        text = ""
        do line_idx = 1, total
            if (line_idx <= import_count) then
                if (allocated(imports(line_idx)%s)) then
                    line_text = imports(line_idx)%s
                else
                    line_text = ""
                end if
            else
                if (allocated(others(line_idx - import_count)%s)) then
                    line_text = others(line_idx - import_count)%s
                else
                    line_text = ""
                end if
            end if
            text = text // line_text
            if (line_idx < total) text = text // nl
        end do
        if (has_trailing_newline) text = text // nl
    end subroutine rebuild_lines_with_imports

    subroutine assign_slice(dest, source, start_pos, end_pos)
        type(string_t), intent(out) :: dest
        character(len=:), allocatable, intent(in) :: source
        integer, intent(in) :: start_pos
        integer, intent(in) :: end_pos

        if (end_pos >= start_pos) then
            dest = source(start_pos:end_pos)
        else
            dest = ""
        end if
    end subroutine assign_slice

end module codegen_utilities
