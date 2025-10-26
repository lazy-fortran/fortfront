module codegen_declaration_grouping
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use codegen_character_normalization, only: normalize_character_type, &
                                               normalize_character_type_param
    use codegen_parameter_info, only: parameter_info_t, find_parameter_info
    use string_utils_mod, only: int_to_string, to_lower
    use type_string_utils, only: is_character_type_string
    implicit none
    private

    public :: can_group_declarations
    public :: can_group_declarations_with_params
    public :: can_group_parameters
    public :: build_param_name_with_dims
    public :: generate_grouped_declaration
    public :: is_type_definition_declaration

contains

    pure logical function declarations_share_basic_flags(node1, node2) result(match)
        type(declaration_node), intent(in) :: node1
        type(declaration_node), intent(in) :: node2

        if (node1%initializer_index > 0) then
            match = .false.
            return
        end if
        if (node2%initializer_index > 0) then
            match = .false.
            return
        end if
        if (node1%is_array .or. node2%is_array) then
            match = .false.
            return
        end if
        if (node1%is_allocatable .neqv. node2%is_allocatable) then
            match = .false.
            return
        end if
        if (node1%is_pointer .neqv. node2%is_pointer) then
            match = .false.
            return
        end if
        if (node1%is_target .neqv. node2%is_target) then
            match = .false.
            return
        end if
        if (node1%is_external .neqv. node2%is_external) then
            match = .false.
            return
        end if
        if (node1%is_parameter .neqv. node2%is_parameter) then
            match = .false.
            return
        end if

        match = .true.
    end function declarations_share_basic_flags

    pure logical function declarations_have_matching_types(node1, node2) result(match)
        type(declaration_node), intent(in) :: node1
        type(declaration_node), intent(in) :: node2
        logical :: both_have_names

        both_have_names = len_trim(node1%type_name) > 0 .and. &
                          len_trim(node2%type_name) > 0
        if (both_have_names) then
            match = trim(node1%type_name) == trim(node2%type_name)
            return
        end if

        if (node1%inferred_type%kind > 0 .and. node2%inferred_type%kind > 0) then
            match = node1%inferred_type%kind == node2%inferred_type%kind
        else
            match = .false.
        end if
    end function declarations_have_matching_types

    pure logical function declarations_match_attributes(node1, node2) result(match)
        type(declaration_node), intent(in) :: node1
        type(declaration_node), intent(in) :: node2
        logical :: intents_match

        if (node1%kind_value /= node2%kind_value) then
            match = .false.
            return
        end if
        if (node1%has_kind .neqv. node2%has_kind) then
            match = .false.
            return
        end if

        if (node1%has_intent .and. node2%has_intent) then
            intents_match = trim(node1%intent) == trim(node2%intent)
        else
            intents_match = (.not. node1%has_intent) .and. (.not. node2%has_intent)
        end if
        if (.not. intents_match) then
            match = .false.
            return
        end if

        if (node1%is_optional .neqv. node2%is_optional) then
            match = .false.
            return
        end if
        if (node1%is_target .neqv. node2%is_target) then
            match = .false.
            return
        end if

        match = .true.
    end function declarations_match_attributes

    function can_group_declarations(node1, node2) result(can_group)
        type(declaration_node), intent(in) :: node1
        type(declaration_node), intent(in) :: node2
        logical :: can_group
        logical :: types_match

        if (.not. declarations_share_basic_flags(node1, node2)) then
            can_group = .false.
            return
        end if

        types_match = declarations_have_matching_types(node1, node2)
        if (.not. types_match) then
            can_group = .false.
            return
        end if

        can_group = declarations_match_attributes(node1, node2)
    end function can_group_declarations

    function can_group_parameters(node1, node2) result(can_group)
        type(parameter_declaration_node), intent(in) :: node1
        type(parameter_declaration_node), intent(in) :: node2
        logical :: can_group

        can_group = .true.
        if (allocated(node1%type_name) .and. allocated(node2%type_name)) then
            can_group = (trim(node1%type_name) == trim(node2%type_name))
        end if
        can_group = can_group .and. &
                    (node1%intent_type == node2%intent_type) .and. &
                    (node1%is_optional .eqv. node2%is_optional) .and. &
                    (node1%is_target .eqv. node2%is_target)
    end function can_group_parameters

    subroutine resolve_parameter_metadata(node, param_map, intent_text, &
                                          optional_flag, &
                                          target_flag)
        type(declaration_node), intent(in) :: node
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=:), allocatable, intent(out) :: intent_text
        logical, intent(out) :: optional_flag
        logical, intent(out) :: target_flag
        integer :: idx

        idx = find_parameter_info(param_map, node%var_name)
        if (idx > 0) then
            intent_text = param_map(idx)%intent_str
            optional_flag = param_map(idx)%is_optional
            target_flag = param_map(idx)%is_target
            return
        end if

        if (node%has_intent) then
            intent_text = node%intent
        else
            intent_text = ""
        end if
        optional_flag = node%is_optional
        target_flag = node%is_target
    end subroutine resolve_parameter_metadata

    function can_group_declarations_with_params(node1, node2, param_map) &
        result(can_group)
        type(declaration_node), intent(in) :: node1
        type(declaration_node), intent(in) :: node2
        type(parameter_info_t), intent(in) :: param_map(:)
        logical :: can_group
        character(len=:), allocatable :: intent1
        character(len=:), allocatable :: intent2
        logical :: optional1
        logical :: optional2
        logical :: target1
        logical :: target2

        if (.not. declarations_share_basic_flags(node1, node2)) then
            can_group = .false.
            return
        end if

        if (trim(node1%type_name) /= trim(node2%type_name)) then
            can_group = .false.
            return
        end if
        if (node1%kind_value /= node2%kind_value) then
            can_group = .false.
            return
        end if
        if (node1%has_kind .neqv. node2%has_kind) then
            can_group = .false.
            return
        end if

        call resolve_parameter_metadata(node1, param_map, intent1, optional1, target1)
        call resolve_parameter_metadata(node2, param_map, intent2, optional2, target2)

        can_group = trim(intent1) == trim(intent2) .and. &
                    optional1 .eqv. optional2 .and. &
                    target1 .eqv. target2
    end function can_group_declarations_with_params

    function build_param_name_with_dims(arena, param_node) result(name_with_dims)
        type(ast_arena_t), intent(in) :: arena
        type(parameter_declaration_node), intent(in) :: param_node
        character(len=:), allocatable :: name_with_dims

        name_with_dims = param_node%name
    end function build_param_name_with_dims

    function generate_grouped_declaration(type_name, kind_value, has_kind, intent, &
                                          var_list, is_optional, is_target) &
        result(stmt)
        character(len=*), intent(in) :: type_name
        integer, intent(in) :: kind_value
        logical, intent(in) :: has_kind
        character(len=*), intent(in) :: intent
        character(len=*), intent(in) :: var_list
        logical, intent(in), optional :: is_optional
        logical, intent(in), optional :: is_target
        character(len=:), allocatable :: stmt
        logical :: opt_flag
        logical :: target_flag
        character(len=:), allocatable :: normalized

        opt_flag = .false.
        if (present(is_optional)) opt_flag = is_optional

        target_flag = .false.
        if (present(is_target)) target_flag = is_target

        stmt = type_name
        if (is_character_type_string(stmt)) then
            normalized = normalize_character_type_param(stmt, has_kind, kind_value)
            stmt = normalized
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

end module codegen_declaration_grouping
