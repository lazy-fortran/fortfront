module codegen_declarations_core
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
                              intent_type_to_string, derived_type_node
    use string_utils_mod, only: int_to_string, to_lower
    use type_system_unified, only: TINT, TREAL, TCHAR, TLOGICAL, TCOMPLEX, &
                                   TDOUBLE, TDERIVED
    use codegen_utilities, only: is_character_type_string, &
                                 normalize_character_type, &
                                 normalize_character_type_param
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_type_utils, only: get_type_standardization
    use declaration_attribute_utils, only: declaration_attribute_info_t, &
                                           reset_declaration_attributes, &
                                           set_declaration_intent, &
                                           append_declaration_attributes
    implicit none
    private
    public :: generate_code_declaration
    public :: generate_code_parameter_declaration
    public :: generate_code_derived_type
    public :: fix_character_len_placeholder

contains

    function generate_code_declaration(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        logical :: standardize_types_enabled
        logical :: has_dimension_attr
        type(declaration_attribute_info_t) :: attr_info

        call get_type_standardization(standardize_types_enabled)
        code = resolve_declaration_type(node, standardize_types_enabled)
        has_dimension_attr = index(to_lower(trim(code)), "dimension(") > 0

        code = fix_character_len_placeholder(code)
        code = apply_kind_modifier(node, code)

        call populate_declaration_attributes(node, attr_info)
        call append_declaration_attributes(code, attr_info)

        code = code // " :: " // build_declaration_entity_list(arena, node, &
                                                               has_dimension_attr)
        code = code // build_declaration_initializer(arena, node)
        code = fix_character_len_placeholder(code)
    end function generate_code_declaration

    function resolve_declaration_type(node, standardize_types_enabled) &
        result(type_str)
        type(declaration_node), intent(in) :: node
        logical, intent(in) :: standardize_types_enabled
        character(len=:), allocatable :: type_str
        logical :: treat_as_character

        type_str = select_declared_type(node, standardize_types_enabled)

        treat_as_character = is_character_type_string(type_str) .or. &
                             node%inferred_type%kind == TCHAR
        if (.not. treat_as_character) return

        type_str = normalize_character_type(node, type_str)
        type_str = normalize_character_length(node, type_str)
    end function resolve_declaration_type

    function select_declared_type(node, standardize_types_enabled) result(type_str)
        type(declaration_node), intent(in) :: node
        logical, intent(in) :: standardize_types_enabled
        character(len=:), allocatable :: type_str

        if (len_trim(node%type_name) > 0) then
            type_str = node%type_name
            return
        end if

        if (node%inferred_type%kind <= 0) then
            type_str = "real"
            return
        end if

        select case (node%inferred_type%kind)
        case (TINT)
            type_str = "integer"
        case (TREAL)
            if (standardize_types_enabled) then
                type_str = "real(8)"
            else
                type_str = "real"
            end if
        case (TCHAR)
            type_str = select_character_type(node)
        case (TLOGICAL)
            type_str = "logical"
        case (TCOMPLEX)
            type_str = "complex"
        case (TDOUBLE)
            type_str = "double precision"
        case (TDERIVED)
            type_str = merge(node%type_name, "type(unknown_t)", &
                             len_trim(node%type_name) > 0)
        case default
            type_str = "real"
        end select
    end function select_declared_type

    function select_character_type(node) result(type_str)
        type(declaration_node), intent(in) :: node
        character(len=:), allocatable :: type_str

        if (node%inferred_type%alloc_info%needs_allocatable_string) then
            type_str = "character(len=:)"
        else if (node%inferred_type%size > 0) then
            type_str = "character(len=" // &
                trim(adjustl(int_to_string(node%inferred_type%size))) // ")"
        else
            type_str = "character(len=0)"
        end if
    end function select_character_type

    function normalize_character_length(node, type_str) result(adjusted)
        type(declaration_node), intent(in) :: node
        character(len=*), intent(in) :: type_str
        character(len=:), allocatable :: adjusted
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: kind_text

        adjusted = type_str

        select case (trim(adjusted))
        case ("character(len=))", "character(len=)")
            adjusted = "character(len=*)"
        end select

        if (.not. is_character_type_string(adjusted)) return

        lowered = to_lower(trim(adjusted))
        if (index(lowered, "len=)") == 0) return
        if (.not. node%has_kind) return

        select case (node%kind_value)
        case (-1)
            adjusted = "character(len=*)"
        case default
            if (node%kind_value > 0) then
                kind_text = trim(adjustl(int_to_string(node%kind_value)))
                adjusted = "character(len=" // kind_text // ")"
            end if
        end select
    end function normalize_character_length

    function apply_kind_modifier(node, type_code) result(result_type)
        type(declaration_node), intent(in) :: node
        character(len=*), intent(in) :: type_code
        character(len=:), allocatable :: result_type

        result_type = type_code
        if (.not. node%has_kind) return
        if (node%kind_value <= 0) return
        if (is_character_type_string(result_type)) return

        result_type = result_type // "(" // &
                      trim(adjustl(int_to_string(node%kind_value))) // ")"
    end function apply_kind_modifier

    subroutine populate_declaration_attributes(node, attr_info)
        type(declaration_node), intent(in) :: node
        type(declaration_attribute_info_t), intent(out) :: attr_info

        call reset_declaration_attributes(attr_info)
        if (node%has_intent .and. allocated(node%intent)) then
            call set_declaration_intent(attr_info, node%intent)
        end if
        attr_info%is_allocatable = node%is_allocatable
        if (.not. attr_info%is_allocatable) then
            if (node%inferred_type%kind > 0) then
                if (node%inferred_type%alloc_info%needs_allocatable_string) then
                    attr_info%is_allocatable = .true.
                end if
            end if
        end if
        attr_info%is_optional = node%is_optional
        attr_info%is_pointer = node%is_pointer
        attr_info%is_target = node%is_target
        attr_info%is_external = node%is_external
        attr_info%is_parameter = node%is_parameter
        attr_info%is_save = node%is_save
    end subroutine populate_declaration_attributes

    function build_declaration_entity_list(arena, node, has_dimension_attr) &
        result(entities)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: node
        logical, intent(in) :: has_dimension_attr
        character(len=:), allocatable :: entities
        integer :: i

        entities = ""
        if (node%is_multi_declaration .and. allocated(node%var_names)) then
            do i = 1, size(node%var_names)
                if (i > 1) entities = entities // ", "
                entities = entities // trim(node%var_names(i))
                if (node%is_array .and. allocated(node%dimension_indices)) then
                    if (.not. has_dimension_attr) then
                        entities = trim(entities) // build_dimension_clause( &
                                   arena, node%dimension_indices)
                    end if
                end if
            end do
        else
            entities = node%var_name
            if (node%is_array .and. allocated(node%dimension_indices)) then
                if (.not. has_dimension_attr) then
                    entities = trim(entities) // build_dimension_clause( &
                               arena, node%dimension_indices)
                end if
            end if
        end if
    end function build_declaration_entity_list

    function build_dimension_clause(arena, dimension_indices) result(clause)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: dimension_indices(:)
        character(len=:), allocatable :: clause
        integer :: i
        integer :: dim_index

        if (size(dimension_indices) == 0) then
            clause = ""
            return
        end if

        clause = "("
        do i = 1, size(dimension_indices)
            if (i > 1) clause = clause // ","
            dim_index = dimension_indices(i)
            if (dim_index > 0 .and. dim_index <= arena%size) then
                clause = clause // generate_code_from_arena(arena, dim_index)
            else if (dim_index > arena%size) then
                clause = clause // int_to_string(dim_index)
            else
                clause = clause // ":"
            end if
        end do
        clause = clause // ")"
    end function build_dimension_clause

    function build_declaration_initializer(arena, node) result(initializer)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_node), intent(in) :: node
        character(len=:), allocatable :: initializer
        character(len=:), allocatable :: init_code

        initializer = ""

        if (node%initializer_index <= 0) return
        if (node%initializer_index > arena%size) return
        if (.not. allocated(arena%entries(node%initializer_index)%node)) return

        init_code = generate_code_from_arena(arena, node%initializer_index)

        if (node%is_pointer) then
            if (to_lower(trim(init_code)) == "null") init_code = "null()"
            initializer = " => " // init_code
        else
            initializer = " = " // init_code
        end if
    end function build_declaration_initializer

    function generate_code_parameter_declaration(arena, node, node_index) &
        result(code)
        type(ast_arena_t), intent(in) :: arena
        type(parameter_declaration_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        if (parameter_requires_full_declaration(node)) then
            code = build_parameter_declaration(arena, node)
        else
            code = node%name
        end if

        code = fix_character_len_placeholder(code)
    end function generate_code_parameter_declaration

    logical function parameter_requires_full_declaration(node) result(required)
        type(parameter_declaration_node), intent(in) :: node

        required = len_trim(node%type_name) > 0
    end function parameter_requires_full_declaration

    function build_parameter_declaration(arena, node) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(parameter_declaration_node), intent(in) :: node
        character(len=:), allocatable :: decl_code
        character(len=:), allocatable :: type_code
        type(declaration_attribute_info_t) :: attr_info

        type_code = format_parameter_type(node)

        call populate_parameter_attributes(node, attr_info)
        call append_declaration_attributes(type_code, attr_info)

        decl_code = type_code // " :: " // node%name
        decl_code = decl_code // build_parameter_dimensions(arena, node)
    end function build_parameter_declaration

    function format_parameter_type(node) result(type_code)
        type(parameter_declaration_node), intent(in) :: node
        character(len=:), allocatable :: type_code

        type_code = node%type_name

        if (is_character_type_string(type_code)) then
            type_code = normalize_character_type_param(type_code, node%has_kind, &
                                                       node%kind_value)
            return
        end if

        if (node%has_kind .and. node%kind_value > 0) then
            type_code = type_code // "(" // &
                trim(adjustl(int_to_string(node%kind_value))) // ")"
        end if
    end function format_parameter_type

    subroutine populate_parameter_attributes(node, attr_info)
        type(parameter_declaration_node), intent(in) :: node
        type(declaration_attribute_info_t), intent(out) :: attr_info
        character(len=:), allocatable :: intent_str

        call reset_declaration_attributes(attr_info)

        intent_str = intent_type_to_string(node%intent_type)
        if (len_trim(intent_str) > 0) then
            call set_declaration_intent(attr_info, intent_str)
        end if

        attr_info%is_optional = node%is_optional
    end subroutine populate_parameter_attributes

    function build_parameter_dimensions(arena, node) result(dim_clause)
        type(ast_arena_t), intent(in) :: arena
        type(parameter_declaration_node), intent(in) :: node
        character(len=:), allocatable :: dim_clause
        integer :: j
        integer :: dim_index

        dim_clause = ""

        if (.not. allocated(node%dimension_indices)) return
        if (size(node%dimension_indices) == 0) return

        dim_clause = "("
        do j = 1, size(node%dimension_indices)
            if (j > 1) dim_clause = dim_clause // ", "
            dim_index = node%dimension_indices(j)
            dim_clause = dim_clause // generate_code_from_arena(arena, dim_index)
        end do
        dim_clause = dim_clause // ")"
    end function build_parameter_dimensions

    function generate_code_derived_type(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(derived_type_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = build_derived_type_header(node)
        code = code // collect_derived_components(arena, node)
        code = code // "end type " // node%name
    end function generate_code_derived_type

    function build_derived_type_header(node) result(header)
        type(derived_type_node), intent(in) :: node
        character(len=:), allocatable :: header
        character(len=:), allocatable :: clause

        clause = derived_type_attribute_clause(node)
        if (len_trim(clause) == 0) then
            header = "type :: " // node%name // new_line('A')
        else if (clause(1:1) == ",") then
            header = "type" // clause // " :: " // node%name // new_line('A')
        else
            header = "type " // trim(clause) // " :: " // node%name // new_line('A')
        end if
    end function build_derived_type_header

    function derived_type_attribute_clause(node) result(clause)
        type(derived_type_node), intent(in) :: node
        character(len=:), allocatable :: clause
        integer :: i
        integer :: trimmed_length

        clause = ""
        if (.not. node%has_attributes) return
        if (.not. allocated(node%attribute_clause)) return

        trimmed_length = len_trim(node%attribute_clause)
        if (trimmed_length == 0) return

        clause = ""
        do i = 1, trimmed_length
            clause = clause // node%attribute_clause(i:i)
            if (node%attribute_clause(i:i) == "," .and. i < trimmed_length) then
                if (node%attribute_clause(i + 1:i + 1) /= " " .and. &
                    node%attribute_clause(i + 1:i + 1) /= new_line('A')) then
                    clause = clause // " "
                end if
            end if
        end do
    end function derived_type_attribute_clause

    function collect_derived_components(arena, node) result(component_block)
        type(ast_arena_t), intent(in) :: arena
        type(derived_type_node), intent(in) :: node
        character(len=:), allocatable :: component_block
        character(len=:), allocatable :: component_code
        integer :: i
        integer :: component_index

        component_block = ""
        if (.not. allocated(node%component_indices)) return

        do i = 1, size(node%component_indices)
            component_index = node%component_indices(i)
            if (component_index <= 0 .or. component_index > arena%size) cycle
            if (.not. allocated(arena%entries(component_index)%node)) cycle

            select type (child => arena%entries(component_index)%node)
            type is (derived_type_node)
                cycle
            class default
                component_code = generate_code_from_arena(arena, component_index)
            end select

            if (len_trim(component_code) == 0) cycle
            component_block = component_block // "    " // component_code // &
                              new_line('A')
        end do
    end function collect_derived_components

    pure function fix_character_len_placeholder(text) result(out)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: pos

        out = text

        pos = index(out, "len=))")
        do while (pos > 0)
            out = out(:pos - 1) // "len=*" // out(pos + 5:)
            pos = index(out, "len=))")
        end do

        pos = index(out, "len=)")
        do while (pos > 0)
            out = out(:pos - 1) // "len=*" // out(pos + 4:)
            pos = index(out, "len=)")
        end do
    end function fix_character_len_placeholder

end module codegen_declarations_core
