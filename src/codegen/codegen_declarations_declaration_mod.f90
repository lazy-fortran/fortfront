module codegen_declarations_declaration_mod
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
                              derived_type_node, intent_type_to_string, module_node
    use ast_nodes_misc, only: contains_node, comment_node, blank_line_node, &
                              implicit_statement_node, interface_block_node, &
                              module_procedure_node, use_statement_node
    use ast_nodes_core, only: literal_node
    use string_utils_mod, only: int_to_string, to_lower
    use type_system_unified
    use codegen_utilities, only: generate_grouped_body, is_character_type_string, &
                                 normalize_character_type, normalize_character_type_param
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_type_utils, only: get_type_standardization
    use declaration_attribute_utils, only: declaration_attribute_info_t, &
                                           reset_declaration_attributes, &
                                           set_declaration_intent, &
                                           append_declaration_attributes
    use codegen_declarations_shared_mod, only: fix_character_len_placeholder
    implicit none
    private
    public :: generate_code_declaration
    public :: generate_code_parameter_declaration
    public :: generate_code_module
    public :: generate_code_interface_block
    public :: generate_code_module_procedure
    public :: generate_code_derived_type

contains

    ! Generate code for declarations
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

    function resolve_declaration_type(node, standardize_types_enabled) result(type_str)
        type(declaration_node), intent(in) :: node
        logical, intent(in) :: standardize_types_enabled
        character(len=:), allocatable :: type_str

        if (len_trim(node%type_name) > 0) then
            type_str = node%type_name
        else if (node%inferred_type%kind > 0) then
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
                if (node%inferred_type%alloc_info%needs_allocatable_string) then
                    type_str = "character(len=:)"
                else if (node%inferred_type%size > 0) then
                    type_str = "character(len=" // &
                        trim(adjustl(int_to_string(node%inferred_type%size))) // ")"
                else
                    type_str = "character(len=0)"
                end if
            case (TLOGICAL)
                type_str = "logical"
            case (TCOMPLEX)
                type_str = "complex"
            case (TDOUBLE)
                type_str = "double precision"
            case (TDERIVED)
                if (len_trim(node%type_name) > 0) then
                    type_str = node%type_name
                else
                    type_str = "type(unknown_t)"
                end if
            case default
                type_str = "real"
            end select
        else
            type_str = "real"
        end if

        if (is_character_type_string(type_str) .or. node%inferred_type%kind == &
            TCHAR) then
            type_str = normalize_character_type(node, type_str)
        end if

        select case (trim(type_str))
        case ("character(len=))", "character(len=)")
            type_str = "character(len=*)"
        end select

        if (.not. is_character_type_string(type_str)) return

        if (index(to_lower(trim(type_str)), "len=)") > 0) then
            if (node%has_kind) then
                select case (node%kind_value)
                case (-1)
                    type_str = "character(len=*)"
                case default
                    if (node%kind_value > 0) then
                        type_str = "character(len=" // &
                            trim(adjustl(int_to_string(node%kind_value))) // ")"
                    end if
                end select
            end if
        end if
    end function resolve_declaration_type

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
                        entities = trim(entities) // &
                                   build_dimension_clause(arena, node%dimension_indices)
                    end if
                end if
            end do
        else
            entities = node%var_name
            if (node%is_array .and. allocated(node%dimension_indices)) then
                if (.not. has_dimension_attr) then
                    entities = trim(entities) // &
                               build_dimension_clause(arena, node%dimension_indices)
                end if
            end if
        end if
    end function build_declaration_entity_list

    function build_dimension_clause(arena, dimension_indices) result(clause)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: dimension_indices(:)
        character(len=:), allocatable :: clause
        integer :: i, dim_index

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

    ! Generate code for parameter declarations
    function generate_code_parameter_declaration(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(parameter_declaration_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: intent_str
        integer :: j
        type(declaration_attribute_info_t) :: attr_info

        ! Check if this node has a parent that needs just the name (parameter list)
        ! vs full declaration (in body). For now, generate full declaration when
        ! the node has type and attributes.
        if (len_trim(node%type_name) > 0) then
            ! Generate full declaration (when in body)
            code = node%type_name

            if (is_character_type_string(code)) then
                code = normalize_character_type_param(code, node%has_kind, &
                                                      node%kind_value)
            else if (node%has_kind .and. node%kind_value > 0) then
                code = code // "(" // &
                       trim(adjustl(int_to_string(node%kind_value))) // ")"
            end if

            intent_str = intent_type_to_string(node%intent_type)
            call reset_declaration_attributes(attr_info)
            if (len_trim(intent_str) > 0) then
                call set_declaration_intent(attr_info, intent_str)
            end if
            attr_info%is_optional = node%is_optional
            call append_declaration_attributes(code, attr_info)

            code = code // " :: " // node%name

            ! Add dimensions if present
            if (allocated(node%dimension_indices) .and. &
                size(node%dimension_indices) > 0) then
                code = code // "("
                do j = 1, size(node%dimension_indices)
                    if (j > 1) code = code // ", "
                    code = code // generate_code_from_arena(arena, &
                                                            node%dimension_indices(j))
                end do
                code = code // ")"
            end if
        else
            ! Just emit the name (when in parameter list)
            code = node%name
        end if

        code = fix_character_len_placeholder(code)
    end function generate_code_parameter_declaration

    ! Generate code for modules
    function generate_code_module(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: body_code
        integer :: i
        logical :: has_implicit

        ! Module header
        code = "module " // node%name // new_line('A')

        ! Ensure module includes implicit none (quality requirement for lazy Fortran)
        has_implicit = .false.
        if (allocated(node%declaration_indices)) then
            do i = 1, size(node%declaration_indices)
                if (node%declaration_indices(i) > 0 .and. &
                    node%declaration_indices(i) <= &
                    arena%size) then
                    if (allocated(arena%entries(node%declaration_indices(i))%node)) then
                        select type (decl => &
                                     arena%entries(node%declaration_indices(i))%node)
                        type is (implicit_statement_node)
                            if (decl%is_none) then
                                has_implicit = .true.
                                exit
                            end if
                        type is (literal_node)
                            if (allocated(decl%value)) then
                                if (index(decl%value, 'implicit none') > 0) then
                                    has_implicit = .true.
                                    exit
                                end if
                            end if
                        end select
                    end if
                end if
            end do
        end if
        if (.not. has_implicit) then
            code = code // "    implicit none" // new_line('A')
        end if

        ! Generate module declarations
        if (allocated(node%declaration_indices)) then
            body_code = generate_grouped_body(arena, node%declaration_indices, 1)
            if (len(body_code) > 0) then
                code = code // body_code
            end if
        end if

        ! Check for contains section
        if (node%has_contains .and. allocated(node%procedure_indices)) then
            code = code // "contains" // new_line('A')

            ! Generate contained procedures
            do i = 1, size(node%procedure_indices)
                if (node%procedure_indices(i) > 0 .and. &
                    node%procedure_indices(i) <= arena%size) then
                    body_code = generate_code_from_arena(arena, &
                                                         node%procedure_indices(i))
                    if (len(body_code) > 0) then
                        ! Add proper indentation for contained procedures
                        code = code // "    " // body_code
                        if (i < size(node%procedure_indices)) then
                            code = code // new_line('A') // new_line('A')
                        else
                            code = code // new_line('A')
                        end if
                    end if
                end if
            end do
        end if

        ! Module end
        code = code // "end module " // node%name
    end function generate_code_module

    function generate_code_interface_block(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(interface_block_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: body_code

        code = "interface"
        if (allocated(node%name)) then
            if (len_trim(node%name) > 0) code = code // " " // trim(node%name)
        end if
        code = code // new_line('A')

        if (allocated(node%procedure_indices)) then
            body_code = generate_grouped_body(arena, node%procedure_indices, 1)
            if (len(body_code) > 0) code = code // body_code
        end if

        code = code // "end interface"
        if (allocated(node%name)) then
            if (len_trim(node%name) > 0) code = code // " " // trim(node%name)
        end if
    end function generate_code_interface_block

    function generate_code_module_procedure(node) result(code)
        type(module_procedure_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i
        character(len=:), allocatable :: name_text
        logical :: first_name

        code = "module procedure"
        first_name = .true.
        if (allocated(node%procedure_names)) then
            do i = 1, size(node%procedure_names)
                if (.not. allocated(node%procedure_names(i)%s)) cycle
                name_text = trim(node%procedure_names(i)%s)
                if (len_trim(name_text) == 0) cycle
                if (first_name) then
                    code = code // " " // name_text
                    first_name = .false.
                else
                    code = code // ", " // name_text
                end if
            end do
        end if
    end function generate_code_module_procedure

    ! Generate code for derived types
    function generate_code_derived_type(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(derived_type_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: component_code
        character(len=:), allocatable :: header_clause
        integer :: i

        ! Type definition header
        if (node%has_attributes .and. allocated(node%attribute_clause) .and. &
            len_trim(node%attribute_clause) > 0) then
            header_clause = ""
            do i = 1, len_trim(node%attribute_clause)
                header_clause = header_clause // node%attribute_clause(i:i)
                if (node%attribute_clause(i:i) == "," .and. i < &
                    len_trim(node%attribute_clause)) then
                    if (node%attribute_clause(i + 1:i + 1) /= " " .and. &
                        node%attribute_clause(i + 1:i + 1) /= new_line('A')) then
                        header_clause = header_clause // " "
                    end if
                end if
            end do

            if (header_clause(1:1) == ",") then
                code = "type" // header_clause // " :: " // node%name // &
                       new_line('A')
            else
                code = "type " // trim(header_clause) // " :: " // node%name // &
                       new_line('A')
            end if
        else
            code = "type :: " // node%name // new_line('A')
        end if

        ! Generate components
        if (allocated(node%component_indices)) then
            do i = 1, size(node%component_indices)
                if (node%component_indices(i) > 0 .and. &
                    node%component_indices(i) <= arena%size) then
                    if (.not. &
                        allocated(arena%entries(node%component_indices(i))%node)) cycle
                    select type (child => &
                                 arena%entries(node%component_indices(i))%node)
                    type is (derived_type_node)
                        cycle
                    class default
                        component_code = generate_code_from_arena( &
                                         arena, node%component_indices(i))
                    end select
                    if (len_trim(component_code) == 0) cycle
                    code = code // "    " // component_code // new_line('A')
                end if
            end do
        end if

        ! Type definition end
        code = code // "end type " // node%name
    end function generate_code_derived_type
end module codegen_declarations_declaration_mod
