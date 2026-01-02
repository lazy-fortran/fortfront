module codegen_procedure_shared
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node, literal_node, program_node
    use ast_nodes_data, only: declaration_node, module_node, &
                              parameter_declaration_node, intent_type_to_string
    use ast_nodes_misc, only: implicit_statement_node
    use codegen_declarations_core, only: build_parameter_dimensions, &
                                         fix_character_len_placeholder
    use codegen_parameter_info, only: parameter_info_t
    use string_utils_mod, only: int_to_string, to_lower
    use type_string_utils, only: mono_type_to_string
    use type_system_unified, only: mono_type_t, TARRAY
    use type_array_safe, only: safe_extract_array_rank
    implicit none
    private
    public :: build_parameter_clause
    public :: derive_parameter_name
    public :: filter_implicit_statements
    public :: maybe_add_procedure_implicit_none
    public :: apply_default_intents
    public :: apply_function_default_intents
    public :: gather_prefix
    public :: copy_indices
    public :: append_parameter_declaration
    public :: get_param_type_from_identifier
    public :: get_param_type_from_param_decl
    public :: get_param_type_fallback
    public :: is_parameter_name
    public :: is_local_var_collected
    public :: ensure_local_var_capacity
    public :: add_declared_vars
    public :: add_single_declared_var

contains

    function build_parameter_clause(arena, param_indices) result(clause)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_indices(:)
        character(len=:), allocatable :: clause
        integer :: i

        if (size(param_indices) == 0) then
            clause = "()"
            return
        end if

        clause = "("
        do i = 1, size(param_indices)
            if (i > 1) clause = clause // ", "
            clause = clause // derive_parameter_name(arena, param_indices(i), i)
        end do
        clause = clause // ")"
    end function build_parameter_clause

    function derive_parameter_name(arena, param_index, fallback_index) result(name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        integer, intent(in) :: fallback_index
        character(len=:), allocatable :: name

        name = "param" // trim(adjustl(int_to_string(fallback_index)))

        if (param_index <= 0 .or. param_index > arena%size) return
        if (.not. allocated(arena%entries(param_index)%node)) return

        select type (param_node => arena%entries(param_index)%node)
        type is (identifier_node)
            name = param_node%name
        type is (parameter_declaration_node)
            name = param_node%name
        type is (declaration_node)
            name = param_node%var_name
        end select
    end function derive_parameter_name

    subroutine filter_implicit_statements(arena, body_indices, filtered_indices)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, allocatable, intent(out) :: filtered_indices(:)
        integer :: j, count
        logical :: is_implicit_none

        count = 0
        do j = 1, size(body_indices)
            is_implicit_none = .false.
            if (body_indices(j) > 0 .and. body_indices(j) <= arena%size) then
                if (allocated(arena%entries(body_indices(j))%node)) then
                    select type (body_node => arena%entries(body_indices(j))%node)
                    type is (implicit_statement_node)
                        is_implicit_none = body_node%is_none
                    end select
                end if
            end if
            if (.not. is_implicit_none) count = count + 1
        end do

        allocate (filtered_indices(count))
        count = 0
        do j = 1, size(body_indices)
            is_implicit_none = .false.
            if (body_indices(j) > 0 .and. body_indices(j) <= arena%size) then
                if (allocated(arena%entries(body_indices(j))%node)) then
                    select type (body_node => arena%entries(body_indices(j))%node)
                    type is (implicit_statement_node)
                        is_implicit_none = body_node%is_none
                    end select
                end if
            end if
            if (.not. is_implicit_none) then
                count = count + 1
                filtered_indices(count) = body_indices(j)
            end if
        end do
    end subroutine filter_implicit_statements

    function maybe_add_procedure_implicit_none(arena, body_indices, &
                                               procedure_index) result(prolog)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: procedure_index
        character(len=:), allocatable :: prolog
        integer :: j
        logical :: has_implicit_none
        logical :: has_other_implicit

        prolog = ""
        has_implicit_none = .false.
        has_other_implicit = .false.

        do j = 1, size(body_indices)
            if (body_indices(j) <= 0 .or. body_indices(j) > arena%size) cycle
            if (.not. allocated(arena%entries(body_indices(j))%node)) cycle
            select type (body_node => arena%entries(body_indices(j))%node)
            type is (implicit_statement_node)
                if (body_node%is_none) then
                    has_implicit_none = .true.
                else
                    has_other_implicit = .true.
                end if
            end select
        end do

        if (has_other_implicit) return
        if (has_implicit_none) return
        ! Per ISO/IEC 1539-1:2018 Section 8.7: each scoping unit needs
        ! its own implicit statement. Do not skip adding implicit none
        ! to internal procedures even if host has implicit none.
        prolog = "    implicit none" // new_line('A')
    end function maybe_add_procedure_implicit_none

    logical function enclosing_has_implicit(arena, procedure_index) &
        result(has_implicit)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: procedure_index
        integer :: parent_index

        has_implicit = .false.
        if (procedure_index <= 0 .or. procedure_index > arena%size) return

        parent_index = arena%entries(procedure_index)%parent_index
        if (parent_index <= 0 .or. parent_index > arena%size) return
        if (.not. allocated(arena%entries(parent_index)%node)) return

        select type (parent => arena%entries(parent_index)%node)
        type is (module_node)
            has_implicit = module_scope_has_implicit(arena, parent)
        type is (program_node)
            has_implicit = program_scope_has_implicit(arena, parent)
        class default
            has_implicit = .false.
        end select
    end function enclosing_has_implicit

    logical function module_scope_has_implicit(arena, mod) result(has_implicit)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: mod
        integer :: i

        has_implicit = .false.
        if (.not. allocated(mod%declaration_indices)) return

        do i = 1, size(mod%declaration_indices)
            if (node_index_has_implicit(arena, mod%declaration_indices(i))) then
                has_implicit = .true.
                return
            end if
        end do
    end function module_scope_has_implicit

    logical function program_scope_has_implicit(arena, prog) result(has_implicit)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        integer :: i

        has_implicit = .false.
        if (.not. allocated(prog%body_indices)) return

        do i = 1, size(prog%body_indices)
            if (node_index_has_implicit(arena, prog%body_indices(i))) then
                has_implicit = .true.
                return
            end if
        end do
    end function program_scope_has_implicit

    logical function node_index_has_implicit(arena, node_index) &
        result(has_implicit)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable :: lowered_text

        has_implicit = .false.
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (stmt => arena%entries(node_index)%node)
        type is (implicit_statement_node)
            has_implicit = .true.
        type is (literal_node)
            if (allocated(stmt%value)) then
                lowered_text = to_lower(adjustl(stmt%value))
                if (index(lowered_text, "implicit") == 1) has_implicit = .true.
            end if
        end select
    end function node_index_has_implicit

    subroutine apply_default_intents(prefix_keywords, param_map)
        character(len=*), intent(in) :: prefix_keywords(:)
        type(parameter_info_t), intent(inout) :: param_map(:)
        integer :: i, j

        do j = 1, size(prefix_keywords)
            select case (trim(prefix_keywords(j)))
            case ("pure", "elemental")
                do i = 1, size(param_map)
                    if (.not. allocated(param_map(i)%name)) cycle
                    if (len_trim(param_map(i)%intent_str) == 0) then
                        param_map(i)%intent_str = "in"
                    end if
                end do
            end select
        end do

    end subroutine apply_default_intents

    subroutine apply_function_default_intents(param_map, arena, body_indices)
        use ast_nodes_data, only: declaration_node
        type(parameter_info_t), intent(inout) :: param_map(:)
        type(ast_arena_t), intent(in), optional :: arena
        integer, intent(in), optional :: body_indices(:)
        integer :: i, j, idx
        logical :: is_procedure_param

        do i = 1, size(param_map)
            if (.not. allocated(param_map(i)%name)) cycle
            if (param_map(i)%is_mutated) cycle
            if (len_trim(param_map(i)%intent_str) == 0) then
                ! Check if this parameter is a procedure type
                ! ISO/IEC 1539-1:2018 Section 15.4.3.2: procedure dummy arguments
                ! cannot have INTENT attribute
                is_procedure_param = .false.
                if (present(arena) .and. present(body_indices)) then
                    do j = 1, size(body_indices)
                        idx = body_indices(j)
                        if (idx <= 0 .or. idx > arena%size) cycle
                        if (.not. allocated(arena%entries(idx)%node)) cycle
                        select type (node => arena%entries(idx)%node)
                        type is (declaration_node)
                            if (trim(node%var_name) == trim(param_map(i)%name)) then
                                if (allocated(node%type_name)) then
                                    if (index(to_lower(trim(node%type_name)), &
                                              'procedure(') == 1) then
                                        is_procedure_param = .true.
                                        exit
                                    end if
                                end if
                            end if
                        end select
                    end do
                end if

                if (.not. is_procedure_param) then
                    param_map(i)%intent_str = "in"
                end if
            end if
        end do
    end subroutine apply_function_default_intents

    function gather_prefix(prefix_keywords, recursive_in_prefix) result(prefix)
        character(len=*), allocatable, intent(in) :: prefix_keywords(:)
        logical, intent(out) :: recursive_in_prefix
        character(len=:), allocatable :: prefix
        integer :: i
        character(len=:), allocatable :: term

        prefix = ""
        recursive_in_prefix = .false.

        if (.not. allocated(prefix_keywords)) return

        do i = 1, size(prefix_keywords)
            term = prefix_keywords(i)
            if (len_trim(term) == 0) cycle
            if (len(prefix) > 0) prefix = prefix // " "
            prefix = prefix // trim(term)
            if (trim(term) == "recursive") recursive_in_prefix = .true.
        end do
    end function gather_prefix

    subroutine copy_indices(source, target)
        integer, allocatable, intent(in) :: source(:)
        integer, allocatable, intent(out) :: target(:)

        if (allocated(source)) then
            allocate (target(size(source)))
            target = source
        else
            allocate (target(0))
        end if
    end subroutine copy_indices

    subroutine append_parameter_declaration(arena, param_idx, param_info, decl_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_idx
        type(parameter_info_t), intent(in) :: param_info
        character(len=:), allocatable, intent(inout) :: decl_code
        character(len=:), allocatable :: param_type
        character(len=:), allocatable :: decl_line
        logical :: has_intent_attr
        logical :: has_optional_attr
        logical :: has_target_attr

        if (len_trim(param_info%name) == 0) return

        call build_param_type_with_attrs(arena, param_idx, param_info, param_type, &
                                         has_intent_attr, has_optional_attr, &
                                         has_target_attr)

        decl_line = "    " // trim(param_type) // " :: " // trim(param_info%name)

        call append_array_dimensions(arena, param_idx, param_type, decl_line)
        call fix_declaration_formatting(decl_line)

        decl_code = decl_code // decl_line // new_line('A')
    end subroutine append_parameter_declaration

    subroutine build_param_type_with_attrs(arena, param_idx, param_info, &
                                           param_type, has_intent_attr, &
                                           has_optional_attr, has_target_attr)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_idx
        type(parameter_info_t), intent(in) :: param_info
        character(len=:), allocatable, intent(out) :: param_type
        logical, intent(out) :: has_intent_attr
        logical, intent(out) :: has_optional_attr
        logical, intent(out) :: has_target_attr
        character(len=:), allocatable :: intent_str

        has_intent_attr = .false.
        has_optional_attr = .false.
        has_target_attr = .false.

        select type (param_node => arena%entries(param_idx)%node)
        type is (identifier_node)
            param_type = get_param_type_from_identifier(param_node)
        type is (parameter_declaration_node)
            param_type = get_param_type_from_param_decl(param_node)
            intent_str = intent_type_to_string(param_node%intent_type)
            if (len_trim(intent_str) > 0) then
                param_type = trim(param_type) // ", intent(" // trim(intent_str) // ")"
                has_intent_attr = .true.
            end if
            if (param_node%is_optional) then
                param_type = trim(param_type) // ", optional"
                has_optional_attr = .true.
            end if
            if (param_node%is_target) then
                param_type = trim(param_type) // ", target"
                has_target_attr = .true.
            end if
        class default
            param_type = get_param_type_fallback(param_node)
        end select

        call apply_param_info_attrs(param_info, param_type, has_intent_attr, &
                                    has_optional_attr, has_target_attr)
    end subroutine build_param_type_with_attrs

    subroutine apply_param_info_attrs(param_info, param_type, has_intent_attr, &
                                      has_optional_attr, has_target_attr)
        type(parameter_info_t), intent(in) :: param_info
        character(len=:), allocatable, intent(inout) :: param_type
        logical, intent(inout) :: has_intent_attr
        logical, intent(inout) :: has_optional_attr
        logical, intent(inout) :: has_target_attr

        if (.not. has_intent_attr) then
            if (allocated(param_info%intent_str)) then
                if (len_trim(param_info%intent_str) > 0) then
                    param_type = trim(param_type) // ", intent(" // &
                                 trim(param_info%intent_str) // ")"
                    has_intent_attr = .true.
                end if
            end if
        end if
        if (.not. has_optional_attr) then
            if (param_info%is_optional) then
                param_type = trim(param_type) // ", optional"
                has_optional_attr = .true.
            end if
        end if
        if (.not. has_target_attr) then
            if (param_info%is_target) then
                param_type = trim(param_type) // ", target"
                has_target_attr = .true.
            end if
        end if
    end subroutine apply_param_info_attrs

    subroutine append_array_dimensions(arena, param_idx, param_type, decl_line)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_idx
        character(len=*), intent(in) :: param_type
        character(len=:), allocatable, intent(inout) :: decl_line
        logical :: is_procedure_parameter
        character(len=:), allocatable :: dim_clause

        is_procedure_parameter = .false.
        select type (param_node => arena%entries(param_idx)%node)
        type is (parameter_declaration_node)
            if (allocated(param_node%type_name)) then
                is_procedure_parameter = is_procedure_type(param_node%type_name)
            end if
        end select
        if (.not. is_procedure_parameter) then
            is_procedure_parameter = is_procedure_type(param_type)
        end if

        if (.not. is_procedure_parameter) then
            select type (param_node => arena%entries(param_idx)%node)
            type is (parameter_declaration_node)
                if (param_node%is_array .or. &
                    param_node%inferred_type%kind == TARRAY) then
                    dim_clause = build_parameter_dimensions(arena, param_node)
                    if (len(dim_clause) == 0) then
                        dim_clause = build_assumed_shape_dimensions( &
                                     param_node%inferred_type)
                    end if
                    decl_line = trim(decl_line) // trim(dim_clause)
                end if
            end select
        end if
    end subroutine append_array_dimensions

    subroutine fix_declaration_formatting(decl_line)
        character(len=:), allocatable, intent(inout) :: decl_line
        integer :: pos_char
        integer :: intent_pos
        logical :: has_comma

        decl_line = fix_character_len_placeholder(decl_line)

        pos_char = index(decl_line, 'character(len=:)')
        if (pos_char > 0) then
            decl_line = decl_line(1:pos_char + 13) // '*)' // &
                        decl_line(pos_char + 17:)
        end if

        intent_pos = index(decl_line, ' intent(')
        has_comma = index(decl_line, ', intent(') > 0
        if (intent_pos > 0 .and. .not. has_comma) then
            decl_line = decl_line(:intent_pos - 1) // ', intent(' // &
                        decl_line(intent_pos + 8:)
        end if
    end subroutine fix_declaration_formatting

    function get_param_type_from_identifier(param_node) result(param_type)
        type(identifier_node), intent(in) :: param_node
        character(len=:), allocatable :: param_type
        integer :: pos

        param_type = mono_type_to_string(param_node%inferred_type, &
                                         include_shape=.true., fallback='real')
        if (len_trim(param_type) == 0) param_type = 'real'

        pos = index(param_type, 'character(len=:)')
        if (pos > 0) then
            param_type = param_type(1:pos + 13) // '*' // param_type(pos + 16:)
        end if
    end function get_param_type_from_identifier

    function get_param_type_from_param_decl(param_node) result(param_type)
        type(parameter_declaration_node), intent(in) :: param_node
        character(len=:), allocatable :: param_type

        if (allocated(param_node%type_name) .and. &
            len_trim(param_node%type_name) > 0) then
            param_type = param_node%type_name
        else
            param_type = mono_type_to_string(param_node%inferred_type, &
                                             include_shape=.false., fallback='real')
            if (len_trim(param_type) == 0) param_type = 'real'
        end if
    end function get_param_type_from_param_decl

    function get_param_type_fallback(param_node) result(param_type)
        class(*), intent(in) :: param_node
        character(len=:), allocatable :: param_type

        param_type = 'real'
    end function get_param_type_fallback

    logical function is_parameter_name(name, param_map) result(is_param)
        character(len=*), intent(in) :: name
        type(parameter_info_t), intent(in) :: param_map(:)
        integer :: i

        is_param = .false.
        do i = 1, size(param_map)
            if (allocated(param_map(i)%name)) then
                if (trim(param_map(i)%name) == trim(name)) then
                    is_param = .true.
                    return
                end if
            end if
        end do
    end function is_parameter_name

    logical function is_local_var_collected(name, collected, n) result(found)
        character(len=*), intent(in) :: name
        character(len=*), allocatable, intent(in) :: collected(:)
        integer, intent(in) :: n
        integer :: i

        found = .false.
        if (.not. allocated(collected)) return
        do i = 1, n
            if (trim(collected(i)) == trim(name)) then
                found = .true.
                return
            end if
        end do
    end function is_local_var_collected

    subroutine ensure_local_var_capacity(local_vars, capacity, required_size)
        character(len=64), allocatable, intent(inout) :: local_vars(:)
        integer, intent(inout) :: capacity
        integer, intent(in) :: required_size
        character(len=64), allocatable :: grown(:)
        integer :: new_capacity

        if (capacity >= required_size) return

        if (capacity > 0) then
            new_capacity = max(capacity * 2, required_size)
            allocate (grown(new_capacity))
            grown = ''
            grown(1:capacity) = local_vars(1:capacity)
            call move_alloc(grown, local_vars)
        else
            new_capacity = max(4, required_size)
            allocate (local_vars(new_capacity))
            local_vars = ''
        end if

        capacity = new_capacity
    end subroutine ensure_local_var_capacity

    subroutine add_declared_vars(var_names, declared_vars, n_declared, capacity)
        character(len=*), intent(in) :: var_names(:)
        character(len=64), allocatable, intent(inout) :: declared_vars(:)
        integer, intent(inout) :: n_declared
        integer, intent(inout) :: capacity
        integer :: i

        do i = 1, size(var_names)
            if (len_trim(var_names(i)) == 0) cycle
            if (.not. is_local_var_collected(trim(var_names(i)), declared_vars, &
                                             n_declared)) then
                call ensure_local_var_capacity(declared_vars, capacity, &
                                               n_declared + 1)
                n_declared = n_declared + 1
                declared_vars(n_declared) = trim(var_names(i))
            end if
        end do
    end subroutine add_declared_vars

    subroutine add_single_declared_var(var_name, declared_vars, n_declared, capacity)
        character(len=*), intent(in) :: var_name
        character(len=64), allocatable, intent(inout) :: declared_vars(:)
        integer, intent(inout) :: n_declared
        integer, intent(inout) :: capacity

        if (len_trim(var_name) == 0) return
        if (.not. is_local_var_collected(trim(var_name), declared_vars, n_declared)) &
            then
            call ensure_local_var_capacity(declared_vars, capacity, n_declared + 1)
            n_declared = n_declared + 1
            declared_vars(n_declared) = trim(var_name)
        end if
    end subroutine add_single_declared_var

    function build_assumed_shape_dimensions(inferred_type) result(dim_clause)
        type(mono_type_t), intent(in) :: inferred_type
        character(len=:), allocatable :: dim_clause
        type(mono_type_t) :: base_type
        integer :: rank, j

        dim_clause = ""
        call safe_extract_array_rank(inferred_type, rank, base_type)

        if (rank > 0) then
            dim_clause = "("
            do j = 1, rank
                if (j > 1) dim_clause = dim_clause // ","
                dim_clause = dim_clause // ":"
            end do
            dim_clause = dim_clause // ")"
        end if
    end function build_assumed_shape_dimensions

    pure logical function is_procedure_type(type_text) result(is_proc)
        character(len=*), intent(in) :: type_text
        character(len=:), allocatable :: lowered

        if (len_trim(type_text) == 0) then
            is_proc = .false.
            return
        end if

        lowered = to_lower(trim(type_text))
        if (len(lowered) < 9) then
            is_proc = .false.
        else
            is_proc = lowered(1:9) == 'procedure'
        end if
    end function is_procedure_type

end module codegen_procedure_shared
