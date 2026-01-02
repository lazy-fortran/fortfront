module codegen_character_types
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use ast_nodes_procedure, only: function_def_node
    use string_utils_mod, only: to_lower
    implicit none
    private
    public :: derive_character_return_type
    public :: character_len_references_params
    public :: is_deferred_character_return
    public :: is_allocatable_array_return
    public :: is_deferred_shape_array
    public :: has_character_len_result_decl
    public :: is_character_len_declaration

contains

    subroutine derive_character_return_type(arena, node, override)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable, intent(out) :: override
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: target_name
        integer :: i, decl_index

        override = ""

        if (allocated(node%return_type)) then
            lowered = to_lower(trim(node%return_type))
            if (index(lowered, "character(len=:), allocatable") == 0) return
        else
            return
        end if

        if (allocated(node%result_variable)) then
            if (len_trim(node%result_variable) > 0) then
                target_name = trim(node%result_variable)
            else
                target_name = trim(node%name)
            end if
        else
            target_name = trim(node%name)
        end if

        if (.not. allocated(node%body_indices)) return
        do i = 1, size(node%body_indices)
            decl_index = node%body_indices(i)
            if (decl_index <= 0 .or. decl_index > arena%size) cycle
            if (.not. allocated(arena%entries(decl_index)%node)) cycle
            select type (stmt => arena%entries(decl_index)%node)
            type is (declaration_node)
                if (len_trim(stmt%var_name) == 0) cycle
                if (trim(stmt%var_name) /= target_name) cycle
                if (.not. allocated(stmt%type_name)) cycle
                lowered = to_lower(trim(stmt%type_name))
                if (index(lowered, "len=") > 0) then
                    if (.not. character_len_references_params(arena, node, &
                                                              stmt%type_name)) then
                        override = trim(stmt%type_name)
                        return
                    end if
                end if
            end select
        end do
    end subroutine derive_character_return_type

    logical function character_len_references_params(arena, node, type_spec) &
        result(refs_params)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=*), intent(in) :: type_spec
        integer :: len_pos, paren_pos, i
        character(len=:), allocatable :: len_expr
        character(len=:), allocatable :: param_name

        refs_params = .false.
        len_pos = index(type_spec, 'len=')
        if (len_pos == 0) return

        paren_pos = index(type_spec(len_pos:), ')')
        if (paren_pos == 0) return

        len_expr = type_spec(len_pos + 4:len_pos + paren_pos - 2)
        if (.not. allocated(node%param_indices)) return

        do i = 1, size(node%param_indices)
            if (node%param_indices(i) <= 0 .or. node%param_indices(i) > &
                arena%size) cycle
            if (.not. allocated(arena%entries(node%param_indices(i))%node)) cycle

            select type (param_node => arena%entries(node%param_indices(i))%node)
            type is (identifier_node)
                param_name = trim(param_node%name)
            type is (parameter_declaration_node)
                param_name = trim(param_node%name)
            type is (declaration_node)
                param_name = trim(param_node%var_name)
            class default
                cycle
            end select

            if (index(len_expr, trim(param_name)) > 0) then
                refs_params = .true.
                return
            end if
        end do
    end function character_len_references_params

    pure logical function is_deferred_character_return(text) result(is_deferred)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lowered

        lowered = to_lower(trim(text))
        is_deferred = (index(lowered, 'character') == 1) .and. &
                      (index(lowered, 'len=:') > 0)
        if (is_deferred) then
            if (index(lowered, 'allocatable') == 0) then
                is_deferred = .false.
            end if
        end if
    end function is_deferred_character_return

    pure logical function is_allocatable_array_return(text) result(is_alloc_array)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lowered

        lowered = to_lower(trim(text))
        is_alloc_array = (index(lowered, 'dimension') > 0 .or. &
                         index(lowered, '(') > 0) .and. &
                         (index(lowered, 'allocatable') > 0)
    end function is_allocatable_array_return

    pure logical function is_deferred_shape_array(text) result(is_deferred)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lowered
        integer :: dim_pos, colon_pos

        is_deferred = .false.
        lowered = to_lower(trim(text))

        ! Check for dimension(:) or dimension(:,:) etc.
        dim_pos = index(lowered, 'dimension')
        if (dim_pos > 0) then
            ! Look for colon after dimension keyword
            colon_pos = index(lowered(dim_pos:), ':')
            if (colon_pos > 0) then
                is_deferred = .true.
                return
            end if
        end if

        ! Also check for shorthand syntax like real(:) or real(:,:)
        ! by looking for type followed by parentheses with colons
        if (index(lowered, 'real(') > 0 .or. index(lowered, 'integer(') > 0 .or. &
            index(lowered, 'logical(') > 0 .or. index(lowered, 'complex(') > 0 .or. &
            index(lowered, 'character(') > 0) then
            colon_pos = index(lowered, ':')
            if (colon_pos > 0) is_deferred = .true.
        end if
    end function is_deferred_shape_array

    logical function has_character_len_result_decl(arena, node) result(has_decl)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        character(len=:), allocatable :: target_name
        integer :: i, decl_index, name_idx
        character(len=:), allocatable :: lowered

        has_decl = .false.

        if (allocated(node%result_variable)) then
            target_name = trim(node%result_variable)
        else if (allocated(node%name)) then
            target_name = trim(node%name)
        else
            target_name = ''
        end if

        if (len_trim(target_name) == 0) return
        if (.not. allocated(node%body_indices)) return

        do i = 1, size(node%body_indices)
            decl_index = node%body_indices(i)
            if (decl_index <= 0 .or. decl_index > arena%size) cycle
            if (.not. allocated(arena%entries(decl_index)%node)) cycle
            select type (stmt => arena%entries(decl_index)%node)
            type is (declaration_node)
                if (is_character_len_declaration(stmt%type_name)) then
                    if (trim(stmt%var_name) == target_name) then
                        has_decl = .true.
                        return
                    end if
                    if (stmt%is_multi_declaration .and. &
                        allocated(stmt%var_names)) then
                        do name_idx = 1, size(stmt%var_names)
                            if (trim(stmt%var_names(name_idx)) == target_name) then
                                has_decl = .true.
                                return
                            end if
                        end do
                    end if
                end if
            end select
        end do
    end function has_character_len_result_decl

    pure logical function is_character_len_declaration(type_name) result(matches)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: lowered

        lowered = to_lower(trim(type_name))
        if (len_trim(lowered) == 0) then
            matches = .false.
            return
        end if

        matches = (index(lowered, 'character') == 1) .and. &
                  (index(lowered, 'len=') > 0) .and. &
                  (index(lowered, 'len=*') == 0) .and. &
                  (index(lowered, 'len=:') == 0)
    end function is_character_len_declaration
end module codegen_character_types
