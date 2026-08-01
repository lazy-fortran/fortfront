module semantic_derived_type_reference_validation
    ! Reject a TYPE(...) or CLASS(...) specifier that names a derived type
    ! which is not visible in the declaration's lexical scope. An unresolved
    ! name is diagnosed only when a type of that name exists in this source;
    ! external module types remain outside this single-file check.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: declaration_node, derived_type_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use frontend_compiler_resolution, only: BINDING_DERIVED_TYPE, &
        declaration_binding_t, find_enclosing_scope, &
        get_scope_statement_indices, is_scope_node, resolve_name_in_scope
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: validate_derived_type_references

contains

    subroutine validate_derived_type_references(arena, errors)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer :: i
        character(len=:), allocatable :: type_name
        integer :: scope_index
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (declaration_node)
                type_name = derived_type_name(node%type_name)
                if (len_trim(type_name) == 0) cycle

                scope_index = find_enclosing_scope(arena, i)
                if (scope_index <= 0) then
                    scope_index = direct_scope_for_node(arena, i)
                end if
                if (scope_index <= 0) cycle

                call resolve_name_in_scope(arena, scope_index, type_name, binding, &
                    error_msg)
                if (binding%found .and. binding_is_derived_type(binding)) cycle
                if (.not. source_defines_type(arena, type_name)) cycle

                call errors%add_error( &
                    "Derived type '"//trim(type_name)// &
                    "' is not accessible in this scope", &
                    severity=ERROR_SEMANTIC, &
                    component="semantic_derived_type_reference", &
                    line=node%line, column=node%column)
            end select
        end do
    end subroutine validate_derived_type_references

    ! A declaration may not have a parent link on every parser path. Look for
    ! the direct owner as a fallback so module-spec declarations are checked too.
    integer function direct_scope_for_node(arena, node_index) result(scope_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable :: indices(:)
        integer :: i, j

        scope_index = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (.not. is_scope_node(arena, i)) cycle
            call get_scope_statement_indices(arena, i, indices)
            do j = 1, size(indices)
                if (indices(j) == node_index) then
                    scope_index = i
                    return
                end if
            end do
        end do
    end function direct_scope_for_node

    logical function binding_is_derived_type(binding) result(is_derived)
        type(declaration_binding_t), intent(in) :: binding

        is_derived = binding%binding_kind == BINDING_DERIVED_TYPE
    end function binding_is_derived_type

    logical function source_defines_type(arena, wanted) result(found)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: wanted
        integer :: i

        found = .false.
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (derived_type_node)
                if (.not. allocated(node%name)) cycle
                if (same_name(node%name, wanted)) then
                    found = .true.
                    return
                end if
            end select
        end do
    end function source_defines_type

    function derived_type_name(type_spec) result(name)
        character(len=:), allocatable, intent(in) :: type_spec
        character(len=:), allocatable :: name
        character(len=:), allocatable :: text
        integer :: open_pos, close_pos, comma_pos

        name = ''
        if (.not. allocated(type_spec)) return
        text = to_lower(trim(adjustl(type_spec)))
        if (index(text, 'type(') /= 1 .and. index(text, 'class(') /= 1) return
        open_pos = index(text, '(')
        close_pos = index(text, ')', back=.true.)
        if (open_pos <= 0 .or. close_pos <= open_pos + 1) return
        name = trim(adjustl(text(open_pos + 1:close_pos - 1)))
        comma_pos = index(name, ',')
        if (comma_pos > 0) name = trim(name(:comma_pos - 1))
        if (name == '*' .or. name == 'unlimited polymorphic') name = ''
    end function derived_type_name

    logical function same_name(first, second) result(matches)
        character(len=*), intent(in) :: first, second

        matches = to_lower(trim(first)) == to_lower(trim(second))
    end function same_name

end module semantic_derived_type_reference_validation
