module semantic_local_name_collision_validation
    ! Issue #2888 (reject-scope-02). F2023 19.3.1 puts derived-type names and
    ! variable names in the same class of local identifier, so within one
    ! scoping unit a name shall not be both. gfortran.dg/type_decl_4.f90 is the
    ! reference case ("Symbol 'xx' at (1) also declared as a type at (2)").
    !
    ! Only that pairing is diagnosed here: a derived-type definition whose name
    ! is also the name of a variable or named constant declared in the same
    ! scoping unit. Declaring an entity OF the type is untouched, and so is any
    ! collision that involves host or use association rather than two local
    ! declarations.
    use ast_arena_modern, only: ast_arena_t
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use frontend_compiler_resolution, only: declaration_binding_t, &
        get_scope_bindings, is_scope_node, BINDING_DECLARATION, &
        BINDING_NAMED_CONSTANT, BINDING_DERIVED_TYPE
    implicit none
    private

    public :: validate_local_name_collisions

contains

    ! Report every scoping unit that declares one name both as a derived type
    ! and as a variable or named constant.
    subroutine validate_local_name_collisions(arena, errors)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (.not. is_scope_node(arena, i)) cycle
            call check_scope(arena, i, errors)
        end do
    end subroutine validate_local_name_collisions

    ! Compare the direct bindings of one scoping unit pairwise. At most one
    ! diagnostic is emitted per entity binding.
    subroutine check_scope(arena, scope_index, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        type(error_collection_t), intent(inout) :: errors
        type(declaration_binding_t), allocatable :: bindings(:)
        character(len=:), allocatable :: error_msg
        integer :: i, j

        call get_scope_bindings(arena, scope_index, bindings, error_msg)
        if (len_trim(error_msg) > 0) return
        do j = 1, size(bindings)
            if (.not. is_entity_binding(bindings(j))) cycle
            do i = 1, size(bindings)
                if (bindings(i)%binding_kind /= BINDING_DERIVED_TYPE) cycle
                if (.not. same_name(bindings(i)%name, bindings(j)%name)) cycle
                call report_collision(arena, bindings(j), errors)
                exit
            end do
        end do
    end subroutine check_scope

    ! Whether a binding names a data entity rather than a type or procedure.
    logical function is_entity_binding(binding) result(is_entity)
        type(declaration_binding_t), intent(in) :: binding

        is_entity = binding%binding_kind == BINDING_DECLARATION .or. &
            binding%binding_kind == BINDING_NAMED_CONSTANT
    end function is_entity_binding

    subroutine report_collision(arena, binding, errors)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_binding_t), intent(in) :: binding
        type(error_collection_t), intent(inout) :: errors
        integer :: line, column

        call binding_position(arena, binding, line, column)
        call errors%add_error( &
            "Symbol '"//trim(binding%name)//"' is also declared as a type "// &
            "in the same scoping unit", &
            severity=ERROR_SEMANTIC, component="semantic_scope_collision", &
            line=line, column=column)
    end subroutine report_collision

    ! Source position of the declaration that carries the colliding name.
    subroutine binding_position(arena, binding, line, column)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_binding_t), intent(in) :: binding
        integer, intent(out) :: line
        integer, intent(out) :: column

        line = 0
        column = 0
        if (.not. arena%has_node_at(binding%declaration_node_index)) return
        line = arena%entries(binding%declaration_node_index)%node%line
        column = arena%entries(binding%declaration_node_index)%node%column
    end subroutine binding_position

    ! Case-insensitive comparison of Fortran names.
    logical function same_name(lhs, rhs) result(equal)
        character(len=*), intent(in) :: lhs
        character(len=*), intent(in) :: rhs

        equal = lowered(trim(lhs)) == lowered(trim(rhs))
    end function same_name

    function lowered(text) result(out)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: out
        integer :: i, code

        do i = 1, len(text)
            code = iachar(text(i:i))
            if (code < iachar('A')) then
                out(i:i) = text(i:i)
            else if (code > iachar('Z')) then
                out(i:i) = text(i:i)
            else
                out(i:i) = achar(code + 32)
            end if
        end do
    end function lowered

end module semantic_local_name_collision_validation
