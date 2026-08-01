module semantic_local_name_collision_validation
    ! Issue #2888 (reject-scope-02): name collisions inside one scoping unit,
    ! and between a scoping unit and the entities it inherits by host or use
    ! association. F2023 19.3.1 puts derived-type names, variable names,
    ! procedure names and construct names into the same class of local
    ! identifier, so within one scoping unit a name shall not denote two of
    ! them, and a locally declared name shall not clash with an inherited name
    ! that the same scoping unit also uses.
    !
    ! Diagnosed families, with their gfortran.dg reference cases:
    !   * type/entity and procedure/entity collisions in one scoping unit
    !     (type_decl_4.f90, pr104351.f90)
    !   * a contained procedure whose name repeats its host's own name
    !     (pr77414.f90)
    !   * an internal procedure whose name repeats a host entity that the host
    !     scope also references (pr96102.f90)
    !   * a local derived-type definition of a use-associated name
    !     (used_types_25.f90)
    !   * a COMMON member or a construct name that repeats a host-associated
    !     derived type used in the same scope (common_29.f90,
    !     host_assoc_types_1.f90)
    !   * IMPORT of a name that is already use-associated in the local scope
    !     (pr123375.f90)
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node
    use ast_nodes_data, only: declaration_node
    use ast_nodes_legacy, only: common_block_node
    use ast_nodes_loops, only: do_loop_node
    use ast_nodes_misc, only: import_statement_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use frontend_compiler_resolution, only: declaration_binding_t, &
        get_scope_bindings, get_scope_statement_indices, is_scope_node, &
        find_enclosing_scope, find_host_scope, resolve_name_in_scope, &
        resolve_use_binding, BINDING_DECLARATION, BINDING_NAMED_CONSTANT, &
        BINDING_DERIVED_TYPE, BINDING_FUNCTION, BINDING_SUBROUTINE, &
        ASSOCIATION_HOST, ASSOCIATION_USE
    implicit none
    private

    integer, parameter :: CLASS_NONE = 0
    integer, parameter :: CLASS_ENTITY = 1
    integer, parameter :: CLASS_TYPE = 2
    integer, parameter :: CLASS_PROCEDURE = 3

    public :: validate_local_name_collisions

contains

    ! Run every collision rule over every scoping unit in the arena.
    subroutine validate_local_name_collisions(arena, errors)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (.not. is_scope_node(arena, i)) cycle
            call check_scope(arena, i, errors)
            call check_inherited_collisions(arena, i, errors)
            call check_common_and_construct_names(arena, i, errors)
            call check_imports(arena, i, errors)
        end do
    end subroutine validate_local_name_collisions

    ! Compare the direct bindings of one scoping unit pairwise, and against the
    ! name of the scoping unit itself. At most one diagnostic per binding.
    subroutine check_scope(arena, scope_index, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        type(error_collection_t), intent(inout) :: errors
        type(declaration_binding_t), allocatable :: bindings(:)
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: scope_name
        integer :: i, j

        call get_scope_bindings(arena, scope_index, bindings, error_msg)
        if (len_trim(error_msg) > 0) return
        scope_name = scope_unit_name(arena, scope_index)
        do j = 1, size(bindings)
            if (binding_class(bindings(j)) == CLASS_NONE) cycle
            if (binding_class(bindings(j)) == CLASS_PROCEDURE) then
                if (len_trim(scope_name) > 0) then
                    if (same_name(scope_name, bindings(j)%name)) then
                        call report(arena, bindings(j), errors, &
                            "' is already defined as the name of its host "// &
                            "scoping unit")
                        cycle
                    end if
                end if
            end if
            if (binding_class(bindings(j)) == CLASS_TYPE) cycle
            do i = 1, size(bindings)
                if (i == j) cycle
                if (binding_class(bindings(i)) == CLASS_NONE) cycle
                if (binding_class(bindings(i)) == binding_class(bindings(j))) cycle
                if (.not. same_name(bindings(i)%name, bindings(j)%name)) cycle
                if (binding_class(bindings(i)) == CLASS_TYPE) then
                    call report(arena, bindings(j), errors, &
                        "' is also declared as a type in the same scoping unit")
                else
                    call report(arena, bindings(j), errors, &
                        "' is already defined in the same scoping unit")
                end if
                exit
            end do
        end do
    end subroutine check_scope

    ! Collisions between a local declaration and an inherited name: an internal
    ! procedure that repeats a host entity the host also references, and a
    ! derived-type definition of a use-associated name.
    subroutine check_inherited_collisions(arena, scope_index, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        type(error_collection_t), intent(inout) :: errors
        type(declaration_binding_t), allocatable :: bindings(:)
        type(declaration_binding_t) :: inherited
        character(len=:), allocatable :: error_msg
        integer :: i, host_index
        logical :: internal_scope

        call get_scope_bindings(arena, scope_index, bindings, error_msg)
        if (len_trim(error_msg) > 0) return
        host_index = find_host_scope(arena, scope_index)
        internal_scope = is_procedure_scope(arena, scope_index)
        do i = 1, size(bindings)
            if (bindings(i)%binding_kind == BINDING_DERIVED_TYPE) then
                call resolve_use_binding(arena, scope_index, bindings(i)%name, &
                                         inherited)
                if (inherited%found) then
                    call report(arena, bindings(i), errors, &
                        "' has already been defined by use association")
                end if
                cycle
            end if
            if (binding_class(bindings(i)) /= CLASS_PROCEDURE) cycle
            if (host_index <= 0) cycle
            if (.not. internal_scope) cycle
            call resolve_name_in_scope(arena, host_index, bindings(i)%name, &
                                       inherited, error_msg)
            if (len_trim(error_msg) > 0) cycle
            if (.not. inherited%found) cycle
            if (binding_class(inherited) /= CLASS_ENTITY) cycle
            if (.not. name_referenced_as_data_object(arena, scope_index, &
                                                     bindings(i)%name)) cycle
            call report(arena, bindings(i), errors, &
                "' is host associated and cannot also name an internal "// &
                "procedure of the same name")
        end do
    end subroutine check_inherited_collisions

    ! A COMMON member or a construct name may not repeat a derived type that the
    ! same scoping unit uses by host or use association.
    subroutine check_common_and_construct_names(arena, scope_index, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        type(error_collection_t), intent(inout) :: errors
        integer, allocatable :: indices(:)
        integer :: i, j

        call get_scope_statement_indices(arena, scope_index, indices)
        do i = 1, size(indices)
            if (.not. arena%has_node_at(indices(i))) cycle
            select type (node => arena%entries(indices(i))%node)
            type is (common_block_node)
                if (.not. allocated(node%member_names)) cycle
                do j = 1, size(node%member_names)
                    if (.not. inherited_type_used_here(arena, scope_index, &
                                                       node%member_names(j)%s)) cycle
                    call report_at(errors, node%member_names(j)%s, node%line, &
                                   node%column, incompatible_object_message())
                end do
            type is (do_loop_node)
                if (.not. allocated(node%label)) cycle
                if (.not. inherited_type_used_here(arena, scope_index, &
                                                   node%label)) cycle
                call report_at(errors, node%label, node%line, node%column, &
                               incompatible_object_message())
            end select
        end do
    end subroutine check_common_and_construct_names

    function incompatible_object_message() result(text)
        character(len=:), allocatable :: text

        text = "' names an incompatible object of the same name as a host "// &
            "associated derived type"
    end function incompatible_object_message

    ! IMPORT may only bring in names of the host scoping unit, so importing a
    ! name that the local scope already accesses by use association is invalid.
    subroutine check_imports(arena, scope_index, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        type(error_collection_t), intent(inout) :: errors
        type(declaration_binding_t) :: binding
        integer, allocatable :: indices(:)
        integer :: i, j

        call get_scope_statement_indices(arena, scope_index, indices)
        do i = 1, size(indices)
            if (.not. arena%has_node_at(indices(i))) cycle
            select type (node => arena%entries(indices(i))%node)
            type is (import_statement_node)
                if (.not. node%has_list) cycle
                if (.not. allocated(node%import_list)) cycle
                do j = 1, size(node%import_list)
                    call resolve_use_binding(arena, scope_index, &
                                             node%import_list(j)%s, binding)
                    if (.not. binding%found) cycle
                    call report_at(errors, node%import_list(j)%s, node%line, &
                                   node%column, &
                                   "' cannot be imported because it is "// &
                                   "already accessible in the local scope")
                end do
            end select
        end do
    end subroutine check_imports

    ! Whether NAME is used as a derived type in this scope and resolves to a
    ! type inherited by host or use association.
    logical function inherited_type_used_here(arena, scope_index, name) &
            result(is_inherited)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        character(len=*), intent(in) :: name
        type(declaration_binding_t) :: binding
        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: spec_name
        integer, allocatable :: indices(:)
        logical :: used
        integer :: i

        is_inherited = .false.
        if (len_trim(name) == 0) return
        used = .false.
        call get_scope_statement_indices(arena, scope_index, indices)
        do i = 1, size(indices)
            if (.not. arena%has_node_at(indices(i))) cycle
            select type (node => arena%entries(indices(i))%node)
            type is (declaration_node)
                if (.not. allocated(node%type_name)) cycle
                spec_name = derived_type_spec_name(node%type_name)
                if (len_trim(spec_name) == 0) cycle
                if (same_name(spec_name, name)) used = .true.
            end select
        end do
        if (.not. used) return
        call resolve_name_in_scope(arena, scope_index, name, binding, error_msg)
        if (len_trim(error_msg) > 0) return
        if (.not. binding%found) return
        if (binding%binding_kind /= BINDING_DERIVED_TYPE) return
        if (binding%association == ASSOCIATION_HOST) is_inherited = .true.
        if (binding%association == ASSOCIATION_USE) is_inherited = .true.
    end function inherited_type_used_here

    ! Derived-type name inside a type(...) or class(...) spec, empty otherwise.
    function derived_type_spec_name(type_name) result(name)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: name
        character(len=:), allocatable :: text
        integer :: open_pos, close_pos

        name = ''
        text = trim(adjustl(type_name))
        open_pos = index(text, '(')
        close_pos = index(text, ')', back=.true.)
        if (open_pos <= 1) return
        if (close_pos <= open_pos + 1) return
        if (.not. same_name(text(1:open_pos - 1), 'type')) then
            if (.not. same_name(text(1:open_pos - 1), 'class')) return
        end if
        name = trim(adjustl(text(open_pos + 1:close_pos - 1)))
    end function derived_type_spec_name

    ! Whether NAME appears directly in SCOPE_INDEX as a data object, that is a
    ! bare name rather than a procedure reference or an array section. A bare
    ! name can never denote an internal procedure, so it still means the
    ! inherited entity and the two spellings of the name conflict.
    logical function name_referenced_as_data_object(arena, scope_index, name) &
            result(referenced)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        character(len=*), intent(in) :: name
        integer :: i

        referenced = .false.
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            if (.not. is_bare_name_reference(arena, i, name)) cycle
            if (find_enclosing_scope(arena, i) /= scope_index) cycle
            referenced = .true.
            return
        end do
    end function name_referenced_as_data_object

    ! Whether the node is a bare identifier reference spelling NAME.
    logical function is_bare_name_reference(arena, node_index, name) result(is_bare)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: name

        is_bare = .false.
        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            if (.not. allocated(node%name)) return
            is_bare = same_name(node%name, name)
        end select
    end function is_bare_name_reference

    ! Whether the scoping unit is a procedure, so procedures contained in it are
    ! internal procedures rather than module procedures.
    logical function is_procedure_scope(arena, scope_index) result(is_procedure)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index

        is_procedure = .false.
        select type (node => arena%entries(scope_index)%node)
        type is (function_def_node)
            is_procedure = .true.
        type is (subroutine_def_node)
            is_procedure = .true.
        end select
    end function is_procedure_scope

    ! Declared name of a scoping unit, empty when it has none.
    function scope_unit_name(arena, scope_index) result(name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: scope_index
        character(len=:), allocatable :: name

        name = ''
        select type (node => arena%entries(scope_index)%node)
        type is (function_def_node)
            if (allocated(node%name)) name = node%name
        type is (subroutine_def_node)
            if (allocated(node%name)) name = node%name
        end select
    end function scope_unit_name

    ! Class of local identifier a binding belongs to.
    integer function binding_class(binding) result(class_id)
        type(declaration_binding_t), intent(in) :: binding

        select case (binding%binding_kind)
        case (BINDING_DECLARATION, BINDING_NAMED_CONSTANT)
            class_id = CLASS_ENTITY
        case (BINDING_DERIVED_TYPE)
            class_id = CLASS_TYPE
        case (BINDING_FUNCTION, BINDING_SUBROUTINE)
            class_id = CLASS_PROCEDURE
        case default
            class_id = CLASS_NONE
        end select
    end function binding_class

    subroutine report(arena, binding, errors, suffix)
        type(ast_arena_t), intent(in) :: arena
        type(declaration_binding_t), intent(in) :: binding
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: suffix
        integer :: line, column

        call binding_position(arena, binding, line, column)
        call report_at(errors, binding%name, line, column, suffix)
    end subroutine report

    subroutine report_at(errors, name, line, column, suffix)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: name
        integer, intent(in) :: line
        integer, intent(in) :: column
        character(len=*), intent(in) :: suffix

        call errors%add_error( &
            "Symbol '"//trim(name)//suffix, &
            severity=ERROR_SEMANTIC, component="semantic_scope_collision", &
            line=line, column=column)
    end subroutine report_at

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
