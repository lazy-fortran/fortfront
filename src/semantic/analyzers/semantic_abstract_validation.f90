module semantic_abstract_validation
    ! Validates that a concrete (non-ABSTRACT) derived type extending an
    ! ABSTRACT type implements every DEFERRED type-bound procedure declared by
    ! its ABSTRACT ancestors (F2003 C464). The check walks the
    ! extends_parent chain, collects DEFERRED bindings from ABSTRACT ancestors,
    ! and reports an ERROR_SEMANTIC diagnostic for each deferred binding that no
    ! type in the chain overrides with a concrete implementation.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: derived_type_node, type_binding_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use string_utils_mod, only: int_to_string
    implicit none
    private

    public :: validate_abstract_extension, is_abstract_type

contains

    ! Validate that a concrete derived type satisfies all inherited DEFERRED
    ! bindings. ABSTRACT types and types without a parent are accepted.
    subroutine validate_abstract_extension(arena, node, errors)
        type(ast_arena_t), intent(in) :: arena
        type(derived_type_node), intent(in) :: node
        type(error_collection_t), intent(inout) :: errors
        character(len=:), allocatable :: parent_name
        integer :: parent_index
        integer :: guard

        if (is_abstract_type(node)) return
        if (.not. allocated(node%extends_parent)) return
        if (len_trim(node%extends_parent) == 0) return

        parent_name = trim(node%extends_parent)
        guard = 0
        do
            guard = guard + 1
            if (guard > arena%size) return
            parent_index = find_derived_type(arena, parent_name)
            if (parent_index == 0) return
            call check_parent_deferred(arena, node, parent_index, errors)
            call next_parent(arena, parent_index, parent_name)
            if (len_trim(parent_name) == 0) return
        end do
    end subroutine validate_abstract_extension

    ! Whether a derived type carries the ABSTRACT attribute in its header.
    function is_abstract_type(node) result(is_abstract)
        type(derived_type_node), intent(in) :: node
        logical :: is_abstract

        is_abstract = .false.
        if (.not. node%has_attributes) return
        if (.not. allocated(node%attribute_clause)) return
        is_abstract = clause_has_abstract(node%attribute_clause)
    end function is_abstract_type

    ! Whether a comma-separated attribute clause contains the ABSTRACT keyword.
    function clause_has_abstract(clause) result(found)
        character(len=*), intent(in) :: clause
        logical :: found
        character(len=:), allocatable :: token
        integer :: i, start

        found = .false.
        start = 1
        do i = 1, len(clause) + 1
            if (i > len(clause)) then
                token = lower_trim(clause(start:i - 1))
            else if (clause(i:i) == ',') then
                token = lower_trim(clause(start:i - 1))
            else
                cycle
            end if
            if (token == 'abstract') then
                found = .true.
                return
            end if
            start = i + 1
        end do
    end function clause_has_abstract

    ! Report every DEFERRED binding of an ABSTRACT parent that the concrete
    ! child does not override.
    subroutine check_parent_deferred(arena, child, parent_index, errors)
        type(ast_arena_t), intent(in) :: arena
        type(derived_type_node), intent(in) :: child
        integer, intent(in) :: parent_index
        type(error_collection_t), intent(inout) :: errors
        character(len=:), allocatable :: binding_name
        integer :: i

        if (.not. arena%has_node_at(parent_index)) return
        select type (parent => arena%entries(parent_index)%node)
        type is (derived_type_node)
            if (.not. is_abstract_type(parent)) return
            if (.not. allocated(parent%binding_indices)) return
            do i = 1, size(parent%binding_indices)
                if (.not. binding_is_deferred(arena, parent%binding_indices(i), &
                                              binding_name)) cycle
                if (concrete_overrides(arena, child, binding_name)) cycle
                call report_unimplemented(errors, child, binding_name)
            end do
        end select
    end subroutine check_parent_deferred

    ! Whether the binding at binding_index is DEFERRED; returns its name.
    function binding_is_deferred(arena, binding_index, name) result(deferred)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: binding_index
        character(len=:), allocatable, intent(out) :: name
        logical :: deferred

        deferred = .false.
        name = ''
        if (.not. arena%has_node_at(binding_index)) return
        select type (binding => arena%entries(binding_index)%node)
        type is (type_binding_node)
            if (.not. binding%is_deferred) return
            if (.not. allocated(binding%binding_name)) return
            deferred = .true.
            name = trim(binding%binding_name)
        end select
    end function binding_is_deferred

    ! Whether the concrete child overrides binding_name with a non-deferred
    ! binding of the same name.
    function concrete_overrides(arena, child, binding_name) result(overrides)
        type(ast_arena_t), intent(in) :: arena
        type(derived_type_node), intent(in) :: child
        character(len=*), intent(in) :: binding_name
        logical :: overrides
        integer :: i

        overrides = .false.
        if (.not. allocated(child%binding_indices)) return
        do i = 1, size(child%binding_indices)
            if (binding_overrides(arena, child%binding_indices(i), binding_name)) then
                overrides = .true.
                return
            end if
        end do
    end function concrete_overrides

    ! Whether the binding at binding_index implements binding_name concretely.
    function binding_overrides(arena, binding_index, binding_name) result(overrides)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: binding_index
        character(len=*), intent(in) :: binding_name
        logical :: overrides

        overrides = .false.
        if (.not. arena%has_node_at(binding_index)) return
        select type (binding => arena%entries(binding_index)%node)
        type is (type_binding_node)
            if (binding%is_deferred) return
            if (.not. allocated(binding%binding_name)) return
            if (trim(binding%binding_name) == trim(binding_name)) overrides = .true.
        end select
    end function binding_overrides

    ! Find the arena index of a derived type by name, or 0 if absent.
    function find_derived_type(arena, type_name) result(type_index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: type_name
        integer :: type_index
        integer :: i

        type_index = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
            type is (derived_type_node)
                if (.not. allocated(node%name)) cycle
                if (trim(node%name) == trim(type_name)) then
                    type_index = i
                    return
                end if
            end select
        end do
    end function find_derived_type

    ! Advance parent_name to the parent of the type at parent_index, or '' when
    ! there is no further parent.
    subroutine next_parent(arena, parent_index, parent_name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: parent_index
        character(len=:), allocatable, intent(out) :: parent_name

        parent_name = ''
        if (.not. arena%has_node_at(parent_index)) return
        select type (node => arena%entries(parent_index)%node)
        type is (derived_type_node)
            if (.not. allocated(node%extends_parent)) return
            parent_name = trim(node%extends_parent)
        end select
    end subroutine next_parent

    subroutine report_unimplemented(errors, child, binding_name)
        type(error_collection_t), intent(inout) :: errors
        type(derived_type_node), intent(in) :: child
        character(len=*), intent(in) :: binding_name
        character(len=:), allocatable :: type_name

        type_name = ''
        if (allocated(child%name)) type_name = trim(child%name)

        call errors%add_error( &
            message='concrete type "'//type_name//'" does not implement '// &
            'inherited DEFERRED binding "'//trim(binding_name)//'"', &
            code=ERROR_SEMANTIC, &
            component='semantic_abstract_validation', &
            context='line '//int_to_string(child%line)//', column '// &
            int_to_string(child%column), &
            suggestion='provide a type-bound procedure for "'// &
            trim(binding_name)//'", or declare the type ABSTRACT')
    end subroutine report_unimplemented

    ! Lower-case and trim a substring for case-insensitive keyword matching.
    function lower_trim(text) result(lowered)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lowered
        integer :: i, code

        lowered = trim(adjustl(text))
        do i = 1, len(lowered)
            code = iachar(lowered(i:i))
            if (code >= iachar('A')) then
                if (code <= iachar('Z')) lowered(i:i) = achar(code + 32)
            end if
        end do
    end function lower_trim

end module semantic_abstract_validation
