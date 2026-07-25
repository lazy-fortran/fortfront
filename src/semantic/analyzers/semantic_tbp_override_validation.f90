module semantic_tbp_override_validation
    ! Validates a type-bound procedure that overrides an inherited binding
    ! against F2018 C1526/7.5.7.3 (F2003 C468/4.5.7.3). An overriding binding
    ! and the binding it overrides must agree in everything except the
    ! passed-object dummy argument: the other dummy arguments must have the
    ! same declared type and INTENT, and a function result must have the same
    ! rank and, when the overridden result has a constant character length, the
    ! same constant character length.
    !
    ! The comparison is deliberately conservative. A rule fires only when both
    ! sides of the comparison are known from the AST, so an unresolved
    ! procedure or an undeclared dummy leaves the program accepted.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: derived_type_node, type_binding_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use semantic_procedure_signature, only: procedure_signature_t, &
        build_procedure_signature, find_procedure_definition
    use semantic_tbp_override_result, only: compare_function_results
    use string_utils_mod, only: int_to_string, to_lower
    implicit none
    private

    integer, parameter :: MAX_ANCESTOR_DEPTH = 64

    public :: validate_type_bound_overrides

contains

    ! Walk every extending derived type and compare each of its concrete
    ! bindings with the binding of the same name inherited from an ancestor.
    subroutine validate_type_bound_overrides(arena, errors)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer :: i, b

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (derived_type_node)
                if (.not. allocated(node%extends_parent)) cycle
                if (.not. allocated(node%binding_indices)) cycle
                do b = 1, size(node%binding_indices)
                    call check_binding(arena, node%binding_indices(b), &
                        node%extends_parent, errors)
                end do
            end select
        end do
    end subroutine validate_type_bound_overrides

    ! Compare one binding of an extending type with its overridden counterpart.
    subroutine check_binding(arena, binding_index, parent_type, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: binding_index
        character(len=*), intent(in) :: parent_type
        type(error_collection_t), intent(inout) :: errors
        type(procedure_signature_t) :: child_sig, parent_sig
        integer :: parent_binding

        if (binding_index <= 0) return
        if (.not. arena%has_node_at(binding_index)) return

        select type (binding => arena%entries(binding_index)%node)
            type is (type_binding_node)
            if (binding%is_final) return
            if (binding%is_generic) return
            if (binding%is_deferred) return
            if (.not. allocated(binding%binding_name)) return
            parent_binding = find_inherited_binding(arena, parent_type, &
                binding%binding_name)
            if (parent_binding <= 0) return
            child_sig = signature_of_binding(arena, binding_index)
            parent_sig = signature_of_binding(arena, parent_binding)
            if (.not. child_sig%found) return
            if (.not. parent_sig%found) return
            if (child_sig%is_function .neqv. parent_sig%is_function) return
            call compare_dummies(arena, binding_index, child_sig, parent_sig, &
                binding%binding_name, binding%line, binding%column, errors)
            call compare_function_results(child_sig, parent_sig, &
                binding%binding_name, binding%line, binding%column, errors)
        end select
    end subroutine check_binding

    ! Resolve the procedure a binding names and reduce it to a signature.
    function signature_of_binding(arena, binding_index) result(sig)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: binding_index
        type(procedure_signature_t) :: sig
        integer :: proc_index

        if (.not. arena%has_node_at(binding_index)) return
        select type (binding => arena%entries(binding_index)%node)
            type is (type_binding_node)
            if (allocated(binding%implementation)) then
                proc_index = find_procedure_definition(arena, &
                    binding%implementation)
            else if (allocated(binding%binding_name)) then
                proc_index = find_procedure_definition(arena, &
                    binding%binding_name)
            else
                proc_index = 0
            end if
            if (proc_index > 0) sig = build_procedure_signature(arena, proc_index)
        end select
    end function signature_of_binding

    ! Search the ancestor chain of type_name for a concrete binding called
    ! binding_name and return its arena index, or 0.
    function find_inherited_binding(arena, type_name, binding_name) &
            result(found_index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: type_name, binding_name
        integer :: found_index
        character(len=:), allocatable :: current
        integer :: type_index, depth

        found_index = 0
        current = trim(type_name)
        do depth = 1, MAX_ANCESTOR_DEPTH
            type_index = find_derived_type(arena, current)
            if (type_index <= 0) return
            found_index = binding_in_type(arena, type_index, binding_name)
            if (found_index > 0) return
            select type (node => arena%entries(type_index)%node)
                type is (derived_type_node)
                if (.not. allocated(node%extends_parent)) return
                current = trim(node%extends_parent)
            class default
                return
            end select
        end do
    end function find_inherited_binding

    ! Arena index of the derived type definition called type_name, or 0 when it
    ! is absent or defined more than once.
    function find_derived_type(arena, type_name) result(type_index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: type_name
        integer :: type_index
        character(len=:), allocatable :: wanted
        integer :: i, matches

        type_index = 0
        matches = 0
        wanted = to_lower(trim(type_name))
        if (len_trim(wanted) == 0) return

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (derived_type_node)
                if (.not. allocated(node%name)) cycle
                if (to_lower(trim(node%name)) /= wanted) cycle
                matches = matches + 1
                type_index = i
            end select
        end do

        if (matches /= 1) type_index = 0
    end function find_derived_type

    ! Arena index of the concrete binding called binding_name declared directly
    ! in the derived type at type_index, or 0.
    function binding_in_type(arena, type_index, binding_name) result(found_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: type_index
        character(len=*), intent(in) :: binding_name
        integer :: found_index
        character(len=:), allocatable :: wanted
        integer :: b

        found_index = 0
        wanted = to_lower(trim(binding_name))
        select type (node => arena%entries(type_index)%node)
            type is (derived_type_node)
            if (.not. allocated(node%binding_indices)) return
            do b = 1, size(node%binding_indices)
                if (.not. arena%has_node_at(node%binding_indices(b))) cycle
                select type (binding => arena%entries(node%binding_indices(b))%node)
                    type is (type_binding_node)
                    if (binding%is_final) cycle
                    if (binding%is_generic) cycle
                    if (.not. allocated(binding%binding_name)) cycle
                    if (to_lower(trim(binding%binding_name)) /= wanted) cycle
                    found_index = node%binding_indices(b)
                    return
                end select
            end do
        end select
    end function binding_in_type

    ! Compare every dummy argument except the passed-object dummy. Both sides
    ! must be known before a mismatch is reported.
    subroutine compare_dummies(arena, binding_index, child_sig, parent_sig, &
            binding_name, line, column, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: binding_index
        type(procedure_signature_t), intent(in) :: child_sig, parent_sig
        character(len=*), intent(in) :: binding_name
        integer, intent(in) :: line, column
        type(error_collection_t), intent(inout) :: errors
        integer :: passed_position, i

        if (.not. allocated(child_sig%dummies)) return
        if (.not. allocated(parent_sig%dummies)) return
        if (size(child_sig%dummies) /= size(parent_sig%dummies)) return

        passed_position = passed_object_position(arena, binding_index, child_sig)
        do i = 1, size(child_sig%dummies)
            if (i == passed_position) cycle
            call compare_one_dummy(child_sig%dummies(i), parent_sig%dummies(i), &
                binding_name, line, column, errors)
        end do
    end subroutine compare_dummies

    ! Report a declared-type or INTENT mismatch for a single dummy argument.
    subroutine compare_one_dummy(child, parent, binding_name, line, column, errors)
        use semantic_procedure_signature, only: dummy_info_t
        type(dummy_info_t), intent(in) :: child, parent
        character(len=*), intent(in) :: binding_name
        integer, intent(in) :: line, column
        type(error_collection_t), intent(inout) :: errors
        character(len=:), allocatable :: label

        label = 'argument'
        if (allocated(child%name)) then
            if (len_trim(child%name) > 0) label = 'argument '''//child%name//''''
        end if

        if (child%category_known) then
            if (parent%category_known) then
                if (child%category /= parent%category) then
                    call report(errors, 'Type mismatch in '//label// &
                        ' of overriding type-bound procedure '''// &
                        trim(binding_name)//''': overridden binding declares '// &
                        spell_category(parent%category)// &
                        ' but the override declares '// &
                        spell_category(child%category), &
                        'declare the overriding dummy argument with the same '// &
                        'type as the overridden binding', line, column)
                    return
                end if
            end if
        end if

        if (.not. child%has_intent) return
        if (.not. parent%has_intent) return
        if (child%intent_text == parent%intent_text) return
        call report(errors, 'INTENT mismatch in '//label// &
            ' of overriding type-bound procedure '''//trim(binding_name)// &
            ''': overridden binding declares INTENT('//parent%intent_text// &
            ') but the override declares INTENT('//child%intent_text//')', &
            'give the overriding dummy argument the same INTENT as the '// &
            'overridden binding', line, column)
    end subroutine compare_one_dummy

    ! Render an internal type category back as Fortran source spelling, so the
    ! diagnostic reads `CLASS(base_type)` rather than `class:base_type`.
    function spell_category(category) result(spelled)
        character(len=*), intent(in) :: category
        character(len=:), allocatable :: spelled
        integer :: colon

        colon = index(category, ':')
        if (colon <= 0) then
            spelled = upper(category)
            return
        end if
        spelled = upper(category(1:colon - 1))//'('//category(colon + 1:)//')'
    end function spell_category

    function upper(text) result(uppered)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: uppered
        integer :: i, code

        uppered = text
        do i = 1, len(uppered)
            code = iachar(uppered(i:i))
            if (code < iachar('a')) cycle
            if (code > iachar('z')) cycle
            uppered(i:i) = achar(code - 32)
        end do
    end function upper

    ! Position of the passed-object dummy in the overriding procedure, or 0 for
    ! a NOPASS binding.
    function passed_object_position(arena, binding_index, sig) result(position)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: binding_index
        type(procedure_signature_t), intent(in) :: sig
        integer :: position
        integer :: i

        position = 0
        select type (binding => arena%entries(binding_index)%node)
            type is (type_binding_node)
            if (.not. binding%pass_arg) return
            if (.not. allocated(binding%pass_name)) then
                position = 1
                return
            end if
            if (.not. allocated(sig%dummies)) return
            do i = 1, size(sig%dummies)
                if (.not. allocated(sig%dummies(i)%name)) cycle
                if (to_lower(sig%dummies(i)%name) /= &
                    to_lower(trim(binding%pass_name))) cycle
                position = i
                return
            end do
            position = 1
        end select
    end function passed_object_position

    subroutine report(errors, message, suggestion, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: message, suggestion
        integer, intent(in) :: line, column

        call errors%add_error( &
            message=message, &
            code=ERROR_SEMANTIC, &
            component='semantic_tbp_override_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), &
            suggestion=suggestion, line=line, column=column, &
            end_line=line, end_column=column + 1)
    end subroutine report

end module semantic_tbp_override_validation
