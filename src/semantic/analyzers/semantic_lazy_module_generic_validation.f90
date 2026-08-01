module semantic_lazy_module_generic_validation
    ! Issue #2978. Monomorphization (ast_monomorphization) specializes an
    ! untyped Lazy procedure only when it is a child of the compilation-unit
    ! root. A procedure contained in a module keeps a single body, so a
    ! reference at two incompatible argument types would compile to one call
    ! target and silently return garbage.
    !
    ! Until module procedures are monomorphized as well, that shape is
    ! rejected here. The rule is deliberately narrow, so that missing
    ! knowledge means silence and never a rejection:
    !
    !   * Lazy input only. Standard Fortran gives an undeclared dummy its
    !     implicit type, which is a definite type checked elsewhere.
    !   * The procedure must be contained in a module and must have at least
    !     one dummy argument without an explicit type.
    !   * Its name must be defined exactly once in the whole arena, so every
    !     reference to that name is a reference to this procedure.
    !   * Only references whose actual arguments are all literals are
    !     classified. A reference carrying anything else is ignored.
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: LITERAL_INTEGER, LITERAL_LOGICAL, LITERAL_REAL, &
        LITERAL_STRING, LITERAL_COMPLEX
    use ast_nodes_core, only: call_or_subscript_node, literal_node
    use ast_nodes_data, only: module_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use procedure_classification, only: procedure_has_explicit_types
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: validate_lazy_module_generics

contains

    ! Reject every module-contained untyped Lazy procedure that is referenced
    ! with conflicting literal argument types.
    subroutine validate_lazy_module_generics(arena, errors)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer :: i
        integer :: j

        if (arena%size <= 0) return

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (module_node)
                if (.not. allocated(node%procedure_indices)) cycle
                do j = 1, size(node%procedure_indices)
                    call check_module_procedure(arena, errors, node%name, &
                        node%procedure_indices(j))
                end do
            end select
        end do
    end subroutine validate_lazy_module_generics

    subroutine check_module_procedure(arena, errors, module_name, proc_index)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: module_name
        integer, intent(in) :: proc_index
        character(len=:), allocatable :: proc_name
        character(len=:), allocatable :: first_signature
        character(len=:), allocatable :: other_signature
        character(len=:), allocatable :: signature
        integer :: conflict_line
        integer :: conflict_column
        integer :: i

        if (.not. arena%has_node_at(proc_index)) return

        select type (proc => arena%entries(proc_index)%node)
            type is (function_def_node)
            if (.not. allocated(proc%name)) return
            proc_name = proc%name
            type is (subroutine_def_node)
            if (.not. allocated(proc%name)) return
            proc_name = proc%name
        class default
            return
        end select

        if (procedure_has_explicit_types(arena, proc_index)) return
        if (.not. name_defined_once(arena, proc_name)) return

        conflict_line = 0
        conflict_column = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (call_node => arena%entries(i)%node)
                type is (call_or_subscript_node)
                if (.not. allocated(call_node%name)) cycle
                if (to_lower(call_node%name) /= to_lower(proc_name)) cycle
                call classify_arguments(arena, call_node%arg_indices, signature)
                if (.not. allocated(signature)) cycle
                if (.not. allocated(first_signature)) then
                    first_signature = signature
                else if (signature /= first_signature) then
                    if (.not. allocated(other_signature)) then
                        other_signature = signature
                        conflict_line = call_node%line
                        conflict_column = call_node%column
                    end if
                end if
            end select
        end do

        if (.not. allocated(other_signature)) return

        call errors%add_error( &
            "Lazy generic procedure '"//trim(proc_name)//"' contained in "// &
            "module '"//trim(module_name)//"' is referenced with conflicting "// &
            "argument types ("//first_signature//" and "//other_signature// &
            "); module procedures are not specialized, so one body would be "// &
            "called at both types", &
            severity=ERROR_SEMANTIC, component="semantic_lazy_module_generic", &
            line=conflict_line, column=conflict_column)
    end subroutine check_module_procedure

    ! .true. when exactly one procedure definition in the arena carries this
    ! name, so that a reference to the name is unambiguous.
    logical function name_defined_once(arena, proc_name) result(unique)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: proc_name
        integer :: count
        integer :: i

        count = 0
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (function_def_node)
                if (allocated(node%name)) then
                    if (to_lower(node%name) == to_lower(proc_name)) then
                        count = count + 1
                    end if
                end if
                type is (subroutine_def_node)
                if (allocated(node%name)) then
                    if (to_lower(node%name) == to_lower(proc_name)) then
                        count = count + 1
                    end if
                end if
            end select
        end do

        unique = count == 1
    end function name_defined_once

    ! Build a comma separated type list for an argument list made only of
    ! literals. Leaves the result unallocated when any argument is anything
    ! else, or when there are no arguments.
    subroutine classify_arguments(arena, arg_indices, signature)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: arg_indices(:)
        character(len=:), allocatable, intent(out) :: signature
        character(len=:), allocatable :: text
        character(len=:), allocatable :: arg_type
        integer :: i

        if (.not. allocated(arg_indices)) return
        if (size(arg_indices) == 0) return

        text = ''
        do i = 1, size(arg_indices)
            call classify_literal(arena, arg_indices(i), arg_type)
            if (.not. allocated(arg_type)) return
            if (i > 1) text = text//', '
            text = text//arg_type
        end do

        signature = text
    end subroutine classify_arguments

    subroutine classify_literal(arena, node_index, arg_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: arg_type

        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
            type is (literal_node)
            select case (node%literal_kind)
            case (LITERAL_INTEGER)
                arg_type = 'integer'
            case (LITERAL_REAL)
                arg_type = 'real'
            case (LITERAL_LOGICAL)
                arg_type = 'logical'
            case (LITERAL_STRING)
                arg_type = 'character'
            case (LITERAL_COMPLEX)
                arg_type = 'complex'
            end select
        end select
    end subroutine classify_literal

end module semantic_lazy_module_generic_validation
