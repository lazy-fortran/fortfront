module semantic_type_hierarchy_validation
    ! Wires the F2003 derived-type inheritance tracker (type_hierarchy) into
    ! semantic analysis. Every derived type and its EXTENDS parent is registered
    ! so that downstream resolution (SELECT TYPE class-is guards, polymorphic
    ! assignment compatibility) can consult is_subtype_of on a populated
    ! hierarchy. Registration also surfaces two semantic errors that were
    ! previously undetected: an EXTENDS of an unknown parent type and a circular
    ! inheritance chain.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_data, only: derived_type_node
    use ast_nodes_misc, only: use_statement_node
    use type_hierarchy, only: type_hierarchy_t
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use string_utils_mod, only: int_to_string
    implicit none
    private

    public :: populate_type_hierarchy

contains

    ! Register every derived type in the arena into the hierarchy. Types are
    ! registered parents-before-children by repeated passes until no further
    ! progress is made; any type whose parent never resolves is reported as an
    ! EXTENDS of an unknown type. Cycle detection is handled by register_type.
    subroutine populate_type_hierarchy(arena, hierarchy, errors)
        type(ast_arena_t), intent(in) :: arena
        type(type_hierarchy_t), intent(inout) :: hierarchy
        type(error_collection_t), intent(inout) :: errors
        logical, allocatable :: registered(:)
        logical :: progressed
        integer :: i

        if (arena%size <= 0) return
        allocate (registered(arena%size))
        registered = .false.

        do
            progressed = .false.
            do i = 1, arena%size
                if (registered(i)) cycle
                if (.not. arena%has_node_at(i)) then
                    registered(i) = .true.
                    cycle
                end if
                call try_register_node(arena, i, hierarchy, registered, &
                                       progressed, errors)
            end do
            if (.not. progressed) exit
        end do

        ! A USE statement may import the parent type from another module, so a
        ! locally unresolved parent is only a definite error when nothing is
        ! imported into this scope.
        if (.not. arena_has_use_statement(arena)) then
            call report_unresolved_parents(arena, registered, errors)
        end if
    end subroutine populate_type_hierarchy

    ! Detect whether the arena contains any USE statement, in which case a
    ! parent type may be defined in an imported module rather than locally.
    function arena_has_use_statement(arena) result(has_use)
        type(ast_arena_t), intent(in) :: arena
        logical :: has_use
        integer :: i

        has_use = .false.
        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
            type is (use_statement_node)
                has_use = .true.
                return
            end select
        end do
    end function arena_has_use_statement

    ! Attempt to register a single arena node if it is a derived type whose
    ! parent (if any) is already registered. Marks non-type nodes as done.
    subroutine try_register_node(arena, idx, hierarchy, registered, progressed, &
                                 errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        type(type_hierarchy_t), intent(inout) :: hierarchy
        logical, intent(inout) :: registered(:)
        logical, intent(inout) :: progressed
        type(error_collection_t), intent(inout) :: errors
        character(len=:), allocatable :: error_msg
        logical :: parent_ready

        select type (node => arena%entries(idx)%node)
        type is (derived_type_node)
            if (.not. allocated(node%name)) then
                registered(idx) = .true.
                progressed = .true.
                return
            end if
            parent_ready = .true.
            if (allocated(node%extends_parent)) then
                parent_ready = hierarchy%find_entry(node%extends_parent) > 0
            end if
            if (.not. parent_ready) return

            if (allocated(node%extends_parent)) then
                call hierarchy%register_type(node%name, &
                                             parent_name=node%extends_parent, &
                                             error_msg=error_msg)
            else
                call hierarchy%register_type(node%name, error_msg=error_msg)
            end if
            registered(idx) = .true.
            progressed = .true.
            if (allocated(error_msg)) then
                if (len_trim(error_msg) > 0) then
                    call report_hierarchy_error(errors, error_msg, node%line, &
                                                node%column)
                end if
            end if
        class default
            registered(idx) = .true.
            progressed = .true.
        end select
    end subroutine try_register_node

    ! Any derived type left unregistered after the fixpoint extends a parent
    ! that is never defined; report it once per offending type.
    subroutine report_unresolved_parents(arena, registered, errors)
        type(ast_arena_t), intent(in) :: arena
        logical, intent(in) :: registered(:)
        type(error_collection_t), intent(inout) :: errors
        character(len=:), allocatable :: message
        integer :: i

        do i = 1, arena%size
            if (registered(i)) cycle
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
            type is (derived_type_node)
                if (.not. allocated(node%extends_parent)) cycle
                message = 'Parent type not found: '//trim(node%extends_parent)
                call report_hierarchy_error(errors, message, node%line, &
                                            node%column)
            end select
        end do
    end subroutine report_unresolved_parents

    subroutine report_hierarchy_error(errors, message, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: message
        integer, intent(in) :: line, column

        call errors%add_error( &
            message=message, &
            code=ERROR_SEMANTIC, &
            component='semantic_type_hierarchy_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), &
            suggestion='define the parent type before extending it, or '// &
            'break the circular EXTENDS chain')
    end subroutine report_hierarchy_error

end module semantic_type_hierarchy_validation
