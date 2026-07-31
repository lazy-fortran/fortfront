module semantic_enum_validation
    ! Report the ENUM constraint violations that the parser recorded on
    ! enum_node (F2003 R460 and 4.6): only ENUMERATOR statements may appear in
    ! an enum-def body, an initialized enumerator needs the "::" separator, and
    ! an enumerator value must be an integer within the kind of the
    ! enumeration. The sweep runs over the whole arena so that enumerations in
    ! programs, modules, and multi-unit files are all covered.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_legacy, only: enum_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    implicit none
    private

    public :: validate_enum_definitions

contains

    subroutine validate_enum_definitions(arena, errors)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
                type is (enum_node)
                call report_enum_violations(node, errors)
            end select
        end do
    end subroutine validate_enum_definitions

    subroutine report_enum_violations(node, errors)
        type(enum_node), intent(in) :: node
        type(error_collection_t), intent(inout) :: errors
        integer :: i, violation_line, violation_column

        if (.not. allocated(node%violation_messages)) return

        do i = 1, size(node%violation_messages)
            violation_line = node%line
            violation_column = node%column
            if (allocated(node%violation_lines)) then
                if (i <= size(node%violation_lines)) then
                    violation_line = node%violation_lines(i)
                end if
            end if
            if (allocated(node%violation_columns)) then
                if (i <= size(node%violation_columns)) then
                    violation_column = node%violation_columns(i)
                end if
            end if
            call errors%add_error(node%violation_messages(i)%s, &
                code=ERROR_SEMANTIC, component="semantic_enum_validation", &
                line=violation_line, column=violation_column)
        end do
    end subroutine report_enum_violations

end module semantic_enum_validation
