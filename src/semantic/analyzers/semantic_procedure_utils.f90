module semantic_procedure_utils
    use type_system_unified, only: mono_type_t, create_mono_type, TINT, TREAL, &
                                   TLOGICAL, TCHAR
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: assignment_node, identifier_node
    use ast_nodes_procedure, only: function_def_node
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: declaration_type_to_mono
    public :: detect_result_name

contains

    function declaration_type_to_mono(type_name) result(mono)
        character(len=*), intent(in) :: type_name
        type(mono_type_t) :: mono
        character(len=:), allocatable :: trimmed
        integer :: paren_pos

        mono%kind = 0
        trimmed = adjustl(type_name)
        if (.not. allocated(trimmed)) return
        if (len_trim(trimmed) == 0) return

        paren_pos = index(trimmed, '(')
        if (paren_pos > 0) then
            trimmed = trim(trimmed(1:paren_pos - 1))
        else
            trimmed = trim(trimmed)
        end if

        select case (trimmed)
        case ('integer')
            mono = create_mono_type(TINT)
        case ('real')
            mono = create_mono_type(TREAL)
        case ('logical')
            mono = create_mono_type(TLOGICAL)
        case ('character')
            mono = create_mono_type(TCHAR)
        case default
            mono%kind = 0
        end select
    end function declaration_type_to_mono

    function detect_result_name(arena, func_node) result(res_name)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: func_node
        character(len=:), allocatable :: res_name
        integer :: i, stmt_index, target_index
        character(len=:), allocatable :: first_assigned
        character(len=:), allocatable :: target_name
        character(len=:), allocatable :: target_lower

        res_name = ''
        first_assigned = ''
        if (.not. allocated(func_node%body_indices)) return

        do i = 1, size(func_node%body_indices)
            stmt_index = func_node%body_indices(i)
            if (stmt_index <= 0 .or. stmt_index > arena%size) cycle
            if (.not. allocated(arena%entries(stmt_index)%node)) cycle
            select type (stmt => arena%entries(stmt_index)%node)
            type is (assignment_node)
                target_index = stmt%target_index
                if (target_index <= 0 .or. target_index > arena%size) cycle
                if (.not. allocated(arena%entries(target_index)%node)) cycle
                select type (target => arena%entries(target_index)%node)
                type is (identifier_node)
                    if (.not. allocated(target%name)) cycle
                    target_name = trim(target%name)
                    if (len_trim(target_name) == 0) cycle
                    target_lower = to_lower(target_name)
                    if (target_lower == 'result') then
                        res_name = target_name
                        return
                    end if
                    if (len_trim(first_assigned) == 0) &
                        first_assigned = target_name
                end select
            end select
        end do

        if (len_trim(first_assigned) > 0) res_name = first_assigned
    end function detect_result_name

end module semantic_procedure_utils
