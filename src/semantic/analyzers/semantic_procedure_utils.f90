module semantic_procedure_utils
    use type_system_unified, only: mono_type_t, create_mono_type, TDOUBLE, TINT, &
        TLOGICAL, TREAL, TCHAR
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
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: base_name
        character(len=:), allocatable :: kind_spec
        integer :: paren_pos
        integer :: kind_value

        mono%kind = 0
        trimmed = adjustl(type_name)
        if (.not. allocated(trimmed)) return
        if (len_trim(trimmed) == 0) return
        lowered = to_lower(trimmed)

        paren_pos = index(trimmed, '(')
        if (paren_pos > 0) then
            base_name = trim(lowered(1:paren_pos - 1))
            kind_spec = extract_paren_content(lowered(paren_pos + 1:))
            kind_value = parse_kind_value(kind_spec)
        else
            base_name = trim(lowered)
            kind_value = 0
        end if

        select case (base_name)
        case ('integer')
            mono = create_mono_type(TINT)
        case ('real')
            if (kind_value == 8) then
                mono = create_mono_type(TDOUBLE)
            else
                mono = create_mono_type(TREAL)
            end if
        case ('double precision')
            mono = create_mono_type(TDOUBLE)
        case ('logical')
            mono = create_mono_type(TLOGICAL)
        case ('character')
            mono = create_mono_type(TCHAR)
        case default
            mono%kind = 0
        end select
    end function declaration_type_to_mono

    pure function extract_paren_content(text) result(content)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: content
        integer :: close_pos

        close_pos = index(text, ')')
        if (close_pos <= 0) then
            content = trim(text)
            return
        end if
        if (close_pos == 1) then
            content = ""
            return
        end if
        content = trim(text(1:close_pos - 1))
    end function extract_paren_content

    pure integer function parse_kind_value(spec) result(kind_value)
        character(len=*), intent(in) :: spec
        character(len=:), allocatable :: trimmed
        integer :: eq_pos
        integer :: ios

        kind_value = 0
        trimmed = trim(adjustl(spec))
        if (len_trim(trimmed) == 0) return

        eq_pos = index(trimmed, '=')
        if (eq_pos > 0 .and. eq_pos < len_trim(trimmed)) then
            trimmed = trim(adjustl(trimmed(eq_pos + 1:)))
        end if

        read (trimmed, *, iostat=ios) kind_value
        if (ios /= 0) kind_value = 0
    end function parse_kind_value

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
            if (.not. arena%has_node_at(stmt_index)) cycle
            select type (stmt => arena%entries(stmt_index)%node)
                type is (assignment_node)
                target_index = stmt%target_index
                if (.not. arena%has_node_at(target_index)) cycle
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
