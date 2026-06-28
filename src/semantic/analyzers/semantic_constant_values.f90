module semantic_constant_values
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: literal_node
    use ast_base, only: LITERAL_INTEGER
    implicit none
    private

    public :: get_constant_integer_value
    public :: parse_literal_integer_value

contains

    logical function get_constant_integer_value(arena, expr_index, value) &
            result(found)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: expr_index
        integer, intent(out) :: value

        found = .false.
        value = 0

        if (expr_index <= 0) return
        if (expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return

        select type (node => arena%entries(expr_index)%node)
            type is (literal_node)
            if (node%is_constant .and. node%constant_type == LITERAL_INTEGER) then
                value = node%constant_integer
                found = .true.
                return
            end if

            if (allocated(node%value)) then
                found = parse_literal_integer_value(node%value, value)
            end if
        class default
            if (node%is_constant .and. node%constant_type == LITERAL_INTEGER) &
                then
                value = node%constant_integer
                found = .true.
            end if
        end select
    end function get_constant_integer_value

    logical function parse_literal_integer_value(raw_text, number) &
            result(success)
        character(len=*), intent(in) :: raw_text
        integer, intent(out) :: number
        character(len=:), allocatable :: cleaned
        integer :: underscore_pos
        integer :: ios

        success = .false.
        number = 0

        cleaned = trim(adjustl(raw_text))
        underscore_pos = index(cleaned, '_')
        if (underscore_pos > 0) then
            if (underscore_pos == 1) then
                cleaned = ''
            else
                cleaned = cleaned(1:underscore_pos - 1)
            end if
        end if

        if (len(cleaned) == 0) return

        read (cleaned, *, iostat=ios) number
        success = ios == 0
    end function parse_literal_integer_value

end module semantic_constant_values
