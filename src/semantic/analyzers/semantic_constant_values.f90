module semantic_constant_values
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: literal_node
    use ast_base, only: LITERAL_INTEGER
    implicit none
    private

    public :: get_constant_integer_value
    public :: parse_literal_integer_value
    public :: integer_literal_fits_default_kind

contains

    ! Decide whether an integer literal, written with or without a kind
    ! parameter, denotes a value representable in the default integer kind.
    ! An ENUM with BIND(C) is limited to that range because its kind is the
    ! companion C processor's int (F2003 4.6), so a value outside it is an
    ! error rather than a silently widened constant.
    logical function integer_literal_fits_default_kind(raw_text, value, &
            is_integer_literal) result(fits)
        character(len=*), intent(in) :: raw_text
        integer, intent(out) :: value
        logical, intent(out), optional :: is_integer_literal
        character(len=:), allocatable :: cleaned
        integer(int64) :: wide_value
        integer :: underscore_pos, ios, i, digit_count

        fits = .false.
        value = 0
        if (present(is_integer_literal)) is_integer_literal = .false.

        cleaned = trim(adjustl(raw_text))
        underscore_pos = index(cleaned, '_')
        if (underscore_pos > 0) cleaned = cleaned(1:underscore_pos - 1)
        if (len(cleaned) == 0) return

        ! Only plain decimal digits with an optional sign are handled here;
        ! anything else is not an integer literal and is not this rule's case.
        digit_count = 0
        do i = 1, len(cleaned)
            if (i == 1) then
                if (cleaned(i:i) == '+' .or. cleaned(i:i) == '-') cycle
            end if
            if (cleaned(i:i) < '0' .or. cleaned(i:i) > '9') return
            digit_count = digit_count + 1
        end do
        if (digit_count == 0) return
        if (present(is_integer_literal)) is_integer_literal = .true.

        read (cleaned, *, iostat=ios) wide_value
        if (ios /= 0) return
        if (wide_value > int(huge(0_int32), int64)) return
        if (wide_value < -int(huge(0_int32), int64) - 1_int64) return

        value = int(wide_value)
        fits = .true.
    end function integer_literal_fits_default_kind

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
