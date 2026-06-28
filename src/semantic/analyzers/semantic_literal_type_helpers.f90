module semantic_literal_type_helpers
    use type_system_unified, only: mono_type_t, create_mono_type, TINT, TREAL, &
        TDOUBLE
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL
    use ast_nodes_core, only: literal_node
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: literal_numeric_type
    public :: infer_real_literal_kind

contains

    function literal_numeric_type(lit) result(typ)
        type(literal_node), intent(in) :: lit
        type(mono_type_t) :: typ

        select case (lit%literal_kind)
        case (LITERAL_INTEGER)
            typ = create_mono_type(TINT)
        case (LITERAL_REAL)
            typ = infer_real_literal_kind(lit)
        case default
            typ%kind = 0
        end select
    end function literal_numeric_type

    function infer_real_literal_kind(lit) result(typ)
        type(literal_node), intent(in) :: lit
        type(mono_type_t) :: typ
        character(len=:), allocatable :: literal_value
        character(len=:), allocatable :: lowered_value
        character(len=:), allocatable :: kind_token
        integer :: underscore_pos
        integer :: read_status
        integer :: kind_int

        typ = create_mono_type(TREAL)
        if (.not. allocated(lit%value)) return

        literal_value = trim(lit%value)
        if (len(literal_value) == 0) return

        lowered_value = to_lower(literal_value)
        if (contains_double_exponent(lowered_value)) then
            typ = create_mono_type(TDOUBLE)
            return
        end if

        underscore_pos = index(lowered_value, "_")
        if (underscore_pos <= 0) return
        if (underscore_pos == len(lowered_value)) return

        kind_token = adjustl(lowered_value(underscore_pos + 1:))
        kind_token = trim(kind_token)
        if (len(kind_token) == 0) return

        select case (kind_token)
        case ("real64", "double", "doubleprecision", "dp")
            typ = create_mono_type(TDOUBLE)
            return
        case ("real32", "sp")
            typ = create_mono_type(TREAL)
            return
        case default
            read (kind_token, *, iostat=read_status) kind_int
            if (read_status /= 0) return
            if (kind_int >= 8) then
                typ = create_mono_type(TDOUBLE)
            else
                typ = create_mono_type(TREAL)
            end if
        end select
    end function infer_real_literal_kind

    pure logical function contains_double_exponent(text) result(has_double)
        character(len=*), intent(in) :: text
        integer :: i
        integer :: trimmed_length

        has_double = .false.
        trimmed_length = len_trim(text)

        do i = 1, trimmed_length
            if (text(i:i) /= 'd') cycle
            if (i <= 1) cycle
            if (.not. is_real_digit_or_dot(text(i - 1:i - 1))) cycle
            if (i == trimmed_length) then
                has_double = .true.
                return
            end if
            if (.not. is_digit_or_sign(text(i + 1:i + 1))) cycle
            has_double = .true.
            return
        end do
    end function contains_double_exponent

    pure logical function is_real_digit_or_dot(ch) result(is_valid)
        character(len=1), intent(in) :: ch
        integer :: code

        code = iachar(ch)
        is_valid = (ch == '.') .or. (code >= iachar('0') .and. code <= iachar('9'))
    end function is_real_digit_or_dot

    pure logical function is_digit_or_sign(ch) result(is_valid)
        character(len=1), intent(in) :: ch
        integer :: code

        code = iachar(ch)
        is_valid = (ch == '+') .or. (ch == '-') .or. &
            (code >= iachar('0') .and. code <= iachar('9'))
    end function is_digit_or_sign

end module semantic_literal_type_helpers
