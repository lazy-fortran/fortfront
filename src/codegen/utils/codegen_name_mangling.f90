module codegen_name_mangling
    use string_utils_mod, only: int_to_string, to_lower
    use type_constants, only: TINT, TREAL, TCHAR, TLOGICAL, TARRAY, &
        TCOMPLEX, TDOUBLE
    implicit none
    private

    public :: mangle_procedure_name
    public :: type_signature_to_string

contains

    function mangle_procedure_name(base_name, signature, param_types) &
            result(mangled_name)
        character(len=*), intent(in) :: base_name
        integer, intent(in) :: signature(:)
        character(len=*), intent(in), optional :: param_types(:)
        character(len=:), allocatable :: mangled_name
        character(len=:), allocatable :: suffix
        integer :: i

        if (size(signature) == 0) then
            mangled_name = base_name
            return
        end if

        suffix = ""
        do i = 1, size(signature)
            if (i > 1) suffix = suffix // "_"
            if (present(param_types)) then
                if (i <= size(param_types)) then
                    suffix = suffix // kind_to_string(signature(i), param_types(i))
                else
                    suffix = suffix // kind_to_string(signature(i))
                end if
            else
                suffix = suffix // kind_to_string(signature(i))
            end if
        end do

        mangled_name = trim(base_name) // "__" // suffix
    end function mangle_procedure_name

    function kind_to_string(kind_value, type_hint) result(kind_str)
        integer, intent(in) :: kind_value
        character(len=*), intent(in), optional :: type_hint
        character(len=:), allocatable :: kind_str
        character(len=:), allocatable :: lowered
        character(len=:), allocatable :: base_from_hint
        integer :: rank

        rank = 0
        if (present(type_hint)) then
            lowered = to_lower(trim(type_hint))
            rank = extract_rank_from_type(lowered)
        else
            lowered = ""
        end if

        select case (kind_value)
        case (TINT)
            kind_str = "i32"
        case (TREAL)
            kind_str = "r32"
        case (TDOUBLE)
            kind_str = "r64"
        case (TCOMPLEX)
            kind_str = "c64"
        case (TCHAR)
            if (present(type_hint)) then
                kind_str = "ch" // extract_char_length_string(lowered)
            else
                kind_str = "ch"
            end if
        case (TLOGICAL)
            kind_str = "l32"
        case (TARRAY)
            if (present(type_hint)) then
                base_from_hint = infer_base_from_type(lowered)
                kind_str = base_from_hint
            else
                kind_str = "arr"
            end if
        case default
            if (present(type_hint)) then
                base_from_hint = infer_base_from_type(lowered)
                kind_str = base_from_hint
            else
                select case (kind_value)
                case (4)
                    kind_str = "i32"
                case (8)
                    kind_str = "i64"
                case (16)
                    kind_str = "i128"
                case (32)
                    kind_str = "r32"
                case (64)
                    kind_str = "r64"
                case (128)
                    kind_str = "r128"
                case (84)
                    kind_str = "c32"
                case (168)
                    kind_str = "c64"
                case (1)
                    kind_str = "l8"
                case default
                    kind_str = "k" // trim(int_to_string(kind_value))
                end select
            end if
        end select

        if (rank > 0) then
            kind_str = trim(kind_str) // "rank" // trim(int_to_string(rank))
        end if
    end function kind_to_string

    function infer_base_from_type(type_desc) result(base)
        character(len=*), intent(in) :: type_desc
        character(len=:), allocatable :: base

        if (index(type_desc, 'double precision') > 0 .or. &
            index(type_desc, 'real(8)') > 0 .or. &
            index(type_desc, 'real(dp)') > 0) then
            base = 'r64'
        else if (index(type_desc, 'real(16)') > 0) then
            base = 'r128'
        else if (index(type_desc, 'real') > 0) then
            base = 'r32'
        else if (index(type_desc, 'integer') > 0) then
            base = 'i32'
        else if (index(type_desc, 'complex(16)') > 0 .or. &
                index(type_desc, 'complex(8)') > 0 .or. &
                index(type_desc, 'double complex') > 0) then
            base = 'c128'
        else if (index(type_desc, 'complex') > 0) then
            base = 'c64'
        else if (index(type_desc, 'logical') > 0) then
            base = 'l32'
        else if (index(type_desc, 'character') > 0) then
            base = 'ch' // extract_char_length_string(type_desc)
        else
            base = 'arr'
        end if
    end function infer_base_from_type

    integer function extract_rank_from_type(type_desc) result(rank)
        character(len=*), intent(in) :: type_desc
        integer :: pos, end_pos, i
        character(len=:), allocatable :: inside

        rank = 0
        pos = index(type_desc, 'dimension(')
        if (pos <= 0) return

        pos = pos + len('dimension(')
        end_pos = pos
        do while (end_pos <= len(type_desc) .and. type_desc(end_pos:end_pos) /= ')')
            end_pos = end_pos + 1
        end do
        if (end_pos > len(type_desc)) then
            inside = type_desc(pos:)
        else
            inside = type_desc(pos:end_pos - 1)
        end if

        inside = adjustl(inside)
        if (len_trim(inside) == 0) then
            rank = 1
            return
        end if

        rank = 1
        do i = 1, len_trim(inside)
            if (inside(i:i) == ',') rank = rank + 1
        end do
    end function extract_rank_from_type

    function extract_char_length_string(type_desc) result(length_str)
        character(len=*), intent(in) :: type_desc
        character(len=:), allocatable :: length_str
        integer :: len_pos, equals_pos, paren_pos, comma_pos, end_pos
        integer :: length_value, i
        character(len=:), allocatable :: inside

        length_str = ""

        ! Look for "len=" pattern in character(len=N)
        len_pos = index(type_desc, 'len=')
        if (len_pos > 0) then
            ! Extract the length value after "len="
            equals_pos = len_pos + 4 ! Position after "len="
            end_pos = equals_pos

            ! Find the end of the number (either ')', ',' or end of string)
            do while (end_pos <= len(type_desc))
                if (type_desc(end_pos:end_pos) == ')' .or. &
                    type_desc(end_pos:end_pos) == ',' .or. &
                    type_desc(end_pos:end_pos) == ' ') exit
                end_pos = end_pos + 1
            end do

            if (end_pos > equals_pos) then
                inside = adjustl(type_desc(equals_pos:end_pos - 1))
                ! Check if it's a number
                if (is_numeric(inside)) then
                    length_str = trim(inside)
                end if
            end if
        else
            ! Look for character(N) pattern without "len="
            paren_pos = index(type_desc, 'character(')
            if (paren_pos > 0) then
                equals_pos = paren_pos + 10 ! Position after "character("
                end_pos = equals_pos

                do while (end_pos <= len(type_desc))
                    if (type_desc(end_pos:end_pos) == ')' .or. &
                        type_desc(end_pos:end_pos) == ',') exit
                    end_pos = end_pos + 1
                end do

                if (end_pos > equals_pos) then
                    inside = adjustl(type_desc(equals_pos:end_pos - 1))
                    ! Check if it's just a number (not "len=" or other keyword)
                    if (is_numeric(inside)) then
                        length_str = trim(inside)
                    end if
                end if
            end if
        end if
    end function extract_char_length_string

    logical function is_numeric(str) result(numeric)
        character(len=*), intent(in) :: str
        integer :: i

        numeric = .false.
        if (len_trim(str) == 0) return

        numeric = .true.
        do i = 1, len_trim(str)
            if (str(i:i) < '0' .or. str(i:i) > '9') then
                numeric = .false.
                return
            end if
        end do
    end function is_numeric

    function type_signature_to_string(signature) result(sig_str)
        integer, intent(in) :: signature(:)
        character(len=:), allocatable :: sig_str
        integer :: i

        if (size(signature) == 0) then
            sig_str = ""
            return
        end if

        sig_str = "("
        do i = 1, size(signature)
            if (i > 1) sig_str = sig_str // ", "
            sig_str = sig_str // kind_to_string(signature(i))
        end do
        sig_str = sig_str // ")"
    end function type_signature_to_string

end module codegen_name_mangling
