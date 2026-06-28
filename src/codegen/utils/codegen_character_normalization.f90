module codegen_character_normalization
    use type_system_unified, only: TCHAR
    use ast_nodes_data, only: declaration_node
    use string_utils_mod, only: int_to_string, to_lower
    use type_string_utils, only: is_character_type_string
    implicit none
    private

    public :: normalize_character_type
    public :: normalize_character_type_param

contains

    pure subroutine try_extract_length_from_star(trimmed_str, open_paren, has_length, &
            length_spec)
        character(len=*), intent(in) :: trimmed_str
        integer, intent(in) :: open_paren
        logical, intent(inout) :: has_length
        character(len=:), allocatable, intent(inout) :: length_spec
        integer :: star_pos
        integer :: trimmed_len
        character(len=:), allocatable :: candidate

        if (has_length) return

        star_pos = index(trimmed_str, "*")
        if (star_pos <= 0) return
        if (open_paren /= 0) then
            if (star_pos > open_paren) return
        end if

        trimmed_len = len_trim(trimmed_str)
        if (star_pos >= trimmed_len) return

        candidate = trim(trimmed_str(star_pos + 1:trimmed_len))
        if (len_trim(candidate) == 0) return

        length_spec = candidate
        has_length = .true.
    end subroutine try_extract_length_from_star

    pure subroutine try_extract_length_from_parentheses(trimmed_str, open_paren, &
            has_length, length_spec)
        character(len=*), intent(in) :: trimmed_str
        integer, intent(in) :: open_paren
        logical, intent(inout) :: has_length
        character(len=:), allocatable, intent(inout) :: length_spec
        integer :: close_paren
        integer :: depth
        integer :: last_char
        integer :: idx
        character(len=:), allocatable :: candidate

        if (has_length) return
        if (open_paren <= 0) return

        depth = 0
        close_paren = 0
        last_char = len_trim(trimmed_str)

        do idx = open_paren + 1, last_char
            select case (trimmed_str(idx:idx))
            case ("(")
                depth = depth + 1
            case (")")
                if (depth == 0) then
                    close_paren = idx
                    exit
                else
                    depth = depth - 1
                end if
            end select
        end do

        if (close_paren <= open_paren + 1) return

        candidate = trim(trimmed_str(open_paren + 1:close_paren - 1))
        if (len_trim(candidate) == 0) return

        length_spec = candidate
        has_length = .true.
    end subroutine try_extract_length_from_parentheses

    subroutine extract_character_length(type_str, has_length, length_spec)
        character(len=*), intent(in) :: type_str
        logical, intent(out) :: has_length
        character(len=:), allocatable, intent(out) :: length_spec
        integer :: open_paren
        character(len=:), allocatable :: trimmed_str

        has_length = .false.
        length_spec = ""

        trimmed_str = trim(type_str)
        open_paren = index(trimmed_str, "(")

        call try_extract_length_from_star(trimmed_str, open_paren, has_length, &
            length_spec)
        if (has_length) return

        call try_extract_length_from_parentheses(trimmed_str, open_paren, &
            has_length, length_spec)
    end subroutine extract_character_length

    subroutine preprocess_character_type(raw_type, trimmed, has_length, length_spec, &
            needs_post_process, type_str)
        character(len=*), intent(in) :: raw_type
        character(len=:), allocatable, intent(out) :: trimmed
        logical, intent(out) :: has_length
        character(len=:), allocatable, intent(out) :: length_spec
        logical, intent(out) :: needs_post_process
        character(len=:), allocatable, intent(out) :: type_str
        integer :: comma_pos
        character(len=:), allocatable :: lowered_trim
        character(len=:), allocatable :: lowered_len

        trimmed = trim(raw_type)
        has_length = .false.
        needs_post_process = .false.

        if (.not. is_character_type_string(trimmed)) then
            type_str = trimmed
            return
        end if

        comma_pos = index(trimmed, ",")
        if (comma_pos > 0) then
            trimmed = trim(trimmed(:comma_pos - 1))
        end if

        lowered_trim = to_lower(trimmed)
        if (index(lowered_trim, "kind=") > 0 .and. index(lowered_trim, &
            "len") == 0) then
            type_str = trimmed
            return
        end if

        call extract_character_length(trimmed, has_length, length_spec)

        if (has_length) then
            lowered_len = to_lower(length_spec)
            if (index(lowered_len, "kind=") > 0 .and. index(lowered_len, &
                "len=") == 0) then
                type_str = trimmed
                return
            end if
        end if

        needs_post_process = .true.
    end subroutine preprocess_character_type

    subroutine ensure_character_length_from_node(node, has_length, length_spec)
        type(declaration_node), intent(in) :: node
        logical, intent(inout) :: has_length
        character(len=:), allocatable, intent(inout) :: length_spec

        if (.not. has_length) then
            if (node%has_kind) then
                if (node%kind_value > 0) then
                    length_spec = trim(adjustl(int_to_string(node%kind_value)))
                    has_length = .true.
                else if (node%kind_value == -1) then
                    length_spec = "*"
                    has_length = .true.
                end if
            end if
        end if

        if (.not. has_length) then
            if (node%inferred_type%kind == TCHAR) then
                if (node%inferred_type%alloc_info%needs_allocatable_string) then
                    length_spec = ":"
                    has_length = .true.
                else if (node%inferred_type%size > 0) then
                    length_spec = trim(adjustl(int_to_string(node%inferred_type%size)))
                    has_length = .true.
                else if (node%inferred_type%size == -1) then
                    length_spec = "*"
                    has_length = .true.
                end if
            end if
        end if
    end subroutine ensure_character_length_from_node

    subroutine ensure_character_length_from_kind(has_kind, kind_value, has_length, &
            length_spec)
        logical, intent(in) :: has_kind
        integer, intent(in) :: kind_value
        logical, intent(inout) :: has_length
        character(len=:), allocatable, intent(inout) :: length_spec

        if (.not. has_length) then
            if (has_kind) then
                if (kind_value > 0) then
                    length_spec = trim(adjustl(int_to_string(kind_value)))
                    has_length = .true.
                else if (kind_value == -1) then
                    length_spec = "*"
                    has_length = .true.
                end if
            end if
        end if
    end subroutine ensure_character_length_from_kind

    subroutine finalize_character_type(has_length, length_spec, type_str)
        logical, intent(inout) :: has_length
        character(len=:), allocatable, intent(inout) :: length_spec
        character(len=:), allocatable, intent(out) :: type_str
        character(len=:), allocatable :: lowered_len

        if (has_length) then
            if (.not. allocated(length_spec)) then
                has_length = .false.
            else if (len_trim(length_spec) == 0) then
                has_length = .false.
            end if
        end if

        if (has_length) then
            lowered_len = to_lower(trim(length_spec))
            select case (trim(lowered_len))
            case ("-1")
                length_spec = "*"
            case ("len=-1")
                length_spec = "len=*"
            end select
        end if

        if (.not. has_length) then
            type_str = "character"
        else
            lowered_len = to_lower(length_spec)
            if (index(lowered_len, "len=") == 0) then
                length_spec = "len=" // trim(length_spec)
            end if
            type_str = "character(" // trim(length_spec) // ")"
        end if
    end subroutine finalize_character_type

    function normalize_character_type(node, raw_type) result(type_str)
        type(declaration_node), intent(in) :: node
        character(len=*), intent(in) :: raw_type
        character(len=:), allocatable :: type_str
        character(len=:), allocatable :: trimmed
        character(len=:), allocatable :: length_spec
        logical :: has_length
        logical :: needs_post_process

        call preprocess_character_type(raw_type, trimmed, has_length, length_spec, &
            needs_post_process, type_str)
        if (.not. needs_post_process) return

        call ensure_character_length_from_node(node, has_length, length_spec)
        call finalize_character_type(has_length, length_spec, type_str)
    end function normalize_character_type

    function normalize_character_type_param(raw_type, has_kind, kind_value, &
            character_length_expr) result(type_str)
        character(len=*), intent(in) :: raw_type
        logical, intent(in) :: has_kind
        integer, intent(in) :: kind_value
        character(len=*), intent(in), optional :: character_length_expr
        character(len=:), allocatable :: type_str
        character(len=:), allocatable :: trimmed
        character(len=:), allocatable :: length_spec
        logical :: has_length
        logical :: needs_post_process

        call preprocess_character_type(raw_type, trimmed, has_length, length_spec, &
            needs_post_process, type_str)
        if (.not. needs_post_process) return

        if (present(character_length_expr)) then
            if (len_trim(character_length_expr) > 0) then
                has_length = .true.
                length_spec = character_length_expr
            end if
        end if

        call ensure_character_length_from_kind(has_kind, kind_value, has_length, &
            length_spec)
        call finalize_character_type(has_length, length_spec, type_str)
    end function normalize_character_type_param

end module codegen_character_normalization
