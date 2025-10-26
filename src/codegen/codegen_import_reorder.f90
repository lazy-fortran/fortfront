module codegen_import_reorder
    use string_types, only: string_t
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: reorder_import_lines
    public :: is_import_statement_line

contains

    subroutine reorder_import_lines(text)
        character(len=:), allocatable, intent(inout) :: text
        type(string_t), allocatable :: lines(:)
        type(string_t), allocatable :: imports(:)
        type(string_t), allocatable :: others(:)
        integer :: total_lines
        integer :: import_count
        integer :: other_count
        logical :: has_trailing_newline

        if (.not. allocated(text)) return
        if (len(text) == 0) return

        call split_text_lines(text, lines, total_lines, has_trailing_newline)
        if (total_lines == 0) return

        call partition_import_lines(lines, total_lines, imports, import_count, &
                                    others, other_count)
        if (import_count == 0) return

        call rebuild_lines_with_imports(text, imports, import_count, others, &
                                        other_count, has_trailing_newline)
    end subroutine reorder_import_lines

    subroutine split_text_lines(text, lines, total_lines, has_trailing_newline)
        character(len=:), allocatable, intent(in) :: text
        type(string_t), allocatable, intent(out) :: lines(:)
        integer, intent(out) :: total_lines
        logical, intent(out) :: has_trailing_newline
        integer :: len_text
        integer :: idx_line
        integer :: start_pos
        integer :: line_idx
        character(len=1) :: nl

        nl = new_line('A')
        total_lines = 0
        has_trailing_newline = .false.
        len_text = len(text)
        if (len_text == 0) return

        has_trailing_newline = (text(len_text:len_text) == nl)
        start_pos = 1
        do idx_line = 1, len_text
            if (text(idx_line:idx_line) == nl) then
                total_lines = total_lines + 1
                start_pos = idx_line + 1
            end if
        end do
        if (start_pos <= len_text) total_lines = total_lines + 1
        if (total_lines == 0) return

        allocate (lines(total_lines))
        start_pos = 1
        line_idx = 0
        do idx_line = 1, len_text
            if (text(idx_line:idx_line) == nl) then
                line_idx = line_idx + 1
                call assign_slice(lines(line_idx), text, start_pos, idx_line - 1)
                start_pos = idx_line + 1
            end if
        end do
        if (start_pos <= len_text) then
            line_idx = line_idx + 1
            call assign_slice(lines(line_idx), text, start_pos, len_text)
        end if
    end subroutine split_text_lines

    subroutine partition_import_lines(lines, total_lines, imports, import_count, &
                                      others, other_count)
        type(string_t), intent(in) :: lines(:)
        integer, intent(in) :: total_lines
        type(string_t), allocatable, intent(out) :: imports(:)
        type(string_t), allocatable, intent(out) :: others(:)
        integer, intent(out) :: import_count
        integer, intent(out) :: other_count
        integer :: line_idx
        character(len=:), allocatable :: line_text
        character(len=:), allocatable :: trimmed

        allocate (imports(total_lines))
        allocate (others(total_lines))
        import_count = 0
        other_count = 0

        do line_idx = 1, total_lines
            if (allocated(lines(line_idx)%s)) then
                line_text = lines(line_idx)%s
            else
                line_text = ""
            end if
            trimmed = adjustl(line_text)
            if (len_trim(trimmed) == 0) then
                other_count = other_count + 1
                others(other_count) = lines(line_idx)
            else
                trimmed = to_lower(trim(trimmed))
                if (is_import_statement_line(trimmed)) then
                    import_count = import_count + 1
                    imports(import_count) = lines(line_idx)
                    cycle
                end if
                other_count = other_count + 1
                others(other_count) = lines(line_idx)
            end if
        end do
    end subroutine partition_import_lines

    logical function is_import_statement_line(text) result(is_import)
        character(len=*), intent(in) :: text
        integer :: len_line
        character(len=1) :: next_char

        is_import = .false.
        len_line = len(text)
        if (len_line < 6) return
        if (text(1:6) /= "import") return
        if (len_line == 6) then
            is_import = .true.
            return
        end if

        next_char = text(7:7)
        select case (next_char)
        case (" ", achar(9), ",")
            is_import = .true.
        case (":")
            if (len_line >= 8) then
                if (text(8:8) == ":") is_import = .true.
            end if
        end select
    end function is_import_statement_line

    subroutine rebuild_lines_with_imports(text, imports, import_count, others, &
                                          other_count, has_trailing_newline)
        character(len=:), allocatable, intent(inout) :: text
        type(string_t), intent(in) :: imports(:)
        type(string_t), intent(in) :: others(:)
        integer, intent(in) :: import_count
        integer, intent(in) :: other_count
        logical, intent(in) :: has_trailing_newline
        integer :: line_idx
        integer :: total
        character(len=:), allocatable :: line_text
        character(len=1) :: nl

        nl = new_line('A')
        total = import_count + other_count
        text = ""
        do line_idx = 1, total
            if (line_idx <= import_count) then
                if (allocated(imports(line_idx)%s)) then
                    line_text = imports(line_idx)%s
                else
                    line_text = ""
                end if
            else
                if (allocated(others(line_idx - import_count)%s)) then
                    line_text = others(line_idx - import_count)%s
                else
                    line_text = ""
                end if
            end if
            text = text // line_text
            if (line_idx < total) text = text // nl
        end do
        if (has_trailing_newline) text = text // nl
    end subroutine rebuild_lines_with_imports

    subroutine assign_slice(dest, source, start_pos, end_pos)
        type(string_t), intent(out) :: dest
        character(len=:), allocatable, intent(in) :: source
        integer, intent(in) :: start_pos
        integer, intent(in) :: end_pos

        if (end_pos >= start_pos) then
            dest = source(start_pos:end_pos)
        else
            dest = ""
        end if
    end subroutine assign_slice

end module codegen_import_reorder
