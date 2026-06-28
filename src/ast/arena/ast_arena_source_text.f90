module ast_arena_source_text
    use ast_arena_modern, only: ast_arena_t
    use lexer_core, only: normalize_line_endings
    implicit none
    private

    public :: set_source_text, has_source_text, get_source_text
    public :: get_source_line, get_source_range, get_source_range_by_pos

contains

    subroutine set_source_text(arena, source_code)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: source_code

        arena%source_text = normalize_line_endings(source_code)
        call build_source_line_starts(arena)
    end subroutine set_source_text

    pure logical function has_source_text(arena) result(has)
        type(ast_arena_t), intent(in) :: arena

        has = allocated(arena%source_text) .and. &
            allocated(arena%source_line_starts)
    end function has_source_text

    subroutine get_source_text(arena, text, found)
        type(ast_arena_t), intent(in) :: arena
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out), optional :: found

        if (.not. allocated(arena%source_text)) then
            allocate (character(len=0) :: text)
            if (present(found)) found = .false.
            return
        end if

        text = arena%source_text
        if (present(found)) found = .true.
    end subroutine get_source_text

    subroutine get_source_line(arena, line_number, line_text, found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: line_number
        character(len=:), allocatable, intent(out) :: line_text
        logical, intent(out) :: found

        integer :: start_pos, end_pos

        found = .false.
        allocate (character(len=0) :: line_text)

        if (.not. has_source_text(arena)) return
        if (line_number < 1) return
        if (line_number > size(arena%source_line_starts)) return

        call get_line_bounds(arena, line_number, start_pos, end_pos)
        if (start_pos <= 0) return

        if (end_pos < start_pos) then
            found = .true.
            return
        end if

        line_text = arena%source_text(start_pos:end_pos)
        found = .true.
    end subroutine get_source_line

    subroutine get_source_range(arena, start_line, start_col, end_line, end_col, &
            text, found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: start_line, start_col
        integer, intent(in) :: end_line, end_col
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: found

        integer :: start_pos, end_pos
        integer :: src_len

        found = .false.
        allocate (character(len=0) :: text)

        if (.not. has_source_text(arena)) return

        start_pos = line_column_to_pos(arena, start_line, start_col)
        end_pos = line_column_to_pos(arena, end_line, end_col)

        if (start_pos <= 0 .or. end_pos <= 0) return
        if (end_pos < start_pos) return

        src_len = len(arena%source_text)

        if (start_pos == src_len + 1 .and. end_pos == start_pos) then
            found = .true.
            return
        end if

        if (end_pos > src_len) return

        text = arena%source_text(start_pos:end_pos)
        found = .true.
    end subroutine get_source_range

    subroutine get_source_range_by_pos(arena, start_pos, end_pos, text, found)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: start_pos, end_pos
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: found

        integer :: src_len

        found = .false.
        allocate (character(len=0) :: text)

        if (.not. allocated(arena%source_text)) return
        if (start_pos < 1) return
        if (end_pos < start_pos) return

        src_len = len(arena%source_text)

        if (start_pos == src_len + 1 .and. end_pos == start_pos) then
            found = .true.
            return
        end if

        if (end_pos > src_len) return

        text = arena%source_text(start_pos:end_pos)
        found = .true.
    end subroutine get_source_range_by_pos

    subroutine build_source_line_starts(arena)
        type(ast_arena_t), intent(inout) :: arena

        integer :: i, line_count, src_len, line_index
        character :: ch

        if (.not. allocated(arena%source_text)) then
            if (allocated(arena%source_line_starts)) then
                deallocate (arena%source_line_starts)
            end if
            return
        end if

        src_len = len(arena%source_text)
        line_count = 1
        do i = 1, src_len
            ch = arena%source_text(i:i)
            if (ch == new_line('A')) line_count = line_count + 1
        end do

        if (allocated(arena%source_line_starts)) deallocate (arena%source_line_starts)
        allocate (arena%source_line_starts(line_count))

        arena%source_line_starts(1) = 1
        line_index = 2
        do i = 1, src_len
            ch = arena%source_text(i:i)
            if (ch == new_line('A')) then
                arena%source_line_starts(line_index) = i + 1
                line_index = line_index + 1
            end if
        end do
    end subroutine build_source_line_starts

    pure subroutine get_line_bounds(arena, line_number, start_pos, end_pos)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: line_number
        integer, intent(out) :: start_pos, end_pos

        integer :: src_len, next_start

        start_pos = 0
        end_pos = -1

        if (.not. has_source_text(arena)) return
        if (line_number < 1) return
        if (line_number > size(arena%source_line_starts)) return

        src_len = len(arena%source_text)
        start_pos = arena%source_line_starts(line_number)
        if (start_pos > src_len) then
            end_pos = start_pos - 1
            return
        end if

        if (line_number < size(arena%source_line_starts)) then
            next_start = arena%source_line_starts(line_number + 1)
            end_pos = next_start - 2
        else
            end_pos = src_len
        end if

        if (end_pos < start_pos) end_pos = start_pos - 1
    end subroutine get_line_bounds

    pure integer function get_line_length(arena, line_number) result(line_length)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: line_number

        integer :: start_pos, end_pos

        line_length = 0
        call get_line_bounds(arena, line_number, start_pos, end_pos)
        if (start_pos <= 0) return
        if (end_pos < start_pos) return
        line_length = end_pos - start_pos + 1
    end function get_line_length

    pure integer function line_column_to_pos(arena, line_number, column) result(pos)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: line_number, column

        integer :: start_pos, src_len, line_len

        pos = 0
        if (.not. has_source_text(arena)) return
        if (line_number < 1) return
        if (line_number > size(arena%source_line_starts)) return
        if (column < 1) return

        src_len = len(arena%source_text)
        start_pos = arena%source_line_starts(line_number)
        if (start_pos > src_len + 1) return

        line_len = get_line_length(arena, line_number)
        if (line_len == 0) then
            if (column /= 1) return
            pos = start_pos
            return
        end if

        if (column > line_len) return
        pos = start_pos + column - 1
    end function line_column_to_pos

end module ast_arena_source_text
