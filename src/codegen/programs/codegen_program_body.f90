module codegen_program_body
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use codegen_loop_vars_mod, only: add_loop_variable_decls
    use codegen_grouped_body, only: generate_grouped_body_context
    use string_utils_mod, only: to_lower
    implicit none
    private
    public :: append_program_body

contains

    subroutine append_program_body(arena, node, code, non_use_indices, &
                                   non_use_count, extra_decl_code, &
                                   context_has_executable_before_contains)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(in) :: non_use_indices(:)
        integer, intent(in) :: non_use_count
        character(len=*), intent(in) :: extra_decl_code
        logical, intent(in) :: context_has_executable_before_contains
        character(len=:), allocatable :: body_code

        if (non_use_count <= 0) return

        body_code = generate_grouped_body_with_context( &
                    arena, non_use_indices(1:non_use_count), 1, &
                    context_has_executable_before_contains)

        if (index(body_code, 'output_unit') > 0) then
            call ensure_iso_clause(code, 'output_unit')
        end if

        call maybe_require_dp_kind_use(code, body_code)

        call add_loop_variable_decls(code, body_code)

        code = code // body_code
    end subroutine append_program_body

    subroutine ensure_iso_clause(code, clause)
        character(len=:), allocatable, intent(inout) :: code
        character(len=*), intent(in) :: clause
        logical :: has_clause

        has_clause = try_augment_existing_iso_clause(code, clause)
        if (.not. has_clause) call insert_new_iso_use(code, clause)
    end subroutine ensure_iso_clause

    logical function try_augment_existing_iso_clause(code, clause) result(found_clause)
        character(len=:), allocatable, intent(inout) :: code
        character(len=*), intent(in) :: clause
        integer :: search_pos, iso_pos

        found_clause = .false.
        search_pos = 1

        do
            iso_pos = index(code(search_pos:), 'iso_fortran_env')
            if (iso_pos == 0) exit
            iso_pos = search_pos + iso_pos - 1

            call augment_iso_line_at_position(code, iso_pos, search_pos, found_clause, &
                                              clause)
            if (found_clause) exit

            if (search_pos <= 0 .or. search_pos > len(code)) exit
        end do
    end function try_augment_existing_iso_clause

    subroutine augment_iso_line_at_position(code, iso_pos, search_pos, found_clause, &
                                            clause)
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(in) :: iso_pos
        integer, intent(inout) :: search_pos
        logical, intent(inout) :: found_clause
        character(len=*), intent(in) :: clause
        integer :: line_start, line_end
        logical :: iso_has_only, iso_has_clause
        character(len=:), allocatable :: iso_line

        call find_line_bounds(code, iso_pos, line_start, line_end)

        if (line_end > len(code)) then
            iso_line = code(line_start:)
        else
            iso_line = code(line_start:line_end - 1)
        end if

        iso_has_only = index(to_lower(iso_line), 'only:') > 0
        iso_has_clause = iso_line_has_clause(iso_line, clause)

        if (iso_has_only .and. .not. iso_has_clause) then
            call inject_clause_into_line(code, line_start, line_end, iso_line, clause)
            iso_has_clause = .true.
        end if

        found_clause = iso_has_clause
        if (found_clause) then
            search_pos = -1
        else if (line_end <= len(code)) then
            search_pos = line_end + 1
        else
            search_pos = -1
        end if
    end subroutine augment_iso_line_at_position

    subroutine find_line_bounds(code, position, line_start, line_end)
        character(len=*), intent(in) :: code
        integer, intent(in) :: position
        integer, intent(out) :: line_start, line_end

        line_start = position
        do while (line_start > 1 .and. code(line_start - 1:line_start - 1) /= &
                  new_line('A'))
            line_start = line_start - 1
        end do

        line_end = position
        do while (line_end <= len(code) .and. code(line_end:line_end) /= &
                  new_line('A'))
            line_end = line_end + 1
        end do
    end subroutine find_line_bounds

    subroutine inject_clause_into_line(code, line_start, line_end, iso_line, clause)
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(in) :: line_start, line_end
        character(len=*), intent(in) :: iso_line
        character(len=*), intent(in) :: clause
        character(len=:), allocatable :: prefix, suffix, modified_line

        call split_code_at_line(code, line_start, line_end, prefix, suffix)
        modified_line = append_clause_to_iso_line(iso_line, clause)
        code = prefix // modified_line // new_line('A') // suffix
    end subroutine inject_clause_into_line

    subroutine split_code_at_line(code, line_start, line_end, prefix, suffix)
        character(len=*), intent(in) :: code
        integer, intent(in) :: line_start, line_end
        character(len=:), allocatable, intent(out) :: prefix, suffix

        if (line_start > 1) then
            prefix = code(1:line_start - 1)
        else
            prefix = ''
        end if

        if (line_end <= len(code)) then
            if (line_end < len(code)) then
                suffix = code(line_end + 1:)
            else
                suffix = ''
            end if
        else
            suffix = ''
        end if
    end subroutine split_code_at_line

    function append_clause_to_iso_line(iso_line, clause) result(modified_line)
        character(len=*), intent(in) :: iso_line
        character(len=*), intent(in) :: clause
        character(len=:), allocatable :: modified_line
        integer :: comment_pos
        character(len=:), allocatable :: trimmed_line, iso_comment

        comment_pos = scan(iso_line, '!')
        if (comment_pos > 0) then
            if (comment_pos > 1) then
                trimmed_line = iso_line(1:comment_pos - 1)
            else
                trimmed_line = ''
            end if
            iso_comment = iso_line(comment_pos:)
        else
            trimmed_line = iso_line
            iso_comment = ''
        end if

        if (len_trim(trimmed_line) > 0) then
            trimmed_line = trimmed_line(1:len_trim(trimmed_line))
        end if

        modified_line = trimmed_line // ', ' // clause
        if (len_trim(iso_comment) > 0) then
            modified_line = modified_line // ' ' // iso_comment
        end if
    end function append_clause_to_iso_line

    subroutine insert_new_iso_use(code, clause)
        character(len=:), allocatable, intent(inout) :: code
        character(len=*), intent(in) :: clause
        integer :: header_end
        character(len=:), allocatable :: prefix, suffix

        header_end = index(code, new_line('A'))
        if (header_end <= 0) header_end = len(code)

        if (header_end > 0) then
            prefix = code(1:header_end)
        else
            prefix = ''
        end if

        if (header_end < len(code)) then
            suffix = code(header_end + 1:)
        else
            suffix = ''
        end if

        code = prefix // &
               '    use, intrinsic :: iso_fortran_env, only: ' // trim(clause) // &
               new_line('A') // suffix
    end subroutine insert_new_iso_use

    pure logical function iso_line_has_clause(iso_line, clause) result(has_clause)
        character(len=*), intent(in) :: iso_line
        character(len=*), intent(in) :: clause
        character(len=:), allocatable :: normalized_line
        character(len=:), allocatable :: normalized_clause

        normalized_line = normalize_clause_target(iso_line)
        normalized_clause = normalize_clause_target(clause)
        has_clause = index(normalized_line, normalized_clause) > 0
    end function iso_line_has_clause

    pure function normalize_clause_target(text) result(normalized)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: normalized
        integer :: i, length_text, pos
        character(len=:), allocatable :: buffer
        character(len=1) :: ch

        length_text = len(text)
        pos = 0
        allocate (character(len=length_text) :: buffer)

        do i = 1, length_text
            ch = text(i:i)
            if (is_whitespace_char(ch)) cycle
            pos = pos + 1
            buffer(pos:pos) = to_lower(ch)
        end do

        if (pos > 0) then
            normalized = buffer(1:pos)
        else
            normalized = ''
        end if
    end function normalize_clause_target

    pure logical function is_whitespace_char(ch) result(is_ws)
        character(len=1), intent(in) :: ch
        integer :: code

        code = iachar(ch)
        is_ws = (code == 9) .or. (code == 32) .or. (code == 10) .or. (code == 13)
    end function is_whitespace_char

    subroutine maybe_require_dp_kind_use(code, body_code)
        character(len=:), allocatable, intent(inout) :: code
        character(len=:), allocatable, intent(in) :: body_code
        logical :: needs_dp

        needs_dp = code_requires_dp_kind(code)
        if (.not. needs_dp) needs_dp = code_requires_dp_kind(body_code)
        if (.not. needs_dp) return

        if (iso_line_has_clause(code, 'dp => real64')) return

        call ensure_iso_clause(code, 'dp => real64')
    end subroutine maybe_require_dp_kind_use

    pure logical function code_requires_dp_kind(text) result(needs_dp)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lowered

        needs_dp = .false.
        if (len_trim(text) == 0) return

        lowered = to_lower(text)

        if (contains_kind_parentheses(lowered)) then
            needs_dp = .true.
            return
        end if

        if (contains_kind_assignment(lowered)) then
            needs_dp = .true.
            return
        end if

        if (contains_dp_literal(lowered)) needs_dp = .true.
    end function code_requires_dp_kind

    pure logical function contains_kind_parentheses(text) result(found)
        character(len=*), intent(in) :: text
        integer :: i, j, n
        found = .false.
        n = len(text)

        do i = 1, n
            if (text(i:i) /= '(') cycle
            j = i + 1
            do while (j <= n)
                if (.not. is_whitespace_char(text(j:j))) exit
                j = j + 1
            end do
            if (j > n .or. j + 1 > n) exit
            if (text(j:j + 1) /= 'dp') cycle
            if (j + 2 <= n) then
                if (is_identifier_char(text(j + 2:j + 2))) cycle
            end if
            found = .true.
            return
        end do
    end function contains_kind_parentheses

    pure logical function contains_kind_assignment(text) result(found)
        character(len=*), intent(in) :: text
        integer :: start, pos, n, idx

        found = .false.
        n = len(text)
        start = 1

        do
            pos = index(text(start:), 'kind')
            if (pos == 0) exit
            pos = start + pos - 1
            idx = pos + 4
            do while (idx <= n)
                if (.not. is_whitespace_char(text(idx:idx))) exit
                idx = idx + 1
            end do
            if (idx > n) then
                start = pos + 4
                cycle
            end if
            if (text(idx:idx) /= '=') then
                start = pos + 4
                cycle
            end if
            idx = idx + 1
            do while (idx <= n)
                if (.not. is_whitespace_char(text(idx:idx))) exit
                idx = idx + 1
            end do
            if (idx > n .or. idx + 1 > n) then
                start = pos + 4
                cycle
            end if
            if (text(idx:idx + 1) /= 'dp') then
                start = pos + 4
                cycle
            end if
            if (idx + 2 <= n) then
                if (is_identifier_char(text(idx + 2:idx + 2))) then
                    start = pos + 4
                    cycle
                end if
            end if
            found = .true.
            return
        end do
    end function contains_kind_assignment

    pure logical function contains_dp_literal(text) result(found)
        character(len=*), intent(in) :: text
        integer :: start, pos, n
        character(len=1) :: prev, prev2, next_char

        found = .false.
        n = len(text)
        start = 1

        do
            pos = index(text(start:), '_dp')
            if (pos == 0) exit
            pos = start + pos - 1
            if (pos <= 1) then
                start = pos + 3
                cycle
            end if

            prev = text(pos - 1:pos - 1)
            if (.not. is_dp_literal_lead(prev)) then
                start = pos + 3
                cycle
            end if

            if (pos >= 2) then
                prev2 = text(pos - 2:pos - 2)
            else
                prev2 = ' '
            end if
            if (is_identifier_char(prev2)) then
                start = pos + 3
                cycle
            end if

            if (pos + 3 <= n) then
                next_char = text(pos + 3:pos + 3)
                if (is_identifier_char(next_char)) then
                    start = pos + 3
                    cycle
                end if
            end if

            found = .true.
            return
        end do
    end function contains_dp_literal

    pure logical function is_dp_literal_lead(ch) result(is_valid)
        character(len=1), intent(in) :: ch
        integer :: code

        code = iachar(ch)
        is_valid = (ch == '.') .or. &
                   (code >= iachar('0') .and. code <= iachar('9'))
    end function is_dp_literal_lead

    pure logical function is_identifier_char(ch) result(is_ident)
        character(len=1), intent(in) :: ch
        integer :: code

        code = iachar(ch)
        is_ident = (code >= iachar('a') .and. code <= iachar('z')) .or. &
                   (code >= iachar('0') .and. code <= iachar('9')) .or. &
                   ch == '_'
    end function is_identifier_char

    function generate_grouped_body_with_context(arena, body_indices, indent, &
                                                has_exec_before_contains) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: indent
        logical, intent(in) :: has_exec_before_contains
        character(len=:), allocatable :: code

        code = generate_grouped_body_context(arena, body_indices, indent, &
                                             has_exec_before_contains)
    end function generate_grouped_body_with_context

end module codegen_program_body
