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
            call ensure_output_unit_use(code)
        end if

        call add_loop_variable_decls(code, body_code)

        code = code // body_code
    end subroutine append_program_body

    subroutine ensure_output_unit_use(code)
        character(len=:), allocatable, intent(inout) :: code
        logical :: has_iso_line

        has_iso_line = try_augment_existing_iso_use(code)
        if (.not. has_iso_line) call insert_new_iso_use(code)
    end subroutine ensure_output_unit_use

    logical function try_augment_existing_iso_use(code) result(found_iso)
        character(len=:), allocatable, intent(inout) :: code
        integer :: search_pos, iso_pos

        found_iso = .false.
        search_pos = 1

        do
            iso_pos = index(code(search_pos:), 'iso_fortran_env')
            if (iso_pos == 0) exit
            iso_pos = search_pos + iso_pos - 1

            call augment_iso_line_at_position(code, iso_pos, search_pos, found_iso)
            if (found_iso) exit

            if (search_pos <= 0 .or. search_pos > len(code)) exit
        end do
    end function try_augment_existing_iso_use

    subroutine augment_iso_line_at_position(code, iso_pos, search_pos, found_iso)
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(in) :: iso_pos
        integer, intent(inout) :: search_pos
        logical, intent(inout) :: found_iso
        integer :: line_start, line_end
        logical :: iso_has_only, iso_has_output
        character(len=:), allocatable :: iso_line

        call find_line_bounds(code, iso_pos, line_start, line_end)
        found_iso = .true.

        if (line_end > len(code)) then
            iso_line = code(line_start:)
        else
            iso_line = code(line_start:line_end - 1)
        end if

        iso_has_only = index(to_lower(iso_line), 'only:') > 0
        iso_has_output = index(to_lower(iso_line), 'output_unit') > 0

        if (iso_has_only .and. .not. iso_has_output) then
            call inject_output_unit_into_line(code, line_start, line_end, iso_line)
            iso_has_output = .true.
        end if

        if (.not. iso_has_only .or. iso_has_output) then
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

    subroutine inject_output_unit_into_line(code, line_start, line_end, iso_line)
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(in) :: line_start, line_end
        character(len=*), intent(in) :: iso_line
        character(len=:), allocatable :: prefix, suffix, modified_line

        call split_code_at_line(code, line_start, line_end, prefix, suffix)
        modified_line = append_output_unit_to_iso_line(iso_line)
        code = prefix // modified_line // new_line('A') // suffix
    end subroutine inject_output_unit_into_line

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

    function append_output_unit_to_iso_line(iso_line) result(modified_line)
        character(len=*), intent(in) :: iso_line
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

        modified_line = trimmed_line // ', output_unit'
        if (len_trim(iso_comment) > 0) then
            modified_line = modified_line // ' ' // iso_comment
        end if
    end function append_output_unit_to_iso_line

    subroutine insert_new_iso_use(code)
        character(len=:), allocatable, intent(inout) :: code
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
               '    use, intrinsic :: iso_fortran_env, only: output_unit' // &
               new_line('A') // suffix
    end subroutine insert_new_iso_use

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
