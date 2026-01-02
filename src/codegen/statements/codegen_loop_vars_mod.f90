module codegen_loop_vars_mod
    implicit none
    private
    public :: add_loop_variable_decls

contains

    pure logical function is_var_declared_in_code(code, var_name) &
        result(declared)
        character(len=*), intent(in) :: code
        character(len=*), intent(in) :: var_name
        integer :: search_start, decl_pos_rel, decl_pos_abs, var_search_start
        integer :: var_pos_rel, var_pos_abs, name_len
        character(len=1) :: prev_char, next_char
        character(len=:), allocatable :: name_trimmed

        declared = .false.
        name_trimmed = trim(adjustl(var_name))
        name_len = len_trim(name_trimmed)
        if (name_len == 0) return

        search_start = 1
        do while (search_start <= len(code))
            decl_pos_rel = index(code(search_start:), "integer ::")
            if (decl_pos_rel == 0) exit
            decl_pos_abs = search_start + decl_pos_rel - 1
            var_search_start = decl_pos_abs + 10
            do while (var_search_start <= len(code))
                var_pos_rel = index(code(var_search_start:), name_trimmed)
                if (var_pos_rel == 0) exit
                var_pos_abs = var_search_start + var_pos_rel - 1
                prev_char = ' '
                if (var_pos_abs > 1) prev_char = code(var_pos_abs - 1:var_pos_abs - 1)
                next_char = ' '
                if (var_pos_abs + name_len <= len(code)) then
                    next_char = code(var_pos_abs + name_len:var_pos_abs + name_len)
                end if
                if ((prev_char == ' ' .or. prev_char == ',' .or. &
                     prev_char == ':') .and. &
                    (next_char == ' ' .or. next_char == ',' .or. &
                     next_char == new_line('A') .or. next_char == '(' .or. &
                     next_char == char(13))) then
                    declared = .true.
                    return
                end if
                var_search_start = var_pos_abs + 1
            end do
            search_start = decl_pos_abs + 10
        end do
    end function is_var_declared_in_code

    subroutine add_loop_variable_decls(code, body_code)
        character(len=:), allocatable, intent(inout) :: code
        character(len=:), allocatable, intent(inout) :: body_code
        character(len=:), allocatable :: loop_vars(:)
        integer :: n_vars

        if (len(body_code) == 0) return

        allocate (character(len=32) :: loop_vars(20))
        loop_vars = ""
        n_vars = 0

        call scan_for_loop_variables(body_code, loop_vars, n_vars)
        call inject_loop_var_declarations(code, body_code, loop_vars, n_vars)

        deallocate (loop_vars)
    end subroutine add_loop_variable_decls

    subroutine scan_for_loop_variables(body_code, loop_vars, n_vars)
        character(len=*), intent(in) :: body_code
        character(len=*), intent(inout) :: loop_vars(:)
        integer, intent(inout) :: n_vars
        integer :: pos, start_pos, end_pos

        pos = 1
        do while (pos <= len(body_code))
            start_pos = index(body_code(pos:), "= (/(")
            if (start_pos == 0) start_pos = index(body_code(pos:), "= (/ (")
            if (start_pos == 0) then
                start_pos = index(body_code(pos:), "= [(")
                if (start_pos > 0) then
                    start_pos = pos + start_pos - 1
                    end_pos = index(body_code(start_pos:), ")]")
                    if (end_pos > 0) then
                        end_pos = start_pos + end_pos - 1
                        call extract_loop_vars_from_section( &
                            body_code(start_pos:end_pos), loop_vars, n_vars)
                    end if
                    pos = start_pos + 3
                else
                    exit
                end if
            else
                start_pos = pos + start_pos - 1
                end_pos = index(body_code(start_pos:), "/)")
                if (end_pos > 0) then
                    end_pos = start_pos + end_pos - 1
                    call extract_loop_vars_from_section( &
                        body_code(start_pos:end_pos), loop_vars, n_vars)
                end if
                pos = start_pos + 5
            end if
        end do
    end subroutine scan_for_loop_variables

    subroutine inject_loop_var_declarations(code, body_code, loop_vars, n_vars)
        character(len=:), allocatable, intent(inout) :: code
        character(len=:), allocatable, intent(inout) :: body_code
        character(len=*), intent(in) :: loop_vars(:)
        integer, intent(in) :: n_vars
        integer :: impl_pos

        if (n_vars == 0 .and. .not. (index(body_code, "[(") > 0 .and. &
                                     index(body_code, ")]") > 0)) return

        impl_pos = index(body_code, "implicit none")
        if (impl_pos > 0) then
            call inject_decls_after_implicit(body_code, impl_pos, loop_vars, n_vars)
        else
            call inject_decls_before_body(code, body_code, loop_vars, n_vars)
        end if
    end subroutine inject_loop_var_declarations

    subroutine inject_decls_after_implicit(body_code, impl_pos, loop_vars, n_vars)
        character(len=:), allocatable, intent(inout) :: body_code
        integer, intent(in) :: impl_pos
        character(len=*), intent(in) :: loop_vars(:)
        integer, intent(in) :: n_vars
        integer :: insert_pos, i
        character(len=:), allocatable :: before_code, after_code, name_buf
        logical :: already_declared

        insert_pos = impl_pos + 13
        do while (insert_pos <= len(body_code))
            if (body_code(insert_pos:insert_pos) == new_line('A')) then
                insert_pos = insert_pos + 1
                exit
            end if
            insert_pos = insert_pos + 1
        end do

        before_code = body_code(1:insert_pos - 1)
        after_code = body_code(insert_pos:)

        if (n_vars > 0) then
            do i = 1, n_vars
                name_buf = trim(loop_vars(i))
                already_declared = is_var_declared_in_code(body_code, name_buf)
                if (.not. already_declared) then
                    before_code = before_code // "    integer :: " // &
                                  name_buf // new_line('A')
                end if
            end do
        else
            call add_default_loop_var_if_needed(body_code, before_code)
        end if

        body_code = before_code // after_code
    end subroutine inject_decls_after_implicit

    subroutine inject_decls_before_body(code, body_code, loop_vars, n_vars)
        character(len=:), allocatable, intent(inout) :: code
        character(len=*), intent(in) :: body_code
        character(len=*), intent(in) :: loop_vars(:)
        integer, intent(in) :: n_vars
        integer :: i
        character(len=:), allocatable :: name_buf
        logical :: already_declared

        if (n_vars > 0) then
            do i = 1, n_vars
                name_buf = trim(loop_vars(i))
                already_declared = is_var_declared_in_code(body_code, name_buf)
                if (.not. already_declared) then
                    already_declared = is_var_declared_in_code(code, name_buf)
                end if
                if (.not. already_declared) then
                    code = code // "    integer :: " // name_buf // new_line('A')
                end if
            end do
        else
            call add_default_loop_var_to_code(code, body_code)
        end if
    end subroutine inject_decls_before_body

    subroutine add_default_loop_var_if_needed(body_code, before_code)
        character(len=*), intent(in) :: body_code
        character(len=:), allocatable, intent(inout) :: before_code

        if (index(body_code, "[(") > 0 .and. index(body_code, ")]") > 0) then
            if (.not. is_var_declared_in_code(body_code, "i")) then
                before_code = before_code // "    integer :: i" // new_line('A')
            end if
        end if
    end subroutine add_default_loop_var_if_needed

    subroutine add_default_loop_var_to_code(code, body_code)
        character(len=:), allocatable, intent(inout) :: code
        character(len=*), intent(in) :: body_code

        if (index(body_code, "[(") > 0 .and. index(body_code, ")]") > 0) then
            if (.not. is_var_declared_in_code(body_code, "i") .and. &
                .not. is_var_declared_in_code(code, "i")) then
                code = code // "    integer :: i" // new_line('A')
            end if
        end if
    end subroutine add_default_loop_var_to_code

    subroutine extract_loop_vars_from_section(section, loop_vars, n_vars)
        character(len=*), intent(in) :: section
        character(len=*), intent(inout) :: loop_vars(:)
        integer, intent(inout) :: n_vars
        integer :: pos, eq_pos

        pos = 1
        do
            eq_pos = find_next_equal(section, pos)
            if (eq_pos <= 0) exit
            call try_register_loop_var(section, eq_pos, loop_vars, n_vars)
            pos = eq_pos + 1
        end do
    end subroutine extract_loop_vars_from_section

    integer function find_next_equal(section, start_pos) result(position)
        character(len=*), intent(in) :: section
        integer, intent(in) :: start_pos
        integer :: local_pos

        position = 0
        if (start_pos < 1 .or. start_pos > len_trim(section)) return

        local_pos = index(section(start_pos:), "=")
        if (local_pos == 0) return

        position = start_pos + local_pos - 1
    end function find_next_equal

    subroutine try_register_loop_var(section, eq_pos, loop_vars, n_vars)
        character(len=*), intent(in) :: section
        integer, intent(in) :: eq_pos
        character(len=*), intent(inout) :: loop_vars(:)
        integer, intent(inout) :: n_vars
        character(len=:), allocatable :: var_name

        var_name = extract_loop_var_name(section, eq_pos)
        if (len_trim(var_name) == 0) return
        if (len_trim(var_name) > 8) return
        if (.not. has_loop_range_after(section, eq_pos)) return

        call add_loop_variable(loop_vars, n_vars, var_name)
    end subroutine try_register_loop_var

    function extract_loop_var_name(section, eq_pos) result(var_name)
        character(len=*), intent(in) :: section
        integer, intent(in) :: eq_pos
        character(len=:), allocatable :: var_name
        integer :: i, start_pos

        var_name = ""
        if (eq_pos <= 1) return

        start_pos = eq_pos - 1
        do i = start_pos, 1, -1
            if (section(i:i) == ' ' .or. section(i:i) == ',' .or. &
                section(i:i) == '(') then
                exit
            end if
            start_pos = i - 1
        end do

        var_name = adjustl(trim(section(start_pos + 1:eq_pos - 1)))
    end function extract_loop_var_name

    logical function has_loop_range_after(section, eq_pos) result(has_range)
        character(len=*), intent(in) :: section
        integer, intent(in) :: eq_pos

        has_range = index(section(eq_pos + 1:), ",") > 0
    end function has_loop_range_after

    subroutine add_loop_variable(loop_vars, n_vars, var_name)
        character(len=*), intent(inout) :: loop_vars(:)
        integer, intent(inout) :: n_vars
        character(len=*), intent(in) :: var_name
        integer :: i

        do i = 1, n_vars
            if (trim(loop_vars(i)) == trim(var_name)) return
        end do

        if (n_vars >= size(loop_vars)) return

        n_vars = n_vars + 1
        loop_vars(n_vars) = trim(var_name)
    end subroutine add_loop_variable
end module codegen_loop_vars_mod
