program test_alternate_return_frontend
    ! Issue #2933: the frontend must represent alternate returns:
    !   * `*<label>` actual arguments
    !   * the `RETURN <scalar-int-expr>` selector
    !   * `*` dummy arguments marked by an explicit attribute
    use fortfront, only: compile_frontend_from_string, &
        compiler_frontend_options_t, &
        compiler_frontend_result_t, INPUT_MODE_STANDARD, &
        get_node_type_at, emit_fortran
    use fortfront_compiler, only: get_alternate_return_label, &
        get_return_selector, &
        is_alternate_return_dummy
    implicit none

    call test_alt_return_actual_argument()
    call test_return_selector()
    call test_plain_return_has_no_selector()
    call test_alternate_return_dummy()

    print *, 'PASS: alternate return representation available'

contains

    subroutine compile_ok(src, result)
        character(len=*), intent(in) :: src
        type(compiler_frontend_result_t), intent(out) :: result
        type(compiler_frontend_options_t) :: options

        options = compiler_frontend_options_t()
        options%run_semantics = .true.
        options%input_mode = INPUT_MODE_STANDARD
        call compile_frontend_from_string(src, result, options)
        if (.not. result%success()) then
            print *, 'FAIL: frontend rejected source: ', &
                trim(result%diagnostic_text)
            error stop 1
        end if
    end subroutine compile_ok

    subroutine test_alt_return_actual_argument()
        type(compiler_frontend_result_t) :: result
        character(:), allocatable :: src, code
        integer :: i, found, label

        src = 'program p'//new_line('a')// &
            '  call s(1, *10)'//new_line('a')// &
            '10 continue'//new_line('a')// &
            'end program p'

        call compile_ok(src, result)

        found = 0
        do i = 1, result%arena%size
            label = get_alternate_return_label(result%arena, i)
            if (label == 0) cycle
            found = found + 1
            if (label /= 10) then
                print *, 'FAIL: wrong alternate return label ', label
                error stop 1
            end if
        end do
        if (found /= 1) then
            print *, 'FAIL: expected one alternate return spec, got ', found
            error stop 1
        end if

        call emit_fortran(result%arena, result%root_index, code)
        if (index(code, '*10') == 0) then
            print *, 'FAIL: emitted code lost alternate return spec:'
            print *, code
            error stop 1
        end if
    end subroutine test_alt_return_actual_argument

    subroutine test_return_selector()
        type(compiler_frontend_result_t) :: result
        character(:), allocatable :: src, code
        logical :: has_selector
        integer :: i, selector_index, found

        src = 'subroutine s(a, *)'//new_line('a')// &
            '  integer, intent(in) :: a'//new_line('a')// &
            '  if (a > 0) return 1'//new_line('a')// &
            '  return'//new_line('a')// &
            'end subroutine s'

        call compile_ok(src, result)

        found = 0
        do i = 1, result%arena%size
            if (trim(get_node_type_at(result%arena, i)) /= 'return_node') cycle
            call get_return_selector(result%arena, i, has_selector, &
                selector_index)
            if (.not. has_selector) cycle
            found = found + 1
            if (selector_index <= 0) then
                print *, 'FAIL: selector index not set'
                error stop 1
            end if
        end do
        if (found /= 1) then
            print *, 'FAIL: expected one return with selector, got ', found
            error stop 1
        end if

        call emit_fortran(result%arena, result%root_index, code)
        if (index(code, 'return 1') == 0) then
            print *, 'FAIL: emitted code lost return selector:'
            print *, code
            error stop 1
        end if
    end subroutine test_return_selector

    subroutine test_plain_return_has_no_selector()
        type(compiler_frontend_result_t) :: result
        character(:), allocatable :: src
        logical :: has_selector
        integer :: i, selector_index, found

        src = 'subroutine s2()'//new_line('a')// &
            '  return'//new_line('a')// &
            'end subroutine s2'

        call compile_ok(src, result)

        found = 0
        do i = 1, result%arena%size
            if (trim(get_node_type_at(result%arena, i)) /= 'return_node') cycle
            call get_return_selector(result%arena, i, has_selector, &
                selector_index)
            found = found + 1
            if (has_selector) then
                print *, 'FAIL: plain RETURN reported a selector'
                error stop 1
            end if
        end do
        if (found /= 1) then
            print *, 'FAIL: expected one plain return, got ', found
            error stop 1
        end if
    end subroutine test_plain_return_has_no_selector

    subroutine test_alternate_return_dummy()
        type(compiler_frontend_result_t) :: result
        character(:), allocatable :: src
        integer :: i, alt_count, plain_count

        src = 'subroutine s3(a, *)'//new_line('a')// &
            '  integer, intent(in) :: a'//new_line('a')// &
            'end subroutine s3'

        call compile_ok(src, result)

        alt_count = 0
        plain_count = 0
        do i = 1, result%arena%size
            if (trim(get_node_type_at(result%arena, i)) /= &
                'parameter_declaration') cycle
            if (is_alternate_return_dummy(result%arena, i)) then
                alt_count = alt_count + 1
            else
                plain_count = plain_count + 1
            end if
        end do
        if (alt_count /= 1) then
            print *, 'FAIL: expected one alternate return dummy, got ', &
                alt_count
            error stop 1
        end if
        if (plain_count /= 1) then
            print *, 'FAIL: expected one ordinary dummy, got ', plain_count
            error stop 1
        end if
    end subroutine test_alternate_return_dummy

end program test_alternate_return_frontend
