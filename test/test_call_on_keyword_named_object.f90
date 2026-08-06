program test_call_on_keyword_named_object
    !! Fortran keywords are not reserved, so a variable may be called
    !! `operator`, `data` or `file`, and a type-bound call on it is legal.
    !!
    !! The CALL parser used to require an identifier token for the object,
    !! which left the whole statement unconsumed for a keyword-classified
    !! name. The damage showed up far away: the next construct absorbed the
    !! remainder of the program unit, so a following DO loop swallowed the
    !! contained procedures and the frontend rejected `end program`.
    !!
    !! The checks are on the emitted code, not on the parser's internals: the
    !! call must survive with its designator intact and the statements after
    !! it must still be there.
    use fortfront, only: compile_frontend_from_string, &
        compiler_frontend_options_t, compiler_frontend_result_t, &
        INPUT_MODE_STANDARD, emit_fortran
    implicit none

    call check_call_then_loop('operator')
    call check_call_then_loop('data')
    call check_call_then_loop('file')
    call check_contained_procedure_survives()

    print *, 'PASS: calls on keyword-named objects keep the program unit'

contains

    subroutine check_call_then_loop(object_name)
        character(len=*), intent(in) :: object_name
        type(compiler_frontend_result_t) :: result
        character(:), allocatable :: source, code

        source = 'program p'//new_line('a')// &
            '    call '//object_name//'%initialize(a, b)'//new_line('a')// &
            '    do i = 1, 2'//new_line('a')// &
            '        total = total + i'//new_line('a')// &
            '    end do'//new_line('a')// &
            'end program p'
        call compile_ok(source, result, object_name)
        call emit_fortran(result%arena, result%root_index, code)
        call require(index(code, object_name//'%initialize') > 0, &
            'the call designator survives for '//object_name)
        call require(index(code, 'total + i') > 0, &
            'the loop after the call survives for '//object_name)
    end subroutine check_call_then_loop

    subroutine check_contained_procedure_survives()
        type(compiler_frontend_result_t) :: result
        character(:), allocatable :: source, code

        source = 'program p'//new_line('a')// &
            '    call operator%initialize(a, b)'//new_line('a')// &
            '    do i = 1, 2'//new_line('a')// &
            '        total = total + i'//new_line('a')// &
            '    end do'//new_line('a')// &
            'contains'//new_line('a')// &
            '    subroutine direct_value(row)'//new_line('a')// &
            '        integer, intent(in) :: row'//new_line('a')// &
            '        print *, row'//new_line('a')// &
            '    end subroutine direct_value'//new_line('a')// &
            'end program p'
        call compile_ok(source, result, 'contained procedure')
        call emit_fortran(result%arena, result%root_index, code)
        call require(index(code, 'direct_value') > 0, &
            'the contained procedure survives the call')
    end subroutine check_contained_procedure_survives

    subroutine compile_ok(source, result, label)
        character(len=*), intent(in) :: source, label
        type(compiler_frontend_result_t), intent(out) :: result
        type(compiler_frontend_options_t) :: options

        options = compiler_frontend_options_t()
        options%run_semantics = .false.
        options%input_mode = INPUT_MODE_STANDARD
        call compile_frontend_from_string(source, result, options)
        if (.not. result%parse_ok) then
            print *, 'FAIL: frontend rejected '//label//': ', &
                trim(result%diagnostic_text)
            error stop 1
        end if
    end subroutine compile_ok

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (condition) return
        print *, 'FAIL: ', message
        error stop 1
    end subroutine require

end program test_call_on_keyword_named_object
