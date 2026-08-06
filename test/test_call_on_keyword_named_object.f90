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
    call check_keyword_named_entity_in_declaration()
    call check_assignment_to_keyword_named_variable()

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

    subroutine check_keyword_named_entity_in_declaration()
        !! `real :: error, work(n)` declares two entities. Treating the
        !! keyword-named first one as the end of the statement left the rest of
        !! the entity list unconsumed.
        type(compiler_frontend_result_t) :: result
        character(:), allocatable :: source, code

        source = 'subroutine s(n)'//new_line('a')// &
            '    integer, intent(in) :: n'//new_line('a')// &
            '    real :: error, work(n)'//new_line('a')// &
            '    error = 0.0'//new_line('a')// &
            '    work = 1.0'//new_line('a')// &
            'end subroutine s'
        call compile_ok(source, result, 'keyword-named declaration entity')
        call emit_fortran(result%arena, result%root_index, code)
        call require(index(code, 'work') > 0, &
            'the entity after a keyword-named one survives')
    end subroutine check_keyword_named_entity_in_declaration

    subroutine check_assignment_to_keyword_named_variable()
        !! Any keyword followed by `=` is a variable of that name.
        type(compiler_frontend_result_t) :: result
        character(:), allocatable :: source, code

        call assign_inside_loop('file', result, code)
        call require(index(code, 'file') > 0, 'file is assignable')
        call assign_inside_loop('pure', result, code)
        call require(index(code, 'pure') > 0, 'pure is assignable')
        call assign_inside_loop('external', result, code)
        call require(index(code, 'external') > 0, 'external is assignable')
        call assign_construct_array(result, code)
        call require(index(code, 'block') > 0, &
            'a subscripted construct-keyword array is assignable')
        call assign_element(result, code)
        call require(index(code, 'precision') > 0, &
            'an element of a keyword-named array is assignable')
    end subroutine check_assignment_to_keyword_named_variable

    subroutine assign_construct_array(result, code)
        !! `block(:, 1) = column` inside a procedure body. The statement-span
        !! scanner used to take this for a BLOCK construct and hunt for an
        !! `end block` that is not there, swallowing the rest of the procedure.
        type(compiler_frontend_result_t), intent(out) :: result
        character(:), allocatable, intent(out) :: code
        character(:), allocatable :: source

        source = 'program p'//new_line('a')// &
            'contains'//new_line('a')// &
            '    subroutine s()'//new_line('a')// &
            '        real :: block(4, 2), column(4)'//new_line('a')// &
            '        column = 1.0'//new_line('a')// &
            '        block(:, 1) = column'//new_line('a')// &
            '        block(1, 2) = 2.0'//new_line('a')// &
            '    end subroutine s'//new_line('a')// &
            'end program p'
        call compile_ok(source, result, 'subscripted block assignment')
        call emit_fortran(result%arena, result%root_index, code)
    end subroutine assign_construct_array

    subroutine assign_element(result, code)
        !! The designator may be indexed: `precision(i, j) = x`.
        type(compiler_frontend_result_t), intent(out) :: result
        character(:), allocatable, intent(out) :: code
        character(:), allocatable :: source

        source = 'program p'//new_line('a')// &
            '    real :: precision(2, 2)'//new_line('a')// &
            '    integer :: i, j'//new_line('a')// &
            '    do i = 1, 2'//new_line('a')// &
            '        do j = 1, 2'//new_line('a')// &
            '            precision(i, j) = 1.0'//new_line('a')// &
            '        end do'//new_line('a')// &
            '    end do'//new_line('a')// &
            'end program p'
        call compile_ok(source, result, 'indexed keyword-named assignment')
        call emit_fortran(result%arena, result%root_index, code)
    end subroutine assign_element

    subroutine assign_inside_loop(name, result, code)
        character(len=*), intent(in) :: name
        type(compiler_frontend_result_t), intent(out) :: result
        character(:), allocatable, intent(out) :: code
        character(:), allocatable :: source

        source = 'program p'//new_line('a')// &
            '    integer :: i, '//name//new_line('a')// &
            '    do i = 1, 2'//new_line('a')// &
            '        '//name//' = i'//new_line('a')// &
            '    end do'//new_line('a')// &
            'end program p'
        call compile_ok(source, result, 'assignment to '//name)
        call emit_fortran(result%arena, result%root_index, code)
    end subroutine assign_inside_loop

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
