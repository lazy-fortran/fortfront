program test_issue_2934_pointer_target_marking
    ! Issue #2934: the pointer-target standardizer marked every name appearing
    ! in a pointer-assignment target, including derived-type component names and
    ! entities that are themselves POINTER. That emitted
    !   type(node_type), pointer, target :: next => null()
    ! inside a type definition and TARGET+POINTER on dummy arguments, both of
    ! which gfortran rejects. The parameter standardizer additionally invented
    ! INTENT(IN) for the POINTER dummy of a PURE procedure, which makes the
    ! pointer assignment in its body illegal (F2018 C1583).
    use, intrinsic :: iso_fortran_env, only: error_unit
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use string_utils_mod, only: to_lower
    use transformation_api, only: transform_with_context, transform_context_t
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg, lowered
    type(transform_context_t) :: context

    call read_example('examples/f90/impure_assignment_2_valid.f90', source)

    context%input_mode = INPUT_MODE_STANDARD
    context%has_filename = .true.
    context%source_name = 'impure_assignment_2_valid'

    call transform_with_context(source, output, error_msg, context)
    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: unexpected error: '//trim(error_msg)
        error stop 1
    end if
    lowered = to_lower(output)

    if (index(lowered, ', target') > 0 .or. index(lowered, 'target ::') > 0) then
        write (error_unit, '(A)') 'FAIL: TARGET invented for pointer entities'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    if (index(lowered, 'intent(in)') > 0) then
        write (error_unit, '(A)') &
            'FAIL: INTENT(IN) invented for a POINTER dummy of a PURE procedure'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    call check_plain_target_still_marked()

    print *, 'PASS: issue #2934 pointer target marking'

contains

    include '../common/read_example.inc'

    subroutine check_plain_target_still_marked()
        character(len=:), allocatable :: src, out, err

        src = 'program pt'//new_line('a')// &
            '    implicit none'//new_line('a')// &
            '    integer, pointer :: p'//new_line('a')// &
            '    integer :: a'//new_line('a')// &
            '    a = 1'//new_line('a')// &
            '    p => a'//new_line('a')// &
            'end program pt'//new_line('a')

        call transform_lazy_fortran_string(src, out, err)
        if (len_trim(err) > 0) then
            write (error_unit, '(A)') 'FAIL: unexpected error: '//trim(err)
            error stop 1
        end if
        if (index(to_lower(out), ', target') == 0) then
            write (error_unit, '(A)') &
                'FAIL: plain pointer target lost its TARGET attribute'
            write (error_unit, '(A)') trim(out)
            error stop 1
        end if
    end subroutine check_plain_target_still_marked

end program test_issue_2934_pointer_target_marking
