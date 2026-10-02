program test_scope_stack_identifier_lifetime
    use, intrinsic :: iso_fortran_env, only: error_unit
    use scope_manager, only: scope_stack_t, create_scope_stack
    use type_system_unified, only: mono_type_t, poly_type_t, type_var_t, &
        create_mono_type, create_poly_type, TINT
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, INPUT_MODE_STANDARD
    implicit none
    integer :: i

    do i = 1, 20
        call check_stack_copies()
    end do
    call check_frontend_result_reuse()
    print *, 'PASS: identifier-table ownership across copies and reinitialization'

contains

    subroutine check_stack_copies()
        type(scope_stack_t) :: original, copied, returned
        type(mono_type_t) :: mono
        type(poly_type_t) :: scheme
        type(poly_type_t), allocatable :: found

        call create_scope_stack(original)
        mono = create_mono_type(TINT)
        scheme = create_poly_type([type_var_t ::], mono)
        call original%define('alpha', scheme)
        copied = original
        returned = original%deep_copy()
        call original%define('beta', scheme)
        call copied%lookup('beta', found)
        call require(.not. allocated(found), 'copy does not inherit later definitions')
        call create_scope_stack(original)
        call original%lookup('alpha', found)
        call require(.not. allocated(found), 'reinitialization clears original binding')
        call copied%lookup('alpha', found)
        call require(allocated(found), 'assignment copy survives original reset')
        call returned%lookup('alpha', found)
        call require(allocated(found), 'function copy survives original reset')
        copied = original
        call copied%lookup('alpha', found)
        call require(.not. allocated(found), 'reassignment replaces copied bindings')
    end subroutine check_stack_copies

    subroutine check_frontend_result_reuse()
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result
        integer :: i

        options%input_mode = INPUT_MODE_STANDARD
        do i = 1, 20
            call compile_frontend_from_string('program p'//new_line('a')// &
                'end program p', result, options)
            call require(result%success(), 'empty program accepted on every compile')
        end do
    end subroutine check_frontend_result_reuse

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (condition) return
        write (error_unit, '(a)') 'FAIL: '//message
        stop 1
    end subroutine require

end program test_scope_stack_identifier_lifetime
