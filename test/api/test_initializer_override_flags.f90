program test_initializer_override_flags
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_compiler, only: ast_arena_t, &
        compiler_frontend_options_t, compiler_frontend_result_t, &
        compile_frontend_from_string, INPUT_MODE_STANDARD, &
        is_declaration_node, &
        get_declaration_var_name, get_declaration_has_initializer, &
        get_declaration_initializer_was_overridden, &
        get_declaration_shape_was_overridden
    implicit none

    call test_parameter_overrides_initializer()
    call test_dimension_overrides_initializer()
    call test_valid_parameter_not_flagged()
    call test_valid_dimension_not_flagged()
    print *, 'PASS: initializer/shape override flags'
contains

    subroutine test_parameter_overrides_initializer()
        type(compiler_frontend_result_t) :: result
        integer :: decl_index

        call compile_standard( &
            'program p'//new_line('a')// &
            '  integer :: n = 5'//new_line('a')// &
            '  parameter (n = 42)'//new_line('a')// &
            '  print *, n'//new_line('a')// &
            'end program p', result)

        decl_index = find_declaration(result%arena, 'n')
        if (.not. get_declaration_initializer_was_overridden(result%arena, &
            decl_index)) then
            call fail('PARAMETER should flag an overridden initializer')
        end if
    end subroutine test_parameter_overrides_initializer

    subroutine test_dimension_overrides_initializer()
        type(compiler_frontend_result_t) :: result
        integer :: decl_index

        call compile_standard( &
            'program p'//new_line('a')// &
            '  integer :: m = 7'//new_line('a')// &
            '  dimension :: m(3)'//new_line('a')// &
            '  print *, m'//new_line('a')// &
            'end program p', result)

        decl_index = find_declaration(result%arena, 'm')
        if (.not. get_declaration_shape_was_overridden(result%arena, &
            decl_index)) then
            call fail('DIMENSION should flag an overridden shape')
        end if
    end subroutine test_dimension_overrides_initializer

    subroutine test_valid_parameter_not_flagged()
        type(compiler_frontend_result_t) :: result
        integer :: decl_index

        call compile_standard( &
            'program p'//new_line('a')// &
            '  integer :: n'//new_line('a')// &
            '  parameter (n = 42)'//new_line('a')// &
            'end program p', result)

        decl_index = find_declaration(result%arena, 'n')
        if (get_declaration_initializer_was_overridden(result%arena, &
            decl_index)) then
            call fail('valid PARAMETER must not flag an overridden initializer')
        end if
    end subroutine test_valid_parameter_not_flagged

    subroutine test_valid_dimension_not_flagged()
        type(compiler_frontend_result_t) :: result
        integer :: decl_index

        call compile_standard( &
            'program p'//new_line('a')// &
            '  integer :: m'//new_line('a')// &
            '  dimension :: m(3)'//new_line('a')// &
            'end program p', result)

        decl_index = find_declaration(result%arena, 'm')
        if (get_declaration_shape_was_overridden(result%arena, &
            decl_index)) then
            call fail('valid DIMENSION must not flag an overridden shape')
        end if
    end subroutine test_valid_dimension_not_flagged

    subroutine compile_standard(source, result)
        character(len=*), intent(in) :: source
        type(compiler_frontend_result_t), intent(out) :: result
        type(compiler_frontend_options_t) :: options

        options = compiler_frontend_options_t()
        options%input_mode = INPUT_MODE_STANDARD
        options%run_semantics = .true.
        call compile_frontend_from_string(source, result, options)
        if (result%success()) return
        write (error_unit, '(A)') 'FAIL: frontend rejected test source'
        if (allocated(result%error_msg)) write (error_unit, '(A)') result%error_msg
        error stop 1
    end subroutine compile_standard

    integer function find_declaration(arena, expected_name) result(index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: expected_name
        character(len=:), allocatable :: name, error_msg
        integer :: i

        index = 0
        do i = 1, arena%size
            if (.not. is_declaration_node(arena, i)) cycle
            call get_declaration_var_name(arena, i, name, error_msg)
            if (len_trim(error_msg) > 0) cycle
            if (trim(name) == trim(expected_name)) then
                index = i
                return
            end if
        end do
        call fail('declaration not found: '//trim(expected_name))
    end function find_declaration

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(A)') 'FAIL: '//trim(message)
        error stop 1
    end subroutine fail

end program test_initializer_override_flags
