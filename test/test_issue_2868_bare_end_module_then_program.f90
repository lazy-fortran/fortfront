program test_issue_2868_bare_end_module_then_program
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_context_t, transform_with_context, &
        & INPUT_MODE_STANDARD
    implicit none

    call verify_bare_end_module_then_program()
    call verify_bare_end_subroutine_then_program()
    call verify_end_module_regression()
    print *, 'PASS: Issue #2868 bare end module keeps following program unit'

contains

    subroutine transform_source(source_code, output_code, name)
        character(len=*), intent(in) :: source_code
        character(len=:), allocatable, intent(out) :: output_code
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: src, error_msg
        type(transform_context_t) :: ctx

        src = source_code
        ctx%input_mode = INPUT_MODE_STANDARD
        ctx%source_name = name
        ctx%has_filename = .true.
        call transform_with_context(src, output_code, error_msg, ctx)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') 'FAIL ('//name//'): '//trim(error_msg)
                error stop 1
            end if
        end if
    end subroutine transform_source

    subroutine verify_bare_end_module_then_program()
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: src

        src = 'module m'//new_line('a')// &
              '    integer :: x'//new_line('a')// &
              'end'//new_line('a')// &
              'program p'//new_line('a')// &
              '    print *, 1'//new_line('a')// &
              'end program'//new_line('a')

        call transform_source(src, output_code, 'issue_2868_module_then_program')
        call assert_has(output_code, 'module m', 'module unit dropped')
        call assert_has(output_code, 'program p', 'program unit dropped after bare end')
        call assert_has(output_code, 'print', 'program body statement dropped')
    end subroutine verify_bare_end_module_then_program

    subroutine verify_bare_end_subroutine_then_program()
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: src

        src = 'subroutine s(x)'//new_line('a')// &
              '    integer :: x'//new_line('a')// &
              '    x = 2'//new_line('a')// &
              'end'//new_line('a')// &
              'program p'//new_line('a')// &
              '    print *, 3'//new_line('a')// &
              'end program'//new_line('a')

        call transform_source(src, output_code, 'issue_2868_subroutine_then_program')
        call assert_has(output_code, 'subroutine s', 'subroutine unit dropped')
        call assert_has(output_code, 'program p', 'program unit dropped after bare end')
        call assert_has(output_code, 'print', 'program body statement dropped')
    end subroutine verify_bare_end_subroutine_then_program

    subroutine verify_end_module_regression()
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: src

        src = 'module m'//new_line('a')// &
              '    integer :: x'//new_line('a')// &
              'contains'//new_line('a')// &
              '    subroutine s()'//new_line('a')// &
              '        x = 4'//new_line('a')// &
              '    end subroutine s'//new_line('a')// &
              'end module m'//new_line('a')// &
              'program p'//new_line('a')// &
              '    print *, 5'//new_line('a')// &
              'end program'//new_line('a')

        call transform_source(src, output_code, 'issue_2868_end_module_regression')
        call assert_has(output_code, 'module m', 'module unit dropped')
        call assert_has(output_code, 'subroutine s', 'contained subroutine dropped')
        call assert_has(output_code, 'program p', &
            'program unit dropped after end module')
    end subroutine verify_end_module_regression

    subroutine assert_has(text, pattern, failure_message)
        character(len=:), allocatable, intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: failure_message

        if (.not. allocated(text)) then
            write (error_unit, '(A)') 'FAIL: transformation produced no output'
            error stop 1
        end if
        if (index(text, trim(pattern)) == 0) then
            write (error_unit, '(A)') 'FAIL: '//trim(failure_message)
            write (error_unit, '(A)') '---- output ----'
            write (error_unit, '(A)') trim(text)
            write (error_unit, '(A)') '----------------'
            error stop 1
        end if
    end subroutine assert_has

end program test_issue_2868_bare_end_module_then_program
