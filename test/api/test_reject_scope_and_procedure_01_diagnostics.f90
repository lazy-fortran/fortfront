program test_reject_scope_and_procedure_01_diagnostics
    ! Reduced independent oracles for two semantic corpus constraints:
    ! procedure dummy characteristics and derived-type visibility through USE.
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string
    use semantic_input_mode, only: INPUT_MODE_STANDARD
    implicit none

    integer :: failures

    failures = 0
    call expect_rejected(procedure_source('b2'), 'incompatible type', &
        'mismatched recursive procedure dummy')
    call expect_accepted(procedure_source('b1'), 'matching procedure dummy')
    call expect_rejected(type_visibility_source(.false.), &
        "Derived type 'vector' is not accessible", &
        'type visible only in nested interface procedure')
    call expect_accepted(type_visibility_source(.true.), &
        'directly use-associated derived type')

    if (failures /= 0) error stop 1
    write (output_unit, '(A)') &
        'PASS: reject-scope-and-procedure-01 diagnostics'

contains

    function procedure_source(actual_name) result(source)
        character(len=*), intent(in) :: actual_name
        character(len=:), allocatable :: source

        source = 'program recursive_interface'//new_line('a')// &
            '  call c('//trim(actual_name)//')'//new_line('a')// &
            'contains'//new_line('a')// &
            '  subroutine a1(x)'//new_line('a')// &
            '    real :: x'//new_line('a')// &
            '  end subroutine a1'//new_line('a')// &
            '  subroutine a2(i)'//new_line('a')// &
            '    integer :: i'//new_line('a')// &
            '  end subroutine a2'//new_line('a')// &
            '  subroutine b1(f1)'//new_line('a')// &
            '    procedure(a1) :: f1'//new_line('a')// &
            '  end subroutine b1'//new_line('a')// &
            '  subroutine b2(f2)'//new_line('a')// &
            '    procedure(a2) :: f2'//new_line('a')// &
            '  end subroutine b2'//new_line('a')// &
            '  subroutine c(g)'//new_line('a')// &
            '    procedure(b1) :: g'//new_line('a')// &
            '  end subroutine c'//new_line('a')// &
            'end program recursive_interface'//new_line('a')
    end function procedure_source

    function type_visibility_source(direct_use) result(source)
        logical, intent(in) :: direct_use
        character(len=:), allocatable :: source
        character(len=:), allocatable :: use_line

        if (direct_use) then
            use_line = '  use class_vector'
        else
            use_line = '  use tools_math'
        end if
        source = 'module class_vector'//new_line('a')// &
            '  type vector'//new_line('a')// &
            '  end type vector'//new_line('a')// &
            'end module class_vector'//new_line('a')// &
            'module tools_math'//new_line('a')// &
            '  interface lin_interp'//new_line('a')// &
            '    function lin_interp_v()'//new_line('a')// &
            '      use class_vector'//new_line('a')// &
            '      type(vector) :: lin_interp_v'//new_line('a')// &
            '    end function lin_interp_v'//new_line('a')// &
            '  end interface'//new_line('a')// &
            'end module tools_math'//new_line('a')// &
            'module smooth_mesh'//new_line('a')// &
            trim(use_line)//new_line('a')// &
            '  type(vector) :: new_pos'//new_line('a')// &
            'end module smooth_mesh'//new_line('a')
    end function type_visibility_source

    subroutine expect_rejected(source, expected, label)
        character(len=*), intent(in) :: source, expected, label
        type(compiler_frontend_result_t) :: result
        type(compiler_frontend_options_t) :: options

        options%run_semantics = .true.
        options%input_mode = INPUT_MODE_STANDARD
        options%standardize = .false.
        call compile_frontend_from_string(source, result, options)
        if (result%success() .or. index(result%diagnostic_text, expected) == 0) then
            write (output_unit, '(A)') 'FAIL: '//trim(label)
            if (allocated(result%diagnostic_text)) then
                write (output_unit, '(A)') trim(result%diagnostic_text)
            end if
            failures = failures + 1
        else
            write (output_unit, '(A)') 'PASS: '//trim(label)
        end if
    end subroutine expect_rejected

    subroutine expect_accepted(source, label)
        character(len=*), intent(in) :: source, label
        type(compiler_frontend_result_t) :: result
        type(compiler_frontend_options_t) :: options

        options%run_semantics = .true.
        options%input_mode = INPUT_MODE_STANDARD
        options%standardize = .false.
        call compile_frontend_from_string(source, result, options)
        if (.not. result%success()) then
            write (output_unit, '(A)') 'FAIL: '//trim(label)
            if (allocated(result%diagnostic_text)) then
                write (output_unit, '(A)') trim(result%diagnostic_text)
            end if
            failures = failures + 1
        else
            write (output_unit, '(A)') 'PASS: '//trim(label)
        end if
    end subroutine expect_accepted

end program test_reject_scope_and_procedure_01_diagnostics
