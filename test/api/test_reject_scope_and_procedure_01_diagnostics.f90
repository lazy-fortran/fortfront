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

    include '../common/read_example.inc'

    function procedure_source(actual_name) result(source)
        character(len=*), intent(in) :: actual_name
        character(len=:), allocatable :: source

        if (actual_name == 'b1') then
            call read_example( &
                'examples/f90/reject_scope_procedure_b1.f90', source)
        else
            call read_example( &
                'examples/f90/reject_scope_procedure_b2.f90', source)
        end if
    end function procedure_source

    function type_visibility_source(direct_use) result(source)
        logical, intent(in) :: direct_use
        character(len=:), allocatable :: source
        if (direct_use) then
            call read_example( &
                'examples/f90/reject_scope_type_visibility_direct.f90', source)
        else
            call read_example( &
                'examples/f90/reject_scope_type_visibility_nested.f90', source)
        end if
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
