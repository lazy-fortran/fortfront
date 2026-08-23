program test_issue_2951_module_prefix_duplicates
    ! gfortran.dg/recursive_check_3.f90: repeated procedure prefixes remain
    ! visible and invalid when the procedure is contained in a module.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront_compiler, only: compiler_diagnostic_t, &
        compiler_frontend_options_t, compiler_frontend_result_t, &
        compile_frontend_from_string, get_compiler_diagnostics, &
        DIAGNOSTIC_CODE_PARSER, DIAGNOSTIC_PHASE_PARSER, &
        INPUT_MODE_STANDARD, OPERATING_MODE_STRICT
    use fortfront_types, only: DIAGNOSTIC_ERROR
    implicit none

    integer :: failures

    failures = 0
    call check_prefix('pure', failures)
    call check_prefix('elemental', failures)
    call check_prefix('recursive', failures)

    if (failures /= 0) then
        write (error_unit, '(A,I0,A)') 'FAIL: ', failures, &
            ' duplicate module procedure prefix check(s)'
        error stop 1
    end if
    write (*, '(A)') 'PASS: duplicate module procedure prefixes are rejected'

contains

    subroutine check_prefix(prefix, failures)
        character(len=*), intent(in) :: prefix
        integer, intent(inout) :: failures
        character(len=:), allocatable :: invalid_source, valid_source
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result
        type(compiler_diagnostic_t), allocatable :: diagnostics(:)
        integer :: expected_column

        invalid_source = 'module m_'//trim(prefix)//nl()// &
            'contains'//nl()// &
            trim(prefix)//' '//trim(prefix)//' subroutine s()'//nl()// &
            'end subroutine s'//nl()// &
            'end module'
        valid_source = 'module m_'//trim(prefix)//nl()// &
            'contains'//nl()// &
            trim(prefix)//' subroutine s()'//nl()// &
            'end subroutine s'//nl()// &
            'end module'

        options = compiler_frontend_options_t()
        options%input_mode = INPUT_MODE_STANDARD
        options%operating_mode = OPERATING_MODE_STRICT
        call compile_frontend_from_string(invalid_source, result, options)
        diagnostics = get_compiler_diagnostics(result)

        call require(.not. result%parse_ok, trim(prefix)// &
            ' duplicate was accepted', failures)
        call require(.not. result%semantic_ok, trim(prefix)// &
            ' duplicate passed semantics', failures)
        call require(size(diagnostics) == 1, trim(prefix)// &
            ' duplicate did not produce one diagnostic', failures)
        if (size(diagnostics) == 1) then
            expected_column = len_trim(prefix) + 2
            call require(diagnostics(1)%phase == DIAGNOSTIC_PHASE_PARSER, &
                trim(prefix)//' duplicate has wrong phase', failures)
            call require(diagnostics(1)%code == DIAGNOSTIC_CODE_PARSER, &
                trim(prefix)//' duplicate has wrong code', failures)
            call require(diagnostics(1)%severity == DIAGNOSTIC_ERROR, &
                trim(prefix)//' duplicate has wrong severity', failures)
            call require(diagnostics(1)%span%start%line == 3, &
                trim(prefix)//' duplicate has wrong line', failures)
            call require(diagnostics(1)%span%start%column == expected_column, &
                trim(prefix)//' duplicate has wrong column', failures)
            call require(index(diagnostics(1)%message, &
                'duplicate '//trim(prefix)//' attribute specified') > 0, &
                trim(prefix)//' duplicate has wrong message', failures)
        end if

        call compile_frontend_from_string(valid_source, result, options)
        call require(result%parse_ok, trim(prefix)// &
            ' valid module procedure did not parse', failures)
        call require(result%semantic_ok, trim(prefix)// &
            ' valid module procedure failed semantics', failures)
    end subroutine check_prefix

    subroutine require(condition, message, failures)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        integer, intent(inout) :: failures

        if (condition) return
        failures = failures + 1
        write (error_unit, '(A)') 'FAIL: '//trim(message)
    end subroutine require

    function nl() result(newline)
        character(len=1) :: newline

        newline = new_line('a')
    end function nl

end program test_issue_2951_module_prefix_duplicates
