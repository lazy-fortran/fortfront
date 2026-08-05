program test_nested_procedure_constructs
    use fortfront, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result

    options = compiler_frontend_options_t()
    call compile_frontend_from_string(source_text(), result, options)
    if (.not. result%success()) then
        write (error_unit, '(a)') 'FAIL: nested procedure constructs rejected: '// &
            trim(result%diagnostic_text)
        error stop 1
    end if

    print '(a)', 'PASS: nested procedure constructs parsed'

contains

    include 'common/read_example.inc'

    function source_text() result(source)
        character(len=:), allocatable :: source

        call read_example('examples/f90/nested_procedure_constructs.f90', source)
    end function source_text

end program test_nested_procedure_constructs
