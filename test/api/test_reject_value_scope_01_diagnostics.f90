program test_reject_value_scope_01_diagnostics
    use frontend_compiler_api, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string
    use semantic_input_mode, only: INPUT_MODE_STANDARD
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    character(len=:), allocatable :: invalid_source, valid_source

    options%run_semantics = .true.
    options%input_mode = INPUT_MODE_STANDARD
    options%standardize = .false.

    invalid_source = 'program p'//new_line('a')// &
        'integer, value :: k'//new_line('a')// &
        'integer :: j'//new_line('a')// &
        'value :: j'//new_line('a')// &
        'end program p'
    call compile_frontend_from_string(invalid_source, result, options)
    if (result%success()) error stop 'non-dummy VALUE was accepted'
    if (index(result%diagnostic_text, 'VALUE attribute') == 0) then
        error stop 'non-dummy VALUE diagnostic is missing'
    end if

    valid_source = 'subroutine take(x)'//new_line('a')// &
        'integer, value :: x'//new_line('a')// &
        'end subroutine take'
    call compile_frontend_from_string(valid_source, result, options)
    if (.not. result%success()) error stop 'valid VALUE dummy was rejected'

    print *, 'PASS: reject-value-scope-01 diagnostics'
end program test_reject_value_scope_01_diagnostics
