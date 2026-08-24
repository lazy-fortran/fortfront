program test_write_end_control
    use fortfront_compiler, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD
    implicit none

    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result

    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string( &
        "write(unit=6,end=999) 0", result, options)

    if (.not. result%success()) then
        error stop 'valid WRITE END= control rejected by compiler API'
    end if

    print *, 'PASS: compiler API parses WRITE END= control'
end program test_write_end_control
