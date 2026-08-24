program test_parser_diagnostics_io
    use fortfront_compiler, only: compiler_frontend_result_t, &
        compiler_frontend_options_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD
    implicit none

    type(compiler_frontend_result_t) :: result
    type(compiler_frontend_options_t) :: options

    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string( &
        "write(fmt='(a)', 'abc'", result, options)

    if (result%success()) then
        error stop 'malformed WRITE unexpectedly accepted'
    end if
    if (.not. allocated(result%diagnostic_text)) then
        error stop 'malformed WRITE returned no diagnostic text'
    end if
    if (index(result%diagnostic_text, &
        "Expected ')' after write unit and format") == 0) then
        error stop 'malformed WRITE returned the wrong diagnostic'
    end if
    if (.not. allocated(result%parser_errors)) then
        error stop 'malformed WRITE returned no parser diagnostics'
    end if
    if (size(result%parser_errors) /= 1) then
        error stop 'malformed WRITE returned an unexpected diagnostic count'
    end if

    print *, 'PASS: parser diagnostics propagate through compiler API'
end program test_parser_diagnostics_io
