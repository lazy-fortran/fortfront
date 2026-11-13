program test_issue_3030_conditional_diagnostic
    use fortfront_compiler, only: compiler_frontend_result_t, &
                            compiler_frontend_options_t, compile_frontend_from_string, &
                    INPUT_MODE_STANDARD, compiler_diagnostic_t, get_compiler_diagnostics
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=*), parameter :: nl = new_line('a')
    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    type(compiler_diagnostic_t), allocatable :: diagnostics(:)
    character(len=:), allocatable :: source, output, message

    options%input_mode = INPUT_MODE_STANDARD
    source = 'program p'//nl//'integer :: a'//nl// &
             'a=(.false. ? 7 : 9)'//nl//'print *,a'//nl//'end program p'//nl
    call compile_frontend_from_string(source, result, options)
    if (result%success()) error stop 'conditional expression was silently accepted'
    if (index(result%diagnostic_text, 'not supported') == 0) &
        error stop 'conditional expression has no unsupported-feature diagnostic'
    if (index(result%diagnostic_text, 'line 3, column 12') == 0) &
        error stop 'conditional diagnostic lost its source location'
    diagnostics = get_compiler_diagnostics(result)
    if (size(diagnostics) /= 1) error stop 'structured conditional diagnostic missing'
    if (diagnostics(1)%span%start%line /= 3 .or. &
        diagnostics(1)%span%start%column /= 12) &
        error stop 'structured conditional location changed'

    call transform_lazy_fortran_string(source, output, message)
    if (.not. allocated(message)) error stop 'transformation lost diagnostic'
    if (index(message, 'not supported') == 0) &
        error stop 'transformation silently lowered conditional expression'

    source = 'program p'//nl//'print *, "?" ! conditional ? in comment'//nl// &
             'end program p'//nl
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) error stop 'quoted/comment question mark rejected'
    call transform_lazy_fortran_string(source, output, message)
    if (len_trim(message) /= 0) error stop 'quoted question mark failed transformation'
    if (index(output, '"?"') == 0) error stop 'quoted question mark changed'
    print *, 'PASS: unsupported conditionals fail with a source diagnostic'
end program test_issue_3030_conditional_diagnostic
