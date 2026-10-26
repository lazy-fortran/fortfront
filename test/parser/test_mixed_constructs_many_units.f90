program test_mixed_constructs_many_units
    ! More than 100 top-level units, as in f90wrap-generated wrapper files,
    ! overflowed the fixed range capacity of detect_mixed_constructs.
    use lexer_core, only: token_t, tokenize_core
    use mixed_construct_detector, only: mixed_construct_result_t, &
        detect_mixed_constructs
    use frontend_compiler_api, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string
    use semantic_input_mode, only: INPUT_MODE_STANDARD
    implicit none

    integer, parameter :: n_units = 257
    character(len=:), allocatable :: source
    character(len=16) :: label
    type(token_t), allocatable :: tokens(:)
    type(mixed_construct_result_t) :: ranges
    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result
    integer :: i
    logical :: ok

    source = ''
    do i = 1, n_units
        write (label, '(i0)') i
        source = source//'subroutine wrap_'//trim(label)//'(x)'//new_line('a')// &
            '    integer, intent(inout) :: x'//new_line('a')// &
            '    x = x + '//trim(label)//new_line('a')// &
            'end subroutine wrap_'//trim(label)//new_line('a')
    end do

    ok = .true.
    call tokenize_core(source, tokens)
    call detect_mixed_constructs(tokens, ranges)
    if (ranges%num_explicit_ranges /= n_units) then
        print *, 'FAIL: explicit ranges', ranges%num_explicit_ranges, '/=', n_units
        ok = .false.
    else if (size(ranges%explicit_ranges, 1) /= n_units) then
        print *, 'FAIL: explicit range array size', size(ranges%explicit_ranges, 1)
        ok = .false.
    else
        do i = 2, n_units
            if (ranges%explicit_ranges(i, 1) <= ranges%explicit_ranges(i - 1, 2)) then
                print *, 'FAIL: range', i, 'does not follow range', i - 1
                ok = .false.
                exit
            end if
        end do
    end if

    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_STANDARD
    options%run_semantics = .false.
    call compile_frontend_from_string(source, result, options)
    if (.not. result%parse_ok) then
        print *, 'FAIL: frontend did not parse', n_units, 'external subroutines'
        ok = .false.
    end if

    if (.not. ok) stop 1
    print *, 'PASS: mixed construct detection with', n_units, 'units'
end program test_mixed_constructs_many_units
