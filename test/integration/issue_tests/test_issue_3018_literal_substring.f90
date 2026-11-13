program test_issue_3018_literal_substring
    ! #3018: a string-literal base with a constant substring range must not
    ! lose its range at parse time. 'abcdef'(2:4) folds to the literal
    ! 'bcd'; before the fix the postfix was dropped and the whole literal
    ! was lowered - a silent miscompile (ffc printed 'abcdef', gfortran
    ! printed 'bcd').
    use transformation_api, only: transform_lazy_fortran_string
    use fortfront_compiler, only: compiler_frontend_result_t, &
                            compiler_frontend_options_t, compile_frontend_from_string, &
                          INPUT_MODE_STANDARD, array_slice_query_t, query_array_slice, &
                   range_expression_query_t, query_range_expression, get_identifier_name
    use fortfront_compiler, only: compiler_diagnostic_t, get_compiler_diagnostics
    implicit none

    character(len=:), allocatable :: lowered, error_msg
    integer :: passed, total

    passed = 0
    total = 0

    call check_fold("'abcdef'(2:4)", "'bcd'", "'abcdef'(")
    call check_fold("'hello'(2:4)", "'ell'", "'hello'(")
    call check_fold('"dqtest"(2:3)', '"qt"', '"dqtest"(')
    call check_fold("'xyzw'(4:4)", "'w'", "'xyzw'(")
    call check_runtime_bounds()

    call check_rejected("'abcdef'(3:10)")
    call check_rejected("'abcdef'(0:2)")
    call check_rejected("'abcdef'(-1:2)")
    call check_rejected("''(1:1)")
    call check_rejected("'a''b'(1:4)")
    call check_rejected("'abcdef'(1:4:1)")
    call check_rejected("'abcdef'(1:4:)")
    call check_rejected("'abcdef'(::)")
    call check_rejected("'abcdef'(1:4]")
    call check_rejected("'abcdef'(1:4")
    call check_rejected("'abcdef'(1:4,)")
    call check_rejected("'abcdef'(1:4::)")
    call check_rejected("'abcdef'(1:2,3:4)")
    call check_rejected("'abcdef'(2)")
    call check_rejected("'abcdef'(1.0:2)")
    call check_rejected("'abcdef'(n:2.0)")
    call check_rejected("'abcdef'(1_8:7_8)")
    call check_rejected("'abcdef'(1_8:2147483648_8)")
    call check_fold("'abcdef'(10:9)", "''", "'abcdef'(")
    call check_fold("'abcdef'(-1:-2)", "''", "'abcdef'(")
    call check_fold("'abcdef'(:0)", "''", "'abcdef'(")
    call check_fold("'abcdef'(10:)", "''", "'abcdef'(")
    call check_fold("'abcdef'(2147483648_8:2147483647_8)", "''", "'abcdef'(")
    call check_fold("''(:)", "''", "''(")

    print '(a,i0,a,i0)', 'test_issue_3018: ', passed, '/', total
    if (passed /= total) stop 1

contains

    subroutine check_rejected(expression)
        character(len=*), intent(in) :: expression
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result
        type(compiler_diagnostic_t), allocatable :: diagnostics(:)
        character(len=:), allocatable :: source
        integer :: column

        source = 'program p'//new_line('a')//'print *, '//expression// &
                 new_line('a')//'end program p'//new_line('a')
        options%input_mode = INPUT_MODE_STANDARD
        call compile_frontend_from_string(source, result, options)
        if (result%success()) then
            print '(a,a)', 'FAIL: invalid substring accepted: ', expression
            error stop 1
        end if
        if (index(result%diagnostic_text, 'literal substring') == 0) &
            error stop 'invalid substring has no specific diagnostic'
        diagnostics = get_compiler_diagnostics(result)
        if (size(diagnostics) /= 1) &
            error stop 'invalid substring has no single structured diagnostic'
        column = len('print *, ') + index(expression, '(')
        if (diagnostics(1)%span%start%line /= 2 .or. &
            diagnostics(1)%span%start%column /= column) &
            error stop 'invalid substring diagnostic lost its source location'
        call transform_lazy_fortran_string(source, lowered, error_msg)
        if (.not. allocated(error_msg)) &
            error stop 'invalid substring transformation has no diagnostic'
        if (index(error_msg, 'literal substring') == 0) &
            error stop 'invalid substring transformation silently succeeded'
        total = total + 1
        passed = passed + 1
    end subroutine check_rejected

    subroutine check_runtime_bounds()
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result
        type(array_slice_query_t) :: slice
        type(range_expression_query_t) :: bounds
        character(len=:), allocatable :: source, name, message
        integer :: i, slices

        source = 'program p'//new_line('a')// &
                 'integer :: n'//new_line('a')//'n=3'//new_line('a')// &
                 "print *, 'abcdef'(n:n)"//new_line('a')//'end program p'
        options%input_mode = INPUT_MODE_STANDARD
        options%run_semantics = .true.
        call compile_frontend_from_string(source, result, options)
        if (.not. result%success()) error stop 'runtime literal substring rejected'
        slices = 0
        do i = 1, result%arena%size
            slice = query_array_slice(result%arena, i)
            if (.not. slice%found) cycle
            slices = slices + 1
            if (.not. slice%is_character_substring) &
                error stop 'literal substring reported as an array section'
            if (size(slice%bounds_node_indices) /= 1) &
                error stop 'runtime substring lost its bounds'
            bounds = query_range_expression(result%arena, slice%bounds_node_indices(1))
            if (.not. bounds%found) error stop 'runtime substring range missing'
            call get_identifier_name(result%arena, bounds%start_node_index, &
                                     name, message)
            if (name /= 'n') error stop 'runtime substring lost lower bound'
            call get_identifier_name(result%arena, bounds%end_node_index, &
                                     name, message)
            if (name /= 'n') error stop 'runtime substring lost upper bound'
        end do
        if (slices /= 1) error stop 'literal substring was dropped'
        total = total + 1
        passed = passed + 1
    end subroutine check_runtime_bounds

    subroutine check_fold(src_frag, expect, forbid)
        character(len=*), intent(in) :: src_frag, expect, forbid
        character(len=:), allocatable :: text
        text = "program p"//new_line('a')// &
            "  print *, "//src_frag//new_line('a')// &
            "end program p"//new_line('a')
        total = total + 1
        call transform_lazy_fortran_string(text, lowered, error_msg)
        if (index(lowered, expect) == 0) then
            print '(a,a)', 'FAIL missing fold: ', expect
        else if (index(lowered, forbid) > 0) then
            print '(a,a)', 'FAIL unfolded base survives: ', forbid
        else
            passed = passed + 1
        end if
    end subroutine check_fold
end program test_issue_3018_literal_substring
