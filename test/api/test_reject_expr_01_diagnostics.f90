program test_reject_expr_01_diagnostics
    ! Rejection coverage for unclassifiable execution expressions (issue #2897).
    !
    ! Each constraint member carries a corrected neighbour that must stay
    ! accepted so the check cannot silently become a blanket rejection:
    !   * trailing tokens after a complete assignment  (gfortran.dg/pr56520.f90)
    use, intrinsic :: iso_fortran_env, only: output_unit
    use frontend_compiler_api, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string
    implicit none

    call assert_rejected( &
        "trailing paren after assignment", &
        "program misleading"//new_line('a')// &
        "    implicit none"//new_line('a')// &
        "    real a, c"//new_line('a')// &
        "    a = 1.0"//new_line('a')// &
        "    c = exp(+a) )"//new_line('a')// &
        "end program misleading", &
        "Unclassifiable statement")

    call assert_accepted( &
        "assignment without trailing paren", &
        "program corrected"//new_line('a')// &
        "    implicit none"//new_line('a')// &
        "    real a, c"//new_line('a')// &
        "    a = 1.0"//new_line('a')// &
        "    c = exp(+a)"//new_line('a')// &
        "    c = exp(-a)"//new_line('a')// &
        "    c = exp((a))"//new_line('a')// &
        "end program corrected")

    write (output_unit, '(A)') &
        "PASS: reject-expr-01 unclassifiable execution expressions"

contains

    subroutine assert_rejected(label, source, expected_fragment)
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: expected_fragment
        type(compiler_frontend_result_t) :: result
        type(compiler_frontend_options_t) :: options
        character(len=:), allocatable :: text

        options%run_semantics = .false.
        call compile_frontend_from_string(source, result, options)
        text = collected_diagnostics(result)

        if (result%parse_ok .and. len_trim(text) == 0) then
            write (output_unit, '(A)') "FAIL: accepted invalid source: "//label
            error stop 1
        end if

        if (index(text, expected_fragment) == 0) then
            write (output_unit, '(A)') "FAIL: wrong diagnostic for: "//label
            write (output_unit, '(A)') "  expected fragment: "// &
                expected_fragment
            write (output_unit, '(A)') "  got: "//text
            error stop 1
        end if
    end subroutine assert_rejected

    subroutine assert_accepted(label, source)
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: source
        type(compiler_frontend_result_t) :: result
        type(compiler_frontend_options_t) :: options
        character(len=:), allocatable :: text

        options%run_semantics = .false.
        call compile_frontend_from_string(source, result, options)
        text = collected_diagnostics(result)

        if (.not. result%parse_ok) then
            write (output_unit, '(A)') "FAIL: rejected valid source: "//label
            write (output_unit, '(A)') "  got: "//text
            error stop 1
        end if

        if (len_trim(text) > 0) then
            write (output_unit, '(A)') "FAIL: diagnostic on valid source: "// &
                label
            write (output_unit, '(A)') "  got: "//text
            error stop 1
        end if
    end subroutine assert_accepted

    function collected_diagnostics(result) result(text)
        type(compiler_frontend_result_t), intent(in) :: result
        character(len=:), allocatable :: text

        text = ""
        if (allocated(result%error_msg)) text = text//result%error_msg
        if (allocated(result%diagnostic_text)) then
            text = text//" "//result%diagnostic_text
        end if
    end function collected_diagnostics
end program test_reject_expr_01_diagnostics
