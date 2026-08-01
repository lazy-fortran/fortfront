program test_reject_end_01_diagnostics
    ! Issue #2893 [reject-end-01]: matching construct terminators.
    ! Negative fixtures mirror the gfortran.dg sources error_recovery_3.f90,
    ! interface_operator_1.f90, interface_operator_2.f90, pr103508.f90,
    ! pr69497.f90 and use_31.f90. Each must be rejected with a source
    ! diagnostic, while the corrected neighbour must still parse.
    use, intrinsic :: iso_fortran_env, only: output_unit
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    character(len=1) :: nl

    nl = new_line('a')

    ! error_recovery_3.f90: truncated ONLY list at end of file.
    call assert_rejected("error_recovery_3.f90", &
        "MODULE M1"//nl// &
        " INTEGER :: I"//nl// &
        "END MODULE M1"//nl// &
        ""//nl// &
        "USE M1,                    ONLY: I,"//nl, &
        "Missing generic specification in USE statement")
    call assert_accepted("error_recovery_3-fixed", &
        "MODULE M1"//nl// &
        " INTEGER :: I"//nl// &
        "END MODULE M1"//nl// &
        ""//nl// &
        "USE M1, ONLY: I"//nl// &
        "PRINT *, I"//nl// &
        "END"//nl)

    ! interface_operator_1.f90: END INTERFACE OPERATOR without generic spec.
    call assert_rejected("interface_operator_1.f90", &
        "program p"//nl// &
        "   interface operator ( .gt. )"//nl// &
        "   end interface operator"//nl// &
        "end program p"//nl, &
        "Expecting END INTERFACE operator(.gt.) statement")

    ! interface_operator_2.f90: END INTERFACE OPERATOR with a different spec.
    call assert_rejected("interface_operator_2.f90", &
        "program p"//nl// &
        "   interface operator ( .gt. )"//nl// &
        "   end interface operator (.lt.)"//nl// &
        "end program p"//nl, &
        "Expecting END INTERFACE operator(.gt.) statement")

    call assert_accepted("interface_operator-fixed", &
        "program p"//nl// &
        "   interface operator ( .gt. )"//nl// &
        "   end interface operator ( .gt. )"//nl// &
        "end program p"//nl)

    ! pr103508.f90: BLOCK construct closed by a bare END.
    call assert_rejected("pr103508.f90", &
        "program p"//nl// &
        "   block"//nl// &
        "      block"//nl// &
        "         integer :: x"//nl// &
        "         x = 1"//nl// &
        "      end"//nl// &
        "   end"//nl// &
        "end"//nl, &
        "END BLOCK statement expected")
    call assert_accepted("pr103508-fixed", &
        "program p"//nl// &
        "   block"//nl// &
        "      block"//nl// &
        "         integer :: x"//nl// &
        "         x = 1"//nl// &
        "      end block"//nl// &
        "   end block"//nl// &
        "end"//nl)

    ! pr69497.f90: DO construct closed by END BLOCK.
    call assert_rejected("pr69497.f90", &
        "program p"//nl// &
        "   block"//nl// &
        "   do"//nl// &
        "   end block"//nl// &
        "end"//nl, &
        "Expecting END DO statement")
    call assert_accepted("pr69497-fixed", &
        "program p"//nl// &
        "   block"//nl// &
        "   do"//nl// &
        "      exit"//nl// &
        "   end do"//nl// &
        "   end block"//nl// &
        "end"//nl)

    ! use_31.f90: BIND spec without a closing paren for its binding label.
    call assert_rejected("use_31.f90", &
        "module m"//nl// &
        "contains"//nl// &
        "   subroutine p() bind(c)"//nl// &
        "      use, intrinsic :: iso_c_binding"//nl// &
        "      integer, target :: a = 1"//nl// &
        "      type(c_ptr) :: z"//nl// &
        "      interface"//nl// &
        "         subroutine s(x) bind(cc)"//nl// &
        "            use, intrinsic :: iso_c_binding"//nl// &
        "            integer(c_int), value :: x"//nl// &
        "         end"//nl// &
        "      end interface"//nl// &
        "      z = c_loc(a)"//nl// &
        "      call s(z)"//nl// &
        "   end"//nl// &
        "end"//nl, &
        "Missing closing paren for binding label")
    call assert_accepted("use_31-fixed", &
        "module m"//nl// &
        "contains"//nl// &
        "   subroutine p() bind(c)"//nl// &
        "      use, intrinsic :: iso_c_binding"//nl// &
        "      integer, target :: a = 1"//nl// &
        "      type(c_ptr) :: z"//nl// &
        "      interface"//nl// &
        "         subroutine s(x) bind(c)"//nl// &
        "            use, intrinsic :: iso_c_binding"//nl// &
        "            type(c_ptr), value :: x"//nl// &
        "         end subroutine s"//nl// &
        "      end interface"//nl// &
        "      z = c_loc(a)"//nl// &
        "      call s(z)"//nl// &
        "   end subroutine p"//nl// &
        "end module m"//nl)

    ! Neighbouring forms that must keep parsing.
    call assert_accepted("named-block-in-do", &
        "program p"//nl// &
        "   integer :: i"//nl// &
        "   do i = 1, 3"//nl// &
        "      inner: block"//nl// &
        "         integer :: x"//nl// &
        "         x = i"//nl// &
        "      end block inner"//nl// &
        "   end do"//nl// &
        "end program p"//nl)

    ! Issue #2949: end_block_label_1.f90 terminates BLOCK with a labelled
    ! END BLOCK used as a GOTO target.
    call assert_accepted("end_block_label_1.f90", &
        "program p"//nl// &
        "   integer :: i"//nl// &
        "   i = 0"//nl// &
        "   block"//nl// &
        "     if (i == 0) goto 1"//nl// &
        "     i = 2"//nl// &
        "1  end block"//nl// &
        "   print *, i"//nl// &
        "end"//nl)
    call assert_accepted("labelled-end-do", &
        "program p"//nl// &
        "   integer :: i"//nl// &
        "   do"//nl// &
        "      exit"//nl// &
        "2  end do"//nl// &
        "end"//nl)

    ! A labelled terminator must still be validated against its construct.
    call assert_rejected("labelled-mismatched-terminator", &
        "program p"//nl// &
        "   block"//nl// &
        "   do"//nl// &
        "3  end block"//nl// &
        "end"//nl, &
        "Expecting END DO statement")

    write (output_unit, '(A)') "PASS: reject-end-01 construct terminator diagnostics"

contains

    subroutine assert_rejected(name, source, expected_message)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: expected_message

        character(len=5000) :: parse_error

        call parse_source(name, source, parse_error)

        if (len_trim(parse_error) == 0) then
            write (output_unit, '(A)') "FAIL: "//name//" was accepted"
            error stop 1
        end if

        if (index(parse_error, expected_message) == 0) then
            write (output_unit, '(A)') "FAIL: "//name//" wrong diagnostic"
            write (output_unit, '(A)') "FAIL: expected: "//expected_message
            write (output_unit, '(A)') "FAIL: actual:   "//trim(parse_error)
            error stop 1
        end if
    end subroutine assert_rejected

    subroutine assert_accepted(name, source)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: source

        character(len=5000) :: parse_error

        call parse_source(name, source, parse_error)

        if (len_trim(parse_error) /= 0) then
            write (output_unit, '(A)') "FAIL: "//name//" was rejected"
            write (output_unit, '(A)') "FAIL: "//trim(parse_error)
            error stop 1
        end if
    end subroutine assert_accepted

    subroutine parse_source(name, source, parse_error)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: source
        character(len=*), intent(out) :: parse_error

        character(len=:), allocatable :: lex_error
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: root_index

        arena = create_ast_arena()
        parse_error = ''
        call lex_source(source, tokens, lex_error)

        if (allocated(lex_error)) then
            if (len_trim(lex_error) > 0) then
                write (output_unit, '(A)') "FAIL: "//name//" lexing error: "// &
                    trim(lex_error)
                error stop 1
            end if
        end if

        call parse_tokens(tokens, arena, root_index, parse_error)
    end subroutine parse_source

end program test_reject_end_01_diagnostics
