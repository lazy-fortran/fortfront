program test_program_unit_detection
    use, intrinsic :: iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, tokenize_core
    use frontend_program_unit_detection, only: detect_explicit_program_unit
    implicit none

    call check_case('x = 1', .false., '')
    call check_case('program p'//new_line('A')//'end program p', .true., 'p')
    call check_case('module m'//new_line('A')//'end module m', .true., 'm')
    call check_case('module', .false., '')
    call check_case('subroutine s()'//new_line('A')//'end subroutine s', &
        .true., 's')
    call check_case('function f() result(r)'//new_line('A')//'end function f', &
        .true., 'f')
    call check_case('block data'//new_line('A')//'end block data', .true., '')
    call check_case('module procedure foo', .false., '')

    print *, 'PASS: strict-mode program unit detection'

contains

    subroutine check_case(source, expect_has_unit, expect_name)
        character(len=*), intent(in) :: source
        logical, intent(in) :: expect_has_unit
        character(len=*), intent(in) :: expect_name
        type(token_t), allocatable :: tokens(:)
        logical :: has_unit
        character(len=:), allocatable :: unit_name

        call tokenize_core(source, tokens)
        call detect_explicit_program_unit(tokens, has_unit, unit_name)

        if (has_unit .neqv. expect_has_unit) then
            write (error_unit, '(A)') 'FAIL: unexpected has_unit for source:'
            write (error_unit, '(A)') trim(source)
            write (error_unit, '(A,L1)') 'Expected: ', expect_has_unit
            write (error_unit, '(A,L1)') 'Got:      ', has_unit
            error stop 1
        end if

        if (len_trim(expect_name) == 0) then
            if (allocated(unit_name)) then
                if (len_trim(unit_name) > 0) then
                    write (error_unit, '(A)') 'FAIL: unexpected unit_name for source:'
                    write (error_unit, '(A)') trim(source)
                    write (error_unit, '(A)') 'Got:'
                    write (error_unit, '(A)') trim(unit_name)
                    error stop 1
                end if
            end if
            return
        end if

        if (.not. allocated(unit_name)) then
            write (error_unit, '(A)') 'FAIL: expected unit_name for source:'
            write (error_unit, '(A)') trim(source)
            error stop 1
        end if

        if (trim(unit_name) /= trim(expect_name)) then
            write (error_unit, '(A)') 'FAIL: unexpected unit_name for source:'
            write (error_unit, '(A)') trim(source)
            write (error_unit, '(A)') 'Expected: ' // trim(expect_name)
            write (error_unit, '(A)') 'Got:      ' // trim(unit_name)
            error stop 1
        end if
    end subroutine check_case

end program test_program_unit_detection
