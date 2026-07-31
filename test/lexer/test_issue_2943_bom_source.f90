program test_issue_2943_bom_source
    ! Issue #2943: a leading byte-order mark must not be rejected as an
    ! invalid source character. gfortran accepts BOM-prefixed sources
    ! (gfortran.dg/bom_*.f90); fortfront must too, while still rejecting
    ! stray non-source bytes that are not part of a leading BOM.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use frontend_compiler_api, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_file
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    call test_utf8_bom_example_file(all_passed)
    call test_utf8_bom_string(all_passed)
    call test_utf16_bom_strings(all_passed)
    call test_utf32_bom_strings(all_passed)
    call test_stray_byte_still_rejected(all_passed)
    call test_bare_nul_still_rejected(all_passed)

    if (.not. all_passed) then
        write (error_unit, '(A)') 'FAIL: issue 2943 BOM source handling'
        error stop 1
    end if

    print *, 'PASS: issue 2943 BOM source handling'

contains

    subroutine test_utf8_bom_example_file(passed)
        logical, intent(inout) :: passed
        type(compiler_frontend_result_t) :: res
        type(compiler_frontend_options_t) :: options
        character(len=*), parameter :: path = 'examples/f90/utf8_bom_example.f90'
        logical :: exists

        inquire (file=path, exist=exists)
        if (.not. exists) then
            write (error_unit, '(A)') 'SKIP: '//path//' not reachable from cwd'
            return
        end if

        options%run_semantics = .true.
        call compile_frontend_from_file(path, res, options)
        if (.not. res%parse_ok) then
            passed = .false.
            write (error_unit, '(A)') 'FAIL: utf8_bom_example.f90 rejected'
            if (allocated(res%diagnostic_text)) then
                write (error_unit, '(A)') trim(res%diagnostic_text)
            end if
        end if
    end subroutine test_utf8_bom_example_file

    subroutine test_utf8_bom_string(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        source = char(239)//char(187)//char(191)// &
            'print *, "Hello world"'//new_line('A')//'end'//new_line('A')
        call expect_accepted('UTF-8 BOM', source, passed)
    end subroutine test_utf8_bom_string

    subroutine test_utf16_bom_strings(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        call widen(plain_source(), 2, .false., char(254)//char(255), source)
        call expect_accepted('UTF-16 BE BOM', source, passed)

        call widen(plain_source(), 2, .true., char(255)//char(254), source)
        call expect_accepted('UTF-16 LE BOM', source, passed)
    end subroutine test_utf16_bom_strings

    subroutine test_utf32_bom_strings(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        call widen(plain_source(), 4, .true., &
            char(255)//char(254)//char(0)//char(0), source)
        call expect_accepted('UTF-32 LE BOM', source, passed)

        call widen(plain_source(), 4, .false., &
            char(0)//char(0)//char(254)//char(255), source)
        call expect_accepted('UTF-32 BE BOM', source, passed)
    end subroutine test_utf32_bom_strings

    subroutine test_stray_byte_still_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        source = 'x = 1'//new_line('A')//'y'//char(239)//' = 2'//new_line('A')
        call expect_rejected('stray 0xEF byte', source, passed)
    end subroutine test_stray_byte_still_rejected

    subroutine test_bare_nul_still_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        source = 'x = 1'//char(0)//new_line('A')
        call expect_rejected('bare NUL byte without BOM', source, passed)
    end subroutine test_bare_nul_still_rejected

    function plain_source() result(text)
        character(len=:), allocatable :: text

        text = 'print *, "Hello world"'//new_line('A')//'end'//new_line('A')
    end function plain_source

    ! Build a BOM-prefixed wide-encoded source: every ASCII byte padded with
    ! NULs, little endian when low_first is true.
    subroutine widen(ascii_src, width, low_first, bom, wide_src)
        character(len=*), intent(in) :: ascii_src
        integer, intent(in) :: width
        logical, intent(in) :: low_first
        character(len=*), intent(in) :: bom
        character(len=:), allocatable, intent(out) :: wide_src
        integer :: n, i, k, pos

        n = len(ascii_src)
        allocate (character(len=len(bom) + width*n) :: wide_src)
        wide_src(1:len(bom)) = bom

        pos = len(bom) + 1
        do i = 1, n
            do k = 1, width
                wide_src(pos + k - 1:pos + k - 1) = char(0)
            end do
            if (low_first) then
                wide_src(pos:pos) = ascii_src(i:i)
            else
                wide_src(pos + width - 1:pos + width - 1) = ascii_src(i:i)
            end if
            pos = pos + width
        end do
    end subroutine widen

    subroutine expect_accepted(label, source, passed)
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: source
        logical, intent(inout) :: passed
        character(len=:), allocatable :: output, error_msg

        call transform_lazy_fortran_string(source, output, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                passed = .false.
                write (error_unit, '(A)') 'FAIL: '//label//' rejected: '// &
                    trim(error_msg)
            end if
        end if
    end subroutine expect_accepted

    subroutine expect_rejected(label, source, passed)
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: source
        logical, intent(inout) :: passed
        character(len=:), allocatable :: output, error_msg
        logical :: rejected

        call transform_lazy_fortran_string(source, output, error_msg)
        rejected = .false.
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) rejected = .true.
        end if
        if (.not. rejected) then
            passed = .false.
            write (error_unit, '(A)') 'FAIL: '//label//' was accepted'
        end if
    end subroutine expect_rejected

end program test_issue_2943_bom_source
