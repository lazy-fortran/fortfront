program test_issue_2402_preprocessor_directives
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        iostat_eor
    use transformation_api, only: transform_context_t, transform_with_context, &
        INPUT_MODE_STANDARD
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx

    call read_example('examples/f90/issue_2402_missing_then_preprocessor.f90', &
        source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2402_missing_then_preprocessor'

    call transform_with_context(source_code, output_code, error_msg, ctx)
    call assert_no_error(error_msg)

    ! Verify that preprocessor directives are treated as comments (stripped from output)
    if (index(output_code, '#if') /= 0) then
        write (error_unit, '(A)') 'FAIL: #if directive not stripped'
        error stop 1
    end if

    if (index(output_code, '#endif') /= 0) then
        write (error_unit, '(A)') 'FAIL: #endif directive not stripped'
        error stop 1
    end if

    ! Verify that the actual Fortran code is preserved
    if (index(output_code, 'program test_preprocessor') == 0) then
        write (error_unit, '(A)') 'FAIL: program statement missing'
        error stop 1
    end if

    if (index(output_code, 'print *, "Linux platform"') == 0) then
        write (error_unit, '(A)') 'FAIL: print statement missing'
        error stop 1
    end if

    print *, 'PASS: Issue #2402 preprocessor directives treated as comments'

contains

    include 'common/read_example.inc'


    subroutine assert_no_error(message)
        character(len=:), allocatable, intent(in) :: message

        if (.not. allocated(message)) return
        if (len_trim(message) == 0) return

        write (error_unit, '(A)') 'FAIL: ' // trim(message)
        error stop 1
    end subroutine assert_no_error

end program test_issue_2402_preprocessor_directives
