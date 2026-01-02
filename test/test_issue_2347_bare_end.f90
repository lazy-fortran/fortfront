program test_issue_2347_bare_end
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use transformation_api, only: transform_context_t, transform_with_context, &
        & INPUT_MODE_STANDARD
    implicit none

    call verify_program_unit()
    call verify_module_unit()
    print *, 'PASS: Issue #2347 bare END statements handled correctly'

contains

    include 'common/cli_io_reader.inc'
    include 'common/read_example.inc'

    subroutine verify_program_unit()
        character(len=:), allocatable :: source_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        type(transform_context_t) :: ctx

        call read_example('examples/f90/issue_2347_bare_end_program.f90', &
            & source_code)

        ctx%input_mode = INPUT_MODE_STANDARD
        ctx%source_name = 'issue_2347_bare_end_program'
        ctx%has_filename = .true.

        call transform_with_context(source_code, output_code, error_msg, ctx)
        call assert_no_error(error_msg, 'program example')
        call assert_has(output_code, 'end program issue_2347_bare_end_program', &
            & 'missing end program statement')
        call assert_has(output_code, 'end subroutine increment', &
            & 'missing end subroutine statement')
        call assert_has(output_code, 'end function double_value', &
            & 'missing end function statement')
    end subroutine verify_program_unit

    subroutine verify_module_unit()
        character(len=:), allocatable :: source_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        type(transform_context_t) :: ctx

        call read_example('examples/f90/issue_2347_bare_end_module.f90', &
            & source_code)

        ctx%input_mode = INPUT_MODE_STANDARD
        ctx%source_name = 'issue_2347_bare_end_module'
        ctx%has_filename = .true.

        call transform_with_context(source_code, output_code, error_msg, ctx)
        call assert_no_error(error_msg, 'module example')
        call assert_has(output_code, 'end module issue_2347_bare_end_module', &
            & 'missing end module statement')
        call assert_has(output_code, 'end subroutine reset_value', &
            & 'missing end subroutine in module')
    end subroutine verify_module_unit

    subroutine assert_no_error(message, context)
        character(len=*), intent(in), optional :: context
        character(len=:), allocatable, intent(in) :: message

        if (allocated(message)) then
            if (len_trim(message) > 0) then
                if (present(context)) then
                    write (error_unit, '(A)') 'FAIL ('//trim(context)//'): ' // &
                        & trim(message)
                else
                    write (error_unit, '(A)') 'FAIL: ' // trim(message)
                end if
                error stop 1
            end if
        end if
    end subroutine assert_no_error

    subroutine assert_has(text, pattern, failure_message)
        character(len=:), allocatable, intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: failure_message

        if (.not. allocated(text)) then
            write (error_unit, '(A)') 'FAIL: Transformation produced no output'
            error stop 1
        end if

        if (index(text, trim(pattern)) == 0) then
            write (error_unit, '(A)') 'FAIL: ' // trim(failure_message)
            error stop 1
        end if
    end subroutine assert_has


end program test_issue_2347_bare_end
