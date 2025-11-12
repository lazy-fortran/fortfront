program test_issue_2293_legacy_implicit
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx
    logical :: preserved_implicit, has_injected_identifier

    call read_example('examples/f90/issue_2293_legacy_implicit.f', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2293_legacy_implicit.f'

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
     &        trim(error_msg)
        error stop 1
    end if

    preserved_implicit = index(output_code, 'implicit real (a-h)') > 0
    has_injected_identifier = index(output_code, 'integer :: implicit') > 0

    if (.not. preserved_implicit) then
        write (error_unit, '(A)') 'FAIL: legacy IMPLICIT statement was not preserved'
        error stop 1
    end if

    if (has_injected_identifier) then
        write (error_unit, '(A)') 'FAIL: transformer inserted integer :: implicit'
        error stop 1
    end if

    if (index(output_code, new_line('a') // '    implicit' // new_line('a')) > 0) &
        & then
        write (error_unit, '(A)') 'FAIL: stray bare IMPLICIT line detected'
        error stop 1
    end if

    print *, 'PASS: Issue #2293 - legacy IMPLICIT statement preserved'

contains

    include 'common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example

end program test_issue_2293_legacy_implicit
