program test_issue_2282_external_subroutine
    use transformation_api, only: transform_with_context, transform_context_t, &
        INPUT_MODE_STANDARD
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        iostat_eor
    implicit none

    character(len=:), allocatable :: source, transformed, error_msg
    type(transform_context_t) :: context

    call read_example('examples/f90/issue_2282_external_subroutine.f90', source)

    context%source_name = 'issue_2282_external_subroutine'
    context%module_name = context%source_name
    context%program_name = 'main'
    context%has_filename = .false.
    context%input_mode = INPUT_MODE_STANDARD

    call transform_with_context(source, transformed, error_msg, context)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: unexpected error message'
        write (error_unit, '(A)') trim(error_msg)
        error stop 'FAIL: transform_with_context returned error'
    end if

    if (index(transformed, 'program main') > 0) then
        write (error_unit, '(A)') 'FAIL: synthetic program main emitted'
        write (error_unit, '(A)') trim(transformed)
        error stop 'FAIL: external subroutine wrapped'
    end if

    if (index(transformed, 'contains') > 0) then
        write (error_unit, '(A)') 'FAIL: unexpected contains block emitted'
        write (error_unit, '(A)') trim(transformed)
        error stop 'FAIL: contains emitted for standalone procedure'
    end if

    if (index(transformed, 'subroutine only_demo') == 0) then
        write (error_unit, '(A)') 'FAIL: expected subroutine missing'
        write (error_unit, '(A)') trim(transformed)
        error stop 'FAIL: subroutine dropped'
    end if

    print *, 'PASS: test_issue_2282_external_subroutine'

contains

    include '../../common/cli_io_reader.inc'

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

end program test_issue_2282_external_subroutine
