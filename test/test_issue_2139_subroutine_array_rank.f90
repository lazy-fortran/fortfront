program test_issue_2139_subroutine_array_rank
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_with_context, transform_context_t
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use string_utils_mod, only: to_lower
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lowered_output
    type(transform_context_t) :: context

    call read_example('examples/f90/issue_playtest5_array_parameter_rank_lost.f90', &
                      input_code)

    context%input_mode = INPUT_MODE_STANDARD
    context%has_filename = .true.
    context%source_name = 'test_issue_2139_input'

    call transform_with_context(input_code, output_code, error_msg, context)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transformation reported an error'
        write (error_unit, '(A)') trim(error_msg)
        error stop 1
    end if

    lowered_output = to_lower(output_code)

    if (index(lowered_output, 'intent(in) :: a(n)') == 0) then
        write (error_unit, '(A)') 'FAIL: parameter a lost intent or shape'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    if (index(lowered_output, 'b(n)') == 0) then
        write (error_unit, '(A)') 'FAIL: parameter b lost array shape in output'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    if (index(lowered_output, 'real, intent(out) :: c(n)') == 0) then
        write (error_unit, '(A)') 'FAIL: parameter c lost array rank in output'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    if (index(lowered_output, 'real :: b') > 0) then
        write (error_unit, '(A)') 'FAIL: scalar declaration for b detected'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    print *, 'PASS: array parameter ranks preserved'

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

end program test_issue_2139_subroutine_array_rank
