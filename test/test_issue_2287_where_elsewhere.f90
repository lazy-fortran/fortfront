program test_issue_2287_where_elsewhere
    use, intrinsic :: iso_fortran_env, only: error_unit
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx
    integer :: pos_if, pos_stop, pos_end_if
    integer :: pos_elsewhere, pos_assignment

    call read_example('examples/f90/issue_2287_where_elsewhere.f90', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2287_where_elsewhere'

    call transform_with_context(source_code, output_code, error_msg, ctx)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
            trim(error_msg)
        error stop 1
    end if

    if (.not. allocated(output_code)) then
        write (error_unit, '(A)') 'FAIL: transform_with_context produced no output'
        error stop 1
    end if

    pos_elsewhere = index(output_code, 'elsewhere')
    if (pos_elsewhere == 0) then
        write (error_unit, '(A)') 'FAIL: ELSEWHERE block missing in output'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    pos_assignment = index(output_code(pos_elsewhere:), 'b = 2')
    if (pos_assignment == 0) then
        write (error_unit, '(A)') 'FAIL: ELSEWHERE assignment missing'
        write (error_unit, '(A)') output_code
        error stop 1
    end if
    pos_assignment = pos_assignment + pos_elsewhere - 1
    if (pos_assignment <= pos_elsewhere) then
        write (error_unit, '(A)') 'FAIL: ELSEWHERE assignment out of order'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    pos_if = index(output_code, 'if (any(b == 0)) then')
    pos_stop = index(output_code, 'stop 1')
    pos_end_if = index(output_code, 'end if')
    if (pos_if == 0 .or. pos_stop == 0 .or. pos_end_if == 0) then
        write (error_unit, '(A)') 'FAIL: expected IF/STOP/END IF sequence missing'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. (pos_if < pos_stop .and. pos_stop < pos_end_if)) then
        write (error_unit, '(A)') 'FAIL: STOP statement moved outside IF block'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: Issue #2287 WHERE/ELSEWHERE preserved'

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

end program test_issue_2287_where_elsewhere
