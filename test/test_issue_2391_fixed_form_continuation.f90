program test_issue_2391_fixed_form_continuation
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, &
        & iostat_end, iostat_eor
    use transformation_api, only: compile_source, compilation_options_t
    implicit none

    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: output_path
    character(len=256) :: error_msg
    logical :: has_bounds
    type(compilation_options_t) :: options

    output_path = 'test_issue_2391_fixed_form_continuation_out.f90'
    options%output_file = output_path

    call compile_source('examples/f90/issue_2391_fixed_form_allocation.f', &
                        options, error_msg)
    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: compile_source error: ' // &
            trim(error_msg)
        call cleanup_output(output_path)
        error stop 1
    end if

    call read_example(output_path, output_code)

    has_bounds = index(output_code, 'allocate(lla(') > 0 .and. &
                 index(output_code, '2:3') > 0 .and. &
                 index(output_code, 'nf10:1') > 0 .and. &
                 index(output_code, '-2:7') > 0

    call cleanup_output(output_path)

    if (.not. has_bounds) then
        write (error_unit, '(A)') 'FAIL: fixed-form bounds not preserved'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: fixed-form continuation lines parsed successfully'

contains

    include 'common/read_example.inc'


    subroutine cleanup_output(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios
        logical :: has_file

        inquire (file=path, exist=has_file)
        if (.not. has_file) return

        open (newunit=unit, file=path, status='old', action='readwrite', &
              iostat=ios)
        if (ios == 0) then
            close (unit, status='delete')
        end if
    end subroutine cleanup_output

end program test_issue_2391_fixed_form_continuation
