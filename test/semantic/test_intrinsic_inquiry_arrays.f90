program test_intrinsic_inquiry_arrays
    use transformation_api, only: compile_source, compilation_options_t
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, &
                                                                           iostat_end, &
                                                                              iostat_eor
    implicit none

    character(len=*), parameter :: example_path = 'examples/lf/intrinsic_functions.lf'
    character(len=*), parameter :: output_path = 'tmp_intrinsic_inquiry_arrays_out.f90'
    character(len=256) :: error_msg
    type(compilation_options_t) :: options
    character(len=:), allocatable :: generated_code
    logical :: all_passed

    print *, 'Testing intrinsic inquiry array inference...'

    options%output_file = output_path
    call compile_source(example_path, options, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: compile_source error: ' // trim(error_msg)
        stop 1
    end if

    call read_example(output_path, generated_code)

    all_passed = .true.
    call assert_contains(generated_code, 'real, allocatable :: transposed(:,:)', &
                         'transpose result stays rank-2', all_passed)
    call assert_contains(generated_code, 'integer, allocatable :: min_loc(:)', &
                         'minloc result inferred as integer array', all_passed)
    call assert_contains(generated_code, 'integer, allocatable :: max_loc(:)', &
                         'maxloc result inferred as integer array', all_passed)
    call assert_contains(generated_code, 'integer, allocatable :: lb(:)', &
                         'lbound result inferred as integer array', all_passed)
    call assert_contains(generated_code, 'integer, allocatable :: ub(:)', &
                         'ubound result inferred as integer array', all_passed)

    call cleanup_output_file(output_path)

    if (all_passed) then
        print *, 'All intrinsic inquiry inference checks passed!'
    else
        stop 1
    end if

contains

    include '../common/cli_io_reader.inc'
    include '../common/read_example.inc'


    subroutine assert_contains(text, needle, description, status)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: needle
        character(len=*), intent(in) :: description
        logical, intent(inout) :: status

        if (index(text, needle) <= 0) then
            write (error_unit, '(A)') 'FAIL: ' // trim(description)
            status = .false.
        else
            print *, '  PASS: ' // trim(description)
        end if
    end subroutine assert_contains

    subroutine cleanup_output_file(path)
        character(len=*), intent(in) :: path
        logical :: exists
        integer :: unit

        inquire (file=path, exist=exists)
        if (exists) then
            open (newunit=unit, file=path, status='old')
            close (unit, status='delete')
        end if
    end subroutine cleanup_output_file


end program test_intrinsic_inquiry_arrays
