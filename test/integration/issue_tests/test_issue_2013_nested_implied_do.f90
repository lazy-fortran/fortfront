program test_issue_2013_nested_implied_do
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_with_context, transform_context_t, &
                                  INPUT_MODE_STANDARD
    implicit none

    type(transform_context_t) :: context
    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    call read_example('examples/f90/issue_2013_nested_implied_do_duplicate_var.f90', &
                      source)

    context%input_mode = INPUT_MODE_STANDARD
    call transform_with_context(source, transformed, error_msg, context)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a)') 'FAIL: unexpected error: ' // trim(error_msg)
            stop 1
        end if
    end if

    ! Primary fix: array size should be 12 (3*4), not 3
    if (index(transformed, 'matrix_flat(12)') == 0) then
        write (error_unit, '(a)') 'FAIL: array size not correctly calculated as 12'
        write (error_unit, '(a)') 'Output: ' // trim(transformed)
        stop 1
    end if

    ! Verify nested implied-do structure is preserved
    if (index(transformed, 'j=1, 4') == 0 .or. &
        index(transformed, 'i=1, 3') == 0) then
        write (error_unit, '(a)') 'FAIL: nested implied-do structure not preserved'
        stop 1
    end if

    write (*, '(a)') 'PASS: nested implied-do array size correctly calculated'


contains

    include '../../common/cli_io_reader.inc'

    include '../../common/read_example.inc'
end program test_issue_2013_nested_implied_do
