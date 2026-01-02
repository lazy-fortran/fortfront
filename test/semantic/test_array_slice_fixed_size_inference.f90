program test_array_slice_fixed_size_inference
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    implicit none
    character(len=:), allocatable :: source, output, error_msg

    call read_example( &
        'examples/lf/issue_playtest5_array_slices_allocatable.lf', &
        source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: Transformation failed: ' // error_msg
            stop 1
        end if
    end if

    call assert_contains_any(output, 'integer :: slice1(5)', 'integer :: slice1( 5)', &
                             'Expected fixed-size declaration for slice1')
    call assert_contains_any(output, 'integer :: slice2(5)', 'integer :: slice2( 5)', &
                             'Expected fixed-size declaration for slice2')
    call assert_contains_any(output, 'integer :: slice3(5)', 'integer :: slice3( 5)', &
                             'Expected fixed-size declaration for slice3')

    call ensure_absent(output, 'allocatable :: slice1', &
                       'slice1 incorrectly marked allocatable')
    call ensure_absent(output, 'allocatable :: slice2', &
                       'slice2 incorrectly marked allocatable')
    call ensure_absent(output, 'allocatable :: slice3', &
                       'slice3 incorrectly marked allocatable')

    write (*, '(A)') 'PASS: array slice fixed size inference'

contains

    include '../common/cli_io_reader.inc'
    include '../common/read_example.inc'

    subroutine assert_contains(text, pattern, message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: message

        if (index(text, pattern) == 0) then
            write (error_unit, '(A)') 'FAIL: ' // message
            write (error_unit, '(A)') 'Pattern: ' // trim(pattern)
            write (error_unit, '(A)') 'Output:'
            write (error_unit, '(A)') text
            stop 1
        end if
    end subroutine assert_contains

    subroutine assert_contains_any(text, pattern_primary, pattern_alt, message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern_primary
        character(len=*), intent(in) :: pattern_alt
        character(len=*), intent(in) :: message

        if (index(text, pattern_primary) == 0 .and. &
            index(text, pattern_alt) == 0) then
            call assert_contains(text, pattern_primary, message)
        end if
    end subroutine assert_contains_any

    subroutine ensure_absent(text, pattern, message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: message

        if (index(text, pattern) > 0) then
            write (error_unit, '(A)') 'FAIL: ' // message
            write (error_unit, '(A)') 'Unexpected pattern: ' // trim(pattern)
            write (error_unit, '(A)') 'Output:'
            write (error_unit, '(A)') text
            stop 1
        end if
    end subroutine ensure_absent



end program test_array_slice_fixed_size_inference
