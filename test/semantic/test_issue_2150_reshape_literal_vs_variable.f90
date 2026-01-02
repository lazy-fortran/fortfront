program test_issue_2150_reshape_literal_vs_variable
    ! Test for issue #2150: Reshape array sizing inconsistent (literal vs variable shape)
    ! Verifies that both literal [2, 3] and variable [n, n] shapes produce fixed-size arrays
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    implicit none
    character(len=:), allocatable :: source, output, error_msg

    call read_example('examples/lf/issue_2150_reshape_variable_vs_literal_shape.lf', &
                      source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: Transformation failed: ' // error_msg
            stop 1
        end if
    end if

    ! Check literal shape produces fixed-size array
    call assert_contains_any(output, 'integer :: matrix_literal(2, 3)', &
                             'integer :: matrix_literal(2,3)', &
                             'Expected fixed-size for literal shape [2, 3]')

    ! Check variable shape produces fixed-size array
    call assert_contains_any(output, 'real :: matrix_var(3, 3)', &
                             'real :: matrix_var(3,3)', &
                             'Expected fixed-size for variable shape [n, n] where n=3')

    ! Ensure neither is allocatable
    call ensure_absent(output, 'allocatable :: matrix_literal', &
                       'matrix_literal incorrectly marked allocatable')
    call ensure_absent(output, 'allocatable :: matrix_var', &
                       'matrix_var incorrectly marked allocatable')

    write (*, '(A)') 'PASS: Issue #2150 - reshape literal vs variable shape'

contains

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



end program test_issue_2150_reshape_literal_vs_variable
