program test_reshape_variable_shape_inference
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    implicit none
    character(len=:), allocatable :: source, output, error_msg

    call read_example( &
        'examples/lf/issue_playtest5_reshape_allocatable_never_allocated.lf', &
        source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: Transformation failed: ' // error_msg
            stop 1
        end if
    end if

    call assert_contains_any(output, 'real :: a(3, 3)', 'real :: a(3,3)', &
        'Expected fixed-size declaration for a')
    call assert_contains_any(output, 'real :: b(3, 3)', 'real :: b(3,3)', &
        'Expected fixed-size declaration for b')
    call assert_contains_any(output, 'real :: c(3, 3)', 'real :: c(3,3)', &
        'Expected fixed-size declaration for c')

    call ensure_absent(output, 'allocatable :: a', &
        'a incorrectly marked allocatable')
    call ensure_absent(output, 'allocatable :: b', &
        'b incorrectly marked allocatable')
    call ensure_absent(output, 'allocatable :: c', &
        'c incorrectly marked allocatable')

    write (*, '(A)') 'PASS: reshape variable shape inference'

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
        character(len=:), allocatable :: pdp_primary, pdp_alt
        character(len=:), allocatable :: p8_primary, p8_alt

        if (index(text, pattern_primary) > 0) return
        if (index(text, pattern_alt) > 0) return
        ! Also accept real(dp) / real(8) kind-promoted forms.
        pdp_primary = replace_first(pattern_primary, 'real ::', 'real(dp) ::')
        pdp_alt = replace_first(pattern_alt, 'real ::', 'real(dp) ::')
        p8_primary = replace_first(pattern_primary, 'real ::', 'real(8) ::')
        p8_alt = replace_first(pattern_alt, 'real ::', 'real(8) ::')
        if (index(text, pdp_primary) > 0) return
        if (index(text, pdp_alt) > 0) return
        if (index(text, p8_primary) > 0) return
        if (index(text, p8_alt) > 0) return
        call assert_contains(text, pattern_primary, message)
    end subroutine assert_contains_any

    function replace_first(text, needle, replacement) result(out)
        character(len=*), intent(in) :: text, needle, replacement
        character(len=:), allocatable :: out
        integer :: pos

        pos = index(text, needle)
        if (pos == 0) then
            out = text
        else
            out = text(1:pos - 1) // replacement // text(pos + len(needle):)
        end if
    end function replace_first

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



end program test_reshape_variable_shape_inference
