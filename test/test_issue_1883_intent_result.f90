program test_issue_1883_intent_result
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    logical :: has_intent

    call read_example('examples/lf/issue_1883_intent_result.lf', input_code)
    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transformation returned error: ' // &
            trim(error_msg)
        error stop 1
    end if

    has_intent = index(output_code, 'real, intent(in) :: a, b') > 0 .or. &
                 index(output_code, 'real(8), intent(in) :: a, b') > 0 .or. &
                 index(output_code, 'real(dp), intent(in) :: a, b') > 0
    if (.not. has_intent) then
        write (error_unit, '(A)') 'FAIL: intent attributes dropped from parameters'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    print *, 'PASS: intent attributes preserved in functions with result clause'


contains


    include 'common/read_example.inc'
end program test_issue_1883_intent_result
