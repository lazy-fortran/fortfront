program test_issue_2994_lazy_real_r64
    ! Issue #2994: in a Lazy unit whose default real is real64, a kind-less real
    ! dummy must be specialized as r64 with resolved kind 8, not r32 with kind 4.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source, output, error_msg

    call read_example('examples/lf/issue_2994_lazy_real_r64.lf', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transformation error: '//trim(error_msg)
        error stop 1
    end if

    if (index(output, 'twice__r64') <= 0) then
        write (error_unit, '(A)') 'FAIL: missing real64 specialization twice__r64'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    if (index(output, 'twice__r32') > 0) then
        write (error_unit, '(A)') &
            'FAIL: real32 specialization twice__r32 should not exist in a real64 unit'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    print *, 'PASS: lazy real specialization named r64 in a real64 unit'

contains

    include 'common/read_example.inc'
end program test_issue_2994_lazy_real_r64
