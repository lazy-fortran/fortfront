program test_issue_2142_mono_return_types
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source, output, error_msg
    integer :: i32_pos, r64_pos

    call read_example('examples/lf/issue_2142_mono_wrong_return_types.lf', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'test_issue_2142: transformation error: '//trim(error_msg)
        error stop 1
    end if

    i32_pos = index(output, 'integer function add__i32_i32')
    if (i32_pos <= 0) then
        i32_pos = index(output, 'function add__i32_i32')
        if (i32_pos <= 0) then
            write (error_unit, '(A)') 'FAIL: missing add__i32_i32 specialization'
            error stop 1
        end if
    end if

    r64_pos = index(output, 'double precision function add__r64_r64')
    if (r64_pos <= 0) then
        r64_pos = index(output, 'real(dp) function add__r64_r64')
        if (r64_pos <= 0) r64_pos = index(output, 'real(8) function add__r64_r64')
        if (r64_pos <= 0) then
            r64_pos = index(output, 'function add__r64_r64')
            if (r64_pos <= 0) then
                write (error_unit, '(A)') 'FAIL: missing add__r64_r64 specialization'
                error stop 1
            end if
        end if
    end if

    if (index(output, 'add__r32_r32') > 0) then
        write (error_unit, '(A)') &
            'FAIL: r32 variant should not exist when standardization promotes real to double'
        error stop 1
    end if

    write (*, '(A)') 'PASS: All monomorphized functions have correct return types'

contains

    include 'common/read_example.inc'
end program test_issue_2142_mono_return_types
