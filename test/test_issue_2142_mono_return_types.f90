program test_issue_2142_mono_return_types
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source, output, error_msg
    integer :: i32_pos, r32_pos, r64_pos

    call read_example('examples/lf/issue_2142_mono_wrong_return_types.lf', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'test_issue_2142: transformation error: '//trim(error_msg)
        error stop 1
    end if

    ! Check that integer specialization has integer return type
    i32_pos = index(output, 'integer function add__i32_i32')
    if (i32_pos <= 0) then
        i32_pos = index(output, 'function add__i32_i32')
        if (i32_pos > 0) then
            ! Check that it's NOT double precision
            if (index(output(i32_pos:), 'double precision function add__i32_i32') > 0) then
                write (error_unit, '(A)') 'FAIL: add__i32_i32 should be integer, not double precision'
                error stop 1
            end if
            ! Verify it IS integer
            if (index(output(i32_pos:i32_pos+200), 'integer, intent(in)') <= 0) then
                write (error_unit, '(A)') 'FAIL: add__i32_i32 parameters should be integer'
                error stop 1
            end if
        else
            write (error_unit, '(A)') 'FAIL: missing add__i32_i32 specialization'
            error stop 1
        end if
    end if

    ! Check that real specialization has real return type
    r32_pos = index(output, 'real function add__r32_r32')
    if (r32_pos <= 0) then
        r32_pos = index(output, 'function add__r32_r32')
        if (r32_pos > 0) then
            ! Check that it's NOT double precision
            if (index(output(r32_pos:), 'double precision function add__r32_r32') > 0) then
                write (error_unit, '(A)') 'FAIL: add__r32_r32 should be real, not double precision'
                error stop 1
            end if
            ! Verify it IS real
            if (index(output(r32_pos:r32_pos+200), 'real, intent(in)') <= 0) then
                write (error_unit, '(A)') 'FAIL: add__r32_r32 parameters should be real'
                error stop 1
            end if
        else
            write (error_unit, '(A)') 'FAIL: missing add__r32_r32 specialization'
            error stop 1
        end if
    end if

    ! Check that double specialization has double precision return type
    r64_pos = index(output, 'double precision function add__r64_r64')
    if (r64_pos <= 0) then
        r64_pos = index(output, 'real(dp) function add__r64_r64')
        if (r64_pos <= 0) r64_pos = index(output, 'real(8) function add__r64_r64')
        if (r64_pos <= 0) then
            r64_pos = index(output, 'function add__r64_r64')
            if (r64_pos > 0) then
                ! Verify it IS double precision
                if (index(output(r64_pos:r64_pos+200), 'double precision, intent(in)') <= 0) then
                    write (error_unit, '(A)') 'FAIL: add__r64_r64 parameters should be double precision'
                    error stop 1
                end if
            else
                write (error_unit, '(A)') 'FAIL: missing add__r64_r64 specialization'
                error stop 1
            end if
        end if
    end if

    write (*, '(A)') 'PASS: All monomorphized functions have correct return types'


contains


    include 'common/read_example.inc'
end program test_issue_2142_mono_return_types
