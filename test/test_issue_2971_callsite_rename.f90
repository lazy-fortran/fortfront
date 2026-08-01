program test_issue_2971_callsite_rename
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source, output, error_msg

    call read_example('examples/lf/issue_2971_callsite_rename.lf', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'test_issue_2971: transformation error: '// &
            trim(error_msg)
        error stop 1
    end if

    if (index(output, 'twice__i32') <= 0) then
        write (error_unit, '(A)') 'FAIL: missing twice__i32 specialization'
        write (error_unit, '(A)') output
        error stop 1
    end if
    if (index(output, 'twice__r') <= 0) then
        write (error_unit, '(A)') 'FAIL: missing real specialization of twice'
        write (error_unit, '(A)') output
        error stop 1
    end if

    ! Each call site must name the specialization created for it, so both
    ! mangled names appear inside print statements.
    if (index(output, 'print *, twice__i32(') <= 0) then
        write (error_unit, '(A)') 'FAIL: integer call site not renamed'
        write (error_unit, '(A)') output
        error stop 1
    end if
    if (index(output, 'print *, twice__r') <= 0) then
        write (error_unit, '(A)') 'FAIL: real call site not renamed'
        write (error_unit, '(A)') output
        error stop 1
    end if

    write (*, '(A)') 'PASS: monomorphized call sites name their specialization'

contains

    include 'common/read_example.inc'
end program test_issue_2971_callsite_rename
