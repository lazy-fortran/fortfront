program test_issue_1745_equivalence
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    call read_example('examples/f90/issue_1745_equivalence.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    ! Check that EQUIVALENCE statement is preserved
    if (index(output, 'equivalence') == 0 .and. index(output, 'EQUIVALENCE') == 0) then
        print *, 'FAIL: EQUIVALENCE statement was removed'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    ! Check that it appears as a statement, not a variable declaration
    ! Accept with or without spaces around comma
    if (index(output, 'equivalence (i, r)') == 0 .and. &
        index(output, 'EQUIVALENCE (i, r)') == 0 .and. &
        index(output, 'equivalence(i, r)') == 0 .and. &
        index(output, 'EQUIVALENCE(i, r)') == 0 .and. &
        index(output, 'equivalence(i,r)') == 0 .and. &
        index(output, 'EQUIVALENCE(i,r)') == 0) then
        print *, 'FAIL: EQUIVALENCE statement not found in correct form'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: EQUIVALENCE statement preserved correctly'


contains


    include '../../common/read_example.inc'
end program test_issue_1745_equivalence
