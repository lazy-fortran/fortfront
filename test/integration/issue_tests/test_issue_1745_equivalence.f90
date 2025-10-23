program test_issue_1745_equivalence
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    ! Test case from issue #1745
    source = "program test_equivalence" // new_line('a') // &
             "    implicit none" // new_line('a') // &
             "    integer :: i" // new_line('a') // &
             "    real :: r" // new_line('a') // &
             "    equivalence (i, r)" // new_line('a') // &
             "    " // new_line('a') // &
             "    i = 42" // new_line('a') // &
             "    print *, 'i =', i" // new_line('a') // &
             "    print *, 'r =', r" // new_line('a') // &
             "    " // new_line('a') // &
             "end program test_equivalence"

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
end program test_issue_1745_equivalence
