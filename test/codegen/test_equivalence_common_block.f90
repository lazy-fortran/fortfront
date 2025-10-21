program test_equivalence_common_block
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    source = "program legacy_statements" // new_line('a') // &
             "    implicit none" // new_line('a') // &
             "    integer :: i" // new_line('a') // &
             "    real :: r" // new_line('a') // &
             "    equivalence (i, r)" // new_line('a') // &
             "    common /blk/ i, r" // new_line('a') // &
             "    i = 1" // new_line('a') // &
             "end program legacy_statements"

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'allocatable :: equivalence') > 0) then
        print *, 'FAIL: generated allocatable declaration for legacy keyword'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'equivalence(i, r) =') > 0) then
        print *, 'FAIL: generated array assignment for legacy keyword'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: legacy equivalence/common statements handled gracefully'
end program test_equivalence_common_block
