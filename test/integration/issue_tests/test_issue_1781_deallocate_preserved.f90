program test_issue_1781_deallocate_preserved
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #1781: DEALLOCATE statements preserved ==='

    if (.not. test_deallocate_in_if_block()) all_passed = .false.
    if (.not. test_allocate_and_deallocate()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #1781 fixed!'
    else
        print *, 'Issue #1781 test failed!'
        stop 1
    end if

contains

    logical function test_deallocate_in_if_block()
        character(len=:), allocatable :: source, output, error_msg

        test_deallocate_in_if_block = .true.
        print *, 'Testing DEALLOCATE in IF block...'

        source = 'program test_array_allocation_stat' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    integer, allocatable :: arr(:)' // new_line('a') // &
                 '    integer :: stat' // new_line('a') // &
                 '    allocate(arr(10), stat=stat)' // new_line('a') // &
                 '    if (stat == 0) then' // new_line('a') // &
                 '        print *, ''Allocation successful''' // new_line('a') // &
                 '        arr = 42' // new_line('a') // &
                 '        print *, ''Sum:'', sum(arr)' // new_line('a') // &
                 '        deallocate(arr)' // new_line('a') // &
                 '    else' // new_line('a') // &
                 '        print *, ''Allocation failed''' // new_line('a') // &
                 '    end if' // new_line('a') // &
                 'end program test_array_allocation_stat'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error:', trim(error_msg)
                test_deallocate_in_if_block = .false.
                return
            end if
        end if

        if (index(output, 'deallocate') == 0) then
            print *, '  FAIL: deallocate statement missing in output'
            print *, 'Output:'
            print *, trim(output)
            test_deallocate_in_if_block = .false.
        else
            print *, '  PASS: deallocate statement preserved'
        end if
    end function test_deallocate_in_if_block

    logical function test_allocate_and_deallocate()
        character(len=:), allocatable :: source, output, error_msg
        integer :: alloc_pos, dealloc_pos

        test_allocate_and_deallocate = .true.
        print *, 'Testing both ALLOCATE and DEALLOCATE...'

        source = 'program test_both' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    integer, allocatable :: data(:)' // new_line('a') // &
                 '    allocate(data(100))' // new_line('a') // &
                 '    data = 0' // new_line('a') // &
                 '    if (size(data) > 0) then' // new_line('a') // &
                 '        print *, ''Data allocated''' // new_line('a') // &
                 '        deallocate(data)' // new_line('a') // &
                 '    end if' // new_line('a') // &
                 'end program test_both'

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error:', trim(error_msg)
                test_allocate_and_deallocate = .false.
                return
            end if
        end if

        alloc_pos = index(output, 'allocate')
        dealloc_pos = index(output, 'deallocate')

        if (alloc_pos == 0) then
            print *, '  FAIL: allocate statement missing'
            test_allocate_and_deallocate = .false.
        else if (dealloc_pos == 0) then
            print *, '  FAIL: deallocate statement missing'
            test_allocate_and_deallocate = .false.
        else if (dealloc_pos < alloc_pos) then
            print *, '  FAIL: deallocate appears before allocate'
            test_allocate_and_deallocate = .false.
        else
            print *, '  PASS: both allocate and deallocate preserved'
        end if
    end function test_allocate_and_deallocate

end program test_issue_1781_deallocate_preserved
