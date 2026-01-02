program test_issue_1781_deallocate_preserved
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    if (.not. test_deallocate_in_if_block()) all_passed = .false.
    if (.not. test_allocate_and_deallocate()) all_passed = .false.

    if (all_passed) then
        print *, 'PASS: Issue #1781 - DEALLOCATE statements preserved'
    else
        error stop 'FAIL: Issue #1781 regression detected'
    end if

contains

    include '../../common/read_example.inc'


    logical function test_deallocate_in_if_block()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_deallocate_in_if_block = .true.

        call read_example('examples/lf/issue_1781_deallocate_if_block.lf', &
                          source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') &
                    'FAIL: Unexpected error: ' // trim(error_msg)
                test_deallocate_in_if_block = .false.
                return
            end if
        end if

        if (index(output, 'deallocate') == 0) then
            write (error_unit, '(A)') &
                'FAIL: deallocate statement missing in output'
            write (error_unit, '(A)') trim(output)
            test_deallocate_in_if_block = .false.
        end if
    end function test_deallocate_in_if_block

    logical function test_allocate_and_deallocate()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        integer :: alloc_pos
        integer :: dealloc_pos

        test_allocate_and_deallocate = .true.

        call read_example('examples/lf/issue_1781_allocate_deallocate.lf', &
                          source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') &
                    'FAIL: Unexpected error: ' // trim(error_msg)
                test_allocate_and_deallocate = .false.
                return
            end if
        end if

        alloc_pos = index(output, 'allocate')
        dealloc_pos = index(output, 'deallocate')

        if (alloc_pos == 0) then
            write (error_unit, '(A)') 'FAIL: allocate statement missing'
            test_allocate_and_deallocate = .false.
        else if (dealloc_pos == 0) then
            write (error_unit, '(A)') 'FAIL: deallocate statement missing'
            test_allocate_and_deallocate = .false.
        else if (dealloc_pos < alloc_pos) then
            write (error_unit, '(A)') 'FAIL: deallocate appears before allocate'
            test_allocate_and_deallocate = .false.
        end if
    end function test_allocate_and_deallocate

end program test_issue_1781_deallocate_preserved
