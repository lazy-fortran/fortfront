program test_issue_1412_use_intrinsic
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Codegen: preserve USE intrinsic ONLY clause ==="

    source = '! ensure intrinsic use clauses survive transformation' // new_line('a') // &
             'use, intrinsic :: iso_fortran_env, only: int32' // new_line('a') // &
             'integer(int32) :: value' // new_line('a') // &
             'value = 123_int32' // new_line('a') // &
             'print *, value' // new_line('a')

    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (.not. allocated(output)) success = .false.

    if (success) then
        if (index(output, 'use, intrinsic :: iso_fortran_env, only: int32') == 0) success = .false.
        if (index(output, 'integer(int32) :: value') == 0) success = .false.
        if (index(output, 'value = 123_int32') == 0) success = .false.
    end if

    if (success) then
        print *, 'PASSED'
    else
        print *, 'FAILED: intrinsic USE clause or kind expressions stripped'
        if (allocated(output)) then
            print *, 'OUTPUT:'
            print *, trim(output)
        end if
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, 'ERRORS:'
                print *, trim(error_msg)
            end if
        end if
        stop 1
    end if

end program test_issue_1412_use_intrinsic
