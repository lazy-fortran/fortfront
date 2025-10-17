program test_issue_1413_array_function_result
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Codegen: preserve array function result ==="

    source = '! array-valued function should retain rank' // new_line('a') // &
             'function create_vector()' // new_line('a') // &
             '    implicit none' // new_line('a') // &
             '    real, dimension(3) :: create_vector' // new_line('a') // &
             '    create_vector = (/1.0, 2.0, 3.0/)' // new_line('a') // &
             'end function create_vector' // new_line('a') // &
             new_line('a') // &
             'print *, create_vector()' // new_line('a')

    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (.not. allocated(output)) success = .false.
    if (success) then
        if (index(output, 'real :: create_vector(3)') == 0) success = .false.
        if (index(output, 'print *, create_vector()') == 0) success = .false.
    end if

    if (success) then
        print *, 'PASSED'
    else
        print *, 'FAILED: array function result not preserved'
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

end program test_issue_1413_array_function_result
