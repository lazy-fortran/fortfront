program test_nested_array_reshape
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    call read_example('examples/f90/nested_array_literal_square.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'reshape') > 0) then
        if (index(output, '[1, 2, 3, 4') > 0 .or. &
            index(output, '[1,2,3,4') > 0) then
            if (index(output, '[2, 2]') > 0 .or. &
                index(output, '[2,2]') > 0) then
                if (index(output, 'order=[2, 1]') > 0 .or. &
                    index(output, 'order=[2,1]') > 0) then
                    print *, 'PASS: nested array transformed to reshape'
                else
                    print *, 'FAIL: reshape order missing'
                    print *, 'Output:'
                    print *, trim(output)
                    stop 1
                end if
            else
                print *, 'FAIL: reshape dimensions incorrect'
                print *, 'Output:'
                print *, trim(output)
                stop 1
            end if
        else
            print *, 'FAIL: reshape elements incorrect'
            print *, 'Output:'
            print *, trim(output)
            stop 1
        end if
    else
        print *, 'FAIL: nested array not transformed to reshape'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    call read_example('examples/f90/nested_array_literal_rectangular.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'reshape') > 0) then
        if (index(output, '[1, 2, 3, 4, 5, 6') > 0 .or. &
            index(output, '[1,2,3,4,5,6') > 0) then
            if (index(output, '[3, 2]') > 0 .or. &
                index(output, '[3,2]') > 0) then
                if (index(output, 'order=[2, 1]') > 0 .or. &
                    index(output, 'order=[2,1]') > 0) then
                    print *, 'PASS: rectangular nested array reshaped'
                else
                    print *, 'FAIL: reshape order missing for rectangular case'
                    print *, 'Output:'
                    print *, trim(output)
                    stop 1
                end if
            else
                print *, 'FAIL: reshape dimensions incorrect for rectangular case'
                print *, 'Output:'
                print *, trim(output)
                stop 1
            end if
        else
            print *, 'FAIL: reshape elements incorrect for rectangular case'
            print *, 'Output:'
            print *, trim(output)
            stop 1
        end if
    else
        print *, 'FAIL: rectangular nested array not transformed'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if


contains


    include '../common/read_example.inc'
end program test_nested_array_reshape
