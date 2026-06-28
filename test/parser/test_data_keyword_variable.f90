program test_data_keyword_variable
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg

    call read_example('examples/f90/issue_2419_data_variable_in_allocate.f90', &
        source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a)') &
                'FAIL: parser rejected allocate with data identifier'
            write (error_unit, '(a)') trim(error_msg)
            error stop 1
        end if
    end if

    if (.not. allocated(output)) then
        write (error_unit, '(a)') 'FAIL: transformation produced no output'
        error stop 1
    end if

    if (len_trim(output) == 0) then
        write (error_unit, '(a)') 'FAIL: transformation produced empty output'
        error stop 1
    end if

    print *, 'PASS: allocate statement with data identifier parsed'


contains


    include '../common/read_example.inc'
end program test_data_keyword_variable
