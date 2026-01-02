program test_module_parsing_basic
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, iostat_eor

    logical :: all_passed
    all_passed = .true.

    if (.not. test_single_module_not_wrapped()) all_passed = .false.

    if (all_passed) then
        stop 0
    else
        stop 1
    end if

contains

    include '../../common/read_example.inc'


    logical function test_single_module_not_wrapped()
        character(len=:), allocatable :: input, output, error_msg

        call read_example('examples/f90/module_parsing_basic.f90', input)

        call transform_lazy_fortran_string(input, output, error_msg)

        ! Add diagnostic output for CI debugging
        write (*, *) 'DEBUG: error_msg=', trim(error_msg)
        write (*, *) 'DEBUG: output begins:'
        write (*, *) output
        write (*, *) 'DEBUG: has program main=', index(output, 'program main') > 0
        write (*, *) 'DEBUG: has module m=', index(output, 'module m') > 0
        write (*, *) 'DEBUG: has function add=', index(output, 'function add') > 0

        if (len_trim(error_msg) > 0) then
            test_single_module_not_wrapped = .false.
            return
        end if

        if (index(output, 'program main') > 0) then
            test_single_module_not_wrapped = .false.
            return
        end if

        if (index(output, 'module m') == 0) then
            test_single_module_not_wrapped = .false.
            return
        end if

        if (index(output, 'function add') == 0) then
            test_single_module_not_wrapped = .false.
            return
        end if

        test_single_module_not_wrapped = .true.
    end function test_single_module_not_wrapped

end program test_module_parsing_basic

