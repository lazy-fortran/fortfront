program test_interface_function_result_in_typespec
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source, output, error_msg
    logical :: test_passed

    test_passed = .true.

    call test_function_result_in_character_length()

    if (test_passed) then
        print *, "test_interface_function_result_in_typespec PASSED"
    else
        print *, "test_interface_function_result_in_typespec FAILED"
        error stop 1
    end if

contains

    include 'common/read_example.inc'


    subroutine test_function_result_in_character_length()
        call read_example('examples/f90/issue_2413_interface_function_result_valid.f90', &
                         source)

        call transform_lazy_fortran_string(source, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: Errors during transformation:', trim(error_msg)
            test_passed = .false.
            return
        end if

        if (index(output, 'character(len=compute_length(n))') == 0) then
            print *, 'FAIL: Function call in character length not preserved'
            print *, 'Output:', output
            test_passed = .false.
            return
        end if

        if (index(output, 'interface') == 0) then
            print *, 'FAIL: Interface block not preserved'
            test_passed = .false.
            return
        end if

        print *, 'PASS: Interface function result in character length'
    end subroutine test_function_result_in_character_length

end program test_interface_function_result_in_typespec
