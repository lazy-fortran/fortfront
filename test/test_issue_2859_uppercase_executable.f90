program test_issue_2859_uppercase_executable
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(:), allocatable :: input_code, output_code, error_msg

    call read_example('examples/lf/issue_2859_uppercase_executable.lf', input_code)

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transformation error: '//trim(error_msg)
        error stop 1
    end if

    if (index(output_code, 'program main') == 0) then
        write (error_unit, '(A)') &
            'FAIL: uppercase executable not wrapped in program main'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    if (index(output_code, 'implicit none') == 0) then
        write (error_unit, '(A)') 'FAIL: implicit none missing'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    if (index(output_code, 'print *, x') == 0) then
        write (error_unit, '(A)') 'FAIL: print statement missing from output'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    print *, 'PASS: uppercase executable standardized like lowercase'

contains

    include 'common/read_example.inc'
end program test_issue_2859_uppercase_executable
