program test_issue_2074_arithmetic_if
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, &
        iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    use lexer_core, only: to_lower
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: lowered_output
    character(len=:), allocatable :: error_msg

    call read_example('examples/f90/issue_2074_arithmetic_if_not_supported.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: arithmetic IF transformation error'
            write (error_unit, '(A)') trim(error_msg)
            error stop 1
        end if
    end if

    if (.not. allocated(output)) then
        write (error_unit, '(A)') 'FAIL: no output emitted for arithmetic IF example'
        error stop 1
    end if

    lowered_output = to_lower(output)

    if (index(lowered_output, 'if (x < 0) then') == 0) then
        write (error_unit, '(A)') 'FAIL: missing negative branch guard'
        error stop 1
    end if

    if (index(lowered_output, 'go to 10') == 0) then
        write (error_unit, '(A)') 'FAIL: negative branch goto missing'
        error stop 1
    end if

    if (index(lowered_output, 'else if (x == 0) then') == 0) then
        write (error_unit, '(A)') 'FAIL: zero branch guard missing'
        error stop 1
    end if

    if (index(lowered_output, 'go to 20') == 0) then
        write (error_unit, '(A)') 'FAIL: zero branch goto missing'
        error stop 1
    end if

    if (index(lowered_output, 'go to 30') == 0) then
        write (error_unit, '(A)') 'FAIL: positive branch goto missing'
        error stop 1
    end if

    if (index(lowered_output, 'if (x) 10, 20, 30') > 0) then
        write (error_unit, '(A)') 'FAIL: arithmetic IF form still present'
        error stop 1
    end if

    print *, 'PASS: arithmetic IF transformed to IF/ELSEIF structure'


contains


    include '../../common/read_example.inc'
end program test_issue_2074_arithmetic_if
