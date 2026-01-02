program test_issue_2072_pointer_not_declared
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, &
     &                                         iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    integer :: decl_pos
    integer :: assign_pos

    call read_example('examples/lf/issue_2072_pointer_not_declared.lf', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') &
                'FAIL: transform_lazy_fortran_string returned error: ' // &
                trim(error_msg)
            error stop 1
        end if
    end if

    decl_pos = index(output, 'integer, pointer :: ptr')
    assign_pos = index(output, 'ptr => null()')

    if (decl_pos == 0) then
        write (error_unit, '(A)') &
            'FAIL: pointer declaration missing or incorrect type'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    if (assign_pos == 0) then
        write (error_unit, '(A)') 'FAIL: pointer assignment missing in output'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    if (decl_pos > assign_pos) then
        write (error_unit, '(A)') &
            'FAIL: pointer declaration must precede pointer assignment'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    print *, 'PASS: pointer declaration generated before use'


contains


    include '../../common/read_example.inc'
end program test_issue_2072_pointer_not_declared
