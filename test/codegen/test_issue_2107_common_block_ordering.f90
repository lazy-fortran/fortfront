program test_issue_2107_common_block_ordering
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    integer :: declaration_pos
    integer :: common_pos

    call read_example('examples/f90/issue_common_block_ordering.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: transformation error: ' // &
                trim(error_msg)
            error stop 1
        end if
    end if

    declaration_pos = index(output, 'integer :: a, b')
    if (declaration_pos == 0) then
        write (error_unit, '(A)') 'FAIL: missing declaration for COMMON vars'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    common_pos = index(output, 'common /myblock/a, b')
    if (common_pos == 0) then
        write (error_unit, '(A)') 'FAIL: missing COMMON statement in output'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    if (common_pos <= declaration_pos) then
        write (error_unit, '(A)') 'FAIL: COMMON emitted before declarations'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    print *, 'PASS: COMMON statement emitted after declarations'


contains


    include '../common/read_example.inc'
end program test_issue_2107_common_block_ordering
