program test_issue_1738_derived_type_allocatable
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_message

    ! Test case from issue #1738
    input_code = &
        'program test_derived_type_alloc' // new_line('A') // &
        '    implicit none' // new_line('A') // &
        '' // new_line('A') // &
        '    type :: vector_t' // new_line('A') // &
        '        integer :: size' // new_line('A') // &
        '        real, allocatable :: data(:)' // new_line('A') // &
        '    end type vector_t' // new_line('A') // &
        '' // new_line('A') // &
        '    type(vector_t) :: v' // new_line('A') // &
        '    integer :: i' // new_line('A') // &
        '' // new_line('A') // &
        '    v%size = 3' // new_line('A') // &
        '    allocate(v%data(v%size))' // new_line('A') // &
        '' // new_line('A') // &
        '    do i = 1, v%size' // new_line('A') // &
        '        v%data(i) = real(i) * 1.5' // new_line('A') // &
        '    end do' // new_line('A') // &
        '' // new_line('A') // &
        '    print *, "Vector size:", v%size' // new_line('A') // &
        '    print *, "Vector data:", v%data' // new_line('A') // &
        '' // new_line('A') // &
        '    deallocate(v%data)' // new_line('A') // &
        '' // new_line('A') // &
        'end program test_derived_type_alloc'

    ! Transform the code
    call transform_lazy_fortran_string(input_code, output_code, error_message)

    ! Check that transformation succeeded
    if (len_trim(error_message) > 0) then
        print *, 'FAIL: Transformation failed'
        print *, 'Error:', error_message
        stop 1
    end if

    ! Check that allocatable component is preserved
    if (index(output_code, 'allocatable') == 0) then
        print *, 'FAIL: allocatable attribute missing from output'
        print *, 'Output:', output_code
        stop 1
    end if

    ! Check that data component exists in type definition
    if (index(output_code, 'data') == 0) then
        print *, 'FAIL: data component missing from output'
        print *, 'Output:', output_code
        stop 1
    end if

    ! Check that component access is preserved (v%data not v)
    if (index(output_code, 'v%data') == 0) then
        print *, 'FAIL: component access v%data not preserved'
        print *, 'Output:', output_code
        stop 1
    end if

    print *, 'PASS: Issue #1738 - derived type with allocatable components'

end program test_issue_1738_derived_type_allocatable
