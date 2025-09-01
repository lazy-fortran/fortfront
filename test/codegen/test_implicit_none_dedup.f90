program test_implicit_none_dedup
    use frontend
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    integer :: count, pos, start

    print *, 'Testing no duplicate implicit none in program header...'

    input_code = 'program main' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    integer :: x' // new_line('a') // &
                 '    x = 5' // new_line('a') // &
                 'end program main'

    call transform_lazy_fortran_string(input_code, output_code, error_msg)
    if (len(error_msg) > 0) then
        print *, 'ERROR: transform failed:', trim(error_msg)
        stop 1
    end if

    count = 0
    start = 1
    do
        pos = index(output_code(start:), 'implicit none')
        if (pos == 0) exit
        count = count + 1
        start = start + pos + 13
    end do

    if (count /= 1) then
        print *, 'ERROR: expected 1 implicit none, found', count
        print *, trim(output_code)
        stop 1
    end if

    print *, '✓ No duplicate implicit none statements'
end program test_implicit_none_dedup

