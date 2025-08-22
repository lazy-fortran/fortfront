program debug_module_transform
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    character(len=:), allocatable :: input, output, error_msg
    
    ! Same input as the failing test
    input = 'module constants' // new_line('A') // &
            'implicit none' // new_line('A') // &
            'real(8), parameter :: pi = 3.14159265358979d0' // new_line('A') // &
            'real(8), parameter :: e = 2.71828182845905d0' // new_line('A') // &
            'contains' // new_line('A') // &
            'function circle_area(radius) result(area)' // new_line('A') // &
            '    real(8), intent(in) :: radius' // new_line('A') // &
            '    real(8) :: area' // new_line('A') // &
            '    area = pi * radius**2' // new_line('A') // &
            'end function circle_area' // new_line('A') // &
            'end module constants'
    
    call transform_lazy_fortran_string(input, output, error_msg)
    
    print *, "=== INPUT ==="
    print *, input
    print *, ""
    print *, "=== ERROR MESSAGE ==="
    print *, "Error:", trim(error_msg)
    print *, ""
    print *, "=== OUTPUT ==="
    print *, "Length:", len_trim(output)
    print *, output
    print *, ""
    print *, "=== SEARCH TESTS ==="
    print *, "Contains 'module constants':", index(output, 'module constants') > 0
    print *, "Contains 'implicit none':", index(output, 'implicit none') > 0
    print *, "Contains 'real(8) ::':", index(output, 'real(8) ::') > 0
    print *, "Contains 'pi':", index(output, 'pi') > 0
    print *, "Contains 'e':", index(output, 'e') > 0
    print *, "Contains 'end module':", index(output, 'end module') > 0
    
end program debug_module_transform
