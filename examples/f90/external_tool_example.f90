! Example: Using FortFront as an external tool
! This demonstrates calling FortFront API from external Fortran code

program external_tool_example
    use frontend_transformation, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: lazy_code
    character(len=:), allocatable :: standard_code
    character(len=:), allocatable :: error_msg

    ! Example Lazy Fortran code
    lazy_code = 'program test' // new_line('a') // &
        'integer :: x = 5' // new_line('a') // &
        'print *, x' // new_line('a') // &
        'end program test'

    ! Transform using FortFront
    call transform_lazy_fortran_string(lazy_code, standard_code, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, 'Transformation failed with error:'
        print *, trim(error_msg)
    else
        print *, 'Transformation successful!'
        print *, 'Standard Fortran output:'
        print *, '------------------------'
        print '(a)', trim(standard_code)
    end if

end program external_tool_example
