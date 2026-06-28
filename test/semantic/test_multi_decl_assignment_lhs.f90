program test_multi_decl_assignment_lhs
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: src
    character(len=:), allocatable :: out_code, err

    print *, 'Testing LHS assignment for multi-declarations...'

    src = 'program p'//new_line('a')// &
        '  implicit none'//new_line('a')// &
        '  real :: a = 1.0, b = 2.0, c = 3.0'//new_line('a')// &
        '  b = b + c'//new_line('a')// &
        'end program'

    call transform_lazy_fortran_string(src, out_code, err)

    if (len(err) /= 0) then
        print *, 'ERROR: Unexpected diagnostic:', trim(err)
        stop 1
    end if

    print *, 'SUCCESS: No undefined-variable error for LHS assignment'
end program test_multi_decl_assignment_lhs

