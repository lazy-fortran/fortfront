program test_basic_allocate_codegen
    use frontend, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg
    logical :: ok

    print *, '=== Codegen: allocate/deallocate preservation ==='

    source = '' // &
        'integer, parameter :: n = 100' // new_line('a') // &
        'integer, dimension(n) :: arr'   // new_line('a') // &
        'integer, allocatable :: dyn_arr(:)' // new_line('a') // &
        'allocate(dyn_arr(n))' // new_line('a')

    call transform_lazy_fortran_string(source, output, error_msg)

    ok = index(output, 'allocate(dyn_arr(n))') > 0

    if (.not. ok) then
        print *, 'FAIL: allocate statement not preserved in output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: allocate statement preserved'
    stop 0
end program test_basic_allocate_codegen

