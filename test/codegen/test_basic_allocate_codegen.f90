program test_basic_allocate_codegen
    use transformation_api, only: transform_lazy_fortran_string

    character(len=:), allocatable :: source, output, error_msg
    logical :: ok

    print *, '=== Codegen: allocate/deallocate preservation ==='

    source = '' // &
             'integer, parameter :: n = 100' // new_line('a') // &
             'integer, dimension(n) :: arr' // new_line('a') // &
             'integer, allocatable :: dyn_arr(:)' // new_line('a') // &
             'allocate(dyn_arr(n))' // new_line('a') // &
             'deallocate(dyn_arr)'

    call transform_lazy_fortran_string(source, output, error_msg)

    ok = index(output, 'allocate(dyn_arr(n))') > 0 .and. &
         index(output, 'deallocate(dyn_arr)') > 0

    if (.not. ok) then
        print *, 'FAIL: allocate/deallocate statement(s) not preserved in output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: allocate/deallocate statements preserved'
    print *, ''
    print *, '=== Codegen: allocate inference for missing declarations ==='

    source = '' // &
             'allocate(arr(5))' // new_line('a') // &
             'is_alloc = allocated(arr)' // new_line('a') // &
             'print *, is_alloc' // new_line('a') // &
             'deallocate(arr)'

    call transform_lazy_fortran_string(source, output, error_msg)

    ok = index(output, 'integer, dimension(:), allocatable :: arr') > 0 .and. &
         index(output, 'allocate(arr(5))') > 0

    if (.not. ok) then
        print *, 'FAIL: allocate inference missing allocatable declaration'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: allocate inference declared allocatable array'
    stop 0
end program test_basic_allocate_codegen
