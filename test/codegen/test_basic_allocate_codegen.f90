program test_basic_allocate_codegen
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string

    character(len=:), allocatable :: source, output, error_msg
    logical :: ok

    print *, '=== Codegen: allocate/deallocate preservation ==='

    call read_example('examples/lf/basic_allocate_preservation.lf', source)

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

    call read_example('examples/lf/allocate_inference_missing_declaration.lf', &
        source)

    call transform_lazy_fortran_string(source, output, error_msg)

    ok = index(output, 'integer, allocatable :: arr(:)') > 0 .and. &
        index(output, 'allocate(arr(5))') > 0

    if (.not. ok) then
        print *, 'FAIL: allocate inference missing allocatable declaration'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: allocate inference declared allocatable array'
    stop 0


contains


    include '../common/read_example.inc'
end program test_basic_allocate_codegen
