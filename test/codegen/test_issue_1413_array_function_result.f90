program test_issue_1413_array_function_result
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Codegen: preserve array function result ==="

    call read_example('examples/lf/array_function_result.lf', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (.not. allocated(output)) success = .false.
    if (success) then
        if (index(output, 'function create_vector()') == 0) success = .false.
        if (index(output, 'real function create_vector') /= 0) success = .false.
        ! After fix for #2151, array functions use result clauses and rename declarations
        if (index(output, 'real :: create_vector_result(3)') == 0 .and. &
            index(output, 'real(8) :: create_vector_result(3)') == 0) success = .false.
        if (index(output, 'create_vector_result = ') == 0) success = .false.
        if (index(output, 'print *, create_vector()') == 0) success = .false.
    end if

    if (success) then
        print *, 'PASSED'
    else
        print *, 'FAILED: array function result not preserved'
        if (allocated(output)) then
            print *, 'OUTPUT:'
            print *, trim(output)
        end if
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, 'ERRORS:'
                print *, trim(error_msg)
            end if
        end if
        stop 1
    end if


contains


    include '../common/read_example.inc'
end program test_issue_1413_array_function_result
