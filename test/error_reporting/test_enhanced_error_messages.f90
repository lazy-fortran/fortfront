program test_enhanced_error_messages
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    print *, '=== Testing Enhanced Error Message Behavior ==='
    print *

    print *, 'Test 1: Missing "then" in if statement'
    call read_example('examples/lf/error_missing_then.lf', source)

    call transform_lazy_fortran_string(source, output, error_msg)
    print *, 'Error message length:', len_trim(error_msg)
    print *, 'Error message: "' // trim(error_msg) // '"'
    print *, 'Output contains error comment:', index(output, '! COMPILATION') > 0
    print *, 'First few lines of output:'
    print *, output(1:min(200, len(output)))
    print *

    print *, 'Test 2: Complete garbage input'
    call read_example('examples/lf/error_complete_garbage.lf', source)

    call transform_lazy_fortran_string(source, output, error_msg)
    print *, 'Error message length:', len_trim(error_msg)
    print *, 'Error message: "' // trim(error_msg) // '"'
    print *, 'Output contains error comment:', index(output, '! COMPILATION') > 0
    print *, 'First few lines of output:'
    print *, output(1:min(200, len(output)))
    print *


contains


    include '../common/read_example.inc'
end program test_enhanced_error_messages
