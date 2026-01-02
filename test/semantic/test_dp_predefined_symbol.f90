program test_dp_predefined_symbol
    ! Test for Issue #2590: Predefine dp symbol as real64 per LFortran Standard
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg
    logical :: all_passed

    all_passed = .true.
    call read_example('examples/lf/dp_predefined_symbol.lf', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'Unexpected error: '//trim(error_msg)
        error stop 1
    end if

    ! Verify dp is imported, not declared as a local variable
    if (index(output, 'use, intrinsic :: iso_fortran_env') == 0) then
        write (error_unit, '(A)') 'FAIL: Missing iso_fortran_env import'
        all_passed = .false.
    end if

    if (index(output, 'dp => real64') == 0) then
        write (error_unit, '(A)') 'FAIL: Missing dp => real64 clause'
        all_passed = .false.
    end if

    ! dp should NOT be declared as a local variable
    if (index(output, 'integer :: dp') > 0 .or. &
        index(output, 'real :: dp') > 0) then
        write (error_unit, '(A)') 'FAIL: dp incorrectly declared as local var'
        all_passed = .false.
    end if

    ! Verify dp is used correctly in output
    if (index(output, '1.0_dp') == 0) then
        write (error_unit, '(A)') 'FAIL: dp suffix not preserved'
        all_passed = .false.
    end if

    if (index(output, 'real(dp)') == 0) then
        write (error_unit, '(A)') 'FAIL: real(dp) declaration not preserved'
        all_passed = .false.
    end if

    if (all_passed) then
        print *, 'PASS: dp predefined symbol works correctly (Issue #2590)'
    else
        write (error_unit, '(A)') 'Output was:'
        write (error_unit, '(A)') trim(output)
        error stop 'FAIL: dp predefined symbol regression'
    end if


contains

    include '../common/cli_io_reader.inc'

    include '../common/read_example.inc'
end program test_dp_predefined_symbol
