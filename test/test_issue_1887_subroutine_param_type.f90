program test_issue_1887_subroutine_param_type
    use, intrinsic :: iso_fortran_env, only: &
        error_unit, input_unit, iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(:), allocatable :: input_code
    character(:), allocatable :: output_code
    character(:), allocatable :: error_msg
    logical :: has_integer_decl
    logical :: lacks_real_decl

    print *, "=== Issue #1887: infer subroutine parameter types from call site ==="

    call read_example('examples/lf/issue_1887_subroutine_param_type.lf', &
                      input_code)

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: transformation returned error:", trim(error_msg)
        error stop 1
    end if

    has_integer_decl = index(output_code, "integer :: x") > 0
    lacks_real_decl = index(output_code, "real :: x") == 0
    if (.not. (has_integer_decl .and. lacks_real_decl)) then
        print *, "FAIL: parameter type inference did not propagate call-site type"
        print *, "Output:"
        print *, trim(output_code)
        error stop 1
    end if

    print *, "PASS: subroutine parameter type inferred as integer from call site"

contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_1887_subroutine_param_type
