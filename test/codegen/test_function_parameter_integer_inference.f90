program test_function_parameter_integer_inference
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    if (test_square_integer_inference()) then
        write (*, '(A)') 'PASS: integer inference for ambiguous function parameters'
        stop 0
    else
        write (error_unit, '(A)') 'FAIL: integer inference for ambiguous parameters'
        stop 1
    end if

contains

    include '../common/cli_io_reader.inc'
    include '../common/read_example.inc'

    function test_square_integer_inference() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        character(len=:), allocatable :: errors

        call read_example('examples/lf/function_parameter_integer_inference.lf', source)
        call transform_lazy_fortran_string(source, generated, errors)
        if (.not. allocated(generated)) generated = ''

        passed = .true.
        if (allocated(errors) .and. len_trim(errors) > 0) then
            write (error_unit, '(A)') 'transform reported errors:'
            write (error_unit, '(A)') trim(errors)
            passed = .false.
        end if
        if (index(generated, 'integer function square') == 0) then
            write (error_unit, '(A)') 'missing integer function signature'
            passed = .false.
        end if
        if (index(generated, 'integer, intent(in) :: x') == 0) then
            write (error_unit, '(A)') 'missing integer parameter declaration'
            passed = .false.
        end if
        if (index(generated, 'integer :: val') == 0) then
            write (error_unit, '(A)') 'missing integer declaration for caller variable'
            passed = .false.
        end if
        if (index(generated, 'real :: x') > 0) then
            write (error_unit, '(A)') 'unexpected real parameter declaration'
            passed = .false.
        end if
        if (index(generated, 'real function noise()') == 0) then
            write (error_unit, '(A)') 'missing noise function signature'
            passed = .false.
        end if
        if (index(generated, 'real :: noise') > 0) then
            write (error_unit, '(A)') &
                'noise function name declared as variable'
            passed = .false.
        end if
    end function test_square_integer_inference


end program test_function_parameter_integer_inference
