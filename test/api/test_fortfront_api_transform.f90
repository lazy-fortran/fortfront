program test_fortfront_api_transform
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    if (.not. test_basic_transform()) all_passed = .false.
    if (.not. test_type_inference_transform()) all_passed = .false.
    if (.not. test_function_transform()) all_passed = .false.
    if (.not. test_complex_transform()) all_passed = .false.
    if (.not. test_error_handling()) all_passed = .false.
    if (.not. test_empty_input()) all_passed = .false.

    if (all_passed) then
        print *, 'PASS: fortfront public API transform suite'
        stop 0
    else
        error stop 'FAIL: fortfront public API transform suite'
    end if

contains

    include '../common/read_example.inc'


    logical function test_basic_transform()
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_basic_transform = .true.

        call read_example('examples/lf/api_basic_transform.lf', input)
        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: basic transform error: ' // &
                trim(error_msg)
            test_basic_transform = .false.
            return
        end if

        if (.not. allocated(output)) then
            write (error_unit, '(A)') 'FAIL: basic transform produced no output'
            test_basic_transform = .false.
            return
        end if

        if (index(output, 'program main') == 0 .or. &
            index(output, 'implicit none') == 0 .or. index(output, 'x = 42') == 0) then
            write (error_unit, '(A)') 'FAIL: basic transform missing elements'
            write (error_unit, '(A)') trim(output)
            test_basic_transform = .false.
        end if
    end function test_basic_transform

    logical function test_type_inference_transform()
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_type_inference_transform = .true.

        call read_example('examples/lf/api_type_inference.lf', input)
        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: type inference error: ' // &
                trim(error_msg)
            test_type_inference_transform = .false.
            return
        end if

        if (.not. allocated(output)) then
            write (error_unit, '(A)') 'FAIL: type inference produced no output'
            test_type_inference_transform = .false.
            return
        end if

        if (index(output, 'integer :: x') == 0 .or. &
            (index(output, 'real :: y') == 0 .and. &
             index(output, 'real(dp) :: y') == 0) &
            .or. index(output, 'character') == 0) then
            write (error_unit, '(A)') 'FAIL: missing inferred declarations'
            write (error_unit, '(A)') trim(output)
            test_type_inference_transform = .false.
        end if
    end function test_type_inference_transform

    logical function test_function_transform()
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_function_transform = .true.

        call read_example('examples/lf/api_function_transform.lf', input)
        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: function transform error: ' // &
                trim(error_msg)
            test_function_transform = .false.
            return
        end if

        if (.not. allocated(output)) then
            write (error_unit, '(A)') 'FAIL: function transform produced no output'
            test_function_transform = .false.
        end if
    end function test_function_transform

    logical function test_complex_transform()
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_complex_transform = .true.

        call read_example('examples/lf/api_complex_transform.lf', input)
        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: complex transform error: ' // &
                trim(error_msg)
            test_complex_transform = .false.
            return
        end if

        if (.not. allocated(output)) then
            write (error_unit, '(A)') 'FAIL: complex transform produced no output'
            test_complex_transform = .false.
        end if
    end function test_complex_transform

    logical function test_error_handling()
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_error_handling = .true.

        input = 'x = "unclosed string'
        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) == 0 .and. .not. allocated(output)) then
            write (error_unit, '(A)') 'FAIL: error handling produced no output'
            test_error_handling = .false.
        end if
    end function test_error_handling

    logical function test_empty_input()
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_empty_input = .true.

        call transform_lazy_fortran_string('', output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: empty input produced error: ' // &
                trim(error_msg)
            test_empty_input = .false.
            return
        end if

        if (.not. allocated(output)) then
            write (error_unit, '(A)') 'FAIL: no output for empty input'
            test_empty_input = .false.
            return
        end if

        if (index(output, 'program main') == 0 .or. &
            index(output, 'end program') == 0) then
            write (error_unit, '(A)') 'FAIL: minimal program not generated'
            test_empty_input = .false.
        end if
    end function test_empty_input

end program test_fortfront_api_transform
