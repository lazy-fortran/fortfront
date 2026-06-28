program test_parser_iteration_limits
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    print *, "=== Parser Iteration Limit Tests (Issue #2451) ==="
    print *

    call test_complex_nested_calls()
    call test_many_function_arguments()
    call test_large_array_literal()

    print *
    print *, "=== Summary ==="
    print *, "Tests run:   ", test_count
    print *, "Tests passed:", pass_count

    if (pass_count == test_count) then
        print *, "All parser iteration limit tests passed!"
    else
        print *, "FAILURE: Some tests failed"
        stop 1
    end if

contains

    subroutine test_complex_nested_calls()
        character(len=:), allocatable :: source, result, error_msg

        test_count = test_count + 1
        print *, "Testing complex nested function calls..."

        source = "program test" // new_line('a') // &
            "  implicit none" // new_line('a') // &
            "  integer :: arr(3,3), res(2)" // new_line('a') // &
            "  arr = reshape([1,2,3,4,5,6,7,8,9], [3,3])" // new_line('a') // &
            "  res = maxloc(arr, dim=1, mask=arr > 5)" // new_line('a') // &
            "  print *, res" // new_line('a') // &
            "end program test"

        call transform_lazy_fortran_string(source, result, error_msg)

        if (len_trim(result) > 0) then
            print *, "  PASS: Complex nested calls parsed without hang"
            pass_count = pass_count + 1
        else
            print *, "  FAIL: Parser failed on complex nested calls"
            if (allocated(error_msg)) print *, "  Error: ", trim(error_msg)
        end if
    end subroutine test_complex_nested_calls

    subroutine test_many_function_arguments()
        character(len=:), allocatable :: source, result, error_msg

        test_count = test_count + 1
        print *, "Testing function with many arguments..."

        source = "program test" // new_line('a') // &
            "  implicit none" // new_line('a') // &
            "  call sub(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)" // &
            new_line('a') // &
            "end program test"

        call transform_lazy_fortran_string(source, result, error_msg)

        if (len_trim(result) > 0) then
            print *, "  PASS: Many arguments parsed without hang"
            pass_count = pass_count + 1
        else
            print *, "  FAIL: Parser failed on many arguments"
            if (allocated(error_msg)) print *, "  Error: ", trim(error_msg)
        end if
    end subroutine test_many_function_arguments

    subroutine test_large_array_literal()
        character(len=:), allocatable :: source, result, error_msg

        test_count = test_count + 1
        print *, "Testing large array literal..."

        source = "program test" // new_line('a') // &
            "  implicit none" // new_line('a') // &
            "  integer :: x(50)" // new_line('a') // &
            "  x = [1,2,3,4,5,6,7,8,9,10," // &
            "11,12,13,14,15,16,17,18,19,20," // &
            "21,22,23,24,25,26,27,28,29,30," // &
            "31,32,33,34,35,36,37,38,39,40," // &
            "41,42,43,44,45,46,47,48,49,50]" // new_line('a') // &
            "end program test"

        call transform_lazy_fortran_string(source, result, error_msg)

        if (len_trim(result) > 0) then
            print *, "  PASS: Large array literal parsed without hang"
            pass_count = pass_count + 1
        else
            print *, "  FAIL: Parser failed on large array literal"
            if (allocated(error_msg)) print *, "  Error: ", trim(error_msg)
        end if
    end subroutine test_large_array_literal

end program test_parser_iteration_limits
