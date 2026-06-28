program test_implied_do_loops
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: output, error_msg

    write (*, '(A)') "=== Testing Implied Do Loop Array Constructors ==="
    call test_simple_implied_do()
    call test_complex_implied_do()
    call test_nested_implied_do_levels()
    call test_nested_function_with_implied_do()
    write (*, '(A)') "All implied do loop tests passed!"

contains

    subroutine test_simple_implied_do()
        character(len=:), allocatable :: input
        input = 'result = [(i, i=1,5)]'
        call transform_lazy_fortran_string(input, output, error_msg)

        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "FAIL: Simple implied do loop - Transform failed:"
            print *, "Error:", trim(error_msg)
            print *, "Input was:", trim(input)
            error stop 1
        end if

        ! The generated code should use modern [...] syntax for compatibility
        if (contains_without_spaces(output, "[(i,i=1,5)]")) then
            print *, "  PASS: Simple implied do loop"
        else
            print *, "  FAIL: Simple implied do loop - Expected" // &
                " [(i, i=1, 5)] syntax"
            print *, "  Got:", trim(output)
            error stop 1
        end if
    end subroutine test_simple_implied_do

    subroutine test_complex_implied_do()
        character(len=:), allocatable :: input
        input = 'result = sum([(i*2, i=1,10)])'
        call transform_lazy_fortran_string(input, output, error_msg)

        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "FAIL: Implied do with expression - Transform failed:", &
                trim(error_msg)
            error stop 1
        end if

        if (contains_without_spaces(output, "sum([(i*2,i=1,10)])")) then
            print *, "  PASS: Implied do with expression"
        else
            print *, "  FAIL: Implied do with expression - Expected" // &
                " sum([(i*2, i=1, 10)])"
            print *, "  Got:", trim(output)
            error stop 1
        end if
    end subroutine test_complex_implied_do

    subroutine test_nested_implied_do_levels()
        character(len=:), allocatable :: input
        character(len=*), parameter :: nested2_pattern = &
            "reshape([((i*10+j,j=1,3),i=1,3)],[3,3])"
        character(len=*), parameter :: nested3_pattern = &
            "reshape([(((i*100+j*10+k,k=1,3),j=1,3),i=1,3)],[3,3,3])"
        character(len=*), parameter :: nested2_expect = &
            "reshape([((i*10 + j, j=1, 3), i=1, 3)], [3, 3])"
        character(len=*), parameter :: nested3_expect = &
            "reshape([(((i*100 + j*10 + k, k=1, 3), j=1, 3), i=1, 3)], " // &
            "[3, 3, 3])"

        input = 'matrix = reshape([((i*10 + j, j=1,3), i=1,3)], [3,3])'
        call transform_lazy_fortran_string(input, output, error_msg)

        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "FAIL: Nested implied do - Transform failed:", &
                trim(error_msg)
            error stop 1
        end if

        if (contains_without_spaces(output, nested2_pattern)) then
            print *, "  PASS: Nested implied do (2 levels)"
        else
            print *, "  FAIL: Nested implied do (2 levels) - Expected", nested2_expect
            print *, "  Got:", trim(output)
            error stop 1
        end if

        input = 'cube = reshape([(((i*100 + j*10 + k, k=1,3), j=1,3), i=1,3)], ' // &
            '[3,3,3])'
        call transform_lazy_fortran_string(input, output, error_msg)

        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "FAIL: Nested implied do (3 levels) - Transform failed:", &
                trim(error_msg)
            error stop 1
        end if

        if (contains_without_spaces(output, nested3_pattern)) then
            print *, "  PASS: Nested implied do (3 levels)"
        else
            print *, "  FAIL: Nested implied do (3 levels) - Expected", nested3_expect
            print *, "  Got:", trim(output)
            error stop 1
        end if
    end subroutine test_nested_implied_do_levels

    subroutine test_nested_function_with_implied_do()
        character(len=:), allocatable :: input
        input = 'result = maxval([(sqrt(real(i)), i=1,5)])'
        call transform_lazy_fortran_string(input, output, error_msg)

        if (allocated(error_msg) .and. len(error_msg) > 0) then
            print *, "FAIL: Nested functions with implied do - Transform failed:", &
                trim(error_msg)
            error stop 1
        end if

        if (contains_without_spaces(output, "[(sqrt(real(i)),i=1,5)]")) then
            print *, "  PASS: Nested functions with implied do"
        else
            print *, "  FAIL: Nested functions with implied do - Expected modern syntax"
            print *, "  Got:", trim(output)
            error stop 1
        end if
    end subroutine test_nested_function_with_implied_do

    logical function contains_without_spaces(text, pattern)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: compressed
        integer :: i

        compressed = ''
        do i = 1, len_trim(text)
            if (text(i:i) /= ' ') compressed = compressed // text(i:i)
        end do
        contains_without_spaces = index(compressed, pattern) > 0
    end function contains_without_spaces

end program test_implied_do_loops
