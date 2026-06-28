program test_optional_polymorphic
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    type :: array_t
        real(dp), allocatable :: values(:)
        integer :: size
    end type array_t

    type, extends(array_t) :: named_array_t
        character(len=:), allocatable :: name
    end type named_array_t

    type(array_t) :: arr1, arr2
    type(named_array_t) :: narr1
    integer :: count

    print *, "=== Optional Polymorphic Arguments Tests ==="
    print *

    allocate (arr1%values(3))
    arr1%values = [1.0d0, 2.0d0, 3.0d0]
    arr1%size = 3

    allocate (arr2%values(2))
    arr2%values = [10.0d0, 20.0d0]
    arr2%size = 2

    narr1%name = "test_array"
    allocate (narr1%values(4))
    narr1%values = [5.0d0, 6.0d0, 7.0d0, 8.0d0]
    narr1%size = 4

    call test_class_typed_optional()
    call test_class_unlimited_optional()
    call test_class_typed_intent_in()

    print *
    print *, "All optional polymorphic tests passed!"

contains

    subroutine test_class_typed_optional()
        print *, "Test 1: class(array_t), optional argument"
        call array_init_optional(arr1, 5)
        if (arr1%size /= 5) then
            print *, "  FAIL: size should be 5, got", arr1%size
            stop 1
        end if
        if (.not. allocated(arr1%values)) then
            print *, "  FAIL: values should be allocated"
            stop 1
        end if
        if (size(arr1%values) /= 5) then
            print *, "  FAIL: values size should be 5"
            stop 1
        end if
        print *, "  PASS: optional argument absent"

        call array_init_optional(arr1, 3, arr2)
        if (arr1%size /= 3) then
            print *, "  FAIL: size should be 3, got", arr1%size
            stop 1
        end if
        if (abs(arr1%values(1) - arr2%values(1)) > 1.0d-10) then
            print *, "  FAIL: copied values incorrect"
            stop 1
        end if
        print *, "  PASS: optional argument present"
    end subroutine test_class_typed_optional

    subroutine test_class_unlimited_optional()
        integer :: int_val
        real(dp) :: real_val
        character(len=20) :: char_val

        print *
        print *, "Test 2: class(*), optional argument"

        int_val = 0
        call process_unlimited(int_val)
        if (int_val /= 42) then
            print *, "  FAIL: int_val should be 42, got", int_val
            stop 1
        end if
        print *, "  PASS: unlimited polymorphic without optional"

        int_val = 0
        real_val = 3.14d0
        call process_unlimited(int_val, real_val)
        if (int_val /= 100) then
            print *, "  FAIL: int_val should be 100, got", int_val
            stop 1
        end if
        print *, "  PASS: unlimited polymorphic with real optional"

        int_val = 0
        char_val = "test"
        call process_unlimited(int_val, char_val)
        if (int_val /= 200) then
            print *, "  FAIL: int_val should be 200, got", int_val
            stop 1
        end if
        print *, "  PASS: unlimited polymorphic with char optional"
    end subroutine test_class_unlimited_optional

    subroutine test_class_typed_intent_in()
        integer :: result

        print *
        print *, "Test 3: class(array_t), optional, intent(in)"

        result = count_elements(arr1)
        if (result /= arr1%size) then
            print *, "  FAIL: count should match size"
            stop 1
        end if
        print *, "  PASS: without optional argument"

        result = count_elements(arr1, arr2)
        if (result /= arr1%size + arr2%size) then
            print *, "  FAIL: count should be sum of sizes"
            stop 1
        end if
        print *, "  PASS: with optional argument"

        result = count_elements(arr1, narr1)
        if (result /= arr1%size + narr1%size) then
            print *, "  FAIL: extended type as optional failed"
            stop 1
        end if
        print *, "  PASS: extended type as optional argument"
    end subroutine test_class_typed_intent_in

    subroutine array_init_optional(this, n, optional_arg)
        class(array_t), intent(inout) :: this
        integer, intent(in) :: n
        class(array_t), optional, intent(in) :: optional_arg
        real(dp), allocatable :: new_values(:)

        this%size = n

        allocate (new_values(n))
        new_values = 0.0d0

        if (present(optional_arg)) then
            new_values(1:min(n, optional_arg%size)) = &
                optional_arg%values(1:min(n, optional_arg%size))
        end if

        call move_alloc(new_values, this%values)
    end subroutine array_init_optional

    subroutine process_unlimited(output, optional_data)
        integer, intent(out) :: output
        class(*), intent(in), optional :: optional_data

        if (present(optional_data)) then
            select type (optional_data)
                type is (real(dp))
                output = 100
                type is (character(len=*))
                output = 200
            class default
                output = 999
            end select
        else
            output = 42
        end if
    end subroutine process_unlimited

    function count_elements(arr, optional_arr) result(total)
        class(array_t), intent(in) :: arr
        class(array_t), intent(in), optional :: optional_arr
        integer :: total

        total = arr%size
        if (present(optional_arr)) then
            total = total + optional_arr%size
        end if
    end function count_elements

end program test_optional_polymorphic
