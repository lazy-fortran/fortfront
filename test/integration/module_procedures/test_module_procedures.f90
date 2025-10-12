module test_mod_for_procedures
    implicit none
contains

    function add_nums(a, b) result(c)
        integer :: a, b, c
        c = a + b
    end function add_nums

    subroutine print_sum(x, y)
        integer :: x, y
        print *, "Sum is:", add_nums(x, y)
    end subroutine print_sum

end module test_mod_for_procedures

program test_module_procedures
    use test_mod_for_procedures
    implicit none
    integer :: result

    result = add_nums(2, 3)
    if (result /= 5) then
        print *, "FAIL: add_nums(2,3) returned", result, "expected 5"
        error stop 1
    end if

    call print_sum(10, 20)

    print *, "PASS: Module procedures work correctly"

end program test_module_procedures
