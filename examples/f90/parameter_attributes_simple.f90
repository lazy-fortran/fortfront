program test_params
contains
    subroutine test(required, opt, output)
        integer, intent(in) :: required
        integer, intent(in), optional :: opt
        integer, intent(out) :: output
        output = required * 2
    end subroutine test
end program test_params
