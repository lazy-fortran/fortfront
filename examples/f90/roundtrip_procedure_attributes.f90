! Procedure attributes round-trip test
module roundtrip_procedure_attributes
    implicit none
contains
    pure function pure_add(a, b) result(c)
        integer, intent(in) :: a, b
        integer :: c
        c = a + b
    end function pure_add

    elemental function double_val(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = x * 2
    end function double_val

    subroutine test_intent(val_in, val_out, val_inout)
        integer, intent(in) :: val_in
        integer, intent(out) :: val_out
        integer, intent(inout) :: val_inout
        val_out = val_in
        val_inout = val_inout + val_in
    end subroutine test_intent
end module roundtrip_procedure_attributes
