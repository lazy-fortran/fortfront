! Module features round-trip test
module roundtrip_module_features
    implicit none
contains
    subroutine calc(a, b, c)
        integer, intent(in) :: a, b
        integer, intent(out) :: c
        c = a + b
    end subroutine calc
end module roundtrip_module_features
