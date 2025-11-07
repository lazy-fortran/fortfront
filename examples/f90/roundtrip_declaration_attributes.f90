! Declaration attributes round-trip test
module roundtrip_declaration_attributes
    implicit none
    integer, parameter :: MAX_SIZE = 100
    real, parameter :: PI = 3.14159
contains
    subroutine test_optional(required, optional1)
        integer, intent(in) :: required
        integer, intent(in), optional :: optional1

        if (present(optional1)) then
            continue
        end if
    end subroutine test_optional
end module roundtrip_declaration_attributes
