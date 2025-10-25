module codegen_name_mangling
    use string_utils_mod, only: int_to_string
    implicit none
    private

    public :: mangle_procedure_name
    public :: type_signature_to_string

contains

    function mangle_procedure_name(base_name, signature) result(mangled_name)
        character(len=*), intent(in) :: base_name
        integer, intent(in) :: signature(:)
        character(len=:), allocatable :: mangled_name
        character(len=:), allocatable :: suffix
        integer :: i

        if (size(signature) == 0) then
            mangled_name = base_name
            return
        end if

        suffix = ""
        do i = 1, size(signature)
            if (i > 1) suffix = suffix // "_"
            suffix = suffix // kind_to_string(signature(i))
        end do

        mangled_name = trim(base_name) // "__" // suffix
    end function mangle_procedure_name

    function kind_to_string(kind_value) result(kind_str)
        integer, intent(in) :: kind_value
        character(len=:), allocatable :: kind_str

        select case (kind_value)
        case (4)
            kind_str = "i32"
        case (8)
            kind_str = "i64"
        case (16)
            kind_str = "i128"
        case (32)
            kind_str = "r32"
        case (64)
            kind_str = "r64"
        case (128)
            kind_str = "r128"
        case (84)
            kind_str = "c32"
        case (168)
            kind_str = "c64"
        case (1)
            kind_str = "l8"
        case default
            kind_str = "k" // trim(int_to_string(kind_value))
        end select
    end function kind_to_string

    function type_signature_to_string(signature) result(sig_str)
        integer, intent(in) :: signature(:)
        character(len=:), allocatable :: sig_str
        integer :: i

        if (size(signature) == 0) then
            sig_str = ""
            return
        end if

        sig_str = "("
        do i = 1, size(signature)
            if (i > 1) sig_str = sig_str // ", "
            sig_str = sig_str // kind_to_string(signature(i))
        end do
        sig_str = sig_str // ")"
    end function type_signature_to_string

end module codegen_name_mangling
