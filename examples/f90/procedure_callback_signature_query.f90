program procedure_callback_signature_query
    implicit none
    procedure(), pointer :: resolved_callback
    procedure(), pointer :: unresolved_callback
    procedure(), pointer :: unresolved_pointer
    procedure(), pointer :: null_callback
    procedure(), pointer :: incompatible_callback
    integer :: marker

    resolved_callback => callback_target
    unresolved_callback => unresolved_pointer
    null_callback => null()
    incompatible_callback => scalar_target
    marker = 1
    marker = marker + 1

contains

    real function callback_target(scalar, values, scale)
        real, intent(in) :: scalar
        real, intent(inout), optional :: values(:)
        integer, value :: scale

        callback_target = scalar + real(scale)
        if (present(values)) callback_target = callback_target + values(1)
    end function callback_target

    real function scalar_target(value)
        real, intent(in) :: value

        scalar_target = value
    end function scalar_target

end program procedure_callback_signature_query
