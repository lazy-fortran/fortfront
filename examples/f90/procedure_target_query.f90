program procedure_target_query
    implicit none
    procedure(real), pointer :: selected_callback
    procedure(real), pointer :: external_callback
    procedure(real), pointer :: null_callback
    procedure(real), pointer :: unresolved_callback
    real, external :: external_scale
    real :: value

    selected_callback => internal_scale
    external_callback => external_scale
    null_callback => null()
    unresolved_callback => missing_scale
    value = 1.0

contains

    real function internal_scale(x)
        real, intent(in) :: x
        internal_scale = 2.0 * x
    end function internal_scale

end program procedure_target_query

real function external_scale(x)
    real, intent(in) :: x
    external_scale = 3.0 * x
end function external_scale
