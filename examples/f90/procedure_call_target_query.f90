program procedure_call_target_query
    implicit none
    procedure(real), pointer :: internal_callback
    procedure(real), pointer :: external_callback
    procedure(real), pointer :: reassigned_callback
    procedure(real), pointer :: branched_callback
    procedure(real), pointer :: null_callback
    procedure(real), pointer :: nullified_callback
    procedure(), pointer :: action_callback
    real, external :: external_scale
    real :: value
    logical :: flag

    internal_callback => internal_scale
    external_callback => external_scale
    reassigned_callback => internal_scale
    reassigned_callback => external_scale
    if (flag) branched_callback => internal_scale
    null_callback => null()
    nullified_callback => internal_scale
    nullify(nullified_callback)
    action_callback => internal_action

    value = internal_callback(1.0)
    value = external_callback(1.0)
    value = reassigned_callback(1.0)
    value = branched_callback(1.0)
    value = null_callback(1.0)
    value = nullified_callback(1.0)
    call action_callback()

contains

    real function internal_scale(x)
        real, intent(in) :: x
        internal_scale = 2.0 * x
    end function internal_scale

    subroutine internal_action()
    end subroutine internal_action

end program procedure_call_target_query

real function external_scale(x)
    real, intent(in) :: x
    external_scale = 3.0 * x
end function external_scale
