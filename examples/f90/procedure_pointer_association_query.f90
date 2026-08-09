program procedure_pointer_association_query
    implicit none
    procedure(real), pointer :: active_callback
    procedure(real), pointer :: cleared_callback
    procedure(real), pointer :: reassigned_callback
    procedure(real), pointer :: branched_callback
    real, target :: storage
    real, pointer :: data_pointer
    logical :: active_state
    logical :: cleared_state
    logical :: reassigned_state
    logical :: branched_state
    logical :: data_state
    logical :: pair_state
    logical :: flag

    flag = .false.
    active_callback => callback_scale
    active_state = associated(active_callback)

    cleared_callback => callback_scale
    nullify (cleared_callback)
    cleared_state = associated(cleared_callback)

    reassigned_callback => callback_scale
    reassigned_callback => callback_shift
    reassigned_state = associated(reassigned_callback)

    nullify (branched_callback)
    if (flag) branched_callback => callback_scale
    branched_state = associated(branched_callback)

    storage = 4.0
    data_pointer => storage
    data_state = associated(data_pointer)
    pair_state = associated(data_pointer, storage)

    if (.not. active_state) error stop 'active callback is not associated'
    if (cleared_state) error stop 'cleared callback is associated'
    if (.not. reassigned_state) error stop 'reassigned callback is not associated'
    if (branched_state) error stop 'branched callback is associated'
    if (.not. data_state) error stop 'data pointer is not associated'
    if (.not. pair_state) error stop 'associated target comparison failed'

contains

    real function callback_scale(value)
        real, intent(in) :: value

        callback_scale = 2.0 * value
    end function callback_scale

    real function callback_shift(value)
        real, intent(in) :: value

        callback_shift = value + 1.0
    end function callback_shift

end program procedure_pointer_association_query
