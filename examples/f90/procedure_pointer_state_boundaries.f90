module procedure_pointer_state_global
    implicit none
    procedure(real), pointer :: global_callback

contains

    real function global_target(value)
        real, intent(in) :: value

        global_target = value + 2.0
    end function global_target

end module procedure_pointer_state_global

program procedure_pointer_state_boundaries
    use procedure_pointer_state_global, only: global_callback, global_target
    implicit none
    procedure(real), pointer :: local_callback
    logical :: local_state
    logical :: if_state
    logical :: global_state
    logical :: host_state
    logical :: dummy_state

    local_callback => local_target
    global_callback => global_target
    local_state = associated(local_callback)
    global_state = associated(global_callback)
    if (associated(local_callback)) then
        if_state = .true.
    else
        if_state = .false.
    end if
    call observe_dummy_alias(local_callback)
    call observe_host_alias()

    if (.not. local_state) error stop 'local callback is not associated'
    if (.not. if_state) error stop 'associated IF guard is false'
    if (.not. global_state) error stop 'global callback is not associated'
    if (.not. dummy_state) error stop 'dummy callback is not associated'
    if (.not. host_state) error stop 'host callback is not associated'

contains

    real function local_target(value)
        real, intent(in) :: value

        local_target = 3.0 * value
    end function local_target

    subroutine observe_dummy_alias(dummy_callback)
        procedure(real), pointer :: dummy_callback

        dummy_state = associated(dummy_callback)
    end subroutine observe_dummy_alias

    subroutine observe_host_alias()
        local_callback => local_target
        host_state = associated(local_callback)
        nullify (local_callback)
    end subroutine observe_host_alias

end program procedure_pointer_state_boundaries
