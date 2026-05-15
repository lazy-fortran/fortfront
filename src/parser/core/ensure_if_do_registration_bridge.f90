subroutine ensure_if_do_registration_bridge()
    ! Keep this as an external subroutine to avoid a module dependency cycle
    ! between IF parsing and DO construct parsing.
    use parser_do_constructs_module, only: ensure_if_do_registration
    implicit none

    call ensure_if_do_registration()

end subroutine ensure_if_do_registration_bridge
