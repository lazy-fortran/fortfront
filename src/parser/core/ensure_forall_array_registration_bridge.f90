subroutine ensure_forall_array_registration_bridge()
    ! Keep this as an external subroutine to avoid a module dependency cycle
    ! between forall parsing and array/associate construct parsing.
    use parser_forall_module, only: register_forall_body_parsers
    use parser_array_constructs_module, only: parse_where_construct, &
        parse_associate
    implicit none

    call register_forall_body_parsers(parse_where_construct, parse_associate)

end subroutine ensure_forall_array_registration_bridge
