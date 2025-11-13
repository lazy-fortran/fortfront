module issue_2265_module_program_mod
    implicit none
contains
    subroutine no_op()
    end subroutine no_op
end module issue_2265_module_program_mod

program issue_2265_roundtrip_app
    use issue_2265_module_program_mod, only: no_op
    implicit none

    call no_op()
end program issue_2265_roundtrip_app
