module issue_2253_procedure_interface
    implicit none
    interface my_iface
        procedure :: foo
    end interface my_iface
contains
    subroutine foo()
    end subroutine foo
end module issue_2253_procedure_interface
