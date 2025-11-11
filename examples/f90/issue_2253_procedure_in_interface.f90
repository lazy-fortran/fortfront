module demo_procedure_in_interface
    implicit none
    interface my_iface
        procedure :: foo
    end interface my_iface
contains
    subroutine foo()
    end subroutine foo
end module demo_procedure_in_interface
