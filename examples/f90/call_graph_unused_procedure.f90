module call_graph_unused_procedure_mod
    implicit none
contains
    integer function used_value()
        used_value = 1
    end function used_value

    integer function unused_value()
        unused_value = 2
    end function unused_value
end module call_graph_unused_procedure_mod

program call_graph_unused_procedure
    use call_graph_unused_procedure_mod, only: used_value
    implicit none

    print *, used_value()
end program call_graph_unused_procedure
