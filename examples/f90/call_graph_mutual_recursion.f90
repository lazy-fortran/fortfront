module call_graph_mutual_recursion_mod
    implicit none
contains
    recursive integer function first(n) result(value)
        integer, intent(in) :: n

        if (n <= 0) then
            value = 0
        else
            value = second(n - 1)
        end if
    end function first

    recursive integer function second(n) result(value)
        integer, intent(in) :: n

        if (n <= 0) then
            value = 1
        else
            value = first(n - 1)
        end if
    end function second
end module call_graph_mutual_recursion_mod

program call_graph_mutual_recursion
    use call_graph_mutual_recursion_mod, only: first
    implicit none

    print *, first(2)
end program call_graph_mutual_recursion
