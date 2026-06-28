program issue_2347_bare_end_program
    implicit none
contains
    subroutine increment(value)
        integer, intent(inout) :: value
        value = value + 1
        end

        integer function double_value(input) result(result_value)
            integer, intent(in) :: input
            integer :: result_value
            result_value = input * 2
            end
            end
