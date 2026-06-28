! Regression example for preserving non-none implicit rules in modules
module issue_2390_module_implicit_rules
    implicit integer (a-z)
contains
    subroutine assign_value()
        val = 7
    end subroutine assign_value
end module issue_2390_module_implicit_rules
