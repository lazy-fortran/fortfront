module math_mod
contains
    subroutine compute()
        call helper()
    contains
        subroutine helper()
        end subroutine helper
    end subroutine compute
end module math_mod
program app
    use math_mod
contains
    subroutine run()
        call compute()
    end subroutine run
end program app
