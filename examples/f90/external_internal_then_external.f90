subroutine host_procedure()
    implicit none

contains

    subroutine internal_procedure()
        implicit none
    end subroutine internal_procedure

end subroutine host_procedure

subroutine following_external_procedure()
    implicit none
end subroutine following_external_procedure
