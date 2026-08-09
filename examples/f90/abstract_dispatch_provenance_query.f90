module abstract_dispatch_provenance_query
    implicit none

    type, abstract :: root_t
    contains
        procedure :: work => root_work
        procedure(run_interface), deferred, pass(self) :: run
    end type root_t

    type, abstract, extends(root_t) :: middle_t
    contains
        procedure :: work => middle_work
    end type middle_t

    type, extends(middle_t) :: leaf_t
    contains
        procedure, pass(self) :: run => leaf_run
    end type leaf_t

    type :: generic_t
    contains
        procedure, nopass :: choose_left
        procedure, nopass :: choose_right
        generic :: choose => choose_left, choose_right
    end type generic_t

    abstract interface
        subroutine run_interface(self)
            import root_t
            class(root_t), intent(inout) :: self
        end subroutine run_interface
    end interface

contains

    subroutine invoke(self)
        class(root_t), intent(inout) :: self

        call self%work()
        call self%run()
    end subroutine invoke

    subroutine root_work(self)
        class(root_t), intent(inout) :: self
    end subroutine root_work

    subroutine middle_work(self)
        class(middle_t), intent(inout) :: self
    end subroutine middle_work

    subroutine leaf_run(self)
        class(leaf_t), intent(inout) :: self
    end subroutine leaf_run

    subroutine choose_left(value)
        integer, intent(in) :: value
    end subroutine choose_left

    subroutine choose_right(value)
        real, intent(in) :: value
    end subroutine choose_right

end module abstract_dispatch_provenance_query

program abstract_dispatch_provenance_runtime
    use abstract_dispatch_provenance_query, only: leaf_t, invoke
    implicit none

    type(leaf_t) :: object

    call invoke(object)
    print *, 'PASS: abstract dispatch provenance runtime'
end program abstract_dispatch_provenance_runtime
