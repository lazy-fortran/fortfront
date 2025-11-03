! Issue #1611: allocate(TypeName :: var) should keep the type-spec verbatim
module mymod_allocate_type
    implicit none
    type :: mytype
        integer :: value
    end type mytype
contains
    function create() result(obj)
        class(mytype), allocatable :: obj
        allocate(mytype :: obj)
    end function create
end module mymod_allocate_type
