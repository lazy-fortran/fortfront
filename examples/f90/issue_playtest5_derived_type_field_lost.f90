! Issue #2164: Derived type field lost in definition, assignment targets wrong object
program test_derived_field_lost
    implicit none

    type :: inner_type
        integer :: value
    end type inner_type

    type :: outer_type
        type(inner_type) :: inner
        real :: data
    end type outer_type

    type(outer_type) :: obj

    obj%inner%value = 42
    obj%data = 3.14

    print *, obj%inner%value
    print *, obj%data
end program test_derived_field_lost
