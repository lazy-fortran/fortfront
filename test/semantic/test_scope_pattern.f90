program test_scope_pattern
    implicit none

    ! Exact types from type_system_hm
    type :: type_var_t
        integer :: id
        character(len=:), allocatable :: name
    end type

    type :: mono_type_t
        integer :: kind
        type(type_var_t) :: var
        type(mono_type_t), allocatable :: args(:)
        integer :: size
    end type

    type :: poly_type_t
        type(type_var_t), allocatable :: forall (:)
        type(mono_type_t) :: mono
    end type

    type :: env_entry
        character(len=:), allocatable :: name
        type(poly_type_t) :: scheme
    end type

    type(poly_type_t), allocatable :: result
    type(env_entry), allocatable :: entries(:)

    ! Create some entries
    allocate (entries(2))

    ! Entry 1
    entries(1)%name = "sin"
    entries(1)%scheme%mono%kind = 1
    entries(1)%scheme%mono%var%id = 1
    entries(1)%scheme%mono%var%name = "real_to_real"

    ! Entry 2
    entries(2)%name = "cos"
    entries(2)%scheme%mono%kind = 1
    entries(2)%scheme%mono%var%id = 2
    entries(2)%scheme%mono%var%name = "real_to_real"

    print *, "=== Test lookup pattern ==="
    call lookup("sin", result)
    if (allocated(result)) then
        print *, "Found result"
        print *, "mono%var%name = ", result%mono%var%name
    else
        print *, "Not found"
    end if

    print *, ""
    print *, "=== Test lookup not found ==="
    if (allocated(result)) deallocate (result)  ! Make sure it's clean
    print *, "About to call lookup('tan')"
    call lookup("tan", result)
    print *, "Returned from lookup('tan')"
    if (allocated(result)) then
        print *, "Found result (unexpected)"
    else
        print *, "Not found (expected)"
    end if

contains

    subroutine lookup(name, scheme)
        character(len=*), intent(in) :: name
        type(poly_type_t), allocatable, intent(out) :: scheme
        integer :: i

        print *, "Looking for '", trim(name), "'"

        do i = 1, size(entries)
            print *, "  Checking entry ", i, ": '", trim(entries(i)%name), "'"
            if (entries(i)%name == name) then
                print *, "  Found match! Doing deep_copy..."
                scheme = poly_deep_copy(entries(i)%scheme)
                print *, "  Deep copy complete"
                return
            end if
        end do

        print *, "  Not found in entries"
    end subroutine

    function mono_deep_copy(this) result(copy)
        class(mono_type_t), intent(in) :: this
        type(mono_type_t) :: copy

        copy%kind = this%kind
        copy%var = this%var  ! This should do automatic deep copy
        copy%size = this%size

        if (allocated(this%args)) then
            allocate (copy%args(size(this%args)))
            ! Would need recursive deep copy here
        end if
    end function

    function poly_deep_copy(this) result(copy)
        class(poly_type_t), intent(in) :: this
        type(poly_type_t) :: copy
        integer :: i

        if (allocated(this%forall)) then
            allocate (copy%forall(size(this%forall)))
            copy%forall = this%forall
        end if

        copy%mono = mono_deep_copy(this%mono)
    end function

end program test_scope_pattern
