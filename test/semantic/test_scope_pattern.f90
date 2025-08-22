program test_scope_pattern
    use type_system_unified, only: mono_type_t, poly_type_t, type_var_t, &
                                  create_mono_type, create_poly_type, TINT
    implicit none

    type :: env_entry
        character(len=:), allocatable :: name
        type(poly_type_t) :: scheme
    end type

    type(poly_type_t), allocatable :: result
    type(env_entry), allocatable :: entries(:)
    type(mono_type_t) :: mono_sin, mono_cos

    ! Create some entries using unified arena system
    allocate (entries(2))

    ! Entry 1 - create mono type using arena
    mono_sin = create_mono_type(TINT)  ! Basic integer type
    entries(1)%name = "sin"
    ! Use single element array instead of empty array
    block
        type(type_var_t) :: empty_forall(0)
        entries(1)%scheme = create_poly_type(empty_forall, mono_sin)
    end block

    ! Entry 2 - create mono type using arena  
    mono_cos = create_mono_type(TINT)  ! Basic integer type
    entries(2)%name = "cos"
    block
        type(type_var_t) :: empty_forall(0)
        entries(2)%scheme = create_poly_type(empty_forall, mono_cos)
    end block

    print *, "=== Test lookup pattern ==="
    call lookup("sin", result)
    if (allocated(result)) then
        print *, "Found result"
        block
            type(mono_type_t) :: mono
            mono = result%get_mono()
            print *, "mono type kind = ", mono%kind
        end block
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
                print *, "  Found match! Doing assignment..."
                allocate(scheme)
                scheme = entries(i)%scheme  ! Uses unified arena assignment
                print *, "  Assignment complete"
                return
            end if
        end do

        print *, "  Not found in entries"
    end subroutine

end program test_scope_pattern
