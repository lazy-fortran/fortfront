program test_type_hierarchy
    use type_hierarchy
    implicit none
    logical :: all_passed

    all_passed = .true.

    print *, '=== Type Hierarchy Tests ==='
    print *

    if (.not. test_basic_registration()) all_passed = .false.
    if (.not. test_parent_lookup()) all_passed = .false.
    if (.not. test_subtype_checking()) all_passed = .false.
    if (.not. test_inheritance_chain()) all_passed = .false.
    if (.not. test_cycle_detection()) all_passed = .false.
    if (.not. test_depth_tracking()) all_passed = .false.
    if (.not. test_capacity_growth()) all_passed = .false.
    if (.not. test_case_insensitive()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All type hierarchy tests passed!'
        stop 0
    else
        print *, 'Some type hierarchy tests failed!'
        stop 1
    end if

contains

    logical function test_basic_registration()
        type(type_hierarchy_t) :: h
        character(len=:), allocatable :: error_msg

        test_basic_registration = .true.
        print *, 'Test: Basic type registration'

        h = create_type_hierarchy()

        call h%register_type('base_type', error_msg=error_msg)
        if (allocated(error_msg)) then
            print *, '  FAIL: Error registering base_type:', error_msg
            test_basic_registration = .false.
            return
        end if

        call h%register_type('derived_type', parent_name='base_type', &
            error_msg=error_msg)
        if (allocated(error_msg)) then
            print *, '  FAIL: Error registering derived_type:', error_msg
            test_basic_registration = .false.
            return
        end if

        if (h%count /= 2) then
            print *, '  FAIL: Expected 2 types, got', h%count
            test_basic_registration = .false.
            return
        end if

        print *, '  PASS: Basic registration'
        call destroy_type_hierarchy(h)
    end function test_basic_registration

    logical function test_parent_lookup()
        type(type_hierarchy_t) :: h
        character(len=:), allocatable :: parent

        test_parent_lookup = .true.
        print *, 'Test: Parent lookup'

        h = create_type_hierarchy()
        call h%register_type('base_type')
        call h%register_type('derived_type', parent_name='base_type')

        call h%find_parent('derived_type', parent)
        if (.not. allocated(parent)) then
            print *, '  FAIL: Parent not found for derived_type'
            test_parent_lookup = .false.
            return
        end if

        if (parent /= 'base_type') then
            print *, '  FAIL: Expected base_type, got', parent
            test_parent_lookup = .false.
            return
        end if

        call h%find_parent('base_type', parent)
        if (allocated(parent)) then
            print *, '  FAIL: Base type should have no parent'
            test_parent_lookup = .false.
            return
        end if

        print *, '  PASS: Parent lookup'
        call destroy_type_hierarchy(h)
    end function test_parent_lookup

    logical function test_subtype_checking()
        type(type_hierarchy_t) :: h
        logical :: is_sub

        test_subtype_checking = .true.
        print *, 'Test: Subtype checking'

        h = create_type_hierarchy()
        call h%register_type('animal')
        call h%register_type('mammal', parent_name='animal')
        call h%register_type('dog', parent_name='mammal')

        is_sub = h%is_subtype_of('dog', 'mammal')
        if (.not. is_sub) then
            print *, '  FAIL: dog should be subtype of mammal'
            test_subtype_checking = .false.
            return
        end if

        is_sub = h%is_subtype_of('dog', 'animal')
        if (.not. is_sub) then
            print *, '  FAIL: dog should be subtype of animal'
            test_subtype_checking = .false.
            return
        end if

        is_sub = h%is_subtype_of('mammal', 'dog')
        if (is_sub) then
            print *, '  FAIL: mammal should not be subtype of dog'
            test_subtype_checking = .false.
            return
        end if

        is_sub = h%is_subtype_of('dog', 'dog')
        if (.not. is_sub) then
            print *, '  FAIL: type should be subtype of itself'
            test_subtype_checking = .false.
            return
        end if

        print *, '  PASS: Subtype checking'
        call destroy_type_hierarchy(h)
    end function test_subtype_checking

    logical function test_inheritance_chain()
        type(type_hierarchy_t) :: h
        character(len=:), allocatable :: chain(:)

        test_inheritance_chain = .true.
        print *, 'Test: Inheritance chain'

        h = create_type_hierarchy()
        call h%register_type('base')
        call h%register_type('level1', parent_name='base')
        call h%register_type('level2', parent_name='level1')
        call h%register_type('level3', parent_name='level2')

        call h%get_inheritance_chain('level3', chain)

        if (.not. allocated(chain)) then
            print *, '  FAIL: Chain not allocated'
            test_inheritance_chain = .false.
            return
        end if

        if (size(chain) /= 4) then
            print *, '  FAIL: Expected chain length 4, got', size(chain)
            test_inheritance_chain = .false.
            return
        end if

        if (chain(1) /= 'level3') then
            print *, '  FAIL: Expected level3, got', chain(1)
            test_inheritance_chain = .false.
            return
        end if

        if (chain(4) /= 'base') then
            print *, '  FAIL: Expected base at end, got', chain(4)
            test_inheritance_chain = .false.
            return
        end if

        print *, '  PASS: Inheritance chain'
        call destroy_type_hierarchy(h)
    end function test_inheritance_chain

    logical function test_cycle_detection()
        type(type_hierarchy_t) :: h
        character(len=:), allocatable :: error_msg
        logical :: is_valid

        test_cycle_detection = .true.
        print *, 'Test: Cycle detection'

        h = create_type_hierarchy()
        call h%register_type('type_a')
        call h%register_type('type_b', parent_name='type_a')

        call h%register_type('type_c', parent_name='type_c', error_msg=error_msg)
        if (.not. allocated(error_msg)) then
            print *, '  FAIL: Should detect self-reference'
            test_cycle_detection = .false.
            return
        end if

        is_valid = h%validate_no_cycles('type_b', error_msg)
        if (.not. is_valid) then
            print *, '  FAIL: Valid hierarchy marked as cyclic'
            test_cycle_detection = .false.
            return
        end if

        print *, '  PASS: Cycle detection'
        call destroy_type_hierarchy(h)
    end function test_cycle_detection

    logical function test_depth_tracking()
        type(type_hierarchy_t) :: h
        integer :: depth

        test_depth_tracking = .true.
        print *, 'Test: Depth tracking'

        h = create_type_hierarchy()
        call h%register_type('base')
        call h%register_type('level1', parent_name='base')
        call h%register_type('level2', parent_name='level1')

        depth = h%get_depth('base')
        if (depth /= 1) then
            print *, '  FAIL: Expected depth 1 for base, got', depth
            test_depth_tracking = .false.
            return
        end if

        depth = h%get_depth('level1')
        if (depth /= 2) then
            print *, '  FAIL: Expected depth 2 for level1, got', depth
            test_depth_tracking = .false.
            return
        end if

        depth = h%get_depth('level2')
        if (depth /= 3) then
            print *, '  FAIL: Expected depth 3 for level2, got', depth
            test_depth_tracking = .false.
            return
        end if

        print *, '  PASS: Depth tracking'
        call destroy_type_hierarchy(h)
    end function test_depth_tracking

    logical function test_capacity_growth()
        type(type_hierarchy_t) :: h
        integer :: i
        character(len=20) :: name

        test_capacity_growth = .true.
        print *, 'Test: Capacity growth'

        h = create_type_hierarchy()

        do i = 1, 100
            write (name, '("type_", I0)') i
            call h%register_type(trim(name))
        end do

        if (h%count /= 100) then
            print *, '  FAIL: Expected 100 types, got', h%count
            test_capacity_growth = .false.
            return
        end if

        if (h%capacity < 100) then
            print *, '  FAIL: Capacity should have grown'
            test_capacity_growth = .false.
            return
        end if

        print *, '  PASS: Capacity growth'
        call destroy_type_hierarchy(h)
    end function test_capacity_growth

    logical function test_case_insensitive()
        type(type_hierarchy_t) :: h
        character(len=:), allocatable :: parent
        logical :: is_sub

        test_case_insensitive = .true.
        print *, 'Test: Case insensitive matching'

        h = create_type_hierarchy()
        call h%register_type('BaseType')
        call h%register_type('DerivedType', parent_name='basetype')

        call h%find_parent('derivedtype', parent)
        if (.not. allocated(parent)) then
            print *, '  FAIL: Case insensitive parent lookup failed'
            test_case_insensitive = .false.
            return
        end if

        is_sub = h%is_subtype_of('DERIVEDTYPE', 'BASETYPE')
        if (.not. is_sub) then
            print *, '  FAIL: Case insensitive subtype check failed'
            test_case_insensitive = .false.
            return
        end if

        print *, '  PASS: Case insensitive matching'
        call destroy_type_hierarchy(h)
    end function test_case_insensitive

end program test_type_hierarchy
